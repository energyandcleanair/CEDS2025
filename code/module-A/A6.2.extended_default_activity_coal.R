# ------------------------------------------------------------------------------------------
# Program Name: A6.2.extended_default_activity_coal.R
# Author: Presley Muwan, Rachel Hoesly, Patrick O'Rourke
# Date Last Updated: March 9, 2020
# Program Purpose: Create the default activity coal data for CEDS following these three steps:
#                 * Merge IEA coal data (2014 - 1960/1971) to UNSD coal data (1959/1970 - 1950)
#                   and extend the merged data from 1950 to 1750 using CDIAC data.
#                 * Disaggregate the merge data by iso-fuel
#                 * Disaggregate the merged data by CEDS iso-fuel-Sector
# Input Files: CDA1_UNSD_Energy_Final_Consumption_by_Ctry.csv, A.default_comb_activity_with_other.csv,
#              E.CO2_CDIAC_inventory.csv, A.full_default_sector_shares.csv,
#              IEA_iso_start_data.csv
# Output Files:  A.comb_activity_extended_coal.csv
# TODO: (Future:) The fossil extension scripts could be functionalized more,
#       as a large amount of the processing here is the same for each fossil fuel.
# -----------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c("data_functions.R","process_db_functions.R", "default_activity_functions.R") # Additional function files may be required.
log_msg <- "Extending coal data with CDIAC and UNSD data..." # First message to be printed to the log
script_name <- "A6.2.extended_default_activity_coal.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

#-------------------------------------------------------------------------------------
# 1. Read in Files

# TODO: (Future) "CDA1_UNSD_Energy_Final_Consumption_by_Ctry.csv" may have international shipping activity data
#        within its total by fuel. If so, this amount would need to be removed from the total. We currently assume
#        that this total does not include international shipping fuel consumption.
UNSD_Energy_Final_Consumption_all <- readData( 'EXT_IN',"CDA1_UNSD_Energy_Final_Consumption_by_Ctry" , meta = F )
# TODO: (Future) ^ Remove meta = F for UNSD when metadata file is added to the system
A.comb_activity_all <- readData( 'MED_OUT', "A.default_comb_activity_with_other" )
cdiac_fuel_all <- readData( 'MED_OUT' , 'E.CO2_CDIAC_inventory' )
full_default_sector_shares_all <- readData( 'MED_OUT', 'A.full_default_sector_shares' )
iea_start_year_all <- readData('ENERGY_IN' , 'IEA_iso_start_data' )

#-------------------------------------------------------------------------------------
# 2. Define Variables and Filter and Process inputs

# Define fuels
ceds_extension_fuels <- c( 'brown_coal','coal_coke','hard_coal' )
aggregate_fuel_name <- 'coal'
cdiac_fuel_name <- 'solid_fuels'
default_fuel_share <- data.frame( fuel = ceds_extension_fuels,
                                  breakdown = c( 0, 0, 1 ) )

# Process UN data
#   Check that there are no negative values
    if( any( UNSD_Energy_Final_Consumption_all[ , X_UNSD_years ] < 0 & !is.na( UNSD_Energy_Final_Consumption_all[ , X_UNSD_years ] ) ) ){

        stop( "CDA1_UNSD_Energy_Final_Consumption_by_Ctry.csv contains negative activity data. Please check..." )

    }

#   Filter out countries that have lines of data, but the data has all zero values
    UN_countries <- UNSD_Energy_Final_Consumption_all %>%
        dplyr::filter( fuel %in% ceds_extension_fuels ) %>%
        dplyr::group_by( iso ) %>%
        dplyr::summarize_if( is.numeric, sum ) %>%
        dplyr::mutate( sum_all = rowSums(.[grep( "X", names( . ) ) ], na.rm = TRUE ) ) %>% # Sum rows over all columns
        dplyr::filter( sum_all > 0 ) %>%
        pull( iso )

    UNSD_Energy_Final_Consumption_filtered <- UNSD_Energy_Final_Consumption_all %>%
        dplyr::filter( fuel %in% ceds_extension_fuels,
                       iso %in% UN_countries )

#   Interpolate and extend
    printLog( "Extending and interpolating UNSD energy combustion activity data...")
    UNSD_Energy_Final_Consumption <- extend_and_interpolate( UNSD_Energy_Final_Consumption_filtered, X_UNSD_years )

# Filter other input data for fuels and remove international shipping
A.default_comb_activity_with_other <- A.comb_activity_all %>%
    dplyr::filter( fuel %in% ceds_extension_fuels,
                   iso != 'global',
                   sector != "1A3di_International-shipping" )

cdiac_fuel <- cdiac_fuel_all %>%
    dplyr::filter( fuel %in% cdiac_fuel_name ) %>%
    dplyr::filter( iso != 'global' )

full_default_sector_shares <- full_default_sector_shares_all %>%
    dplyr::filter( fuel %in% ceds_extension_fuels )

iea_start_year <- iea_start_year_all %>%
    dplyr::filter( iso != 'global' )

#-------------------------------------------------------------------------------------
# 3. Process_and_combine_un_ced_data function is called to format CEDS and UN data
#    into a common format, and the merged together. The merged data is then extended
#    backward from 1950 to 1750, by CDIAC
#
# Extend the coal data by CDIAC
printLog( paste( "Extending aggregate ", aggregate_fuel_name, ' with UN and CDIAC data back to', cdiac_start_year, '...' ) )

ceds_un_extended_data <- merge_extend_UN_CEDS_data( a.CEDS_data = A.default_comb_activity_with_other,
                                                    a.CDIAC_data = cdiac_fuel,
                                                    a.UN_data = UNSD_Energy_Final_Consumption,
                                                    a.ceds_extension_fuels = ceds_extension_fuels,
                                                    a.extension_start_year = cdiac_start_year,
                                                    a.iea_start_years = iea_start_year,
                                                    a.iea_end_year = end_year,
                                                    a.aggregate_fuel = aggregate_fuel_name,
                                                    a.CDIAC_fuel = cdiac_fuel_name )

#-----------------------------------------------------------------------------------
# 4. Compute the fuel Percentage breakdown
#
#    Take note of coutries with no UN data: for these isos, we extend back from their
#    earliest reported year of IEA data.
printLog( 'Disaggregating total coal into CEDS coal subfuel types...' )

#    Call function to disaggregate by fuel
all_disaggregate_fuel <- fuel_breakdown( a.UN_data = UNSD_Energy_Final_Consumption,
                                         a.CEDS_UN_aggregate = ceds_un_extended_data$un_ceds ,
                                         a.CEDS_only_aggregate = ceds_un_extended_data$ceds_only,
                                         a.CEDS_comb_with_other = A.default_comb_activity_with_other,
                                         a.ceds_extension_fuels = ceds_extension_fuels,
                                         a.extension_start_year = cdiac_start_year,
                                         a.aggregate_fuel = aggregate_fuel_name,
                                         a.default_fuel_share = default_fuel_share,
                                         a.UN_start = UNSD_start_year,
                                         a.iea_start_years = iea_start_year )

# ------------------------------------------------------------------------------------------------
# 5. CEDS fuel_types into sector split.
#
#    Disaggregate_to_CEDS_Sectors function is called to disaggregrate fuel values into CEDS sectors.
#    The default break down values (sector ratios) are read from default_sector_breakdown csv file.

printLog( 'Disaggregating fuel_types into sector split...' )

CEDS_default_actvity <- sector_breakdown( fuel_totals = all_disaggregate_fuel,
                                          sector_shares = full_default_sector_shares_all,
                                          iea_start_years = iea_start_year,
                                          ceds_extension_fuels = ceds_extension_fuels )

# ------------------------------------------------------------------------------------------------
# 6. Arrange final df
CEDS_default_actvity_final <- CEDS_default_actvity %>%
    dplyr::mutate( units = 'kt' ) %>%
    dplyr::arrange( iso, sector, fuel, units )

#-----------------------------------------------------------------------------------------------
# 7. Print output
writeData( CEDS_default_actvity_final , "MED_OUT", "A.comb_activity_extended_coal" )

logStop( )
# END
