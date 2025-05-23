# ------------------------------------------------------------------------------
# Program Name: A7.2.add_activity_fossil_fuel_production.R
# Author: Linh Vu
# Date Last Modified: December 21, 2020
# Program Purpose: Extend CEDS activity backward with fossil fuel data
# Input Files:  A.NC_activity_extended_db.csv, CEDS_historical_extension_drivers_activity.csv,
#               A.crude_oil_production_data.csv
# Output Files: A.NC_activity_extended_db.csv
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending CEDS activity_data with fossil fuel consumption data" # First message to be printed to the log
script_name <- "A7.2.add_activity_fossil_fuel_production.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT', 'A.NC_activity_extended_db' )
extension_drivers_all <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')
oil_production <- readData( "MED_OUT" , 'A.crude_oil_production_data' )

# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

# TODO: Code here and below needs to be expaneded to accomidate production data for fuels other than crude oil.
drivers <- extension_drivers_all[which( extension_drivers_all$driver_data_source == 'fossil_fuel_production' &
                                        extension_drivers_all$extra_driver_info == 'crude_oil' ), ]
activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Driver data processing
op_id_cols <- names( oil_production )[ !grepl( "X", names( oil_production ) ) ]
Hyde_years_extended <- paste0("X", historical_pre_extension_year:(Hyde_start_year-1) )
oil_production[,Hyde_years_extended] <- 0
oil_prod <- oil_production[, c( op_id_cols, paste0("X", historical_pre_extension_year:end_year ) ) ]


# ---------------------------------------------------------------------------
# 4. Extend Data
  ratio_year <- unique(drivers[,'ext_end_year'])
  ext_start_year <- unique(drivers[,'ext_start_year'])

  # if this driver is not used, exit
  if ( ( length( ext_start_year ) == 0 ) ) {
      printLog( "Exiting - no fossil fuel drivers needed" )
  } else {
  # Rest of routine

  extension_years <- paste0('X',ext_start_year:ratio_year)

  # select extension data for current method
  sectors <- drivers[, c('sector','fuel') ]
  sectors <- paste(sectors$sector,sectors$fuel,sep='-')

  # select CEDS data to extend
  ceds_extension <- activity[ which( paste(activity$sector, activity$fuel, sep="-") %in% sectors  ) , ]

  # add fossil fuel
  ceds_extension[extension_years] <- oil_prod[match(ceds_extension$iso, oil_prod$iso)  , extension_years ]

  # add to final activity
  activity <- replaceValueColMatch(activity, ceds_extension,
                                   x.ColName = extension_years,
                                   match.x = c('iso','sector','fuel'),
                                   addEntries = FALSE)


# ---------------------------------------------------------------------------
# 5. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , 'A.NC_activity_extended_db' ) }

 # End of big loop - if no fossil production data needed
 }

logStop()
