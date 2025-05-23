# ------------------------------------------------------------------------------
# Program Name: G1.4.grid_solidbiofuel_emissions.R
# Authors: Leyang Feng, Caleb Braun, Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated biomass emissions into NetCDF grids for bulk emissions (excluding AIR)
# Input Files:  MED_OUT:    [em]_total_CEDS_emissions.csv
# Output Files: MED_OUT:    gridded-emissions/CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].nc
#                           gridded-emissions/CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT:   G.[em]_solidfuel_emissions_checksum_comparison_diff.csv
#                           G.[em]_solidfuel_emissions_checksum_comparison_per.csv
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R' )
log_msg <- "Gridding anthropogenic biomass emissions (excluding AIR) "
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G1.4.grid_solidbiofuel_emissions.R", log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01

# Set up directories
output_dir          <- filePath( "MED_OUT",  "gridded-emissions/",     extension = "" )
total_grid_dir      <- filePath( "DIAG_OUT", "total-emissions-grids/", extension = "" )
proxy_dir           <- filePath( "GRIDDING", "proxy/",                 extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
mask_dir            <- filePath( "GRIDDING", "mask/",                  extension = "" )
seasonality_dir     <- filePath( "GRIDDING", "seasonality/",           extension = "" )
final_emissions_dir <- filePath( "FIN_OUT",  "current-versions/",      extension = "" )

# Initialize the gridding parameters
gridding_initialize( grid_resolution = res,
                     start_year = 1750,
                     end_year = end_year,
                     load_masks = T,
                     load_seasonality_profile = T )


# ------------------------------------------------------------------------------
# 1. Read in files

# read in the emission data
  emissions <- readData( "MED_OUT", paste0( em, '_total_CEDS_emissions'  ) )

# If defined, remove emissions from one iso from gridding
if ( grid_remove_iso != "" ) {
  emissions <- dplyr::mutate_at( emissions, vars( all_of(X_extended_years) ),
                                 list( ~ifelse( iso == grid_remove_iso, 0, . )))
}

# read in some mapping files
# read in the region location index, which indicates the location of each region mask in the 'world' matrix
  location_index <- readData( "GRIDDING", domain_extension = "gridding_mappings/", file_name =  "country_location_index_05", meta = FALSE )
# read in the CEDS gridding sector mapping
  ceds_gridding_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
# read in the proxy mapping
  proxy_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'proxy_mapping', meta = FALSE )
# read in the seasonality mapping
  seasonality_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'seasonality_mapping', meta = FALSE )
# read in the proxy substitution mapping
  proxy_substitution_mapping <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'proxy_subsititution_mapping', meta = FALSE )
# read in CEDS final gridding sector name list
  sector_name_mapping <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'CEDS_gridding_sectors', meta = FALSE )
  sector_name_mapping <- unique( sector_name_mapping[ , c( "CEDS_fin_sector", "CEDS_fin_sector_short" ) ] )
# Tolerances for checksums to give warning or throw error
checksum_tols                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'checksums_error_tolerance', meta = FALSE )

# ------------------------------------------------------------------------------
# 2. Pre-processing

# a) Convert the emission data from CEDS working sectors to CEDS level 1 gridding sector
# b) Drop non-matched sectors
# c) Select biomass only and drop non-AIR sectors
# d) Aggregate the emissions at the gridding sectors
# e) Fix names
gridding_emissions <- ceds_gridding_mapping %>%
  dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
  dplyr::inner_join( emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
  dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
  dplyr::rename( sector = CEDS_int_gridding_sector_short ) %>%
  dplyr::filter( fuel == 'biomass', sector != 'AIR' ) %>%
  dplyr::select( -fuel ) %>%
  dplyr::group_by( iso, sector ) %>%
  dplyr::summarise_at( paste0( 'X', year_list ), sum ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( sector, iso ) %>%
  as.data.frame()

proxy_files <- list( primary = list.files( proxy_dir ), backup = list.files( proxy_backup_dir ) )

#Extend to last year
proxy_mapping <- extendProxyMapping( proxy_mapping )
seasonality_mapping <- extendSeasonalityMapping( seasonality_mapping )

# ------------------------------------------------------------------------------
# 3. Gridding and writing output data

# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. Future work could parallelize the year loop.

printLog( paste( 'Gridding', em, 'emissions for each year...' ) )

pb <- txtProgressBar(min = 0, max = length(year_list), style = 3)

for ( year in year_list ) {
  setTxtProgressBar(pb, year - min(year_list))

  # grid one years emissions
  int_grids_list <- grid_one_year( year, em, grid_resolution, gridding_emissions, location_index,
                                   proxy_mapping, proxy_substitution_mapping, proxy_files )

  # generate nc file for gridded one years emissions,
  # a checksum file is also generated along with the nc file
  # which summarize the emissions in mass by sector by month.
  generate_final_grids_nc_solidbiofuel( int_grids_list, output_dir, grid_resolution, year,
                                        em, sector_name_mapping, seasonality_mapping )
}

close(pb)


# -----------------------------------------------------------------------------
# 4. Checksum
printLog( 'Start checksum check' )

# calculate global total emissions by sector by year
gridding_emissions_fin <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short ) %>%
    dplyr::distinct() %>%
    dplyr::right_join( gridding_emissions, by = c( 'CEDS_int_gridding_sector_short' = 'sector' ) ) %>%
    dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
    dplyr::summarise_at( vars( starts_with( 'X' ) ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_final_gridding_sector_short ) %>%
    dplyr::arrange( sector )

# manually add in all 0 lines for AGR, SLV, and WST because they do have emissions from fuel biomass
temp_matrix <- matrix( 0, 3, length( year_list ) )
temp_df <- data.frame( temp_matrix )
temp_df$sector <- c( 'AGR', 'SLV', 'WST' )
names( temp_df ) <- c( paste0( 'X', year_list ), 'sector' )
temp_df <- temp_df[ ,c( 'sector', paste0( 'X', year_list ) ) ]
gridding_emissions_fin <- rbind( gridding_emissions_fin, temp_df)
gridding_emissions_fin <- gridding_emissions_fin[ order( gridding_emissions_fin$sector ), ]

# consolidate different checksum files to have total emissions by sector by year
checksum_df <- list.files( output_dir, paste0( '_', em, '_solidbiofuel_anthro_.*[.]csv' ), full.names = TRUE ) %>%
    lapply( read.csv ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by( sector, year ) %>%
    dplyr::summarise( value = sum( value ) ) %>%
    dplyr::ungroup() %>%
    tidyr::spread( year, value ) %>%
    dplyr::rename_all( make.names ) %>%
    dplyr::arrange( sector )

# comparison
X_year_list <- paste0( 'X', year_list )
diag_diff_df <- cbind( checksum_df$sector, abs( gridding_emissions_fin[ X_year_list ] - checksum_df[ X_year_list ] ) )
diag_per_df <- cbind( checksum_df$sector, ( diag_diff_df[ X_year_list ] / gridding_emissions_fin[ X_year_list ] ) * 100 )
diag_per_df[ is.nan.df( diag_per_df ) ] <- NA
diag_per_df <- diag_per_df %>%
  dplyr::rename('sector' = 'checksum_df.sector')
diag_diff_df <- diag_diff_df %>%
  dplyr::rename('sector' = 'checksum_df.sector')


# -----------------------------------------------------------------------------
# 5. Write-out

out_name <- paste0( 'G.', em, '_solidfuel_emissions_checksum_comparison_diff' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', em, '_solidfuel_emissions_checksum_comparison_per' )
writeData( diag_per_df, "DIAG_OUT", out_name )


# -----------------------------------------------------------------------------
# 6. Checksum warnings
# Get max deviation per sector, and combine that with user defined tolerance mapping file
error_check_df <- diag_per_df %>%
  tidyr::gather('year', 'em', X_year_list) %>%
  dplyr::group_by(sector) %>%
  na.omit() %>%
  dplyr::summarise( M = max(em) ) %>%
  dplyr::left_join(checksum_tols, by = 'sector')

{
print('===============================================================================')
print('===============================================================================')
# Warn / Error for any sectors that warrant it
throw_error <- FALSE
for(i in 1:dim(error_check_df)[1]){
  if(error_check_df[i, 'M'] > error_check_df[i, 'warning_tol']){
    print(paste0('Warning: Checksum diagnostics have found deviations in the ', error_check_df[i, 'sector'], ' sector beyond ', error_check_df[i, 'warning_tol'], '%.'))
  }
  if(error_check_df[i, 'M'] > error_check_df[i, 'error_tol']){
    print(paste0('ERROR: Checksum diagnostics have found deviations in the ', error_check_df[i, 'sector'], ' sector beyond ', error_check_df[i, 'error_tol'], '%.'))
    throw_error <- TRUE
  }
}
if(throw_error){
  print('===============================================================================')
  print('===============================================================================')
  stop('Checksum Deviation Error')
}

print('No checksum deviations above the set error thresholds.')
print('===============================================================================')
print('===============================================================================')
}


# END -------------------------------------------------------------------------
logStop()
