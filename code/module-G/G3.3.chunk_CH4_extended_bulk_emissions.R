# ------------------------------------------------------------------------------
# Program Name: G3.3.chunk_CH4_extended_bulk_emissions.R
# Author(s): Leyang Feng
# Date Last Updated: Jun 27 2017
# Program Purpose: Generate multi-year emissions chunks for bulk emissions.
# Input Files: CEDS_[em]_anthro_[year]_0.5_[CEDS_version].nc
# Output Files: FIN_OUT: [em]-em-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_gn_[time_range].nc
# Notes:
# TODO: Convert to work with chunk_emissions function.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
  headers <- c( 'gridding_functions.R', 'data_functions.R', 'nc_generation_functions.R' )
  log_msg <- "Generates chunk NetCDF files for bulk emissions"
  script_name <- "G3.3.chunk_CH4_extended_bulk_emissions.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

  # Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
  if ( is.na( em ) ) em <- "CH4"
  if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

  grid_resolution <- res
  start_year <- 1850
  end_year <- 1960
  chunk_density <- 10
  chunk_years <- 12

# basic start year/end year check

  if ( end_year < start_year ) { stop( ' End year must not be earlier than start year. ') }

# calculate chunk start years
  total_years <- ( end_year - start_year ) / chunk_density + 1
  chunk_count <- ceiling( total_years / chunk_years  )
# calculate chunk end years
  chunk_start_years <- unlist( lapply( 1 : chunk_count, function( i ) { chunk_start_years <- start_year + ( i - 1 ) * chunk_years * chunk_density } ) )
  chunk_end_years <- chunk_start_years + ( chunk_years * chunk_density - 1 * chunk_density )
  if ( chunk_end_years[ length( chunk_end_years ) ] > end_year ) { chunk_end_years[ length( chunk_end_years ) ] <- end_year }

# setup dirs
  input_dir <- filePath( 'MED_OUT', 'gridded-emissions/', extension = "" )
  output_dir <- filePath( 'FIN_OUT', 'gridded-emissions/', extension = "" )

# ------------------------------------------------------------------------------
# 1. Chunking


# Start chunking

  printLog( paste0( 'Start ', em, ' grids chunking from ', start_year, ' to ', end_year ) )

  for ( chunk_count_index in 1 : chunk_count ) {

    singleVarChunking_extendedCH4bulk( em,
                                       grid_resolution,
                                       chunk_start_years,
                                       chunk_end_years,
                                       chunk_density,
                                       chunk_count_index,
                                       input_dir,
                                       output_dir )
  } # END of for loop

# -----------------------------------------------------------------------------
# 2. Stop

# Every script should finish with this line:
  logStop()


