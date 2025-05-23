# ------------------------------------------------------------------------------
# Program Name: C3.1.calc_sintering_emissions.R
# Author: Andrea Mott
# Date Last Updated: 17 August 2021
# Program Purpose: Process sintering emissions based on sintering production data.
# Input Files:  A.Sintering_production.csv, U.[em]_sintering_EF.csv
# Output Files: C.(em)_sintering_emissions.csv
# TODO:

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "common_data.R", "data_functions.R", "analysis_functions.R",
              "process_db_functions.R", "timeframe_functions.R",
              "interpolation_extension_functions.R" ) # Additional function files may be required.
log_msg <- "Process pig iron production"
script_name <- "C3.1.calc_sintering_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------

# Input kt sinter
kt_sinter <- readData( "MED_OUT", "A.Sintering_production")

kt_sinter_long <- kt_sinter %>%
    select(-c(sector, units)) %>%
    gather(key = "year", value = "kt_sinter", -iso)

# This script is intended only for those emission with user added EFs for sintering:
if (em %in% c("CO", "NOx","SO2")){

    EF <- readData( "DEFAULT_EF_PARAM_NC", paste0( 'U.', em, '_sintering_EF'), domain_extension = "sintering_EFs/")
    EF_value <- EF$X2000

    EF_long <- EF %>%
        filter( sector == "2C1_Iron-steel-alloy-prod_sintering") %>%
        select(-c(sector,fuel,units)) %>%
        gather(key = "year", value = "EF", -iso)

    emissions <- kt_sinter_long$kt_sinter * EF$X2000[1]
    sint_em_bind <- cbind(kt_sinter_long, emissions)

    # Calculate sintering emissions
    sint_em <- sint_em_bind %>%
        select(iso,year,emissions) %>%
        na.omit() %>%
        spread(key = "year","emissions") %>%
        mutate(sector = "sintering") %>%
        mutate(units = "kt*kt/kt") %>%
        mutate(fuel = "process") %>%
        select(iso, sector, fuel, units, everything())

    # Write out sintering emissions
    writeData( sint_em, "MED_OUT", paste0( "C.", em, "_sintering_emissions"))

} else {print("does not have any user defined input")}

logStop()

#END
