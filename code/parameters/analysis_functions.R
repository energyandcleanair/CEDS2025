# ----------------------------------------------------------------------------------
# CEDS R header file: data analysis functions
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen, Patrick O'Rourke
# Last Updated: July 17, 2020

# This file should be sourced by any R script running diagnostics on CEDS data.
# Functions contained:
#   activityCheck, sectorCheck, fuelCheck, CountryCheck, iso_check, mapCEDS_sector_fuel

# ----------------------------------------------------------------------------------
# activityCheck
# Brief:         Checks whether all the activities in the given dataset are in the
# 		         master list, and whether all activities in the master list are present
#		         in the data.
# Details:       Uses printlog functions to print out detailed results: must be called
#                while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Tyler Pitkanen, Jon Seibert
# Params:
#   x:        data frame to check, containing activity column [required]
#   colname:     name of the column containing activity designations [default: "activity"]
#   check_valid: boolean to control whether the function checks that all present activities
#                are valid [default: TRUE]
#   check_all:   boolean to control whether the function checks if all activities are
#                present [default: TRUE]
# Return:        boolean indicating pass or failure on all checks run, collectively.
# Input Files:   none
# Output Files:  none

activityCheck <- function( x, colname = "activity", check_valid = TRUE, check_all = TRUE ){

    if( nrow(x) == 0 ) return(TRUE)

	activity_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                                sheet_selection = "Sectors", mute = TRUE )

    valid <- TRUE

    sec_col <- x[ , names( x ) == colname ]

    # Check that all activities in set x exist in the master activity list. Ignore blank
    #   spaces / NA values
    if( check_valid ){

        printLog( "Checking activity validity ", cr=F )

        invalid_activities <- list()
        n <- 1
        for( i in 1:length( sec_col ) ) {
            if( sec_col[[ i ]] %!in% activity_list$activity & !is.na( sec_col[[ i ]] ) ) {
                invalid_activities[[ n ]] <- sec_col[[ i ]]
                n <- n + 1
            }
        }
        invalid_activities <- unique( invalid_activities )
        if( length( invalid_activities ) > 0 ) {
            printLog( "...Invalid activities found:", paste( invalid_activities, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All activities valid.", ts=F )
        }
    }

    # Check that all activities in the master list are found in data set x.
    if( check_all ){
        printLog( "Checking for missing activities ", cr=F )

        missing_activities <- list()
        n <- 1
        for( i in 1:length( activity_list$activity ) ) {
            target <- activity_list$activity[[ i ]]
            found <- grep( target, sec_col, fixed=T, value=T )
            if( length( found ) == 0 ) {
                missing_activities[[ n ]] <- target
                n <- n + 1
            }
        }
        missing_activities <- unique( missing_activities )
        missing_activities <- missing_activities[ !is.na( missing_activities ) ]
        if( length( missing_activities ) > 0 ) {
            printLog( "...activities missing:", paste( missing_activities, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All activities present.", ts=F )
        }
    }

    return( valid )
}

# ----------------------------------------------------------------------------------
# sectorCheck
# Brief:         Checks whether all the sectors in the given dataset are in the
# 		         master list, and whether all sectors in the master list are present
#		         in the data.
# Details:       Uses printlog functions to print out detailed results: must be called
#                while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Tyler Pitkanen, Jon Seibert
# Params:
#   x:        data frame to check, containing sector column [required]
#   colname:     name of the column containing sector designations [default: "sector"]
#   check_valid: boolean to control whether the function checks that all present sectors
#                are valid [default: TRUE]
#   check_all:   boolean to control whether the function checks if all sectors are
#                present [default: TRUE]
# Return:        boolean indicating pass or failure on all checks run, collectively.
# Input Files:   none
# Output Files:  none

sectorCheck <- function( x, colname = "sector", check_valid = TRUE, check_all = TRUE ){

	sector_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                              sheet_selection = "Sectors", mute = TRUE )

    valid <- TRUE

    sec_col <- x[ , names( x ) == colname ]

    # Check that all sectors in set x exist in the master sector list. Ignore blank
    #   spaces / NA values
    if( check_valid ){

        printLog( "Checking sector validity ", cr=F )

        invalid_sectors <- list()
        n <- 1
        for( i in 1:length( sec_col ) ) {
            if( sec_col[[ i ]] %!in% sector_list$sector & !is.na( sec_col[[ i ]] ) ) {
                invalid_sectors[[ n ]] <- sec_col[[ i ]]
                n <- n + 1
            }
        }
        invalid_sectors <- unique( invalid_sectors )
        if( length( invalid_sectors ) > 0 ) {
            printLog( "...Invalid sectors found:", paste( invalid_sectors, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All sectors valid.", ts=F )
        }
    }

    # Check that all sectors in the master list are found in data set x.
    if( check_all ){
        printLog( "Checking for missing sectors ", cr=F )

        missing_sectors <- list()
        n <- 1
        for( i in 1:length( sector_list$sector ) ) {
            target <- sector_list$sector[[ i ]]
            found <- grep( target, sec_col, fixed=T, value=T )
            if( length( found ) == 0 ) {
                missing_sectors[[ n ]] <- target
                n <- n + 1
            }
        }
        missing_sectors <- unique( missing_sectors )
        missing_sectors <- missing_sectors[ !is.na( missing_sectors ) ]
        if( length( missing_sectors ) > 0 ) {
            printLog( "...Sectors missing:", paste( missing_sectors, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All sectors present.", ts=F )
        }
    }

    return( valid )
}

# ----------------------------------------------------------------------------------
# duplicateCEDSSectorCheck
# Brief:         Checks that there are no duplicate ceds_sectors in the mapping file.
# Dependencies:  IO_functions.R
# Author(s):     Andrea Mott
# Params:
#   x:        data frame to check, containing sector column [required]
#   colname:     name of the column containing sector designations [default: "sector"]
#   check_valid: boolean to control whether the function checks that all present sectors
#                are valid [default: TRUE]
#   check_all:   boolean to control whether the function checks if all sectors are
#                present [default: TRUE]
# Return:        boolean indicating pass or failure on all checks run, collectively.
# Input Files:   none
# Output Files:  none

duplicateCEDSSectorCheck <- function( scaling_map, check_valid = TRUE, check_all = TRUE ){

    valid <- TRUE

    df <- na.omit(data.frame(vec = scaling_map$ceds_sector))
    df_length <-length(df$vec)
    unique_sectors <- unique(df)
    unique_sectors_length <- length(unique_sectors$vec)

    # duplicated sectors
    df_dup <- df$vec[duplicated(df$vec)]

    # Check for missing sectors and error if there are.
    if (check_valid){
        if (df_length != unique_sectors_length){
            stop("Duplicated CEDS sectors:",paste0(df_dup),". Check mapping file for ", inv_name)
            } else {
                printLog( "...OK. No duplicate CEDS sectors.", ts=F )
            }
    }
    return( valid )
}

# ----------------------------------------------------------------------------------
# OldtoNewCEDSSectors
# Brief:         Replaces old CEDS sectors with new CEDS sectors.
# Dependencies:  IO_functions.R
# Author(s):     Andrea Mott
# Params:        `map` is the original mapping file
# Return:        the modified mapping file
# Input Files:   none
# Output Files:  none
# Notes: The only thing this function assumes is a two-column old_to_new mapping file: search_col, new_sectors.
# When we refer to names(old_to_new)[1] this is the "ceds_sector" for a scaling mapping file.
# And names(old_to_new)[2] is typically the "new_sector"
# search_col gives the names of the column in map to search on

OldtoNewCEDSSectors <- function(map, search_col, inv_sector = NULL) {

    # Input old to new CEDS sectors mapping file
    old_to_new <- readData("MAPPINGS", "old_to_new_sectors.csv")

    # Apply checks before running
    stopifnot(ncol(old_to_new) == 2)
    stopifnot(search_col %in% names(map))

    # Replace sectors
    names(old_to_new)[1] <- search_col # overwrite old_to_new search col name with whatever user specifies
    search_name <- names(old_to_new)[1]
    replace_name <- names(old_to_new)[2]
    map <- left_join(map, old_to_new, by = search_name)
    replace_vals <- map[[replace_name]]
    map[[search_name]][!is.na(replace_vals)] <- map[[replace_name]][!is.na(replace_vals)]
    map <- dplyr::select(map,-replace_name)

    # If inventory sector exists, then assume a scaling mapping file.
    # Remove duplicated inv_sector and replace with NA
    if (inv_sector %in% colnames(map)) {
        map$inv_sector[duplicated(map$inv_sector)] <- NA
    }

    return(map)
}

# ----------------------------------------------------------------------------------
# diagnosticsMappingFile
# Brief : Lists 1) any inventory sectors from modE that are not mapped to scaling sectors.
#               2) any inventory sectors that are not mapped to scaling sectors.
#               3) any CEDS sectors that are not mapped to scaling sectors.
# Params: scaling_map (mapping file)
#         inv_data_full (module E inventory)
# Author(s):     Andrea Mott, Noah Prime

diagnosticsMappingFile  <- function( scaling_map, inv_data_full){

    # write out inventory sectors not included in mapping file
    inv <- inv_data_full
    inv$inv_sector <- inv$sector
    inv_sectors_not_in_mapping_file <- anti_join(inv, scaling_map, by = "inv_sector") %>%
        select(inv_sector) %>%
        mutate( inv_sector = ifelse( inv_sector == 'NA', NA, inv_sector ) ) %>%
        na.omit()
    if( dim( inv_sectors_not_in_mapping_file )[1] > 0 ){
        inv_sectors_not_in_mapping_file <- inv_sectors_not_in_mapping_file %>%
            mutate( description = 'inventory sectors not included in mapping file' ) %>%
            mutate(sector_origin = "inventory") %>%
            dplyr::rename(sector = inv_sector) %>%
            select(sector_origin, sector, description)
    }

    # write out inventory sectors that are not mapped to scalings sectors
    inv_sectors_not_scaled <- scaling_map %>%
        filter(is.na(scaling_sector)) %>%
        select(inv_sector) %>%
        na.omit()
    if( dim( inv_sectors_not_scaled )[1] > 0 ){
        inv_sectors_not_scaled <- inv_sectors_not_scaled %>%
            mutate(description = "inventory sectors not mapped to scalings sectors") %>%
            dplyr::rename(sector = inv_sector) %>%
            mutate(sector_origin = "inventory") %>%
            select(sector_origin, sector, description)
    }

    # write out any CEDS sectors that are not mapped to scaling sectors
    ceds_sectors_not_scaled <- scaling_map %>%
        filter(is.na(scaling_sector)) %>%
        select(ceds_sector) %>%
        na.omit()
    if( dim( ceds_sectors_not_scaled )[1] > 0 ){
        ceds_sectors_not_scaled <- ceds_sectors_not_scaled %>%
            mutate(description = "CEDS sectors not mapped to scaling sectors") %>%
            mutate(sector_origin = "CEDS") %>%
            dplyr::rename(sector = ceds_sector) %>%
            select(sector_origin, sector, description)
    }

    # combine all diagnostics into one df
    all_diagnostics <- rbind(ceds_sectors_not_scaled, inv_sectors_not_in_mapping_file, inv_sectors_not_scaled)

    if(nrow(all_diagnostics) > 1){
        writeData(all_diagnostics,'DIAG_OUT', paste0('F.',em,'_',inv_name,'_mapping_file_diagnostics'),meta = FALSE)
    }
}

# ---------------------------------------------------------------------------------
# fuelCheck
# Brief:         Checks whether all the fuels in the given dataset are in the
# 		            master list, and whether all fuels in the master list are present in
#	  	            the data.
# Details:       Uses printlog functions to print out detailed results: must be called
#                   while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Tyler Pitkanen, Jon Seibert
# Params:
#   x:           Data frame to check, containing fuel column [required]
# Return:        Boolean indicating pass or failure on all checks run, collectively.
# Input Files:   none
# Output Files:  none

fuelCheck <- function( x ){

	fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                            sheet_selection = "Fuels", mute = TRUE )

    valid <- TRUE

# Check that all fuels in set x exist in the master fuel list. Ignore blank
#   spaces / NA values
	printLog( "Checking fuel validity ", cr=F )

    invalid_fuels <- list()
    n <- 1
	for( i in 1:length( x$fuel ) ) {
		if( x$fuel[[i]] %!in% fuel_list$fuel & !is.na( x$fuel[[i]] ) ) {
			invalid_fuels[[n]] <- x$fuel[[i]]
            n <- n + 1
		}
	}
    invalid_fuels <- unique( invalid_fuels )
    if( length( invalid_fuels ) > 0 ) {
        valid <- FALSE
        printLog( "...Invalid fuels found:", paste( invalid_fuels,
            collapse=', ' ), ts=F )
    } else {
        printLog( "...OK. All fuels valid.", ts=F )
    }

# Check that all fuels in the master list are found in data set x.
	printLog( "Checking for missing fuels ", cr=F )

	missing <- 0
    missing_fuels <- list()
    n <- 1
	for( i in 1:length( fuel_list$fuel ) ) {
		target <- fuel_list$fuel[[i]]
        found <- grep( target, x$fuel, fixed=T, value=T )
		if( length( found ) == 0 ) {
			missing_fuels[[n]] <- target
			n <- n + 1
		}
	}
    missing_fuels <- unique( missing_fuels )
    missing_fuels <- missing_fuels[ !is.na( missing_fuels ) ]
    if( length( missing_fuels ) > 0 ) {
        valid <- FALSE
		printLog( "...Fuels missing:", paste( missing_fuels,
        collapse=', ' ), ts=F )
	} else {
        printLog( "...OK. All fuels present.", ts=F )
    }

    return( valid )
}


# ---------------------------------------------------------------------------------
# countryCheck
# Brief:         Checks whether all the countries in the given dataset are in the master list,
# 		            and whether all countries in the master list are present in the data.
# Details:       Uses printlog functions to print out detailed results: must be called
#                   while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Tyler Pitkanen
# Params:
#   data:        Data frame to check, containing iso column [required]
#   cols:
# Return:        Boolean indicating pass or failure on all checks run, collectively.
# Input Files:   none
# Output Files:  none

countryCheck <- function( data, cols = 1, convention = "ISO" ) {
# Generally, there will be one region column per data set and it will be the first
#   column in that data set, so cols = 1 by default.
# Naming convention can be specified with the convention argument set as "ISO",
#   "IEA", "IEA_Fert", or "BP" (not case-sensitive).

    valid <- TRUE

	country_list <- readData( "MAPPINGS", "Master_Country_List", meta=F, mute=T )

    conv_list    <- c( "ISO", "IEA", "IEA_Fert", "BP" )
    conv_col_num <- match( convention, conv_list )

# Choose sections of reference country list to search based on user input
    if ( is.na( conv_col_num ) == T ) {  # if convention is invalid or unspecified
        ref_names <- unlist( country_list )
        valid <- FALSE
        printLog( "Country check failed. Specify naming convention." )
    } else {  # if a valid convention is specified
        ref_names <- country_list[ , conv_col_num ]
    }

    printLog( "Checking col(s)", cols, "for", convention, "names", cr=F )

# Check for name matching between data and reference section
    unmatched_names <- list()
    n = 1
    for ( i in 1:length( data[ , cols ] ) ) {
        check <- grep( data[ i, cols ], ref_names, value = T, fixed = T )
        if ( length( check ) == 0 ) {
            unmatched_names[n] <- data[ i, cols ]
            n = n + 1
        }
    }

# Print output to log
    if ( length( unmatched_names ) > 0 ) {
        valid <- FALSE
        unmatched_string <- paste( unmatched_names, collapse = ", " )
        printLog( "...Invalid names found for ",
            convention, " naming: ", paste( unmatched_string, collapse = ', ' ), ts=F )
    } else {
        printLog( "...OK. All countries valid.", ts=F )
    }

    return( valid )
}

# ----------------------------------------------------------------------------------
# iso_check
# Brief:         Checks whether the given dataset's isos are all CEDS isos
#                (within Master_Country_List.csv), (optionally) whether all
#                CEDS isos are within the given dataset, and (optionally)
#                whether the given dataset has any duplicated CEDS isos. CEDS isos are further
#                defined as srb (kosovo), gum, and those which have a final_data_flag value of 1.
# Details:       Uses printlog functions to print out results: must be called while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Patrick O'Rourke
# Params:
#   data_to_check:                           the dataset being checked for CEDS isos [required as class -data.frame-]
#   data_to_check_iso_colname:               the name of the column containing CEDS isos in data_to_check
#                                            [required as class -character-]
#   provided_data_needs_all_ceds_isos:       a boolean indicating whether or not data_to_check needs to contain all
#                                            CEDS isos [default: T]
#   provided_data_contains_unique_isos_only: a boolean indicating whether or not to check data_to_check for duplicated
#                                            CEDS isos [default: T]
# Return:        A log message notifying the user that the dataset passed the iso check (or why it failed).
# Input Files:   Master_Country_List.csv
# Output Files:  None
# Usage examples: iso_check( data_frame, "iso_col" )
#                 iso_check( data_frame, "iso.col", provided_data_needs_all_ceds_isos = F )
#                 iso_check( data_frame, "iso col", provided_data_contains_unique_isos_only = F)
#                 iso_check( data_frame, "iso", provided_data_needs_all_ceds_isos = F
#                            provided_data_contains_unique_isos_only = F)
# TODO: option to not count NAs towards passing the check on whether all CEDS isos are within the provided
#       data's isos
# TODO: should we include the global iso optionally?
iso_check <- function(data_to_check, data_to_check_iso_colname,
                      provided_data_needs_all_ceds_isos = T,
                      provided_data_contains_unique_isos_only = T) {

#   Check if data_to_check was provided as class -data.frame-
  if ( is.data.frame( data_to_check ) == FALSE ) {

    stop (  paste0 ("The function iso_check requires the parameter data_to_check to be class -data.frame- ." ) )

  } else {

#       Check if data_to_check_iso_colname is a column name in data_to_check
        if ( ( data_to_check_iso_colname %in% colnames( data_to_check ) ) == FALSE ) {

            stop (  paste0 ("The function iso_check requires the parameter data_to_check_iso_colname ",
                      "to be a column name of the parameter data_to_check." ) )

        } else {

#           Check if data_to_check_iso_colname is class -character- (in quotes)
            if ( is.character ( data_to_check_iso_colname ) == FALSE ) {

                stop (  paste0 ("The function iso_check expects the parameter data_to_check_iso_colname",
                        " to be class -character- ." ) )

            } else {

#               Initial data processing - select iso columns
                MCL <- readData( "MAPPINGS", "Master_Country_List" )

                MCL_countries <- MCL %>%
                  dplyr::filter( final_data_flag == 1  | iso %in% c( "srb (kosovo)", "gum" ) ) %>%
                  dplyr::select( iso ) %>%
                  dplyr::distinct( )

               data_to_check_countries <- data_to_check %>%
                  dplyr::select( data_to_check_iso_colname )

#               Check that data_to_check doesn't have duplicated CEDS isos. This is only necessary when
#               provided_data_contains_unique_isos_only = T, as this states that the user only wants unique isos.

                if ( provided_data_contains_unique_isos_only == T ){

                    printLog ( paste0 ( "Checking if the provided data has any duplicated CEDS isos..." ) )

#                   Remove NAs for isos, as NAs are not CEDS isos, and will get caught later when checking if data contains all CEDS isos
                    data_to_check_countries_noNA <- dplyr::filter(data_to_check_countries,
                                                        ! (is.na ( data_to_check_countries[ , data_to_check_iso_colname] ) ) )

                    data_to_check_unique_isos_noNA <- list( sort ( unique( data_to_check_countries_noNA[, data_to_check_iso_colname ] ) ) )

                    if ( nrow ( data_to_check_countries_noNA) !=  length ( data_to_check_unique_isos_noNA[[1]] ) ){

                    duplicated_isos <- data_to_check_countries %>%
                      count( data_to_check_iso_colname ) %>%
                      dplyr::filter( freq > 1)

                    printLog ( "Duplicated CEDS isos:", duplicated_isos[[1]] )

                    stop (  paste0 ("The provided data has duplicated CEDS isos. If this is okay,",
                                    " then set provided_data_contains_unique_isos_only to F .",
                                    " If this is not expected, then double check the provided data",
                                    " for the isos listed above.") )

                    } else {

                      printLog ( paste0 ( "The provided data does not have duplicated CEDS isos." ) )

                    }

              } else if ( provided_data_contains_unique_isos_only == F ) {

                data_to_check_countries <- data_to_check_countries

              } else {

                  stop (  paste0 ("The function iso_to check expects the parameter provided_data_contains_unique_isos_only",
                          " to be set to T or F ." ) )

              }

#               Check that all of the isos from the provided data are within the MCL
                printLog ( paste0 ( "Checking if all of the provided data's isos are CEDS isos..." ) )

                data_to_check_countries_not_in_MCL <- dplyr::filter(data_to_check_countries,
                                                      ! ( data_to_check_countries[ , data_to_check_iso_colname ] %in% MCL_countries$iso ) )

                data_to_check_countries_not_in_MCL_list <- list( sort ( unique( data_to_check_countries_not_in_MCL[, data_to_check_iso_colname ] ) ) )

                if ( length ( data_to_check_countries_not_in_MCL_list[[1]] ) > 0 ){

                  printLog ( "Non-CEDS isos:", data_to_check_countries_not_in_MCL_list[[1]] )

                  stop (  paste ("The above countries in the provided data are not contained within",
                                 "CEDS Master_Country_List.csv (or MCL, found in the input/mappings directory)." ) )

                } else {

                  printLog ( paste0 ( "Check 1 completed and passed - All countries from the provided",
                                      " data are CEDS isos defined in Master_Country_List.csv ." ) )
                }

#       Check that all CEDS isos are within the provided data
        printLog ( paste0 ( "Checking if all CEDS isos are within the provided data's isos..." ) )

        MCL_countries_not_in_provided_data <- MCL_countries %>%
          dplyr::filter( ! ( iso %in% data_to_check_countries[, data_to_check_iso_colname ] ) )

        MCL_countries_not_in_provided_data_list <- list( sort ( unique( MCL_countries_not_in_provided_data$iso) ) )

        if ( length ( MCL_countries_not_in_provided_data_list[[1]] ) == 0 ) {

          printLog ( paste0 ( "Check 2 completed and passed - All countries from CEDS Master_Country_List.csv",
                              " are within the provided data." ) )

        } else if ( length ( MCL_countries_not_in_provided_data_list[[1]] ) > 0 ) {

          printLog ( "The following isos are missing:", MCL_countries_not_in_provided_data_list[[1]] )

          if ( provided_data_needs_all_ceds_isos == T) {

            stop (  paste0 ("The above countries from CEDS Master_Country_List.csv (or MCL, ",
                            "found in the input/mappings directory) are not contained within the provided data." ) )

          } else if ( provided_data_needs_all_ceds_isos == F ) {

            warning (  paste0 ("The above countries from CEDS Master_Country_List.csv (or MCL, ",
                               "found in the input/mappings directory) are not contained within the provided data." ) )

          } else {

            stop (  paste0 ("Parameter provided_data_needs_all_ceds_isos in function iso_check must be set to",
                            "T or F ." ) )

          }

        }

      }

    }

  }

}

# ----------------------------------------------------------------------------------
# EDGARcheck
# Brief:         Checks whether all the sectors in an EDGAR dataset are in the
# 		         master list, and whether all sectors in the master list are present
#		         in the data.
# Details:       Uses printlog functions to print out detailed results: must be called
#                while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Jon Seibert
# Params:
#   x:           data frame to check, containing edgar_sector column [required]
#   colname:     name of the column containing sector designations [default: "edgar_sector"]
#   check_valid: boolean to control whether the function checks that all present sectors
#                are valid [default: TRUE]
#   check_all:   boolean to control whether the function checks if all sectors are
#                present [default: TRUE]
# Return:        boolean indicating pass or failure on all checks run, collectively.
# Input Files:   none
# Output Files:  none

EDGARcheck <- function( x, colname = "edgar_sector", check_valid = TRUE, check_all = TRUE ){

	sector_list <- readData( "MAPPINGS", "Master_EDGAR_sector_mapping", mute = TRUE )

    valid <- TRUE

    sec_col <- x[ , names( x ) == colname ]

    # Check that all sectors in set x exist in the master sector list. Ignore blank
    #   spaces / NA values
    if( check_valid ){

        printLog( "Checking EDGAR sector validity ", cr=F )

        invalid_sectors <- list()
        n <- 1
        for( i in 1:length( sec_col ) ) {
            if( sec_col[[ i ]] %!in% sector_list$edgar_sector & !is.na( sec_col[[ i ]] ) ) {
                invalid_sectors[[ n ]] <- sec_col[[ i ]]
                n <- n + 1
            }
        }
        invalid_sectors <- unique( invalid_sectors )
        if( length( invalid_sectors ) > 0 ) {
            printLog( "...Invalid sectors found:", paste( invalid_sectors, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All sectors valid.", ts=F )
        }
    }

    # Check that all sectors in the master list are found in data set x.
    if( check_all ){
        printLog( "Checking for missing EDGAR sectors ", cr=F )

        missing_sectors <- list()
        n <- 1
        for( i in 1:length( sector_list$edgar_sector ) ) {
            target <- sector_list$edgar_sector[[ i ]]
            found <- grep( target, sec_col, fixed=T, value=T )
            if( length( found ) == 0 ) {
                missing_sectors[[ n ]] <- target
                n <- n + 1
            }
        }
        missing_sectors <- unique( missing_sectors )
        missing_sectors <- missing_sectors[ !is.na( missing_sectors ) ]
        if( length( missing_sectors ) > 0 ) {
            printLog( "...EDGAR ectors missing:", paste( missing_sectors, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All EDGAR sectors present.", ts=F )
        }
    }

    return( valid )
}

# ---------------------------------------------------------------------------------

# mapCEDS_sector_fuel
# Brief:        Map to CEDS sectors and/or fuels
# Details:      Map any data to CEDS sectors (sectors and fuels are seperate in original
#               data and thus map sererately in two steps) or to CEDS sectors and fuels
#               (original data notes a sector and a fuel and must map in one step, such as IEA FLOWS)
# Dependencies: None
# Author(s):    Rachel Hoesly
# Params:
#       mapping_data:   original data (not in CEDS format)
#       mapping_file:   mapping file specific to original data
#       data_match_col: name(s) of the matching columns in the data file (by.x) ex: c('sector')
#       map_match_col:  name(s) of the matching data label column in the mapping file (by.y) ex: c('data_sector')
#       map_merge_col:  name of the column in the mapping file to merge with original data: ex: ('ceds_sector')
#       new_col_names:  name of the merged column (ceds sector or fuel) in the output c('sector')
#       level_map_in:   aggregation of the scaling map. Should always map to detailed when possible
#                       possible choices:'working_sectors_v1', 'working_sectors_v2', 'detailed_sectors'
#       level_out:      aggregation of the output file
#                       possible choices:'working_sectors_v1', 'working_sectors_v2', 'detailed_sectors'
#       aggregate:      boolean T/F, aggregate data by output sectors/fuels?
#       aggregate_col:  col or columns to aggregate data over, usually 'years' or vector of column names of data
#       oneToOne:       does the data map 1:1 with the mapping file. Now, The function only works for data used for emission factors, where
#                       there is no danger of double counting emissions.
#       agg.fun:        function used to aggergate (default is sum)
# Examples: used in B1.1.base_comb_GAINS_EMF-30.R
#               mapCEDS_sector_fuel(mapping_data = emissions_ceds,
#                                   mapping_file = fuel_sector_map,
#                                   data_match_col = 'Sector',
#                                   map_match_col = 'emf_sector',
#                                   map_merge_col = c('ceds_sector', 'ceds_fuel'),
#                                   new_col_names = c('sector', 'fuel'),
#                                   level_map_in = 'working_sectors_v1',
#                                   level_out = 'working_sectors_v1',
#                                   aggregate = TRUE,
#                                   aggregate_col = years,
#                                   oneToOne = FALSE,
#                                   agg.fun = sum )
#           used in B1.2.add_SO2_GAINS_AshRet.R
#                 mapCEDS_sector_fuel( mapping_data = gainsash_ret,
#                                      mapping_file = sectormap,
#                                      data_match_col = 'sector',
#                                      map_match_col = 'GAINS.sectors',
#                                      map_merge_col = c('detailed_sectors'),
#                                      new_col_names = c('sector'),
#                                      level_map_in = 'detailed_sectors',
#                                      level_out = 'working_sectors_v1',
#                                      aggregate = TRUE,
#                                      aggregate_col = c('X2005'),
#                                      oneToOne = FALSE,
#                                      agg.fun = mean)
#
# Return:       mapped data to ceds sectors and fuels and designated level
# Input Files:  Master_Sector_Level_map.csv
# Output Files:
# TODO: some of this processing would benefit from using tidyverse packages

mapCEDS_sector_fuel <- function( mapping_data,
                                 mapping_file,
                                 data_match_col,
                                 map_match_col,
                                 map_merge_col,
                                 new_col_names = NA,
                                 level_map_in,
                                 level_out = 'working_sectors_v1',
                                 aggregate = TRUE,
                                 aggregate_col = NA,
                                 oneToOne,
                                 agg.fun = NA ){

  ceds_sector_map <- readData( 'MAPPINGS','Master_Sector_Level_map' )

  #Check Input options
  # Match columns exist in input files
  # Valid method
  # Valid level
  # Valid data cols provided for aggregate==TRUE
  if (aggregate){
    if (anyNA( aggregate_col )) stop('in mapCEDS_sector_fuel, for aggregate = TRUE, must provide
                                        valid numeric data columns to aggreate over. Check argument "data_col" ')
    }
  # Noted level matches map
  # Extra sectors in map
  # Unmapped sectors
  # data_match_col in mapping_data
  if ( all(data_match_col %!in% names(mapping_data))) stop('data_match_col not in in mapping_data')
  # map_match_col
  if ( all(map_match_col %!in% names(mapping_file))) stop('map_match_col not in in mapping_file')
  # map_merge_col
  if ( all(map_merge_col %!in% names(mapping_file))) stop('map_merge_col not in in mapping_file')
  # Levels
  if ( level_map_in %!in% names(ceds_sector_map)) stop('level_map_in not in ceds_sector_map')
  if ( level_out %!in% names(ceds_sector_map)) stop('level_out not in ceds_sector_map')



  if(oneToOne ){stop("mapCEDS_sector_fuel does not have oneToOne functionality yet")}

  out <- merge(x=mapping_data, y=mapping_file,
               by.x = data_match_col,
               by.y = map_match_col,
               all = TRUE)
  # Remove unmatched rows
  # here to write out unmapped values
  out <- out[complete.cases(out[,map_merge_col]),]

  if( aggregate){
    # Aggregate
    mapping_data_names <- names(mapping_data)
    new_mapping_data_names <- mapping_data_names[which(mapping_data_names %!in% c(aggregate_col,data_match_col))]
    by_cols <- c(map_merge_col,new_mapping_data_names)

    by.list <- list()
    for(i in seq_along(by_cols)) by.list[[i]] <- out[,by_cols[i]]
    names(by.list) <- by_cols
    out <- aggregate( out[,aggregate_col],
                      by = by.list,
                      FUN = agg.fun, na.rm=TRUE)
    names(out)[which(names(out)=='x')] <- aggregate_col
  }

  # Rename columns
  if(!anyNA( new_col_names )){
    names(out)[which(names(out) %in% map_merge_col)] <- new_col_names }

  # Aggregate to Working Level if noted
  if ( level_map_in != level_out){
    out <- merge(x=out, y=ceds_sector_map[,c(level_map_in,level_out)],
                 by.x = new_col_names,
                 by.y = level_map_in,
                 all = TRUE)
    # Remove unmatched rows
    # here to write out unmapped values
    out <- out[complete.cases(out[,level_out]),]

    if( aggregate){
      # Aggregate
      out_data_names <- names(out)
      by_cols <- out_data_names[which(out_data_names %!in% c(aggregate_col,data_match_col))]
      by.list <- list()
      for(i in seq_along(by_cols)) by.list[[i]] <- out[,by_cols[i]]
      names(by.list) <- by_cols
      out <- aggregate( out[,aggregate_col],
                        by = by.list,
                        FUN = agg.fun, na.rm=TRUE)
      names(out)[which(names(out)=='x')] <- aggregate_col
    }
    if(!is.na(new_col_names)){
      names(out)[which(names(out) == level_out)] <- new_col_names }
  }

  return(out)
  }

