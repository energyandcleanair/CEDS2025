# ------------------------------------------------------------------------------
# Program Name: Create_Master_Val_Metadata_Heatmap.R
# Author: Ben Goldstein
# Date Last Updated: 26 June 2017
# Program Purpose: Uses the F.create_EF_value_meta_heatmap to create a heatmap
#                  diagnostic of the value metadata for a single country.
# Input Files: F.[em]_scaled_EF-value_metadata.csv
#
# Output Files: A figure in the diagnostic-output
# TODO: Update to include all inventory updates in strings below (for instance,
#       MEIC is not listed, EDGAR still says v4.2)
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R",'common_data.R',
                  'IO_functions.R', 'emissions_scaling_functions.R') # Additional function files may be required.
    log_msg <- "Create value metadata heatmap" # First message to be printed to the log
    script_name <- "Create_Master_Val_Metadata_Heatmap.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO"

# ---------------------------------------------------------------------------
# 0.5 Define functions

# Brief: extracts and returns the legend from a ggplot graph
    g_legend <- function( a.gplot ) {
        tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
        leg <- which( sapply( tmp$grobs, function(x) x$name ) == "guide-box" )
        legend <- tmp$grobs[[leg]]
        return( legend )
    }

# ------------------------------------------------------------------------------
# createSinglePlot
# Brief: creates a stacked bar chart of ef scaling inventories for a specific
#        region or sector
# Dependencies: None
# Author: Ben Goldstein
# parameters:
#     identifier: the name of the agg sector or region to be plotted
#     meta_classified: the value metadata (reclassified) to be plotted
#     id_type: does the identifier refer to a region or an agg sector?
#     inventory_colors: the manually-determined list of colors for each inventory
# return: a ggplot of the bar chart
# input files: reclassified value metadata
# output: none
    createSinglePlot <- function( identifier, meta_classified, id_type = "Region", inventory_colors,
                                  weight_by_em, normalize, to_group = T ) {

        factor_levels <- c( "Default",
                  "Zero emissions",
                  "EDGAR 4.3-PEGASOS",
                  "EMEP_NFR09",
                  "REAS 2.1",
                  "EMEP_NFR14",
                  "UNFCCC, 2015",
                  "Environment Canada, 2013",
                  "Environment and Climate Change Canada, 2016",
                  "US EPA, 2016",
                  "US",
                  "Li et al., 2017",
                  "TEPA, 2016",
                  "Argentina UNFCCC submission, 2016",
                  "Kurokawa et. al, 2013",
                  "South Korea National Institute of Environmental Research, 2016",
                  "Australian Department of the Environment, 2016",
                  "EDGAR 4.2" )


    # Separate extraction procedure by region vs. sector.
        if ( id_type == "Region" ) {

        # Extract metadata for this region
            meta_this_region <- meta_classified[ which(meta_classified$Region == identifier ),
                                                 c( "year", "value", "prepost", "emissions" ) ]

        # Count the frequency of each year/value/prepost occurance
            regional_counts <- meta_this_region %>%
                                    dplyr::count( year, value, prepost )

        # Make the years numeric so we can have a continunous x-axis

            id_cols <- c("year","value","prepost")

        # If we want our values to be adjusted by weight (emissions):
            if (weight_by_em) {
            # Aggregate and sum emissions by the desired categories
                regional_counts <- aggregate( meta_this_region$emissions, by = meta_this_region[id_cols], sum )
                colnames(regional_counts)[which(colnames(regional_counts) == "x")] <- "n"
                regional_counts$year <- substr( regional_counts$year, 2, 5 ) %>%
                                      as.numeric()
            # If data is to be normalize
                if (normalize) {
                    year_totals <- aggregate(regional_counts$n, by = regional_counts["year"], sum)
                    regional_counts <- left_join( regional_counts, year_totals, by = c("year"))
                    regional_counts$n <- regional_counts$n / regional_counts$x
                }

                regional_counts_all <- expand.grid( unique( regional_counts$value ),
                                                    unique( regional_counts$prepost ),
                                                    1971:2014 )
                colnames(regional_counts_all) <- c("value", "prepost", "year")

                regional_counts <- merge( regional_counts_all, regional_counts,
                                              by = c("year", "value", "prepost"), all.x = T) %>%
                                   dplyr::arrange(value)

                regional_counts$n[ is.na( regional_counts$n ) ] <- 0

                regional_counts$n <- regional_counts$n / 1000 # Convert to Tg

                regional_counts$value <- factor( regional_counts$value,
                                         levels = factor_levels )

        # Make a geom_col plot of frequency (n, determined in count() above) by year.
        # Fill is determined by inventory (as stated in the colors list) and
        #   transparency is determined by extension direction
            p <- ggplot( regional_counts, aes( year, n ) ) +
              geom_area( aes( fill = value, alpha = prepost ),
                        position = 'stack' ) +
              theme( legend.position = "none" ) +
              scale_fill_manual( values = inventory_colors ) +
              ylab(paste0( em, " emissions [Tg]" ) ) +
              theme( axis.ticks.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.grid.minor = element_line( colour="gray95", size = 0.2 ),
                     panel.grid.major = element_line( colour="gray88", size = 0.2 ),
                     plot.title = element_text( hjust = 0.5 ),
                     panel.background = element_blank(),
                     panel.border = element_rect( colour = "grey80",
                                                  fill = NA, size = .8 ) ) +
              ggtitle( identifier ) +
              scale_alpha_discrete( range = c( 0.85, 0.4 ) ) +
              scale_x_continuous( breaks = c( 1970, 1990, 2010 ) ) + # TODO: Make this year breaks more flexible
              theme( text = element_text( size = 6 ) )

              if (!to_group) p <- p + theme( legend.position = "right",
                                             legend.key.size = unit( 5, "point" ),
                                             legend.text = element_text( size = 5 ),
                                             text = element_text( size = 10 ) ) +
                                      ggtitle( paste0( "Emissions per year scaled by each inventory\nfor emissions species ",
                                                       em, ", region ", identifier ) )

              if (normalize) p <- p + ylab( "% of global emissions" )

              return( p )

            }

            regional_counts$year <- substr( regional_counts$year, 2, 5 ) %>%
                                      as.numeric()

            regional_counts$value <- factor( regional_counts$value,
                                 levels = factor_levels )

        # Make a geom_col plot of frequency (n, determined in count() above) by year.
        # Fill is determined by inventory (as stated in the colors list) and
        #   transparency is determined by extension direction
            p <- ggplot( regional_counts, aes( year, n ) ) +
                 geom_col( aes( fill = value, alpha = prepost ),
                           position = 'stack', width = 1 ) +
                 theme( legend.position = "none" ) +
                 scale_fill_manual( values = inventory_colors ) +
                 ylab("% total emissions") +
                 theme( axis.ticks.y = element_blank(),
                        axis.title.x = element_blank(),
                        panel.background = element_blank(),
                        plot.title = element_text( hjust = 0.5 ),
                        panel.grid.minor = element_line(colour="gray95", size = 0.2 ),
                        panel.grid.major = element_line(colour="gray88", size = 0.2 ),
                        panel.border = element_rect( colour = "grey80",
                                                     fill = NA, size = .8 ) ) +
                 ggtitle( identifier ) +
                 scale_alpha_discrete( range = c( 0.85, 0.4 ) ) +
                 theme( text = element_text( size = 6 ) ) +
                 scale_x_continuous(breaks = c(1970, 1990, 2010)) # TODO: Make this year breaks more flexible

        } else if ( id_type == "Sector" ) {
        # Extract metadata for this sector
            meta_this_sector <- meta_classified[ which( meta_classified$Figure_sector == identifier ),
                                                 c( "year", "value", "prepost", "emissions" ) ]

        # Count the frequency of each year/value/prepost occurance
            sectoral_counts <- meta_this_sector %>%
                                dplyr::count( year, value, prepost )

        # A list of column names that will be used for grouping and weighting
            id_cols <- c("year","value","prepost")
        # Make the years numeric so we can have a continunous x-axis
            sectoral_counts$year <- substr( sectoral_counts$year, 2, 5 ) %>%
                                      as.numeric()

        # If we want our values to be adjusted by weight (emissions):
            if (weight_by_em) {
            # Aggregate and sum emissions by the desired categories
                sectoral_counts <- aggregate( meta_this_sector$emissions, by = meta_this_sector[id_cols], sum )
                colnames(sectoral_counts)[which(colnames(sectoral_counts) == "x")] <- "n"
                sectoral_counts$year <- substr( sectoral_counts$year, 2, 5 ) %>%
                                      as.numeric()
            # If data is to be normalize
                if (normalize) {
                    year_totals <- aggregate(sectoral_counts$n, by = sectoral_counts["year"], sum)
                    sectoral_counts <- left_join( sectoral_counts, year_totals, by = c("year"))
                    sectoral_counts$n <- sectoral_counts$n / sectoral_counts$x
                }

                sectoral_counts_all <- expand.grid( unique( sectoral_counts$value ),
                                                    unique( sectoral_counts$prepost ),
                                                    1971:2014 ) # TODO: Use year objects
                colnames(sectoral_counts_all) <- c("value", "prepost", "year")

                sectoral_counts <- merge( sectoral_counts_all, sectoral_counts,
                                              by = c("year", "value", "prepost"), all.x = T) %>%
                                   dplyr::arrange(value)

                sectoral_counts$n[ is.na( sectoral_counts$n ) ] <- 0

                sectoral_counts$n <- sectoral_counts$n / 1000 # Convert to Tg

                sectoral_counts$value <- factor( sectoral_counts$value,
                                         levels = factor_levels )

        # Make a geom_col plot of frequency (n, determined in count() above) by year.
        # Fill is determined by inventory (as stated in the colors list) and
        #   transparency is determined by extension direction
            p <- ggplot( sectoral_counts, aes( year, n ) ) +
              geom_area( aes( fill = value, alpha = prepost ),
                        position = 'stack' ) +
              theme( legend.position = "none" ) +
              scale_fill_manual( values = inventory_colors ) +
              ylab(paste0( em, " emissions [Tg]" ) ) +
              theme( axis.ticks.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.grid.minor = element_line(colour="gray95", size = 0.2 ),
                     panel.grid.major = element_line(colour="gray88", size = 0.2 ),
                     plot.title = element_text(hjust = 0.5),
                     panel.background = element_blank(),
                     panel.border = element_rect( colour = "grey80",
                                                  fill = NA, size = .8 ) ) +
              ggtitle( identifier ) +
              scale_alpha_discrete( range = c( 0.85, 0.4 ) ) +
              scale_x_continuous(breaks = c(1970, 1990, 2010)) + # TODO: Make this year breaks more flexible
              theme( text = element_text( size = 6 ) )

              if (!to_group) p <- p + ggtitle( paste0( "Emissions per year scaled by each inventory\nfor emissions species ",
                                                       em, ", sector ", identifier ) ) +
                                      theme( legend.position = "right",
                                             legend.key.size = unit( 5, "point" ),
                                             legend.text = element_text(size=5),
                                             text = element_text( size = 10 ))
              if (normalize) p <- p + ylab("% of global emissions")

              return( p )

            }


        sectoral_counts$value <- factor( sectoral_counts$value,
                                         levels = factor_levels )

        # Make a geom_col plot of frequency (n, determined in count() above) by year.
        # Fill is determined by inventory (as stated in the colors list) and
        #   transparency is determined by extension direction
            p <- ggplot( sectoral_counts, aes( year, n ) ) +
              geom_col( aes( fill = value, alpha = prepost ),
                        position = 'stack', width = 1 ) +
              theme( legend.position = "none" ) +
              scale_fill_manual( values = inventory_colors ) +
              ylab("% total emissions") +
              theme( axis.ticks.y = element_blank(),
                     axis.title.x = element_blank(),
                     panel.grid.minor = element_line(colour="gray95", size = 0.2 ),
                     panel.grid.major = element_line(colour="gray88", size = 0.2 ),
                     plot.title = element_text(hjust = 0.5),
                     panel.background = element_blank(),
                     panel.border = element_rect( colour = "grey80",
                                                  fill = NA, size = .8 ) ) +
              ggtitle( identifier ) +
              scale_alpha_discrete( range = c( 0.85, 0.4 ) ) +
              scale_x_continuous(breaks = c(1970, 1990, 2010)) +  # TODO: Make this year breaks more flexible
              theme( text = element_text( size = 6 ) )

        }

      # Return the created plot
      return( p )

    }

# ------------------------------------------------------------------------------
# createMasterValMetaHeatmap
# Brief: creates a single image of scaling inventory percent breakdowns by
#        aggregate sector or region
# Dependencies: None
# Author: Ben Goldstein
# parameters:
#     meta_notes: the properly formatted, long-form value_metadata
#     country_map: the Master_Country_List
#     sector_map: the Master_Sector_Level_Map
#     map_by: Should the data be aggregated by agg sector or region?
# return: reclassified metadata by inventory
# input files: value metadata
# output: ("Inventory scaling percentages of ", em, " by ", map_by).png

    createMasterValMetaHeatmap <- function( meta_notes, country_map, sector_map, map_by = "Sector",
                                            weight_by_em = T, normalize = weight_by_em) {

    # Map the value_metadata notes to region and sector. Collect a list of
    #   all unique regions and sectors present in the data.
        mapped_meta_notes <- left_join( meta_notes, country_map[ , c( 'iso', 'Region' ) ] )
        all_regions <- unique( mapped_meta_notes$Region )
        all_regions <- all_regions[ which( all_regions != "Global" ) ]

        sector_map <- sector_map[ , c( 'working_sectors_v1', 'Figure_sector' ) ] # Rename for col comparison
        colnames( sector_map )[1] <- "sector"

        mapped_meta_notes <- left_join( mapped_meta_notes, sector_map )
        all_sectors <- unique( mapped_meta_notes$Figure_sector )
        all_sectors <- all_sectors[ !is.na( all_sectors ) ]

    # Hand the data off to a new df for editing content
        meta_split <- mapped_meta_notes

    # remove the semicolon from the end of all non-default entries
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment <- as.character( meta_split$comment )
        meta_split$comment[ indices ] <- substr( meta_split$comment[ indices ], 0,
                                                 nchar( meta_split$comment[ indices ] ) - 2 )

    # Remove sectors that aren't actually present in CEDS
        sectors_to_remove <- c( "11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural" )
        meta_split <- meta_split[ which( meta_split$sector %!in% sectors_to_remove), ]

    # Discard all value metadata notes that occur before the final semicolon,
    #   now that line-end semicolons have been removed
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment[ indices ]  <- sub( ".*; ", "", meta_split$comment[ indices ] )
        meta_split$comment <- as.character( meta_split$comment )

    # Reclassify the notes for display purposes using the function in
    #   emissions_scaling_functions.R
        printLog( "Reclassifying value metadata" )
        meta_classified <- F.reclass_metavalue( meta_split )

    # Hand off only relevant columns to a dataframe that will be used for
    #   plotting only; operating on notes is finished
        meta_for_plots <- meta_classified[ , c( "Region", "Figure_sector", "year", "value", "prepost", "emissions" ) ]

    # A list of pre-determined colors for each inventory allows us to force
    #   Default to be gray and Zero to be white
        inventory_colors <- c( "Default" = "#cccccc",
                               "Zero emissions" = "#ffffff",
                               "EDGAR 4.3-PEGASOS" = "#026fff",
                               "EMEP_NFR09" = "#00BE67",
                               "REAS 2.1" = "#d966ff",
                               "EMEP_NFR14" = "#73e600",
                               "UNFCCC, 2015" = "#f75555",
                               "Environment Canada, 2013" = "#ff8c1a",
                               "Environment and Climate Change Canada, 2016" = "#ffe11a",
                               "US EPA, 2016" = "#990033",
                               "US" = "#1d3d84",
                               "Li et al., 2017" = "#552bff",
                               "TEPA, 2016" = "#1de0cc",
                               "Argentina UNFCCC submission, 2016" = "#ff8484",
                               "Kurokawa et. al, 2013" = "#e53d6e",
                               "South Korea National Institute of Environmental Research, 2016" = "#875c1d",
                               "Australian Department of the Environment, 2016" = "#1c661b",
                               "EDGAR 4.2" = "#80d4ff" )

    # For either all_sectors or all_regions, iterate through and create a single
    #   plot (see above function) based on counts for that region/sector and add
    #   to a list of plots
        list_of_plots <- list()
        printLog( paste0( "Generating aggregate metaval heatmaps by ", map_by ) )
        if ( map_by == "Sector" ) {
            list_of_plots <- lapply( all_sectors,
                                     createSinglePlot,
                                     meta_classified = meta_for_plots,
                                     id_type = "Sector",
                                     inventory_colors = inventory_colors,
                                     weight_by_em = weight_by_em,
                                     normalize = normalize )
        } else if ( map_by == "Region" ) {
            list_of_plots <- lapply( all_regions,
                                     createSinglePlot,
                                     meta_classified = meta_for_plots,
                                     id_type = "Region",
                                     inventory_colors = inventory_colors,
                                     weight_by_em = weight_by_em,
                                     normalize = normalize )
        }

    # Get a quick count of all data
        all_counts <- meta_for_plots %>%
                          count( year, value, prepost )
        all_counts$value <- factor( all_counts$value, levels = c( "Default",
                  "Zero emissions",
                  "EDGAR 4.3-PEGASOS",
                  "EMEP_NFR09",
                  "REAS 2.1",
                  "EMEP_NFR14",
                  "UNFCCC, 2015",
                  "Environment Canada, 2013",
                  "Environment and Climate Change Canada, 2016",
                  "US EPA, 2016",
                  "US",
                  "Li et al., 2017",
                  "TEPA, 2016",
                  "Argentina UNFCCC submission, 2016",
                  "Kurokawa et. al, 2013",
                  "South Korea National Institute of Environmental Research, 2016",
                  "Australian Department of the Environment, 2016",
                  "EDGAR 4.2" ) )

    # Create a single master plot. This plot will never be arranged or
    #   displayed; its purpose is to generate a unifying legend for all the data,
    #   as it contains all the inventories present

        if (weight_by_em) all_counts <- all_counts[ all_counts$value != "Zero emissions", ]

        plot_for_legend <- ggplot(all_counts, aes( year, n ) ) +
          geom_area(aes(fill = value, alpha = prepost), position = 'stack') +
          scale_fill_manual(values = inventory_colors) +
          labs(fill="Inventory", alpha="Extension \n(Color Transparency)") +
          ggtitle("Don't use this plot") +
          scale_alpha_discrete(range = c(1, 0.4)) +
          theme(text = element_text(size=4),
                legend.key.size = unit(7, "points"))

    # Extract the plot's legend
        inventory_legend <- g_legend( plot_for_legend )
        # layout <- rbind( c(1,1,1,1,NA),
        #                  c(1,1,1,1,2),
        #                  c(1,1,1,1,2),
        #                  c(1,1,1,1,NA))
        if (map_by == 'Region'){
        layout <- rbind( c(1,1,2),
                         c(1,1,2),
                         c(1,1,2),
                         c(1,1,2))
        } else if(map_by == 'Sector'){
          layout <- rbind( c(1,1,1,1),
                           c(1,1,1,2),
                           c(1,1,1,2),
                           c(1,1,1,2))}

        title_text <- paste0( "Inventory scaling percentages of ",
                              em, " by ", map_by )
        if (weight_by_em && !normalize) {
            title_text <- paste0( "Global ", em,
                                  " - Inventory Scaling by ", map_by )
        }
    # Arrange the list of plots into a grid, next to the legend
        if (map_by == "Sector") { col_n <- 3
                                  row_n <- 4
                                  arranged_plots <- grid.arrange( arrangeGrob( grobs=list_of_plots , ncol=col_n, nrow=row_n, as.table = FALSE),
                                                                  inventory_legend,
                                                                  layout_matrix = layout,
                                                                  nrow = 1,
                                                                  top = textGrob( title_text,
                                                                                  gp = gpar( fontsize = 15, font = 8 ) ) )

                                  }
        if (map_by == "Region") { col_n <- 2
                                  row_n <- 4

                                  arranged_plots <- grid.arrange( arrangeGrob( grobs=list_of_plots , ncol=col_n, nrow=row_n, as.table = FALSE),
                                                                  inventory_legend,
                                                                  layout_matrix = layout,
                                                                  nrow = 1,
                                                                  widths = c(2,1),
                                                                  top = textGrob( title_text,
                                                                                  gp = gpar( fontsize = 15, font = 8 ) ) )
                                  }


        arrangeGrob()

    # Save the output file and return
        ggsave( paste0( "../diagnostic-output/value-meta-heatmaps/", em, "-MasterHeatmapBy",
                        map_by, ".png" ),
                arranged_plots, width = 7, height = 7 )

        return( meta_classified )
    }

# ---------------------------------------------------------------------------
# 1. Read in data
    library( grid )

# Read in and format value metadata for the given emissions species
    value_metadata <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_EF-value_metadata" ),
                                meta = FALSE, to_numeric = FALSE )[ , c( "iso", "sector", "fuel",
                                                                         paste0( "X", 1971:2014 ) ) ]
    value_metadata <- melt( value_metadata, id.vars = c( 'iso', 'sector', 'fuel' ) )
    names( value_metadata ) <- c( "iso", "sector", "fuel", "year", "comment")
    value_metadata$comment <- as.character( value_metadata$comment )

# Read in absolute emissions for weights
    corresponding_data <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_emissions" ),
                                meta = FALSE )
    corresponding_data <- melt( corresponding_data[, c("iso","sector","fuel",
                                                       paste0( "X", 1971:2014 ) ) ],
                                id.vars = c( 'iso', 'sector', 'fuel' ) )
    names( corresponding_data ) <- c( "iso", "sector", "fuel", "year", "emissions" )

# Add the absolute emissions to the value_metadata dataframe
    value_metadata <- left_join( value_metadata, corresponding_data,
                                 by = c( "iso", "sector", "fuel", "year" ) )
    names( value_metadata ) <- c( "iso", "sector", "fuel", "year", "comment", "emissions" )

# We need these two files for mapping to aggregate regions and sectors
    country_map <- readData( "MAPPINGS", "Master_Country_List" )
    country_map <- country_map[ , c( "iso", "Paper_Figure_Region" ) ]
    colnames(country_map) <- c ("iso", "Region" )
    sector_map <- readData( "MAPPINGS", "Master_Sector_Level_Map" )

# ---------------------------------------------------------------------------
# 2. Exectue function

    classed_meta <- createMasterValMetaHeatmap( value_metadata, country_map, sector_map,
                                                map_by = "Sector", weight_by_em = T, normalize = F )
    classed_meta <- createMasterValMetaHeatmap( value_metadata, country_map, sector_map,
                                                map_by = "Region", weight_by_em = T, normalize = F )
