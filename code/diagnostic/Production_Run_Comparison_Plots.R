# ------------------------------------------------------------------------------
# Program Name: Production Run Comparison Plots.R
# Authors: Rachel Hoesly
# Date Last Updated: January 26, 2023
# Program Purpose: Produces complete comparison plot for diagnostics for a
#                  production run
# Input Files: Comparison_Plots_[em].
# Output Files: Data in final-emissions folder
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('summary_visualization_functions.R')
log_msg <- "Writes summary comparison plots to pdf for Production Run diagnostics" # First message to be printed to the log
script_name <- "Production Run Comparison Plots.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

#----------------------------------

#Check that all comparison plots are made

em_list <- c('BC','CO','NH3','NMVOC','OC','NOx','SO2','CH4','CO2','N2O')
if( any(sapply(em_list,function(EM) !file.exists(
    paste0('../final-emissions/diagnostics/Comparison_Plots_',EM,'.RData'))))){
    stop('Not all comparison plot files exist. Must complete production run.
         Comparison plot R data files are created in the S1.1.write_summary_data.R')
}

# Import data
# Need to load the RData file first, then rename the list from
# "summary_plot_output" to emissions specific name

load('../final-emissions/diagnostics/Comparison_Plots_BC.RData')
    BC_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_CO.RData')
    CO_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_OC.RData')
    OC_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_NH3.RData')
    NH3_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_NMVOC.RData')
    NMVOC_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_NOx.RData')
    NOx_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_SO2.RData')
    SO2_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_CH4.RData')
    CH4_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_CO2.RData')
    CO2_plots <- summary_plot_output
load('../final-emissions/diagnostics/Comparison_Plots_N2O.RData')
    N2O_plots <- summary_plot_output
all_plot_lists  <- list(BC_plots,
                     CO_plots,
                     NH3_plots,
                     NMVOC_plots,
                     NOx_plots,
                     OC_plots,
                     SO2_plots,
                     CH4_plots,
                     CO2_plots,
                     N2O_plots)
# Construct list of plots (is there a select countries plot?)
# Print Graphs

# Check that all the plot lists are the same. The code below will not work if the
# plot lists for all emission species are not identical
all_names <- lapply(all_plot_lists, names)

for(i in 2:length(all_names)){
  if( ! setequal(all_names[[1]],all_names[[i]]) ){stop('Not all the summary plot objects are equal, they contain different plots.
     The selected countries in different emission plots may be different. Plot objects must be equal
     for this script to work. May need to run S1.1write_summary_data again for certain em species - check selected countries.')
  } }

# Print Figures
printLog('Printing plots.')
plot_list <- names(CO2_plots)

pdf( "../final-emissions/diagnostics/Summary_Comparison_Plots.pdf" ,
     width = 9, height = 10, paper ='special' )

for( plot in seq_along(plot_list)){
grid.arrange( g_legend(OC_plots[[plot]]+ theme( legend.position = "bottom" ,
                                                legend.background = element_rect(color = "black"))),
              BC_plots[[plot]]+ theme( legend.position = "none" ),
              CO_plots[[plot]]+ theme( legend.position = "none" ),
              NH3_plots[[plot]]+ theme( legend.position = "none" ),
              NMVOC_plots[[plot]]+ theme( legend.position = "none" ),
              NOx_plots[[plot]]+ theme( legend.position = "none" ),
              OC_plots[[plot]]+ theme( legend.position = "none" ),
              SO2_plots[[plot]]+ theme( legend.position = "none" ),
              CH4_plots[[plot]]+ theme( legend.position = "none" ),
              CO2_plots[[plot]]+ theme( legend.position = "none" ),
              N2O_plots[[plot]]+ theme( legend.position = "none" ),
              layout_matrix = rbind(c(1,1,1),
                                    c(2,3,4),
                                    c(5,6,7),
                                    c(8,9,10),
                                    c(11,NA,NA)),
              heights=c(1,3,3,3,3),
              ncol = 3, nrow = 5,padding = unit(3, "line"),
              top = paste0( plot_list[plot] ) )
}
dev.off()

# End script -----------------------------------
logStop()



