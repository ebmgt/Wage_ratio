# This file is available at https://github.com/ebmgt/Wage_ratio/
# Author: bob.badgett@gmail.com
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-11-17

library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

# Troubleshooting
#options(error = NULL)   # Default
#options(warn = 2)       # Converts warnings into errors
options(warn = 2, error = browser)
options(error = recover) # This will provide a menu showing the call stack, and you can choose which environment to inspect.
options(error = recover) # This will provide a menu showing the call stack, and you can choose which environment to inspect.
typeof('bob')
class('bob') # Better
# database contents:
#sapply(regdat, class)
# browser()
# Key Commands in browser()
# n: Next
#Executes the next line of code and stays in the debugging mode. This is similar to "step over" in other debuggers.
# s: Step into
# If the next line is a function call, s steps into that function, allowing you to debug inside the function. If it's not a function, it behaves like n.
# c: Continue
# Continues execution until the next breakpoint or until the script completes.
# Q: Quit
# Exits the browser and stops the debugging session.

# Global variables -----
Pb <- NULL  # For function_progress
Pallette_RoyGBiv <- c('red','orange','yellow', 'green', 'blue', '#4B0082', 'violet')
Palette_KU <- c("KUBlue" = "#0022B4", "KUSkyBlue" = "#6DC6E7", "KUCrimson" = "#e8000d", "KUYellow" = "#ffc82d")
# Access: Palette_KU["KU Blue"]

##* Footnotes ===========
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \u00A7
# Double Vertical Line \u2016 "\u2016\u2016"
# Para    \B6 or \u0086 or \u204a or \u204b

# Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
getwd()
# Did the script load? -----
#tk_messageBox(type = "ok", paste('1. ', 'R has loaded.\n\nWorking directory:\n', getwd(), sepo=''), caption = "Hello")

# Functions -----
`%notin%` <- Negate(`%in%`)
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))

# Call with function_progress(0,'Libraries')
# Call within a loop:
#  function_progress(100*(index)/length(sheet_names), paste('Processing ',sheet_name))
#  if (index+1 == length(sheet_names)){function_progress(100,'Done')}
function_progress <- function(progress, titletext){
  #if  (!exists("titletext") || is.null(titletext)){titletext <- 'testing'}
  #if  (!exists("progress") || is.null(progress)){progress <- 0}
  if (progress == 0){
    Pb <<- tkProgressBar(titletext, "", 0, 100, 0)
  }
  info <- sprintf("%d%% done", round(progress))
  setTkProgressBar(Pb, value = progress, title = paste(titletext,sprintf("(%s)", info)), label = info)
  if (progress == 100){
    close(Pb)
  }
}

function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), 
                   repos = "https://cloud.r-project.org/",
                   #type = "binary"
  )
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
  #tk_messageBox(type = "ok", paste(packages, collapse="\n"), title="Packages installed")
}

current_date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}  

#* Functions to show data -----

function_table_nice <- function(dataframe, row_name = 'RESPONDENT_STATUS', column_name) {
  # Make the table
  contingency_table <- base::table(
    dataframe[,row_name], dataframe[,column_name], 
    dnn =  c(row_name, column_name), useNA = 'always')
  
  print(addmargins(contingency_table))
  
  # Print the column percentages
  print (round(prop.table(contingency_table, 2) * 100,1))
  
  # Perform chi-square test and display results
  chi_sq_test <- chisq.test(contingency_table)
  print(paste("chi-square: ", chi_sq_test$statistic, " p-value: ", chi_sq_test$p.value, sep=' '))
  
  # Suppress warnings
  suppressWarnings(paste("(warnings suppressed)"))
}

# Display the dataframe using your custom function with escaping disabled
function_display_df_in_viewer <- function(df, caption) {
  DT::datatable(df, caption = caption, options = list(pageLength = 25), escape = FALSE)
}


function_display_model_in_viewer <- function(model, caption) {
  temp <- summary(model)
  summary(model_final)
  odds_ratios_df <- data.frame(Coefficient = round(temp$coefficients$mean[, "Estimate"],3) ,
                               Odds_Ratio  = sprintf(temp$coefficients$mean[, "Estimate"], fmt='%#.2f'), 
                               P.value     = sprintf(temp$coefficients$mean[, "Pr(>|z|)"], fmt='%#.3f'))
  DT::datatable(odds_ratios_df, caption = caption, options = list(pageLength = 25))
}

# Display openxlsx worksheet in the viewer
function_display_xlsx_in_viewer <- function(wb_path_or_object, sheet) {
  # Check if input is a file path or a workbook object
  if (is.character(wb_path_or_object)) {
    # If it's a file path, load the workbook
    wb <- loadWorkbook(wb_path_or_object)
  } else {
    # If it's already a workbook object, use it
    wb <- wb_path_or_object
  }
  
  # Extract data from the specified sheet
  sheet_data <- read.xlsx(wb, sheet = sheet)
  
  # Check if sheet_data is valid
  if (is.null(sheet_data) || nrow(sheet_data) == 0) {
    stop("The sheet is empty or doesn't exist.")
  }
  
  # Convert the data frame to an HTML table
  html_table <- HTML(paste0(
    '<table border="1" style="width:100%; border-collapse:collapse;">',
    paste0(
      '<tr><th>', paste(names(sheet_data), collapse = '</th><th>'), '</th></tr>',
      paste(apply(sheet_data, 1, function(row) {
        paste0('<tr><td>', paste(row, collapse = '</td><td>'), '</td></tr>')
      }), collapse = "")
    ),
    '</table>'
  ))
  
  # Render the HTML table in the RStudio Viewer
  html_print(html_table)
}

function_plot_print <- function (plotname, plotheight, plotwidth){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.tif',sep=''),
    format = "tiff", width = plotwidth, height = plotheight)
}

# Packages/libraries -----
function_progress(0,'Libraries')
#* Essential -----
packages_essential <- c("tcltk", # Helps troublshooting
                        'rstudioapi', # function_plot_print
                        'stringr', 'openxlsx','readr','png')
function_libraries_install(packages_essential)

function_progress(50,'Libraries')

#* Viewer output -----
packages_viewer_output <- c('knitr','kableExtra','dplyr','DT')
function_libraries_install(packages_viewer_output)

function_progress(100,'Libraries')

# Data creation ===============================

#* Remove all but functions from the environment ----
rm(list = ls()[sapply(ls(), function(x) !is.function(get(x)))])

#* Data grab ===================================

# Remove all but functions from the environment
rm(list = ls()[sapply(ls(), function(x) !is.function(get(x)))])

# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Text","*.txt","Spreadsheets","*.csv;*.xls;*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 2,multi=FALSE)
#file.extension<- substr(filename, nchar(filename) - 2, nchar(filename))
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
data.import <- NULL
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  #data.import   <- read.xlsx(filename)
  #data.import<- read.table(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  # Help https://rdrr.io/cran/openxlsx/man/loadWorkbook.html
  wb.temp <- loadWorkbook(filename)
  data.import <- read.xlsx (wb.temp, sheet = 1, startRow = 6, colNames = TRUE, na.strings = "NA", detectDates = TRUE)
}

# ____________________-----
# Start here ------
# Take only Year > 1969
names(data.import)[1] <- "Year"
data.import$Year <- substr(data.import$Year,1,4)
data.import <- data.import[grepl("^[0-9]+$", data.import$Year), ]
data.import <- data.import[!is.na(data.import$Year) & data.import$Year > 1969, ]

# Calculate the median income
data.import$median_income <- data.import$`50th.percentile.(median)`

# Correct data grab: Extract the first 11 columns and insert a new column of zeros as the second column
data_subset <- data.import[, 1:11]
data_subset <- cbind(data_subset[, 1], rep(0, nrow(data_subset)), data_subset[, -1])  # Add a column of zeros as the second column, keep "Year" as the first
names(data_subset)[2] <- "0th.percentile"
names(data_subset)[1] <- "Year"

data_subset$Year <- as.numeric(as.character(data_subset$Year))
data_subset <- data_subset[data_subset$Year %% 5 == 0 & !is.na(data_subset$Year), ]
years_filtered <- data_subset$Year[data_subset$Year %% 5 == 0 & !is.na(data_subset$Year)]

#* Colors --------------
# Generate a smooth gradient of blue colors from lightest (1975) to darkest (2015)
years_to_color <- seq(1975, 2015, by = 5)
num_years <- length(years_to_color)
selected_colors <- colorRampPalette(c("lightblue1", "navy"))(num_years)  # Generate gradient colors

# Reverse the colors so that 1975 is the lightest and 2015 is the darkest
#reversed_colors <- rev(selected_colors)

# Correct data grab: Extract the first 11 columns and insert a new column of zeros as the second column
data_subset <- data.import[, 1:11]
data_subset <- cbind(data_subset[, 1], rep(0, nrow(data_subset)), data_subset[, -1])  # Add a column of zeros as the second column, keep "Year" as the first
names(data_subset)[2] <- "0th.percentile"
names(data_subset)[1] <- "Year"

# Convert Year to numeric and include year 2023 for filtering
data_subset$Year <- as.numeric(as.character(data_subset$Year))
data_subset <- data_subset[(data_subset$Year %% 5 == 0 | data_subset$Year == 2023) & !is.na(data_subset$Year), ]
years_filtered <- data_subset$Year[(data_subset$Year %% 5 == 0 | data_subset$Year == 2023) & !is.na(data_subset$Year)]is.true(colors)
# Color assignment with special colors for 1970, 2020, and 2023
colors <- sapply(years_filtered, function(year) {
  if (year == 1970) {
    return("green")
  } else if (year == 2020) {
    return("black")
  } else if (year == 2023) {
    return("red")
  } else {
    index <- match(year, years_to_color)
    if (!is.na(index)) {
      return(selected_colors[index])  # Use reversed gradient colors for years 1975-2015
    } else {
      return("blue")  # Fallback color if the year is not in the predefined range
    }
  }
})

# _____________________________ -----
# Plot -----
par(mar = c(5.1 + 5, 4.1+1, 4.1, 2.1), mfrow = c(1, 1)) # (bottom, left, top, right)

# Open a plot with appropriate y-axis limits
plot(
  NA, NA,  # Empty plot to start
  xlim = c(0, 10),  # X-axis limits from 0 to 10
  ylim = c(0, 4),  # Fixed y-axis limits
  xlab = "Percentile",
  ylab = "Ratio of income at\nPercentile to Median",
  main = "Income Ratio for Each Percentile to Median Distribution\nIn the U.S. Since 1970",
  xaxt = "n",  # Suppress default x-axis ticks
  yaxt = "n"  # Suppress default x-axis ticks
)

# Custom x-axis ticks and labels with 95th percentile at x = 9.5
x_labels <- c(paste0(seq(0, 90, by = 10), "th"), "95th")
axis(1, at = c(seq(0, 9), 9.5), labels = FALSE)  # Suppress default labels

# Add x-axis labels with "50th" bold and larger
text(x = c(seq(0, 9), 9.5), y = par("usr")[3] - 0.2, labels = x_labels, 
     cex = ifelse(x_labels == "50th", 1.1, 1), 
     font = ifelse(x_labels == "50th", 2, 1), 
     xpd = TRUE)

# Custom y-axis labels with only integer values and making "1" bold
y_labels <- sprintf("%.1f", seq(0, 4, by = 1))
axis(2, at = seq(0, 4, by = 1), labels = FALSE)  # Suppress default labels

# Add y-axis labels with "1" bold and larger
text(x = par("usr")[1] - 0.2, y = seq(0, 4, by = 1), labels = y_labels, 
     cex = ifelse(y_labels == "1.0", 1.1, 1), 
     font = ifelse(y_labels == "1.0", 2, 1), 
     xpd = TRUE, adj = 1)



segments(x0 = 5, y0 = 0.5, x1 = 5, y1 = 1.5, col = "green", lwd = 2)
segments(x0 = 4, y0 = 1, x1 = 6, y1 = 1, col = "green", lwd = 2)

# Declare row_data_df as a data frame for debugging in the global environment
row_data_df <- data.frame()

# Apply the operations for each row and add lines
for (i in seq_len(nrow(data_subset))) {
  # Convert to numeric (handle potential non-numeric entries with suppression of warnings)
  row_data <- suppressWarnings(as.numeric(data_subset[i, -1]))  # Exclude "Year" from calculations
  
  # Remove NA values caused by non-numeric entries (if any)
  row_data <- row_data[!is.na(row_data)]
  
  # Only proceed if there are exactly 11 values (matching expected columns)
  if (length(row_data) == 11) {
    # Calculate the median of the row excluding the first zero value
    # median_value <- median(row_data[-1], na.rm = TRUE)
    median_value <- row_data[6]
    
    # Calculate the ratio to the median excluding the first zero value
    ratio_to_median <- row_data / median_value
    
    # Corrected x-values for 11 points (from 10th to 95th percentiles)
    x_values <- c(seq(0, 9), 9.5)  # Ensure proper alignment with the data
    
    # Add lines and points to the plot only if lengths match
    if (length(ratio_to_median) == length(x_values)) {
      lines(
        x_values,  # Corrected x-values
        ratio_to_median,  # y-values with 11 elements
        type = "o",
        col = colors[i],  # Color for each line based on year
        lty = 1,  # Line type
        lwd = 2   # Line width
      )
      points(
        x_values,  # x-values for points
        ratio_to_median,  # y-values for points
        col = colors[i],  # Color matching the line
        pch = 16  # Filled circle
      )
      # Add year label to the right of the last point if it's a special line (2020, 2023) or other blue lines
      # Add year label to the right of the last point with the same color as the line
      text(
        x = x_values[length(x_values)] + 0.2,  # Slightly to the right of the last point
        y = ratio_to_median[length(ratio_to_median)],  # Same y-coordinate as the last point
        labels = years_filtered[i],  # Year label
        pos = 4,  # Position to the right
        cex = 0.7,  # Text size
        col = colors[i]  # Use the color from the colors vector
      )
    }
  }
}

# Add legend in the upper left corner
legend("topleft", legend = years_filtered, col = colors, lty = 1, cex = 0.8, inset = 0.05)

text(5.5, 0.5, "Notes:", font = 2, adj = 0)
text(5.5, 0.3, "CEO pay ratio disclosures publicly available starting in 2018.", adj = 0)

# Sources ---------
mtext("Sources:", side = 1, line = 4, adj = 0, font = 2)  # Bold "Notes:" text aligned left
mtext("Data downloaded from \nhttps://www.census.gov/topics/income-poverty/income-inequality/data/data-tables.All.List_1412438315.html", 
      side = 1, line = 6, adj = 0, cex = 1)

mtext(paste0("Data and code available at https://ebmgt.github.io/Wage_ratio"), 
      side = 1, line = 7, adj = 0, cex = 1,
      col = "black" )

mtext(paste0("Printed: ", current_date(), ". Attribution-NonCommercial 4.0 International (CC BY-NC 4.0). bob.badgett@gmail.com"), 
      side = 1, line = 8, adj = 0, cex = 1,
      col = "black" )

qr_image <- readPNG("qrcode-ebmgt.github.io.png")

# Adjust the figure parameters to place the QR code
par(fig = c(0.85, 0.98, 0, 0.15), new = TRUE, mar = c(0, 0, 0, 0))  # Minimize margins for the QR code
plot.new()  # Start a new plot region
rasterImage(qr_image, 0, 0, 1, 1)  # Place the QR code image

function_plot_print("Income_distribution_v3",900,1100)

