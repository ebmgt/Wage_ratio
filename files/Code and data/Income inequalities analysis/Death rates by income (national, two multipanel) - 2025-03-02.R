# This file is available at https://ebmgt.github.io/Wage_ratio/
# Author: rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-03-04
# Log:
# 2025-02-24: clarified the central tendency of the upper quintile

#== Startup ======
library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#* Cleanup ======
# rm(df_with_cutoffs)
# CLEAN ENVIRONMENT -----------------------
# Remove all but functions from the environment
rm(list = ls(envir = .GlobalEnv)[!sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x, envir = .GlobalEnv)))])
# Remove all but functions & data frames from the environment
rm(list = ls(envir = .GlobalEnv)[
  !sapply(ls(envir = .GlobalEnv), function(x) {
    is.function(get(x, envir = .GlobalEnv)) || 
      is.data.frame(get(x, envir = .GlobalEnv))
  })
])
# OR PRESERVE GLOBAL VALUES:
rm(list = ls()[sapply(ls(), function(x) {
  var <- get(x)
  !is.function(var) && !(is.character(var) || is.numeric(var) || is.factor(var))
})])
rm(list = ls(pattern = "^I2")) # global variables

#* Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
} else {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
getwd()

#* Troubleshooting options -----
options(error = NULL)   # Default
options(warn=1) # print error; but dont convert to error. Default is 0
getOption("warn")

# Functions -----
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

function_plot_print <- function (plotname, plotwidth, plotheight, imagetype) {
  
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  
  current.date <- as.character(strftime(Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
  
  rstudioapi::savePlotAsImage(
    paste(plotname, ' -- ', current.date, '.', imagetype, sep=''),
    format = imagetype, width = plotwidth, height = plotheight)
}

# Libraries -----
library(dplyr)
library(ineq)
library(openxlsx) # or openxlsx, whichever is in use
library(readr)
library(lubridate) # Year
library(DescTools) # for Gini_test skewness() and kurtosis()
library(boot)
library(cocor)  #Compare model accuracies
function_libraries_install('epitools')
# DATA GRAB ===================================
## MORTALITY ------------------------
### CDC Wonder ----------------
filename <- 'Deaths by state and year - 1968 - 2016.txt'
filename <- paste0("CDC Wonder\\",filename)
# Read the tab-delimited file, specifying col_names=TRUE because the file has a header row.
df <- read_tsv(
  file = filename,
  col_names = TRUE,
  comment = "#"
)

df <- df[!is.na(df$Year), ]

write.xlsx(df, paste0("CDC Wonder\\Deaths by state and year -", Sys.Date(),".xlsx"))

colnames(df)
length(unique(df$Year))
cat("df analytic units: ", nrow(df)/length(unique(df$Year))/nrow(unique(df['State Code'])), " (is this the number of counties per state)")
# EXAMPLE (B): Population-weighted average Age Adjusted Rate
# Often, if states have different population sizes, you might want an overall "Age Adjusted Rate"
# that accounts for how large or small each state is. For instance:

## Parameters for CDC Wonder mortality-----
Start_year <- function_tcltk_input (1990)
Start_year <- as.numeric(Start_year)

df <- df[df$Year >= Start_year, ]

# A global variable that can be either "Age Adjusted Rate" or "Crude Rate"
mortality_type <- tk_select.list(c('Age Adjusted Rate','Crude Rate'), preselect = 'Age Adjusted Rate', multiple = FALSE,
                              title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat are we studying?\n\n"))

df$DeathsAdjusted <- df$`Age Adjusted Rate`*df$Population/1e5

# STEP 1: For each Year, compute the quintile cutoffs for the chosen column.
df_cutoffs <- df %>%
  group_by(Year) %>%
  summarize(
    TopCut       = quantile(.data[[mortality_type]], 0.80, na.rm = TRUE),
    BottomCut    = quantile(.data[[mortality_type]], 0.20, na.rm = TRUE),
    LowerMidCut  = quantile(.data[[mortality_type]], 0.40, na.rm = TRUE),
    UpperMidCut  = quantile(.data[[mortality_type]], 0.60, na.rm = TRUE)
  ) %>%
  ungroup()

# STEP 2: Join those cutoffs back into the row-level data, and define "RateUsed"
# so we can refer to the chosen column in later calculations:
df_with_cutoffs <- df %>%
  left_join(df_cutoffs, by = "Year") %>%
  mutate(
    # The user wants to pick either Age Adjusted Rate or Crude Rate at runtime.
    # We'll store it in "RateUsed" for consistent use.
    RateUsed = .data[[mortality_type]],
    
    # If we want a "DeathsAdjusted" measure for either rate:
    # only do this if the chosen column is also "per 100k."
    DeathsAdjusted = RateUsed * Population / 1e5
  )

# STEP 3: Now we still have row-by-row data for each state.
# We can group by Year and subset which states are in top/mid/bottom.
df_death_yearly <- df_with_cutoffs %>%
  group_by(Year) %>%
  summarize(
    # Basic aggregates for the whole year
    YearPopulation = sum(Population, na.rm = TRUE),
    YearDeaths     = sum(Deaths, na.rm = TRUE),  # or sum(DeathsAdjusted) if you prefer
    YearDeathsAdjusted     = sum(DeathsAdjusted, na.rm = TRUE),

    #Gini_Death = ineq::Gini(RateUsed, corr = TRUE, na.rm = TRUE),
    Gini_Death = DescTools::Gini(RateUsed, weights = NULL, na.rm = TRUE, conf.level = NA, 
                         # method = 3, ci.type = "bca", 
                         R = 1000),
    #Mortality_Kurtosis = kurtosis(RateUsed, na.rm = TRUE),
    Mortality_Kurtosis = DescTools::Kurt(RateUsed, weights = NULL, na.rm = TRUE, method = 3, 
                         # conf.level = NA, ci.type = "bca", 
                         R = 1000),
    #Mortality_skewness = skewness(RateUsed, na.rm = TRUE),
    Mortality_skewness = DescTools::Skew(RateUsed, weights = NULL, na.rm = TRUE, method = 3, 
                         # conf.level = NA, ci.type = "bca", 
                         R = 1000),
    MortalityIQR = IQR(RateUsed, na.rm = TRUE),
    # Subset for top quintile (≥ TopCut)
    top_pop    = sum(Population[RateUsed >= TopCut], na.rm = TRUE),
    top_deaths = sum(DeathsAdjusted[RateUsed >= TopCut], na.rm = TRUE),
    
    # Subset for "middle" (40th–60th)
    mid_pop    = sum(Population[RateUsed >= LowerMidCut & 
                                  RateUsed <= UpperMidCut], na.rm = TRUE),
    mid_deaths = sum(DeathsAdjusted[RateUsed >= LowerMidCut & 
                                      RateUsed <= UpperMidCut], na.rm = TRUE),
    
    # Subset for bottom quintile (≤ BottomCut)
    bottom_pop    = sum(Population[RateUsed <= BottomCut], na.rm = TRUE),
    bottom_deaths = sum(DeathsAdjusted[RateUsed <= BottomCut], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Crude mortality rate for entire year
    MortalityRate_Crude = 1e5 * YearDeaths / YearPopulation,
    MortalityRate_Adjusted = 1e5 * YearDeathsAdjusted / YearPopulation,
    
    # Top/mid/bottom mortality rates
    TopQuintileMortality    = 1e5 * top_deaths    / top_pop,
    MidQuintileMortality    = 1e5 * mid_deaths    / mid_pop,
    BottomQuintileMortality = 1e5 * bottom_deaths / bottom_pop,
    Diff_Top_Mid = TopQuintileMortality - MidQuintileMortality,
    RR_Top_Mid = (top_deaths / top_pop) / (mid_deaths / mid_pop),
    log_RR = log(RR_Top_Mid),
    var_log_RR = (1 / top_deaths) - (1 / top_pop) + (1 / mid_deaths) - (1 / mid_pop),
    se_log_RR = sqrt(var_log_RR)
    )

write.xlsx(df_death_yearly, paste0("CDC Wonder\\CDC-Wonder-df_death_yearly (", mortality_type," since ", Start_year,") -", Sys.Date(),".xlsx"))

df_merged <- df_death_yearly

## INCOME (CDC) including GINI and quintiles ----------------
### Census.gov ----------------
filename <- "tableA4b.xlsx"
filename <- paste0("Census.gov\\",filename)
df_income <-  read.xlsx(filename, 
                        sheet = 1, 
                        startRow = 7, 
                        colNames = TRUE)

# Force unique column names to avoid the "Input columns in `y` must be unique" error
colnames(df_income) <- make.unique(colnames(df_income))

# Make sure your Year and Gini_Income columns exist
names(df_income)[1] <- "Year"
names(df_income)[14] <- "Gini_Income"

df_income <- df_income[df_income$Year != "2013 3", ]
df_income <- df_income[df_income$Year != "2017", ]
df_income$Year <- substr(df_income$Year,1,4)
df_income$Year <- as.numeric(df_income$Year)

df_income <- df_income[!is.na(df_income$Year),]

recession_years <- c(
  1907, 1908,
  1910, 1911, 1913, 1914,
  1918, 1919,
  1920, 1921,
  1923, 1924,
  1926, 1927,
  1929, 1930, 1931, 1932, 1933,
  1937, 1938,
  1945,
  1949,
  1953, 1954,
  1957, 1958,
  1960, 1961,
  1969, 1970,
  1973, 1974, 1975,
  1980, 1981, 1982,
  1990, 1991,
  2001,
  2007, 2008, 2009,
  2020
)

# Create a new boolean column indicating whether each row's 
df_income$recession_year <- df_income$Year %in% recession_years

### Merge on "Year" -----
# The full_join checks that df_income has unique column names
df_merged <- df_merged %>%
  full_join(df_income, by = "Year")

## ECONOMIC HARDSHIP (Census.gov) including GINI and quintiles ----------------
### https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-poverty-people/ ----------------
filename <- "hstpov6.xlsx"
filename <- paste0("Census.gov\\",filename)
df_economic_hardship <-  read.xlsx(filename, 
                           sheet = 1, 
                           startRow = 5, 
                           colNames = TRUE)
names(df_economic_hardship)[1] <- "Year"
names(df_economic_hardship)[2] <- "Total"
names(df_economic_hardship)[3] <- "Number(0.5)"
names(df_economic_hardship)[4] <- "Economic_Hardship(0.5)"
names(df_economic_hardship)[5] <- "Number(1.25)"
names(df_economic_hardship)[6] <- "Economic_Hardship(1.25)"
names(df_economic_hardship)[7] <- "Number(tween)"
names(df_economic_hardship)[8] <- "Economic_Hardship(tween)"

# Clean up Year
df_economic_hardship <- df_economic_hardship[df_economic_hardship$Year != "2013 (5)", ]
df_economic_hardship <- df_economic_hardship[df_economic_hardship$Year != "2017 (3)", ]
df_economic_hardship$Year <-  substr(df_economic_hardship$Year,1,4)
df_economic_hardship$Year <- as.numeric (df_economic_hardship$Year)
df_economic_hardship$`Economic_Hardship(0.5)` <- as.numeric (df_economic_hardship$`Economic_Hardship(0.5)`)
df_economic_hardship$`Economic_Hardship(1.25)` <- as.numeric (df_economic_hardship$`Economic_Hardship(1.25)`)

df_economic_hardship <- df_economic_hardship[!is.na(df_economic_hardship$Year),]

### Merge on "Year" -----
# The full_join checks that df_income has unique column names
df_merged <- df_merged %>%
  full_join(df_economic_hardship, by = "Year")

## Inflation ----------------
### https://fred.stlouisfed.org/series/FPCPITOTLZGUSA----------------
filename <- "FPCPITOTLZGUSA.xlsx"
filename <- paste0("FRED\\",filename)
df_inflation <-  read.xlsx(filename, 
                        sheet = 2, 
                        startRow = 1, 
                        colNames = TRUE)

df_inflation$observation_date <- as.Date(
  df_inflation$observation_date, 
  origin = "1899-12-30"
)

df_inflation$Year <- as.numeric(
  format(df_inflation$observation_date, "%Y")
)
df_inflation$Year  <- df_inflation$Year  - 1

names(df_inflation)[names(df_inflation) == "FPCPITOTLZGUSA"] <- "Inflation_rate"

df_inflation <- df_inflation[!is.na(df_inflation$Year),]

### Merge on "Year" -----
df_merged <- merge(
  x = df_merged,
  y = df_inflation,       # or a subset: df_inflation[, c("Year", "some_value")]
  by = "Year",
  all.x = TRUE           # Keep all rows in df_merged
)

# Historical notes ------
# Early 2000s Dot-Com Crash
# from overvalueing of stocks of tech stocks
# 2007–2009 economic downturn in the United States is known as the Great Recession. It was the worst economic crisis in the U.S. since the Great Depression. 
# Causes: The bursting of the U.S. housing bubble, The global financial crisis, and The collapse of the U.S. housing market.

# __________________________________________------------
# Figure 1: Histogram of Age Death Adjusted Rate -----------------
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1)) # (bottom, left, top, right)
#plot(df$`Age Adjusted Rate`, df$Year)
hist(
  df$`Age Adjusted Rate`, 
  # df_merged$MortalityRate_Adjusted, 
  breaks = 100,                      # number of bins (optional)
  main = "",
  xlab = "Mortality Rate, Age Adjusted, per 100,000",
  col = "skyblue",
  border = "white"
)
# Title
mtext(bquote(
  bold("Figure 1. ") ~
    "Distribution of "~ .(nrow(df)) ~ "mortality rates, age adjusted, across states (" *
    .(min(df$Year)) ~ " to " ~ .(max(df$Year)) * ")."
),
side = 3, line = 2, adj = 0)
mtext(bquote(
    "The rates are not normally distributed and are best represented by a Gini coefficient and a Lorenz curve."
),
side = 3, line = 1, adj = 0)

#* Analyses -----
message(paste0("\033[32m \033[1m\ndf: \033[1m ", nrow(df), " rows: consisting of ", length(unique(df_merged$Year)), " years (",min(df_merged$Year), " to ", min(df_merged$Year), ") of ", length(unique(df$State)), " states.\012\033[0m"))

message(paste0("\033[32m \033[1m\ndf_merged$Year: \033[1m ", nrow(df_merged), " values ranging from ", min(df_merged$Year), " to ", min(df_merged$Year), ".\012\033[0m"))

# Normal distribution
shapiro_result <- shapiro.test(df$`Age Adjusted Rate`)
results_sw <- formatC(shapiro_result$p.value, format = "f", digits = 3)
#ad_result <- ad.test(df$`Age Adjusted Rate`)
#results_ad <- formatC(ad_result$p.value, format = "f", digits = 3)

# Skew
results <- DescTools::Skew(df$`Age Adjusted Rate`, weights = NULL, na.rm = TRUE, method = 3, conf.level = 0.95, 
                    ci.type = "bca", R = 1000)
#--
result_se <- (abs(results[3] - results[2])) / 1.96
result_Z         <- results[1] / result_se
result_Z_CIlow   <- results[2] / result_se
result_Z_CIhigh  <- results[3] / result_se
results <- paste0(formatC(results[1], format = "f", digits = 3), " (", formatC(results[2], format = "f", digits = 3), " to ", formatC(results[3], format = "f", digits = 3), ")")
results_Z <- paste0(formatC(result_Z, format = "f", digits = 1), " (", formatC(result_Z_CIlow, format = "f", digits = 1), " to ", formatC(result_Z_CIhigh, format = "f", digits = 1), ")")
message ("\033[32m \033[1m\nSkew: ", results, "\n  Z score (est): ", results_Z, ")\n\012\033[0m")
results <- formatC(results, format = "f", digits = 3)
#--
results_S <- results
result_Z_S <- results_Z

# Kurtosis
results <- DescTools::Kurt(df$`Age Adjusted Rate`, weights = NULL, na.rm = TRUE, method = 3, conf.level = 0.95, 
                  ci.type = "bca", R = 1000)
#--
result_se <- (abs(results[3] - results[2])) / 1.96
result_Z         <- results[1] / result_se
result_Z_CIlow   <- results[2] / result_se
result_Z_CIhigh  <- results[3] / result_se
results <- paste0(formatC(results[1], format = "f", digits = 3), " (", formatC(results[2], format = "f", digits = 3), " to ", formatC(results[3], format = "f", digits = 3), ")")
results_Z <- paste0(formatC(result_Z, format = "f", digits = 1), " (", formatC(result_Z_CIlow, format = "f", digits = 1), " to ", formatC(result_Z_CIhigh, format = "f", digits = 1), ")")
message ("\033[32m \033[1m\nKurtosis: ", results, "\n  Z score (est): ", results_Z, ")\n\012\033[0m")
results <- formatC(results, format = "f", digits = 3)
#--
results_K <- results
result_Z_K <- results_Z

# Gini
# SLOW PROCESSING HERE
message(paste0("\033[32m \033[1mThis next line of code may be slow to run!!\012\033[0m"))
results <- DescTools::Gini(df$`Age Adjusted Rate`, 
                 weights = NULL, unbiased = TRUE,
                 conf.level = 0.95,  # use 0.95 rather than TRUE
                 R = 10000, type = "bca", na.rm = TRUE)
#--
result_se <- (abs(results[3] - results[2])) / 1.96
result_Z         <- results[1] / result_se
result_Z_CIlow   <- results[2] / result_se
result_Z_CIhigh  <- results[3] / result_se
results <- paste0(formatC(results[1], format = "f", digits = 3), " (", formatC(results[2], format = "f", digits = 3), " to ", formatC(results[3], format = "f", digits = 3), ")")
results_Z <- paste0(formatC(result_Z, format = "f", digits = 1), " (", formatC(result_Z_CIlow, format = "f", digits = 1), " to ", formatC(result_Z_CIhigh, format = "f", digits = 1), ")")
message ("\033[32m \033[1m\nGini: ", results, "\n  Z score (est): ", results_Z, ")\n\012\033[0m")
results <- formatC(results, format = "f", digits = 3)
#--
results_G <- results
result_Z_G <- results_Z

#* Display summary measures -----
#mtext(expression(paste(bold("Note: "),"blah, blah, blah.")), side = 1, line = 4.2,adj=0,cex=1)
# Normality
results_sw <- ifelse(as.numeric(results_sw) < 0.05, paste0(results_sw, " (not normal)"), paste0(results_sw, " (normal)") )
text(par("usr")[2] - 450, # x max
     par("usr")[4] - 1.2*strheight('A'), # y max,
     bquote(bold("Normality p-value: ") ~ .(results_sw)), 
     adj = 0, cex = 1)
# Skew
text(
  x = par("usr")[2] - 450,
  y = par("usr")[4] - 3 * strheight("A"),
  labels = bquote(bold("Skew: ") ~ .(results_S)),
  adj = 0,
  cex = 1
)
text(
  x = par("usr")[2] - 450,
  y = par("usr")[4] - 4.2 * strheight("A"),  # shift the second line up slightly
  labels = bquote("  Z score (est): " ~ .(result_Z_S)),
  adj = 0,
  cex = 1
)
# Kurtosis
text(
  x = par("usr")[2] - 450,
  y = par("usr")[4] - 5.7 * strheight("A"),
  labels = bquote(bold("Kurtosis: ") ~ .(results_K)),
  adj = 0,
  cex = 1
)
text(
  x = par("usr")[2] - 450,
  y = par("usr")[4] - 6.9 * strheight("A"),  # shift the second line up slightly
  labels = bquote("  Z score (est): " ~ .(result_Z_K)),
  adj = 0,
  cex = 1
)
# Gini
text(
  x = par("usr")[2] - 450,
  y = par("usr")[4] - 8.4 * strheight("A"),
  labels = bquote(bold("Gini: ") ~ .(results_G)),
  adj = 0,
  cex = 1
)
text(
  x = par("usr")[2] - 450,
  y = par("usr")[4] - 9.6 * strheight("A"),  # shift the second line up slightly
  labels = bquote("  Z score (est): " ~ .(result_Z_G)),
  adj = 0,
  cex = 1
)
# Footer -----
mtext(paste0("rbadgett@kumc.edu, ", Sys.Date()),
      side=1, line = 4, cex = 1.2, adj=1)

#* Print ------
function_plot_print("Figure 1. Distribution of mortality rates", 800, 600, "png")

# Skewness of a normal distribution is 0.
#   For skewness, if absolute skew <0.5, it’s fairly symmetrical; 0.5-1 is moderately skewed; >1 is highly skewed.
# Kurtosis of a normal distribution is often said to be 3,
#  -- For kurtosis, excess kurtosis near 0 is normal; -1 to +1 is fairly normal; >2 is highly peaked or heavy tails.

# _______________________________________________-----
# Figure  #2 (USED):  Gini_Death vs Gini_Income --------------
#* Par restore
#par(mar=c(5.1 + 2,4.1,4.1,2.1), mfrow=c(1,1)) # (bottom, left, top, right)
layout(matrix(1:5, ncol = 1), 
       heights = c(0.05, 0.3, 0.3, 0.3, 0.3))
par(oma = c(1, 1, 1, 1))  # Outer margins: bottom, left, top, right
par(cex.axis = 1.3)
par(cex.lab  = 1.3)

df_merged <- df_merged[df_merged$Year >= Start_year, ]

#---- Title ----
par(mar = c(0, 0, 0, 0))
plot.new()
mtext(bquote(bold("Figure 2. ") ~ .("Income (Gini) predicting Mortality Measures (National 1999 through 2016)")), 
      side = 3, line = -1, cex = 1, adj = 0, outer = FALSE)

# Reset margins for the subsequent panels
par(mar = c(5 + 2, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap

# 1) Gini_Income vs MortalityAdjusted --------------
plot(df_merged$Gini_Income,
     df_merged$MortalityRate_Adjusted,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     col = "darkgreen", pch  = 19,
     xlab = "Income (Gini Coefficient)",
     ylab = "Mortality rate\n(per 100,000)",
     main = "")
mtext(bquote(bold("Panel A. ") ~ .("Predicting Mortality (absolute, age-adjusted)")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#* Regression -----
lm_out <- lm(MortalityRate_Adjusted ~ Gini_Income, data=df_merged)
lm_sum <- summary(lm_out)
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2)

# Display summary measures -----
p_val <- lm_sum$coefficients["Gini_Income", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

# Line 1: P-value
text(par("usr")[2] - 0.01, # x max
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value  (linear regression ) = ", p_val),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript)
text(par("usr")[2] - 0.01, # x max
     par("usr")[4] - 2.1*strheight('A'), # y max,
     labels = bquote(R^2 == .(sum_R2)),
     cex = 1.5,
     adj = c(0, 1))

# Line 3: correlation
# Need negative sign:
cor_value <- cor(df_merged$MortalityRate_Adjusted, df_merged$Gini_Income, use = "complete.obs")
cor_value <- sprintf(cor_value, fmt='%#.3f')
text(par("usr")[2] - 0.01, # x max
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(r == .(cor_value)),
     cex = 1.5,
     col = "black",
     adj = c(0, 1))

# 2) Gini_Income vs MortalityIQR --------------
plot(df_merged$Gini_Income,
     df_merged$MortalityIQR,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     col = "black", pch  = 19,
     xlab = "Income (Gini Coefficient)",
     ylab = "Mortality\n(IQR range)",
     main = "")
mtext(bquote(bold("Panel B. ") ~ .("Predicting Mortality (IQR range)")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#* Regression -----
lm_out <- lm(MortalityIQR ~ Gini_Income, data=df_merged)
lm_sum <- summary(lm_out)
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2)

# Display summary measures -----
p_val <- lm_sum$coefficients["Gini_Income", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

# Line 1: P-value
text(par("usr")[2] - 0.01, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value  (linear regression ) = ", p_val),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript)
text(par("usr")[2] - 0.01, # x min
     par("usr")[4] - 2.1*strheight('A'), # y max,
     labels = bquote(R^2 == .(r_sq)),
     cex = 1.5,
     adj = c(0, 1))

# Line 3: correlation
# Need negative sign:
cor_value <- cor(df_merged$MortalityIQR, df_merged$Gini_Income, use = "complete.obs")
cor_value <- sprintf(cor_value, fmt='%#.3f')
text(par("usr")[2] - 0.01, # x min
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(r == .(cor_value)),
     cex = 1.5,
     col = "black",
     adj = c(0, 1))

# 3) Gini_Income vs Gini_Death --------------
plot(df_merged$Gini_Income,
     df_merged$Gini_Death,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     col = "red", pch  = 19,
     xlab = "Income (Gini Coefficient)",
     ylab = "Mortality\n(Gini Coefficient)",
     main = "")
mtext(bquote(bold("Panel C. ") ~ .("Predicting Mortality (Gini Coefficient)")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#* Regression -----
lm_out <- lm(Gini_Death ~ Gini_Income, data=df_merged)
lm_sum <- summary(lm_out)
# Draw regression line
abline(lm_out, col = "red", lwd = 2)
length(predict(lm_out))
df_time <- data.frame(Year = min(df_merged$Year):max(df_merged$Year), Predicted = predict(lm_out))
cor.test(df_merged$Gini_Income, df_merged$Gini_Death)

# Display summary measures -----
p_val <- lm_sum$coefficients["Gini_Income", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

# Line 1: P-value
text(par("usr")[1] + 0.001, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value  (linear regression ) = ", p_val),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript)
text(par("usr")[1] + 0.001, # x min
     par("usr")[4] - 2.1*strheight('A'), # y max,
     labels = bquote(R^2 == .(r_sq)),
     cex = 1.5,
     adj = c(0, 1))

# Line 3: correlation
# Need negative sign:
cor_value <- cor(df_merged$Gini_Death, df_merged$Gini_Income, use = "complete.obs")
cor_value <- sprintf(cor_value, fmt='%#.3f')
text(par("usr")[1] + 0.001, # x min
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(r == .(cor_value)),
     cex = 1.5,
     col = "red", font = 2,
     adj = c(0, 1))

# Line 3b: correlation with years
# Need negative sign:
cor_value <- cor(df_merged$Gini_Death, df_merged$Gini_Income, use = "complete.obs")
cor_value <- sprintf(cor_value, fmt='%#.3f')
if (1==2){
  text(par("usr")[1] + 0.001, # x min
       par("usr")[4] - 9*strheight('A'), # y max,
       labels = paste0(('r (with Year) = Pending')),
       cex = 1.5,
       col = "black",
       adj = c(0, 1))
}

# 4) Gini_Income vs Highest.quintile --------------
plot(df_merged$Gini_Income,
     df_merged$TopQuintileMortality,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     col = "darkgreen", pch  = 19,
     xlab = "Income (Gini Coefficient)",
     ylab = "Mortality\n(Top Quintile)",
     main = "")
mtext(bquote(bold("Panel D. ") ~ .("Predicting Mortality in top quintile")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#* Regression -----
lm_out <- lm(TopQuintileMortality ~ Gini_Income, data=df_merged)
lm_sum <- summary(lm_out)
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2)

# Display summary measures -----
p_val <- lm_sum$coefficients["Gini_Income", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

# Line 1: P-value
text(par("usr")[2] - 0.01, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value  (linear regression ) = ", p_val),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript)
text(par("usr")[2] - 0.01, # x min
     par("usr")[4] - 2.1*strheight('A'), # y max,
     labels = bquote(R^2 == .(sum_R2)),
     cex = 1.5,
     adj = c(0, 1))

# Line 3: correlation
# Need negative sign:
cor_value <- cor(df_merged$Highest.quintile, df_merged$Gini_Income, use = "complete.obs")
cor_value <- sprintf(cor_value, fmt='%#.3f')
text(par("usr")[2] - 0.01, # x min
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(r == .(cor_value)),
     cex = 1.5,
     col = "black",
     adj = c(0, 1))

# Footer -----------------------
mtext(paste0("rbadgett@kumc.edu, ",Sys.Date()),
      side=1, line = 5, adj=1)
#* Print -----
function_plot_print("Figure 2. Income Gini predicting Mortality Measures Over Time", 800, 1500, imagetype = "png")

# _______________________________________________-----
# Figure 2 (NOT USED):  Mortality markers across years --------------
# dev.off()
#par(mar=c(5.1 ,4.1,4.1,2.1), mfrow=c(4,1)) # (bottom, left, top, right)
#par(mar=c(5.1 - 1,4.1,4.1,2.1), mfrow=c(4,1)) # (bottom, left, top, right)

layout(matrix(1:5, ncol = 1), 
       heights = c(0.05, 0.3, 0.3, 0.3, 0.3))
par(oma = c(1, 1, 1, 1))  # Outer margins: bottom, left, top, right
par(cex.axis = 1.3)
par(cex.lab  = 1.3)

#---- Title ----
par(mar = c(0, 0, 0, 0))
plot.new()
mtext(bquote(bold("Figure 2. ") ~ .("Mortality Measures Over Time. The Gini mortality has the strongest positive association with income inequality.")), 
      side = 3, line = -1, cex = 1, adj = 0, outer = FALSE)

# Reset margins for the subsequent panels
par(mar = c(5, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap

df_merged <- df_merged[!is.na(df_merged$YearDeathsAdjusted),]

#---- 1) MortalityAbsoluteAdjusted vs. Year ----
plot(df_merged$Year,
     df_merged$MortalityRate_Adjusted,
     cex = 1.5,
     col = "darkgreen", pch  = 19,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Year",
     ylab = "Mortality\n(age adjusted)",
     main = "")
mtext(bquote(bold("Panel A. ") ~ .("Mortality (age adjusted) Across Years")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

lm_out <- lm(MortalityRate_Adjusted ~ Year, data = df_merged)
lm_sum <- summary(lm_out)
MiddleQuintileMortalitySlope <- lm_sum$coefficients[2,1]

# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2)

slope_pvalue <- lm_sum$coefficients[2,4]
slope_pvalue <- sprintf(slope_pvalue, fmt='%#.3f')
p_val <- lm_sum$coefficients["Year", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
r_sq <- lm_sum$adj.r.squared
cor_value <- r_sq^2
r_sq <- sprintf(r_sq, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.2f')

#* Display summary measures -----
# Line 1: P-value
text(par("usr")[2] - 10, # x max
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value* = ", p_val),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript)
text(par("usr")[2] - 10, # x max
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(R^2 == .(r_sq) ~ ", r = " ~ .(cor_value)),
     cex = 1.5,
     adj = c(0, 1))

#* Print  ---
#function_plot_print("Plot0 - Mortality across years", 600, 800, imagetype = "png")

#---- 2) Mortality Gini vs. Year ----
plot(df_merged$Year,
     df_merged$Gini_Death,
     cex = 1.5,
     col = "red", pch  = 19,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Year",
     ylab = "Mortality \nGini coefficient)",
     main = "")
mtext(bquote(bold("Panel B. ") ~ .("Mortality Gini Across Years")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

# Fit a linear model & extract slope
lm_out <- lm(Gini_Death ~ Year, data = df_merged)
# Draw regression line
abline(lm_out, col = "red", lwd = 2)
lm_sum <- summary(lm_out)

slope_pvalue <- lm_sum$coefficients[2,4]
slope_pvalue <- sprintf(slope_pvalue, fmt='%#.3f')
p_val <- lm_sum$coefficients["Year", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
r_sq <- lm_sum$adj.r.squared
cor_value <- r_sq^2
r_sq <- sprintf(r_sq, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.2f')

#* Display summary measures -----
# Line 1: P-value
text(par("usr")[1] + 1, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("p-value* = ", slope_pvalue),
     cex = 1.5,
     adj = c(0,1))
# Line 2: R² (the 2 in superscript)
text(par("usr")[1] + 1, # x min
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(R^2 == .(r_sq) ~ ", r = " ~ .(cor_value)),
     cex = 1.5,
     adj = c(0, 1))

#---- 3) Mortality_skewness vs. Year ----
plot(df_merged$Year,
     #df_merged$Mortality_Kurtosis,
     df_merged$Mortality_skewness,
     cex = 1.5,
     col = "red", pch  = 19,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Year",
     ylab = "Mortality\nskewness",
     main = "")
mtext(bquote(bold("Panel C. ") ~ .("Mortality Skewness Across Years")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#text(df_merged$Year, df_merged$Mortality_skewness, df_merged$Year, 
#     pos = ifelse(df_merged$Year %in% c(2000, 2002, 2004, 2007, 2014:2016), 3, 4))

# Fit a linear model & extract slope
lm_out <- lm(Mortality_skewness ~ Year, data = df_merged)
# Draw regression line
abline(lm_out, col = "red", lwd = 2)

lm_sum <- summary(lm_out)
slope_pvalue <- lm_sum$coefficients[2,4]
slope_pvalue <- sprintf(slope_pvalue, fmt='%#.3f')
p_val <- lm_sum$coefficients["Year", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
r_sq <- lm_sum$adj.r.squared
cor_value <- r_sq^2
r_sq <- sprintf(r_sq, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.2f')

#* Display summary measures -----
# Line 1: P-value
text(par("usr")[1] + 1, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value* = ", slope_pvalue),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript) AND R
text(par("usr")[1] + 1, # x min
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(R^2 == .(r_sq) ~ ", r = " ~ .(cor_value)),
     cex = 1.5,
     adj = c(0, 1))

#---- 4) Top Quintile Mortality vs. Year ----
plot(df_merged$Year,
     df_merged$TopQuintileMortality,
     cex = 1.5,
     col = "darkgreen", pch  = 19,
     cex.lab = 1.5,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Year",
     ylab = "Mortality\nTop Quintile",
     main = ""
     )

mtext(bquote(bold("Panel D. ") ~ .("Top Quintile Mortality Across Years")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

# Fit a linear model & extract slope
#lm_out <- lm(TopQuintileMortality ~ Year + Mortality_skewness, data = df_merged)
lm_out <- lm(TopQuintileMortality ~ Year, data = df_merged)
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2)

(lm_sum <- summary(lm_out))
TopQuintileMortalitySlope <- lm_sum$coefficients[2,1]
slope_pvalue <- lm_sum$coefficients[2,4]
slope_pvalue <- sprintf(slope_pvalue, fmt='%#.3f')
p_val <- lm_sum$coefficients["Year", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
r_sq <- lm_sum$adj.r.squared
cor_value <- r_sq^2
r_sq <- sprintf(r_sq, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.2f')

#* Display summary measures -----
# Line 1: P-value
text(par("usr")[2] - 10, # x max
     par("usr")[4] - 1.2*strheight('A'), # y max,
     labels = paste0("P-value* = ", slope_pvalue),
     cex = 1.5,
     adj = c(0, 1))

# Line 2: R² (the 2 in superscript)
text(par("usr")[2] - 10, # x max
     par("usr")[4] - 3*strheight('A'), # y max,
     labels = bquote(R^2 == .(r_sq) ~ ", r = " ~ .(cor_value)),
     cex = 1.5,
     adj = c(0, 1))

# Footer -----
mtext(paste0("* Linear regression."),
      side=1, line = 3, cex = 1.2, adj=0)

mtext(paste0("rbadgett@kumc.edu, ", Sys.Date()),
      side=1, line = 4.5, cex = 1.2, adj=1)

#* Print -----
function_plot_print("Figure 2. Mortality Measures Over Time", 800, 1500, imagetype = "png")

# _______________________________________________-----
# Figure 4 (used). Income Gini and outcomes -----------------
layout(matrix(1:5, ncol = 1), 
       heights = c(0.05, 0.3, 0.3, 0.3, 0.3))
par(oma = c(1, 1, 1, 1))  # Outer margins: bottom, left, top, right
par(cex.axis = 1.3)
par(cex.lab  = 1.3)

#---- Title ----
par(mar = c(0, 0, 0, 0))
plot.new()
mtext(bquote(
  bold("Figure 4. ") ~
    "Outcomes of increased economic inequalities: increased economic " #*
    #.(min(df$Year)) ~ " to " ~ .(max(df$Year)) * ")."
),
side = 3, line = -1, adj = 0)
mtext(bquote(
  "hardship and mortality difference in the highest quintile."
),
side = 3, line = -2, adj = 0)

# Reset margins for the subsequent panels
par(mar = c(5, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap

#* Panel A. Gini_Income by Year -----------------
plot(df_merged$Year[!is.na(df_merged$Gini_Income)],
     df_merged$Gini_Income[!is.na(df_merged$Gini_Income)],
     #ylim = c(0,50),
     cex = 1.5,
     col = "darkgreen", pch  = 19,
     cex.lab = 1.25,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Year", # "Income inequality (Gini coefficient",
     ylab = "Income inequality \n(Gini coefficient)",
     main = "")
mtext(bquote(bold("Panel A. ") ~ .("Income inequality by year")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#** Analyses -----
lm_out <- lm(Gini_Income ~ Year, data = df_merged)
(lm_sum <- summary(lm_out))
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2, lty = 1)

#** Display results -----
text(par("usr")[1] + 2, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     "P values: ", adj = 0, font = 2)

text(par("usr")[1] + 2, # x min
     par("usr")[4] - 2*strheight('A'), # y max,
     paste0("Bivariate: ", sprintf(lm_sum$coefficients[2,4], fmt='%#.3f')),
     adj = 0, font = 2)

#* Panel B: Gini_Income vs `Economic_Hardship(0.05) ---------
par(mar = c(5, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap
plot(df_merged$Gini_Income,
     df_merged$`Economic_Hardship(0.5)`,
     cex = 1.5,
     col = ifelse(df_merged$recession_year == TRUE, "darkgreen", "darkgreen"),
     pch  = 19,
     cex.lab = 1.25,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Income disparities (Gini coeficient)",
     ylab = "Economic\nhardship rate (%)",
     main = "")

mtext(bquote(bold("Panel B. ") ~ .("Economic Hardship (income less than 50% of poverty).")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

for (i in 1:2){
  cat("\ni: ", i, "\nx: ",df_merged$Gini_Income[df_merged$Year == i], "y: ",df_merged$`Economic_Hardship(0.5)`[df_merged$Year == i], "\nRecession year: ", df_merged$recession_year[df_merged$Year == i])
  text(df_merged$Gini_Income[df_merged$Year == i], df_merged$`Economic_Hardship(0.5)`[df_merged$Year == i], 
       labels = df_merged$Year[df_merged$Year == i],
       pos = ifelse(df_merged$Year[i] %in% c(2012), 2, 2),
       col = ifelse(df_merged$recession_year[df_merged$Year == i] == TRUE, "red", "black")
       #col = ifelse(df_merged$Year %in% c(2009:2016), "black", "white")
  )
}

#lines(df_merged$Gini_Income, df_merged$`Economic_Hardship(0.5)`)

#** Analyses -----
lm_out <- lm(`Economic_Hardship(0.5)` ~ Gini_Income, 
             data = df_merged)
(lm_sum <- summary(lm_out))
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2, lty = 1)

p_val <- lm_sum$coefficients["Gini_Income", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

#** Display results -----
text(par("usr")[1] + 0.005, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     "P values: ", adj = 0, font = 2)

text(par("usr")[1] + 0.005, # x min
     par("usr")[4] - 2*strheight('A'), # y max,
     paste0("Bivariate: ", sprintf(lm_sum$coefficients[2,4], fmt='%#.3f')),
     adj = 0, font = 2)

# Line 3: R² (the 2 in superscript)
text(par("usr")[1] + 0.005, # x min
     par("usr")[4] - 2.8*strheight('A'), # y max,
     labels = bquote(R^2 == .(sum_R2) ~ ", r = " ~ .(cor_value)),
     cex = 1,
     adj = c(0, 1))

#* Panel C. Gini_Income By Gini_Death -----------------
# Reset margins for panels
par(mar = c(5, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap
plot(df_merged$Gini_Income,
     df_merged$Gini_Death,
     #ylim = c(0,50),
     cex = 1.5,
     col = "darkgreen", pch  = 19,
     cex.lab = 1.25,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Income disparities (Gini coefficient)", # "Income inequality (Gini coefficient",
     ylab = "Mortality\n(Gini coefficient)",
     main = "")
mtext(bquote(bold("Panel C. ") ~ .("Mortality dispersion by income inequality")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

#** Analyses -----
lm_out <- lm(Gini_Death ~ Gini_Income, data = df_merged)
(lm_sum <- summary(lm_out))
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2, lty = 1)

p_val <- lm_sum$coefficients["Gini_Income", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

#** Display results -----
text(par("usr")[1] + 0.005, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     "P values: ", adj = 0, font = 2)

text(par("usr")[1] + 0.005, # x min
     par("usr")[4] - 2.1*strheight('A'), # y max,
     paste0("Bivariate: ", sprintf(lm_sum$coefficients[2,4], fmt='%#.3f')),
     adj = 0, font = 2)

# Line 3: R² (the 2 in superscript)
text(par("usr")[1] + 0.005, # x min
     par("usr")[4] - 2.8*strheight('A'), # y max,
     labels = bquote(R^2 == .(sum_R2) ~ ", r = " ~ .(cor_value)),
     cex = 1,
     adj = c(0, 1))

#** Print ------
#function_plot_print("Figure. Distrbution of mortality", 600,400)

#* Panel D: Gini_Death vs mort difference in quintiles ---------
# Reset margins for panels
par(mar = c(5, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap
plot(df_merged$Gini_Death,
     df_merged$Diff_Top_Mid,
     cex = 1.5,
     col = ifelse(df_merged$recession_year == TRUE, "darkgreen", "darkgreen"),
     pch  = 19,
     cex.lab = 1.25,   # Increase axis labels size
     cex.main = 2,    # Increase main title size
     cex.axis = 1.2,  # Increase tick labels size
     xlab = "Mortality (Gini coefficient)", # "Income inequality (Gini coefficient",
     ylab = "Mortality\n(Q5Q3* difference)",
     main = "")

mtext(bquote(bold("Panel D. ") ~ .("Mortality in the top vs the middle quintile by income inequality.")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

for (i in 1:1){
  cat("\ni: ", i, "\nx: ",df_merged$Gini_Income[df_merged$Year == i], "y: ",df_merged$`Economic_Hardship(0.5)`[df_merged$Year == i], "\nRecession year: ", df_merged$recession_year[df_merged$Year == i])
  text(df_merged$Gini_Income[df_merged$Year == i], df_merged$`Economic_Hardship(0.5)`[df_merged$Year == i], 
       labels = df_merged$Year[df_merged$Year == i],
       pos = ifelse(df_merged$Year[i] %in% c(2012), 2, 2),
       col = ifelse(df_merged$recession_year[df_merged$Year == i] == TRUE, "red", "black")
       #col = ifelse(df_merged$Year %in% c(2009:2016), "black", "white")
  )
}

#lines(df_merged$Gini_Income, df_merged$`Economic_Hardship(0.5)`)

#** Analyses -----
lm_out <- lm(Diff_Top_Mid ~ Gini_Death, 
             data = df_merged)
(lm_sum <- summary(lm_out))
# Draw regression line
abline(lm_out, col = "darkgreen", lwd = 2, lty = 1)

p_val <- lm_sum$coefficients["Gini_Death", "Pr(>|t|)"]
p_val <- sprintf(p_val, fmt='%#.3f')
sum_R2 <- lm_sum$adj.r.squared
cor_value <- sum_R2^2
sum_R2 <- sprintf(sum_R2, fmt='%#.2f')
cor_value <- sprintf(cor_value, fmt='%#.3f')

#** Display results -----
text(par("usr")[1] + 0.00065, # x min
     par("usr")[4] - 1.2*strheight('A'), # y max,
     "P values: ", adj = 0, font = 2)

text(par("usr")[1] + 0.00065, # x min
     par("usr")[4] - 2.1*strheight('A'), # y max,
     paste0("Bivariate: ", sprintf(lm_sum$coefficients[2,4], fmt='%#.3f')),
     adj = 0, font = 2)

# Line 3: R² (the 2 in superscript)
text(par("usr")[1] + 0.00065, # x min
     par("usr")[4] - 2.8*strheight('A'), # y max,
     labels = bquote(R^2 == .(sum_R2) ~ ", r = " ~ .(cor_value)),
     cex = 1,
     adj = c(0, 1))

# Footer -----
mtext(paste0("Notes:"),
      side=1, line = 3.2, cex = 1,, adj=0, font = 2)
mtext(paste0("* Q5Q3. Highest (fifth quintile) versus middle (third quintile)"),
      side=1, line = 4.2, cex = 0.8,, adj=0)

mtext(paste0("rbadgett@kumc.edu, ", Sys.Date()),
      side=1, line = 5.2, cex = 0.8, adj=1)

#** Print ------
function_plot_print("Figure 4. Income Inequalities And Outcomes", 600,1000, "png")
