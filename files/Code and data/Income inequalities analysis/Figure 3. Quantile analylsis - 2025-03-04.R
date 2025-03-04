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


## Libraries ------

library(epiR)
library(DescTools)
library(metafor)

## Parameters for CDC Wonder mortality-----
# Data starts 1968
# Consider starting with 1999 ( 20 years) or 2000
Start_year <- function_tcltk_input (1990)
Start_year <- as.numeric(Start_year)

mortality_type <- tk_select.list(c('Age Adjusted Rate', 'Crude Rate'), 
                                 preselect = 'Age Adjusted Rate', 
                                 multiple = FALSE,
                                 title = paste0("\n\n", Sys.info()["user"], ":\n\nWhat are we studying?\n\n"))

## Data grab ------
# df_death_yearly is from the R script, "Death rates by income (national, two multipanel)"
df_death_yearly2 <- df_death_yearly[df_death_yearly$Year >= Start_year, ]

write.xlsx(df_death_yearly2, paste0("CDC Wonder\\CDC-Wonder-df_death_yearly2(", mortality_type," since ", Start_year,")-", Sys.Date(),".xlsx"))

# Plot ------
par(mar=c(5.1 + 2,4.1 + 2,4.1,2.1), mfrow=c(1,1)) # (bottom, left, top, right)

# Calculate the overall y-axis range from the three variables:
yrange <- range(c(df_death_yearly2$TopQuintileMortality,
                  df_death_yearly2$MidQuintileMortality,
                  df_death_yearly2$BottomQuintileMortality,
                  df_death_yearly2$Diff_Top_Mid),
                na.rm = TRUE)

# Plot TopQuintileMortality over time (red line)
plot(df_death_yearly2$Year, df_death_yearly2$TopQuintileMortality,
     type = "l",
     col = "red",
     lwd = 2,
     ylim = yrange,
     xlab = "Year",
     ylab = paste0("Mortality, ", mortality_type, "\n(per 100,000)"),
     main = "")
mtext(bquote(bold("Figure 1. ") ~ "Mortality (" * .(mortality_type) * " per 100,000) by quintile since" ~ .(Start_year) * ". The difference between"), 
      side = 3, line = 2, at = par("usr")[1] - 3, cex = 1, adj = 0, outer = FALSE)
mtext("the top and middle quintiles is significant, increasing by 0.24% yearly, and leads to a 15-year lag in parity.",
      side = 3, line = 1, at = par("usr")[1] - 3, cex = 1, adj = 0, outer = FALSE)

# Add Middle Quintile Mortality (blue line)
lines(df_death_yearly2$Year, df_death_yearly2$MidQuintileMortality,
      col = "blue", lwd = 2)

# Add Bottom Quintile Mortality (green line)
#lines(df_death_yearly2$Year, df_death_yearly2$BottomQuintileMortality,
#      col = "green", lwd = 2)

# Add top-mid difference Mortality (green line)
lines(df_death_yearly2$Year, df_death_yearly2$Diff_Top_Mid,
      col = "black", lwd = 3)

# Add a legend in the top right
legend(2011, 700,
       legend = c("Difference", "Top Quintile", "Middle Quintile"),
       col = c("black", "red", "blue"),
       lwd = c(3,2,2))

# Analyses -----
temp <- df_death_yearly2$Diff_Top_Mid[nrow(df_death_yearly2)]*100000
cat("Death rate in middle quartile FINAL year (per 100,000 population): ", round(temp,1))
temp <- df_death_yearly2$TopQuintileMortality[nrow(df_death_yearly2)] / df_death_yearly2$BottomQuintileMortality[nrow(df_death_yearly2)]
cat("Death relative risk (top quartile / middle quartile) in final year: ", round(temp,2))

#* Perform a paired t-test on these rates across years -----
ttest_result <- t.test(df_death_yearly2$top_deaths, 
                       df_death_yearly2$mid_deaths,
                       paired = TRUE)
p_t <- sprintf(ttest_result$p.value, fmt='%#.3f')

#* Perform a MH chi-square on these rates across years -----
k <- nrow(df_death_yearly2)
arr <- array(NA, dim = c(2, 2, k))  # Create a 3D array with dimensions 2 x 2 x k

# For example, if 'arr' is built like this:
for(i in 1:k) {
  arr[1,1,i] = df_death_yearly2$top_deaths[i]
  arr[1,2,i] = df_death_yearly2$top_pop[i] - df_death_yearly2$top_deaths[i]
  arr[2,1,i] = df_death_yearly2$mid_deaths[i]
  arr[2,2,i] = df_death_yearly2$mid_pop[i] - df_death_yearly2$mid_deaths[i]
}

# SLOW PROCESSING HERE!!
message(paste0("\033[32m \033[1mThis next line of code may be slow to run!!\012\033[0m"))
mh_rr <- epi.2by2(arr, method = "cohort.count", conf.level = 0.95)
mh_results <- paste0(round(mh_rr$massoc.summary[2,2],3),
               " (",round(mh_rr$massoc.summary[2,3],3)," - ",
               round(mh_rr$massoc.summary[2,4],3),")")

#* Diff_Top_Mid regression -----
# log_RR is from R script, "Death rates by income..."
res <- metafor::rma(yi = log_RR, sei = se_log_RR, mods = ~ Year, data = df_death_yearly2, method = "REML")
res_sum <- summary(res)

# Extract the coefficient for Year:
year_coef <- coef(res)["Year"]

ci_full <- confint(res, fixed = TRUE)
ci_fixed_only <- ci_full$fixed
ci_year <- ci_fixed_only["Year", ]

# Exponentiate to interpret as multiplicative change in the RR per year:
annual_change <- exp(year_coef)
annual_change_ci <- exp(ci_year)

cat("The meta-regression of log(RR) on Year yields a slope of", round(year_coef, 4),
    "\nwhich corresponds to an annual multiplicative change in the RR of", round(annual_change, 3),
    "\n95% CI: [", round(annual_change_ci[2], 3), ",", round(annual_change_ci[3], 3), "]\n")

yearlyRR <- paste0("Yearly increase (RR†): ", # † \u2020
       round(annual_change, 3),
      " (", 
      round(annual_change_ci[2], 3), ",", round(annual_change_ci[3], 3), ")")

yearlySlope<- paste0("Yearly increase (slope): ", 
       round(100*ci_year[1], 2),
       "% (", 
       round(100*ci_year[2], 2), "% to ", round(100*ci_year[3], 2), "%)")

rrrrr <- bquote(
  R^2 == .(round(res$R2 / 100, 2)) ~ ", r = " ~ .(round(sqrt(res$R2 / 100), 2))
  )

# Display analyses results -----
#text(2011, 0.003, "P values for difference:", adj = 0, font = 2)
#text(2011, 0.003 - 1.2*strheight("A"), paste0("Paired t-test: ", p_t), adj = 0)
#text(2011, 0.003 - 2.5*strheight("A"), paste0("Year (regression): ", p_r), adj = 0)
text(2006, 450, "Analyses of top vs middle quintiles: ", adj = 0, font = 2)
text(2006, 450 - 1.5*strheight("A"), paste0("Relative risk of death (MH*): ", mh_results),adj = 0)
text(2006, 450 - 3.25*strheight("A"), yearlyRR, adj = 0)
text(2006, 450 - 5*strheight("A"), yearlySlope, adj = 0)
text(2006, 450 - 6.75*strheight("A"), rrrrr, adj = 0)

# Footnotes
mtext(paste0("Notes:"),
      side=1, line = 3, font = 2, cex = 1, adj=0)
mtext(paste0("* MH. Mantel-Haenszel relative risk."),
      side=1, line = 4, cex = 1, adj=0)
mtext(paste0("† RR. Risk ratio."), # \u2020 †
      #family="symbol",
      side=1, line = 5, cex = 1, adj=0)

windowsFonts(AUMS = windowsFont("Arial Unicode MS"))
#install.packages("showtext")  # one-time install
library(showtext)
showtext_auto() 

# Footer -----
mtext(paste0("rbadgett@kumc.edu, ", Sys.Date()),
      side=1, line = 7, cex = 1, adj=1)

# Print -----
function_plot_print("Figure 3. Mortality (Adjusted) By Quintile Over Time", 800, 600, imagetype = "png")

stop()
Me:
Attached plot does not seem correct as in the most recent years, the bottom quintile has more mortality than the middle quintile.

ChatGPT:
  It’s not necessarily an error. What’s happening is that you are defining the quintiles based on “Crude Rate” (AAR) for each state, ignoring population size, and then summing Deaths and Population for those states that fall in each quintile. In some years, it’s entirely possible that the “bottom quintile” of states (lowest AAR) ends up with a higher overall mortality rate (
    ∑ Deaths / ∑ Population
    ∑Population
    ∑Deaths​
  ) than the “middle quintile,” especially if large-population states with slightly higher raw death counts end up in that bottom group.

Below are two common reasons why this can happen, plus a population-weighted approach if you want the quintile cutoffs to reflect actual population distribution:
  
1) Quintiles Are Based on State-Level AAR, Not Population

2) “Crude Rate” vs. Actual Deaths/Population

How to Use Population-Weighted Quintiles
If your goal is to ensure that each quintile truly represents, say, the bottom 20% of the population by “Crude Rate,” you’d need a population-weighted ranking. In other words, you’d sort states from lowest to highest AAR and then “accumulate” states in that order until you reach 20% of the total population, 40%, etc. This is more complex than a simple “quantile()” call on the state-level AAR, but it ensures that each quintile represents 20% of people, not 20% of states.

One approach is:....

SWITCH TO???:
  2) “Crude Rate” vs. Actual Deaths/Population


