library(readxl)
library(rstudioapi)
library(magrittr)
library(dplyr) 
library(lubridate)
library(tidyverse)
library(broom)
library(knitr)
library(sandwich)
library(lmtest)

setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

rm(list=ls())


# Function to format results
format_results <- function(summary_obj) {
  tidy_summary <- tidy(summary_obj)
  return(data.frame(
    alpha = tidy_summary$estimate[1],
    alpha_se = tidy_summary$std.error[1],
    alpha_t = tidy_summary$statistic[1],
    beta = tidy_summary$estimate[2],
    beta_se = tidy_summary$std.error[2],
    beta_t = tidy_summary$statistic[2],
    R2 = summary_obj$r.squared
  ))
}


# 1) Stock return predictability with overlapped observations ####

# a (different samples) ####

# Load the data
data2 <- read_excel("US SPX.xlsx", skip = 1)
# Convert date to proper date format and filter for December of each year
data2 <- data2 %>%
  mutate(Date = ymd(paste0(Date, ".01"))) %>%
  filter(month(Date) == 12)
# Calculate log differences for dividends and prices
data2 <- data2 %>%
  arrange(Date) %>%
  mutate(dpt = log(D) - log(P),
         deltad = log(lead(D)) - log(D),
         r = log(lead(P) + lead(D)) - log(P))

# Remove NA values created by lead and lag functions
data2 <- na.omit(data2)


# Dividend growth regressions
result_columns <- list()

horizons <- c(0, 1927, 1947)
for (n in horizons) {
  regression <- lm(deltad ~ dpt, data = data2 %>%
                     filter(year(Date) >= n)) 
  summary <- summary(regression) # Get summary
  results <- format_results(summary) # Get values
  result_columns[[as.character(n)]] <- t(results) # Store result column in the list
}

d_reg_samples = kable(as.data.frame(result_columns) %>%
                        rename('full sample' = 'X0',
                               '1927-' = 'X1927',
                               '1947-' = 'X1947'), format = "markdown")


# Return regressions
result_columns <- list()

horizons <- c(0, 1927, 1947)
for (n in horizons) {
  regression <- lm(r ~ dpt, data = data2 %>%
                     filter(year(Date) >= n)) 
  summary <- summary(regression) # Get summary
  results <- format_results(summary) # Get values
  result_columns[[as.character(n)]] <- t(results) # Store result column in the list
}

r_reg_samples = kable(as.data.frame(result_columns) %>%
                        rename('full sample' = 'X0',
                               '1927-' = 'X1927',
                               '1947-' = 'X1947'), format = "markdown")


# b (different horizons) i) ####

# Load the data
data3 <- read_excel("US SPX.xlsx", skip = 1)
# Convert date to proper date format and filter for December of each year
data3 <- data3 %>%
  mutate(Date = ymd(paste0(Date, ".01"))) %>%
  filter(month(Date) == 12)

# Number of lags
lags <- c(1,2,3,5,10)

# Create new columns with lagged values
for (l in lags) {
  data3 <- data3 %>% 
    mutate(dpt = log(D) - log(P),
           !!paste0("deltad_", l) := log(lead(D, n = l)) - log(D),
           !!paste0("r_", l) := {
             d_sum <- 0
             for (i in 1:l) {
               d_sum <- d_sum + lead(D, n = i)
             }
             log(lead(P, n = l) + d_sum) - log(P)
           })
}

# Remove NA values created by lead and lag functions
data3 <- na.omit(data3)


# Delta Regression Loop
result_columns <- list() # Initialize an empty list to store result columns

n_values <- c(1,2, 3, 5, 10)
for (n in n_values) {
  formula <- as.formula(paste0("deltad_", n, " ~ dpt")) # Construct the formula dynamically
  regression <- lm(formula, data = data3) 
  summary <- summary(regression) # Get summary
  results <- format_results(summary) # Get values
  result_columns[[as.character(n)]] <- t(results) # Store result column in the list
}

d_reg_horizons = kable(as.data.frame(result_columns) %>%
                         rename('1 year' = 'X1',
                                '2 year' = 'X2',
                                '3 year' = 'X3',
                                '5 year' = 'X5',
                                '10 year' = 'X10'), format = "markdown")


# Interpretation
# High D/P today is an indicator for low dividend growth in the future (results are all significant) 


# Return Regression Loop
result_columns <- list() # Initialize an empty list to store result columns

n_values <- c(1,2, 3, 5, 10)
for (n in n_values) {
  formula <- as.formula(paste0("r_", n, " ~ dpt")) # Construct the formula dynamically
  regression <- lm(formula, data = data3) 
  summary <- summary(regression) # Get summary
  results <- format_results(summary) # Get values
  result_columns[[as.character(n)]] <- t(results) # Store result column in the list
}


r_reg_horizons = kable(as.data.frame(result_columns) %>%
                         rename('1 year' = 'X1',
                                '2 year' = 'X2',
                                '3 year' = 'X3',
                                '5 year' = 'X5',
                                '10 year' = 'X10'), format = "markdown")


# Interpretation
# High D/P today is an indicator for high future returns and consequently the risk premia


# ii) More lags + Newey West ####

# dividend growth
n_values <- c(1, 2, 3, 5, 10)


result_columns <- list() # Initialize an empty list to store result columns

for (n in n_values) {
  formula <- as.formula(paste0("deltad_", n, " ~ dpt")) # Construct the formula dynamically
  regression <- lm(formula, data = data3) 
  coeftest <- coeftest(regression, vcov = vcovHC(regression, type = "HC3", L=n))  # Calculate Newey-West standard errors where lags matches horizon
  summary_obj <- summary(regression) # Get summary
  # Overwrite se and t values with Newey-West standard errors and corresponding t-statistics
  summary_obj$coefficients[, "Std. Error"] <- coeftest[, "Std. Error"]
  summary_obj$coefficients[, "t value"] <- coeftest[, "t value"]
  results <- format_results(summary_obj) # Get values
  result_columns[[as.character(n)]] <- t(results) # Store result column in the list
}

d_reg_nw <- kable(as.data.frame(result_columns) %>%
                    rename('1 year' = 'X1',
                           '2 year' = 'X2',
                           '3 year' = 'X3',
                           '5 year' = 'X5',
                           '10 year' = 'X10'), format = "markdown")



# return regression

result_columns <- list() # Initialize an empty list to store result columns

for (n in n_values) {
  formula <- as.formula(paste0("r_", n, " ~ dpt")) # Construct the formula dynamically
  regression <- lm(formula, data = data3) 
  coeftest <- coeftest(regression, vcov = vcovHC(regression, type = "HC3", L=n))  # Calculate Newey-West standard errors where lags matches horizon
  summary_obj <- summary(regression) # Get summary
  # Overwrite se and t values with Newey-West standard errors and corresponding t-statistics
  summary_obj$coefficients[, "Std. Error"] <- coeftest[, "Std. Error"]
  summary_obj$coefficients[, "t value"] <- coeftest[, "t value"]
  results <- format_results(summary_obj) # Get values
  result_columns[[as.character(n)]] <- t(results) # Store result column in the list
}

r_reg_nw <- kable(as.data.frame(result_columns) %>%
                    rename('1 year' = 'X1',
                           '2 year' = 'X2',
                           '3 year' = 'X3',
                           '5 year' = 'X5',
                           '10 year' = 'X10'), format = "markdown")





# Task 2 ####

# Load the data
data5 <- read_excel("US SPX.xlsx", skip = 1)
# Convert date to proper date format and filter for December of each year
data5 <- data5 %>%
  mutate(Date = ymd(paste0(Date, ".01"))) %>%
  filter(month(Date) == 12)


# Initialize lists to store results
br_values <- c()
br_se_values <- c()
br_implied_values <- c()


horizons <- c(0, 1927, 1947)
for (n in horizons) {
  data = data5 %>%
    arrange(Date) %>%
    filter(year(Date) >= n) %>%
    mutate(dpt = log(D) - log(P),
           deltad = log(lead(D)) - log(D),
           r = log(lead(P) + lead(D)) - log(P),
           dpt1 = lead(dpt))
  
  # Remove NA values created by lead and lag functions
  data <- na.omit(data)
  
  # Regressions
  
  # calculate bd
  regression <- lm(deltad ~ dpt, data = data) 
  bd <- summary(regression)$coefficients["dpt", "Estimate"]
  bd_se <- summary(regression)$coefficients["dpt", "Std. Error"]
  
  # calculate bdp (which is basically dividend-yield autocorrelation)
  regression <- lm(dpt1 ~ dpt, data = data) 
  bdp <- summary(regression)$coefficients["dpt", "Estimate"]
  
  # calculate p from the mean P/D ratio
  mean_div_yield = mean(data$P)/mean(data$D)
  rho <- mean_div_yield / (1 + mean_div_yield)
  
  # br implied = 1 + bd - p*bdp
  br_implied = 1 + bd -rho*bdp
  
  # calculate acutal br
  regression <- lm(r ~ dpt, data = data) 
  br <- summary(regression)$coefficients["dpt", "Estimate"]
  br_se <- summary(regression)$coefficients["dpt", "Std. Error"]
  
  # Append values to lists
  br_values <- c(br_values, br)
  br_se_values <- c(br_se_values, br_se)
  br_implied_values <- c(br_implied_values, br_implied)
  
}


# Combine lists into a data frame
result_table <- data.frame(
  period = c('full sample', '1927-', '1947-'),
  br = br_values,
  br_se = br_se_values,
  br_implied = br_implied_values
)

reg_implied <- kable(result_table, format = "markdown")





# Interpretation

# Both coefficients (br and br_implied) point in the direction of return predictability but based on different rationales.
# To test stock market predictability, one would have to check the null hypothesis of br=0
# based on the regression above however, the br coefficient is significant at the 99% confidence level.
# Therefore the br coefficent would strengthen the belief of stock market predictability.

# The equation br_implied = 1 + bd - p*bdp indicates that under the assumption of a non explosive bdp,
# then bd or br has to be different from 0. Looking at the regression of deltad ~ dpt, there is little evidence
# of divident growth predictability, as the coefficient bd is not significantly different from zero. 
# Consequently, br_implied has to be different from zero.




# If br and br_implied are close to each other, this should strengthen the believe in stock market predictablity,
# market participants' implied expectations align closely with what the regression model estimates based on historical data.


# 3) Stock return predictability for non-US equity markets ####

# Load the data
data4 <- read_excel("problem two.xlsx")


data4 <- data4 %>%
  rename(dpt = `D(t)/P(t)`) %>%
  mutate(dpt = log(dpt))


# define parameters
add_lags <- c(2,3,5,10)
n_values <- c(1,2, 3, 5, 10)
horizons <- c(0, 1927, 1947)


# Function for data cleaning and transformation

data_transformation <- function(data, country_name, add_lags) {
  data_country <- data %>%
    filter(country == country_name) %>%
    mutate(
      r_1 = log(lead(`R(t)-1`)+1),  # compute one year log returns
      deltad_1 = log(lead(dpt)/dpt*(lead(`P(t)/P(t-1) -1`)+1))  # compute 1-year dividend growth rate
    )
  
  for (l in add_lags) {   # create the d_t and r_t s
    data_country <- data_country %>%   
      mutate(
        !!paste0("deltad_", l) := {
          cumulative_sum <- 0
          for (i in 0:(l-1)) {
            cumulative_sum <- cumulative_sum + lead(deltad_1, n = i)
          }
          cumulative_sum # deltad_t is the summation of the log dividend growth rates up to t-1 (we sum up t growth rates)
        },
        !!paste0("r_", l) := {
          cumulative_prod <- 1
          for (i in 0:(l-1)) {
            cumulative_prod <- cumulative_prod * exp(lead(r_1, n = i))
          }
          log(cumulative_prod) # r_t is the log of the multiplication of t returns
        }
      )
  }
  
  data_country <- na.omit(data_country)  # Remove NA values
  data_country <- mutate_all(data_country, ~ replace(., is.infinite(.), 0)) # remove infitine numbers
  
  return(data_country)
}




# Function to construct regressions

regression_function <- function(data, horizons, n_values) {
  
  regression_results <- list()
  
  
  #a) different samples
  
  # Dividend growth regressions
  result_columns <- list()
  
  for (n in horizons) {
    regression <- lm(deltad_1 ~ dpt, data = data %>%
                       filter(year >= n)) 
    summary <- summary(regression) # Get summary
    results <- format_results(summary) # Get values
    result_columns[[as.character(n)]] <- t(results) # Store result column in the list
  }
  
  d_reg_samples = kable(as.data.frame(result_columns) %>%
                          rename('full sample' = 'X0',
                                 '1927-' = 'X1927',
                                 '1947-' = 'X1947'), format = "markdown") 
  
  regression_results[['d_reg_samples']] = d_reg_samples
  
  
  # Return regressions
  result_columns <- list()
  
  for (n in horizons) {
    regression <- lm(r_1 ~ dpt, data = data %>%
                       filter(year >= n)) 
    summary <- summary(regression) # Get summary
    results <- format_results(summary) # Get values
    result_columns[[as.character(n)]] <- t(results) # Store result column in the list
  }
  
  r_reg_samples = kable(as.data.frame(result_columns) %>%
                          rename('full sample' = 'X0',
                                 '1927-' = 'X1927',
                                 '1947-' = 'X1947'), format = "markdown")
  
  regression_results[['r_reg_samples']] = r_reg_samples
  
  
  
  
  # b) different horizons
  # i)
  
  # Delta Regression Loop
  result_columns <- list() # Initialize an empty list to store result columns
  
  for (n in n_values) {
    formula <- as.formula(paste0("deltad_", n, " ~ dpt")) # Construct the formula dynamically
    regression <- lm(formula, data = data) 
    summary <- summary(regression) # Get summary
    results <- format_results(summary) # Get values
    result_columns[[as.character(n)]] <- t(results) # Store result column in the list
  }
  
  d_reg_horizons = kable(as.data.frame(result_columns) %>%
                           rename('1 year' = 'X1',
                                  '2 year' = 'X2',
                                  '3 year' = 'X3',
                                  '5 year' = 'X5',
                                  '10 year' = 'X10'), format = "markdown")
  
  regression_results[['d_reg_horizons']] = d_reg_horizons
  
  
  # Return Regression Loop
  result_columns <- list() # Initialize an empty list to store result columns
  
  for (n in n_values) {
    formula <- as.formula(paste0("r_", n, " ~ dpt")) # Construct the formula dynamically
    regression <- lm(formula, data = data) 
    summary <- summary(regression) # Get summary
    results <- format_results(summary) # Get values
    result_columns[[as.character(n)]] <- t(results) # Store result column in the list
  }
  
  
  r_reg_horizons = kable(as.data.frame(result_columns) %>%
                           rename('1 year' = 'X1',
                                  '2 year' = 'X2',
                                  '3 year' = 'X3',
                                  '5 year' = 'X5',
                                  '10 year' = 'X10'), format = "markdown")
  
  regression_results[['r_reg_horizons']] = r_reg_horizons
  
  
  
  # ii) More lags + Newey West
  
  # dividend growth
  
  result_columns <- list() # Initialize an empty list to store result columns
  
  for (n in n_values) {
    formula <- as.formula(paste0("deltad_", n, " ~ dpt")) # Construct the formula dynamically
    regression <- lm(formula, data = data) 
    coeftest <- coeftest(regression, vcov = vcovHC(regression, type = "HC3", L=n))  # Calculate Newey-West standard errors where lags matches horizon
    summary_obj <- summary(regression) # Get summary
    # Overwrite se and t values with Newey-West standard errors and corresponding t-statistics
    summary_obj$coefficients[, "Std. Error"] <- coeftest[, "Std. Error"]
    summary_obj$coefficients[, "t value"] <- coeftest[, "t value"]
    results <- format_results(summary_obj) # Get values
    result_columns[[as.character(n)]] <- t(results) # Store result column in the list
  }
  
  d_reg_nw <- kable(as.data.frame(result_columns) %>%
                      rename('1 year' = 'X1',
                             '2 year' = 'X2',
                             '3 year' = 'X3',
                             '5 year' = 'X5',
                             '10 year' = 'X10'), format = "markdown")
  
  regression_results[['d_reg_nw']] = d_reg_nw
  
  
  # return regression
  
  result_columns <- list() # Initialize an empty list to store result columns
  
  for (n in n_values) {
    formula <- as.formula(paste0("r_", n, " ~ dpt")) # Construct the formula dynamically
    regression <- lm(formula, data = data) 
    coeftest <- coeftest(regression, vcov = vcovHC(regression, type = "HC3", L=n))  # Calculate Newey-West standard errors where lags matches horizon
    summary_obj <- summary(regression) # Get summary
    # Overwrite se and t values with Newey-West standard errors and corresponding t-statistics
    summary_obj$coefficients[, "Std. Error"] <- coeftest[, "Std. Error"]
    summary_obj$coefficients[, "t value"] <- coeftest[, "t value"]
    results <- format_results(summary_obj) # Get values
    result_columns[[as.character(n)]] <- t(results) # Store result column in the list
  }
  
  r_reg_nw <- kable(as.data.frame(result_columns) %>%
                      rename('1 year' = 'X1',
                             '2 year' = 'X2',
                             '3 year' = 'X3',
                             '5 year' = 'X5',
                             '10 year' = 'X10'), format = "markdown")
  
  regression_results[['r_reg_nw']] = r_reg_nw
  
  
  return(regression_results)
  
}





# Loop over all countries

result_list = list()

for (i in unique(data4$country)) {
  data = data_transformation(data4, i, add_lags)
  result_list[[i]] = regression_function(data, horizons, n_values)
}
