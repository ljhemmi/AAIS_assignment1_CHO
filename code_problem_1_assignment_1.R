## 0) setup ----
# load libraries
library(tidyverse)
library(rstudioapi)
library(knitr)
library(kableExtra)
library(readxl)
library(moments)
library(MASS)
library(PerformanceAnalytics)
library(corrplot)
library(quadprog)

# clear environmental variables
rm(list=ls())

# set directory
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# required for correct date parsing
Sys.setlocale("LC_TIME", "English")

# Problem 1: ----
## 1) Summary Statistics ----

# Initialize a list to store all available data from the excel
consolidated_data <- list()

# Load the Excel file and get the names of the first seven sheets
file_path <- "problem one.xlsx"
sheet_names <- head(excel_sheets(file_path), 7)

# looping through all excel sheets, performing data cleaning & return calculation
# in a final step the cleaned data is added to the consolidated_data list
for (sheet in sheet_names) {
  # Read the specific sheet
  data <- read_excel(file_path, sheet = sheet)
  
  # Ensure that the Price column is treated as numeric
  data$Price <- as.numeric(as.character(data[[2]]))
  
  # Handle potential NA or non-numeric conversion issues
  if (any(is.na(data$Price))) {
    warning(paste("NA values found in the Price column of sheet", sheet))
  }
  
  # ensure that date column is parsed correctly
  data$Date <- as.Date(data$Date, format = "%b %d, %Y")
  
  # sort dates chronologically (from oldest to newest)for return calculation
  data = data[order(data$Date), ]
  
  # Calculate weekly returns
  temp_returns <- data.frame(Date = data$Date[-1], 
                             Return = log(data$Price[-1] / data$Price[-length(data$Price)]))
  # for discrete returns: Return = diff(data$Price)/data$Price[-length(data$Price)]
  # for cont. returns: Return = log(data$Price[-1] / data$Price[-length(data$Price)])
  
  data <- data %>% left_join(temp_returns, by = "Date") %>% 
    drop_na("Return")                                     
  
  # add current risky assets data-frame to the pre-allocated nested list
  consolidated_data[[sheet]] <- data
}

# when looking at the tail of the data frame we see that we have some suspicious
# returns for SP500, US GOVT, & MSCI EFA which are all equal to 0 which is caused
# by a duplicate price from 2024-02-26 to 2024-03-01
tail(consolidated_data$SP500)
tail(consolidated_data$`US GOVT`)
tail(consolidated_data$`MSCI EFA`)

# this is not the case for the other assets
tail(consolidated_data$`Crude Oil`)
tail(consolidated_data$`RUSSELL 2000`)
tail(consolidated_data$Bitcoin)
tail(consolidated_data$USDJPY)

# to increase data quality we drop the last row of observations for these three assets
consolidated_data$SP500 <- consolidated_data$SP500[-nrow(consolidated_data$SP500),]
consolidated_data$`US GOVT` <- consolidated_data$`US GOVT`[-nrow(consolidated_data$`US GOVT`),]
consolidated_data$`MSCI EFA` <- consolidated_data$`MSCI EFA`[-nrow(consolidated_data$`MSCI EFA`),]

# compute summary statistics for the assets
summary_stats <- data.frame()

# loop through each asset to get the summary stats
for (asset_name in names(consolidated_data)) {
  temp_summary_stat <- data.frame(Asset = asset_name,
                                  n_obs = length(consolidated_data[[asset_name]]$Return),
                                  first_obs = min(consolidated_data[[asset_name]]$Date),
                                  last_obs = max(consolidated_data[[asset_name]]$Date),
                                  Mean = mean(consolidated_data[[asset_name]]$Return, na.rm = TRUE),
                                  SD = sd(consolidated_data[[asset_name]]$Return, na.rm = TRUE),
                                  Skewness = skewness(consolidated_data[[asset_name]]$Return, na.rm = TRUE),
                                  Kurtosis = kurtosis(consolidated_data[[asset_name]]$Return, na.rm = TRUE),
                                  Min = min(consolidated_data[[asset_name]]$Return, na.rm = TRUE),
                                  Q5 = unname(quantile(consolidated_data[[asset_name]]$Return, 0.05, na.rm = TRUE)),
                                  Q25 = unname(quantile(consolidated_data[[asset_name]]$Return, 0.25, na.rm = TRUE)),
                                  Q50 = unname(quantile(consolidated_data[[asset_name]]$Return, 0.50, na.rm = TRUE)),
                                  Q75 = unname(quantile(consolidated_data[[asset_name]]$Return, 0.75, na.rm = TRUE)),
                                  Q95 = unname(quantile(consolidated_data[[asset_name]]$Return, 0.95, na.rm = TRUE)),
                                  Max = max(consolidated_data[[asset_name]]$Return, na.rm = TRUE)
  )
  
  summary_stats <- rbind(summary_stats, temp_summary_stat)
}

# print output summary_stats
print(summary_stats)

# print a nicer table (maybe need to change to format = "latex" for pdf)
summary_stats_table <- kable(summary_stats, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))
print(kable_table)


## 2) Calculate Mean and Variance-Covariance Matrix to fit the Gaussian ----

### 2.1) aligning the return data ----
# from the summary stats we know the first & last observation available
first_date <- min(summary_stats$first_obs)
last_date <- max(summary_stats$last_obs)

# Generate the sequence of (daily) dates
dates_vector <- seq.Date(from = first_date, to = last_date, by = "day")

# this will be used to create a long data-frame where we can left_join()
# all the available risky asset return data to (by the date columns).
returns_daily_dates <- data.frame(Date = dates_vector)

# loop through all risky assets and join them to the daily dates
for (asset_name in names(consolidated_data)) {
  returns_daily_dates <- returns_daily_dates %>% 
    left_join(consolidated_data[[asset_name]][,c("Return","Date")] %>% 
                rename(!!asset_name := Return),
              by = "Date")
}

### 2.2) Mean and Var-Cov Matrix for exact match ----
# as a baseline comparison, look at dates that match exactly
returns_weekly_exact_intersect <- drop_na(returns_daily_dates)

# get the means and covariance matrix from the exact matching
mean_returns_exact_match <- colMeans(returns_weekly_exact_intersect[,-1], na.rm = TRUE)
cov_matrix_exact_match <- cov(returns_weekly_exact_intersect[,-1])

# number of observations with exact match
nrow(returns_weekly_exact_intersect)

# given the very limited number of exact matches and the fact that many mismatches 
# are due to a 1 day difference, we suggest to correct the data-set through
# forward filling the available data

### 2.2) Mean and Var-Cov Matrix for corrected (forward filled) match ----
# when looking at the available dates for the last 3 weeks we see that only
# RUSSEL 2000, Bitcoin & USDJPY have data available, which is also when the last
# Monday is in our dataset
tail(returns_daily_dates,8)

# because of this we drop the last 6 weeks from the tail of our data set and end 
# with the available data on 2024-02-26
returns_daily_dates <- returns_daily_dates[1:(nrow(returns_daily_dates)-6),]

# to not drop as many observations, forward fill the available dates and 
# determine the dates that are end-of-week.
# Why choose Monday? We have data either on Monday or Sunday (but Sunday
# is from the closing price on Friday) if we took Sunday we would compare the returns
# forward filled from Friday from the previous week with data from Monday of the current week
returns_daily_dates_forward_filled <- returns_daily_dates %>% 
  fill(everything(), .direction = "down") %>% 
  mutate(end_of_week = if_else(weekdays(returns_daily_dates$Date) == "Monday",1,0))

# filter to only get one value per week and drop the end_of_week helper column
returns_weekly_corrected <- returns_daily_dates_forward_filled[returns_daily_dates_forward_filled$end_of_week == 1,
                                                               -ncol(returns_daily_dates_forward_filled)]

# reset row names to ensure that the index is from 1 to n
row.names(returns_weekly_corrected) <- NULL

# get the means and covariance matrix from the corrected matching.
# We are using the intersection here (through dropping all rows that contain NA)
# since we would not be able to calculate the variance covariance matrix with NA cells

returns_weekly_corrected_intersect <- drop_na(returns_weekly_corrected)
mean_returns_corrected_match <- colMeans(returns_weekly_corrected_intersect[,-1], na.rm = TRUE)
cov_matrix_corrected_match <- cov(returns_weekly_corrected_intersect[,-1])

print(mean_returns_corrected_match)
print(cov_matrix_corrected_match)

# print a nicer table (maybe need to change to format = "latex" for pdf)
kable_table_mean_returns <- kable(mean_returns_corrected_match, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))
print(kable_table_mean_returns)

kable_table_cov_mat <- kable(cov_matrix_corrected_match, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))
print(kable_table_cov_mat)

## 3) Whole Sample Data Portfolio Optimization ----
# assumptions:
# 1 quarter = 13 weeks
# risk-free rate is used to determine excess returns
# thus the portfolio optimization is between risky assets

### 3.1) Get the risk free rates in correct format ----
risk_free_data <- read_excel(file_path, sheet = "US Risk-free 1yr")%>% 
  rename("Date" = "observation_date")

# order the risk free rates from oldest to newest
risk_free_data <- risk_free_data[order(risk_free_data$Date), ]

# turn the risk free yield into decimal values
risk_free_data$risk_free_yield <- risk_free_data$`yield (in %, and annualized)`/100

# turn the annualized yield into quarterly and weekly yield
risk_free_data$quarterly_risk_free_yield <- (1 + risk_free_data$risk_free_yield)^(1/4) - 1
risk_free_data$weekly_risk_free_yield <- (1 + risk_free_data$risk_free_yield)^(1/52) - 1

# turn the risk free rates into continuous returns
risk_free_data$quarterly_risk_free_yield <- log(1+risk_free_data$quarterly_risk_free_yield)
risk_free_data$weekly_risk_free_yield <- log(1+risk_free_data$weekly_risk_free_yield)

# for this exercise assume the last observation is the risk-free yield
last_obs_risk_free_quarterly <- tail(risk_free_data$quarterly_risk_free_yield, n = 1)
last_obs_risk_free_weekly <- tail(risk_free_data$weekly_risk_free_yield, n = 1)

last_obs_risk_free_quarterly
last_obs_risk_free_weekly

### 3.2) mean variance portfolio optimization using quadprog  ----

#### 3.2.1) myopic approach (using weekly returns)
Dmat <- cov_matrix_corrected_match # define matrix to minimize
dvec <- (mean_returns_corrected_match - last_obs_risk_free_weekly) # define vector to maximize (excess returns)
Amat <- cbind(1, diag(7)) # constraint matrix: full investment & non-negative weights
bvec <- c(1, rep(0, 7)) # initial weights that the quadprog optimizes

# calculate the weights using quadprog
results <- solve.QP(Dmat = Dmat, 
                    dvec = dvec, 
                    Amat = Amat, 
                    bvec = bvec, 
                    meq = 1,
                    factorized = T)

# get the portfolio weights
portfolio_weights <- results$solution
rounded_portfolio_weights <- round(portfolio_weights,10)
# set the column names to the assets
names(rounded_portfolio_weights) <- names(mean_returns_corrected_match)

# print resulting weights
print(rounded_portfolio_weights)

# print a nicer table (maybe need to change to format = "latex" for pdf)
kable_table_portfolio_weights <- kable(rounded_portfolio_weights, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))
print(kable_table_portfolio_weights)



## 4) Portfolio Optimization with "Expanding Window Approach" ----
# join the weekly and quarterly risk free rate to the return data-frame
# so that we know them for each week
returns_weekly_corrected <- returns_weekly_corrected %>% 
  left_join(risk_free_data[,c("Date","weekly_risk_free_yield")], by = "Date") %>%  # add the weekly risk free rate
  fill("weekly_risk_free_yield",.direction = "down") %>% # forward fill the weekly risk free data
  left_join(risk_free_data[,c("Date","quarterly_risk_free_yield")], by = "Date") %>% # same for the quarterly risk-free
  fill("quarterly_risk_free_yield",.direction = "down") # forward fill the quarterly risk free data

# get the last date in 2000 as a starting point for portfolio allocation
last_date_2000 <- as.Date(max(subset(returns_weekly_corrected, format(Date, "%Y") == "2000")$Date, na.rm = TRUE))

# define the last date of Q3 2023
end_date_2023_Q3 <- as.Date("2023-09-25") 

# generate all rebalancing dates from last_date_2000 every 13 weeks till end_date_2023_Q3
rebalance_dates <- seq(from = last_date_2000, to = end_date_2023_Q3, by = "13 weeks")

# create the 'rebalance_date' column by checking if each date in the data is a rebalance date
returns_weekly_corrected$rebalance_date <- ifelse(returns_weekly_corrected$Date %in% rebalance_dates, 1, 0)

# create a new output column(s) with the weights determined by portfolio optimization
returns_weekly_corrected$mean_variance_weights <- NA

# filter to have the assets that have data at the start of analysis
# extract the row for the specific date
# assumption: while we only consider assets as investable after 52 weeks of data
# this initial set is defined by the available assets at last_date_2000. Otherwise,
# Crude Oil would have been ignored for the first optimization.

# look at the starting row to define all assets with non NA returns
starting_row_data <- returns_weekly_corrected[returns_weekly_corrected$Date == last_date_2000, sheet_names]
# find which values are not NA
not_na_columns <- !is.na(starting_row_data)
# get the names of the columns that are not NA
non_na_col_names_2000 <- names(starting_row_data)[not_na_columns]

# based on these assets filter the full returns to only start where data is 
# available for the initially investable assets. Thus the considered return data
# start on 2000-08-28, where we have intersecting data for the available assets in
# 2000-12-25
returns_weekly_filtered = returns_weekly_corrected[complete.cases(returns_weekly_corrected[, names(returns_weekly_corrected[returns_weekly_corrected$Date == last_date_2000,])[!is.na(returns_weekly_corrected[returns_weekly_corrected$Date == last_date_2000,])]]), ]
row.names(returns_weekly_filtered) <- NULL

# loop to perform the expanding window approach
results_list <- list()  # to store weights of mean variance optimization
results_list_equally_weighted <- list()  # to store weights of 1/N

for (date in rebalance_dates){
  # truncate the returns available up until date
  temp_return_data <- returns_weekly_filtered %>% filter(Date <= as.Date(date))
  
  # we need to consider for each rebalancing date the available assets to invest
  temp_available_assets <- data.frame(Date = temp_return_data$Date)
  
  for (asset in sheet_names){
    
    # consider the investable assets at the beginning of the optimization
    if (asset  %in%  non_na_col_names_2000){ 
      temp_available_assets <- temp_available_assets %>% 
        left_join(temp_return_data[,c("Date",asset)], by = "Date")
      
      # else check if there is at least 1 year of return data available to 
      # determine if a "new" asset is added
    } else { 
      if (sum(!is.na(temp_return_data[,asset])) >= 52){
        # if this is the case, add it to the available assets for portfolio opt.
        temp_available_assets <- temp_available_assets %>% 
          left_join(temp_return_data[,c("Date",asset)], by = "Date")
      } else{
        # ignore the asset
      }
    }
  }
  
  # define the subset where we have an intersect of the return data
  temp_available_assets_no_na <- drop_na(temp_available_assets)
  
  # transform them into monthly returns (multiplying continuous by T = 13)
  temp_available_assets_no_na[,-1] <- temp_available_assets_no_na[,-1]*13
  temp_cov_matrix <- cov(temp_available_assets_no_na[,-1])
  temp_means <- colMeans(temp_available_assets_no_na[,-1])
  
  # keep the risk_free constant at the last observed value
  temp_quarterly_risk_free <- tail(temp_return_data$quarterly_risk_free_yield,1)
  
  # get excess returns
  temp_excess_returns <- temp_means - temp_quarterly_risk_free
  
  # portfolio optimization given available assets:
  Dmat <- temp_cov_matrix
  dvec <- temp_excess_returns
  Amat <- cbind(1, diag(ncol(temp_available_assets)-1))
  bvec <- c(1, rep(0, (ncol(temp_available_assets)-1)))
  
  # solve using quadprog
  temp_results <- solve.QP(Dmat = Dmat, 
                           dvec = dvec, 
                           Amat = Amat, 
                           bvec = bvec, 
                           meq = 1,
                           factorized = T)
  
  # get the mean variance portfolio weights
  temp_portfolio_weights <- temp_results$solution
  temp_rounded_portfolio_weights <- round(temp_portfolio_weights,5)
  
  names(temp_rounded_portfolio_weights) <- names(temp_available_assets[,-1])
  
  # get the equally weighted weights
  temp_equally_weighted_weights <- rep(1/(ncol(temp_available_assets)-1),(ncol(temp_available_assets)-1))
  names(temp_equally_weighted_weights) <- names(temp_available_assets[,-1])
  
  # final output will be a weight vector or list
  results_list[[as.character(date)]] <- data.frame(Date = as.Date(date), t(temp_rounded_portfolio_weights))
  results_list_equally_weighted[[as.character(date)]] <- data.frame(Date = as.Date(date), t(temp_equally_weighted_weights))
  
}

# for the mean variance optimized weights:
# Combine all results into a single dataframe for plotting
portfolio_weights_df <- bind_rows(results_list, .id = "Rebalance_Date")
portfolio_weights_df <- portfolio_weights_df[,-1]
portfolio_weights_df[is.na(portfolio_weights_df)] <- 0

# Melt the dataframe for ggplot
portfolio_weights_long <- tidyr::pivot_longer(portfolio_weights_df, cols = -c(Date))
portfolio_weights_long$name <- as.factor(portfolio_weights_long$name)

# Plotting the portfolio weights over time
cbp1 <- c("#E69F00","#CC79A7", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")
mean_variance_plot <- ggplot(portfolio_weights_long, aes(x = Date, y = value, fill = name)) +
  geom_area(position = 'stack') +
  labs(x = "Rebalance Date", 
       y = "Portfolio Weight", 
       title = "Mean-Variance-Optimized Portfolio Weights Over Time",
       color = "Asset") +
  theme_minimal()+ 
  scale_fill_manual(values = cbp1)

mean_variance_plot

# comparison plot with equal weighting for all available asset
# Combine all results into a single dataframe for plotting
portfolio_equal_weights_df <- bind_rows(results_list_equally_weighted, .id = "Rebalance_Date")
portfolio_equal_weights_df <- portfolio_equal_weights_df[,-1]
portfolio_equal_weights_df[is.na(portfolio_equal_weights_df)] <- 0

# Melt the dataframe for ggplot
portfolio_equal_weights_long <- tidyr::pivot_longer(portfolio_equal_weights_df, cols = -c(Date))

portfolio_equal_weights_long$name <- as.factor(portfolio_equal_weights_long$name)

# note: for a "fair" comparison we only considered a risky asset possible to invest after 
# 52 weeks of return data are available as is the case for the mean variance weights
# Plotting the portfolio weights over time
equal_weight_plot <- ggplot(portfolio_equal_weights_long, aes(x = Date, y = value, fill = name)) +
  geom_area(position = 'stack') +
  labs(x = "Rebalance Date", 
       y = "Portfolio Weight", 
       title = "Equal Portfolio Weights Over Time (1/N)",
       color = "Asset") +
  theme_minimal()+ 
  scale_fill_manual(values = cbp1)

equal_weight_plot


## 5) Paper Trade the Assets and compare with 1/N weights ----
# weight tibbles
portfolio_weights_mv <- as_tibble(portfolio_weights_df)
portfolio_weights_1_over_n <- as_tibble(portfolio_equal_weights_df)


# create a new tibble to track the portfolio returns
asset_returns <- as_tibble(returns_weekly_filtered[(end_date_2023_Q3 >= returns_weekly_filtered$Date) 
                                                   & (returns_weekly_filtered$Date >= last_date_2000),1:8])
asset_returns$Date <- as.Date(asset_returns$Date)
#row.names(asset_returns) <- NULL

# ensure that the column names match across all three dataframes
rename_vector <- c('Date' = 'Date',
                   'SP500' = 'SP500',
                   'Crude Oil' = 'Crude.Oil', 
                   'US GOVT' = 'US.GOVT', 
                   'RUSSELL 2000' = 'RUSSELL.2000', 
                   'MSCI EFA' = 'MSCI.EFA',
                   'Bitcoin' = 'Bitcoin',
                   'USDJPY' = 'USDJPY')
names(asset_returns) <- rename_vector[names(asset_returns)]
# ensure that the columns are ordered in the same order
asset_returns <- asset_returns[names(portfolio_weights_mv)]


# define starting wealth
starting_wealth = 1

# allocate the starting wealth according to the initial mean variance weights
fund_asset_values_mv <- portfolio_weights_mv[1,]
fund_asset_values_mv[,-1] <- fund_asset_values_mv[,-1]*starting_wealth

# allocate the starting wealth according to the initial 1/n weights
fund_asset_values_1_over_n <- portfolio_weights_1_over_n[1,]
fund_asset_values_1_over_n[,-1] <- fund_asset_values_1_over_n[,-1]*starting_wealth


# paper trading loop
for (i in 1:nrow(asset_returns)){
  # check if it is the first date
  if (i == 1){
    # nothing happens, since for the first observation there are no returns 
    # but purely the asset allocations based on the weights
  } else {
    # update the previous asset values by the returns of the current period
    fund_asset_values_mv[i,1]  <- asset_returns$Date[i]
    #fund_asset_values_mv[i,-1] <- fund_asset_values_mv[i-1,-1] * (1+asset_returns[i,-1])
    fund_asset_values_mv[i,-1] <- fund_asset_values_mv[i-1,-1] * exp(asset_returns[i,-1])
    fund_asset_values_mv <- na.fill0(fund_asset_values_mv,0)
    
    # update the previous asset values by the returns of the current period
    fund_asset_values_1_over_n[i,1]  <- asset_returns$Date[i]
    #fund_asset_values_1_over_n[i,-1] <- fund_asset_values_1_over_n[i-1,-1] * (1+(exp(asset_returns[i,-1])-1))
    fund_asset_values_1_over_n[i,-1] <- fund_asset_values_1_over_n[i-1,-1] * exp(asset_returns[i,-1])
    fund_asset_values_1_over_n <- na.fill0(fund_asset_values_1_over_n,0)
    
    # check if we have a rebalance date
    if (asset_returns$Date[i] %in% rebalance_dates){
      # for i = i
      # rebalance the current summed up asset value based on the new weights
      # and replace the "old" i row with the rebalanced asset values
      current_total_mv_asset_value <- sum(fund_asset_values_mv[i,-1])
      fund_asset_values_mv[i,-1] <- current_total_mv_asset_value * portfolio_weights_mv[portfolio_weights_mv$Date == asset_returns$Date[i],-1]
      
      current_total_1_over_n_asset_value <- sum(fund_asset_values_1_over_n[i,-1])
      current_total_1_over_n_asset_value * portfolio_weights_1_over_n[portfolio_weights_1_over_n$Date == asset_returns$Date[i],-1]
      fund_asset_values_1_over_n[i,-1] <- current_total_1_over_n_asset_value * portfolio_weights_1_over_n[portfolio_weights_1_over_n$Date == asset_returns$Date[i],-1]
      
    } else {
      # nothing
      
    }
  }
}

# get the cumulative asset values for the two funds
fund_asset_values_mv$mean_variance_fund <- rowSums(fund_asset_values_mv[,-1])
fund_asset_values_1_over_n$"1_over_n_fund" <- rowSums(fund_asset_values_1_over_n[,-1])


# gather sum of the individual assets for performance comparison
performance_plot_data <- fund_asset_values_mv[,c("Date","mean_variance_fund")]
performance_plot_data <- cbind(performance_plot_data,fund_asset_values_1_over_n[,c("1_over_n_fund")])
performance_plot_data_long <- tidyr::pivot_longer(performance_plot_data, cols = -c(Date))
performance_plot_data_long$name <- as.factor(performance_plot_data_long$name)

# plot the cumulative returns
performance_plot <- ggplot(data = performance_plot_data_long)+
  geom_line(aes(x = Date, y = value, color = name))+
  theme_minimal()+
  labs(x = "Date", 
       y = "Portfolio Value", 
       title = "Cumulative returns (Weekly Portfolio Balance)",
       color = "Fund Type")

performance_plot

truncated_performance_plot <- ggplot(data = performance_plot_data_long[performance_plot_data_long$Date <= "2016-01-01",])+
  geom_line(aes(x = Date, y = value, color = name))+
  theme_minimal()+
  labs(x = "Date", 
       y = "Portfolio Value", 
       title = "Cumulative returns (Weekly Portfolio Balance)",
       color = "Fund Type")

truncated_performance_plot