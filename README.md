# forecasting


``` r
library(dplyr) 
library(lubridate) 
library(xts) 
library(tsoutliers) 
library(parallel) 
library(doParallel) 
library(seastests)
library(tseries)
```

Let’s clean the data:

``` r
df<- read.csv("Data/sacrois_complete.csv")
print(unique(df$X3A_CODE))
```

     [1] "ANE"     "BES"     "BOG"     "BSS"     "BZX"     "CTL"     "DCP"    
     [8] "DEX"     "DPS"     "ELX"     "FIN"     "FLX"     "FOX"     "GUX"    
    [15] "HKE"     "JAX"     "JLX"     "JOD"     "MAX"     "MGR"     "MNZ"    
    [22] "MUL"     "MUX_bis" "NEP"     "OCT"     "PAC"     "PEN"     "PIL"    
    [29] "POD"     "SAA"     "SBA"     "SBG"     "SBX"     "SOX"     "SQY"    
    [36] "SQZ"     "SRX"     "SWM"     "TUX"     "VLO"     "WEX"     "WHB"    
    [43] "XOX"     "ZZZ"    

``` r
print(length(unique(df$X3A_CODE))) # 44 species
```

    [1] 44

``` r
df$DATE_SEQ <- as.Date(df$DATE_SEQ)
str(df)
```

    'data.frame':   517404 obs. of  4 variables:
     $ DATE_SEQ               : Date, format: "1999-12-27" "1999-12-27" ...
     $ X3A_CODE               : chr  "ANE" "BES" "BOG" "BSS" ...
     $ QUANT_POIDS_VIF_MOYENNE: num  0 0 0 2636 0 ...
     $ MONTANT_EUROS_MOYENNE  : num  0 0 0 15979 0 ...

``` r
df <- df %>%
  rename("Date" = "DATE_SEQ") %>%
  rename("Species" = "X3A_CODE") %>%
  rename("LandingsKG" = "QUANT_POIDS_VIF_MOYENNE") %>%
  rename("LandingsEUR" = "MONTANT_EUROS_MOYENNE")%>%
  filter(Date >= "2001-01-01" & Date < "2024-01-01")
```

We are getting rid of the late 1999 data and beginning of 2024 data.

# Create time series for each species

We will create a `ts_list()` , a list of lists, that contains the
time-series of each species of the data set. The frequency is **365**
because we are dealing with daily data.

``` r
# Get the list of unique species
species_list <- unique(df$Species)

# Define the exact date range we want
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2023-12-31")

# Create a complete date sequence
date_seq <- seq(start_date, end_date, by = "day")

# Create a time series for each species
ts_list <- list()
for (species in species_list) {
  # Filter data for the current species and aggregate by date
  species_data <- df %>% 
    filter(Species == species) %>% 
    group_by(Date) %>%
    summarize(LandingsKG = sum(LandingsKG, na.rm = TRUE)) %>%
    ungroup()
  
  # Create a data frame with all dates
  full_data <- data.frame(Date = date_seq)
  
  # Merge with the species data
  full_data <- left_join(full_data, species_data, by = "Date")
  
  # Fill NA values with 0
  full_data$LandingsKG[is.na(full_data$LandingsKG)] <- 0
  
  # Create a ts object
  ts_object <- ts(full_data$LandingsKG, 
                  start = c(2000, 1), 
                  frequency = 365)
  
  # Add the ts object to the list
  ts_list[[paste0("ts_", species)]] <- ts_object
}

# Create a vector with all the TS names
ts_names <- names(ts_list)

# Print the names of the time series
print(ts_names)
```

     [1] "ts_ANE"     "ts_BES"     "ts_BOG"     "ts_BSS"     "ts_BZX"    
     [6] "ts_CTL"     "ts_DCP"     "ts_DEX"     "ts_DPS"     "ts_ELX"    
    [11] "ts_FIN"     "ts_FLX"     "ts_FOX"     "ts_GUX"     "ts_HKE"    
    [16] "ts_JAX"     "ts_JLX"     "ts_JOD"     "ts_MAX"     "ts_MGR"    
    [21] "ts_MNZ"     "ts_MUL"     "ts_MUX_bis" "ts_NEP"     "ts_OCT"    
    [26] "ts_PAC"     "ts_PEN"     "ts_PIL"     "ts_POD"     "ts_SAA"    
    [31] "ts_SBA"     "ts_SBG"     "ts_SBX"     "ts_SOX"     "ts_SQY"    
    [36] "ts_SQZ"     "ts_SRX"     "ts_SWM"     "ts_TUX"     "ts_VLO"    
    [41] "ts_WEX"     "ts_WHB"     "ts_XOX"     "ts_ZZZ"    

# Seasonality Detection

We want to apply the `combined_test` on each time series to sea if there
is any seasonality. You should note that:

- **H1** means **seasonality** *(alternative hypothesis)*

- **H0** means **NO seasonality** *(null hypothesis)*

- if p \< 0.05 then H1 : seasonality

``` r
seasonal_combined_test <- c()
seasonal_seasdum <- c()

# Loop over each TS
for (ts_name in names(ts_list)) {
  ts_data <- ts_list[[ts_name]]  # Retrieve data from the time series list
  
  # Apply the combined_test() test to the time series
  ct_res <- combined_test(ts_data)
  
  # Check each p-value and display results if at least one valid H1
  ct_results <- ct_res$stat
  ct_pvals <- ct_res$Pval
  if (ct_results == TRUE) {
    cat("\n")
    cat("\n")
    print(paste0("Combined_test results for the series ", ts_name))
    print(ct_res)
    seasonal_combined_test <- c(seasonal_combined_test, ts_name)
  }
  
  # # Apply the seasdum() test to the time series
  # sd_res <- seasdum(ts_data)
  # 
  # # Check p-value and display results if < 0.05
  # if (sd_res$Pval < 0.05) {
  #   cat("\n")
  #   cat("\n")
  #   print(paste0("Résultats du seasdum pour la série ", ts_name))
  #   print(sd_res)
  #   seasonal_seasdum <- c(seasonal_seasdum, ts_name)
  # }
}
```



    [1] "Combined_test results for the series ts_BOG"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  0.9506946 0.9630191 1.98604e-08


    [1] "Combined_test results for the series ts_BSS"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_BZX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 9.859313e-05


    [1] "Combined_test results for the series ts_CTL"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  7.699966e-05 2.308044e-08 0


    [1] "Combined_test results for the series ts_FLX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 6.402466e-09


    [1] "Combined_test results for the series ts_HKE"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 3.996884e-07


    [1] "Combined_test results for the series ts_JAX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 4.51619e-05


    [1] "Combined_test results for the series ts_MNZ"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 1.821931e-11


    [1] "Combined_test results for the series ts_MUL"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 2.19711e-10


    [1] "Combined_test results for the series ts_MUX_bis"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 0.00247086 0.0004175648


    [1] "Combined_test results for the series ts_NEP"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  0.8851714 0.0552564 2.778844e-10


    [1] "Combined_test results for the series ts_SAA"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 1.880718e-13


    [1] "Combined_test results for the series ts_SBA"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 2.220446e-16


    [1] "Combined_test results for the series ts_SBX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 6.241518e-05


    [1] "Combined_test results for the series ts_SOX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 1.793225e-07


    [1] "Combined_test results for the series ts_SQZ"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 0.007248347 1.016409e-12


    [1] "Combined_test results for the series ts_SRX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 1.232348e-14


    [1] "Combined_test results for the series ts_SWM"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 3.432426e-05


    [1] "Combined_test results for the series ts_TUX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  0.0003509172 0.01070668 0


    [1] "Combined_test results for the series ts_ZZZ"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 0.003959824 9.216575e-06

``` r
assign("seasonal_combined_test", seasonal_combined_test, envir = .GlobalEnv)
```

<u>The `combined_test()` function combines three tests:</u>

- QS test (Quantile Spectral test)

- QS-R test (QS test on residuals)

- KW-R test (Kruskal-Wallis test on residuals)

<u>The function considers a time series as seasonal if:</u>

- The p-value of QS-R is \< 0.01 OR the p-value of KW-R is \< 0.001

- OR if both the p-value of QS is \< 0.01 AND the p-value of KW-R is \<
  0.01

<u>Here are the time series that have Seasonality:</u>

``` r
seasonal_combined_test
```

     [1] "ts_BOG"     "ts_BSS"     "ts_BZX"     "ts_CTL"     "ts_FLX"    
     [6] "ts_HKE"     "ts_JAX"     "ts_MNZ"     "ts_MUL"     "ts_MUX_bis"
    [11] "ts_NEP"     "ts_SAA"     "ts_SBA"     "ts_SBX"     "ts_SOX"    
    [16] "ts_SQZ"     "ts_SRX"     "ts_SWM"     "ts_TUX"     "ts_ZZZ"    

We need to correct that seasonality among those time series.

``` r
for (ts_name in seasonal_combined_test) {
  ts_data <- ts_list[[ts_name]]  # Retrieve time series data from the list
  decomp <- stl(ts_data, s.window = 365)  # STL decomposition
  seasonal <- decomp$time.series[, "seasonal"]  # Get the seasonal component
  ts_data_adjusted <- ts_data - seasonal  # Correcting the seasonal component
  ts_list[[ts_name]] <- ts_data_adjusted  # Update the adjusted time series in the list
}
```

Now that this is done, let’s check again for any seasonality:

``` r
seasonal_combined_test <- c()
seasonal_seasdum <- c()

# Loop over each TS
for (ts_name in names(ts_list)) {
  ts_data <- ts_list[[ts_name]]  # Retrieve data from the time series list
  
  # Apply the combined_test() test to the time series
  ct_res <- combined_test(ts_data)
  
  # Check each p-value and display results if at least one valid H1
  ct_results <- ct_res$stat
  ct_pvals <- ct_res$Pval
  if (ct_results == TRUE) {
    cat("\n")
    cat("\n")
    print(paste0("Combined_test results for the series ", ts_name))
    print(ct_res)
    seasonal_combined_test <- c(seasonal_combined_test, ts_name)
  }
}
```



    [1] "Combined_test results for the series ts_BOG"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_BSS"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_BZX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_CTL"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_FLX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_HKE"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_JAX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_MUL"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_MUX_bis"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_NEP"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 1.660894e-13


    [1] "Combined_test results for the series ts_SAA"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_SBA"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_SBX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_SOX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_SQZ"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_SRX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_SWM"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0


    [1] "Combined_test results for the series ts_TUX"
    Test used:  WO 
     
    Test statistic:  1 
    P-value:  1 1 0

It’s weird that we have such a difference between the first two tests
and the last one. There are quite the opposite, in term of P-values.

For now we will focus on the first two tests *(QS & QS-R)* tests as our
results for seasonality. According to them, **there is no seasonality
left.**

# Stationarity

Now, let’s take care of stationarity. Most of the forecasting models we
are going to use need stationary time-series. We will use the `adf.test`
from the
[tseries](https://www.rdocumentation.org/packages/tseries/versions/0.10-54/topics/adf.test)
package.

Note that for this test we have:

- H0: série non stationnaire (possède une racine unitaire)

- H1: série stationnaire (n’en possède pas)

- if p\<0.05 : the serie is stationary

``` r
# First round of differencing
for (ts_name in names(ts_list)) {
  ts_data <- ts_list[[ts_name]]  # Retrieve time series data from the list
  adf_result <- adf.test(ts_data)  # Apply ADF test for stationarity
  #print(adf_result)
  
  # Check if P-Value > 0.05
  if (adf_result$p.value > 0.05) {
    # Apply a difference to the time series if it is not stationary
    ts_list[[ts_name]] <- diff(ts_data)
    print(paste("The TS", ts_name, "has been differentiated"))
  }
  else {
    print(paste("The TS", ts_name, "is stationary"))
  }
}
```

    [1] "The TS ts_ANE is stationary"
    [1] "The TS ts_BES is stationary"
    [1] "The TS ts_BOG is stationary"
    [1] "The TS ts_BSS is stationary"
    [1] "The TS ts_BZX is stationary"
    [1] "The TS ts_CTL is stationary"
    [1] "The TS ts_DCP is stationary"
    [1] "The TS ts_DEX is stationary"
    [1] "The TS ts_DPS is stationary"
    [1] "The TS ts_ELX is stationary"
    [1] "The TS ts_FIN is stationary"
    [1] "The TS ts_FLX is stationary"
    [1] "The TS ts_FOX is stationary"
    [1] "The TS ts_GUX is stationary"
    [1] "The TS ts_HKE is stationary"
    [1] "The TS ts_JAX is stationary"
    [1] "The TS ts_JLX is stationary"
    [1] "The TS ts_JOD is stationary"
    [1] "The TS ts_MAX is stationary"
    [1] "The TS ts_MGR is stationary"
    [1] "The TS ts_MNZ is stationary"
    [1] "The TS ts_MUL is stationary"
    [1] "The TS ts_MUX_bis is stationary"
    [1] "The TS ts_NEP is stationary"
    [1] "The TS ts_OCT is stationary"
    [1] "The TS ts_PAC is stationary"
    [1] "The TS ts_PEN is stationary"
    [1] "The TS ts_PIL is stationary"
    [1] "The TS ts_POD is stationary"
    [1] "The TS ts_SAA is stationary"
    [1] "The TS ts_SBA is stationary"
    [1] "The TS ts_SBG is stationary"
    [1] "The TS ts_SBX is stationary"
    [1] "The TS ts_SOX is stationary"
    [1] "The TS ts_SQY is stationary"
    [1] "The TS ts_SQZ is stationary"
    [1] "The TS ts_SRX is stationary"
    [1] "The TS ts_SWM is stationary"
    [1] "The TS ts_TUX is stationary"
    [1] "The TS ts_VLO is stationary"
    [1] "The TS ts_WEX is stationary"
    [1] "The TS ts_WHB is stationary"
    [1] "The TS ts_XOX is stationary"
    [1] "The TS ts_ZZZ is stationary"

``` r
# Second round of differencing if necessary
for (ts_name in names(ts_list)) {
  ts_data <- ts_list[[ts_name]]  # Retrieve time series data from the list
  adf_result <- adf.test(ts_data)  # Apply ADF test for stationarity
  #print(adf_result)
  
  # Check if P-Value > 0.05
  if (adf_result$p.value > 0.05) {
    print(paste("The TS", ts_name, "is still not stationary"))
    # Apply a difference to the time series if it is not stationary
    ts_list[[ts_name]] <- diff(ts_data)
    print(paste("The TS", ts_name, "has been differentiated a second time"))
  }
}

# Final check
for (ts_name in names(ts_list)) {
  ts_data <- ts_list[[ts_name]]  # Retrieve time series data from the list
  adf_result <- adf.test(ts_data)  # Apply ADF test for stationarity
  #print(adf_result)
  
  if (adf_result$p.value > 0.05) {
    # Check if P-Value > 0.05
    cat(paste0("\033[1m\033[4m\033[31mThe time series ", ts_name, " is still not stationary:\033[0m\n"))
  }
}
```

It seems that every time-series is now stationary. Let’s check if they
all have the same length

``` r
str(ts_list)
```

    List of 44
     $ ts_ANE    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_BES    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_BOG    : Time-Series [1:8766] from 2000 to 2024: 0.174 0.174 0.174 0.174 0.174 ...
     $ ts_BSS    : Time-Series [1:8766] from 2000 to 2024: 67.6 101.6 113.1 26.9 22.8 ...
     $ ts_BZX    : Time-Series [1:8766] from 2000 to 2024: 0.21 0.21 0.21 0.21 0.21 ...
     $ ts_CTL    : Time-Series [1:8766] from 2000 to 2024: 226.6 234.5 237 122.7 31.8 ...
     $ ts_DCP    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_DEX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_DPS    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_ELX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_FIN    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_FLX    : Time-Series [1:8766] from 2000 to 2024: 18.6 19.2 24 -24.3 -60.6 ...
     $ ts_FOX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_GUX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_HKE    : Time-Series [1:8766] from 2000 to 2024: 121 122 120 117 99 ...
     $ ts_JAX    : Time-Series [1:8766] from 2000 to 2024: 86.1 86.1 86.1 86.1 86.1 ...
     $ ts_JLX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_JOD    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_MAX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_MGR    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_MNZ    : Time-Series [1:8766] from 2000 to 2024: 307 312 301 289 230 ...
     $ ts_MUL    : Time-Series [1:8766] from 2000 to 2024: 4.93 16.15 13.6 15.54 15.81 ...
     $ ts_MUX_bis: Time-Series [1:8766] from 2000 to 2024: 11.07 35.32 44.02 -8.04 -17.73 ...
     $ ts_NEP    : Time-Series [1:8766] from 2000 to 2024: 137 146 138 140 132 ...
     $ ts_OCT    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_PAC    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_PEN    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_PIL    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_POD    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_SAA    : Time-Series [1:8766] from 2000 to 2024: 0.000466 0.000465 0.000465 0.000464 0.000464 ...
     $ ts_SBA    : Time-Series [1:8766] from 2000 to 2024: 0.0566 0.0566 0.0566 0.0566 0.0566 ...
     $ ts_SBG    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_SBX    : Time-Series [1:8766] from 2000 to 2024: 207 -1039 211 205 197 ...
     $ ts_SOX    : Time-Series [1:8766] from 2000 to 2024: 42.7 42.4 40.4 30.1 14.9 ...
     $ ts_SQY    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_SQZ    : Time-Series [1:8766] from 2000 to 2024: 85.5 96.3 99.7 38.9 -62.4 ...
     $ ts_SRX    : Time-Series [1:8766] from 2000 to 2024: 123.23 137.91 99.64 40.25 7.19 ...
     $ ts_SWM    : Time-Series [1:8766] from 2000 to 2024: 2.23 2.23 2.23 2.24 2.24 ...
     $ ts_TUX    : Time-Series [1:8766] from 2000 to 2024: 164 164 164 163 163 ...
     $ ts_VLO    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_WEX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_WHB    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_XOX    : Time-Series [1:8766] from 2000 to 2024: 0 0 0 0 0 0 0 0 0 0 ...
     $ ts_ZZZ    : Time-Series [1:8766] from 2000 to 2024: 760 833 991 862 947 ...

They do. Now, before forecasting we are going to use a variables
selection method. This will help us to reduce the number of irrelevant
variables of our models.

After that, we will apply econometrics and machine learning forecasting
models.

From now on, we’ll reason as follows:

- $Y_t$: will be the variable we seek to predict

- $X_k,_t$: will be the explanatory variables, which will add
  information to enable the models to better predict our $Y_t$.

For your information, here’s a table summarizing our variables. In bold
are the dynamic variables in [Ifremer’s IAM forecasting
model](https://archimer.ifremer.fr/doc/00784/89579/). As these variables
are already considered dynamic, they are the only ones we won’t attempt
to predict.

| HKE     | European hake              | MUT     |                          |
|---------|----------------------------|---------|--------------------------|
| **NEP** |                            | **ARA** |                          |
| **DPS** |                            | ANE     | European anchovy         |
| BES     | Belone                     | BOG     | Bogue                    |
| BSS     | European seabass           | BZX     | Bonitos                  |
| CTL     | Cuttlefish, bobtail squids | DCP     | Natantian decapods       |
| DEX     | Dentex                     | DPS     | Deep-water rose shrimp   |
| ELX     | River eels                 | FIN     | Finfishes                |
| FLX     | Flatfishes                 | FOX     | Forkbeards               |
| GUX     | Gurnards, searobins        | JAX     | Jack and horse mackerels |
| JLX     | Murex shells               | JOD     | John dory                |
| MAX     | Mackerels                  | MGR     | Meagre                   |
| MNZ     | Monkfishes                 | MUL     | Mullets                  |
| MUX_bis | Red mullets                | OCT     | Octopuses, etc.          |
| PAC     | Common pandora             | PEN     | Penaeus shrimps          |
| PIL     | European pilchard          | POD     | Poor cod                 |
| SAA     | Round sardinella           | SBA     | Axillary seabream        |
| SBG     | Gilthead seabream          | SBX     | Porgies, seabreams       |
| SOX     | Soles                      | SQY     | Squillids                |
| SQZ     | Inshore squids             | SRX     | Rays, stingrays, mantas  |
| SWM     | Swimming crabs, etc.       | TUX     | Tuna-like fishes         |
| VLO     | Spiny lobsters             | WEX     | Weevers                  |
| WHB     | Blue whiting               | XOX     | Sandlances               |

Different species of the SACROIS dataset

<u>Note</u>: `ZZZ` therefore includes all species not mentioned in the
table above. It is therefore a collection of several species.
