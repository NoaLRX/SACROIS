library(dplyr)
library(lubridate)
library(xts)
library(tsoutliers)
library(parallel)
library(doParallel)
library(seastests)

df<- read.csv("Data/sacrois_complete.csv")
print(unique(df$X3A_CODE))
print(length(unique(df$X3A_CODE))) # 44 species
df$DATE_SEQ <- as.Date(df$DATE_SEQ)
str(df)

df <- df %>%
  rename("Date" = "DATE_SEQ") %>%
  rename("Species" = "X3A_CODE") %>%
  rename("LandingsKG" = "QUANT_POIDS_VIF_MOYENNE") %>%
  rename("LandingsEUR" = "MONTANT_EUROS_MOYENNE")%>%
  filter(Date >= "2001-01-01" & Date < "2024-01-01")



# CREATE TIME SERIES FOR EACH SPECIES----

# Get the list of unique species
species_list <- unique(df$Species)

# Create a time series for each species
ts_list <- list()
for (species in species_list) {
  # Filter data for the current species
  species_data <- df %>% 
    filter(Species == species) %>% 
    arrange(Date) %>%
    select(Date, LandingsKG)
  
  # Create a ts object
  start_date <- species_data$Date[1]
  end_date <- species_data$Date[nrow(species_data)]
  
  # Create a complete date sequence
  date_seq <- seq(start_date, end_date, by = "day")
  
  # Merge with the complete date sequence
  full_data <- merge(data.frame(Date = date_seq), 
                     species_data, 
                     by = "Date", 
                     all.x = TRUE)
  
  # Fill NA values with 0 (or you might want to use a different method)
  full_data$LandingsKG[is.na(full_data$LandingsKG)] <- 0
  
  # Create a ts object
  ts_object <- ts(full_data$LandingsKG, 
                  start = c(year(start_date), yday(start_date)), 
                  frequency = 365)
  
  # Add the ts object to the list
  ts_list[[paste0("ts_", species)]] <- ts_object
}

# Create a vector with all the TS names
ts_names <- names(ts_list)

# Print the names of the time series
print(ts_names)





#SEASONALITY DETECTION----
#' We are going to test seasonlity for each time series of our data set.
#' Note: H1 is the alternative hypothesis, H0 is the null hypothesis
#' H1 means seasonality and H0 means no seasonality 
#' If p < 0.05 then then we accept H1 so there is seasonality
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

assign("seasonal_combined_test", seasonal_combined_test, envir = .GlobalEnv)


# Calculate the differences between the two lists
only_combined_test <- setdiff(seasonal_combined_test, seasonal_seasdum)
only_seasdum <- setdiff(seasonal_seasdum, seasonal_combined_test)
both_tests <- intersect(seasonal_combined_test, seasonal_seasdum)




seasdum(ts_list$tsANE)

ts_ANE2 <- ts_list$tsANE
seasdum(ts_ANE2, freq=365)
combined_test(ts_list$tsANE, freq=365)
combined_test(ts_list$ts_ANE, freq=365)
