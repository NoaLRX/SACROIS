library(dplyr)
library(lubridate)
library(xts)
library(tsoutliers)
library(parallel)
library(doParallel)

df<- read.csv("Data/sacrois_complete.csv")
print(unique(df$X3A_CODE))
print(length(unique(df$X3A_CODE))) # 44 species

df <- df %>%
  rename("Date" = "DATE_SEQ") %>%
  rename("Species" = "X3A_CODE") %>%
  rename("LandingsKG" = "QUANT_POIDS_VIF_MOYENNE") %>%
  rename("LandingsEUR" = "MONTANT_EUROS_MOYENNE")

df$Date <- as.Date(df$Date)

# Weekly data tranformation----
#' My computer isn't powerful enough to run the tso() function (see next) on a 
#' large daily data of ~8000+ values, so that's why we are going with weekly data
# 
# df_weekly <- df %>%
#   mutate(Week = floor_date(Date, unit = "week")) %>%
#   group_by(Week, Species) %>%
#   summarize(
#     LandingsKG = sum(LandingsKG),
#     LandingsEUR = sum(LandingsEUR),
#     .groups = "drop"
#   ) %>%
#   arrange(Week, Species)
# 
# View(df_weekly)


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
  ts_object <- ts(species_data$LandingsKG, 
                  start = c(year(start_date), as.numeric(format(start_date, "%j"))), 
                  frequency = 365.25)
  
  # Add the ts object to the list
  ts_list[[paste0("ts", species)]] <- ts_object
}
# Create a vector with all the TS names
ts_names <- names(ts_list)
# Print the names of the time series
print(ts_names)



# CREATE TIME SERIES FOR EACH SPECIES (WEEKLY DATA)----
# Get the list of unique species
# species_list <- unique(df_weekly$Species)
# 
# # Create a time series for each species
# for (species in species_list) {
#   # Filter data for the current species
#   species_data <- df_weekly %>% 
#     filter(Species == species) %>% 
#     arrange(Week) %>%
#     select(Week, LandingsKG)
#   
#   # Create a ts object
#   start_date <- species_data$Week[1]
#   ts_object <- ts(species_data$LandingsKG, 
#                   start = c(year(start_date), week(start_date)), 
#                   frequency = 52)
#   
#   # Create a name for the time series
#   ts_name <- paste0("ts_", species)
#   
#   # Assign the ts object to the global environment
#   assign(ts_name, ts_object, envir = .GlobalEnv)
# }
# 
# # Create a vector with all the TS names
# ts_names <- ls(pattern = "^ts_", envir = .GlobalEnv)
# 
# # Print the names of the time series
# print(ts_names)


# ATYPICAL POINTS CORRECTION (WEEKLY DATA)----
# start_time_total <- Sys.time()
# for (i in seq_along(ts_names)) {
#   ts_name <- ts_names[i]
#   cat("\n\033[1m\033[34mProcessing", i, "of", length(ts_names), ":", ts_name, "(Weekly Data)\033[0m\n")
#   
#   start_time <- Sys.time()
#   
#   # Get the time series data
#   ts_data <- get(ts_name, envir = .GlobalEnv)
#   
#   # Create names for TSO object and adjusted series
#   base_name <- sub("^ts_", "", ts_name)
#   tso_name <- paste0("tso_", base_name)
#   adj_name <- paste0(ts_name, "_adj")
#   
#   # Apply TSO and handle potential errors
#   tryCatch({
#     # Fit TSO model
#     cat("Fitting TSO model (Weekly Data)... ")
#     tso_fit <- tso(ts_data)
#     cat("Done.\n")
#     
#     # Print TSO results
#     cat("\033[1m\033[31mTSO for", ts_name, "(Weekly Data):\033[0m\n")
#     print(tso_fit)
#     
#     # Assign TSO object and adjusted series to global environment
#     assign(tso_name, tso_fit, envir = .GlobalEnv)
#     assign(adj_name, tso_fit$yadj, envir = .GlobalEnv)
#     
#     # Plot if there are atypical points
#     if (!is.null(tso_fit$outliers) && nrow(tso_fit$outliers) > 0) {
#       cat("Plotting... ")
#       plot(tso_fit)
#       title(main = paste("TSO for", ts_name, "(Weekly Data)"))
#       cat("Done.\n")
#     } else {
#       cat("No outliers found, skipping plot.\n")
#     }
#   }, error = function(e) {
#     # Print error message if TSO fails
#     cat("\033[1m\033[31mError for", ts_name, "(Weekly Data):\033[0m\n")
#     cat(e$message, "\n")
#   })
#   
#   end_time <- Sys.time()
#   cat("Time taken for", ts_name, ":", difftime(end_time, start_time, units = "secs"), "seconds\n")
# }
# end_time_total <- Sys.time()
# cat("\n\033[1m\033[32mTotal time taken:", difftime(end_time_total, start_time_total, units = "mins"), "minutes\033[0m\n")