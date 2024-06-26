library(dplyr)
library(lubridate)

# Using filtered data
df<- read.csv("Data/sacrois_complete.csv")
print(unique(df$X3A_CODE))
print(length(unique(df$X3A_CODE))) # 44 species


# CHECK FOR MISSING DATA FOR EACH SPECIES
df <- df %>%
  mutate(YEAR = year(MONTH))

species_count <- df %>%
  filter(!YEAR %in% c(1999, 2024)) %>% # Let's get rid of the incomplete years
  group_by(X3A_CODE, YEAR) %>%
  summarise(
    months_count = 12 - n_distinct(month(MONTH)),
    .groups = "drop"
  )
incomplete_species <- species_count %>%
  filter(months_count != 0)

print(unique(incomplete_species$X3A_CODE))
print(length(unique(incomplete_species$X3A_CODE))) # 42 species

vingt1 <- read.csv("Data/sacrois2021.csv")
vingt1_bes <- vingt1%>%
  filter(X3A_CODE=="BES")
View(vingt1_bes)