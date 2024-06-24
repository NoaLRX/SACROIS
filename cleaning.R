library(dplyr)
library(data.table)
library(arrow)
library(tidyverse)
library(lubridate)
library(readxl)

# IMPORTING IMMATRICULATION----
# Importing the Immatriculation data
load("/Volumes/NO NAME/DRI_ANNUEL_FPC_COMPLETE.RData")

immat <- DRI_ANNUEL_FPC_COMPLETE %>%
  # Filter on the Mediterranean facade
  filter(FACADE_LIB == "Méditerranée") %>%
  # Columns selections
  select(
    NAVS_COD,
    NAVP_LONGUEUR_HT,
    FLOTTILLE_IFREMER_LIB,
    S_FLOTTILLE_IFREMER_LIB,
    S_S_FLOTTILLE_IFREMER_LIB,
    DCR_SEGMENT_CE_LIB
  ) %>%
  filter(!is.na(NAVS_COD))

# Set as data table for a faster merging
setDT(immat)
# Transform as character for the merging
immat[, NAVS_COD := as.character(NAVS_COD)]
# Let's take the 'unique' value of the immatriculation data
immat_unique <- unique(immat, by = "NAVS_COD")





# IMPORTING PARQUET FILES----
# Chemin du dossier contenant les fichiers Parquet
folder_path <- "/Volumes/NO NAME/P08_SACROIS_NAVS_MOIS_JOUR/ANNEE"

# Liste tous les fichiers Parquet dans le dossier
files <- list.files(folder_path, pattern = "*.parquet", full.names = TRUE)

# Boucle sur chaque fichier
for (file in files) {
  # Extraire l'année du nom du fichier
  year <- sub(".*_(\\d{4})\\.parquet", "\\1", basename(file))
  
  # Créer le nom de l'objet dynamiquement
  obj_name <- paste0("sacrois", year)
  
  # Lire le fichier Parquet
  df <- read_parquet(file)
  
  # Appliquer les transformations
  df <- df %>%
    filter(SECT_COD_SACROIS_NIV1 == 27) %>%
    filter(PAVILLON == "FRA") %>%
    select(
      NAVS_COD,
      PAVILLON,
      DATE_SEQ,
      ESP_COD_FAO,
      QUANT_POIDS_VIF_MOYENNE,
      MONTANT_EUROS_MOYENNE
    )
  
  setDT(df)
  df[, NAVS_COD := as.character(NAVS_COD)]
  
  # Merging (assurez-vous que immat_unique est défini quelque part)
  sacrois <- df[immat_unique, on = "NAVS_COD", nomatch = 0L]
  sacrois[, DATE_SEQ := as.Date(DATE_SEQ)]
  
  # Vérifier les NA
  cat("Il y a", sum(is.na(df)), "NA pour df", year, "\n")
  
  # Créer la colonne FleetIAM
  sacrois$FleetIAM <- case_when(
    grepl("Fileyeurs", sacrois$S_FLOTTILLE_IFREMER_LIB, ignore.case = TRUE) ~ "DFN",
    grepl("Chalutiers", sacrois$S_FLOTTILLE_IFREMER_LIB, ignore.case = TRUE) ~ "DTS",
    grepl("ligneurs", sacrois$S_FLOTTILLE_IFREMER_LIB, ignore.case = TRUE) ~ "HOK",
    TRUE ~ NA_character_
  )
  
  sacrois <- sacrois %>% filter(!is.na(FleetIAM))
  
  sacrois$NAVP_LONGUEUR_M <- sacrois$NAVP_LONGUEUR_HT / 100
  
  sacrois$FleetIAM2 <- case_when(
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "DFN" & sacrois$NAVP_LONGUEUR_M <= 6 ~ "FRA_DFN_00-06m",
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "DFN" & sacrois$NAVP_LONGUEUR_M > 6 & sacrois$NAVP_LONGUEUR_M <= 12 ~ "FRA_DFN_06-12m",
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "HOK" & sacrois$NAVP_LONGUEUR_M <= 6 ~ "FRA_HOK_00-06m",
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "HOK" & sacrois$NAVP_LONGUEUR_M > 6 & sacrois$NAVP_LONGUEUR_M <= 12 ~ "FRA_HOK_06-12m",
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "HOK" & sacrois$NAVP_LONGUEUR_M > 12 & sacrois$NAVP_LONGUEUR_M <= 18 ~ "FRA_HOK_12-18m",
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "DTS" & sacrois$NAVP_LONGUEUR_M > 18 & sacrois$NAVP_LONGUEUR_M <= 24 ~ "FRA_DTS_18-24m",
    sacrois$PAVILLON == "FRA" & sacrois$FleetIAM == "DTS" & sacrois$NAVP_LONGUEUR_M > 24 ~ "FRA_DTS_>24m",
    TRUE ~ NA_character_
  )
  
  sacrois_final <- sacrois %>%
    filter(!is.na(FleetIAM2)) %>%
    select(DATE_SEQ, ESP_COD_FAO, QUANT_POIDS_VIF_MOYENNE, MONTANT_EUROS_MOYENNE, FleetIAM2) %>%
    mutate(QUANT_POIDS_VIF_MOYENNE = as.numeric(QUANT_POIDS_VIF_MOYENNE),
           MONTANT_EUROS_MOYENNE = as.numeric(MONTANT_EUROS_MOYENNE)) %>%
    group_by(DATE_SEQ, ESP_COD_FAO) %>%
    summarise(QUANT_POIDS_VIF_MOYENNE = sum(QUANT_POIDS_VIF_MOYENNE, na.rm = TRUE),
              MONTANT_EUROS_MOYENNE = sum(MONTANT_EUROS_MOYENNE, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(DATE_SEQ)
  
  # Assigner le résultat à un objet nommé dynamiquement
  assign(obj_name, sacrois_final)
  
  # Afficher un message pour indiquer que le traitement est terminé pour cette année
  cat("Traitement terminé pour", year, "\n")
}


# CREATING THE ULTIMATE DATA FRAME (2000-2024)----
# Fonction pour traiter chaque base de données annuelle
process_sacrois <- function(df) {
  df %>%
    mutate(MONTH = floor_date(DATE_SEQ, "month")) %>%
    group_by(MONTH, ESP_COD_FAO) %>%
    summarise(
      QUANT_POIDS_VIF_MOYENNE = sum(QUANT_POIDS_VIF_MOYENNE, na.rm = TRUE),
      MONTANT_EUROS_MOYENNE = sum(MONTANT_EUROS_MOYENNE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(ESP_COD_FAO != "")
}

# Obtenir la liste de tous les objets sacrois20XX
sacrois_objects <- ls(pattern = "^sacrois20\\d{2}$")

# Traiter chaque base de données et les combiner
sacrois_final <- sacrois_objects %>%
  map(~ get(.x) %>% process_sacrois()) %>%
  bind_rows()

# Trier et regrouper les données finales
sacrois_final <- sacrois_final %>%
  arrange(MONTH, ESP_COD_FAO) %>%
  group_by(MONTH, ESP_COD_FAO) %>%
  summarise(
    QUANT_POIDS_VIF_MOYENNE = sum(QUANT_POIDS_VIF_MOYENNE, na.rm = TRUE),
    MONTANT_EUROS_MOYENNE = sum(MONTANT_EUROS_MOYENNE, na.rm = TRUE),
    .groups = "drop"
  )

# Vérifier le résultat
str(sacrois_final)
head(sacrois_final)
tail(sacrois_final)

# Vérifier la plage de dates
range(sacrois_final$MONTH)


# AGREGATING SPECIES ACCORDING TO GREGOIRE----
taxons <- read_excel("/Users/noa/Desktop/IFREMER/Perso/Species_Focus_CodeDescription_NOA.xlsx")
setDT(taxons)
setDT(sacrois_final)

# Créer une clé pour accélérer la recherche
setkey(taxons, `Other taxonomic code included`)

# Créer une copie de sacrois_final pour sacrois_final_agg
sacrois_final_agg <- copy(sacrois_final)

# Initialiser la nouvelle colonne
sacrois_final_agg[, X3A_CODE := NA_character_]

# Effectuer la jointure et l'attribution des codes en une seule boucle
for (i in 1:nrow(taxons)) {
  codes <- unlist(strsplit(taxons$`Other taxonomic code included`[i], ", "))
  sacrois_final_agg[ESP_COD_FAO %in% codes, X3A_CODE := taxons$X3A_CODE[i]]
}

# Remplacer les NA par "ZZZ"
sacrois_final_agg[is.na(X3A_CODE), X3A_CODE := "ZZZ"]

sacrois <- sacrois_final_agg %>%
  select(-c("ESP_COD_FAO"))%>%
  group_by(MONTH, X3A_CODE) %>%
  summarise(
    QUANT_POIDS_VIF_MOYENNE = sum(QUANT_POIDS_VIF_MOYENNE, na.rm = TRUE),
    MONTANT_EUROS_MOYENNE = sum(MONTANT_EUROS_MOYENNE, na.rm = TRUE),
    .groups = "drop"
  )




# COUNT RANGE FOR EACH SPECIES----
# Ajouter une colonne pour l'année
sacrois <- sacrois %>%
  mutate(YEAR = year(MONTH))

# Compter le nombre de mois par espèce et par année
species_count <- sacrois %>%
  filter(!YEAR %in% c(1999, 2024)) %>%
  group_by(X3A_CODE, YEAR) %>%
  summarise(
    months_count = 12 - n_distinct(month(MONTH)),
    .groups = "drop"
  )

# Identifier les espèces qui n'ont pas 12 mois pour chaque année
incomplete_species <- species_count %>%
  filter(months_count != 0)

# Afficher les résultats
View(incomplete_species)

print(unique(incomplete_species$X3A_CODE))
cat(length(incomplete_species$X3A_CODE)/length(sacrois$X3A_CODE)*100, "% of the species have incomplete data")

# Here is the list of species with incomplete data
# "ANE"     "BES"     "BOG"     "BSS"     "BZX"     "CTL"     "DCP"     "DEX"    
# "DPS"     "ELX"     "FIN"     "FLX"     "FOX"     "GUX"     "HKE"     "JAX"    
# "JLX"     "JOD"     "MAX"     "MGR"     "MNZ"     "MUL"     "MUX_bis" "NEP"    
# "OCT"     "PAC"     "PEN"     "PIL"     "POD"     "SAA"     "SBA"     "SBG"    
# "SBX"     "SQY"     "SQZ"     "SRX"     "SWM"     "TUX"     "VLO"     "WEX"    
# "WHB"     "XOX"   
# Which represents 6.39% of the total species we have

sacrois_filtered <- sacrois %>%
  filter(!X3A_CODE %in% incomplete_species$X3A_CODE)

write.csv(sacrois_filtered, "Data/sacrois_m_filtered.csv", row.names = FALSE)
write.csv(sacrois, "Data/sacrois_m.csv", row.names = FALSE)
write.csv(incomplete_species, "Data/incomplete_species.csv", row.names = FALSE)


