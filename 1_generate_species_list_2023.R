
library(readr)
library(tidyr)
library(tibble)
library(taxize)
library(stringr)
library(dplyr)

species <- read_delim("../Daten/Leuk/Erste Daten von Tom/data_2004_2023_main_with_global_id.csv")
species <- species[!(endsWith(species$`Species_unified_2004-23`, "spec.") | endsWith(species$`Species_unified_2004-23`, "sp.")),]

mosses <- read_csv("../Daten/Leuk/Hand Curated/moss_taxa.csv")
species <- species[!(species$`Species_unified_2004-23` %in% mosses$moss_taxa),]

names <- unique(species$`Species_unified_2004-23`, " ",1)
families <- data.frame(db = character(), query = character(), family = character())
while(length(families$query) < length(names)) {
  tryCatch({for(i in 1:length(names)) {
    if(!names[i] %in% families$query) {
      families <- add_row(families, tax_name(names[i], get = 'family', db = 'ncbi'))
      Sys.sleep(1)
    }
  }},  error = function(w) { print("An abort occored.") })
  Sys.sleep(10)
}

species_with_missing_family <- families[is.na(families$family),]$query
genus_of_missing_families <- unique(str_split_i(species_with_missing_family, " ", 1))

missing_families <- data.frame(db = character(), query = character(), family = character())
while(length(missing_families$query) < length(genus_of_missing_families)) {
  tryCatch({for(i in 1:length(genus_of_missing_families)) {
    if(!genus_of_missing_families[i] %in% missing_families$query) {
      missing_families <- add_row(missing_families, tax_name(genus_of_missing_families[i], get = 'family', db = 'ncbi'))
      Sys.sleep(1)
    }
  }},  error = function(w) { print("An abort occured.") })
  Sys.sleep(10)
}

missing_species_and_family <- data.frame(species = species_with_missing_family, genus = str_split_i(species_with_missing_family, " ", 1))
missing_species_and_family <- full_join(missing_species_and_family, missing_families, join_by(genus == query))

joined_result <- families
joined_result$family[match(missing_species_and_family$species, joined_result$query)] <- missing_species_and_family$family

joined_result[is.na(joined_result$family),]
joined_result$family[match(c("Nigritelle rhellicani", "Arenaria serpyllifolia aggr."), joined_result$query)] <- c("Orchidaceae", "Caryophyllaceae")

write_csv(joined_result, "../Daten/Field/all_species_with_families.csv")

species <- full_join(species, joined_result, join_by(`Species_unified_2004-23` == query))

min_elevation <- min(species$elevation_m)
max_elevation <- max(species$elevation_m)
height_band_width <- (max(species$elevation_m) - min(species$elevation_m))/3

margin <- 100

reduce_subset <- function(subset) {
  subset <- subset[,c("Species_unified_2004-23", "family")]
  subset <- subset[order(subset$`Species_unified_2004-23`),]
  subset <- subset[order(subset$family),]
  subset <- unique(subset)
  return(subset)  
}

subset <- species[species$elevation_m < (min_elevation + height_band_width + margin),]
write_csv(reduce_subset(subset), "../Daten/Field/species_below_1380m.csv")
subset <- species[species$elevation_m > (min_elevation + height_band_width - margin) & species$elevation_m < (min_elevation + 2*height_band_width + margin),]
write_csv(reduce_subset(subset), "../Daten/Field/species_between_1180m_and_1790.csv")
subset <- species[species$elevation_m > (min_elevation + 2*height_band_width - margin),]
write_csv(reduce_subset(subset), "../Daten/Field/species_above_1590.csv")

length(unique(subset$`Species_unified_2004-23`))
max(subset$elevation_m)

species |>
  group_by(year) |>
  summarise(n = length(unique(`Species_unified_2004-23`)))

length(unique(species$`Species_unified_2004-23`))

