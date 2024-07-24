# Script for Combining PREDICTS and Mammal Communities
# Combining the PREDICTS and MCDB data into one Data Frame:

library(dplyr)
library(tidyr)

#-----------------------SUBSETTING/COMBINING DATA FRAMES-----------------------------------------------------

# First subsetting the PREDICTS database to just keep the relevant columns
PREDICTS_subset <- PREDICTS_mammalia %>% select(Reference , Diversity_metric_type , 
                                                Diversity_metric_is_effort_sensitive , SSS , SSB, 
                                                Sampling_effort , Habitat_as_described , Longitude , 
                                                Latitude , Country , Ecoregion , Biome , Order , Family , 
                                                Genus , Species , Best_guess_binomial , Measurement, 
                                                Sample_start_earliest)

# Merging the mammal communities data
MCDB_sites_trapping <- merge(MCDB_sites, MCDB_trapping, by = "Site_ID", all = FALSE)
MCDB_communities_species <- merge(MCDB_communities, MCDB_species, by = "Species_ID", all = FALSE)

MCDB_subset <- left_join(MCDB_communities_species, MCDB_sites_trapping, by = c("Site_ID", "Initial_year"))
MCDB_subset <- MCDB_subset %>% arrange(Site_ID)

MCDB_subset$Best_guess_binomial = paste(MCDB_subset$Genus, MCDB_subset$Species, sep=" ")


# Subsetting the mammal communities data
MCDB_subset <- MCDB_subset %>% select(Site_ID , Initial_year , Presence_only , Abundance , Family , Genus , 
                                      Species , Country , Latitude , Longitude , Habitat_description , Trap_nights ,
                                      Reference_ID)

# Changing the Presence Only column from binary 0 and 1 to character Abundance/Occurrence
MCDB_subset$Presence_only[MCDB_subset$Presence_only == 0] <- "Abundance"
MCDB_subset$Presence_only[MCDB_subset$Presence_only == 1] <- "Occurrence"

# Changing the NULL to 1 (For occurrence data binary 0/1 for present or absent)
MCDB_subset$Presence_only[MCDB_subset$Presence_only == "NULL"] <- 1


#Renaming columns so they match up
names(MCDB_subset)[names(MCDB_subset) == "Reference_ID"] <- "Reference"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Habitat_as_described"] <- "Habitat_description"
names(MCDB_subset)[names(MCDB_subset) == "Trap_nights"] <- "Sampling_effort"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Sample_start_earliest"] <- "Initial_year"
names(MCDB_subset)[names(MCDB_subset) == "Site_ID"] <- "Site_ID/SSS"
names(PREDICTS_subset)[names(PREDICTS_subset) == "SSS"] <- "Site_ID/SSS"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Total_abundance"] <- "Site_abundance"
names(MCDB_subset)[names(MCDB_subset) == "Presence_only"] <- "Diversity_metric_type"
names(MCDB_subset)[names(MCDB_subset) == "Abundance"] <- "Measurement"

# Changing types of data so the columns can match up 
MCDB_subset$Sampling_effort <- as.double(MCDB_subset$Sampling_effort)
as.double(MCDB_subset$Sampling_effort)

MCDB_subset$Longitude <- as.numeric(MCDB_subset$Longitude)
MCDB_subset$Latitude <- as.numeric(MCDB_subset$Latitude)
as.numeric(MCDB_subset$Longitude)
as.numeric(MCDB_subset$Latitude)

MCDB_subset$`Site_ID/SSS` <- as.factor(MCDB_subset$`Site_ID/SSS`)
as.factor(MCDB_subset$`Site_ID/SSS`)

PREDICTS_subset$Initial_year <- as.character(PREDICTS_subset$Initial_year)
as.character(PREDICTS_subset$Initial_year)

MCDB_subset$Measurement <- as.double(MCDB_subset$Measurement)
as.double(MCDB_subset$Measurement)

MCDB_subset$Site_ID <- as.factor(MCDB_subset$Site_ID)
as.factor(MCDB_subset$Site_ID)
write.csv(MCDB_subset,"~/Desktop/DATABASES/data_modified/MCDB_subset.csv", row.names = FALSE)


#Putting them together
dd <- bind_rows(PREDICTS_subset, MCDB_subset)

#---------------------HARMONIZING/RESCALING SAMPLING EFFORT-----------------------------------------

# Adding Diversity Metric TRUE/FALSE to the MCDB data within the combined frame
dd$Diversity_metric_is_effort_sensitive[dd$Diversity_metric_type == "Occurrence"] <- FALSE
dd$Diversity_metric_is_effort_sensitive[dd$Diversity_metric_type == "Abundance"] <- TRUE

# Rescaling Sampling Effort adding column for max effort
dd$Sampling_effort[dd$Diversity_metric_type == "Occurrence"] <- 1

dd <- dd %>%
  group_by(Reference) %>%
  mutate(max_effort = max(Sampling_effort))

#adding column for effort rescaled
dd <- dd %>%
  mutate(effort_rescaled = Sampling_effort / max_effort)

#adding column for rescaling factor
dd$rescaling_factor = 1/dd$effort_rescaled

#adding a column for plain abundance 
dd <- dd %>% 
  mutate(Abundance = Measurement)
dd$Abundance[dd$Diversity_metric_type == "Occurrence"] <- NA

#adding a column for rescaled abundance
dd$Abundance_rescaled = dd$rescaling_factor * dd$Measurement
names(dd)[names(dd) == "Abundance_rescaled"] <- "Measurement_rescaled"

#---------------------------------------------------------------------------------------------------------

# -----------------Removing sites that don't have long/lat information and sites with invalid longitude information (17)
dd <- dd[complete.cases(dd$Longitude, dd$Latitude), ]

invalid_lon <- dd$Longitude < -180 | dd$Longitude > 180
invalid_rows <- invalid_lon
dd <- dd[!invalid_rows, ]

#------------------------------------------------------------------------------------------------------------

# --------------------------------------Finding studies w one geolocation-----------------------------------

# THERE IS SOMETHING WRONG WITH THIS CODE RN

# Finding which study sites only have one geolocation 
# Read in a subset of the data which has reference, Lat, and Long columns called "all_sites"
# Make a new column that states TRUE or FALSE if all geolocations in one reference are the same
sites_i <- all_sites %>%
  group_by(Reference) %>%
  mutate(All_Same = all_sites_same(Latitude == first(Latitude) & Longitude == first(Longitude)))

# Merge the resulting column into the big data frame
dd2 <- dd2 %>%
  left_join(sites_i %>% select(Reference, all_sites_same) %>% distinct(), by = "Reference")

#---------------------------------------------------------------------------------------------------------
#--------------------------------------RESOLVING TAXONOMY---------------------------------------------------
# Merging the NCBI Resolved taxonomy data
# Removing unresolved observations from the mammal binomials resolved data 
mammal_binomials_resolved <- mammal_binomials_resolved %>%
  filter(HostNCBIResolved)

# Combining the "to resolve" and "mammal binomials resolved" data
binoms_final <- bind_rows(to_resolve_edits, mammal_binomials_resolved)

# renaming so they merge
names(binoms_final)[names(binoms_final) == "Binomial"] <- "Best_guess_binomial"

# subsetting so there's no repeat columns 
binoms_final <- binoms_final %>% dplyr::select(Best_guess_binomial, HostTaxID, HostNCBIResolved, Host, 
                                               HostGenus, HostFamily, HostOrder, HostClass)



# throw those bad boys together!!!
dd3 <- left_join(dd2, binoms_final, by = "Best_guess_binomial")

#-------------------------------------------------------------------------------------------------------------

# ----------------------------Adding in the Virion Data ------------------------------------------------------
# Load in Virion database
# subset to only include columns we want
virion_subset <- Virion %>% select(HostTaxID, VirusTaxID, VirusNCBIResolved, VirusGenus, 
                                   VirusFamily, VirusOrder, VirusClass, VirusOriginal)

# Calculating viral richness for each species
virion_subset <- virion_subset %>%
  group_by(HostTaxID) %>%
  mutate(Virus_richness = n_distinct(VirusTaxID))

# subsetting for just unique hosts and associated viral richness
viral_richness <- virion_subset %>% 
  select(HostTaxID, Virus_richness) 

viral_richness <- viral_richness %>%
  distinct(HostTaxID, .keep_all = TRUE)

dd2 <- left_join(dd2, viral_richness, by = "HostTaxID")

# Making the N/As in the virus richness column 0 
dd2 <- dd2 %>%
  mutate(Virus_richness = replace_na(Virus_richness, 0))

#----------------Adding in categorical classification for land use (might not use)---------------------------------

cop_7 <- cop_6 %>% select(`Site_ID/SSS`, classification)
dd4 <- left_join(dd3, cop_7, by = "Site_ID/SSS")

# ---------------------------------------Loop to run through the viral sharing data--------------------

folder_path <- "~/Desktop/DATABASES/data_modified"
file_path <- file.path(folder_path, "dd4.rds")
saveRDS(dd4, file = file_path)

library(dplyr)
library(tidyr)
library(ggplot2)

viralsharing = albersnet_predicts_resolved
dd4 = dd5
dd4$`Site_ID/SSS` = as.vector(dd4$`Site_ID/SSS`)

# add year
dd4$Year = as.numeric(substr(dd4$Initial_year, 1, 4))

# view distribution of years; exclude everything before 1980 
dd4 %>% 
  dplyr::select(`Site_ID/SSS`, Year) %>%
  distinct() %>%
  ggplot() + 
  geom_histogram(aes(x=Year))

# filter out earlier years (keep 1985 onwards) and keep lat lon referenced
dd4 = dd4[ !is.null(dd4$Year) & dd4$Year > 1985, ]
dd4 = dd4[ !is.na(dd4$Longitude) & !is.na(dd4$Latitude), ]
n_distinct(dd4$`Site_ID/SSS`) # 2,888 sites

# exclude domestic species = important
load("domestic_species.R")
domestic = Hmisc::capitalize(domestic)
dd4 = dd4[ !dd4$Host %in% domestic, ]

# subset for testing
#site_data = dd4[ dd4$`Site_ID/SSS` %in% sample(unique(dd4$`Site_ID/SSS`), 40, replace=FALSE), ]

site_data = dd4

vs_results = data.frame()

unique_sites = as.vector(unique(site_data$`Site_ID/SSS`))

for(i in unique_sites){
  
  print(i)
  
  sitedata_i = site_data %>%
    dplyr::filter(`Site_ID/SSS` == i) %>%
    dplyr::filter(Measurement > 0)
  
  total_n_species = nrow(sitedata_i)
  
  site_network = expand.grid(unique(sitedata_i$Host), unique(sitedata_i$Host)) %>%
    as.data.frame() %>%
    dplyr::rename("Species1"=1, "Species2"=2) %>%
    dplyr::filter(Species1 != Species2) %>%
    dplyr::left_join(sitedata_i[ , c("Host", "Measurement_rescaled") ] %>% dplyr::rename("Species1"=1, "Abundance1"=2)) %>%
    dplyr::left_join(sitedata_i[ , c("Host", "Measurement_rescaled") ] %>% dplyr::rename("Species2"=1, "Abundance2"=2))
  
  # # check: are all species in network data
  # all_species_in_network = all(unique(site_network$Species1) %in% viralsharing$Host_1)
  # if(all_species_in_network == FALSE){
  # 
  #   # which species aren't in viral sharing network
  #   missing_sp = unique( site_network$Species1 )[ !unique( site_network$Species1 ) %in% viralsharing$Host_1 ]
  # 
  #   # for each, try and match by genus
  #   # and add to viralsharing network
  #   for(m in missing_sp){
  # 
  #     m_genus = strsplit(m, " ")[[1]][1]
  # 
  #     # get same genus spp from viralsharing
  #     vs_m = viralsharing[ grep(m_genus, viralsharing$Host_1), ]
  # 
  #   }
  # 
  # }
  
  site_vs = viralsharing %>%
    dplyr::select(Host_1, Host_2, ViralSharingProb) %>% # could do this on taxid
    dplyr::rename(Species1=1, Species2=2) %>%
    dplyr::filter(Species1 %in% c(site_network$Species1, site_network$Species2) & Species2 %in% c(site_network$Species1, site_network$Species2)) # keep species that are in the site
  #site_vs$ViralSharingProb[ site_vs$Species1 == site_vs$Species2 ] = 1
  
  # check if any missing
  species_in_vs_data = sitedata_i$Host %in% unique(c(viralsharing$Host_1, viralsharing$Host_2))
  missingdata_n = sum(species_in_vs_data == FALSE)
  
  # combine
  site_network = dplyr::left_join(site_network, site_vs)
  
  # remove pairs with missing probabilities
  site_network = site_network %>% dplyr::filter(!is.na(ViralSharingProb))
  
  # calculate mean and CWM
  site_network$total_interactions = site_network$Abundance1 * site_network$Abundance2
  cmw = sum(site_network$ViralSharingProb * site_network$total_interactions, na.rm=TRUE) / sum(site_network$total_interactions, na.rm=TRUE)
  
  results_site = data.frame(
    site = i,
    mean_viralsharing = mean(site_network$ViralSharingProb, na.rm=TRUE),
    cmw_viralsharing = cmw,
    total_species_present = total_n_species,
    missingspecies_viralsharing = missingdata_n
  )
  
  vs_results = rbind(vs_results, results_site)
  
}

# ---------------------------Add dd5_remotesensing metrics-----------------------------------------------------------

names(dd5_remotesensingmetrics)[names(dd5_remotesensingmetrics) == "Site_ID/SSS"] <- "site"
results <- left_join(dd5_remotesensingmetrics, vs_results1, by = "site")

site_dates <- dd5 %>% select(`Site_ID/SSS`, Initial_year)
site_dates1 <- unique(site_dates)
names(site_dates1)[names(site_dates1) == "Site_ID/SSS"] <- "site"

results1 <- left_join(results, site_dates1, by = "site")
results1$Initial_year <- as.Date(results1$Initial_year)
results1$Year <- year(results1$Initial_year)

names(break_dates_df)[names(break_dates_df) == "Site"] <- "site"
results2 <- left_join(results1, break_dates_df, by = "site")

ggplot(results2, aes(x=Break_Date)) +
  geom_histogram()

results2$time_since_conversion <- results2$Year - results2$Break_Date

results_positive <- subset(results2, time_since_conversion > 0)

# ----------------------------- lo and behold it's a results data frame!-------------------------------------------#

#----------------------------- NOW TIME TO CALCULATE HOST RICHNESS THIS SHOULD BE FUN------------------------------------#

library(tidyverse)
library(Hmisc)
library(stringr)

# Load in zoonotic edge hosts data frame from Rory

# Subset to just the stricter collected data

hosts <- zoonotichosts_edgelist_VIRIONplusCLOVER %>%
  filter(HostCriteria == "Stricter (sequencing or isolation)")

# Make the first letter of each host capitalzied so it will merge correctly with my data
str_sub(hosts$Host, 1, 1) <- str_sub(hosts$Host, 1, 1) %>% str_to_upper()

hosts_sub <- hosts %>% select(Host, ZoonoticHost)

hosts_sub$ZoonoticHost[hosts_sub$ZoonoticHost == "TRUE"] <- 1
hosts_sub$ZoonoticHost[hosts_sub$ZoonoticHost == "FALSE"] <- 0

dd6 <- left_join(dd5, hosts_sub, by = "Host")

names(dd6)[names(dd6) == "Site_ID/SSS"] <- "site"

dd7 <- dd6 %>%
  group_by(site) %>%
  mutate(site_host_richness = sum(ZoonoticHost, na.rm = TRUE)) %>%
  ungroup()

dd_sub <- dd7 %>% select(site, site_host_richness)
dd_sub <- dd_sub %>% distinct(site, .keep_all = TRUE)

folder_path <- "~/Desktop/DATABASES/data_modified"
file_path <- file.path(folder_path, "dd7.rds")
saveRDS(dd7, file = file_path)

# --------------------------------------------------- MODELSSSSSSS------------------------------------------------------------#

library(lme4)
library(lmerTest)
names(results2)[names(results2) == "Reference.x"] <- "Reference"

model1 = lmer(cmw_viralsharing ~ PrimaryLand_5km*Dissim_5km + (1 |site) + (1 |Reference), data = results2)



hist(results2$PrimaryLand_500m)

summary(model1)

print(model1)









