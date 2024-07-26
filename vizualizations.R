library(dplyr)
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)
install.packages("RColorBrewer")
library(RColorBrewer)
library(gridExtra)

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

names(dd4)[names(dd4) == "Site_ID/SSS"] <- "site"
unique(dd4$HostOrder)

dd_zoo <- dplyr::filter(dd5, HostOrder %in% c("Rodentia", "Chiroptera", "Eulipotyphla", "Carnivora", "Artiodactyla", "Primates"))

dd_plot <- ggplot(data.frame(dd_zoo), aes(x=HostOrder, fill = HostOrder)) +
  geom_bar(width = 0.7) + scale_fill_brewer(palette = "Set2") +
  labs(title = "Mammal Communities", x = "Order", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
dd_plot

zoo_predicts <- head(dd_zoo, 26610)

df_with_frequency <- zoo_predicts %>%
  group_by(HostOrder) %>%
  mutate(Frequency = n()) %>%
  ungroup()

frequency_predicts <- data.frame(Order = c("Rodentia", "Chiroptera", "Eulipotyphla", "Carnivora", "Artiodactyla", "Primates"),
                        Frequency = c(7668, 6692, 600, 5936, 3946, 1768),
                        study = c("Predicts", "Predicts", "Predicts", "Predicts", "Predicts", "Predicts"))


frequency_predicts <- frequency_predicts %>%
  dplyr::mutate(Percentage = Frequency / sum(Frequency) * 100)

palette <- brewer.pal(n = 6, name = "Set3")
set3_colors_6 <- c("#8DD3C7", "#FFFFB3", "#FDB462", "#FB8072", "#80B1D3", "#BEBADA")

#FDB462
# Create the pie chart with a legend
predicts_plot <- ggplot(frequency_predicts, aes(x = "", y = Frequency, fill = Order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + # Remove background, grid, and axis elements
  labs(fill = "Order", title = "Predicts Mammals") +
  theme(legend.position = "left") +
  scale_fill_manual(values = set3_colors_6)


pie(frequency_predicts$Frequency, labels = frequency_predicts$Order)

zoo_mc <- tail(dd_zoo, 7450)

df_with_frequency <- zoo_mc %>%
  group_by(HostOrder) %>%
  mutate(Frequency = n()) %>%
  ungroup()

frequency_mc <- data.frame(Order = c("Rodentia", "Eulipotyphla", "Carnivora", "Artiodactyla", "Primates"),
                        Frequency = c(5824, 956, 560, 104, 6),
                        study = c("Mammal Communities", "Mammal Communities", "Mammal Communities", "Mammal Communities", "Mammal Communities"))

frequency_mc <- frequency_mc %>%
  dplyr::mutate(Percentage = Frequency / sum(Frequency) * 100)

set3_colors <- c("#8DD3C7", "#FFFFB3","#FB8072", "#80B1D3","#BEBADA" )
# Create the pie chart with a legend
mc_plot <- ggplot(frequency_mc, aes(x = "", y = Frequency, fill = Order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + # Remove background, grid, and axis elements
  labs(fill = "Order", title = "Mammal Communities") +
  theme(legend.position = "left") +
  scale_fill_manual(values = set3_colors)

combined_plot <- grid.arrange(predicts_plot, mc_plot, ncol = 1)
ggsave("combined_plot_with_legends_on_left.png", combined_plot, width = 10, height = 12)

#pie(frequency_mc$Frequency, labels = frequency_mc$Order)

#frequency <- bind_rows(frequency_mc, frequency_predicts)

#ggplot(frequency, aes(fill=study, y=Frequency, x=Order)) + 
  geom_bar(position="dodge", stat="identity")

##############################----------Exploration---------------#######################
#-----------------------------------------------------------------------------------------

ggplot(dd5_remotesensingmetrics, aes(x=Dissim_5km, y=PrimaryLand_5km)) + 
  geom_point(
      color="black",
      fill="#69b3a2",
      shape=22,
      alpha=1,
      size=2,
      stroke = 0.5) + 
  theme_classic() +
  xlim(0,5)

names(dd5_remotesensingmetrics)[names(dd5_remotesensingmetrics) == "Site_ID/SSS"] <- "site"
results <- left_join(dd5_remotesensingmetrics, vs_results1, by = "site")

ggplot(results, aes(x=Dissim_500m, y=cmw_viralsharing)) +
  geom_point(
    color="black",
    fill="blue",
    shape=22,
    alpha=1,
    size=2,
    stroke=0.5) +
  theme_classic() +
  xlim(0,5)

ggplot(results, aes(x=PrimaryLand_500m, y=cmw_viralsharing)) +
  geom_point(
    color="black",
    fill="blue",
    shape=22,
    alpha=1,
    size=2,
    stroke=0.5) +
  theme_classic()
  


site_dates <- dd5 %>% select(`Site_ID/SSS`, Initial_year)
site_dates1 <- unique(site_dates)
names(site_dates1)[names(site_dates1) == "Site_ID/SSS"] <- "site"

results1 <- left_join(results, site_dates1, by = "site")
results1$Initial_year <- as.Date(results1$Initial_year)
results1$Year <- year(results1$Initial_year)

ggplot(results1, aes(x=Year, y=cmw_viralsharing)) +
  geom_point(
    color="black",
    fill="blue",
    shape=22,
    alpha=1,
    size=2,
    stroke=0.5) +
  theme_classic()

ggplot(results1, aes(x=Initial_year, y=cmw_viralsharing)) +
  geom_line() + 
  xlab("")

ggplot(results1, aes(x=Dissim_500m, y=PrimaryLand_500m, size = cmw_viralsharing)) +
  geom_point(alpha=0.7) +
  xlim(0,5) +
  scale_size(range = c(.001, 2))

names(break_dates_df)[names(break_dates_df) == "Site"] <- "site"
results2 <- left_join(results1, break_dates_df, by = "site")

ggplot(results2, aes(x=Break_Date)) +
  geom_histogram()

results2$time_since_conversion <- results2$Year - results2$Break_Date

results_positive <- subset(results2, time_since_conversion > 0)

ggplot(results_positive, aes(x=time_since_conversion, y=cmw_viralsharing)) +
  geom_point() +
  xlim(0,2) +
  theme_classic()


folder_path <- "~/Desktop/DATABASES/data_modified"
file_path <- file.path(folder_path, "results2.rds")
saveRDS(results2, file = file_path)

install.packages("hrbthemes")
library(hrbthemes)

###################################----------- MAP--------------------#################################
#------------------------------------------------------------------------------------------------------

# Define the destination directory
dest_dir <- "DATA"
# Check if the directory exists, and create it if it doesn't
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir)
}

# Define the destination file path
dest_file <- file.path(dest_dir, "world_shape_file.zip")

# Download the file
download.file(
  "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/world_shape_file.zip",
  destfile = dest_file
)

# Unzip the file
unzip(dest_file, exdir = dest_dir)

# List the contents of the DATA directory to find the shapefile path
print(list.files(dest_dir, recursive = TRUE))

# Load the required packages
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(sf)

# Determine the correct path to the shapefile
shapefile_path <- list.files(dest_dir, pattern = "\\.shp$", full.names = TRUE)

# Check if the shapefile was found
if (length(shapefile_path) == 0) {
  stop("Shapefile not found in the unzipped contents.")
} else {
  # Read the shapefile
  world <- st_read(shapefile_path[1])
  
  # Print a summary of the shapefile
  print(world)
}

ggplot(world) +
  geom_sf() +
  theme_minimal() +
  labs(title = "World Map")

predicts_countries <- PREDICTS_mammalia %>% select(SSS, Longitude, Latitude, Country)
predicts_countries <- predicts_countries %>% distinct(SSS, .keep_all = TRUE)
predicts_countries <- predicts_countries %>% select(Longitude, Latitude, Country)
MCDB_countries <- MCDB_sites %>% select(Longitude, Latitude, Country)

MCDB_countries$Longitude <- as.double(MCDB_countries$Longitude)
MCDB_countries$Latitude <- as.double(MCDB_countries$Latitude)
as.double(MCDB_countries$Longitude)
as.double(MCDB_countries$Latitude)


all_countries <- bind_rows(MCDB_countries, predicts_countries)

all_countries <- all_countries %>%
  mutate(Country = recode(Country,
                          "United States of America" = "United States"))

country_counts <- all_countries %>%
  group_by(Country) %>%
  summarize(num_sites = n())

world1 <- left_join(world, country_counts, by = c("NAME" = "Country"))
world1$num_sites[is.na(world$num_sites)] <- 0

ggplot(world1) +
  geom_sf(aes(fill = num_sites)) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "white") +
  theme_minimal() +
  labs(
    title = "Number of Sites per Country",
    fill = "Number of Sites"
  )


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

names(dd4)[names(dd4) == "Site_ID/SSS"] <- "site"

sites <- dd4 %>% dplyr::select(site, Longitude, Latitude)
sites <- sites %>% distinct(site, .keep_all = TRUE)
sites_predicts <- head(sites, 2409)
sites_mc <- tail(sites, 479)

predicts_sf <- st_as_sf(sites_predicts, coords = c("Longitude", "Latitude"), crs = 4326)
mc_sf <- st_as_sf(sites_mc, coords = c("Longitude", "Latitude"), crs = 4326)

map_plot <- ggplot(world1) +
  geom_sf(aes(fill = num_sites)) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "white") +
  theme_minimal() +
  labs(fill = "Number of Sites per Country") +
  geom_sf(data = predicts_sf, aes(color = "Predicts Mammals"), size = 0.6) +
  geom_sf(data = mc_sf, aes(color = "Mammal Communities"), size = 0.6) +
  scale_color_manual(
    values = c("Predicts Mammals" = "red", "Mammal Communities" = "#4B0082"),
    guide = guide_legend(
      title = "Site Source",
      title.position = "top",
      title.hjust = 0.2,
      override.aes = list(size = 4)
    )
  ) +
  theme(legend.position = "bottom")

map_plot  
ggsave("map_with_sites2.png", plot = map_plot, width = 10, height = 8, dpi = 300)
  
  
  #geom_sf(data = all_sites_sf, aes(color = "red"), size = 0.2)
 


pru

print(world$NAME)

rm(vs_results)
rm(site_vs)
rm(ras)
rm(mammal_binomials_resolved)
rm(MCDB_subset)
rm(to_resolve_edits)

#install.packages("gridExtra")
#library(gridExtra)

#grid.arrange(predicts1_plot, predicts2_plot, MCDB_plot, ncol = 2) 

