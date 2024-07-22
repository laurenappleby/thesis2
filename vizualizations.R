library(dplyr)
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)
install.packages("RColorBrewer")
library(RColorBrewer)

dd_zoo <- dplyr::filter(dd5, HostOrder %in% c("Rodentia", "Chiroptera", "Eulipotyphla", "Carnivora", "Artiodactyla", "Primates"))

dd_plot <- ggplot(data.frame(dd_zoo), aes(x=HostOrder, fill = HostOrder)) +
  geom_bar(width = 0.7) + scale_fill_brewer(palette = "Set2") +
  labs(title = "Mammal Communities", x = "Order", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
dd_plot

df_with_frequency <- dd_zoo %>%
  group_by(HostOrder) %>%
  mutate(Frequency = n()) %>%
  ungroup()

frequency <- data.frame(Order = c("Rodentia", "Chiroptera", "Eulipotyphla", "Carnivora", "Artiodactyla", "Primates"),
                        Frequency = c(13492, 6692, 1556, 6496, 4050, 1774))

pie(frequency$Frequency, labels = frequency$Order)

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

install.packages("hrbthemes")
library(hrbthemes)



rm(vs_results)
rm(site_vs)
rm(ras)
rm(mammal_binomials_resolved)
rm(MCDB_subset)
rm(to_resolve_edits)

#install.packages("gridExtra")
#library(gridExtra)

#grid.arrange(predicts1_plot, predicts2_plot, MCDB_plot, ncol = 2) 

