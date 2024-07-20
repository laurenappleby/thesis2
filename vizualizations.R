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
  theme_classic()

install.packages("hrbthemes")
library(hrbthemes)


#install.packages("gridExtra")
#library(gridExtra)

#grid.arrange(predicts1_plot, predicts2_plot, MCDB_plot, ncol = 2) 

