
## 
setwd("C:/Users/roryj/Dropbox/Research/predicts_lauren/")

##
dd = predicts_results_aggregate

ss = strsplit(dd$site, split="[ ]")
ss = unlist(
  lapply(
    lapply(ss, "[", 1:2),
    paste, collapse=" "
  )
)
dd$SS = ss

library(lme4)
library(dplyr)
library(ggplot2)
library(tidyr)

dd$Land_use_classification = replace(dd$Land_use_classification, dd$Land_use_classification %in% c("Secondary_minimal", "Secondary_substantial"), "Secondary")
dd$Land_use_classification = replace(dd$Land_use_classification, dd$Land_use_classification %in% c("Primary_minimal"), "0_Primary_minimal")

# Species Richness Model---------------------------------------------------------------------------------------------------------------------------
######################################################################################################################################################
m1 = glmer(species_richness ~ Land_use_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + (1|SS) + (1|SSB), family="poisson", data=dd)
m15 = glmer(species_richness ~ Land_use_classification + scale(Dissim_1km) + scale(PrimaryLand_1km) + (1|SS) + (1|SSB), family="poisson", data=dd)
m20 = glmer(species_richness ~ Land_use_classification + scale(Dissim_500m) + scale(PrimaryLand_500m) + (1|SS) + (1|SSB), family="poisson", data=dd)

summary(m1)
summary(m15)
summary(m20)
hist(resid(m1), 30)
coef(summary(model1))

fixed_effects <- summary(m1)$coefficients
conf_int <- confint(m1, parm = "beta_", method = "Wald")

forest_data <- data.frame(
  term = rownames(fixed_effects),
  estimate = fixed_effects[, "Estimate"],
  lower = conf_int[, 1],
  upper = conf_int[, 2]
)
custom_labels <- c("(Intercept)", "Managed", "Primary Substantial", "Secondary", "Urban", "Dissimilarity", "Primary Land Cover")
forest_data$term <- custom_labels
forest_data <- forest_data[forest_data$term != "(Intercept)",]
forest_data <- forest_data[forest_data$term != "Dissimilarity",]
forest_data <- forest_data[forest_data$term != "Primary Land Cover",]

forest_data$term <- factor(forest_data$term, 
                             levels = c("Managed", "Primary Substantial", "Secondary", "Urban", 
                                        "Dissimilarity", "Primary Land Cover"))

forest_data$term_type <- ifelse(forest_data$term %in% c("Dissimilarity", "Primary Land Cover"),
                                  "Continuous", "Categorical")



ggplot(forest_data, aes(x = term, y = estimate)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Species Richness") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

levels(dd$Land_use_classification)


# total abundance model; residuals look pretty good---------------------------------------------------------------------------------
######################################################################################################################################
library(lmerTest)
m2 = lmer(log(total_abundance+1) ~ Land_use_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + (1|SS) + (1|SSB), data=dd)

summary(m2)

fixed_effects2 <- summary(m2)$coefficients
conf_int2 <- confint(m2, parm = "beta_", method = "Wald")

forest_data2 <- data.frame(
  term = rownames(fixed_effects2),
  estimate = fixed_effects2[, "Estimate"],
  lower = conf_int2[, 1],
  upper = conf_int2[, 2]
)
custom_labels <- c("(Intercept)", "Managed", "Primary Substantial", "Secondary", "Urban", "Dissimilarity", "Primary Land Cover")
forest_data2$term <- custom_labels
forest_data2 <- forest_data2[forest_data2$term != "(Intercept)", ]
forest_data2 <- forest_data2[forest_data2$term != "Dissimilarity",]
forest_data2 <- forest_data2[forest_data2$term != "Primary Land Cover",]

forest_data2$term <- factor(forest_data2$term, 
                           levels = c("Managed", "Primary Substantial", "Secondary", "Urban", 
                                      "Dissimilarity", "Primary Land Cover"))

forest_data2$term_type <- ifelse(forest_data2$term %in% c("Dissimilarity", "Primary Land Cover"),
                                "Continuous", "Categorical")

ggplot(forest_data2, aes(x = term, y = estimate)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Species Abundance") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

summary(m2)
hist(resid(m2), 100)
#Combining species richness and abundance into the same model---------------------------------------------------------------------------------------------------
#################################################################################################################################################################

forest_data$outcome <- "Richness"
forest_data2$outcome <- "Abundance"

combined_forest_data <- rbind(forest_data, forest_data2)

fplot1 <- ggplot(combined_forest_data, aes(x = term, y = estimate, color = outcome, shape = term_type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, fill = "black") + # Adding fill = "black" for filled shapes
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Primary Minimal") +
  ggtitle("Predicts Land Use Classifications and Alternative Landscape Metrics") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Richness" = "#1f77b4",  
                                "Abundance" = "#2ca02c")) + 
  scale_shape_manual(values = c("Categorical" = 16, # Solid circle for categorical variables
                                "Continuous" = 24)) +
  labs(color = "Metric", shape = "Variable Type") 



plot(fplot1)
ggsave("predicts_class_species.png", width = 8, height = 6, dpi = 300)

################################## Richness and Abundance but this time with the modis data#############################################################
################################## Richness and Abundance but this time with the modis data#############################################################
################################## Richness and Abundance but this time with the modis data#############################################################
rm(dd)
dd$sat_classification[dd$sat_classification == "agriculture"] <- "Managed"
dd$sat_classification[dd$sat_classification == "urban"] <- "Urban"
dd$sat_classification[dd$sat_classification == "natural"] <- "Natural"
dd$sat_classification <- factor(dd$sat_classification, levels = c("Natural", "Managed", "Urban"))


#dd <- dd %>%
 # mutate(sat_classification = case_when(
  #  Land_use_classification == "0_Primary_minimal" ~ "0_Primary_minimal",
   # TRUE ~ sat_classification  # Keep the original value otherwise
  #))

m3 = glmer(species_richness ~ sat_classification + scale(Dissim_500m) + scale(PrimaryLand_500m) + (1|SS) + (1|SSB), family="poisson", data=dd)

summary(m3)

fixed_effects3 <- summary(m3)$coefficients
conf_int3 <- confint(m3, parm = "beta_", method = "Wald")

summary(m3)

forest_data3 <- data.frame(
  term = rownames(fixed_effects3),
  estimate = fixed_effects3[, "Estimate"],
  lower = conf_int3[, 1],
  upper = conf_int3[, 2]
)
custom_labels1 <- c("(Intercept)", "Managed", "Urban", "Dissimilarity", "Primary Land Cover")
forest_data3$term <- custom_labels1
forest_data3 <- forest_data3[forest_data3$term != "(Intercept)", ]
forest_data3 <- forest_data3[forest_data3$term != "Dissimilarity",]
forest_data3 <- forest_data3[forest_data3$term != "Primary Land Cover",]

forest_data3$term <- factor(forest_data3$term, 
                            levels = c("Managed", "Urban", 
                                       "Dissimilarity", "Primary Land Cover"))

forest_data3$term_type <- ifelse(forest_data3$term %in% c("Dissimilarity", "Primary Land Cover"),
                                 "Continuous", "Categorical")


ggplot(forest_data3, aes(x = term, y = estimate)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Species Richness") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

#### Satellite Land Classification Abundance----------------------------------------------------------------------------------------------
#############################################################################################################################################

m4 = lmer(log(total_abundance+1) ~ sat_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + (1|SS) + (1|SSB), data=dd)

fixed_effects4 <- summary(m4)$coefficients
conf_int4 <- confint(m4, parm = "beta_", method = "Wald")

summary(m4)

forest_data4 <- data.frame(
  term = rownames(fixed_effects4),
  estimate = fixed_effects4[, "Estimate"],
  lower = conf_int4[, 1],
  upper = conf_int4[, 2]
)
#custom_labels1 <- c("(Intercept)", "Managed", "Natural", "Urban", "Dissimilarity", "Primary Land Cover")
forest_data4$term <- custom_labels1
forest_data4 <- forest_data4[forest_data4$term != "(Intercept)", ]
forest_data4 <- forest_data4[forest_data4$term != "Dissimilarity",]
forest_data4 <- forest_data4[forest_data4$term != "Primary Land Cover",]

forest_data4$term <- factor(forest_data4$term, 
                            levels = c("Managed", "Urban", 
                                       "Dissimilarity", "Primary Land Cover"))

forest_data4$term_type <- ifelse(forest_data4$term %in% c("Dissimilarity", "Primary Land Cover"),
                                 "Continuous", "Categorical")

ggplot(forest_data4, aes(x = term, y = estimate)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Species Abundance") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))
##########################################################################################################################################
#-------------------------------------Combining the sat classification models--------------------------------------------------------
##########################################################################################################################################

forest_data3$outcome <- "Richness"
forest_data4$outcome <- "Abundance"

combined_forest_data2 <- rbind(forest_data3, forest_data4)

fplot2 <- ggplot(combined_forest_data2, aes(x = term, y = estimate, color = outcome, shape = term_type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, fill = "black") + # Adding fill = "black" for filled shapes
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Natural") +
  ggtitle("Satellite Land Use Classifications and Alternative Landscape Metrics") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Richness" = "#1f77b4",  
                                "Abundance" = "#2ca02c")) + 
  scale_shape_manual(values = c("Categorical" = 16, # Solid circle for categorical variables
                                "Continuous" = 24)) +
  labs(color = "Metric", shape = "Variable Type") 

plot(fplot2)
################################################ Put them together ################################################################
################################################ Put them together ################################################################
library(gridExtra)
predicts_land_forest <- grid.arrange(fplot1, fplot2, ncol = 2)

ggsave("predicts_land_forest.png", plot = predicts_land_forest, width = 12, height = 6)

################################################ model 2 ################################################################
################################################ model 2 ################################################################
################################################ model 2 ################################################################


# mean viral sharing model; resids look ok although arguably a beta distribution may be more appropriate
# beta dist: https://rpubs.com/nicoleknight/936037
# (probably a wormhole so I think the lmer is probably fine within project timescale unless you're keen to explore)

# because the viral sharing metrics are bounded between 0 and 1 I would probably scale it to aid in interpretation
# covariates scaled to ensure on a standardised scale

# i think perhaps just a scaled (rather than log) species richness is more appropraite with the Gaussian likelihood

m5 = lmer(scale(mean_viralsharing) ~ Land_use_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + (1|SS) + (1|SSB), data=dd)

summary(m5)

fixed_effects5 <- summary(m5)$coefficients
conf_int5 <- confint(m5, parm = "beta_", method = "Wald")

forest_data5 <- data.frame(
  term = rownames(fixed_effects5),
  estimate = fixed_effects5[, "Estimate"],
  lower = conf_int5[, 1],
  upper = conf_int5[, 2]
)
custom_labels3 <- c("(Intercept)", "Managed", "Primary Substantial", "Secondary", "Urban", "Dissimilarity", "Primary Land Cover", "Species Richness")
forest_data5$term <- custom_labels3
forest_data5 <- forest_data5[forest_data5$term != "(Intercept)", ]
forest_data5$term <- factor(forest_data5$term, 
                            levels = c("Managed", "Primary Substantial", 
                                       "Secondary", "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data5$term_type <- ifelse(forest_data5$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data5, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Mean Viral Sharing") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)


summary(m3)
hist(resid(m3), 100)
plot(m3)
 
############################################## cmw viral sharing model#######################################################################
############################################## cmw viral sharing model#######################################################################
############################################## cmw viral sharing model#######################################################################

dd_ab = dd %>% dplyr::filter(Diversity_metric_type == "Abundance")

m6 = lmer(scale(cmw_viralsharing) ~ Land_use_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + (1|SS) + (1|SSB), data=dd_ab)

summary(m6)

fixed_effects6 <- summary(m6)$coefficients
conf_int6 <- confint(m6, parm = "beta_", method = "Wald")

forest_data6 <- data.frame(
  term = rownames(fixed_effects6),
  estimate = fixed_effects6[, "Estimate"],
  lower = conf_int6[, 1],
  upper = conf_int6[, 2]
)
custom_labels3 <- c("(Intercept)", "Managed", "Primary Substantial", "Secondary", "Urban", "Dissimilarity", "Primary Land Cover", "Species Richness")
forest_data6$term <- custom_labels3
forest_data6 <- forest_data6[forest_data6$term != "(Intercept)", ]
forest_data6$term <- factor(forest_data6$term, 
                            levels = c("Managed", "Primary Substantial", 
                                       "Secondary", "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data6$term_type <- ifelse(forest_data6$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data6, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Mean Viral Sharing") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)

summary(m4)
hist(resid(m4), 100)

############################################## combining plots #######################################################################
############################################## combining plots #######################################################################
############################################## combining plots #######################################################################


forest_data5$outcome <- "Mean Viral Sharing"
forest_data6$outcome <- "Community Mean Weighted Viral Sharing"

combined_forest_data3 <- rbind(forest_data5, forest_data6)
combined_forest_data3$shape_group <- ifelse(combined_forest_data3$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"), 
                                            "Continuous", 
                                            "Categorical")

fplot3 <- ggplot(combined_forest_data3, aes(x = term, y = estimate, color = outcome, shape = shape_group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, fill = "black") + # Adding fill = "black" for filled shapes
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Primary Minimal") +
  ggtitle("Predicts Land Use Classifications and Alternative Landscape Metrics") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Mean Viral Sharing" = "#E69F00",  
                                "Community Mean Weighted Viral Sharing" = "#9B59B6")) + 
  scale_shape_manual(values = c("Categorical" = 16, # Solid circle for categorical variables
                                "Continuous" = 24)) + # Filled triangle for continuous variables
  labs(color = "Metric", shape = "Variable Type")
plot(fplot3)
############################################## mean viral sharing sat class plots #######################################################################
############################################## mean viral sharing sat class plots #######################################################################
############################################## mean viral sharing sat class plots #######################################################################

dd$sat_classification[dd$sat_classification == "agriculture"] <- "Managed"
dd$sat_classification[dd$sat_classification == "urban"] <- "Urban"
dd$sat_classification[dd$sat_classification == "Natural"] <- "Natural"

#dd <- dd %>%
 # mutate(sat_classification = case_when(
  #  Land_use_classification == "0_Primary_minimal" ~ "0_Primary_minimal",
   # TRUE ~ sat_classification  # Keep the original value otherwise
  #))

m7 = lmer(scale(mean_viralsharing) ~ sat_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + (1|SS) + (1|SSB), data=dd)

summary(m7)

fixed_effects7 <- summary(m7)$coefficients
conf_int7 <- confint(m7, parm = "beta_", method = "Wald")

forest_data7 <- data.frame(
  term = rownames(fixed_effects7),
  estimate = fixed_effects7[, "Estimate"],
  lower = conf_int7[, 1],
  upper = conf_int7[, 2]
)
custom_labels4 <- c("(Intercept)", "Managed", "Urban", "Dissimilarity", "Primary Land Cover", "Species Richness")
forest_data7$term <- custom_labels4
forest_data7 <- forest_data7[forest_data7$term != "(Intercept)", ]

forest_data7$term <- factor(forest_data7$term, 
                            levels = c("Managed", "Natural", 
                                      "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data7$term_type <- ifelse(forest_data7$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data7, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Mean Viral Sharing") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)

############################################## mean viral sharing sat class plots #######################################################################
############################################## mean viral sharing sat class plots #######################################################################
############################################## mean viral sharing sat class plots #######################################################################

m8 = lmer(scale(cmw_viralsharing) ~ sat_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + (1|SS) + (1|SSB), data=dd_ab)

summary(m8)

dd_ab$sat_classification[dd_ab$sat_classification == "agriculture"] <- "Managed"
dd_ab$sat_classification[dd_ab$sat_classification == "urban"] <- "Urban"
dd_ab$sat_classification[dd_ab$sat_classification == "natural"] <- "Natural"
dd_ab$sat_classification <- factor(dd_ab$sat_classification, levels = c("Natural", "Managed", "Urban"))

#dd_ab <- dd_ab %>%
 # mutate(sat_classification = case_when(
  #  Land_use_classification == "0_Primary_minimal" ~ "0_Primary_minimal",
   # TRUE ~ sat_classification  # Keep the original value otherwise
  #))

fixed_effects8 <- summary(m8)$coefficients
conf_int8 <- confint(m8, parm = "beta_", method = "Wald")

forest_data8 <- data.frame(
  term = rownames(fixed_effects8),
  estimate = fixed_effects8[, "Estimate"],
  lower = conf_int8[, 1],
  upper = conf_int8[, 2]
)
#custom_labels4 <- c("(Intercept)", "Managed",  "Urban", "Dissimilarity", "Primary Land Cover", "Species Richness")
forest_data8$term <- custom_labels4
forest_data8 <- forest_data8[forest_data8$term != "(Intercept)", ]

forest_data8$term <- factor(forest_data8$term, 
                            levels = c("Managed", "Natural", 
                                       "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data8$term_type <- ifelse(forest_data8$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data8, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Primary Minimal") +
  ggtitle("Mean Viral Sharing") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)

############################################## combining plots #######################################################################
############################################## combining plots #######################################################################
############################################## combining plots #######################################################################
forest_data7$outcome <- "Mean Viral Sharing"
forest_data8$outcome <- "Community Mean Weighted Viral Sharing"

combined_forest_data4 <- rbind(forest_data7, forest_data8)
combined_forest_data4$shape_group <- ifelse(combined_forest_data4$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"), 
                                            "Continuous", 
                                            "Categorical")

fplot4 <- ggplot(combined_forest_data4, aes(x = term, y = estimate, color = outcome, shape = shape_group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, fill = "black") + # Adding fill = "black" for filled shapes
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Primary Minimal") +
  ggtitle("Satellite Land Use Classifications and Alternative Landscape Metrics") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Mean Viral Sharing" = "#E69F00",  
                                "Community Mean Weighted Viral Sharing" = "#9B59B6")) + 
  scale_shape_manual(values = c("Categorical" = 16, # Solid circle for categorical variables
                                "Continuous" = 24)) + # Filled triangle for continuous variables
  labs(color = "Metric", shape = "Variable Type")
plot(fplot4)
################################################ Put them together ################################################################
################################################ Put them together ################################################################
library(gridExtra)
disease_forest <- grid.arrange(fplot3, fplot4, ncol = 1)

ggsave("disease_forest.png", plot = disease_forest, width = 12, height = 10)

################################################ Effect Plot ################################################################
################################################ Effect Plot ################################################################


library(jtools)
effect_plot(m1, pred=Dissim_5km, interval = TRUE, plot.points = FALSE,
            jitter = 0.05)

effect_plot(m1, pred=PrimaryLand_5km, interval = TRUE, plot.points = FALSE,
            jitter = 0.05)

effect_plot(m2, pred=Dissim_5km, interval = TRUE, plot.points = FALSE,
            jitter = 0.05)

effect_plot(m2, pred=PrimaryLand_5km, interval = TRUE, plot.points = FALSE,
            jitter = 0.05)

################################################ MC Plot ################################################################
################################################ MC Plot ################################################################

results$classification[results$classification == "agriculture"] <- "Managed"
results$classification[results$classification == "urban"] <- "Urban"
results$classification[results$classification == "natural"] <- "Natural"
results$classification <- factor(results$classification, levels = c("Natural", "Managed", "Urban"))


m9 = lmer(scale(mean_viralsharing) ~ classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + (1|Reference), data=results)

summary(m9)

fixed_effects9 <- summary(m9)$coefficients
conf_int9 <- confint(m9, parm = "beta_", method = "Wald")

forest_data9 <- data.frame(
  term = rownames(fixed_effects9),
  estimate = fixed_effects9[, "Estimate"],
  lower = conf_int9[, 1],
  upper = conf_int9[, 2]
)
custom_labels5 <- c("(Intercept)", "Managed","Urban", "Dissimilarity", "Primary Land Cover", "Species Richness")
forest_data9$term <- custom_labels5
forest_data9 <- forest_data9[forest_data9$term != "(Intercept)", ]

forest_data9$term <- factor(forest_data9$term, 
                            levels = c("Managed", 
                                       "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data9$term_type <- ifelse(forest_data9$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data9, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Natural") +
  ggtitle("Mean Viral Sharing") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)

################################################ MC Plot ################################################################
################################################ MC Plot ################################################################

results = results %>% left_join(zhost_data)

m10 = lmer(scale(HostRichness) ~ classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + MeanPubs_log + (1|Reference), data=results)

summary(m10)

fixed_effects10 <- summary(m10)$coefficients
conf_int10 <- confint(m10, parm = "beta_", method = "Wald")

forest_data10 <- data.frame(
  term = rownames(fixed_effects10),
  estimate = fixed_effects10[, "Estimate"],
  lower = conf_int10[, 1],
  upper = conf_int10[, 2]
)
custom_labels6 <- c("(Intercept)", "Managed","Urban", "Dissimilarity", "Primary Land Cover", "Species Richness", "Sampling Effort")
forest_data10$term <- custom_labels6
forest_data10 <- forest_data10[forest_data10$term != "(Intercept)", ]
forest_data10 <- forest_data10[forest_data10$term != "Sampling Effort", ]


forest_data10$term <- factor(forest_data10$term, 
                            levels = c("Managed", 
                                       "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data10$term_type <- ifelse(forest_data10$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data10, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Natural") +
  ggtitle("Host Richness") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)

################################################ MC Plot ################################################################
################################################ MC Plot ################################################################

m11 = lmer(log(HostAbundance+1) ~ classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + MeanPubs_log + (1|Reference), data=results)

summary(m11)

fixed_effects11 <- summary(m11)$coefficients
conf_int11 <- confint(m11, parm = "beta_", method = "Wald")

forest_data11 <- data.frame(
  term = rownames(fixed_effects11),
  estimate = fixed_effects11[, "Estimate"],
  lower = conf_int11[, 1],
  upper = conf_int11[, 2]
)
custom_labels6 <- c("(Intercept)", "Managed","Urban", "Dissimilarity", "Primary Land Cover", "Species Richness", "Sampling Effort")
forest_data11$term <- custom_labels6
forest_data11 <- forest_data11[forest_data11$term != "(Intercept)", ]
forest_data11 <- forest_data11[forest_data11$term != "Sampling Effort", ]


forest_data11$term <- factor(forest_data11$term, 
                             levels = c("Managed", 
                                        "Urban", # Add all categorical terms first
                                        "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data11$term_type <- ifelse(forest_data11$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                  "Continuous", "Categorical")

ggplot(forest_data11, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Natural") +
  ggtitle("Host Abundance") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)

################################################ MC Plot ################################################################
################################################ MC Plot ################################################################

results_ab = results %>% dplyr::filter(Diversity_metric_type == "Abundance")

m12 = lmer(scale(cmw_viralsharing) ~ classification + scale(Dissim_500m) + scale(PrimaryLand_500m) + scale(species_richness) + (1|Reference), data=results)

summary(m12)

fixed_effects12 <- summary(m12)$coefficients
conf_int12 <- confint(m12, parm = "beta_", method = "Wald")

forest_data12 <- data.frame(
  term = rownames(fixed_effects12),
  estimate = fixed_effects12[, "Estimate"],
  lower = conf_int12[, 1],
  upper = conf_int12[, 2]
)
custom_labels5 <- c("(Intercept)", "Managed","Urban", "Dissimilarity", "Primary Land Cover", "Species Richness")
forest_data12$term <- custom_labels5
forest_data12 <- forest_data12[forest_data12$term != "(Intercept)", ]

forest_data12$term <- factor(forest_data12$term, 
                            levels = c("Managed", 
                                       "Urban", # Add all categorical terms first
                                       "Dissimilarity", "Primary Land Cover", "Species Richness"))

forest_data12$term_type <- ifelse(forest_data12$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"),
                                 "Continuous", "Categorical")

ggplot(forest_data12, aes(x = term, y = estimate, color = term_type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("") +
  ylab("Difference from Natural") +
  ggtitle("Mean Viral Sharing") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Categorical" = "blue", "Continuous" = "black")) +
  guides(color = FALSE)


################################################ COMBINE Plot ################################################################
################################################ COMBINE Plot ################################################################
################################################ COMBINE Plot ################################################################
################################################ COMBINE Plot ################################################################


forest_data9$outcome <- "Mean Viral Sharing"
forest_data10$outcome <- "Host Richness"
forest_data11$outcome <- "Host Abundance"
forest_data12$outcome <- "Community Mean Weighted Viral Sharing"


combined_forest_data5 <- rbind(forest_data9, forest_data10, forest_data11, forest_data12)
combined_forest_data5$shape_group <- ifelse(combined_forest_data5$term %in% c("Dissimilarity", "Primary Land Cover", "Species Richness"), 
                                            "Continuous", 
                                            "Categorical")

fplot5 <- ggplot(combined_forest_data5, aes(x = term, y = estimate, color = outcome, shape = shape_group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, fill = "black") + # Adding fill = "black" for filled shapes
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Natural Classification") +
  ggtitle("Combined Mammal Data") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Mean Viral Sharing" = "#4169E1",  
                                "Community Mean Weighted Viral Sharing" = "#DC143C", 
                                "Host Richness" = "#228B22",
                                "Host Abundance" = "#DAA520")) + 
  scale_shape_manual(values = c("Categorical" = 16, # Solid circle for categorical variables
                                "Continuous" = 24)) + # Filled triangle for continuous variables
  labs(color = "Metric", shape = "Variable Type")


plot(fplot5)



ggsave("alldisease_forest.png", plot = fplot5, width = 12, height = 10)






