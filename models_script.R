
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

dd$Land_use_classification = replace(dd$Land_use_classification, dd$Land_use_classification %in% c("Secondary_minimal", "Secondary_substantial"), "Secondary")
dd$Land_use_classification = replace(dd$Land_use_classification, dd$Land_use_classification %in% c("Primary_minimal"), "0_Primary_minimal")

# Species Richness Model---------------------------------------------------------------------------------------------------------------------------
######################################################################################################################################################
m1 = glmer(species_richness ~ Land_use_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + (1|SS) + (1|SSB), family="poisson", data=dd)
m2 = glmer(species_richness ~ Land_use_classification + scale(Dissim_1km) + scale(PrimaryLand_1km) + (1|SS) + (1|SSB), family="poisson", data=dd)
m3 = glmer(species_richness ~ Land_use_classification + scale(Dissim_500m) + scale(PrimaryLand_500m) + (1|SS) + (1|SSB), family="poisson", data=dd)

summary(m1)
summary(m2)
summary(m3)
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

fplot1 <- ggplot(combined_forest_data, aes(x = term, y = estimate, color = outcome)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Primary Minimal") +
  ggtitle("Predicts Land Use Classifications") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Richness" = "#1f77b4",  
                                "Abundance" = "#2ca02c")) + 
  labs(color = "Metric")
 plot(fplot1)
ggsave("predicts_class_species.png", width = 8, height = 6, dpi = 300)

################################## Richness and Abundance but this time with the modis data#############################################################
################################## Richness and Abundance but this time with the modis data#############################################################
################################## Richness and Abundance but this time with the modis data#############################################################

dd$sat_classification[dd$sat_classification == "agriculture"] <- "Managed"
dd$sat_classification[dd$sat_classification == "urban"] <- "Urban"
dd$sat_classification[dd$sat_classification == "Natural"] <- "Natural"

dd <- dd %>%
  mutate(sat_classification = case_when(
    Land_use_classification == "0_Primary_minimal" ~ "0_Primary_minimal",
    TRUE ~ sat_classification  # Keep the original value otherwise
  ))

m3 = glmer(species_richness ~ sat_classification + scale(Dissim_500m) + scale(PrimaryLand_500m) + (1|SS) + (1|SSB), family="poisson", data=dd)
summary(m3)
fixed_effects3 <- summary(m3)$coefficients
conf_int3 <- confint(m3, parm = "beta_", method = "Wald")

forest_data3 <- data.frame(
  term = rownames(fixed_effects3),
  estimate = fixed_effects3[, "Estimate"],
  lower = conf_int3[, 1],
  upper = conf_int3[, 2]
)
custom_labels1 <- c("(Intercept)", "Managed", "Natural", "Urban", "Dissimilarity", "Primary Land Cover")
forest_data3$term <- custom_labels1
forest_data3 <- forest_data3[forest_data3$term != "(Intercept)", ]
forest_data3 <- forest_data3[forest_data3$term != "Dissimilarity",]
forest_data3 <- forest_data3[forest_data3$term != "Primary Land Cover",]

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

forest_data4 <- data.frame(
  term = rownames(fixed_effects4),
  estimate = fixed_effects4[, "Estimate"],
  lower = conf_int4[, 1],
  upper = conf_int4[, 2]
)
custom_labels1 <- c("(Intercept)", "Managed", "Natural", "Urban", "Dissimilarity", "Primary Land Cover")
forest_data4$term <- custom_labels1
forest_data4 <- forest_data4[forest_data4$term != "(Intercept)", ]
forest_data4 <- forest_data4[forest_data4$term != "Dissimilarity",]
forest_data4 <- forest_data4[forest_data4$term != "Primary Land Cover",]

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

fplot2 <- ggplot(combined_forest_data2, aes(x = term, y = estimate, color = outcome)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Primary Minimal") +
  ggtitle("Satellite Land Use Classifications") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("Richness" = "#1f77b4",  
                                "Abundance" = "#2ca02c")) + 
  labs(color = "Metric")
plot(fplot2)

################################################ Put them together ################################################################
################################################ Put them together ################################################################
library(gridExtra)
predicts_land_forest <- grid.arrange(fplot1, fplot2, ncol = 2)

ggsave("predicts_land_forest.png", plot = predicts_land_forest, width = 12, height = 6)


# mean viral sharing model; resids look ok although arguably a beta distribution may be more appropriate
# beta dist: https://rpubs.com/nicoleknight/936037
# (probably a wormhole so I think the lmer is probably fine within project timescale unless you're keen to explore)

# because the viral sharing metrics are bounded between 0 and 1 I would probably scale it to aid in interpretation
# covariates scaled to ensure on a standardised scale

# i think perhaps just a scaled (rather than log) species richness is more appropraite with the Gaussian likelihood

m5 = lmer(scale(mean_viralsharing) ~ Land_use_classification + scale(Dissim_5km) + scale(PrimaryLand_5km) + scale(species_richness) + (1|SS) + (1|SSB), data=dd)

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

ggplot(combined_forest_data3, aes(x = term, y = estimate, color = outcome, shape = shape_group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, fill = "black") + # Adding fill = "black" for filled shapes
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  xlab("") +
  ylab("Distance from Primary Minimal") +
  ggtitle("Predicts Land Use Classifications and Continous Landscape Metrics") +
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

