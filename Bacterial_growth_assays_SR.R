# Analysis of the inhibition of bacteria by different concentrations of thermopsine.
# Sébastien Rivest, 26/04/2023

# Load packages
library(ggpubr)
library(glmmTMB)
library(readxl)
library(DHARMa)
library(plyr)

# Import dataset
Bacterial_growth_assays <- read_excel("C:/Users/seb69/OneDrive/Documents/Doc/lupinus_figs/Bacterial_growth_assays.xlsx")

# Histogram of values of Inhibition zones
hist(Bacterial_growth_assays$Inhibition)

# Model of the inhibition of bacteria by different concentrations of thermopsine
# Look at the residuals of a model assuming a normal distribution.
plot(lmer(Inhibition ~ Concentration +
            (1|Species/Plate),
          data = Bacterial_growth_assays)) 
# The response variable is continuous, bounded between 0 and infinity, not normally distributed, and the variance seems to increase with the mean. 
# A Gamma distribution seems appropriate. 
bac_growth_mod <- glmmTMB(Inhibition ~ Concentration +
                            (1|Species/Plate),
                          data = Bacterial_growth_assays, family = ziGamma(link = "log"), 
                          ziformula = ~ 1,na.action = "na.fail")
# Assess model fit
rmod_bac <- simulateResiduals(bac_growth_mod)
plot(rmod_bac)
# Look at model output
summary(bac_growth_mod)

# Plot
ggplot(Bacterial_growth_assays, aes(x = as.factor(Concentration), y = Inhibition, shape = Species, fill = Species)) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 3) + 
  #scale_color_manual(values = c("#cfe4ee95", "#3b85a895", "#25536995")) +
  scale_fill_manual(values = c("#acd5e880", "#3b85a880", "#25536980")) +
  scale_shape_manual(values = c(21,22,23)) + 
  theme_classic() +
  theme(legend.position = "top") +
  labs(x = "Alkaloid concentration (ug/ml)", y = "Diameter of inhibition zone (mm)") +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", colour = "grey")

