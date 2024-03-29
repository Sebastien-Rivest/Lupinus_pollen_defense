# Analysis of Lupinus argenteus pollen and petal alkaloids and flower interactions data.
# The dataset contains values of total alkaloid concentrations in petals and pollen,
# the abundance of thrips, pollen beeltes and bacteria, and three measures of pollinator visitation:
# number of pollinator visits, average number of flowers visited per visit, and total number of flower visits.
# Lupinus argenteus individuals were sampled at and near the RMBL in 2021 and 2022 in three sites: 401, GT, and EL. 
# S?bastien Rivest, 16/02/2023

# Load packages
Packages <- c("readxl", "tidyverse", "lme4", "lmerTest", "MASS", "tidyr", "ggplot2", "blmeco",
              "ggthemes", "lattice", "magrittr", "ggpubr", "extrafont", "gridExtra", "ggExtra",
              "glmmTMB", "corpcor", "DHARMa", "ggeffects", "GGally", "scales", "ggbreak")
lapply(Packages, library, character.only = TRUE)

# Import dataset
lupinus_data <- read_excel("C:/Users/seb69/OneDrive/Documents/Doc/lupinus_figs/Analyses/lupinus_data.xlsx")
# Remove missing values ("NA") and specify numeric variables.
dataset <- lupinus_data %>% 
  mutate(Year = as.factor(Year),
         Pollen_alkaloids = as.numeric(Pollen_alkaloids),
         Petals_alkaloids = as.numeric(Petals_alkaloids),
         Conspecific_abundance = as.numeric(Conspecific_abundance)) %>%
  subset(.,is.na(Pollen_alkaloids)==FALSE)

# Look at variable distributions 
par(mfrow=c(2,5))
par(mar=c(5,5,1,1))
hist(dataset$Pollen_alkaloids,breaks=15,
     xlab = "Pollen alkaloid concentration", main = NULL)
hist(dataset$Petals_alkaloids,breaks=15,
     xlab = "Petal alkaloid concentration", main = NULL, ylab = NULL)
hist(dataset$Pollinator_visits,breaks=15,
     xlab = "Number of pollinator visits", main = NULL, ylab = NULL)
hist(dataset$Flowers_visited_per_visit,breaks=15,
     xlab = "Average number of\nflowers visited per pollinator visit", main = NULL, ylab = NULL)
hist(dataset$Tot_flowers_visits,breaks=15,
     xlab = "Total number of flowers visited", main = NULL, ylab = NULL)
hist(dataset$Thrips_per_flower,breaks=15,
     xlab = "Number of thrips per flower", main = NULL)
hist(dataset$Pollen_beetles_per_flower,breaks=15,
     xlab = "Number of pollen beetles per flower", main = NULL, ylab = NULL)
hist(dataset$CFU_per_mg,breaks=15,
     xlab = "Bacteria colony forming units\nper mg of pollen", main = NULL, ylab = NULL)
hist(dataset$n_flowers,breaks=15,
     xlab = "Number of open flowers", main = NULL, ylab = NULL)
hist(dataset$Conspecific_abundance,breaks=15,
     xlab = "Number of conspecific flowers\nin a 1m2 radius", main = NULL, ylab = NULL)

dev.off()
# Look at colinearity
colinearity <- dataset[,seq(5,14)]
ggpairs(colinearity) # Only high colinearity between different measures of pollinator visitation.

# Model of the relationship between pollen and petal alkaloids
hist(dataset$Pollen_alkaloids, breaks=15,
     xlab = "Pollen alkaloid concentration", main = NULL) 
# Look at the residuals of a model assuming a normal distribution
plot(lmer(Pollen_alkaloids ~ scale(Petals_alkaloids) +
            (1|Site/Plot), data = dataset)) # Variance increases with the mean.
# The response variable is continuous, bounded between 0 and infinity, and the variance increases with the mean. 
# A zero-inflated gamma distribution seems appropriate. 
mod_polalk_full <- glmmTMB(Pollen_alkaloids ~ scale(Petals_alkaloids) + Year + 
                             (1|Site/Plot), data = dataset, ziGamma(link = "log"),
                           ziformula = ~ 1, na.action = "na.fail") 
# Assess model fit
rmod_polalk<- simulateResiduals(mod_polalk_full)
plot(rmod_polalk)
# Look at model output
summary(mod_polalk_full)

# plot
predictions <- ggpredict(mod_polalk_full, c("Petals_alkaloids"),interactive=TRUE)
o<-ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="#bd8200") +
  geom_point(data=dataset,aes(x= Petals_alkaloids , y = Pollen_alkaloids, color = Site))+
  geom_point(data=dataset,aes(x= Petals_alkaloids , y = Pollen_alkaloids),
             shape = 1, fill = "#bd8200", size = 2, alpha = 0.7) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill = "#bd8200") +
  scale_color_manual(values = c("#ca2e3090", "#f69f5690", "darkgrey")) +
  labs(x="Petal alkaloids concentration", y="Pollen alkaloids concentration") +
  scale_y_sqrt(breaks=c(0, 200, 1000,2500, 5000, 7500, 10000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.justification=c(1,1), legend.position=c(0.99,0.99))
ggMarginal(o, type = "boxplot", groupFill = TRUE)

# Remove missing values ("NA") of conspecific flower density (this covariate is used in the models of Lupinus interactions).
dataset_int <- subset(dataset, is.na(Conspecific_abundance)==FALSE) 
# Model of thrips
hist(dataset_int$Thrips_per_flower, breaks=15,
     xlab = "Number of thrips per flower", main = NULL)
# Use zero-inflated gamma distribution
plot(lmer(Thrips_per_flower ~ sqrt(n_flowers) + scale(Conspecific_abundance) +
            sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + 
            (1|Site/Plot),data = dataset_int)) # Assumptions not met. 
# The response variable is continuous, bounded between 0 and infinity, and the variance increases with the mean. 
# A zero-inflated gamma distribution seems appropriate. 
mod_thrips_full <- glmmTMB(Thrips_per_flower ~ sqrt(n_flowers) + scale(Conspecific_abundance) +
                             sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year + 
                             (1|Site/Plot), data = dataset_int, family = ziGamma(link = "log"), 
                           ziformula = ~ 1,na.action = "na.fail")
rmod_thrips <- simulateResiduals(mod_thrips_full)
plot(rmod_thrips) 
summary(mod_thrips_full) 

# Extract model predictions
predictions <- ggpredict(mod_thrips_full, c("Petals_alkaloids"), interactive=TRUE) 
# Plot for petal alkaloids
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="#c0392b") +
  geom_point(data=dataset_int,aes(x= Petals_alkaloids, y = Thrips_per_flower),
             color="black", shape = 21, fill = "#c0392b", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill = "#c0392b") +
  labs(x="Petal alkaloids concentration", y="Thrips per flower") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Plot for pollen alkaloids
predictions <- ggpredict(mod_thrips_full, "Pollen_alkaloids", interactive=TRUE)
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="grey45", linetype = 2) +
  geom_point(data=dataset_int,aes(x= Pollen_alkaloids, y = Thrips_per_flower),
             color="black", shape = 21, fill = "#c0392b", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill = "grey45") +
  scale_x_sqrt(breaks=c(0, 200, 1000,2500, 5000, 7500, 10000)) +
  labs(x="Pollen alkaloids concentration", y="Thrips per flower") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Model of pollen beetles
# The same explanatory variable as for thrips are incorporated in the model.
hist(dataset_int$Pollen_beetles_per_flower, breaks=15,
     xlab = "Number of pollen beetles per flower", main = NULL, ylab = NULL) 
# Look at the residuals of a model assuming a normal distribution
plot(mod_beetles_full<-lmer(Pollen_beetles_per_flower ~ sqrt(n_flowers) + scale(Conspecific_abundance) +
                              sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + 
                              (1|Site/Plot),data = dataset_int)) # Assumptions not met. 
# Use zero-inflated gamma distribution
mod_beetles_full<-glmmTMB(Pollen_beetles_per_flower ~ sqrt(n_flowers) + scale(Conspecific_abundance) +
                            sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year +
                            (1|Site/Plot),data = dataset_int, family = ziGamma(link = "log"),
                          ziformula = ~ 1,na.action="na.fail")
rmod_beetles <- simulateResiduals(mod_beetles_full)
plot(rmod_beetles)
summary(mod_beetles_full)

# Plot for petal alkaloids
predictions <- ggpredict(mod_beetles_full, c("Petals_alkaloids"), interactive=TRUE)
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="grey45", linetype = 2) +
  geom_point(data=dataset_int,aes(x= Petals_alkaloids, y = Pollen_beetles_per_flower),
             color="black", shape = 21, fill = "#629363", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill = "grey45") +
  labs(x="Petals alkaloids concentration", y="Pollen beetles per flower") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
# Plot for pollen alkaloids
predictions <- ggpredict(mod_beetles_full, c("Pollen_alkaloids"), interactive=TRUE)
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="grey45", linetype = 2) +
  geom_point(data=dataset_int,aes(x= Pollen_alkaloids, y = Pollen_beetles_per_flower),
             color="black", shape = 21, fill = "#629363", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill = "grey45") +
  scale_x_sqrt(breaks=c(0, 200, 1000,2500, 5000, 7500, 10000)) +
  labs(x="Pollen alkaloids concentration", y="Pollen beetles per flower") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Model of bacteria
hist(dataset_int$CFU_per_mg, breaks=15,
     xlab = "Number of bacteria\ncolony forming units per flower", main = NULL, ylab = NULL) 
# Look at the residuals of a model assuming a normal distribution
plot(mod_beetles_full<-lmer(CFU_per_mg ~ sqrt(n_flowers) + scale(Conspecific_abundance) +
                              sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year + 
                              (1|Site/Plot),data = dataset_int)) # Assumptions not met. 
# Use zero-inflated gamma distribution
mod_bac_full<-glmmTMB(CFU_per_mg ~ sqrt(n_flowers) + scale(Conspecific_abundance) +
                        sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year + 
                        (1|Site/Plot),data = dataset_int, family = ziGamma(link = "log"),
                      ziformula = ~ 1, na.action="na.fail")
rmod_bac<- simulateResiduals(mod_bac_full)
plot(rmod_bac)
summary(mod_bac_full)

# Transform values of CFU_per_mg to put on a log scale.
dataset_fg <- dataset_int%>% 
  mutate(CFU_per_mg = CFU_per_mg+1)
# Plot for pollen alkaloids
predictions <- ggpredict(mod_bac_full, terms = c("Pollen_alkaloids"), interactive=TRUE)
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="#3b85a8") +
  geom_point(data=dataset_fg,aes(x= Pollen_alkaloids, y = CFU_per_mg),
             color="black", shape = 21, fill = "#3b85a8", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill = "#3b85a8") +
  labs(x="Pollen alkaloids concentration", y="Bacteria colony forming units per mg") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_x_sqrt(breaks=c(0, 200, 1000,2500, 5000, 7500, 10000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# Model of number of pollinator visits
# "Petals alkaloids" is kept in the model as covariate, although it is not hypothesized to affect pollinators.
hist(dataset_int$Pollinator_visits,breaks=15, xlab = "Number of pollinator visits", main = NULL, ylab = NULL)
# A Poisson distribution is used because the number of pollinator visits represents count data.
dispersion_glmer(glmer(Pollinator_visits ~ scale(sqrt(n_flowers)) + scale(Conspecific_abundance) +
                         sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year +
                         (1|Site/Plot), data = dataset_int, family = poisson()))
# Weak overdispersion, use negative binomial distribution.
# Values of number of flowers scaled to avoid problem of convergence.
mod_poll_full<-glmer.nb(Pollinator_visits ~ scale(sqrt(n_flowers)) + scale(Conspecific_abundance) + 
                          sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year +
                          (1|Site/Plot), data = dataset_int, na.action="na.fail") 
rmod_visits<- simulateResiduals(mod_poll_full)
plot(rmod_visits)
plot(mod_poll_full)
summary(mod_poll_full)

# Plot of pollen alkaloids
predictions <- ggpredict(mod_poll_full, terms=c("Pollen_alkaloids"), interactive=TRUE)
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="#bb601b") +
  geom_point(data=dataset_int,aes(x= Pollen_alkaloids, y = Pollinator_visits),
             color="black", shape = 21, fill = "#bb601b", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill = "#bb601b") +
  labs(x="Pollen alkaloids concentration", y="Number of pollinator visits") +
  scale_x_sqrt(breaks=c(0, 200, 1000,2500, 5000, 7500, 10000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Model of flowers visited per visit
# Individuals that received no pollinator visits are removed because the number of flowers visited per visit cannot be measured.
# Create a dataset without values of zero for pollinator visits.
dataset_fv <- dataset_int[dataset_int$Pollinator_visits > 0 & dataset_int$Pollen_alkaloids < 5000,]
hist(dataset_fv$Flowers_visited_per_visit, breaks=15,
     xlab = "Average number of\nflowers visited per pollinator visit", main = NULL, ylab = NULL) 
# Look at the residuals of a model assuming a normal distribution
plot(lmer(Flowers_visited_per_visit ~ log(n_flowers) + scale(Conspecific_abundance) + 
            scale(Pollen_alkaloids) + scale(Petals_alkaloids) + 
            (1|Site/Plot), data = dataset_fv)) # Assumptions not met. 
# A gamma distribution seems appropriate. 
mod_visits_full<-glmmTMB(Flowers_visited_per_visit ~ sqrt(n_flowers)  +scale(Conspecific_abundance) +
                           sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year +
                           (1|Site/Plot), data = dataset_fv, family = Gamma(link = "log"), 
                         ziformula = ~ 1,na.action = "na.fail")
rmod_visits<- simulateResiduals(mod_visits_full)
plot(rmod_visits)
summary(mod_visits_full)
dataset_fv$Year %>% length
# Plot for pollen alkoids
predictions <- ggpredict(mod_visits_full, c("Pollen_alkaloids"), interactive=TRUE)
ggplot(predictions, aes(x, predicted)) + 
  geom_line(color="#bb601b", linetype=1) +
  geom_point(data=dataset_fv,aes(x= Pollen_alkaloids, y = Flowers_visited_per_visit),
             color="black", shape = 21, fill = "#bb601b", size = 2, alpha = 0.7)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill = "#bb601b") +
  labs(x="Pollen alkaloids concentration", y="Flowers visited per visits") +
  theme_bw() +
  scale_x_sqrt(breaks=c(0, 200, 1000,2500, 5000, 7500, 10000)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# Model of Total number of flowers visited
hist(dataset_int$Tot_flowers_visits, breaks=15,
     xlab = "Total number of flowers visited", main = NULL, ylab = NULL) 
# Look at the residuals of a model assuming a normal distribution
plot(lmer(Tot_flowers_visits ~ log(n_flowers) + scale(Conspecific_abundance) + 
            scale(Pollen_alkaloids) + scale(Petals_alkaloids) + 
            (1|Site/Plot), data = dataset_int)) # Assumptions not met. 
# Use zero-inflated gamma distribution
mod_visits_full<-glmmTMB(Tot_flowers_visits ~ sqrt(n_flowers)  +scale(Conspecific_abundance) +
                           sqrt(Pollen_alkaloids) + scale(Petals_alkaloids) + Year +
                           (1|Site/Plot), data = dataset_int, family = ziGamma(link = "log"), 
                         ziformula = ~ 1,na.action = "na.fail")
rmod_visits<- simulateResiduals(mod_visits_full)
plot(rmod_visits)
summary(mod_visits_full)

# Path analysis using piecewise SEM
# The newest version of the package do not support glmmTMB so an older version of the package is used.
packageurl <- "https://cran.r-project.org/src/contrib/Archive/piecewiseSEM/piecewiseSEM_1.2.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
library(piecewiseSEM)

# Convert data
dataset_SEM<-dataset_int 
# The variables are transformed before modeling (scale or square root).
dataset_SEM$n_flowers <- sqrt(dataset_int$n_flowers)
dataset_SEM$Pollen_alkaloids <- sqrt(dataset_int$Pollen_alkaloids) 
dataset_SEM$Petals_alkaloids <- dataset_int$Petals_alkaloids/sd(dataset_int$Petals_alkaloids)
dataset_SEM$Conspecific_abundance <- dataset_int$Conspecific_abundance/sd(dataset_int$Conspecific_abundance)
dataset_SEM$CFU_per_mg <- dataset_int$CFU_per_mg/sd(dataset_int$CFU_per_mg)

# List of the full models
# Note that the model for the relationship between pollen and petal alkaloids was not incorporated.
# Since both variables are included in the other models, adding this model to the SEM would be redundant.
# Its P value was however considered when correcting for multiple testing (see below).
mod1 <- glmmTMB(Thrips_per_flower ~ n_flowers + Conspecific_abundance + 
                  Pollen_alkaloids + Petals_alkaloids + Year +
                  (1|Site/Plot),data = dataset_SEM, family = ziGamma(link = "log"), 
                ziformula = ~ 1,na.action = "na.fail")

mod2 <- glmmTMB(Pollen_beetles_per_flower ~ n_flowers + Conspecific_abundance + 
                  Pollen_alkaloids + Petals_alkaloids + Year + 
                  (1|Site/Plot),data = dataset_SEM, family = ziGamma(link = "log"), 
                ziformula = ~ 1,na.action = "na.fail")

mod3 <- glmmTMB(CFU_per_mg ~ n_flowers + Conspecific_abundance + 
                  Pollen_alkaloids + Petals_alkaloids + Year + 
                  (1|Site/Plot),data = dataset_SEM, family = ziGamma(link = "log"),
                ziformula = ~ 1, na.action="na.fail")

mod4 <- glmmTMB(Flowers_visited_per_visit ~ n_flowers + Conspecific_abundance + 
                  Pollen_alkaloids + Petals_alkaloids + Year +
                  (1|Site/Plot), data = dataset_SEM, family = ziGamma(link = "log"), 
                ziformula = ~ 1,na.action = "na.fail")

modlist = list(mod1, mod2, mod3, mod4)  
# Look for model fit and missing paths
sem.fit(modlist, data = dataset_SEM)

# Update model by adding missing paths to the SEM
mod2 <- update(mod2, ~. + Thrips_per_flower)
modlist2 = list(mod1, mod2, mod3, mod4)  
sem.fit(modlist2, data = dataset_SEM)
# Another missing path idenfied
mod2 <- update(mod2, ~. + CFU_per_mg)
modlist3 = list(mod1, mod2, mod3, mod4) 
sem.fit(modlist3, data = dataset_SEM) # No missing path
# Look at coefficients
sem.coefs(modlist3, data = dataset_SEM)

# Adjust P values for multiple testing
# the P values are those from the SEM and the model of pollen alkaloids, excluding covariates.
c(sem.coefs(modlist3, data = dataset_SEM)$p.value[c(1,7,12,13,15,17,18,22)],
  summary(mod_polalk_full)$coefficients$cond[4,4]) %>%
  p.adjust(., method = "BH") 

# Extract standardized coefficients
# Model for thrips per flower
R2 <- cor(dataset_SEM$Thrips_per_flower, predict(mod1, type = "response"))^2 
sd.yhat <- sqrt(var(predict(mod1, type = "link"))/R2)
sem.coefs(mod1)[1,3] * sd(dataset_SEM$Petals_alkaloids)/sd.yhat # Petals alkaloids
sem.coefs(mod1)[5,3] * sd(dataset_SEM$Pollen_alkaloids)/sd.yhat # Pollen alkaloids
# Model for Pollen beetles per flower
R2 <- cor(dataset_SEM$Pollen_beetles_per_flower, predict(mod2, type = "response"))^2 
sd.yhat <- sqrt(var(predict(mod2, type = "link"))/R2)
sem.coefs(mod2)[2,3] * sd(dataset_SEM$Thrips_per_flower)/sd.yhat # Thrips per flower
sem.coefs(mod2)[3,3] * sd(dataset_SEM$CFU_per_mg)/sd.yhat # CFU per mg
sem.coefs(mod2)[5,3] * sd(dataset_SEM$Petals_alkaloids)/sd.yhat # Petals alkaloids
sem.coefs(mod2)[7,3] * sd(dataset_SEM$Pollen_alkaloids)/sd.yhat # Pollen alkaloids
# Model for CFU_per_mg
R2 <- cor(dataset_SEM$CFU_per_mg, predict(mod3, type = "response"))^2 
sd.yhat <- sqrt(var(predict(mod3, type = "link"))/R2)
sem.coefs(mod3)[1,3] * sd(dataset_SEM$Pollen_alkaloids)/sd.yhat # Pollen alkaloids
# Model for Flowers visited per visit
R2 <- cor(dataset_SEM$Flowers_visited_per_visit, predict(mod4, type = "response"))^2 
sd.yhat <- sqrt(var(predict(mod4, type = "link"))/R2)
sem.coefs(mod4)[2,3] * sd(dataset_SEM$Pollen_alkaloids)/sd.yhat # Pollen alkaloids
# Model for Pollen alkaloids
R2 <- cor(dataset_int$Pollen_alkaloids, predict(mod_polalk_full, type = "response"))^2 
sd.yhat <- sqrt(var(predict(mod_polalk_full, type = "link"))/R2)
sem.coefs(mod_polalk_full)[2,3] * sd(scale(dataset_int$Petals_alkaloids))/sd.yhat # Petals alkaloids
