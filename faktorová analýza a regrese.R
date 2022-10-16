rm(list = ls())
### FAKTOROVÁ ANALÝZA ###

# nahraj balicky
#install.packages("openxlsx")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("psych")
#install.packages("hornpa")
#install.packages("corrplot")
#install.packages("ggcorrplot")

library(openxlsx)
library(plyr)
library(dplyr)
library(tidyverse)
library(psych)
library(hornpa)
library(corrplot)
library(ggcorrplot)
library(car)
library(splines)
library(relaimpo)
library(leaps)


## vysvětlující proměnné:

# pop_dens_url <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tgs00024.tsv.gz"
# employment_rates_url <- "https://ec.europa.eu/eurostat/databrowser/view/edat_lfse_04/default/table?lang=en"
# 

# nastav pracovní adresář
getwd()
#setwd("~/Library/CloudStorage/OneDrive-VysokáškolaekonomickávPraze/Škola/VŠE/ING/Diplomová práce/Data/Evropa/kontextuální faktory")
setwd("~/Desktop/Diplomová práce/Data/Evropa/kontextuální faktory")

c_zeme_eng <- c("Belgium", "Bulgaria", "Czechia", "Denmark", 
            "Germany", "Estonia",
            "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", 
            "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", 
            "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania",
            "Slovenia", "Slovakia", "Finland", "Sweden", "Liechtenstein",
            "Norway", "United Kingdom", "Serbia", "Turkey")

c_zeme_lang <- c("Cesko", "Danmark", "Germany (until 1990 former territory of the FRG)", "Ostösterreich")

dalsi_nechtene <- c("European Union - 27 countries (from 2020)",
                    "European Union - 28 countries (2013-2020)",
                    "European Union - 15 countries (1995-2004)",
                    "Euro area - 19 countries  (from 2015)",
                    "European Union - 27 countries (2007-2013)")

## nahraj data

# hustota obyvatel
pop_dens <- read.xlsx("Population density by NUTS2 region_2019.xlsx", sheet = 3)
View(pop_dens)

colnames(pop_dens) <- pop_dens[5,]
colnames(pop_dens)[1] <- "Region"
colnames(pop_dens)[2] <- "Population density"

(pop_dens <- pop_dens[7:338, 1:2])
pop_dens[pop_dens == ":"] <- NA
pop_dens[,2] <- as.numeric(pop_dens[,2])
pop_dens <- pop_dens[!(pop_dens[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
pop_dens <- distinct(pop_dens)
pop_dens <- na.omit(pop_dens)

apply(pop_dens[sapply(pop_dens, is.numeric)], 2, function(x) mean(x, na.rm = TRUE))

# vzdělání - vysokoškolské

edu_tetr <- read.xlsx("Population by educational attainment_2019.xlsx", sheet = 4)
#View(edu_tetr)

colnames(edu_tetr) <- edu_tetr[9,]
colnames(edu_tetr)[1] <- "Region"
colnames(edu_tetr)[2] <- "Edu_tetr"


edu_tetr <- edu_tetr[-c(1:9, 522:523),]
edu_tetr[,2] <- as.numeric(edu_tetr[,2])
edu_tetr <- edu_tetr[!(edu_tetr[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
edu_tetr <- distinct(edu_tetr)
edu_tetr <- na.omit(edu_tetr)

# nezaměstnanost

unemploy <- read.xlsx("Unemployment rates_NUTS2_2019.xlsx")
View(unemploy)

colnames(unemploy)[1] <- "Region"
colnames(unemploy)[2] <- "Unemployment"


unemploy <- unemploy[-c(1:8, 515:516),]
unemploy[,2] <- as.numeric(unemploy[,2])
unemploy <- unemploy[!(unemploy[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
unemploy <- distinct(unemploy)
unemploy <- na.omit(unemploy)

# zaměstnanost
employ <- read.xlsx("Employment rates_NUTS2_2019.xlsx")
View(employ)

colnames(employ)[1] <- "Region"
colnames(employ)[2] <- "Employment"

employ <- employ[-c(1:7, 352:353),]
employ[,2] <- as.numeric(employ[,2])
employ <- employ[!(employ[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
employ <- distinct(employ)
employ <- na.omit(employ)


# materiální deprivace

depriv <- read.xlsx("Severe material deprivation_NUTS2_2019.xlsx")
#View(depriv)

colnames(depriv)[1] <- "Region"
colnames(depriv)[2] <- "Material_depriv"

depriv <- depriv[-c(1:5, 188:189),]
depriv[,2] <- as.numeric(depriv[,2])
depriv <- depriv[!(depriv[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
depriv <- distinct(depriv)
depriv <- na.omit(depriv)

# population by agegroups

pop_by_age <- read.xlsx("population by agegroup_2019.xlsx")
View(pop_by_age)

colnames(pop_by_age)[2:ncol(pop_by_age)] <- pop_by_age[6,2:ncol(pop_by_age)]
colnames(pop_by_age)[1] <- "Region"
pop_by_age <- pop_by_age[-c(1:7, 520:521),]
pop_by_age[,2:4] <- lapply(pop_by_age[,2:4], as.integer)
pop_by_age <- pop_by_age[!(pop_by_age[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
pop_by_age <- distinct(pop_by_age)
pop_by_age <- na.omit(pop_by_age)

(share_45_54 <- (pop_by_age$`From 45 to 49 years` + pop_by_age$`From 50 to 54 years`) / pop_by_age$Total)
pop_by_age <- cbind(pop_by_age, share_45_54)
summary(pop_by_age$share_45_54)

pop_by_age <- pop_by_age[,-c(2:4)]

# unmet medical needs

unmet_needs <- read.xlsx("Self-reported unmet needs for medical examination by main reason declared and NUTS 2 regions.xlsx", sheet = 5)
View(unmet_needs)

colnames(unmet_needs) <- c("Region", "Unmet_needs")
unmet_needs <- unmet_needs[-c(1:6, 184:185),]
unmet_needs[[2]] <- as.numeric(unmet_needs[[2]])
unmet_needs <- unmet_needs[!(unmet_needs[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
unmet_needs <- distinct(unmet_needs)
unmet_needs <- na.omit(unmet_needs)

# GDP

GDP <- read.xlsx("Gross domestic product (GDP) at current market prices by NUTS 2 regions.xlsx")

View(GDP)
GDP <- GDP[-c(1:5, 475:476),c(1,10)]
colnames(GDP) <- c("Region" , "GDP")
GDP[[2]] <- as.numeric(GDP[[2]])
GDP <- GDP[!(GDP[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]
GDP <- distinct(GDP)
GDP <- na.omit(GDP)



# spojení všech proměnných pro další analýzu

df_list <- list(pop_by_age, unemploy, employ, depriv, edu_tetr, pop_dens, unmet_needs, GDP)   
names(df_list)
#head(df_list)

all_vars <- df_list %>% 
  reduce(left_join, by = "Region")

head(all_vars)

all_vars <- all_vars[!duplicated(all_vars[1]),]
rownames(all_vars) <- all_vars[,1]
#all_vars <- all_vars[,-1]
all_vars[is.na(all_vars)] <- 0
all(complete.cases(all_vars))

# load death rates

#setwd("~/OneDrive - Vysoká škola ekonomická v Praze/Škola/VŠE/ING/Diplomová práce/Data/Evropa/")
setwd("~/Desktop/Diplomová práce/Data/Evropa")
library(janitor)

c_zeme <- c("Belgium", "Bulgaria", "Czechia", "Denmark", 
            "Germany", "Estonia",
            "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", 
            "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", 
            "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania",
            "Slovenia", "Slovakia", "Finland", "Sweden", "Liechtenstein",
            "Norway", "United Kingdom", "Serbia", "Turkey")

F10 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 2)
F_11_16_18_19 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 3)
K70_73_74 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 4)
X60_84_Y870 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 5)
X40_49 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 6)
Y10_34_872 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 7)


dfs <- list(F10, F_11_16_18_19, K70_73_74, X60_84_Y870, X40_49, Y10_34_872)

dfs <- lapply(dfs, function(x) {
  x <- row_to_names(x, 8, remove_row = T)
})

names(dfs) <- c(
                "F10",
                "F_11_16_18_19",
                "K70_73_74",
                "X60_84_Y870",
                "X40_49",
                "Y10_34_872")

for(i in 1:length(dfs)){
  df <- dfs[[i]]
  df[2:ncol(df)] <- sapply(df[2:ncol(df)], as.numeric)
  df <- df[1:479,]
  df <- df[!duplicated(df[1]),]
  colnames(df)[1] <- "Region"
  df[df == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  dfs[[i]] <- df
}

# dfs <- lapply(dfs, function(x) {x[] <- lapply(x,type.convert, as.is = TRUE); x}) #jiný způsob, nefunguje spolehlivě když jsou místo NAs :

list2env(dfs, envir = .GlobalEnv) #unlist dfs
View(F10)

F10 <- F10[,c(1,10)]
F_11_16_18_19 <- F_11_16_18_19[,c(1,10)]
K70_73_74 <- K70_73_74[,c(1,10)]
X60_84_Y870 <- X60_84_Y870[,c(1,10)]
X40_49 <- X40_49[,c(1,10)]
Y10_34_872 <- Y10_34_872[,c(1,10)]


(total_DoD <- matrix(nrow = nrow(F10), ncol = ncol(F10))) 
(total_DoD <- as.data.frame(total_DoD))
colnames(total_DoD) <- colnames(F10)
total_DoD[,1] <- F10[,1]

for(j in 1:nrow(total_DoD)){
  for (i in 2:ncol(total_DoD)){
    total_DoD[j,i] <- sum(F10[j,i], 
                          F_11_16_18_19[j,i], 
                          K70_73_74[j,i], 
                          X60_84_Y870[j,i],
                          X40_49[j,i],
                          Y10_34_872[j,i])
    total_DoD
  }
}

View(total_DoD)

total_DoD <- total_DoD[!(total_DoD[,1] %in% c(c_zeme_eng, dalsi_nechtene, c_zeme_lang)),]

sum(is.na(total_DoD[[2]])) # almost half of the data is missing!

all_vars_with_DoD <- left_join(all_vars, total_DoD)
rownames(all_vars_with_DoD) <- all_vars_with_DoD[,1]
all_vars_with_DoD <- all_vars_with_DoD[,-1]

all_vars_with_DoD[all_vars_with_DoD == 0.0] <- NA
data.frame(lapply(all_vars_with_DoD, function(x) sum(is.na(x))))

#all_vars_with_DoD <- na.omit(all_vars_with_DoD)
View(all_vars_with_DoD)

hist(all_vars_with_DoD$`2019`)



# PCA ---------------------------------------------------------------------
# https://francish.netlify.app/post/principal-components-analysis-using-r/

# first: check if PCA can be done (if there are some correlations - data redundancy)

p <- ncol(all_vars)

data_for_corr <- all_vars
data_for_corr[data_for_corr == 0.0] <- NA
data_for_corr <- na.omit(data_for_corr[,-1])
(corr_mat <- cor(data_for_corr))

options(scipen = 100)

cortest.bartlett(corr_mat) # null.hypo: correlation matrix is identity matrix (without correlations)

KMO(corr_mat) # another criterium for desicion about PCA - KMO statistic (if >=0.5 then appropriate to run FA)

ggcorrplot(corr_mat, method = "square", outline.col = "white", 
           colors = c("#Da0a06", "white", "#138E1D"),
           lab = TRUE,
           insig = "blank",
           ggtheme = ggplot2::theme_minimal,
           legend.title = "Korelace",
           lab_col = "black")

ggsave("Corr_plot.png",
       plot = last_plot(),
       dpi = 300)


(pca <- principal(data_for_corr, nfactors = 3, rotate = "none", scores = T))


# Decision making part - how many components to retain --------------------

## one approach - Kaiser`s rule - eigenvalues > 1 to keep (those < 1 account for less variance than one of the original variables)

(plot_evalues <- as.data.frame(pca$values))

ggplot(plot_evalues, aes(x = rownames(plot_evalues), y = `pca$values`, group = 1))+
  geom_point() +
  geom_line() +
  geom_abline(slope = 0, intercept = 1, linetype = "dotted") +
  geom_text(aes(label = round(`pca$values`, 2)),
            vjust = -1.5,
            hjust = -0.5) +
  labs(x = "Component number", y = "Eigenvalues", title = "Screeplot")+
  scale_y_continuous(breaks = seq(0,4,0.25), limits = c(0,4)) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(vjust=-0.5, face = "bold"),
        axis.title.y = element_text(vjust=5, face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 10))

ggsave("Screeplot_PCA.png",
       plot = last_plot(),
       dpi = 300,
       scale = 1)


## second qpproach - parallel analysis (hornpa package)
# description: provided on the webpage (link above)

hornpa(k = 7, size = 423, reps = 100, seed = 1234)
pca$values #Since our first three eigenvalues are greater than 95 % percentile, we have support to retain the first three components


# REGRESE -----------------------------------------------------------------

plot(all_vars_with_DoD$`2019`)

par(mfrow = c(2,2))
all_vars_with_DoD <- na.omit(all_vars_with_DoD)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$Edu_tetr)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$Unemployment)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$Employment)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$share_45_54)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$Unmet_needs)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$`Population density`)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$Material_depriv)
plot(all_vars_with_DoD$`2019` ~ all_vars_with_DoD$GDP)


names(all_vars_with_DoD)

options(scipen = 100)

fit <- lm(`2019` ~ ., data = all_vars_with_DoD)
summary(fit)

data.frame(lapply(all_vars_with_DoD, function(x) sum(is.na(x))))
length(fitted(fit))


fit2 <- lm(`2019` ~ ., data = all_vars_with_DoD[,!(colnames(all_vars_with_DoD) %in% c("Unmet_needs", "Material_depriv"))])
summary(fit2)

data.frame(lapply(all_vars_with_DoD[,!(colnames(all_vars_with_DoD) %in% c("Unmet_needs", "Material_depriv"))], function(x) sum(is.na(x))))
length(fitted(fit2))


fit3 <- lm(`2019` ~ ., data = all_vars_with_DoD[,!(colnames(all_vars_with_DoD) %in% c("Unmet_needs", "Material_depriv", "Population density"))])
summary(fit3)

data.frame(lapply(all_vars_with_DoD[,!(colnames(all_vars_with_DoD) %in% c("Unmet_needs", "Material_depriv", "Population density"))], function(x) sum(is.na(x))))
length(fitted(fit3))


fit4 <- lm(`2019` ~ share_45_54 + Unemployment + Edu_tetr, data = all_vars_with_DoD)
summary(fit4)

data.frame(lapply(all_vars_with_DoD[,!(colnames(all_vars_with_DoD) %in% c("Unmet_needs", "Material_depriv", "Population density", "Employment", "GDP"))], function(x) sum(is.na(x))))
length(fitted(fit4))

fit4 <- lm(`2019` ~ Edu_tetr, data = all_vars_with_DoD)
summary(fit4)

length(fitted(fit4))



(relImp <-  calc.relimp(fit4, type = "lmg", rela = F)) # relativní důležitost regressorů

par(mfrow = c(1,1))
plot(relImp)


# STEPWISE approach

all_vars_with_DoD <- na.omit(all_vars_with_DoD)

step <- stepAIC(fit, direction = "backward")

fit_after_stepAIC <- lm(`2019` ~ Unemployment + Edu_tetr, data = all_vars_with_DoD) 
summary(fit_after_stepAIC)  


# srovnání 3 modelů:

R2_adj <- c(summary(fit)$adj.r.squared, 
           summary(fit4)$adj.r.squared, 
           summary(fit_after_stepAIC)$adj.r.squared)

s2e <- c(summary(fit)$sigma^2, 
         summary(fit4)$sigma^2, 
         summary(fit_after_stepAIC)$sigma^2)

PRESS <- c(sum((resid(fit)/(1 - hatvalues(fit)))^2), 
           sum((resid(fit4)/(1 - hatvalues(fit4)))^2), 
           sum((resid(fit_after_stepAIC)/(1 - hatvalues(fit_after_stepAIC)))^2))

AIC <- c(AIC(fit),
         AIC(fit4),
         AIC(fit_after_stepAIC))

comparison <- rbind(R2_adj, s2e, AIC, PRESS)

colnames(comparison) <- c("Full", "only Edu, Share_45_54, Unemploy", "only Edu, Unemploy")
comparison




