rm(list = ls())

#install.packages("openxlsx")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("wesanderson")
#install.packages("extrafont")

###################################
# načtení potřebných R balíčků
###################################

library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(wesanderson)
library(extrafont)
#font_import(prompt = TRUE)

###################################
# načtení dat a čištění
###################################

setwd("~/OneDrive - Vysoká škola ekonomická v Praze/Škola/VŠE/ING/Diplomová práce/Data/")


c_zeme <- c("Belgium", "Bulgaria", "Czechia", "Denmark", 
            "Germany", "Estonia",
            "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", 
            "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", 
            "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania",
            "Slovenia", "Slovakia", "Finland", "Sweden", "Liechtenstein",
            "Norway", "United Kingdom", "Serbia", "Turkey")

info_df <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 1)
#View(info_df)
info_df <- info_df[-c(1:6), -c(1)]
colnames(info_df) <- info_df[1,]
info_df <- info_df[-c(1),]

## Popis zkratek:

# CLD - chronic liver disease
# ISH - intentional self-harm
# AP - accidental poisioning
# EUI - event of undetermined intent

ocistit_data <- function(df){
  
  #filtr potřebného
  colnames(df) <- df[8,]
  colnames(df)[1] <- "Region"
  df <- df[-c(1:9, 483:486),-c(3, 10, 12)]
  df <- df[!duplicated(df[1]),]
  df[df == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  rownames(df) <- df[,1]
  
  # NAs handle
  for (j in 1:ncol(df)){
    for (i in 1:nrow(df)){
      if (df[i,j] == ":" | is.na(df[i,j]) == TRUE ){
        df[i,j] <- NA
      }
    }
  }
  
  # změň var typ
  df[2:ncol(df)] <- sapply(df[2:ncol(df)], as.numeric)
  
  return(df)
}

total_CLD <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 3)
total_ISH <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 4)
total_AP <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 5)
total_EUI <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 6)

total_CLD <- ocistit_data(total_CLD)
total_ISH <- ocistit_data(total_ISH)
total_AP <- ocistit_data(total_AP)
total_EUI <- ocistit_data(total_EUI)

total_CLD_zeme <- subset(total_CLD, Region %in% c_zeme)
total_ISH_zeme <- subset(total_ISH, Region %in% c_zeme)
total_AP_zeme <- subset(total_AP, Region %in% c_zeme)
total_EUI_zeme <- subset(total_EUI, Region %in% c_zeme)

View(total_CLD)
View(total_ISH)
View(total_AP)
View(total_EUI)

(total_allcause <- matrix(nrow = nrow(total_CLD), ncol = ncol(total_CLD))) 
(total_allcause <- as.data.frame(total_allcause))
colnames(total_allcause) <- colnames(total_CLD)
total_allcause[,1] <- total_CLD[,1]
  
for(j in 1:nrow(total_allcause)){
  for (i in 2:ncol(total_allcause)){
    total_allcause[j,i] <- sum(total_CLD[j,i], total_EUI[j,i], total_ISH[j,i], total_AP[j,i])
    total_allcause
  }
}

View(total_allcause)
(total_allcause_zeme <- subset(total_allcause, Region %in% c_zeme))
rownames(total_allcause_zeme) <- total_allcause_zeme[,1]
(plot_total_allcause_zeme <- melt(total_allcause_zeme))
plot_total_allcause_zeme$variable <- as.numeric(as.character(plot_total_allcause_zeme$variable))
sapply(plot_total_allcause_zeme, class)

###################################
# grafy
###################################

## popis zkratek:
# NWE - north-west europe
# CE - central europe
# SEE - south-eastern europe

barvy <- c(
  "Belgium" = "#AEB4A9",
  "Bulgaria" = "#E0C1B3",
  "Czechia" = "#D89A9E",
  "Denmark" = "#C37D92",
  "Germany" = "#846267",
  "Estonia" = "#96E072",
  "Ireland" = "#3DA35D",
  "Greece" = "#7F95D1",
  "Spain" = "#847979",
  "France" = "#322E18",
  "Croatia" = "#A63D40",
  "Italy" = "#E9B872",
  "Cyprus" = "#90A959",
  "Latvia" = "#6494AA",
  "Lithuania" = "#D6FFB7",
  "Luxembourg" = "#F5FF90",
  "Hungary" = "#FFC15E",
  "Malta" = "#080357",
  "Netherlands" = "#F5CB5C",
  "Austria" = "#61F2C2",
  "Poland" = "#30F2F2",
  "Portugal" = "#91F291",
  "Romania" = "#C0B298",
  "Slovenia" = "#A4778B",
  "Slovakia" = "#B6EEA6",
  "Finland" = "#AA4586",
  "Sweden" = "#FFCB47",
  "Liechtenstein" = "#3F6C51",
  "Norway" = "#F39A9D",
  "United Kingdom" = "#645986",
  "Serbia" = "#5C0029",
  "Turkey" = "#0267C1"
)

NWE_zeme <- c("Belgium", "Denmark", 
             "Germany", "Estonia",
             "Ireland", "France", "Latvia", "Lithuania", "Luxembourg", 
             "Netherlands",
             "Finland", "Sweden", "Liechtenstein",
             "Norway", "United Kingdom")

CE_zeme <- c("Czechia", "Hungary", "Austria", "Slovenia", "Slovakia")

SEE_zeme <- c("Bulgaria", "Greece", "Spain", "Croatia", "Italy", 
             "Cyprus", "Malta", "Portugal", "Romania",
             "Slovenia", "Serbia")

NWE_plot_total_allcause_zeme <- subset(plot_total_allcause_zeme, Region %in% NWE_zeme)
CE_plot_total_allcause_zeme <- subset(plot_total_allcause_zeme, Region %in% CE_zeme)
SEE_plot_total_allcause_zeme <- subset(plot_total_allcause_zeme, Region %in% SEE_zeme)

(NWE_barvy <- subset(barvy, names(barvy) %in% NWE_plot_total_allcause_zeme$Region))
(CE_barvy <- subset(barvy, names(barvy) %in% CE_plot_total_allcause_zeme$Region))
(SEE_barvy <- subset(barvy, names(barvy) %in% SEE_plot_total_allcause_zeme$Region))

sapply(NWE_plot_total_allcause_zeme, class)
NWE_plot_total_allcause_zeme[[1]] <- as.factor(NWE_plot_total_allcause_zeme[[1]])
NWE_plot_total_allcause_zeme <- cbind(NWE_plot_total_allcause_zeme, barvy = rep(NWE_barvy,8))

# NWE Evropa - celková úmrtnost na DoD
ggplot(NWE_plot_total_allcause_zeme, aes(x = variable, y = value, colour = Region)) +
  geom_line(aes(group = Region), size = 1) + 
  geom_point() +
  scale_colour_manual(values = NWE_barvy) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2018,1), limits = c(2011, 2018)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,100)) +
  labs(title = "Míra úmrtnosti na DoD",
       subtitle = "Severní a západní Evropa") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave("Míra úmrtnosti na DoD_Sev a Záp Evr.png",
       plot = last_plot(),
       dpi = 300)

# CE Evropa - celková úmrtnost na DoD
ggplot(CE_plot_total_allcause_zeme, aes(x = variable, y = value, colour = Region)) +
  geom_line(aes(group = Region), size = 1) + 
  geom_point() +
  scale_colour_manual(values = CE_barvy) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2018,1), limits = c(2011, 2018)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,100)) +
  labs(title = "Míra úmrtnosti na DoD",
       subtitle = "Střední Evropa") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave("Míra úmrtnosti na DoD_Střed Evr.png",
       plot = last_plot(),
       dpi = 300)

# SEE Evropa - celková úmrtnost na DoD
ggplot(SEE_plot_total_allcause_zeme, aes(x = variable, y = value, colour = Region)) +
  geom_line(aes(group = Region), size = 1) + 
  geom_point() +
  scale_colour_manual(values = SEE_barvy) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2018,1), limits = c(2011, 2018)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,100)) +
  labs(title = "Míra úmrtnosti na DoD",
       subtitle = "Jihovýchodní Evropa") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave("Míra úmrtnosti na DoD_Jihovýchod Evr.png",
       plot = last_plot(),
       dpi = 300)

## sledované věky 45-54 let

CLD_45 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 27)
ISH_45 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 28)
AP_45 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 29)
EUI_45 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 30)

CLD_54 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 31)
ISH_54 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 32)
AP_54 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 33)
EUI_54 <- read.xlsx("Evropa/hlth_cd_acdr2__custom_2171111_spreadsheet.xlsx", sheet = 34)

View(CLD_45)
View(ISH_45)
View(AP_45)
View(EUI_45)
View(CLD_54)
View(ISH_54)
View(AP_54)
View(EUI_54)

ocistit_data_45_CLD_ISH <- function(df){
  
  #filtr potřebného
  colnames(df) <- df[8,]
  colnames(df)[1] <- "Region"
  df <- df[-c(1:9, 483:486),-c(3, 8, 11, 13)]
  df <- df[!duplicated(df[1]),]
  df[df == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  rownames(df) <- df[,1]
  
  # NAs handle
  for (j in 1:ncol(df)){
    for (i in 1:nrow(df)){
      if (df[i,j] == ":" | is.na(df[i,j]) == TRUE ){
        df[i,j] <- NA
      }
    }
  }
  
  # změň var typ
  df[2:ncol(df)] <- sapply(df[2:ncol(df)], as.numeric)
  
  return(df)
}

ocistit_data_45_AP_EUI <- function(df){
  
  #filtr potřebného
  colnames(df) <- df[8,]
  colnames(df)[1] <- "Region"
  df <- df[-c(1:9, 483:486),-c(3, 10, 12)]
  df <- df[!duplicated(df[1]),]
  df[df == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  rownames(df) <- df[,1]
  
  # NAs handle
  for (j in 1:ncol(df)){
    for (i in 1:nrow(df)){
      if (df[i,j] == ":" | is.na(df[i,j]) == TRUE ){
        df[i,j] <- NA
      }
    }
  }
  
  # změň var typ
  df[2:ncol(df)] <- sapply(df[2:ncol(df)], as.numeric)
  
  return(df)
}

(CLD_45 <- ocistit_data_45_CLD_ISH(CLD_45))
(ISH_45 <- ocistit_data_45_CLD_ISH(ISH_45))
(AP_45 <- ocistit_data_45_AP_EUI(AP_45))
(EUI_45 <- ocistit_data_45_AP_EUI(EUI_45))

(CLD_45_zeme <- subset(CLD_45, Region %in% c_zeme))
(ISH_45_zeme <- subset(ISH_45, Region %in% c_zeme))
(AP_45_zeme <- subset(AP_45, Region %in% c_zeme))
(EUI_45_zeme <- subset(EUI_45, Region %in% c_zeme))


ocistit_data_54_CLD_ISH_EUI <- function(df){
  
  #filtr potřebného
  colnames(df) <- df[8,]
  colnames(df)[1] <- "Region"
  df <- df[-c(1:9, 483:486),-c(3, 9, 11, 13)]
  df <- df[!duplicated(df[1]),]
  df[df == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  rownames(df) <- df[,1]
  
  # NAs handle
  for (j in 1:ncol(df)){
    for (i in 1:nrow(df)){
      if (df[i,j] == ":" | is.na(df[i,j]) == TRUE ){
        df[i,j] <- NA
      }
    }
  }
  
  # změň var typ
  df[2:ncol(df)] <- sapply(df[2:ncol(df)], as.numeric)
  
  return(df)
}



(CLD_54 <- ocistit_data_54_CLD_ISH_EUI(CLD_54))
(ISH_54 <- ocistit_data_54_CLD_ISH_EUI(ISH_54))
(AP_54 <- ocistit_data_45_AP_EUI(AP_54))
(EUI_54 <- ocistit_data_54_CLD_ISH_EUI(EUI_54))

(CLD_54_zeme <- subset(CLD_54, Region %in% c_zeme))
(ISH_54_zeme <- subset(ISH_54, Region %in% c_zeme))
(AP_54_zeme <- subset(AP_54, Region %in% c_zeme))
(EUI_54_zeme <- subset(EUI_54, Region %in% c_zeme))

(total_allcause_45_54 <- matrix(nrow = nrow(CLD_54_zeme), ncol = ncol(CLD_54_zeme))) 
(total_allcause_45_54 <- as.data.frame(total_allcause_45_54))
colnames(total_allcause_45_54) <- colnames(CLD_54_zeme)
total_allcause_45_54[,1] <- CLD_54_zeme[,1]

for(j in 1:nrow(total_allcause_45_54)){
  for (i in 2:ncol(total_allcause_45_54)){
    total_allcause_45_54[j,i] <- sum(CLD_45_zeme[j,i], CLD_54_zeme[j,i], 
                                     ISH_45_zeme[j,i], ISH_54_zeme[j,i], 
                                     AP_45_zeme[j,i], AP_54_zeme[j,i], 
                                     EUI_45_zeme[j,i], EUI_54_zeme[j,i])
    total_allcause_45_54
  }
}

View(total_allcause_45_54)

(plot_total_allcause_zeme_45_54 <- melt(total_allcause_45_54))
plot_total_allcause_zeme_45_54$variable <- as.numeric(as.character(plot_total_allcause_zeme_45_54$variable))

NWE_plot_total_allcause_zeme_45_54 <- subset(plot_total_allcause_zeme_45_54, Region %in% NWE_zeme)
CE_plot_total_allcause_zeme_45_54 <- subset(plot_total_allcause_zeme_45_54, Region %in% CE_zeme)
SEE_plot_total_allcause_zeme_45_54 <- subset(plot_total_allcause_zeme_45_54, Region %in% SEE_zeme)

head(NWE_plot_total_allcause_zeme_45_54)

# NWE Evropa - úmrtnost na DoD - 45-54 let
ggplot(NWE_plot_total_allcause_zeme_45_54, aes(x = variable, y = value, colour = Region)) +
  geom_line(aes(group = Region), size = 1) + 
  geom_point() +
  scale_colour_manual(values = NWE_barvy) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2018,1), limits = c(2011, 2018)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,max(NWE_plot_total_allcause_zeme_45_54$value))) +
  labs(title = "Míra úmrtnosti na DoD - lidé ve věku 45-54 let",
       subtitle = "Severní a západní Evropa") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave("Míra úmrtnosti na DoD_Sev a Záp Evr_45-54let.png",
       plot = last_plot(),
       dpi = 300)


# CE Evropa - úmrtnost na DoD - 45-54 let
ggplot(CE_plot_total_allcause_zeme_45_54, aes(x = variable, y = value, colour = Region)) +
  geom_line(aes(group = Region), size = 1) + 
  geom_point() +
  scale_colour_manual(values = CE_barvy) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2018,1), limits = c(2011, 2018)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,max(CE_plot_total_allcause_zeme_45_54$value))) +
  labs(title = "Míra úmrtnosti na DoD - lidé ve věku 45-54 let",
       subtitle = "Střední Evropa") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave("Míra úmrtnosti na DoD_Střed Evr_45-54let.png",
       plot = last_plot(),
       dpi = 300)


# CE Evropa - úmrtnost na DoD - 45-54 let
ggplot(SEE_plot_total_allcause_zeme_45_54, aes(x = variable, y = value, colour = Region)) +
  geom_line(aes(group = Region), size = 1) + 
  geom_point() +
  scale_colour_manual(values = SEE_barvy) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2018,1), limits = c(2011, 2018)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,max(SEE_plot_total_allcause_zeme_45_54$value))) +
  labs(title = "Míra úmrtnosti na DoD - lidé ve věku 45-54 let",
       subtitle = "Jihovýchodní Evropa") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave("Míra úmrtnosti na DoD_Jihovýchod Evr_45-54let.png",
       plot = last_plot(),
       dpi = 300)

