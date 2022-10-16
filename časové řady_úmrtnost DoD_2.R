rm(list = ls())

#install.packages("openxlsx")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("wesanderson")
#install.packages("extrafont")
#install.packages("janitor")
#install.packages("RColorBrewer")
#install.packages("randomcoloR")
#install.packages("ggrepel")
#install.packages("reshapes2")


library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(wesanderson)
library(extrafont)
library(janitor)
library(RColorBrewer)
library(randomcoloR)
library(ggrepel)
library(reshape2)
#font_import(prompt = TRUE)

## Popis kódů:

# all_cause - All causes of death (A00-Y89) excluding S00-T98
# F10 - Mental and behavioural disorders due to use of alcohol
# F_11_16_18_19 - Drug dependence, toxicomania (F11-F16, F18-F19)
# K70_73_74 - Chronic liver disease
# X60_84_Y870 - Intentional self-harm
# X40_49 - Accidental poisoning by and exposure to noxious substances
# Y10_34_872 - Event of undetermined intent


setwd("~/OneDrive - Vysoká škola ekonomická v Praze/Škola/VŠE/ING/Diplomová práce/Data/Evropa/")


c_zeme <- c("Belgium", "Bulgaria", "Czechia", "Denmark", 
            "Germany", "Estonia",
            "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", 
            "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", 
            "Malta", "Netherlands", "Austria", "Poland", "Portugal", "Romania",
            "Slovenia", "Slovakia", "Finland", "Sweden", "Liechtenstein",
            "Norway", "United Kingdom", "Serbia", "Turkey")

all_cause <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 1)
F10 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 2)
F_11_16_18_19 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 3)
K70_73_74 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 4)
X60_84_Y870 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 5)
X40_49 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 6)
Y10_34_872 <- read.xlsx("Causes of death - standardised death rate by NUTS 2_2011-2019.xlsx", sheet = 7)


#View(all_cause)

dfs <- list(all_cause, F10, F_11_16_18_19, K70_73_74, X60_84_Y870, X40_49, Y10_34_872)

dfs <- lapply(dfs, function(x) {
  x <- row_to_names(x, 8, remove_row = T)
})

names(dfs) <- c("all_cause",
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
  colnames(df)[1] <- "Region NUTS2"
  df[df == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  dfs[[i]] <- df
}

# dfs <- lapply(dfs, function(x) {x[] <- lapply(x,type.convert, as.is = TRUE); x}) #jiný způsob, nefunguje spolehlivě když jsou místo NAs :

list2env(dfs, envir = .GlobalEnv) #unlist dfs
View(all_cause)


all_cause_zeme <- subset(all_cause, `Region NUTS2` %in% c_zeme)
F10_zeme <- subset(F10, `Region NUTS2` %in% c_zeme)
F_11_16_18_19_zeme <- subset(F_11_16_18_19, `Region NUTS2` %in% c_zeme)
K70_73_74_zeme <- subset(K70_73_74, `Region NUTS2` %in% c_zeme)
X60_84_Y870_zeme <- subset(X60_84_Y870, `Region NUTS2` %in% c_zeme)
X40_49_zeme <- subset(X40_49, `Region NUTS2` %in% c_zeme)
Y10_34_872_zeme <- subset(Y10_34_872, `Region NUTS2` %in% c_zeme)

View(all_cause_zeme)
View(F10_zeme)
View(F_11_16_18_19_zeme)
View(K70_73_74_zeme)
View(X60_84_Y870_zeme)
View(X40_49_zeme)
View(Y10_34_872_zeme)

# klavesova zkratka na zavreni vsech ostatnich oken - Ctrl + Shift + Option/alt + W

(total_DoD <- matrix(nrow = nrow(all_cause_zeme), ncol = ncol(all_cause_zeme))) 
(total_DoD <- as.data.frame(total_DoD))
colnames(total_DoD) <- colnames(all_cause_zeme)
total_DoD[,1] <- all_cause_zeme[,1]
  
for(j in 1:nrow(total_DoD)){
  for (i in 2:ncol(total_DoD)){
    total_DoD[j,i] <- sum(F10_zeme[j,i], 
                          F_11_16_18_19_zeme[j,i], 
                          K70_73_74_zeme[j,i], 
                          X60_84_Y870_zeme[j,i],
                          X40_49_zeme[j,i],
                          Y10_34_872_zeme[j,i])
    total_DoD
  }
}

View(total_DoD)

#t(total_DoD[,2:ncol(total_DoD)])
melt(total_DoD, value.name = "Region NUTS2")


#rownames(total_DoD) <- total_DoD[,1]

# přidat ke srovnání EU 

#recast(total_DoD, )

(plot_total_DoD <- melt(total_DoD))
plot_total_DoD$variable <- as.numeric(as.character(plot_total_DoD$variable))
#sapply(plot_total_DoD, class) #kontrola typu proměnných

# GRAFY --------------------------------------------

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

#qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
#(lepsi_barvy <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))
#lepsi_barvy <- sample(lepsi_barvy, size = 32)

#dalsi_barvy <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
#dalsi_barvy_32 <- sample(dalsi_barvy, size = 32)

set.seed(1)
palette_1 <- distinctColorPalette(33)

plot_total_DoD$`Region NUTS2` <- as.factor(plot_total_DoD$`Region NUTS2`)
#names(barvy) == levels(plot_total_DoD$`Region NUTS2`)
#(barvy <- subset(barvy,names(barvy) %in% levels(plot_total_DoD$`Region NUTS2`)))
#plot_total_DoD$dalsi_barvy_32 <- c(rep(dalsi_barvy_32,9))
#plot_total_DoD$lepsi_barvy <- c(rep(lepsi_barvy,9))

View(plot_total_DoD)

(top_3_2019 <- unlist(select(filter(total_DoD, rank(-`2019`) < 4), `2019`)))
  #total_DoD[which(rank(-total_DoD$`2019`) < 4) == TRUE, c("2019")]
  
  
  
ggplot(plot_total_DoD, aes(x = variable, y = value, colour = `Region NUTS2`)) +
  geom_line() + 
  geom_point() +
  geom_label_repel(aes(label = `Region NUTS2`),
                   data = subset(plot_total_DoD, value %in% top_3_2019),
                   show.legend = F,
                   ) +
  scale_colour_manual(values = palette_1) + 
  scale_x_continuous(name = "Rok", breaks = seq(2011,2019,1), limits = c(2011, 2019)) +
  scale_y_continuous(name = "Míra úmrtnosti", limits = c(0,100))+
  labs(title = "Míra úmrtnosti na DoD", color = "Země") +
  theme_minimal() +
  theme(legend.position="right", 
        text = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 1.5))

ggsave("Míra úmrtnosti na DoD_total_2011_2019.png",
       plot = last_plot(),
       dpi = 300)


EU_plot_total <- all_cause %>% 
  filter(`Region NUTS2` == "European Union - 27 countries (from 2020)")

(EU_plot_total <- melt(EU_plot_total))

View(EU_plot_total)


ggplot(EU_plot_total, aes(x = variable, y = value, group = 1)) +
  geom_path() + 
  geom_point() +
  scale_y_continuous(name = "Míra úmrtnosti v EU", limits = c(0,1100))+
  theme_minimal()
  
  

# ---------------------------------------------------------------------------


