rm(list = ls())


# načtení balíčků
library(dplyr)
library(sp)
library(sf)
library(ggplot2, quietly=T)
library(rgdal, quietly=T)
library(rgeos, quietly=T)
library(dplyr, quietly=T)
library(classInt, quietly=T)
library(eurostat, quietly=T)
#library(viridis)
library(RColorBrewer)

#download Eurostat NUTS 2016 shapefiles
url <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2016-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-nuts-2016-01m.shp.zip") # unzip the boundary data
unzip("NUTS_RG_01M_2016_4326_LEVL_2.shp.zip") # unzip the NUTS-2 folder

#download the country shp
url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2013-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-countries-2013-01m.shp.zip") # unzip the boundary data
unzip("CNTR_RG_01M_2013_4326.shp.zip")

nuts2 <- readOGR(getwd(),
                 "NUTS_RG_01M_2016_4326_LEVL_2",
                 verbose = TRUE,
                 stringsAsFactors = FALSE)

#load country shapefile
cntry <- readOGR(getwd(),
                 "CNTR_RG_01M_2013_4326",
                 verbose = TRUE,
                 stringsAsFactors = FALSE)

#only European countries on the map
out <- c("MA", "TN", "DZ", "EG", "LY",
         "JO", "IL", "PS", "SY", "SA",
         "LB", "IQ", "IR", "GL")
cn <- subset(cntry, !FID%in%out)
c <- fortify(cn) #turn the SpatialPolygonsDataFrame into data.frame for plotting

toc <- eurostat::get_eurostat_toc()
k <- 'NUTS 2'
t <- subset(toc, grepl(k, title))


# tvorba grafů -------------------------

# employment rates
lfst_r_lfe2emprt <- eurostat::get_eurostat("lfst_r_lfe2emprt",
                                       time_format = "num")
lfst_r_lfe2emprt_v2 <- lfst_r_lfe2emprt  %>%
  filter(time==2019, # only the year of 2019
         age=="Y15-64", # ages 15-29
         sex=="T") %>% # all genders
  dplyr::select (geo, values)
names(lfst_r_lfe2emprt_v2)[1] <- "NUTS_ID"
lfst_r_lfe2emprt_v2

f1 <- merge(lfst_r_lfe2emprt_v2, nuts2, by="NUTS_ID")
e <- fortify(nuts2, region = "NUTS_ID") %>%
  mutate(NUTS_ID = as.character(id))
d <- e %>% left_join(f1, by = "NUTS_ID")

filtr <- lfst_r_lfe2emprt_v2 %>% 
  filter(NUTS_ID %in% unique(e$NUTS_ID))
length(filtr$NUTS_ID)

ni = classIntervals(d$values,
                    n = 6,
                    style = 'equal')$brks
ni

hist(lfst_r_lfe2emprt_v2$values)
summary(lfst_r_lfe2emprt_v2$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

p2 <- p1 + 
  labs(x = "",
       title="Employment rates",
       subtitle = "NUTS-2 level")
#p2

(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name= "% of population aged 15-64",
                             values=c(barvy, "grey80"),
                             labels=levels(d$cat),
                             drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_employ_rates_2019.png",
       plot = last_plot(),
       dpi = 300)


# Severe material deprivation

ilc_mddd21 <- eurostat::get_eurostat("ilc_mddd21",
                                     time_format = "num")
ilc_mddd21_2 <- ilc_mddd21  %>%
  filter(time==2019) %>%  # only the year of 2019
  dplyr::select (geo, values)
names(ilc_mddd21_2)[1] <- "NUTS_ID"
ilc_mddd21_2

filtr <- ilc_mddd21_2 %>% 
  filter(NUTS_ID %in% unique(e$NUTS_ID))
length(filtr$NUTS_ID)

f1 <- merge(ilc_mddd21_2, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'equal')$brks
ni

hist(ilc_mddd21_2$values)
summary(ilc_mddd21_2$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

p2 <- p1 + 
  labs(x = "",
       title="Severe material deprivation rates",
       subtitle = "NUTS-2 level")
#p2

p3 <- p2 + 
  scale_fill_manual(name = "% of population",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_matdepr_rates_2019.png",
       plot = last_plot(),
       dpi = 300)

# Population density


tgs00024 <- eurostat::get_eurostat("tgs00024",
                                   time_format = "num")
tgs00024 <- tgs00024  %>%
  filter(time==2019) %>%  # only the year of 2019
  dplyr::select (geo, values)
names(tgs00024)[1] <- "NUTS_ID"
tgs00024

f1 <- merge(tgs00024, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")


CZ <- d %>% 
  filter(CNTR_CODE == "CZ")

CZ %>% filter(FID == "CZ05")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'quantile')$brks
ni

hist(tgs00024$values)
summary(tgs00024$values)
tgs00024[tgs00024 == 0.0] <- NA
sum(is.na(tgs00024$values))

#ni2 <- c(0, 50, 100, 150, 200, 250, 500, 1000)

#hist(d$values)
#summary(d$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

p2 <- p1 + 
  labs(x = "",
       title="Population density",
       subtitle = "NUTS-2 level")
#p2
(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name = "number of people per 1 km2",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_popdensity_2019.png",
       plot = last_plot(),
       dpi = 300)

# Unemployment


tgs00010 <- eurostat::get_eurostat("tgs00010",
                                   time_format = "num")
tgs00010 <- tgs00010  %>%
  filter(time == 2019,
         sex == "T",
         isced11 == "TOTAL") %>%  
  dplyr::select (geo, values)
names(tgs00010)[1] <- "NUTS_ID"
tgs00010

f1 <- merge(tgs00010, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'quantile')$brks
ni


labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

p2 <- p1 + 
  labs(x = "",
       title="Unemployment rates",
       subtitle = "NUTS-2 level")
#p2
(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name = "% of economically active population",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
p4

ggsave("Mapa_unemploy_2019.png",
       plot = last_plot(),
       dpi = 300)


# Education ---------------------------------------------------------------


edat_lfs_9917 <- eurostat::get_eurostat("edat_lfs_9917",
                                   time_format = "num")
edat_lfs_9917 <- edat_lfs_9917  %>%
  filter(time == 2019,
         sex == "T",
         age == "Y15-64",
         isced11 == "ED5-8",
         c_birth == "TOTAL") %>%  
  dplyr::select (geo, values)
names(edat_lfs_9917)[1] <- "NUTS_ID"
edat_lfs_9917

f1 <- merge(edat_lfs_9917, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'quantile')$brks
ni

CZ_edu <- d %>% 
  filter(CNTR_CODE == "CZ")

hist(edat_lfs_9917$values)
summary(edat_lfs_9917$values)

#ni2 <- c(0, 50, 100, 150, 200, 250, 500, 1000)

#hist(d$values)
#summary(d$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

p2 <- p1 + 
  labs(x = "",
       title="Tertiary education attainment",
       subtitle = "NUTS-2 level")
#p2
(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name = "% of population",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_tertiaryedu_2019.png",
       plot = last_plot(),
       dpi = 300)


# Population - share 45-54 agegroup ---------------------------------------

demo_r_pjangroup	 <- eurostat::get_eurostat("demo_r_pjangroup",
                                        time_format = "num")
demo_r_pjangroup_tot <- demo_r_pjangroup  %>%
  filter(time == 2019,
         sex == "T",
         age == "TOTAL") %>%  
  dplyr::select (geo, values)
names(demo_r_pjangroup_tot)[1] <- "NUTS_ID"
demo_r_pjangroup_tot

demo_r_pjangroup_45_49 <- demo_r_pjangroup  %>%
  filter(time == 2019,
         sex == "T",
         age == "Y45-49") %>%  
  dplyr::select (geo, values)
names(demo_r_pjangroup_45_49)[1] <- "NUTS_ID"
demo_r_pjangroup_45_49

demo_r_pjangroup_50_54 <- demo_r_pjangroup  %>%
  filter(time == 2019,
         sex == "T",
         age == "Y50-54") %>%  
  dplyr::select (geo, values)
names(demo_r_pjangroup_50_54)[1] <- "NUTS_ID"
demo_r_pjangroup_50_54

(demo_r_pjangroup_45_54 <- tibble(demo_r_pjangroup_45_49[,1], values = NA))
for (i in 1:nrow(demo_r_pjangroup_45_54)){
  demo_r_pjangroup_45_54[i,2] <- sum(demo_r_pjangroup_45_49[i,2], demo_r_pjangroup_50_54[i,2])
}
demo_r_pjangroup_45_54

(pop_shares <- left_join(demo_r_pjangroup_45_54, demo_r_pjangroup_tot, by = "NUTS_ID"))

(share_45_54 <- tibble(demo_r_pjangroup_45_54[,1], values = NA))
for (i in 1:nrow(share_45_54)){
  share_45_54[i,2] <- (pop_shares[i,2] / pop_shares[i,3]) * 100
}
share_45_54

share_45_54 <- do.call(tibble,                      
                        lapply(share_45_54,
                              function(x) replace(x, is.infinite(x), NA)))

share_45_54[[2]] <- ifelse(share_45_54$values > 100, NA, share_45_54$values)

hist(share_45_54$values)
summary(share_45_54$values)

f1 <- merge(share_45_54, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'quantile')$brks

options(scipen = 100,
        digits = 3)
ni


#ni2 <- c(0, 50, 100, 150, 200, 250, 500, 1000)

hist(d$values)
summary(d$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

table(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

p2 <- p1 + 
  labs(x = "",
       title="Share of people aged 45-54",
       subtitle = "NUTS-2 level")
#p2
(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name = "% of population",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_share_45-54_2019.png",
       plot = last_plot(),
       dpi = 300)


# Self-reported unmet needs -----------------------------------------------


hlth_silc_08_r <- eurostat::get_eurostat("hlth_silc_08_r",
                                        time_format = "num")
hlth_silc_08_r <- hlth_silc_08_r  %>%
  filter(time == 2019,
         reason == "NO_UNMET") %>%  
  dplyr::select (geo, values)
names(hlth_silc_08_r)[1] <- "NUTS_ID"
hlth_silc_08_r

f1 <- merge(hlth_silc_08_r, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'quantile')$brks
ni

hist(hlth_silc_08_r$values)
summary(hlth_silc_08_r$values)

#ni2 <- c(0, 50, 100, 150, 200, 250, 500, 1000)

#hist(d$values)
#summary(d$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

tit <- "No unmet needs for medical examination"

p2 <- p1 + 
  labs(x = "",
       title= tit,
       subtitle = "NUTS-2 level")
#p2
(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name = "% of population",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_unmetNeeds_2019.png",
       plot = last_plot(),
       dpi = 300)

# GDP -----------------------------------------------


nama_10r_2gdp <- eurostat::get_eurostat("nama_10r_2gdp",
                                         time_format = "num")
nama_10r_2gdp <- nama_10r_2gdp  %>%
  filter(time == 2019,
         unit == "MIO_EUR") %>%  
  dplyr::select (geo, values)
names(nama_10r_2gdp)[1] <- "NUTS_ID"
nama_10r_2gdp

nama_10r_2gdp[[2]] <- nama_10r_2gdp[[2]]/1000

f1 <- merge(nama_10r_2gdp, nuts2, by="NUTS_ID")
d <- e %>% left_join(f1, by = "NUTS_ID")

ni = classIntervals(d$values,
                    n = 6,
                    style = 'quantile')$brks
ni

hist(nama_10r_2gdp$values)
summary(nama_10r_2gdp$values)

CZ <- d %>% 
  filter(CNTR_CODE == "CZ")

#ni2 <- c(0, 50, 100, 150, 200, 250, 500, 1000)

#hist(d$values)
#summary(d$values)

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
(labels <- labels[1:length(labels)-1])
labels

d$cat <- cut(d$values,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat) 

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

p <-
  ggplot() +
  geom_polygon(data = c, aes(x = long,
                             y = lat,
                             group = group),
               fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long,
                                                     y = lat,
                                                     group = group,
                                                     fill = cat)) +
  geom_path(data = subset(d, !is.na(values)), aes(x = long,
                                                  y = lat,
                                                  group = group),
            color = NA, size = 0) +
  geom_path(data = c, aes(x = long,
                          y = lat,
                          group = group),
            color = "white", size = 0.2)

#p

p1 <- p + 
  coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=c$long,y=c$lat)
#p1

tit <- "Gross domestic product"

p2 <- p1 + 
  labs(x = "",
       title= tit,
       subtitle = "NUTS-2 level")
#p2
(barvy <- brewer.pal(6, "Blues"))

p3 <- p2 + 
  scale_fill_manual(name = "billions Euros",
                    values=c(barvy, "grey80"),
                    labels=levels(d$cat),
                    drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )
#p3

p4 <- p3 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .07),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=15, color="black", hjust=0.5, vjust=-10, face = "bold"),
        plot.subtitle = element_text(size=12, color="black", hjust=0.5, vjust=-15),
        axis.title.x = element_text(size=6, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#p4

ggsave("Mapa_GDP_2019.png",
       plot = last_plot(),
       dpi = 300)


# Histogramy a summary ----------------------------------------------------

options(digits = 4)

my_summary <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

list <- list(share_45_54, edat_lfs_9917, lfst_r_lfe2emprt_v2, ilc_mddd21_2, tgs00024, tgs00010, hlth_silc_08_r, nama_10r_2gdp)

for (i in 1:length(list)){
  df <- list[[i]]
  df[df == 0] <- NA
  list[[i]] <- df
}

names(list) <- c("share_45_54",
                 "edat_lfs_9917",
                 "lfst_r_lfe2emprt_v2",
                 "ilc_mddd21_2",
                 "tgs00024",
                 "tgs00010",
                 "hlth_silc_08_r",
                 "nama_10r_2gdp")

list2env(list, envir = .GlobalEnv) #unlist dfs

all_vars <- list %>% 
  reduce(left_join, by = "NUTS_ID")

head(all_vars)
all_vars <- all_vars[!duplicated(all_vars[1]),]
colnames(all_vars) <- c("NUTS_ID", names(list))

rbind("Share of people aged 45-54 in pop" = my_summary(all_vars$share_45_54),
      "Tertiary education" = my_summary(all_vars$edat_lfs_9917),
      "Employment" = my_summary(all_vars$lfst_r_lfe2emprt_v2),
      "Severe material deprivation" = my_summary(all_vars$ilc_mddd21_2),
      "Population density" = my_summary(all_vars$tgs00024),
      "Unemployment" = my_summary(all_vars$tgs00010),
      "No unmet needs for med exam." = my_summary(all_vars$hlth_silc_08_r),
      "GDP" = my_summary(all_vars$nama_10r_2gdp))

sum(is.na(all_vars$tgs00024))
