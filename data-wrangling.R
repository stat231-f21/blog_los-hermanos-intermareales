#setup
library(tidyverse)
library(vegan)
library(usedist)

#set working directory -- change path variable to data folder filepath
pathLuis <- "C:/Users/Luis/Desktop/Work/stat231/blog_los-hermanos-intermareales/Data/"
pathDavid <- "C:/Users/dmeta/OneDrive/Desktop/STAT231/blog_los-hermanos-intermareales/Data/"
setwd(pathLuis)

#read in dataframes
#fauna2020 <- readxl::read_xlsx("Fauna_Bait_Experiment_2020_LUIS_Ind_m2.xlsx")
fauna2020Wide <- readxl::read_xlsx("Fauna_Bait_Experiment_2020_LUIS__Ind_m2_TRASPOSED.xlsx")
#fauna2014 <- readxl::read_xls("Fauna_Bait_Experiment_2014_LUIS_m2.xls")
fauna2014Wide <- readxl::read_xls("Fauna_Bait_Experiment_2014_LUIS_m2_TRASPOSED.xls")
algaeCover <- readxl::read_xlsx("Covertura.xlsx")
nutrientsSediment <- readxl::read_xlsx("Nutrientes_Sedimento_4.xlsx")
nutrientsWater <- readxl::read_xlsx("Nutrientes_Agua_Intersticial_y_Surf_4.xlsx")
siteCodes <- readxl::read_xlsx("site codes.xlsx")



#data wrangling
#fauna
#drop top column and fix column names
colnames(fauna2020Wide) <- fauna2020Wide[1,]
colnames(fauna2020Wide)[1] <- "site"
colnames(fauna2020Wide)[2] <- "treatment"
colnames(fauna2020Wide)[3] <- "sample_code"

fauna2020Clean <- fauna2020Wide %>%
  slice(-1) %>%
  mutate("year" = "2020")

#bring in 2014 data
#clean it up
colnames(fauna2014Wide) <- fauna2014Wide[1,]
colnames(fauna2014Wide)[1] <- "site"
colnames(fauna2014Wide)[2] <- "treatment"
colnames(fauna2014Wide)[3] <- "sample_code"

fauna2014Clean <- fauna2014Wide %>%
  slice(-1) %>%
  mutate("year" = "2014")

faunaClean <- bind_rows(fauna2020Clean, fauna2014Clean)
faunaClean <- janitor::clean_names(faunaClean)

#remove NAs
#convert all columns to numerics
faunaClean[,4:ncol(faunaClean)] <- sapply(faunaClean[,4:ncol(faunaClean)], as.numeric)
#replace NAs with 0s
faunaClean[is.na(faunaClean)] <- 0

#make summary dataframes with one line per site for each treatment
controlSummary <- faunaClean %>%
  filter(treatment == "Control") %>%
  group_by(site) %>%
  select(-c("site", "treatment", "sample_code")) %>%
  summarize_all(sum)

tenSummary <- faunaClean %>%
  filter(treatment == "Bait 10 min.") %>%
  group_by(site) %>%
  select(-c("site", "treatment", "sample_code")) %>%
  summarize_all(sum)

twentySummary <- faunaClean %>%
  filter(treatment == "Bait 20 min.") %>%
  group_by(site) %>%
  select(-c("site", "treatment", "sample_code")) %>%
  summarize_all(sum)

#clean up algae cover data and prepare it to bind
algaeClean <- algaeCover %>%
  select(-c(3,4,6)) %>%
  janitor::clean_names() %>%
  rename("site" = x1)

# data wrangling for sediment nutrients
# original excel sheet had names of columns listed in the table itself,
# so setting the names of the columns according to this row
colnames(nutrientsSediment) <- nutrientsSediment[1,]

# cleaning up names and getting rid of original row of names
nutrientsSediment <- nutrientsSediment %>%
  janitor::clean_names() %>%
  slice(-1)

# the values were stored as characters, so changing some of them to numeric
nutrientsSediment[,1:2] <- sapply(nutrientsSediment[,1:2], as.numeric)

# filtering to the rows that have the average data and only selecting these cols
nutrientsSedAverages <- nutrientsSediment %>%
  filter(id_muestra %% 6 == 0) %>%
  select(ordenacion, playa, po4_mean = po4_mg_g_sedimento_2,
         po4_sd = po4_mg_g_sediment, po4_sem = po4_mg_g_sedimento_3,
         no2_no3_mean = no2_no3_mg_g_sediment, 
         no2_no3_sd = no2_no3_mg_g_sediment_2,
         no2_no3_sem = no2_no3_mg_g_sediment_3,
         nh4_mean = nh4_mg_g_sediment, nh4_sd = nh4_mg_g_sediment_2,
         nh4_sem = nh4_mg_g_sediment_3, n_total_mean = n_total_mg_g_sediment,
         n_total_sd = n_total_mg_g_sediment_2,
         n_total_sem = n_total_mg_g_sediment_3
  )

# changing the rest of the numeric columns to numeric
nutrientsSedAverages[,3:ncol(nutrientsSedAverages)] <-
  sapply(nutrientsSedAverages[,3:ncol(nutrientsSedAverages)], as.numeric)

# data wrangling for water nutrients
# cleaning up names for easier use
nutrientsWater <- nutrientsWater %>%
  janitor::clean_names()

# converting characters in columns 1, 2 to numeric
nutrientsWater[,1:2] <- sapply(nutrientsWater[,1:2], as.numeric)

# filtering to the rows that contain mean and sd information
nutrientsWaterAverages <- nutrientsWater %>%
  filter(id_muestra %% 6 == 0)

# converting the rest of the appropriate columns to all numeric values
nutrientsWaterAverages[,6:ncol(nutrientsWaterAverages)] <-
  sapply(nutrientsWaterAverages[,6:ncol(nutrientsWaterAverages)], as.numeric)

# giving each beach its own row along with mean and sd values for interstitial
# and surf water
nutrientsWaterAverages <- nutrientsWaterAverages %>%
  group_by(ordenacion) %>%
  summarize(
    int_po4_mean = first(po4_mg_l2),
    surf_po4_mean = last(po4_mg_l2),
    int_po4_sd = first(po4_mg_l3),
    surf_po4_sd = last(po4_mg_l3),
    int_no2no3_mean = first(no2_no32),
    surf_no2no3_mean = last(no2_no32),
    int_no2no3_sd = first(no2_no322),
    surf_no2no3_sd = last(no2_no322),
    int_nh4_mean = first(nh4_mg_l2),
    surf_nh4_mean = last(nh4_mg_l2),
    int_nh4_sd = first(nh4_mg_l3_17),
    surf_nh4_sd = last(nh4_mg_l3_17),
    int_total_mean = first(n_total_18),
    surf_total_mean = last(n_total_18),
    int_total_sd = first(nh4_mg_l3_19),
    surf_total_sd = last(nh4_mg_l3_19)
  )

#calculate diversity index and create new clean dataframe for graphing
allCleanData <- data.frame("site" = algaeClean$site, 
                           "controlDiversity" = diversity(controlSummary[,-1], index="shannon"), 
                           "tenMinDiversity" = diversity(tenSummary[,-1], index="shannon"), 
                           "twentyMinDiversity" = diversity(twentySummary[,-1], index="shannon"),
                           "speciesRichness" = specnumber(twentySummary[,-1]))
#bring in algae data
allCleanData$algaePercentCover <- algaeClean$cover_percent

#bring in sediment nutrient data and water nutrient data (doing all nutrients)
allCleanData$po4SedMean <- nutrientsSedAverages$po4_mean
allCleanData$po4WaterIntMean <- nutrientsWaterAverages$int_po4_mean
allCleanData$po4WaterSurfMean <- nutrientsWaterAverages$surf_po4_mean
allCleanData$no2no3SedMean <- nutrientsSedAverages$no2_no3_mean
allCleanData$no2no3WaterIntMean <- nutrientsWaterAverages$int_no2no3_mean
allCleanData$no2no3WaterSurfMean <- nutrientsWaterAverages$surf_no2no3_mean
allCleanData$nh4SedMean <- nutrientsSedAverages$nh4_mean
allCleanData$nh4WaterIntMean <- nutrientsWaterAverages$int_nh4_mean
allCleanData$nh4WaterSurfMean <- nutrientsWaterAverages$surf_nh4_mean
allCleanData$totalSedMean <- nutrientsSedAverages$n_total_mean
allCleanData$totalWaterIntMean <- nutrientsWaterAverages$int_total_mean
allCleanData$totalWaterSurfMean <- nutrientsWaterAverages$surf_total_mean

#write
write_csv(allCleanData, 'beachData.csv')



######################################

######################################



#sample visualizations
#diversity vs algae cover
ggplot(data = allCleanData, aes(x = algaePercentCover, y = controlDiversity)) +
  geom_point(size = 5, aes(color = site)) +
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  labs(title = "Diversity vs Algal Cover", x = "Algae Cover (%)", y = "Shannon Diversity Index (Control)", color = "Site")


#jaccard similarity index comparison/groupings
jaccard <- vegdist(controlSummary[-1], method = "jaccard")

names <- controlSummary[[1]]
jaccard <- dist_setNames(jaccard, names)

plot(
  hclust(jaccard),
  hang = -1,
  main = "Sites clustered by Jaccard similarity",
  axes = FALSE, ylab = "")

#comparison of treatments
diversityData <- allCleanData %>%
  select(c("site", "controlDiversity", "tenMinDiversity", "twentyMinDiversity")) %>%
  pivot_longer(-site, names_to = "treatment", values_to = "diversity")

diversityData$treatment <- str_replace_all(diversityData$treatment, pattern = "tenMinDiversity", replacement = "10")
diversityData$treatment <- str_replace_all(diversityData$treatment, pattern = "twentyMinDiversity", replacement = "20")
diversityData$treatment <- str_replace_all(diversityData$treatment, pattern = "controlDiversity", replacement = "control")

#boxplot/histos comparing treatments
ggplot(data = diversityData, aes(x = treatment, y = diversity)) +
  geom_col(aes(fill = treatment)) +
  scale_color_brewer() +
  theme_bw() +
  facet_wrap(~ site)

ggplot(data = diversityData, aes(x = treatment, y = diversity)) +
  geom_boxplot()

#algae cover vs nutrients
ggplot(data = allCleanData, aes(x = po4SedMean, y = algaePercentCover)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(0, 4)

write_csv(diversityData, 'diversityData.csv')

#assemble plots of nutrient data
library(cowplot)

sedpo4 <- ggplot(data = allCleanData, aes(x = po4SedMean, y = controlDiversity)) +
  geom_point() +
  geom_smooth(method = 'lm')

sedno2no3 <- ggplot(data = allCleanData, aes(x = no2no3SedMean, y = controlDiversity)) +
  geom_point() +
  geom_smooth(method = 'lm')

plot_grid(sedpo4, sedno2no3)

#open up photo sphere
library(googleway)
coords <- read.csv('siteCoords.csv')

#cleaning up coords, preparing for left join
coords <- coords %>%
  janitor::clean_names() %>%
  mutate(site = case_when(
    i_site == "America" ~ "America_1",
    i_site == "America 2" ~ "America_2",
    i_site == "de Nerga" ~ "Nerga",
    i_site == "A Lanzada" ~ "Lanzada",
    TRUE ~ i_site
  ))

# left join with allCleanData
allCleanData <- allCleanData %>%
  left_join(coords, by = c("site" = "site")) %>%
  select(-i_site)

coordsVector <- c(allCleanData$lat[1], allCleanData$long[1])
google_map_panorama(coordsVector)

#NMDS
nmds <- metaMDS(controlSummary[-1])
  

plot(nmds)

pointData <- data.frame(nmds$points) %>%
  mutate(site = controlSummary$site)

ggplot() +
  geom_point(data = pointData, size = 8, aes(x = MDS1, y = MDS2, col = site))


#interactive map
library(leaflet)

leaflet(data = allCleanData) %>%
  addTiles() %>%
  addMarkers()


