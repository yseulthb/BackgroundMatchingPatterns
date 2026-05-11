# Data analysis - Visual Pattern preferences (online study)

# Load needed packages
library(dplyr)
library(stringr)
library(ggplot2)
library(lattice)
library(countrycode)
library(tidyverse)
library(magrittr)
library(ggmap)
library(rvest)
library(lme4)
library(EloChoice)
library(ggpubr)
library(lmerTest)
library(jtools)
library(reghelper)
library(report)
library(multilevel)
library(interactions)
library(sjPlot)


# Initialisation ----------------------------------------------------------

rm(list=ls()) #clean work space
setwd("/home/yseult/Documents/CamoStudy/StatisticalAnalyses/Data/")

# 1. Load and clean
# There are 2 different data files: participants_info.csv with information on participants 
# and results_data.csv that contains the response data 
# (latest date of data acquisition: September 8th, 2022)

res_data <- read.csv("results_data.csv", header = TRUE)
ppp_data <- read.csv("participants_info.csv", header = TRUE)

# # We'll start with ppp_data

# remove participants that didn't complete the experiment, using the last 
# questions i.e. for whom columns "hobby" or "freqexpo" include the "NULL" value
ppp_data2<-ppp_data[!(ppp_data$freqexpo=="NULL" | ppp_data$hobby=="NULL"),]

# rename some columns and replace values to have everything in English
ppp_data2 <- rename(ppp_data2, c(gender = sex, year = annee, month = mois , colorblindness = daltonisme ))
ppp_data2$gender = str_replace_all(ppp_data2$gender,"H","M")

# Check how many participants did the experiment more than once ("IP_vierge = non" means more than 1 attempt)
ppp_data2 %>% 
  group_by(IP_vierge) %>%
  summarise(no_rows = length(IP_vierge))

# it might still be necessary to manually check participants who failed their first attempt  
# due to connection issues/see the comment section and keep their second attempt only
double_attempt <- ppp_data2[!(ppp_data2$IP_vierge=="oui"),]
# number of double attempt is low enough not to bother much about it

# remove participants that did the experiment more than once "IP_vierge == non"
ppp_data3<-ppp_data2[!(ppp_data2$IP_vierge=="non"),]

# Compare participants number
ppp_ID <- unique(ppp_data3$ID_juge)
res_ID <- unique(res_data$ID_juge)

# Keep ID_juge that is found in both data sets
shared_ID <- intersect(ppp_ID, res_ID) 
# the number of shared_ID should be equal to the number of ppp kept in ppp_data3

sub_res_data <- res_data[res_data$ID_juge %in% shared_ID, ]
sub_res_ID <- unique(sub_res_data$ID_juge)

ppp_data3 <- ppp_data3[ppp_data3$ID_juge %in% sub_res_ID, ] 

# Participant data ---------------------------------------------------------
# Explore the demographics of participants (ppp_data3)
# 1. Self-reported Gender
ppp_data3 %>% 
  group_by(gender) %>%
  summarise(no_rows = length(gender))
# -> about half ppp identify as F (60%)

# 2. Last diploma
# Replace the French and Spanish diploma denominations by the English ones
ppp_data3$level_studies <- str_replace_all(ppp_data3$level_studies, c("CEP_aucun_diplome"="PSC_no_diploma", "Sin_estudios"="PSC_no_diploma", 
                                                                      "Brevet_des_colleges_BEPC"="CGE_SSCE", "Estudios_primarios_completos"="CGE_SSCE", 
                                                                      "CAP_BEP_ou_diplome_de_meme_niveau"="voc_training_school_certificate", "Estudios_secundarios_1"="voc_training_school_certificate", 
                                                                      "Bac_general_technologique_professionnel_ou_equivalent"="High_School", "Estudios_secundarios_2"="High_School", "High_School "="High_School",
                                                                      "DEUG_BTS_DUT_DEUST_\\(Bac_\\+_2\\)"="Assoc_Degree_or_Bachelors_Degree", "Licence_Licence\\-LMD_Licence_professionnelle_\\(Bac_\\+_3\\) "="Assoc_Degree_or_Bachelors_Degree", 
                                                                      "Estudios_universitarios_medios"="Assoc_Degree_or_Bachelors_Degree",
                                                                      "Maitrise_Master_DEA_DESS_Diplome_dingénieur_\\(Bac_\\+_4_ou_Bac_\\+_5\\)"="Graduate Degree", "Doctorat_\\(Bac_\\+_8\\)"="Graduate Degree",
                                                                      "Estudios_universitarios_superiores"="Graduate Degree", "Graduate Degree "="Graduate Degree"
                                                                      ))
ppp_data3 %>% 
  group_by(level_studies) %>%
  summarise(no_rows = length(level_studies))
# -> vast majority of Graduate degrees: 66%

# 3. Color-blindness
ppp_data3 %>% 
  group_by(colorblindness) %>%
  summarise(no_rows = length(colorblindness))

# 4. Hobby including visual creativity
ppp_data3$hobby <- str_replace_all(ppp_data3$hobby, c("non"="no", "parfois"="sometimes", "Quotidiennement"="daily", "souvent"="often"))

ppp_data3 %>% 
  group_by(hobby) %>%
  summarise(no_rows = length(hobby))

# 5. Frequency of exhibition visits
ppp_data3$freqexpo <- str_replace_all(ppp_data3$freqexpo, c("An"="yearly", "Hebdo"="weekly", "Jamais"="never", "Mens"="monthly"))

ppp_data3 %>% 
  group_by(freqexpo) %>%
  summarise(no_rows = length(freqexpo))

# 6. Country of residency
ppp_data3$pays <- ppp_data3$countries
ppp_data3$pays <- str_replace_all(ppp_data3$pays, c("Coree du sud"="Korea", "Suede"="Suède", "Republique tcheque"="République tchèque", 
                                                    "Nouvelle-Zelande"="Nouvelle-Zélande", "Norvege"="Norvège", "Emirats arabes unis"="Émirats arabes unis",
                                                    "Etats-Unis"="États-Unis"))
country_names <- ppp_data3$pays
ppp_data3$pays <- countryname(country_names)

country <- ppp_data3 %>% 
  group_by(pays) %>%
  summarise(no_rows = length(pays))
country$pays <- recode(country$pays, 'US'='USA')

map.world <-map_data("world")
map.world_joined <- left_join(map.world, country, by = c('region' = 'pays'))
map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(no_rows),F,T))
map.world_joined$ppp <- map.world_joined$no_rows
map.world_joined$ppp[is.na(map.world_joined$no_rows)] <- 0

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = ppp)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + #colorblind friendly palette
  labs(fill = "Number of Participants") +
  theme(text = element_text(family = "Gill Sans"), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()  
        )


# 7. Age of participants
age <- ppp_data3 %>% 
  group_by(year) %>%
  summarise(no_rows = length(year))
age <- age[order(-age$no_rows), ]
age["cumsum"] <- cumsum(age$no_rows)

ggplot(data = age, aes(y = no_rows, x = year)) + geom_point() + xlab("Year of birth") + ylab("Number of participants")

ggplot(ppp_data3, aes(x=year, color=gender)) +
  geom_histogram(fill="white", alpha=0.5, position="stack") + xlab("Year of birth") + ylab("Number of participants")


# Experiment data: Initialisation ---------------------------------------------------------
#There are 2 different tasks: detection and 2AFC
sub_res_data$task <- as.factor(sub_res_data$task)
#There are 4 different background values: 0 (=gray background), -1, -2, -3
sub_res_data$stim_pente_BKG_f <- as.factor(sub_res_data$stim_pente_BKG)
sub_res_data$BKG_type <- as.factor(sub_res_data$BKG_type)

#Slope value of the targets
# > have to be extracted from the stimulus file name
# AFC x noisy BKG: img/Fig-1BKG-1.25vs-0.50-target160_Blur0.50_slopediff-0.25_GS_2.png
# AFC x gray BKG: img/Fig_GrayBKG-2.25vs-3.50-target160_noBlur_targetdiff-1.25_GS_2.png
# detection task: img/Fig-1BKG-1.25target_centreXY_377_303_Blur0.5_slopediff-0.25_GS_2.png

stim_file <- "img/Fig-"
stim_name <- sub_res_data$stim_name
stim_file_AFCg <- "img/Fig_GrayBKG"

sub_res_data$cible1 <- NA
sub_res_data$cible2 <- NA
sub_res_data$cible1[which(sub_res_data$BKG_type=='NULL')] = substr(stim_name[which(sub_res_data$BKG_type=='NULL')],nchar(stim_file)+5,17)
sub_res_data$cible1[sub_res_data$BKG_type=='noisy'] = substr(stim_name[sub_res_data$BKG_type=='noisy'],nchar(stim_file)+5,17)
sub_res_data$cible2[sub_res_data$BKG_type=='noisy'] = substr(stim_name[sub_res_data$BKG_type=='noisy'],nchar(stim_file)+12,24)
sub_res_data$cible1[sub_res_data$BKG_type=='gray'] = substr(stim_name[sub_res_data$BKG_type=='gray'],nchar(stim_file_AFCg)+1,20)
sub_res_data$cible2[sub_res_data$BKG_type=='gray'] = substr(stim_name[sub_res_data$BKG_type=='gray'],nchar(stim_file_AFCg)+8,27)

# there are 5 stimulus versions for each condition
sub_res_data["stim_ver"] <- substr(stim_name, nchar(stim_name)-4 ,nchar(stim_name)-4)

# Extract the difference between BKG slope and target slope
# for DT and 2-AFCxNoisy only
sub_res_data$slopeBKG_cible1 <- NA
sub_res_data$slopeBKG_cible2 <- NA
sub_res_data$slopeBKG_cible1[which(sub_res_data$BKG_type=='NULL')] = abs(as.numeric(as.character(sub_res_data$stim_pente_BKG[which(sub_res_data$BKG_type=='NULL')])))-abs(as.numeric(sub_res_data$cible1[which(sub_res_data$BKG_type=='NULL')]))
sub_res_data$slopeBKG_cible1[which(sub_res_data$BKG_type=='noisy')] = abs(as.numeric(as.character(sub_res_data$stim_pente_BKG[which(sub_res_data$BKG_type=='noisy')])))-abs(as.numeric(sub_res_data$cible1[which(sub_res_data$BKG_type=='noisy')]))
sub_res_data$slopeBKG_cible2[which(sub_res_data$BKG_type=='noisy')] = abs(as.numeric(as.character(sub_res_data$stim_pente_BKG[which(sub_res_data$BKG_type=='noisy')])))-abs(as.numeric(sub_res_data$cible2[which(sub_res_data$BKG_type=='noisy')]))


# Experiment data: Detection task -----------------------------------------

# Determine whether the target was properly detected by the participant
# distance to the centre of the target: sqrt((click_X-centre_X)**2 + (click_Y-centre_Y)**2)
target_rad = 160/2
click_X = as.numeric(sub_res_data$click_x)
click_Y = as.numeric(sub_res_data$click_y)
centre_X = as.numeric(sub_res_data$stim_tarX)
centre_Y = as.numeric(sub_res_data$stim_tarY)
dist2centre = sqrt((click_X-centre_X)**2 + (click_Y-centre_Y)**2)

sub_res_data$within_target <- NA
sub_res_data$within_target[dist2centre>target_rad] = FALSE
sub_res_data$within_target[dist2centre<=target_rad] = TRUE

# compute distance between the target centre and the (absolute) centre of the image stimulus (ie, X,Y = 325)
abs_centre_X = 325
abs_centre_Y = 325
sub_res_data$target_pos <- NA
sub_res_data$target_pos <- sqrt((click_X-abs_centre_X)**2 + (click_Y-abs_centre_Y)**2)


# Create a sub dataset for the detection task
missed_trials <- subset(sub_res_data, task=="detection-task-test" & rt=="NULL")
sub_res_data$rt <- as.integer(sub_res_data$rt)
df_detection_task <- subset(sub_res_data, task=="detection-task-test" & rt>150)
df_detection_task2 <- subset(sub_res_data, task=="detection-task-test" & rt<=150) #trials removed

#df_detection_task$BKGslope <-as.factor(as.numeric(as.factor(df_detection_task$stim_pente_BKG)))

# create a uniqueID per cond (should be 18 for the DT)
df <- data.frame(df_detection_task$cible1, df_detection_task$stim_pente_BKG)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_detection_task.cible1, df_detection_task.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
  ungroup()

df_detection_task["condID"] <- df_interact1$interact

# create a uniqueID per cond and per stim version (should be 18*5 for the DT)
df <- data.frame(df_detection_task$cible1, df_detection_task$stim_pente_BKG, df_detection_task$stim_ver)
df_interact2 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_detection_task.cible1, df_detection_task.stim_pente_BKG, df_detection_task.stim_ver)) %>% paste(., collapse = ".")) %>%
  ungroup()

df_detection_task["condID_ver"] <- df_interact2$interact


df_detection_task <- df_detection_task %>% 
  mutate(experiment_order = if_else(trialnb <= 20, 1, 2))

#hist(as.numeric(df_detection_task$within_target))
detection_rate = 100 * sum(df_detection_task$within_target,  na.rm = TRUE) / sum(df_detection_task$within_target!='NA',  na.rm = TRUE)

# check that there's a similar number of trials per condition (this is the case)
trialpercond <- df_detection_task %>% 
  group_by(condID) %>%  
  summarise(no_rows = length(slopeBKG_cible1))

mean(trialpercond$no_rows)
median(trialpercond$no_rows)
sd(trialpercond$no_rows)

# how many ppp saw each image? > about 395 ppp/image
pppxcond <- df_detection_task %>%
  group_by(condID_ver) %>%
  summarise(no_rows = length(ID_juge))

mean(pppxcond$no_rows)
median(pppxcond$no_rows)
sd(pppxcond$no_rows)

#df_detection_task["BKG"] <- as.numeric(df_detection_task$stim_pente_BKG)

df_detection_task <- rename(df_detection_task, c(BKG = stim_pente_BKG, slope_target = cible1, slope_diff = slopeBKG_cible1 ))


# compute detection rate per cond
DRperCond <- df_detection_task %>% 
  group_by(condID) %>%  
  summarise(no_rows = 100*(sum(within_target,  na.rm = TRUE) / sum(within_target!='NA',  na.rm = TRUE)))

DRperCondperVer <- df_detection_task %>%
  group_by(condID_ver) %>%
  summarise(no_rows = 100*(sum(within_target,  na.rm = TRUE) / sum(within_target!='NA',  na.rm = TRUE)))

mean(DRperCond$no_rows)
median(DRperCond$no_rows)
sd(DRperCond$no_rows)

mean(DRperCondperVer$no_rows)
median(DRperCondperVer$no_rows)
sd(DRperCondperVer$no_rows)


# Plot the detection rate (%) as a function of condition ID
ggplot(data=DRperCond, aes(x= condID, y=no_rows)) + geom_point() +
  geom_hline(yintercept=mean(DRperCond$no_rows), linetype="dashed", color = "red") + 
  geom_text(aes(1, mean(no_rows), label = paste("mean =", round(mean(no_rows), digits = 2)), vjust = - 1, hjust = -5.25), col = "red") +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection rate (%) for the different conditions") +
  xlab("Condition ID") + ylab("Detection rate (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

# Plot the detection rate (%) as a function of condition ID, including stim_ver variability
DRperCondperVer["stim_ver"] <- substr(DRperCondperVer$condID_ver, nchar(DRperCondperVer$condID_ver), nchar(DRperCondperVer$condID_ver))
DRperCondperVer["condID"] <- substr(DRperCondperVer$condID_ver, 1, nchar(DRperCondperVer$condID_ver)-2)

DRperCondperVer %>%
  group_by(condID) %>%
  summarise_at(vars(no_rows), list(name = sd))

ggplot(data=DRperCondperVer, aes(x= condID, y=no_rows)) + geom_boxplot(fill = "grey92", width = .1) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
  geom_hline(yintercept=mean(DRperCondperVer$no_rows), linetype="dashed", color = "red") + 
  geom_text(aes(1, mean(no_rows), label = paste("mean =", round(mean(no_rows), digits = 2)), vjust = - 1, hjust = -5.25), col = "red") +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection rate (%) for the different conditions and their different stimuli") +
  xlab("Condition ID") + ylab("Detection rate (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

# Detection rates (%) - version 2:
library(RColorBrewer)
DRperCondperVer$BKG <- c(rep(-1, 30), rep(-2, 30), rep(-3, 30))
DRperCondperVer$condIDf <- as.factor(DRperCondperVer$condID)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data=DRperCondperVer, aes(x= condIDf, y=no_rows, colour=factor(BKG))) +
  scale_colour_manual(values=cbPalette) +
  geom_boxplot(alpha=0.5, width = .5, outlier.shape = NA) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
  geom_smooth(aes(group=BKG), method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 0.3, linetype = 2) +
  #geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") +
  ggtitle("Detection time (ms) for the different conditions and their different stimuli") +
  xlab("Condition ID") + ylab("Detection time (ms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 



# compute the median of detection time per cond
medianRT <- df_detection_task %>% 
  group_by(condID) %>%  
  summarise(no_rows = median(rt)) 

medianRT_perVer <- df_detection_task %>% 
  group_by(condID_ver) %>%  
  summarise(no_rows = median(rt))

medianRT_perVer_within <- df_detection_task[df_detection_task$within_target==TRUE,] %>% 
  group_by(condID_ver) %>%  
  summarise(no_rows = median(rt))

max(df_detection_task$rt) #should be less than 8000 which is the trial max duration
min(df_detection_task$rt) #should be more than 150

# check the correlation between detection rates and reactions times. should be pretty high.
cor(x=medianRT$no_rows, y=DRperCond$no_rows, method = "spearman")
cor(x=medianRT_perVer$no_rows, y=DRperCondperVer$no_rows, method = "spearman")

# Some plots to visualise the data
#ggplot(df_detection_task, aes(y = rt, x = condID, color = BKG)) +
#  geom_point(size = 2) #not a great plot

ggplot(data=medianRT, aes(x= condID, y=no_rows)) + geom_point() +
  geom_hline(yintercept=median(medianRT$no_rows), linetype="dashed", color = "red") + 
  geom_text(aes(1, median(no_rows), label = paste("median =", round(median(no_rows), digits = 2)), vjust = - 1, hjust = -4.25), col = "red") +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection time (ms) for the different conditions") +
  xlab("Condition ID") + ylab("Detection time (ms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

# Plot the detection time (ms) as a function of condition ID, including stim_ver variability
medianRT_perVer["stim_ver"] <- substr(medianRT_perVer$condID_ver, nchar(medianRT_perVer$condID_ver), nchar(medianRT_perVer$condID_ver))
medianRT_perVer["condID"] <- substr(medianRT_perVer$condID_ver, 1, nchar(medianRT_perVer$condID_ver)-2)

medianRT_perVer_within["stim_ver"] <- substr(medianRT_perVer_within$condID_ver, nchar(medianRT_perVer_within$condID_ver), nchar(medianRT_perVer_within$condID_ver))
medianRT_perVer_within["condID"] <- substr(medianRT_perVer_within$condID_ver, 1, nchar(medianRT_perVer_within$condID_ver)-2)

medianRT_perVer %>%
  group_by(condID) %>%
  summarise_at(vars(no_rows), list(name = sd))

medianRT_perVer_within %>%
  group_by(condID) %>%
  summarise_at(vars(no_rows), list(name = sd))


ggplot(data=medianRT_perVer, aes(x= condID, y=no_rows)) + geom_boxplot(fill = "grey92", width = .1) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
  geom_hline(yintercept=median(medianRT$no_rows), linetype="dashed", color = "red") + 
  geom_text(aes(1, median(no_rows), label = paste("median =", round(median(no_rows), digits = 2)), vjust = - 1, hjust = -4.35), col = "red") +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection time (ms) for the different conditions and their different stimuli") +
  xlab("Condition ID") + ylab("Detection time (ms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 


ggplot(data=medianRT_perVer_within, aes(x= condID, y=no_rows)) + geom_boxplot(fill = "grey92", width = .1) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
  geom_hline(yintercept=median(medianRT$no_rows), linetype="dashed", color = "red") + 
  geom_text(aes(1, median(no_rows), label = paste("median =", round(median(no_rows), digits = 2)), vjust = - 1, hjust = -4.35), col = "red") +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection time (ms) for the different conditions and their different stimuli - within only") +
  xlab("Condition ID") + ylab("Detection time (ms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

# rt.strength.plot and psi.plot were plotted based on a code available at https://gist.github.com/crowding/5846850
rt.strength.plot <- (
  ggplot(df_detection_task)
  + aes(rt, color=factor(condID))
  + stat_density(geom="line", position="identity")
  + scale_color_discrete("Condition ID", h=c(270,0), direction=1)
  + labs(x="Response time", y="Density",
         title="Distribution of response times by stimulus condition")
)
print(rt.strength.plot) #a better plot

#same plot but only for successful trials
rt.strength.plot_within <- (
  ggplot(df_detection_task[df_detection_task$within_target==TRUE,])
  + aes(rt, color=factor(condID))
  + stat_density(geom="line", position="identity")
  + scale_color_discrete("Condition ID", h=c(270,0), direction=1)
  + labs(x="Response time", y="Density",
         title="Distribution of response times by stimulus condition - hits")
)
print(rt.strength.plot_within) #a better plot

#same plot but separately for each BKG
subBKG <- subset(df_detection_task, BKG==-1)
rt.strength.plot <- (
  ggplot(subBKG)
  + aes(rt, color=factor(condID))
  + stat_density(geom="line", position="identity")
  + scale_color_discrete("Condition ID", h=c(270,0), direction=1)
  + ylim(c(-0.0001, 0.0021))
  + labs(x="Response time", y="Density",
         title="Distribution of response times by stimulus condition")
)
print(rt.strength.plot) #a better plot

# Experiment data: Preference tasks - gray BKG ---------------------------

# Create a sub dataset for the gray 2AFC task
df_2AFC_gray <- subset(sub_res_data, BKG_type=="gray" & rt != 'NULL')
#df_2AFC_gray2 <- subset(sub_res_data, BKG_type=="gray")

#check left/right bias
df_2AFC_gray %>% 
  group_by(click_half) %>%
  summarise(no_rows = length(click_half))


# create a uniqueID per cond (should be 90 but 45 will appear: left/right not taken into account at this stage) 
df <- data.frame(df_2AFC_gray$cible1, df_2AFC_gray$cible2)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_gray.cible1,df_2AFC_gray.cible2)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_gray["condID"] <- df_interact1$interact

# check that there's a similar number of trials per condition (this is the case)
df_mean <- df_2AFC_gray %>% 
  group_by(condID) %>%  
  summarise(no_rows = length(slopeBKG_cible1))

mean(df_mean$no_rows)
median(df_mean$no_rows)
sd(df_mean$no_rows)

# how many ppp saw each image? > about 36 ppp/image (group_by(stim_name)) and 71/condID_ver (group_by(condID_ver))
pppxcond <- df_2AFC_gray %>%
  group_by(stim_name) %>%
  summarise(no_rows = length(ID_juge))

mean(pppxcond$no_rows)
median(pppxcond$no_rows)
sd(pppxcond$no_rows)

nstim <- length(unique(df_2AFC_gray$condID)) #nstim = 39

df_2AFC_gray <- df_2AFC_gray %>% 
  mutate(experiment_order = if_else(trialnb <= 20, 1, 2))


# Create two new columns to determine the preferred stim (winner) of a pair (vs loser)
# Using the condID_cibleX to include the BKG slope value in one pair

df_2AFC_gray$winner <- NA
df_2AFC_gray$loser <- NA

# cond: cible1 (left) & cible2 (right)
for (ndx in 1:length(df_2AFC_gray$winner)) {
  if (df_2AFC_gray$click_half[ndx]=="left") {
    df_2AFC_gray$winner[ndx] <- df_2AFC_gray$cible1[ndx]
    df_2AFC_gray$loser[ndx] <- df_2AFC_gray$cible2[ndx]
  }
  else if (df_2AFC_gray$click_half[ndx]=="right") {
    df_2AFC_gray$winner[ndx] <- df_2AFC_gray$cible2[ndx]
    df_2AFC_gray$loser[ndx] <- df_2AFC_gray$cible1[ndx]
  }
}

df_2AFC_gray$winner_ver <- paste(df_2AFC_gray$winner, df_2AFC_gray$stim_ver, sep = ".")
df_2AFC_gray$loser_ver <- paste(df_2AFC_gray$loser, df_2AFC_gray$stim_ver, sep = ".")


# Élő ranking >> for docs, see https://rdrr.io/cran/EloChoice/

set.seed(123)
res1 <- elochoice(df_2AFC_gray$winner, df_2AFC_gray$loser, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res1)

#ratings(res, show = "mean", drawplot = FALSE)
myratings_gray <- ratings(res1, show = "mean", drawplot = TRUE)

# same but with intracondition variab
set.seed(123)
res1b <- elochoice(df_2AFC_gray$winner_ver, df_2AFC_gray$loser_ver, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res1b)

myratings <- ratings(res1b, show = "original", drawplot = FALSE)
ratings(res1b, show = "mean", drawplot = TRUE)


# # Plot the ranking per condition
# temp <- res1$ratmat
# temp2 <- apply(temp, 2, range, na.rm = TRUE) # ncol(temp2) > length(temp2)
# #temp2 <- apply(temp, 2, range, na.rm = TRUE)
# #temp2 <- temp2[, rev(order(temp2[1, ]))]
# plot(0, 0, xlim = c(1, ncol(temp2)), ylim = range(temp2), #temp2 statt temp
#      "n", xlab = "Slope value of the target", ylab = "Elo-rating", xaxt='n', yaxt='n')
# points(1:ncol(temp), colMeans(temp), pch = 16) #temp2[1, ]
# axis(1, at = 1:ncol(temp2), labels = FALSE)
# axis(2, las = 1)
# text(x = (1:ncol(temp2))+0.6, y = par("usr")[3], labels = colnames(temp2),
#      xpd = NA, srt = 35, adj = 1.7)
# title(main = "Élő Ratings for 2-AFC task with gray BKG")


# when 1000 iterations
temp <- res1$ratmat
df <- data.frame(colMeans(temp), apply(temp, 2, sd), c( -0.25, -0.50, -0.75, -1.25, -1.50, -1.75, -2.25, -2.50, -2.75, -3.25, -3.50, -3.75))
colnames(df) <- c("ranking", "sd", "slope")

ggplot(df, aes(x=as.character(slope), y=ranking)) + 
  geom_errorbar(aes(ymin=ranking-sd, ymax=ranking+sd), width=.2) +
  geom_line() +
  geom_point() +
  xlab("Slope") + ylab("Élő Ratings") +
  ggtitle("Élő Ratings for 2-AFC task with gray BKG across 1000 iterations (mean +/- SD)")

#labels = c( -0.25, -0.50, -0.75, -1.25, -1.50, -1.75, -2.25, -2.50, -2.75, -3.25, -3.50, -3.75)

# Plot the average ranking per condition (averaged across stim versions)
#rm(df_elopercondver)
temp <- res1b$ratmat
df_elopercondver <- data.frame(colMeans(temp), apply(temp, 2, sd))
names <- rownames(df_elopercondver)
rownames(df_elopercondver) <- NULL
df_elopercondver <- cbind(names,df_elopercondver)
colnames(df_elopercondver) <- c("condID_ver", "ranking", "sd")

df_elopercondver["stim_ver"] <- substr(df_elopercondver$condID_ver, nchar(df_elopercondver$condID_ver), nchar(df_elopercondver$condID_ver))
df_elopercondver["condID"] <- substr(df_elopercondver$condID_ver, 1, nchar(df_elopercondver$condID_ver)-2)

ggplot(data = df_elopercondver, aes(x = condID, y = ranking)) + 
  geom_boxplot(color = "red", fill = "red", alpha=0.3, width = .5, outlier.shape = NA) + 
  geom_point(color = "red", fill = "red", size = 2, alpha = .2, position = position_jitter(seed = 1, width = .2)) +
  geom_smooth(aes(group=1), color="red", method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 0.5, linetype = 2) +
  ggtitle("Élő Ratings (average across 1000 iterations) for 2-AFC task 
          with gray BKG and their different stimuli") +
  xlab("Condition ID") + ylab("Ranking (Élő score)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

df_elopercondver %>%
  group_by(condID) %>%
  summarise_at(vars(ranking), list(name = mean))


# how many raters are needed to achieve stability in ratings?
# length(unique(df_2AFC_gray$ID_juge))
# set.seed(123)
# res2 <- raterprog(df_2AFC_gray$winner, df_2AFC_gray$loser, df_2AFC_gray$ID_juge, progbar = TRUE) ## takes time!

#raterprogplot(res2)
# xdata <- res2 #res1 or res2
# ids <- 1:nrow(xdata)
# yl <- expression(reliability ~ index ~ italic("R'"))
# plot(0, 0, xlim = range(1:nrow(res2)), ylim = c(0.4, 0.7), las = 1, 
#       type = "n", xlab = "number of raters", ylab = yl)
# x <- apply(xdata, 1, quantile, c(0.25, 0.75))
# segments(ids, x[1, ], ids, x[2, ], col = "black")
# points(ids, rowMeans(xdata), pch = 16)
# title(main = "BKG = noisy")



# Experiment data: Preference tasks - noisy BKG ---------------------------

## Élő score: ranking of the images of the preference tasks

# Create a sub dataset for the noisy 2AFC task
df_2AFC_noisy <- subset(sub_res_data, BKG_type=="noisy" & rt != 'NULL')
#df_2AFC_noisy2 <- subset(sub_res_data, BKG_type=="noisy")

#check left/right bias
df_2AFC_noisy %>% 
  group_by(click_half) %>%
  summarise(no_rows = length(click_half))


# create a uniqueID per cond (should be 90 but 45 will appear: left/right not taken into account at this stage) 
df <- data.frame(df_2AFC_noisy$cible1, df_2AFC_noisy$cible2, df_2AFC_noisy$stim_pente_BKG, df_2AFC_noisy$stim_ver)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_noisy.cible1, df_2AFC_noisy.cible2, df_2AFC_noisy.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_noisy["condID"] <- df_interact1$interact

df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_noisy.cible1, df_2AFC_noisy.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_noisy["condID_cible1"] <- df_interact1$interact

df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_noisy.cible2, df_2AFC_noisy.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_noisy["condID_cible2"] <- df_interact1$interact

df_interact2 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_noisy.cible1, df_2AFC_noisy.cible2, df_2AFC_noisy.stim_pente_BKG, df_2AFC_noisy.stim_ver)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_noisy["condID_ver"] <- df_interact2$interact

df_2AFC_noisy <- df_2AFC_noisy %>% 
  mutate(experiment_order = if_else(trialnb <= 20, 1, 2))

# check that there's a similar number of trials per condition (this is the case)
df_mean <- df_2AFC_noisy %>% 
  group_by(condID) %>%  
  summarise(no_rows = length(slopeBKG_cible1))

mean(df_mean$no_rows)
median(df_mean$no_rows)
sd(df_mean$no_rows)

# how many ppp saw each image? > about 36 ppp/image (group_by(stim_name)) and 71/condID_ver (group_by(condID_ver))
pppxcond <- df_2AFC_noisy %>%
  group_by(stim_name) %>%
  summarise(no_rows = length(ID_juge))

mean(pppxcond$no_rows)
median(pppxcond$no_rows)
sd(pppxcond$no_rows)

# formula: elochoice(winner, loser, kval = 100, startvalue = 0, runs = 1, normprob = FALSE)
nstim <- length(unique(df_2AFC_noisy$condID)) #nstim = 45

# Create two new columns to determine the preferred stim (winner) of a pair (vs loser)
# Using the condID_cibleX to include the BKG slope value in one pair

df_2AFC_noisy$winner <- NA
df_2AFC_noisy$loser <- NA

# cond: cible1 (left) & cible2 (right)
for (ndx in 1:length(df_2AFC_noisy$winner)) {
  if (df_2AFC_noisy$click_half[ndx]=="left") {
    df_2AFC_noisy$winner[ndx] <- df_2AFC_noisy$condID_cible1[ndx]
    df_2AFC_noisy$loser[ndx] <- df_2AFC_noisy$condID_cible2[ndx]
  }
  else if (df_2AFC_noisy$click_half[ndx]=="right") {
    df_2AFC_noisy$winner[ndx] <- df_2AFC_noisy$condID_cible2[ndx]
    df_2AFC_noisy$loser[ndx] <- df_2AFC_noisy$condID_cible1[ndx]
  }
}

df_2AFC_noisy$winner_ver <- paste(df_2AFC_noisy$winner, df_2AFC_noisy$stim_ver, sep = ".")
df_2AFC_noisy$loser_ver <- paste(df_2AFC_noisy$loser, df_2AFC_noisy$stim_ver, sep = ".")


# Élő ranking >> for docs, see https://rdrr.io/cran/EloChoice/

set.seed(123)
res0 <- elochoice(df_2AFC_noisy$winner, df_2AFC_noisy$loser, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res0)

myratings <- ratings(res0, show = "original", drawplot = FALSE)
ratings(res0, show = "mean", drawplot = TRUE)

# same but with the 5 stim versions of each condition
set.seed(123)
res0b <- elochoice(df_2AFC_noisy$winner_ver, df_2AFC_noisy$loser_ver, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res0b)

myratings <- ratings(res0b, show = "original", drawplot = FALSE)
ratings(res0b, show = "mean", drawplot = TRUE)

# Plot the ranking per condition
# with 1000 iterations
temp <- res0$ratmat
df <- data.frame(colMeans(temp), apply(temp, 2, sd),  c( -0.25, -0.50, -0.75, -1.25, -1.50, -1.75, -1.25, -1.50, -1.75, -2.25, -2.50, -2.75, -2.25, -2.50, -2.75, -3.25, -3.50, -3.75))
colnames(df) <- c("ranking", "sd", "slope")
df["BKG"] <- as.factor(c(rep(-1,6), rep(-2,6), rep(-3,6)))

ggplot(df, aes(x=as.character(slope), y=ranking, colour = BKG)) + 
  geom_errorbar(aes(ymin=ranking-sd, ymax=ranking+sd), width=.2) +
  geom_line() +
  geom_point() +
  xlab("Target Slope") + ylab("Élő Ratings") +
  ggtitle("Élő Ratings for 2-AFC task with noisy BKG across 1000 iterations (mean +/- SD)")

#stim ver avg
temp2 <- res0b$ratmat
df2 <- data.frame(colMeans(temp2), apply(temp2, 2, sd),
                  c( rep(-0.25,5), rep(-0.50, 5), rep(-0.75, 5), rep(-1.25, 5), rep(-1.50, 5), rep(-1.75, 5), rep(-1.25, 5), rep(-1.50,5), rep(-1.75, 5), rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), rep(-3.25,5), rep(-3.50,5), rep(-3.75,5)))
colnames(df2) <- c("ranking", "sd", "slope")
df2["BKG"] <- as.factor(c(rep(-1,6*5), rep(-2,6*5), rep(-3,6*5)))
df2["condID"] <- paste(df2$BKG, df2$slope, sep= ".")

df2_mean <- df2 %>% 
  group_by(condID) %>%  
  summarise(ranking = mean(ranking), sd = mean(sd), slope = mean(slope))
df2_mean["BKG"] <- as.factor(c(rep(-1,6), rep(-2,6), rep(-3,6)))

ggplot(df2_mean, aes(x=as.character(slope), y=ranking, colour = BKG)) + 
  geom_errorbar(aes(ymin=ranking-sd, ymax=ranking+sd), width=.2) +
  geom_line() +
  geom_point() +
  xlab("Target Slope") + ylab("Élő Ratings") +
  ggtitle("Élő Ratings for 2-AFC task with noisy BKG across 1000 iterations (mean +/- SD)")


# Plot ratings for each slope target for each BKG
bkg_val <- -2
lvl_order <- as.character(c(-0.75, -0.5, -0.25, 0.25, 0.5, 0.75))
  
ggplot(df2_mean[df2_mean$BKG==bkg_val,], aes(x=as.character(slope_diff), y=ranking, colour = BKG)) + 
  geom_errorbar(aes(ymin=ranking-sd, ymax=ranking+sd), width=.2) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits=lvl_order)+
  xlab("Target Slope") + ylab("Élő Ratings") + ylim(-250, 200)  +
  ggtitle("Élő Ratings for 2-AFC task with noisy BKG across 1000 iterations (mean +/- SD)")



# find the peak value of those slopes
df2_mean$slope2 <- df2_mean$slope^2
lm_bkg1 <- lm(ranking~slope + slope2, data = df2_mean[df2_mean$BKG==-1,])
summary(lm_bkg1)

lm_bkg2 <- lm(ranking~slope + slope2, data = df2_mean[df2_mean$BKG==-2,])
summary(lm_bkg2)

lm_bkg3 <- lm(ranking~slope + slope2, data = df2_mean[df2_mean$BKG==-3,])
summary(lm_bkg3)

#create sequence of slope values
slopeValues <- (seq(0, 4, 0.1))*-1
#create list of predicted happines levels using quadratic model
rankingPredict <- predict(lm_bkg2,list(slope=slopeValues, slope2=slopeValues^2))
#create scatterplot of original data values
plot(df[df$BKG==-2,]$slope, df[df$BKG==-2,]$ranking, pch=16)
#add predicted lines based on quadratic regression model
lines(slopeValues, rankingPredict, col='blue')

x = -2.14

# from the quadratic model: ranking = -64.417(slope_target)^2 -247.911(slope_target) - 164.705 
# df2_mean (18 values)
-64.417*((x)^2) -247.911*(x) - 164.705 #bkg-1: slope -1.92, predicted ranking: 73.81729, R2: 0.9993
-143.46*((x)^2) -613.97*(x) - 612.24 #bkg-2: slope -2.14, predicted ranking: 44.65165,  R2: 0.9418
-148.95*((x)^2) -746.32*(x) - 854.97 #bkg-3: slope -2.51, predicted ranking: 79.89331, R2: 0.9943

# #avg stim ver (df2: 90 values)
# -60.567*((x)^2) -241.705*(x) - 163.473 #bkg-1: slope -2.0, predicted ranking: 77.669, R2: 0.959
# -141.89*((x)^2) -606.45*(x) - 603.94 #bkg-2: slope -2.14, predicted ranking: 44.06356, R2: 0.7806
# -143.42*((x)^2) -711.91*(x) - 803.13 #bkg-3: slope -2.48, predicted ranking: 80.31643, R2: 0.9239


df2_mean["RT"] <- medianRT$no_rows
cor(df2_mean$ranking, df2_mean$RT)
cor(df2_mean$ranking, DRperCond$no_rows)

df2_mean$BKGnum <- c(rep(-1,6), rep(-2,6), rep(-3,6))
df2_mean['slope_diff'] <- df2_mean$BKGnum-df$slope

ggplot(df2_mean[df2_mean$BKG==-3,], aes(x=as.character(abs(slope_diff)), y=ranking, colour = BKG)) + 
  geom_errorbar(aes(ymin=ranking-sd, ymax=ranking+sd), width=.2) +
  geom_line() +
  geom_point() +
  xlab("Slope diff") + ylab("Élő Ratings") +
  ggtitle("Élő Ratings for 2-AFC task with noisy BKG across 1000 iterations (mean +/- SD)")


# Plot the average ranking per condition 
# with 1000 iterations and averaged across 5 stim versions
rm(df_elopercondver)
temp <- res0b$ratmat
df_elopercondver <- data.frame(colMeans(temp), apply(temp, 2, sd))
names <- rownames(df_elopercondver)
rownames(df_elopercondver) <- NULL
df_elopercondver <- cbind(names,df_elopercondver)
colnames(df_elopercondver) <- c("condID_ver", "ranking", "sd")

df_elopercondver["stim_ver"] <- substr(df_elopercondver$condID_ver, nchar(df_elopercondver$condID_ver), nchar(df_elopercondver$condID_ver))
df_elopercondver["condID"] <- substr(df_elopercondver$condID_ver, 1, nchar(df_elopercondver$condID_ver)-2)

ggplot(data = df_elopercondver, aes(x = condID, y = ranking)) + geom_boxplot(fill = "grey92", width = .01) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Élő Ratings for 2-AFC task with noisy BKG and their different stimuli") +
  xlab("Condition ID") + ylab("Ranking (Élő score)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

df_elopercondver$BKG <- c(rep(-1, 30), rep(-2, 30), rep(-3, 30))
#version 2: 
ggplot(data=df_elopercondver, aes(x= condID, y=ranking, colour=factor(BKG))) +
  scale_colour_manual(values=cbPalette) +
  geom_boxplot(alpha=0.5, width = .5, outlier.shape = NA) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
  geom_smooth(aes(group=BKG), method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 0.3, linetype = 2) +
  #geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") +
  ggtitle("Élő Ratings for 2-AFC task with noisy BKG and their different stimuli") +
  xlab("Condition ID") + ylab("Ranking (Élő score)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 


mean_elo_noisy <- df_elopercondver %>%
  group_by(condID) %>%
  summarise_at(vars(ranking), list(avg = mean))
mean_elo_noisy["BKG"] <- c(rep(-1,6), rep(-2,6), rep(-3,6))

DRperCond["BKG"] <- c(rep(-1,6), rep(-2,6), rep(-3,6))
medianRT["BKG"] <- c(rep(-1,6), rep(-2,6), rep(-3,6))

cor(df_elopercondver$ranking, DRperCondperVer$no_rows, method = "spearman") #correlation between ranking and detection rate
cor(df_elopercondver$ranking, medianRT_perVer$no_rows, method = "spearman") #correlation between ranking and response time

cor(mean_elo_noisy$avg, DRperCond$no_rows, method = "spearman") #correlation between ranking and detection rate
cor(mean_elo_noisy$avg, medianRT$no_rows, method = "spearman") #correlation between ranking and response time

cor(x=mean_elo_noisy$avg[mean_elo_noisy$BKG==-2], 
    y=DRperCond$no_rows[DRperCond$BKG==-2], method = "spearman")

cor(x=mean_elo_noisy$avg[mean_elo_noisy$BKG==-2], 
    y=medianRT$no_rows[medianRT$BKG==-2], method = "spearman")


# Plot one graph per BKG value > RTs x Elo score

#temp <- res0b$ratmat
#RTelo_noisy <- data.frame(medianRT, t(temp))
#colnames(RTelo_noisy) <- c("condID", "detectionRT", "ranking")
df_elopercondver["BKG"] <- c(rep(-1,6*5), rep(-2,6*5), rep(-3,6*5))
df_elopercondver["slope_target"] <- c( rep(-0.25, 5), rep(-0.50,5), rep(-0.75,5), rep(-1.25,5), rep(-1.50,5), rep(-1.75,5),
                                rep(-1.25,5), rep(-1.50,5), rep(-1.75,5), rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), 
                                rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), rep(-3.25,5), rep(-3.50,5), rep(-3.75,5))
df_elopercondver["detectionRT"] <- medianRT_perVer$no_rows
df_elopercondver["detectionRT_hits"] <- medianRT_perVer_within$no_rows
df_elopercondver['slope_diff'] <- df_elopercondver$BKG-(df_elopercondver$slope_target)
df_elopercondver["detectionRate"] <- DRperCondperVer$no_rows


# ggplot(data=df_elopercondver[df_elopercondver$BKG==-1,], aes(x = log(detectionRT), y = ranking, label=slope_diff, colour=as.factor(slope_diff))) + 
#   geom_point() + #(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
#   ggtitle("Élő Ratings for 2-AFC task with noisy BKG = -1 and the different stimulus versions") +
#   xlab("log(Detection RT)") + ylab("Ranking (Élő score)") +
#   xlim(min(log(df_elopercondver$detectionRT)), max(log(df_elopercondver$detectionRT))) +
#   ylim(min(df_elopercondver$ranking), max(df_elopercondver$ranking)) + 
#   #geom_text(angle=-0, hjust=-0.25, colour='black') +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=0, hjust = 1), plot.title = element_text(hjust = 0.5)) +
#   labs(colour="Slope difference")
# 
# #all together
# ggplot(data=df_elopercondver, aes(x = log(detectionRT), y = ranking, label=slope_target, colour=as.factor(condID))) + 
#   geom_point() + #(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
#   ggtitle("Élő Ratings for 2-AFC task with noisy BKG and the different stimulus versions") +
#   xlab("log(Detection RT)") + ylab("Ranking (Élő score)") +
#   xlim(min(log(df_elopercondver$detectionRT)), max(log(df_elopercondver$detectionRT))) +
#   ylim(min(df_elopercondver$ranking), max(df_elopercondver$ranking)) + 
#   #geom_text(angle=-0, hjust=-0.25, colour='black') +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=0, hjust = 1), plot.title = element_text(hjust = 0.5)) +
#   labs(colour="CondID")

# save the df for the stat models
df_noisy_lmer <- df_elopercondver


# this code below doesnt work well
# ggplot(data=df_elopercondver[df_elopercondver$BKG==1,], aes(y= ranking, x=(detectionRT), group=slope, label=slope)) + 
#   geom_boxplot(fill = "grey92", width = .01) + 
#   geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
#   #geom_text(angle=0, hjust=1.25, vjust=0.1)+ #hjust: 1.25 for BKG=1 and 2 / -.25 for BKG=3
#   geom_text(angle=-0, hjust=-0.15)+
#   ggtitle("Ranking of noisy BKG as a function of RT for the detection task
#   BKG = -1") +
#   ylab("Ranking (Élő score)") + xlab("(Response time (ms))") +
#   ylim(min(df_elopercondver$ranking), max(df_elopercondver$ranking)) + 
#   #xlim(min(df_elopercondver$detectionRT), max(df_elopercondver$detectionRT)) +
#   theme_bw() 

cor(x=df_elopercondver$ranking[df_elopercondver$BKG==-1], 
    y=df_elopercondver$detectionRT[df_elopercondver$BKG==-1], method = "spearman")

cor(x=df_elopercondver$ranking[df_elopercondver$BKG==-1], 
    y=df_elopercondver$detectionRate[df_elopercondver$BKG==-1], method = "spearman")

cor(x=df_elopercondver$ranking, y=df_elopercondver$detectionRT, method = "spearman")
cor(x=df_elopercondver$ranking, y=df_elopercondver$detectionRate, method = "spearman")



# # Compute the average ranking for the target that have a same slope but different background values
# temp3 <- data.frame(t(res0$ratmat))
# colnames(temp3) <- "ranking"
# temp3["slope"] <- c( -0.25, -0.50, -0.75, -1.25, -1.50, -1.75, -1.25, -1.50, -1.75, -2.25, -2.50, -2.75, -2.25, -2.50, -2.75, -3.25, -3.50, -3.75)
# temp4<- temp3 %>% 
#   group_by(slope) %>%  
#   summarise(no_rows = median(ranking)) 
# temp4 <- temp4[order(-temp4$no_rows),] 

# plot(0, 0, xlim = c(1, nrow(temp4)), ylim = range(temp4), 
#      "n", xlab = "Slope value of the target", ylab = "Elo-rating", xaxt='n', yaxt='n')
# points(1:nrow(temp4), t(temp4[,2]), pch = 16)
# axis(1, at = 1:nrow(temp4), labels = FALSE)
# axis(2, las = 1)
# text(x = (1:nrow(temp4))+0.7, y = par("usr")[3], labels = temp4$slope,
#      xpd = NA, srt = 35, adj = 1.6)
# title(main = "Élő Ratings for 2-AFC task with noisy BKG 
#       averaged per target slope")


# >>>> TO DO
# To check for side bias, compare pref for df_2AFC_noisy$cible1 and 
# df_2AFC_noisy$cible2 of a same condition, there should be no difference

# define left and right side 
# set.seed(123)
# resL <- elochoice(df_2AFC_noisy$winner[], df_2AFC_noisy$loser, kval = 100, startvalue = 0, runs = 1, normprob = FALSE)
# summary(resL)
# 
# myratings_left <- ratings(resL, show = "original", drawplot = FALSE)


# how many raters are needed to achieve stability in ratings?
# commented to avoid re-running several times: time consuming
# length(unique(df_2AFC_noisy$ID_juge))
# set.seed(123)
# res3 <- raterprog(df_2AFC_noisy$winner, df_2AFC_noisy$loser, df_2AFC_noisy$ID_juge, progbar = TRUE) ## takes time!
# 
# raterprogplot(res3)
# title(main = "BKG = noisy")



# # Demographics x Rankings -------------------------------------------------
# 
# # calculate the scores while accounting for some participants only
# # Then randomly select the same number of participants from the whole set,
# # calculate the scores, and compare the two distributions.
# 
# # ppp_data3 contains participants demographics
# 
# # Create a sub dataset "target" for targeted population, eg: male participants
# # target_ppp <- ppp_data3$ID_juge[ppp_data3$gender=="F"] # if "M", select all ID_juge of male ppp (majority is female)
# # target_ppp <- ppp_data3$ID_juge[ppp_data3$level_studies=="Assoc_Degree_or_Bachelors_Degree"] # if "Graduate Degree", select all ID_juge of ppp with that level of education (majority)
# # aa <- ppp_data3[ppp_data3$level_studies %in% c("CGE_SSCE", "voc_training_school_certificate ", "High_School"), ]
# # target_ppp <- aa$ID_juge
# # target_ppp <- ppp_data3$ID_juge[ppp_data3$hobby=="no"] # if "no", select all ID_juge of ppp who don't have hobbies involving visual art (majority)
# # aa <- ppp_data3[ppp_data3$hobby %in% c("daily", "often"), ]
# # target_ppp <- aa$ID_juge
# # aa <- ppp_data3[ppp_data3$freqexpo %in% c("yearly", "never"), ]# if "yearly", select all ID_juge of ppp who go to visual exhibition once a year (majority)
# # aa <- ppp_data3[ppp_data3$freqexpo %in% c("weekly", "monthly"), ]
# # target_ppp <- aa$ID_juge
# # target_ppp <- ppp_data3$ID_juge[ppp_data3$colorblindness=="yes"] # if "yes", select all ID_juge of colourblind ppp (minority)
# # target_ppp <- ppp_data3$ID_juge[ppp_data3$year <=1970]
# aa <- ppp_data3[ppp_data3$year <1995 & ppp_data3$year > 1970,]
# target_ppp <- aa$ID_juge
# 
# 
# target_ppp <- target_ppp[!is.na(target_ppp)]
# 
# bkg_subset = "gray" # "noisy" or "gray"
# 
# df_2AFC_target <- sub_res_data[sub_res_data$ID_juge %in% target_ppp,]
# df_2AFC_target <- subset(df_2AFC_target, BKG_type==bkg_subset & rt != 'NULL')
# 
# # Create a sub dataset "random" to randomly select ppp and match the number of targeted ppp
# random_sample <- sample.int(n = nrow(ppp_data3), size = length(target_ppp), replace = FALSE) #randomly select ppp
# random_sample <- unique(sub_res_data$ID_juge)[random_sample]
# 
# df_2AFC_random <- sub_res_data[sub_res_data$ID_juge %in% random_sample,]
# df_2AFC_random <- subset(df_2AFC_random, BKG_type==bkg_subset & rt != 'NULL')
# 
# 
# # create a uniqueID per cond
# # dataset #1
# df_target <- data.frame(df_2AFC_target$cible1, df_2AFC_target$cible2, df_2AFC_target$stim_pente_BKG, df_2AFC_target$stim_ver)
# df_random <- data.frame(df_2AFC_random$cible1, df_2AFC_random$cible2, df_2AFC_random$stim_pente_BKG, df_2AFC_random$stim_ver)
# 
# df_interact1 <- df_target %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_target.cible1, df_2AFC_target.cible2, df_2AFC_target.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_target["condID"] <- df_interact1$interact
# 
# df_interact1 <- df_target %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_target.cible1, df_2AFC_target.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_target["condID_cible1"] <- df_interact1$interact
# 
# df_interact1 <- df_target %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_target.cible2, df_2AFC_target.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_target["condID_cible2"] <- df_interact1$interact
# 
# df_interact2 <- df_target %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_target.cible1, df_2AFC_target.cible2, df_2AFC_target.stim_pente_BKG, df_2AFC_target.stim_ver)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_target["condID_ver"] <- df_interact2$interact
# 
# df_2AFC_target <- df_2AFC_target %>%
#   mutate(experiment_order = if_else(trialnb <= 20, 1, 2))
# 
# # dataset #2
# df_interact1 <- df_random %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_random.cible1, df_2AFC_random.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_random["condID_cible1"] <- df_interact1$interact
# 
# df_interact1 <- df_random %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_random.cible2, df_2AFC_random.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_random["condID_cible2"] <- df_interact1$interact
# 
# df_interact2 <- df_random %>%
#   mutate_if(is.factor, as.character) %>%
#   rowwise() %>%
#   mutate(interact = sort(c(df_2AFC_random.cible1, df_2AFC_random.cible2, df_2AFC_random.stim_pente_BKG, df_2AFC_random.stim_ver)) %>% paste(., collapse = ".")) %>%
#   ungroup()
# df_2AFC_random["condID_ver"] <- df_interact2$interact
# 
# df_2AFC_random <- df_2AFC_random %>%
#   mutate(experiment_order = if_else(trialnb <= 20, 1, 2))
# 
# # formula: elochoice(winner, loser, kval = 100, startvalue = 0, runs = 1, normprob = FALSE)
# 
# # Create two new columns to determine the preferred stim (winner) of a pair (vs loser)
# # Using the condID_cibleX to include the BKG slope value in one pair
# 
# #dataset #1
# df_2AFC_target$winner <- NA
# df_2AFC_target$loser <- NA
# 
# # cond: cible1 (left) & cible2 (right)
# for (ndx in 1:length(df_2AFC_target$winner)) {
#   if (df_2AFC_target$click_half[ndx]=="left") {
#     df_2AFC_target$winner[ndx] <- df_2AFC_target$condID_cible1[ndx]
#     df_2AFC_target$loser[ndx] <- df_2AFC_target$condID_cible2[ndx]
#   }
#   else if (df_2AFC_target$click_half[ndx]=="right") {
#     df_2AFC_target$winner[ndx] <- df_2AFC_target$condID_cible2[ndx]
#     df_2AFC_target$loser[ndx] <- df_2AFC_target$condID_cible1[ndx]
#   }
# }
# 
# df_2AFC_target$winner_ver <- paste(df_2AFC_target$winner, df_2AFC_target$stim_ver, sep = ".")
# df_2AFC_target$loser_ver <- paste(df_2AFC_target$loser, df_2AFC_target$stim_ver, sep = ".")
# 
# 
# # dataset #2
# df_2AFC_random$winner <- NA
# df_2AFC_random$loser <- NA
# 
# # cond: cible1 (left) & cible2 (right)
# for (ndx in 1:length(df_2AFC_random$winner)) {
#   if (df_2AFC_random$click_half[ndx]=="left") {
#     df_2AFC_random$winner[ndx] <- df_2AFC_random$condID_cible1[ndx]
#     df_2AFC_random$loser[ndx] <- df_2AFC_random$condID_cible2[ndx]
#   }
#   else if (df_2AFC_random$click_half[ndx]=="right") {
#     df_2AFC_random$winner[ndx] <- df_2AFC_random$condID_cible2[ndx]
#     df_2AFC_random$loser[ndx] <- df_2AFC_random$condID_cible1[ndx]
#   }
# }
# 
# df_2AFC_random$winner_ver <- paste(df_2AFC_random$winner, df_2AFC_random$stim_ver, sep = ".")
# df_2AFC_random$loser_ver <- paste(df_2AFC_random$loser, df_2AFC_random$stim_ver, sep = ".")
# 
# 
# 
# # Élő ranking - dataset #1
# # with the 5 stim versions of each condition
# set.seed(123)
# res0b <- elochoice(df_2AFC_target$winner_ver, df_2AFC_target$loser_ver, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
# summary(res0b)
# myratings <- ratings(res0b, show = "original", drawplot = FALSE)
# ratings(res0b, show = "mean", drawplot = TRUE)
# 
# # Élő ranking - dataset #2
# # with the 5 stim versions of each condition
# set.seed(123)
# res1b <- elochoice(df_2AFC_random$winner_ver, df_2AFC_random$loser_ver, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
# summary(res1b)
# myratings <- ratings(res1b, show = "original", drawplot = FALSE)
# ratings(res1b, show = "mean", drawplot = TRUE)
# 
# 
# #Compare rankings for both BKG types
# # Ratings for both BKG types - 1000 iterations & 5 versions
# target_ranking <- data.frame(res0b$ratmat)
# target_ranking <- data.frame(colMeans(target_ranking), apply(target_ranking, 2, sd))
# colnames(target_ranking) <- c("ranking", "sd")
# 
# ks.test(target_ranking$ranking, "pnorm", mean=mean(target_ranking$ranking), sd=sd(target_ranking$ranking))
# 
# random_ranking <- data.frame(res1b$ratmat)
# random_ranking <- data.frame(colMeans(random_ranking), apply(random_ranking, 2, sd))
# colnames(random_ranking) <- c("ranking", "sd")
# 
# ks.test(random_ranking$ranking, "pnorm", mean=mean(random_ranking$ranking), sd=sd(random_ranking$ranking))
# 
# t.test(target_ranking$ranking, random_ranking$ranking)
# 

#############################-

# Compare rankings for both BKG types -------------------------------------
# Plot ratings for both BKG types on the same graph - 1000 iterations & 5 versions averaged--------------
tmp_gray <- data.frame(res1$ratmat)
tmp_gray <- data.frame(colMeans(tmp_gray), apply(tmp_gray, 2, sd))
colnames(tmp_gray) <- c("ranking", "sd")
tmp_gray["slope"] <- c( -0.25, -0.50, -0.75, -1.25, -1.50, -1.75, -2.25, -2.50, -2.75, -3.25, -3.50, -3.75)

tmp_noisy <- data.frame(res0$ratmat)
tmp_noisy <- data.frame(colMeans(tmp_noisy), apply(tmp_noisy, 2, sd))
colnames(tmp_noisy) <- c("ranking", "sd")
tmp_noisy["slope"] <- c( -0.25, -0.50, -0.75, -1.25, -1.50, -1.75, -1.25, -1.50, -1.75, -2.25, -2.50, -2.75, -2.25, -2.50, -2.75, -3.25, -3.50, -3.75)

# Combine both data frames 
data_all <- data.frame(rbind(tmp_noisy, tmp_gray),
                         BKG = c(rep("noisy", nrow(tmp_noisy)), rep("gray", nrow(tmp_gray))))

# Draw combined data frame
ggplot(data_all, aes(y=ranking,x=as.character(slope), colour=factor(BKG))) +             
  geom_point(size=2) +
 # scale_x_reverse() +
  ggtitle("Élő rating for both types of BKG") +
  scale_colour_discrete(name="BKG") +
  scale_x_discrete( breaks=as.character(data_all$slope),
                   labels=as.character(data_all$slope)) +
  #scale_x_continuous(breaks = rev(tmp_gray$slope)) +
  xlab("Slope value of the target") + ylab("Élő Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 
  

# Spearman (rank cor): for each BKG > elo ranking grey vs elo ranking noisy (for each noisy BKG value separately)
# Modèle linéaire avec terme quadratique plutôt que rank corrélation > MSE? Adj R²
# Plot: noisy BKG en ordinates (effet simple + effet quadratique) & gray BKG en abscisse

tmp_gray_doubledup <- rbind(tmp_gray[1:6,], tmp_gray[4:6,], tmp_gray[7:9,], tmp_gray[7:12,])
df_cor <- cbind(tmp_gray_doubledup, tmp_noisy, c(rep(-1,6), rep(-2,6), rep(-3,6)))
colnames(df_cor) <- c("ranking_G", "sd_G", "slope_G", "ranking_N", "sd_N", "slope_N", "BKG")

# save the data for the stats
df_gray_lmer <- tmp_gray_doubledup


lm_cor <- lm(ranking_N~ranking_G, data = df_cor) #all BKG together
lm_cor <- lm(ranking_N[df_cor$BKG=="-2"]~ranking_G[df_cor$BKG=="-2"], data = df_cor) #each BKG separately
summary(lm_cor)

df_cor$ranking_G2 <- df_cor$ranking_G^2
df_cor$ranking_N2 <- df_cor$ranking_N^2

lm_cor2 <- lm(ranking_N~ranking_G + ranking_G2, data = df_cor)
lm_cor2 <- lm(ranking_N[df_cor$BKG=="-2"]~ranking_G[df_cor$BKG=="-2"] + ranking_G2[df_cor$BKG=="-2"], data = df_cor)
summary(lm_cor2)

anova(lm_cor, lm_cor2)

#create sequence of values to predict
rankGValues <- seq(-150, 120, 0.1)

#create list of predicted ranking for noisy BKG using quadratic model
rankNPredict <- predict(lm_cor2,list(ranking_G=rankGValues, ranking_G2=rankGValues^2))

#create scatterplot of original data values
plot(df_cor$ranking_G, df_cor$ranking_N, pch=16)
#add predicted lines based on quadratic regression model
lines(rankGValues, rankNPredict, col='blue')
title('Linear + Quadratic relationship between 
      noisy BKG rankings and gray BKG rankings')

#create scatterplot of original data values
plot(df_cor$ranking_G, df_cor$ranking_N, pch=16)
#add predicted lines based on linear regression model
abline(a=0, b=1, col='blue')
title('Linear relationship between 
      noisy BKG rankings and gray BKG rankings')


# Plot ratings for both BKG types on the same graph - 1000 iterations & 5 versions separately --------
temp <- res1b$ratmat
tmp_gray <- data.frame(colMeans(temp), apply(temp, 2, sd))
names <- rownames(tmp_gray)
rownames(tmp_gray) <- NULL
tmp_gray <- cbind(names,tmp_gray)
colnames(tmp_gray) <- c("condID_ver", "ranking", "sd")
tmp_gray["stim_ver"] <- substr(tmp_gray$condID_ver, nchar(tmp_gray$condID_ver), nchar(tmp_gray$condID_ver))
tmp_gray["condID"] <- substr(tmp_gray$condID_ver, 1, nchar(tmp_gray$condID_ver)-2)
tmp_gray["slope"] <- as.numeric(tmp_gray$condID)

temp <- res0b$ratmat
tmp_noisy <- data.frame(colMeans(temp), apply(temp, 2, sd))
names <- rownames(tmp_noisy)
rownames(tmp_noisy) <- NULL
tmp_noisy <- cbind(names,tmp_noisy)
colnames(tmp_noisy) <- c("condID_ver", "ranking", "sd")
tmp_noisy["stim_ver"] <- substr(tmp_noisy$condID_ver, nchar(tmp_noisy$condID_ver), nchar(tmp_noisy$condID_ver))
tmp_noisy["condID"] <- substr(tmp_noisy$condID_ver, 1, nchar(tmp_noisy$condID_ver)-2)
tmp_noisy["slope"] <- c( rep(-0.25, 5), rep(-0.50,5), rep(-0.75,5), rep(-1.25,5), rep(-1.50,5), rep(-1.75,5),
                         rep(-1.25,5), rep(-1.50,5), rep(-1.75,5), rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), 
                         rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), rep(-3.25,5), rep(-3.50,5), rep(-3.75,5))
  
# Combine both data frames 
data_all <- data.frame(rbind(tmp_noisy, tmp_gray),
                       BKG = c(rep("noisy", nrow(tmp_noisy)), rep("gray", nrow(tmp_gray))))

# Draw combined data frame
ggplot(data_all, aes(y=ranking,x=as.character(slope), colour=factor(BKG))) + 
  geom_boxplot(fill = "grey92", width = .01) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
  # scale_x_reverse() +
  ggtitle("Élő rating for both types of BKG - 5 stimulus versions") +
  scale_colour_discrete(name="BKG") +
  scale_x_discrete( breaks=as.character(data_all$slope),
                    labels=as.character(data_all$slope)) +
  #scale_x_continuous(breaks = rev(tmp_gray$slope)) +
  xlab("Slope value of the target") + ylab("Élő Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 


# # Spearman (rank cor): for each BKG > elo ranking grey vs elo ranking noisy (for each noisy BKG value separately)
# # Modèle linéaire avec terme quadratique plutôt que rank corrélation > MSE? Adj R²
# # Plot: noisy BKG en ordinates (effet simple + effet quadratique) & gray BKG en abscisse
# tmp_gray_doubledup <- rbind(tmp_gray[1:30,], tmp_gray[16:30,], tmp_gray[31:45,], tmp_gray[31:60,])
# df_cor <- cbind(tmp_gray_doubledup, tmp_noisy, c(rep(-1,30), rep(-2,30), rep(-3,30)))
# colnames(df_cor) <- c("condID_ver_G", "ranking_G", "sd_G", "stim_ver_G", "condID_G", "slope_G",
#                       "condID_ver_N", "ranking_N", "sd_N", "stim_ver_N", "condID_N", "slope_N", "BKG")
# 
# lm_cor <- lm(ranking_N~ranking_G, data = df_cor) #all BKG together
# lm_cor <- lm(ranking_N[df_cor$BKG=="-2"]~ranking_G[df_cor$BKG=="-2"], data = df_cor) #each BKG separately
# summary(lm_cor)
# 
# df_cor$ranking_G2 <- df_cor$ranking_G^2
# df_cor$ranking_N2 <- df_cor$ranking_N^2
# 
# lm_cor2 <- lm(ranking_N~ranking_G + ranking_G2, data = df_cor)
# lm_cor2 <- lm(ranking_N[df_cor$BKG=="-2"]~ranking_G[df_cor$BKG=="-2"] + ranking_G2[df_cor$BKG=="-2"], data = df_cor)
# summary(lm_cor2)
# 
# anova(lm_cor, lm_cor2)

# #create sequence of values to predict
# rankGValues <- seq(-150, 120, 0.1)
#
# #create list of predicted ranking for noisy BKG using quadratic model
# rankNPredict <- predict(lm_cor2,list(ranking_G=rankGValues, ranking_G2=rankGValues^2))
# 
# #create scatterplot of original data values
# plot(df_cor$ranking_G, df_cor$ranking_N, pch=16)
# #add predicted lines based on quadratic regression model
# lines(rankGValues, rankNPredict, col='blue')
# title('Linear + Quadratic relationship between 
#       noisy BKG rankings and gray BKG rankings')
# 
# #create scatterplot of original data values
# plot(df_cor$ranking_G, df_cor$ranking_N, pch=16)
# #add predicted lines based on linear regression model
# abline(a=0, b=1, col='blue')
# title('Linear relationship between 
#       noisy BKG rankings and gray BKG rankings')

# Statistical analyses ----------------------------------------------------

#tuto: http://www.let.rug.nl/wieling/Statistics/Mixed-Effects/Mixed-Effects.pdf


# Detection task ----------------------------------------------------------
# RT ~ stim_penteBKG * cible1 + trialnb + target_pos + experiment_order + (1|ID_juge) + (1|cible1) 

# !! lmer assumes a Gaussian distrib! >  Inverse Gaussian distribution: glmer; family=inverse.gaussian(link=invfn())

# invfn <- function() {
#   ## link
#   linkfun <- function(y) -1000/y
#   ## inverse link
#   linkinv <- function(eta)  -1000/eta
#   ## derivative of invlink wrt eta
#   mu.eta <- function(eta) { 1000/(eta^2) }
#   valideta <- function(eta) TRUE
#   link <- "-1000/y"
#   structure(list(linkfun = linkfun, linkinv = linkinv,
#                  mu.eta = mu.eta, valideta = valideta, 
#                  name = link),
#             class = "link-glm")
# }

# lm0_DT_invGauss <- glmer(rt~stim_pente_BKG * cible1 + slopeBKG_cible1 + trialnb + target_pos 
#                + experiment_order + (1|ID_juge) + (1|cible1) , 
#                data = df_detection_task, family=inverse.gaussian(link=invfn())) #doesn't work

df_detection_task$slope_target <- as.numeric(df_detection_task$slope_target)

#gamma models
 
lm0_DT_gamma <- glmer(rt~BKG * slope_target + slope_diff + trialnb + target_pos 
                + experiment_order + (1|ID_juge) + (1|slope_target) , 
                data = df_detection_task, family=Gamma(link="identity")) #fails to converge

lm1_DT_gamma <- glmer(rt~BKG + slope_target + abs(slope_diff) + trialnb + target_pos 
                      + experiment_order + (1|ID_juge) + (1|slope_target) , 
                      data = df_detection_task, family=Gamma(link="identity")) #fails to converge

lm_simple_DT_gamma <- glmer(rt~abs(slope_diff) + (1|ID_juge), 
                            data=df_detection_task, family = Gamma(link = "identity"))

lm_lvlup_DT_gamma <- glmer(rt~abs(slope_diff) + trialnb + target_pos + experiment_order + (1|ID_juge), 
                           data=df_detection_task, family = Gamma(link = "identity"))

lm_lvlup3_DT_gamma <- glmer(rt~abs(slope_diff) + trialnb + target_pos + experiment_order + (1|ID_juge) + (1|BKG), 
                           data=df_detection_task, family = Gamma(link = "identity"))
#Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GHrule(0L), compDev = compDev,  : PIRLS loop resulted in NaN value

lm_lvlup2_DT_gamma <- glmer(rt~slope_diff + trialnb + target_pos + experiment_order + (1|ID_juge), 
                            data=df_detection_task, family = Gamma(link = "identity"))

lm_cplx_DT_gamma <- glmer(rt~slope_target + abs(slope_diff) + trialnb + target_pos + experiment_order + (1|ID_juge), 
                           data=df_detection_task, family = Gamma(link = "identity"))

############-############-############-############-

# Detection task_updated models -------------------------------------------

lm_cplx1_DT_gamma <- glmer(rt~slope_target + slope_diff + BKG + trialnb + target_pos + experiment_order + (1|ID_juge), 
                          data=df_detection_task, family = Gamma(link = "identity"))

summary(lm_cplx1_DT_gamma)
#adding BKG doesn't change the output of the model: this variable is dropped given its collinearty with slope_target

lm_cplx1_DT_gamma_within <- glmer(rt~slope_target + slope_diff + BKG + trialnb + target_pos + experiment_order + (1|ID_juge), 
                           data=df_detection_task[df_detection_task$within_target==TRUE,], family = Gamma(link = "identity"))
#only hit trials
summary(lm_cplx1_DT_gamma_within)

lm_cplx1_DT_gamma_diff <- glmer(rt~slope_target + slope_diff_value*slope_diff_sign + BKG 
                                + trialnb + target_pos + experiment_order + (1|ID_juge), 
                           data=df_detection_task, family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_diff)#model fails to converge

plot(y=df_detection_task$rt, x=df_detection_task$experiment_order)

summ(lm_cplx1_DT_gamma)
effect_plot(lm_cplx1_DT_gamma, pred = slope_target, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)
plot_summs(lm_cplx1_DT_gamma) 


lm_cplx2_DT_gamma <- glmer(rt~ slope_target + slope_diff + BKG + trialnb + target_pos + experiment_order 
                           + (1|ID_juge) + (1|slope_target), 
                           data=df_detection_task, family = Gamma(link = "identity"))
#doesn't run when (1|experiment_order), (1|target_pos), (1|trialnb), (1|slope_diff)
summary(lm_cplx2_DT_gamma)

lm_cplx2_DT_gamma_within <- glmer(rt~ slope_target + slope_diff + BKG + trialnb + target_pos + experiment_order 
                           + (1|ID_juge) + (1|slope_target), 
                           data=df_detection_task[df_detection_task$within_target==TRUE,], family = Gamma(link = "identity"))
#doesn't run when (1|experiment_order), (1|target_pos), (1|trialnb), (1|slope_diff)
summary(lm_cplx2_DT_gamma_within)

#same model as lm_cplx1_DT_gamma but for each BKG separately
lm_cplx1_DT_gamma_BKG1 <- glmer(rt~slope_target + slope_diff_value + slope_diff_sign + trialnb + target_pos + experiment_order + 
                                  (1|ID_juge), 
                           data=df_detection_task[df_detection_task$BKG==-1,], 
                           family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_BKG1)

lm_cplx1_DT_gamma_BKG1_within <- glmer(rt~slope_target + slope_diff_value + slope_diff_sign + trialnb + target_pos + experiment_order + 
                                  (1|ID_juge), 
                                data=df_detection_task[df_detection_task$within_target==TRUE & df_detection_task$BKG==-1,], 
                                family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_BKG1_within) #model fails to converge

lm_cplx1_DT_gamma_BKG2 <- glmer(rt~slope_target + slope_diff_value + slope_diff_sign + trialnb + target_pos + experiment_order + 
                                  (1|ID_juge), 
                                data=df_detection_task[df_detection_task$BKG==-2,], 
                                family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_BKG2)

lm_cplx1_DT_gamma_BKG2_within <- glmer(rt~slope_target + slope_diff_value + slope_diff_sign + trialnb + target_pos + experiment_order + 
                                  (1|ID_juge), 
                                data=df_detection_task[df_detection_task$within_target==TRUE & df_detection_task$BKG==-2,], 
                                family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_BKG2_within)

lm_cplx1_DT_gamma_BKG3 <- glmer(rt~slope_target + slope_diff_value + slope_diff_sign + trialnb + target_pos + experiment_order + 
                                  (1|ID_juge), 
                                data=df_detection_task[df_detection_task$BKG==-3,], 
                                family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_BKG3)

lm_cplx1_DT_gamma_BKG3_within <- glmer(rt~slope_target + slope_diff_value + slope_diff_sign + trialnb + target_pos + experiment_order + 
                                         (1|ID_juge), 
                                       data=df_detection_task[df_detection_task$within_target==TRUE & df_detection_task$BKG==-3,], 
                                       family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma_BKG3_within)
############-############-############-############-

lm_inv_gaussian <- glmer(rt~slope_target + slope_diff + trialnb + target_pos + experiment_order + (1|ID_juge), 
                         data=df_detection_task, family = inverse.gaussian(link = "identity")) #doesn't converge

lm_cplx2_DT_gamma <- glmer(rt~slope_target + abs(slope_diff) + trialnb + target_pos + experiment_order + (1|ID_juge) + (1|slope_target), 
                          data=df_detection_task, family = Gamma(link = "identity")) #doesn't converge


lm_cplx1_DT_gamma_factors <- glmer(rt~as.factor(slope_target) + as.factor(slope_diff) + trialnb + target_pos + experiment_order + (1|ID_juge), 
                           data=df_detection_task, family = Gamma(link = "identity")) #doesn't converge


summary(lm0_DT_gamma)
summary(lm1_DT_gamma)
summary(lm_cplx_DT_gamma)
summary(lm_cplx1_DT_gamma)
summary(lm_simple_DT_gamma)
summary(lm_lvlup2_DT_gamma)

summary(lm0_DT_gamma)$coef
summary(lm0_DT_gamma)$varcor

boxplot(df_detection_task$rt, df_detection_task$slope_diff)

residuals <- resid(lm_cplx1_DT_gamma) #extract the residuals (errors) and summarize them
summary(residuals)
hist(residuals)

plot(fitted(lm_cplx1_DT_gamma), resid(lm_cplx1_DT_gamma, type = "pearson"))# this will create the plot
abline(0,0, col="red")

qqnorm(resid(lm_cplx1_DT_gamma)) 
qqline(resid(lm_cplx1_DT_gamma), col = "red") # add a perfect fit line

# check random slopes for inter participants variability
lm0_DT <- lmer(rt~BKG * slope_target + BKG*slope_diff + trialnb + target_pos 
              + experiment_order + (1|ID_juge) + (1|slope_target) , data = df_detection_task) 

lm3_DT <- lmer(rt~BKG * slope_target + BKG*abs(slope_diff) + trialnb + target_pos 
               + experiment_order + (1|ID_juge) + (1|slope_target) , data = df_detection_task) 


summary(lm3_DT)$coef
summary(lm3_DT)$varcor
ranef(lm3_DT)

anova(lm0_DT_gamma, lm0_DT)

lm1_DT<- lmer(rt~BKG + slope_target + slope_diff + trialnb + target_pos 
              + experiment_order + (1|ID_juge) + (1|slope_target) , data = df_detection_task) #converges

summary(lm1_DT)$coef #fixed effects
summary(lm1_DT)$varcor #random effects

anova(lm0_DT, lm1_DT) #interaction needed
anova(lm0_DT, lm3_DT) #abs(slope_diff) gives a better fit than slope_diff
#report(lm0_DT)

#####################################################-

false_alarms <- df_detection_task[df_detection_task$within_target==FALSE,]
false_alarms %>% 
  group_by(BKG) %>%
  summarise(no_rows = length(BKG))

# Detection Task - stim_ver ----------------------------------

#1/ create a subdataset that only includes hits (excluding 8,198 false alarms, 23%):
df_detection_task_hits <- df_detection_task[df_detection_task$within_target==TRUE,] %>% 
  group_by(condID_ver) %>%  
  summarise(RT = mean(rt), slope_target = mean(slope_target), 
            stim_ver = mean(as.numeric(stim_ver)), BKG = mean(BKG))

df_detection_task_hits['slope_diff'] <- df_detection_task_hits$BKG-(df_detection_task_hits$slope_target)

df_detection_task_hits['slope_diff_sign'] <- df_detection_task_hits$slope_diff/df_detection_task_hits$slope_diff
df_detection_task_hits$slope_diff_sign[which(df_detection_task_hits$slope_diff<=0)] = -1
df_detection_task_hits['slope_diff_value'] <- abs(df_detection_task_hits$slope_diff)
df_detection_task_hits['detectionRate'] <- DRperCondperVer$no_rows
df_detection_task_hits['condID'] <- substr(df_detection_task_hits$condID_ver,1, 8)

#2/ Models on the global df_detection_task_hits data
#DETECTION TIMES as the dependent variable
lm_cplx1_DT_gamma <- glmer(RT~slope_target + slope_diff_value*slope_diff_sign + BKG + stim_ver
                           + (1|BKG) + (1|stim_ver) + (1|slope_target) + (1|slope_diff_sign), 
                           data=df_detection_task_hits, family = Gamma(link = "identity"))
summary(lm_cplx1_DT_gamma) #doesn't converge with  (1|slope_diff_value)


#DETECTION RATE as the dependent variable
lm_cplx1_DT <- lmer(detectionRate~slope_target + slope_diff_value*slope_diff_sign + BKG + stim_ver
                           + (1|BKG) + (1|stim_ver) + (1|slope_target) + (1|slope_diff_value)+ (1|slope_diff_sign), 
                           data=df_detection_task_hits, REML = FALSE)
summary(lm_cplx1_DT)


lm_cplx1_DT_simpler <- lmer(detectionRate~slope_target + slope_diff_value+slope_diff_sign + BKG + stim_ver
                    + (1|BKG) + (1|slope_target), 
                    data=df_detection_task_hits, REML = FALSE)
summary(lm_cplx1_DT_simpler)

#3/ Separate models for each BKG
bkg_value <- -1

#DETECTION TIMES as the dependent variable
lm_cplx_DT_gamma <- glmer(RT~slope_target + slope_diff_value*slope_diff_sign + stim_ver
                           + (1|stim_ver) + (1|slope_target) + (1|slope_diff_value)+ (1|slope_diff_sign), 
                           data=df_detection_task_hits[df_detection_task_hits$BKG==bkg_value,], family = Gamma(link = "identity"))

summary(lm_cplx_DT_gamma)

#var <- 1.494e+04 / (1.494e+04 + 3.244e-02 + 4.821e-04 + 3.419e-04 + 4.555e-02)

lm_cplx_DT_gamma_BKG12 <- glmer(RT~BKG+slope_target + slope_diff_value*slope_diff_sign + stim_ver
                          + (1|stim_ver) + (1|slope_target) + (1|slope_diff_value), 
                          data=df_detection_task_hits[df_detection_task_hits$BKG!=-3,], 
                          family = Gamma(link = "identity"))
#model doesn't converge if (1|slope_sign) + (1|slope_target) + (1|slope_diff_value)
summary(lm_cplx_DT_gamma_BKG12)


#DETECTION RATE as the dependent variable
lm_cplx_DT <- lmer(detectionRate~slope_target + slope_diff_value*slope_diff_sign + stim_ver
                    + (1|slope_target) + (1|slope_diff_value), 
                    data=df_detection_task_hits[df_detection_task_hits$BKG==bkg_value,], REML = FALSE)
summary(lm_cplx_DT)
#(1|slope_diff_sign) and #(1|stim_ver) explained 0% variance and was removed


lm_cplx_DT_BKG12 <- lmer(detectionRate~BKG+slope_target + slope_diff_value*slope_diff_sign + stim_ver
                   + (1|stim_ver) + (1|slope_target) , 
                   data=df_detection_task_hits[df_detection_task_hits$BKG!=-3,], REML = FALSE)
#(1|slope_diff_sign) explained 0% variance and was removed
summary(lm_cplx_DT_BKG12)


#plot the output of the model
plot_model(lm_cplx_DT, show.values = TRUE, sort = TRUE) #all fixed effects
plot_model(lm_cplx1_DT_gamma, type = "pred", terms = "slope_diff_sign") #one predictor of interest
plot_model(lm_cplx1_DT_gamma, type = "pred", terms = c("slope_target", "slope_diff_sign"))

ggplot(data=df_detection_task_hits[df_detection_task_hits$BKG==-2,], aes(x= condID, y=detectionRate)) + geom_point() +
  geom_boxplot(aes(group = condID)) +
  ggtitle("Detection rate (%) for BKG -2") +
  xlab("Slope diff sign") + ylab("Detection rate (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 


ggplot(data=df_detection_task_hits, aes(x= condID, y=RT)) + geom_point() +
  geom_hline(yintercept=mean(df_detection_task_hits$RT), linetype="dashed", color = "red") + 
  geom_text(aes(1, mean(RT), label = paste("mean =", round(mean(RT), digits = 2)), vjust = - 1, hjust = -5.25), col = "red") +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection time (ms) for the different conditions") +
  xlab("Condition ID") + ylab("Detection time (ms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 


# GrayBKG - some models ---------------------------------------------------

df_gray <- tmp_gray
df_gray <- rename(df_gray, slope_target = slope)

lm_grayBKG <- lmer(ranking~slope_target + (1|slope_target), data = df_gray)
summary(lm_grayBKG)
#stim_ver was removed as it explained 0% of the variance

lm_grayBKG_simpler <- lm(ranking~slope_target, data = df_gray)
summary(lm_grayBKG_simpler)

#quadratic model
df_gray$slope_target2 <- df_gray$slope_target^2

quadraticModel <- lm(ranking~slope_target + slope_target2, data=df_gray)
summary(quadraticModel)

#create sequence of slope values
slopeValues <- (seq(0, 4, 0.1))*-1

#create list of predicted happines levels using quadratic model
rankingPredict <- predict(quadraticModel,list(slope_target=slopeValues, slope_target2=slopeValues^2))

#create scatterplot of original data values
plot(df_gray$slope_target, df_gray$ranking, pch=16)
#add predicted lines based on quadratic regression model
lines(slopeValues, rankingPredict, col='blue')


# from the quadratic model: ranking = -52.158(slope_target)^2 -246.169(slope_target) - 216.335 
x <- c(-2.36)
-52.158*((x)^2) -246.169*(x) - 216.335


# NoisyBKG - New attempt with recoding slope variables -------------------------------

# > recoding of slope diff into 2 variables:
#slope_diff_value: 0.25, 0.5, 0.75 
#slope_diff_sign: 1, -1 (pos vs neg) 

df_noisy_lmer['slope_diff_sign'] <- df_noisy_lmer$slope_diff/df_noisy_lmer$slope_diff
df_noisy_lmer$slope_diff_sign[which(df_noisy_lmer$slope_diff<=0)] = -1

df_noisy_lmer['slope_diff_value'] <- abs(df_noisy_lmer$slope_diff)

lm_noisyBKG <- lmer(ranking~ slope_diff_value+slope_diff_sign + BKG*slope_target +I(BKG^2) 
                    + (1|BKG) + (1|slope_target) + (1|stim_ver),
                    data = df_noisy_lmer, REML = FALSE)

lm_noisyBKG <- lmer(ranking~ slope_diff_value+slope_diff_sign + BKG*slope_target +I(BKG^2) 
                    + (1|slope_target), data = df_noisy_lmer, REML = FALSE) #without null random effects
summary(lm_noisyBKG)

# estimate contribution > partial R² [partR2 pkg]
# https://cran.r-project.org/web/packages/partR2/vignettes/Using_partR2.html
R2_lm_noisyBKG_1 <- partR2(lm_noisyBKG, data = df_noisy_lmer, R2_type = "marginal", nboot = 10)

R2_lm_noisyBKG_2 <- partR2(lm_noisyBKG, partvars = c("slope_diff_value", "slope_diff_sign","BKG", 
                                                     "slope_target", "BKG:slope_target"), 
                           R2_type = "marginal", nboot = 10)
R2_lm_noisyBKG_2 <- partR2(lm_noisyBKG, partvars = c("slope_diff_value", "slope_target","BKG"), 
                           R2_type = "marginal", nboot = 10)
summary(R2_lm_noisyBKG_2)

library(patchwork)
p1 <- forestplot(R2_lm_noisyBKG_2, type = "R2", text_size = 10)
p2 <- forestplot(R2_lm_noisyBKG_2, type = "IR2", text_size = 10)
p3 <- forestplot(R2_lm_noisyBKG_2, type = "SC", text_size = 10)
p4 <- forestplot(R2_lm_noisyBKG_2, type = "BW", text_size = 10)
(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

R2_BMb <- R2_lm_noisyBKG_2
str(R2_BMb, max.level = 1)

# (a) point estimates and confidence intervals
R2_BMb$R2   # R2s
R2_BMb$SC   # Structure coefficients
R2_BMb$IR2  # inclusive R2s
R2_BMb$BW # Standardised model estimates
R2_BMb$Ests # Model estimates
# (b) bootstrap replicates
R2_BMb$R2_boot
R2_BMb$SC_boot
R2_BMb$IR2_boot
R2_BMb$BW_boot
R2_BMb$Ests_boot



library(r2glmm) #https://github.com/bcjaeger/r2glmm
(r2.lm_noisyBKG = r2beta(lm_noisyBKG, method = 'kr', partial = T, data = df_noisy_lmer)) #standardized measure of multivariate association between the fixed predictors and the observed outcome
(r2m1 = r2beta(model=lm_noisyBKG,method='sgv',partial=FALSE)) #proportion of generalized variance explained by the fixed predictors
(r2nsj = r2beta(lm_noisyBKG, method = 'nsj', partial = TRUE)) #proportion of variance explained by the fixed predictors

library(nlme)

r2nsj_mum = MuMIn::r.squaredGLMM(lm_noisyBKG)
all.equal(r2nsj[1,'Rsq'],as.numeric(r2nsj_mum[1]), tolerance = 1e-3)



# for each BKG separately
bkg_val = -3
lm_noisyBKG_bkg_sep <- lmer(ranking~ slope_diff_value + slope_diff_sign + slope_target + (1|stim_ver) ,
                    data = df_noisy_lmer[df_noisy_lmer$BKG==bkg_val,])
summary(lm_noisyBKG_bkg_sep)

lm_noisyBKG_bkg_sep <- lmer(ranking~ slope_diff_value*slope_diff_sign + (1|stim_ver) ,
                            data = df_noisy_lmer[df_noisy_lmer$BKG==bkg_val,])
summary(lm_noisyBKG_bkg_sep)


# Restrict the analysis to overlapping slope target values
# ie: between -1.25 and -2.75

lm_noisyBKG_overlap <- lmer(ranking~BKG + slope_diff_value*slope_diff_sign + slope_target +I(BKG^2) + (1|BKG),
                    data = df_noisy_lmer[df_noisy_lmer$slope_target<=-1.25 & df_noisy_lmer$slope_target>=-2.75,])
summary(lm_noisyBKG_overlap) # (1|stim_ver) removed 0% variance explained

lm_noisyBKG_overlap2 <- lmer(ranking~BKG + slope_diff + slope_target +I(BKG^2) + (1|BKG) + (1|stim_ver),
                            data = df_noisy_lmer[df_noisy_lmer$slope_target<=-1.25 & df_noisy_lmer$slope_target>=-2.75,])
summary(lm_noisyBKG_overlap2) # if (1|stim_ver) removed model doesn't converge


#same but separately for BKG -1 and -2 together and BKG -2 and -3 together:
#(1|stim_ver) explained 0% variance and was removed > lm models 
lm_noisyBKG_overlap_BKG1_2 <- lm(ranking~BKG + slope_diff + slope_target,
                             data = df_noisy_lmer[df_noisy_lmer$slope_target<=-1.25 & df_noisy_lmer$slope_target>=-2.75 & df_noisy_lmer$BKG!=-3,])
summary(lm_noisyBKG_overlap_BKG1_2)

lm_noisyBKG_overlap_BKG2_3 <- lm(ranking~BKG + slope_diff + slope_target,
                                   data = df_noisy_lmer[df_noisy_lmer$slope_target<=-1.25 & df_noisy_lmer$slope_target>=-2.75 & df_noisy_lmer$BKG!=-1,])
summary(lm_noisyBKG_overlap_BKG2_3)

#slope_target = NA > colinearity with BKG (about -0.9 correlation in previous models using lmer)

#same but with the recoding of slope_diff
#same but separately for BKG -1 and -2 together and BKG -2 and -3 together:
#(1|stim_ver) explained 0% variance and was removed > lm models 
# slope_diff_value:slope_diff_sign = NA > colinearity > interaction removed
lm_noisyBKG_overlap_BKG1_2_reco <- lm(ranking~BKG + slope_diff_value+slope_diff_sign + slope_target,
                                   data = df_noisy_lmer[df_noisy_lmer$slope_target<=-1.25 & df_noisy_lmer$slope_target>=-2.75 & df_noisy_lmer$BKG!=-3,])
summary(lm_noisyBKG_overlap_BKG1_2_reco)

lm_noisyBKG_overlap_BKG2_3_reco <- lm(ranking~BKG + slope_diff_value+slope_diff_sign + slope_target,
                                   data = df_noisy_lmer[df_noisy_lmer$slope_target<=-1.25 & df_noisy_lmer$slope_target>=-2.75 & df_noisy_lmer$BKG!=-1,])
summary(lm_noisyBKG_overlap_BKG2_3_reco)

# Model 2AFC noisy  --------
# elo noisy explained by the different stim properties, indep of DT and gray BKG

# elo_noisy ~ slope_background * slope_target (+ slope_background + slope_target) + slope_background^2 + 
#   slope_target^2 + slope_difference + (1|slope_background) + (1|slope_difference) + (1|slope_target)

#df_noisy_lmer["abs_slope_diff"] <- abs(df_noisy_lmer$slope_diff)

lmfull <- lmer(ranking~BKG*slope_target + I(BKG^2) + I(slope_target^2) + slope_diff + (1|BKG) + (1|slope_diff) + 
                 (1|slope_target) + (1|stim_ver), data=df_noisy_lmer) 
#fails to converge with 1 neg eigenvalue

#lmfull_bis <- lmer(ranking~BKG*slope_target + I(BKG^2) + abs_slope_diff + slope_diff + (1|BKG) + (1|slope_diff) + 
#                 (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver), data=df_noisy_lmer) #fails to converge with 3 neg eigenvalues


lmfull2 <- lmer(ranking~BKG + slope_target + I(BKG^2) + slope_diff + (1|slope_diff) + 
                 (1|slope_target) , data=df_noisy_lmer) 
summary(lmfull2) #(1|BKG) + (1|stim_ver) explained ~0% variance > removed

plot(y=df_noisy_lmer$ranking, x=df_noisy_lmer$BKG)

summ(lmfull2)
effect_plot(lmfull2, pred = BKG, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05, data=df_noisy_lmer)
plot_summs(lmfull2) 

####--- DOUBLE CHECKED UNTIL HERE --

# lmfullWoI <- lmer(ranking~BKG+slope + I(BKG^2) + I(slope^2) + slope_diff + (1|BKG) + (1|slope_diff) + (1|slope) + (1|stim_ver), data=df_noisy_lmer)
# anova(lmfull, lmfullWoI) #interaction necessary

# boundary (singular) fit: see help('isSingular') = random effects are very small
summary(lmfull2)
fixef(lmfull) #extract fixed effects estimates
ranef(lmfull) #extract random effects estimates
coef(lmfull) #extract coefficients for the random effects intercept and each gp of random effect factors
summary(lmfull)$varcor

yhat <- fitted(lmfull) #extract the fitted or predicted values based on the model parameters and data
summary(yhat)

residuals <- resid(lmfull) #extract the residuals (errors) and summarize them
summary(residuals)
hist(residuals)

1.9355e+01  / (1.1896e+01 + 1.9355e+01 + 8.4353e-16 + 3.9265e-16 + .1061e+01) #var explained by slope
#var explained by slope: 0.37; by slope_diff: 0.6

#intra Class Correlation: assess whether or not the random effect is present in the data
lm_null <- lmer(ranking~ 1 + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver), data=df_noisy_lmer, REML = TRUE)
summary(lm_null) #fails to converge
#total variance estimate:
6.692e+03 + 6.920e+01 + 2.103e-30 + 4.556e-31 + 1.311e+03
#slope/total variance
6.692e+03/8072.2 #ICC = 0.83

aov1 <- aov(ranking~slope_target, data = df_noisy_lmer)
summary(aov1)

ICC1(aov1)
ICC2(aov1)

### ------- Main stat model ----
# preference_score_noisy~detection_time+preference_score_grey(doubled up)+slope_background*slope_difference+
        # slope_target+trial_number+experiment_order+target_side+(1|subject_ID)+(1|slope_target)

# elo_noisy ~ detection_time + (elo_gray) + slope_background * slope_difference 
#      + trial_number + experiment_order + target_side + (1|subject_ID) + (1|slope_target)

# elo_gray ~ slope_target > quadratic effect

#tuto: https://www.rensvandeschoot.com/tutorials/lme4/ 

medianRT_gray_trials <- df_2AFC_gray %>% 
  group_by(winner_ver) %>%  
  summarise(no_rows = median(rt))

medianRT_noisy_trials <- df_2AFC_noisy %>% 
  group_by(winner_ver) %>%  
  summarise(no_rows = median(rt))

# Plot the detection time (ms) as a function of condition ID, including stim_ver variability
medianRT_gray_trials["stim_ver"] <- substr(medianRT_gray_trials$winner_ver, nchar(medianRT_gray_trials$winner_ver), nchar(medianRT_gray_trials$winner_ver))
medianRT_gray_trials["condID"] <- substr(medianRT_gray_trials$winner_ver, 1, nchar(medianRT_gray_trials$winner_ver)-2)

medianRT_noisy_trials["stim_ver"] <- substr(medianRT_noisy_trials$winner_ver, nchar(medianRT_noisy_trials$winner_ver), nchar(medianRT_noisy_trials$winner_ver))
medianRT_noisy_trials["condID"] <- substr(medianRT_noisy_trials$winner_ver, 1, nchar(medianRT_noisy_trials$winner_ver)-2)


medianRT_gray_trials %>%
  group_by(condID) %>%
  summarise_at(vars(no_rows), list(name = sd))

ggplot(data=medianRT_gray_trials, aes(x= condID, y=no_rows)) + geom_boxplot(fill = "grey92", width = .1) + 
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .1)) +
  geom_hline(yintercept=median(medianRT$no_rows), linetype="dashed", color = "red") + 
  #geom_text(aes(1, median(no_rows), label = paste("median =", round(median(no_rows), digits = 2)), vjust = - 1, hjust = -5.25), col = "red") +
  #geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") + 
  ggtitle("Detection time (ms) for the different conditions and their different stimuli") +
  xlab("Condition ID") + ylab("Detection time (ms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) 





# dataframes of reference: df_noisy_lmer & df_gray_lmer
df_big_model <- df_noisy_lmer
df_big_model["ranking_gray"] <- df_gray_lmer$ranking
df_big_model["ranking_sd_gray"] <- df_gray_lmer$sd
df_big_model <- rename(df_big_model, c(ranking_noisy = ranking, ranking_sd_noisy = sd))
#df_big_model["abs_slope_diff"] <- abs(df_big_model$slope_diff)

df_big_model['detectionRate'] <- df_elopercondver$detectionRate

df_big_model['slope_diff_sign'] <- df_big_model$slope_diff/df_big_model$slope_diff
df_big_model$slope_diff_sign[which(df_big_model$slope_diff<=0)] = -1
df_big_model['slope_diff_value'] <- abs(df_big_model$slope_diff)

#correlation between slope_diff_value/slope_diff_sign and detection time/rate
#5 stim versions not averaged

cor(df_big_model$ranking_noisy, df_big_model$detectionRT, method = "spearman")
cor(df_big_model$ranking_noisy, df_big_model$detectionRate, method = "spearman")

cor(df_big_model$slope_diff_value, df_big_model$detectionRT, method = "spearman")
cor(df_big_model$slope_diff_sign, df_big_model$detectionRT, method = "spearman")
cor(df_big_model$slope_diff_value, df_big_model$detectionRate, method = "spearman")
cor(df_big_model$slope_diff_sign, df_big_model$detectionRate, method = "spearman")

BKG_value <- -3
cor(df_big_model$slope_diff_value[df_big_model$BKG==BKG_value], 
    df_big_model$detectionRT[df_big_model$BKG==BKG_value], method = "spearman")
cor(df_big_model$slope_diff_sign[df_big_model$BKG==BKG_value], 
    df_big_model$detectionRT[df_big_model$BKG==BKG_value], method = "spearman")
cor(df_big_model$slope_diff_value[df_big_model$BKG==BKG_value], 
    df_big_model$detectionRate[df_big_model$BKG==BKG_value], method = "spearman")
cor(df_big_model$slope_diff_sign[df_big_model$BKG==BKG_value], 
    df_big_model$detectionRate[df_big_model$BKG==BKG_value], method = "spearman")

# #averaged across the stimuli versions > Not a good idea
# df_big_model_avg5ver <- df_big_model %>% 
#   group_by(condID) %>%  
#   summarise(rt = mean(detectionRT), dRate = mean(detectionRate), 
#             diff_value = mean(slope_diff_value), diff_sign = mean(slope_diff_sign),
#             bkg = mean(BKG))
# 
# cor(df_big_model_avg5ver$diff_value, df_big_model_avg5ver$rt, method = "spearman")
# cor(df_big_model_avg5ver$diff_sign, df_big_model_avg5ver$rt, method = "spearman")
# cor(df_big_model_avg5ver$diff_value, df_big_model_avg5ver$dRate, method = "spearman")
# cor(df_big_model_avg5ver$diff_sign, df_big_model_avg5ver$dRate, method = "spearman")
# 
# BKG_value <- -3
# cor(df_big_model_avg5ver$diff_value[df_big_model_avg5ver$bkg==BKG_value], 
#     df_big_model_avg5ver$rt[df_big_model_avg5ver$bkg==BKG_value], method = "spearman")
# cor(df_big_model_avg5ver$diff_sign[df_big_model_avg5ver$bkg==BKG_value], 
#     df_big_model_avg5ver$rt[df_big_model_avg5ver$bkg==BKG_value], method = "spearman")
# cor(df_big_model_avg5ver$diff_value[df_big_model_avg5ver$bkg==BKG_value], 
#     df_big_model_avg5ver$dRate[df_big_model_avg5ver$bkg==BKG_value], method = "spearman")
# cor(df_big_model_avg5ver$diff_sign[df_big_model_avg5ver$bkg==BKG_value], 
#     df_big_model_avg5ver$dRate[df_big_model_avg5ver$bkg==BKG_value], method = "spearman")




ggplot(df_big_model, aes(x=ranking_noisy)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")


# ranking_noisy ~ detectionRT + ranking_gray + BKG * slope_target + slope_diff
# + (1|BKG) + (1|slope_diff) + (1|slope_target)

big_model <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_target + slope_diff 
                  + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                  data = df_big_model)

# big_model_bis <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_target 
#                       + abs(slope_diff) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
#                   data = df_big_model)

big_model2 <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_target 
                   + BKG*slope_diff + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                  data = df_big_model) #doesnt converge

# big_model2_bis <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_target + BKG*abs_slope_diff + (1|BKG) + (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver),
#                    data = df_big_model)

big_model3 <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_target 
                   + BKG*slope_diff + I(BKG^2) + I(slope_target^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                   data = df_big_model) #doesnt converge

# big_model3_bis <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_target + BKG*abs_slope_diff + I(BKG^2) + I(slope_target^2) + (1|BKG) + (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver),
#                    data = df_big_model)

big_model_simpler <- lmer(formula = ranking_noisy ~ 1+ detectionRT + ranking_gray + BKG*slope_target 
                          + slope_diff + (1|stim_ver),
                   data = df_big_model)

# big_model_simpler_bis <- lmer(formula = ranking_noisy ~ 1+ detectionRT + ranking_gray + BKG*slope_target + abs_slope_diff + (1|stim_ver),
#                           data = df_big_model)

summary(big_model_simpler)
summ(big_model_simpler)
ranova(big_model_simpler)
simple_slopes(big_model_simpler)
#graph_model(big_model_simpler, y = ranking_noisy, x = slope_target, lines = BKG)
df_big_model$BKGf <- as.factor(df_big_model$BKG)

big_model4 <- lmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff 
                   + I(BKG^2) +I(slope_target^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                   data = df_big_model)

big_model4_bis <- lmer(formula = ranking_noisy ~ detectionRT*abs_slope_diff + ranking_gray + BKG*slope_target 
                    + BKG*abs_slope_diff + I(BKG^2) +I(slope_target^2) + (1|BKG) + (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver),
                   data = df_big_model)

big_model4bis <- lmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff 
                    + I(BKG^2) +  BKG*I(slope_target^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                   data = df_big_model)

big_model4bis2 <- lmer(formula = ranking_noisy ~ detectionRT*abs_slope_diff + ranking_gray + BKG*slope_target + BKG*abs_slope_diff 
                      + I(BKG^2) +  BKG*I(slope_target^2) + (1|BKG) + (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver),
                      data = df_big_model)

big_model4ter <- lmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff 
                      + I(BKG^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                      data = df_big_model)

big_model4ter2 <- lmer(formula = ranking_noisy ~ detectionRT*abs_slope_diff + ranking_gray + BKG*slope_target + BKG*abs_slope_diff 
                      + I(BKG^2) + (1|BKG) + (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver),
                      data = df_big_model)

big_model5 <- lmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff + I(BKG^2) 
                   + I(slope_target^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                   data = df_big_model[df_big_model$BKG!=-2,])

big_model5bis <- lmer(formula = ranking_noisy ~ detectionRT*abs_slope_diff + ranking_gray + BKG*slope_target + BKG*abs_slope_diff + I(BKG^2) 
                   +  (1|BKG) + (1|abs_slope_diff) + (1|slope_target) + (1|stim_ver),
                   data = df_big_model[df_big_model$BKG!=-3,])



cor(x=df_big_model$ranking_gray, y=df_big_model$slope_target) #cor = -0.12
cor(x=df_big_model$slope_diff, y=df_big_model$slope_target) #cor = -0.55
cor(x=df_big_model$slope_diff, y=df_big_model$slope_target) #cor = 0.53
cor(x=df_big_model$slope_diff, y=abs(df_big_model$slope_diff)) #cor = 0



# Real deal with statistical models ---------------------------------------
big_model4bis <- lmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target 
                      + BKG*slope_diff + I(BKG^2) +  BKG*I(slope_target^2) + (1|BKG) + (1|slope_diff) 
                      + (1|slope_target) + (1|stim_ver),  data = df_big_model)
# (1|slope_diff), (1|BKG), (1|slope_target) & (1|stim_ver) removed because explained 0% of the variance

big_model4bis_sqless <- lmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray 
                             + BKG*slope_target + BKG*slope_diff + I(BKG^2) + (1|slope_diff) 
                             + (1|slope_target), data = df_big_model)
summary(big_model4bis_sqless)

big_model4bis_recoded <- lmer(formula = ranking_noisy ~ detectionRT + slope_diff_value + slope_diff_sign 
                              + ranking_gray + BKG*slope_target + I(BKG^2) 
                              + (1|slope_diff_sign), data = df_big_model) #, REML = FALSE
summary(big_model4bis_recoded)

big_model4bis_simpler0 <- lm(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray 
                             + BKG*slope_target + BKG*slope_diff 
                      + I(BKG^2) +  BKG*I(slope_target^2) , data = df_big_model) 
alias(big_model4bis_simpler0)
library(car)
vif(big_model4bis_simpler, type = "predictor")

big_model4bis_simpler1 <- lm(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target 
                            + I(BKG^2) +  BKG*I(slope_target^2) , data = df_big_model) 
# BKG*slope_diff removed: "Coefficients: (1 not defined because of singularities)"
# alias(lm) reveals that slope-target is linearly dependent on BKG (positively) and slope_diff (neg)

big_model4bis_simpler1bis <- lm(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG 
                            + I(BKG^2) +  BKG*I(slope_target^2) , data = df_big_model) 

summary(big_model4bis_simpler1bis)

#we keep BKG*slope_diff because both variables have high beta and t values in the models above and so their interaction is likely important
big_model4bis_simpler2 <- lm(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_diff 
                            + I(BKG^2) +  BKG*I(slope_target^2) , data = df_big_model) 

summary(big_model4bis_simpler2)

#without the non-sig interaction detectionRT*slope_diff
big_model4bis_simpler3 <- lm(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_diff 
                             + I(BKG^2) +  BKG*I(slope_target^2) , data = df_big_model) 
summary(big_model4bis_simpler3)

big_model4bis_simpler3bis <- lm(formula = ranking_noisy ~ detectionRT + ranking_gray + BKG*slope_diff 
                             + I(BKG^2) , data = df_big_model) 
summary(big_model4bis_simpler3bis)

anova(big_model4bis_simpler2, big_model4bis_simpler3) #no diff

plot(y=df_detection_task$rt, x=df_detection_task$experiment_order)

summ(big_model4bis_simpler3)
effect_plot(big_model4bis_simpler3, pred = slope_diff, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)
plot_summs(big_model4bis_simpler3) 


interact_plot(big_model4bis_simpler3, modx = BKG, pred = slope_diff,interval = TRUE, plot.points = TRUE, 
              jitter = 0.05, data = df_big_model)


# WINNER MODEL: 
lm_noisy_julien_p54 <- lmer(formula = ranking_noisy ~ slope_diff_value + slope_diff_sign 
                            + ranking_gray + BKG*slope_target + I(BKG^2)  
                            + (1|slope_diff_value) + (1|slope_target), data = df_big_model)
summary(lm_noisy_julien_p54) #doesn't converge if (1|slope_diff_sign) or (1|BKG) are included

#for each BKG sep
bkg_val= -3

lm_noisy_julien_p54_BKGsep <- lmer(formula = ranking_noisy ~ slope_diff_value + slope_diff_sign 
                            + ranking_gray + slope_target + (1|slope_diff_value) + (1|slope_target), 
                            data = df_big_model[df_big_model$BKG==bkg_val,])
summary(lm_noisy_julien_p54_BKGsep)


lm_noisy_julien_p38 <- lmer(formula = ranking_noisy ~ slope_diff_value + slope_diff_sign 
                            + ranking_gray + BKG * slope_target + I(BKG^2)  
                            + (1|slope_diff_value) + (1|slope_target), data = df_big_model)
summary(lm_noisy_julien_p38) #doesn't converge if (1|slope_diff_sign) or (1|BKG) are included

lm_noisy_julien_p38bis <- lmer(formula = ranking_noisy ~ slope_diff_value + slope_diff_sign 
                             + BKG + slope_target + I(BKG^2)  +ranking_gray
                            + (1|slope_diff_value) + (1|slope_target), data = df_big_model)
summary(lm_noisy_julien_p38bis) #doesn't converge if (1|slope_diff_sign) or (1|BKG) are included

#for each BKG separately
bkg_val = -3
lm_noisy_julien_p38bis_BKGsep <- lmer(formula = ranking_noisy ~ slope_diff_value + slope_diff_sign 
                               + slope_target + (1|slope_diff_value) + (1|slope_target), 
                               data = df_big_model[df_big_model$BKG==bkg_val,])
summary(lm_noisy_julien_p38bis_BKGsep) #doesn't converge if (1|slope_diff_sign) or (1|BKG) are included



# with detectionRate

big_model4bis_simpler3_Drate <- lm(formula = ranking_noisy ~ detectionRate + ranking_gray + BKG*slope_diff 
                                   + I(BKG^2) +  BKG*I(slope_target^2) , data = df_big_model)
summary(big_model4bis_simpler3_Drate)


big_model4bis_DRate <- lm(formula = ranking_noisy ~ detectionRate + ranking_gray + BKG*slope_diff 
                          + I(BKG^2) +  BKG*I(slope_target^2),
                          data = df_big_model)
summary(big_model4bis_DRate)


big_model4bis_DRate_recoded <- lmer(formula = ranking_noisy ~ detectionRate + slope_diff_value 
                                    + slope_diff_sign 
                              + ranking_gray + BKG*slope_target + I(BKG^2) 
                              + (1|slope_diff_sign) + (1|slope_diff_value) + (1|slope_target) 
                              , data = df_big_model) #, REML = FALSE
summary(big_model4bis_DRate_recoded)
# won't converge without (1|slope_diff_value) 

model_julien_p54 <- lmer(formula = ranking_noisy ~ detectionRate + ranking_gray + BKG*slope_target
                         + I(BKG^2) + (1|slope_target), data = df_big_model)
summary(model_julien_p54)

# same-ish but for each BKG separately:
bkg_val = -3

model_julien_p54_BKGsep <- lmer(formula = ranking_noisy ~ detectionRate + ranking_gray + BKG*slope_target
                         + I(BKG^2) + (1|slope_target), data = df_big_model[df_big_model$BKG==bkg_val,])
summary(model_julien_p54_BKGsep)

# hits only

model_julien_hits_p54 <- lmer(formula = ranking_noisy ~ detectionRT_hits + ranking_gray + 
                                BKG*slope_target + I(BKG^2) + (1|slope_target),
                              data = df_big_model)
summary(model_julien_hits_p54)

# for each BKG sep
bkg_val = -3

model_julien_hits_p54_BKGsep <- lmer(formula = ranking_noisy ~ detectionRT_hits + ranking_gray + 
                                slope_target + (1|slope_target), 
                                data = df_big_model[df_big_model$BKG==bkg_val,])
summary(model_julien_hits_p54_BKGsep)


lmer_hits <- lmer(formula = ranking_noisy ~ detectionRT_hits + slope_diff_value + slope_diff_sign 
                  + ranking_gray + BKG*slope_target + I(BKG^2) + (1|slope_diff_sign), 
                  data = df_big_model)
summary(lmer_hits)

######################################################"


summ(big_model4bis)
ranova(big_model4bis)
simple_slopes(big_model4bis)
#graph_model(big_model4bis, y = ranking_noisy, x = slope_target, lines = BKG)



summary(big_model)$coef
summary(big_model)$varcor

fixef(big_model) #extract fixed effects estimates
ranef(big_model) #extract random effects estimates
coef(big_model) #extract coefficients for the random effects intercept and each gp of random effect factors

yhat <- fitted(big_model) #extract the fitted or predicted values based on the model parameters and data
summary(yhat)

residuals <- resid(big_model) #extract the residuals (errors) and summarize them
summary(residuals)
hist(residuals)

plot(fitted(big_model), resid(big_model, type = "pearson"))# this will create the plot
abline(0,0, col="red")

qqnorm(resid(big_model)) 
qqline(resid(big_model), col = "red") # add a perfect fit line

# spaMM
library(spaMM)
fitme(SOPincl~paired_stim*Focal_individual_sex*BKG_cat,
      family=gaussian(), data = df_big_model)

big_model_spaMM <- fitme(ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff 
                      + I(BKG^2) +  BKG*I(slope_target^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                      family=gaussian(), data = df_big_model)

big_model_spaMM_2 <- fitme(ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff 
                         + I(BKG^2) +  BKG*I(slope_target^2),
                         family=gaussian(), data = df_big_model)


#log normal distrib?

big_model4bis <- glmer(formula = ranking_noisy ~ detectionRT*slope_diff + ranking_gray + BKG*slope_target + BKG*slope_diff 
                      + I(BKG^2) +  BKG*I(slope_target^2) + (1|BKG) + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                      family = lognormal , data = df_big_model)

# General model for each BKG separately ------------------------------------
bkg_val <- -1
big_model6 <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + slope_target + slope_diff + I(slope_target^2) 
                   + (1|slope_diff) + (1|slope_target) + (1|stim_ver),
                       data = df_big_model[df_big_model$BKG==bkg_val,])
summary(big_model6)


plot(y = df_big_model[df_big_model$BKG==bkg_val,]$ranking_noisy, 
     x = df_big_model[df_big_model$BKG==bkg_val,]$detectionRT)

plot_model(big_model6, show.values = TRUE, sort = TRUE) #all fixed effects
plot_model(big_model6, type = "pred", terms = "detectionRate") #one predictor of interest


#slope_diff recoded in slope_diff_value and slope_diff_sign
big_model6 <- lmer(formula = ranking_noisy ~ detectionRT + ranking_gray + slope_target + slope_diff_sign + slope_diff_value
                   + I(slope_target^2) + (1|slope_target)  + (1|stim_ver),
                   data = df_big_model[df_big_model$BKG==bkg_val,])
summary(big_model6)

big_model6_within <- lmer(formula = ranking_noisy ~ detectionRT + slope_diff_value + slope_diff_sign 
                          + ranking_gray + BKG*slope_target + I(BKG^2) + (1|slope_diff_sign),
                   data = df_big_model[df_big_model$within_target==TRUE & df_big_model$BKG==bkg_val,]) #+ I(slope_target^2)
summary(big_model6_within)

lmer(formula = ranking_noisy ~ detectionRT + slope_diff_value + slope_diff_sign 
     + ranking_gray + BKG*slope_target + I(BKG^2) + (1|slope_diff_sign))




# PKG to test -------------------------------------------------------------

# ggPredict() - Visualize multiple regression model: https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
# sjPlot - Data Visualization for Statistics in Social Science: https://strengejacke.github.io/sjPlot/index.html
# glmTMB - fitting generalized linear mixed models (GLMMs) and extensions: https://glmmtmb.github.io/glmmTMB/
# report: https://easystats.github.io/report/ 

# library(sjstats)

# ongoing analyses/discarded/archived --------------------------------------------------------

## archived
# psi.plot doesn't add much more than ggplot line 236
#Function to compute bernoulli means and CIs for plotting
# bernoulli_mean_ci <- function(x) {
#   conf = prop.test(sum(as.logical(x)), length(x))$conf.int
#   data.frame(y = mean(x), ymin = conf[1], ymax = conf[2])
# }
# 
# psi.plot <- (
#   ggplot(df_detection_task)
#   + aes(x=condID, y=as.numeric(within_target))
#   + stat_summary(fun.data=bernoulli_mean_ci, geom="pointrange")
#   + labs(x="Condition", y="Proportion detected")
#   + geom_hline(yintercept=0.5) + geom_vline(xintercept = 0)
#   + geom_smooth(method="glm", formula=y~x-1,
#                 family=binomial(link=logit)))
# print(psi.plot)

# temp <- res0b$ratmat
# temp2 <- apply(temp, 2, range, na.rm = TRUE)
# temp2 <- temp2[, rev(order(temp2[1, ]))]
# plot(0, 0, xlim = c(1, ncol(temp2)), ylim = range(temp), 
#      "n", xlab = "Condition", ylab = "Elo-rating", xaxt='n', yaxt='n')
# points(1:ncol(temp2), temp2[1, ], pch = 16)
# axis(1, at = 1:ncol(temp2), labels = FALSE)
# axis(2, las = 1)
# text(x = (1:ncol(temp2))+0.7, y = par("usr")[3], labels = colnames(temp2),
#      xpd = NA, srt = 35, adj = 1.6)
# title(main = "Élő Ratings for 2-AFC task with noisy BKG")

# # Spearman correlation between the Élő ratings of avg-noisy and gray BKG
# temp5 <- temp4[order(-temp4$slope),] 
# 
# shapiro.test(temp5$ranking)
# shapiro.test(tmp_gray$ranking)
# cor.test(x=temp5$ranking, y=tmp_gray$ranking, method = "spearman")
# 
# GD_dta <- data_frame(temp5$ranking, tmp_gray$ranking)
# ggscatter(data=GD_dta, y='temp5$ranking', x='tmp_gray$ranking', 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "spearman",
#           ylab = "ranking gray BKG", xlab = "ranking avg-noisy BKG")


# # Spearman correlation between the Élő ratings of noisy and doubled-up gray BKG
# shapiro.test(tmp_noisy$ranking) 
# 
# tmp_gray_doubledup <- rbind(tmp_gray[1:6,], tmp_gray[4:6,], tmp_gray[7:12,], tmp_gray[10:12,])
# shapiro.test(tmp_gray_doubledup$ranking)
# cor.test(x=tmp_noisy$ranking, y=tmp_gray_doubledup$ranking, method = "spearman")
# 
# GD_dta <- data_frame(tmp_noisy$ranking, tmp_gray_doubledup$ranking)
# ggscatter(data=GD_dta, y='tmp_noisy$ranking', x='tmp_gray_doubledup$ranking', 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "spearman",
#           ylab = "ranking doubled-up gray BKG", xlab = "ranking noisy BKG")

# 
# tmp_gray_doubledup <- cbind(tmp_gray_doubledup, RTelo_noisy$detectionRT)
# colnames(tmp_gray_doubledup)[3] = "detectionRT"
# 
# ggplot(data=tmp_gray_doubledup, aes(y= ranking, x=detectionRT, label=slope)) + geom_point() + 
#   #geom_text(angle=0, hjust=1.25, vjust=-0.1)+ #hjust: 1.25 for BKG=1 and 2 / -.25 for BKG=3
#   geom_text(angle=0, hjust=1, vjust=1.4)+
#   ggtitle("Ranking of gray BKG as a function of RT for the detection task") +
#   ylab("Ranking (Élő score)") + xlab("Response time (ms)") +
#   ylim(min(tmp_gray_doubledup$ranking), max(tmp_gray_doubledup$ranking)) + 
#   xlim(min(tmp_gray_doubledup$detectionRT), max(tmp_gray_doubledup$detectionRT)) +
#   theme_bw() 
# 
# cor(x=tmp_gray_doubledup$ranking, y=tmp_gray_doubledup$detectionRT)
# 


# how many raters are needed to achieve stability in ratings?
length(unique(df_elo_gray$IDrater))
set.seed(123)
res1 <- raterprog(df_elo_gray$winner, df_elo_gray$loser, df_elo_gray$IDrater, progbar = TRUE) ## takes time!

#xdata <- write.table(res1, "/home/yseult/Documents/CamoStudy/ExperimentData/res1", sep = "\t")

raterprogplot(res1)
title(main = "how many raters are needed to achieve stability in ratings?")

# It may be advisable, however, to not necessarily rely on the original sequence of raters involved. 
# In the general spirit of the package, we can randomize the order with which raters are included. 
set.seed(123)
res2 <- raterprog(df_elo_gray$winner, df_elo_gray$loser, df_elo_gray$IDrater, progbar = TRUE, ratershuffle = 10) ## takes much more time!


res_total <- res2

set.seed(123)
#res_total <- elochoice(df_elo$winner, df_elo$loser, df_elo$IDrater, runs = 500)
res_total <- elochoice(df_elo$winner, df_elo$loser, kval = 100, startvalue = 0, runs = 500)
summary(res_total)
ratings(res_total, show = "mean", drawplot = FALSE)

par(mar = c(4.1, 4.1, 0.5, 0.5), family = "serif")
ratings(res_total, show = NULL, drawplot = TRUE)
