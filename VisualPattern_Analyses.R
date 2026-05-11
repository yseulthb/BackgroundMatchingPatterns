# Script written by Yseult Héjja-Brichard with input from Innes Cuthill for statistical analyses of Task 3
# Latest update: May 2026
# Data analysis - Visual Pattern preferences

# Load needed packages
library(dplyr)
library(stringr)
library(countrycode)
library(ggplot2)
library(RColorBrewer)
library(EloChoice)
library(lme4)
library(sjPlot)
library(sjstats)
library(DescTools)
library(car)
library(optimx)
library(lmerTest)

# 1/ Initialisation ----------------------------------------------------------

rm(list=ls()) #clean work space
setwd("/YourFolder/data/")

## Load and organize the data
# There are 2 different data files: participants_info.csv with information on participants 
# and results_data.csv that contains the response data 
# (latest date of data acquisition: September 8th, 2022)

res_data <- read.csv("results_data.csv", header = TRUE)
ppp_data <- read.csv("participants_info.csv", header = TRUE)

# We will start with the participant data "ppp_data"

# Remove participants that didn't complete the experiment, using the last 
# questions i.e. for whom columns "hobby" or "freqexpo" include the "NULL" value
ppp_data2<-ppp_data[!(ppp_data$freqexpo=="NULL" | ppp_data$hobby=="NULL"),]

# Rename some columns and replace values to have everything in English
ppp_data2 <- rename(ppp_data2, c(gender = sex, year = annee, month = mois , colorblindness = daltonisme ))
ppp_data2$gender = str_replace_all(ppp_data2$gender,"H","M")

# Check how many participants did the experiment more than once ("IP_vierge = non" means more than 1 attempt)
ppp_data2 %>% 
  group_by(IP_vierge) %>%
  summarise(no_rows = length(IP_vierge))

# It might still be necessary to manually check participants who failed their first attempt  
# due to connection issues/see the comment section and keep their second attempt only
double_attempt <- ppp_data2[!(ppp_data2$IP_vierge=="oui"),]

# Remove participants that did the experiment more than once "IP_vierge == non"
ppp_data3 <- ppp_data2[!(ppp_data2$IP_vierge=="non"),]

# Compare participants number
ppp_ID <- unique(ppp_data3$ID_juge)
res_ID <- unique(res_data$ID_juge)

# Keep ID_juge that is found in both data sets
shared_ID <- intersect(ppp_ID, res_ID) 
# the number of shared_ID should be equal (or slighlty less) to the number of participants kept in ppp_data3

sub_res_data <- res_data[res_data$ID_juge %in% shared_ID, ]
sub_res_ID <- unique(sub_res_data$ID_juge)

ppp_data3 <- ppp_data3[ppp_data3$ID_juge %in% sub_res_ID, ] #only keeps participants that did the experiment once

# 2/ Participant data ---------------------------------------------------------
# Explore the demographics of participants (ppp_data3)
# 1. Self-reported Gender
ppp_data3 %>% 
  group_by(gender) %>%
  summarise(no_rows = length(gender))
# -> more than half of the participants identify as female (60%)

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
# -> only 35 participants self-identify as color-blind

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
# the code below will lead to ploting the number of participants per country of residency: vast majority from France, next are the US
ppp_data3$pays <- ppp_data3$countries
ppp_data3$pays <- str_replace_all(ppp_data3$pays, c("Coree du sud"="Korea", "Suede"="Suède", "Republique tcheque"="République tchèque", 
                                                    "Nouvelle-Zelande"="Nouvelle-Zélande", "Norvege"="Norvège", "Emirats arabes unis"="Émirats arabes unis",
                                                    "Etats-Unis"="États-Unis"))
country_names <- ppp_data3$pays
ppp_data3$pays <- countryname(country_names)

country <- ppp_data3 %>% 
  group_by(pays) %>%
  summarise(no_rows = length(pays))
country$pays <- recode(country$pays, 'United States'='USA')

map.world <- map_data("world")
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


# 3/ Experiment data: Initialization ---------------------------------------------------------
# There are 2 different tasks: detection and 2AFC
sub_res_data$task <- as.factor(sub_res_data$task)
# There are 4 different background values: 0 (=gray background), -1, -2, -3
sub_res_data$stim_pente_BKG_f <- as.factor(sub_res_data$stim_pente_BKG)
sub_res_data$BKG_type <- as.factor(sub_res_data$BKG_type)

#Slope value of the targets
# > those values have to be extracted from the stimulus file name
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

# Extract the difference between the background (BKG) slope and the target slope
# for the detection task and for the 2-AFC task with a patterned ('noisy') background only
sub_res_data$slopeBKG_cible1 <- NA
sub_res_data$slopeBKG_cible2 <- NA
sub_res_data$slopeBKG_cible1[which(sub_res_data$BKG_type=='NULL')] = abs(as.numeric(as.character(sub_res_data$stim_pente_BKG[which(sub_res_data$BKG_type=='NULL')])))-abs(as.numeric(sub_res_data$cible1[which(sub_res_data$BKG_type=='NULL')]))
sub_res_data$slopeBKG_cible1[which(sub_res_data$BKG_type=='noisy')] = abs(as.numeric(as.character(sub_res_data$stim_pente_BKG[which(sub_res_data$BKG_type=='noisy')])))-abs(as.numeric(sub_res_data$cible1[which(sub_res_data$BKG_type=='noisy')]))
sub_res_data$slopeBKG_cible2[which(sub_res_data$BKG_type=='noisy')] = abs(as.numeric(as.character(sub_res_data$stim_pente_BKG[which(sub_res_data$BKG_type=='noisy')])))-abs(as.numeric(sub_res_data$cible2[which(sub_res_data$BKG_type=='noisy')]))


# 4/ Experiment data: Detection task -----------------------------------------

# Determine whether the target was properly detected by the participant
# distance to the center of the target: sqrt((click_X-centre_X)**2 + (click_Y-centre_Y)**2)
target_rad = 160/2
click_X = as.numeric(sub_res_data$click_x)
click_Y = as.numeric(sub_res_data$click_y)
centre_X = as.numeric(sub_res_data$stim_tarX)
centre_Y = as.numeric(sub_res_data$stim_tarY)
dist2centre = sqrt((click_X-centre_X)**2 + (click_Y-centre_Y)**2)

sub_res_data$within_target <- NA
sub_res_data$within_target[dist2centre>target_rad] = FALSE
sub_res_data$within_target[dist2centre<=target_rad] = TRUE

# compute the distance between the target center and the (absolute) center of the image stimulus (ie, X,Y = 325)
abs_centre_X = 325
abs_centre_Y = 325
sub_res_data$target_pos <- NA
sub_res_data$target_pos <- sqrt((click_X-abs_centre_X)**2 + (click_Y-abs_centre_Y)**2)


# Create a sub dataset for the detection task
missed_trials <- subset(sub_res_data, task=="detection-task-test" & rt=="NULL")
sub_res_data$rt <- as.integer(sub_res_data$rt)
df_detection_task <- subset(sub_res_data, task=="detection-task-test" & rt>150)
df_detection_task2 <- subset(sub_res_data, task=="detection-task-test" & rt<=150) #contains the removed trials only

# create a uniqueID per condition (should be 18 for the detection task)
df <- data.frame(df_detection_task$cible1, df_detection_task$stim_pente_BKG)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_detection_task.cible1, df_detection_task.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
  ungroup()

df_detection_task["condID"] <- df_interact1$interact

# create a uniqueID per condition and per stimulus version (should be 18*5 for the detection task)
df <- data.frame(df_detection_task$cible1, df_detection_task$stim_pente_BKG, df_detection_task$stim_ver)
df_interact2 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_detection_task.cible1, df_detection_task.stim_pente_BKG, df_detection_task.stim_ver)) %>% paste(., collapse = ".")) %>%
  ungroup()

df_detection_task["condID_ver"] <- df_interact2$interact

df_detection_task <- df_detection_task %>% 
  mutate(experiment_order = if_else(trialnb <= 20, 1, 2))

# compute the overall detection rate: 76.9%
detection_rate = 100 * sum(df_detection_task$within_target,  na.rm = TRUE) / sum(df_detection_task$within_target!='NA',  na.rm = TRUE)

# check that there is a similar number of trials per condition (this is the case)
trialpercond <- df_detection_task %>% 
  group_by(condID) %>%  
  summarise(no_rows = length(slopeBKG_cible1))

mean(trialpercond$no_rows)
median(trialpercond$no_rows)
sd(trialpercond$no_rows)

# how many participants saw each image? > about 395 participants/image
pppxcond <- df_detection_task %>%
  group_by(condID_ver) %>%
  summarise(no_rows = length(ID_juge))

mean(pppxcond$no_rows)
median(pppxcond$no_rows)
sd(pppxcond$no_rows)

# rename some of the variable from French to English
df_detection_task <- rename(df_detection_task, c(BKG = stim_pente_BKG, slope_target = cible1, slope_diff = slopeBKG_cible1 ))


# compute the detection rate per condition
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

# compute the median of detection time per condition
medianRT <- df_detection_task %>% 
  group_by(condID) %>%  
  summarise(no_rows = median(rt)) 

medianRT_perVer <- df_detection_task %>% 
  group_by(condID_ver) %>%  
  summarise(no_rows = median(rt))

medianRT_perVer_within <- df_detection_task[df_detection_task$within_target==TRUE,] %>% 
  group_by(condID_ver) %>%  
  summarise(no_rows = median(rt))

# check that trial duration is correct
max(df_detection_task$rt) #should be less than 8000 which is the trial max duration
min(df_detection_task$rt) #should be more than 150 (physiological limit)

# check the correlation between detection rates and reactions times. should be pretty high.
cor(x=medianRT$no_rows, y=DRperCond$no_rows, method = "spearman")
cor(x=medianRT_perVer$no_rows, y=DRperCondperVer$no_rows, method = "spearman")

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


# Statistical model for the detection task - model detection success using a binomial model

# bino_ref2_perBKG <- glmer(bino_click ~ abs_diff_diff_scaled + abs_diff_diff_ref + 
#      target_diff_sign + (1|stim_name) + (1|condID) + (1|ID_juge), family = binomial) 

df_detection_task["detected"] <- as.integer(as.logical(df_detection_task$within_target))

glm_detection <- glmer(detected ~ relevel(factor(BKG), ref = "-2") * abs(slope_diff) + (1|stim_name) + (1|condID) + (1|ID_juge),
                       family = binomial, data = df_detection_task,
                       control=glmerControl(optimizer="nloptwrap", calc.derivs = FALSE))
summary(glm_detection)

car::Anova(glm_detection)

# Visualisation of fixed effects in the binomial model (supplementary figure 3)
sjPlot::plot_model(glm_detection, type = "pred", 
                   terms=c("slope_diff [all]", "BKG [all]"),
                   title = "",
                   legend.title = "Background",
                   axis.title = c("Slope difference","Predicted probabilities of detecting the target (%)")) +
  theme(axis.ticks.length = unit(-0.15, "cm"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# since log transformation doesn't fully normalise the data, we'll use a gamma distribution for our glm
glm_rt <- glmer(rt ~ relevel(factor(BKG), ref = "-2") * abs(slope_diff) + (1|stim_name) + (1|condID) + (1|ID_juge), 
                family = Gamma(link = "log"), 
                data = subset(df_detection_task, detected == 1),
                control=glmerControl(optimizer="nloptwrap", calc.derivs = FALSE))
summary(glm_rt)

car::Anova(glm_rt)

# Visualisation of fixed effects in the binomial model (supplementary figure 4)
sjPlot::plot_model(glm_rt, type = "pred", 
                   terms=c("slope_diff [all]", "BKG [all]"),
                   title = "",
                   legend.title = "Background",
                   axis.title = c("Slope difference","Predicted detection times for detected targets (ms)")) +
  theme(axis.ticks.length = unit(-0.15, "cm"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# detection probability per condition (binomial outcome)
# Compute detection proportions with CIs
summary_df <- df_detection_task %>%
  group_by(BKG, slope_target) %>%
  summarise(
    detection_rate = mean(detected),
    n = n(),
    se = sqrt(detection_rate * (1 - detection_rate) / n),
    ci_lower = detection_rate - 1.96 * se,
    ci_upper = detection_rate + 1.96 * se
  )

summary_df["cond_ID"] <- interaction(summary_df$slope_target, as.character(summary_df$BKG))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# PLOT FIGURE 2B
ggplot(data=summary_df, aes(x= cond_ID, y=detection_rate, colour=factor(BKG))) +
  scale_colour_manual(values=cbPalette) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_smooth(aes(group=BKG), method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 0.3, linetype = 2) +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") +
  annotate('rect', ymin=-Inf, ymax=Inf, xmin=6.5, xmax=12.5, alpha=.1, fill='grey') +
  xlab("Target slope") + ylab("Detection probabilities") +
  scale_x_discrete(labels=(slope_Tar1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme(axis.ticks.length = unit(-0.15, "cm"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# PLOT FIGURE 2C: RTs for detected trials only
df_detection_task["cond_ID"] <- interaction(df_detection_task$slope_diff, as.character(df_detection_task$BKG))

ggplot(data = subset(df_detection_task, detected == 1),
       aes(x = cond_ID, y = rt, fill = factor(BKG), color = factor(BKG))) +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  scale_fill_manual(values = alpha(c("#999999", "#E69F00", "#56B4E9"), .7)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") +
  annotate('rect', ymin=-Inf, ymax=Inf, xmin=6.5, xmax=12.5, alpha=.1, fill='grey') +
  xlab("Target slope") + ylab("Reaction Time (ms)") +
  scale_x_discrete(labels=(slope_Tar1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none") +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme(axis.ticks.length = unit(-0.15, "cm"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# 5/ Experiment data: Preference tasks - gray background ---------------------------

# Create a sub dataset for the gray 2AFC task
df_2AFC_gray <- subset(sub_res_data, BKG_type=="gray" & rt != 'NULL')


#check left/right bias > no bias
df_2AFC_gray %>% 
  group_by(click_half) %>%
  summarise(no_rows = length(click_half))


# create a uniqueID per condition 
df <- data.frame(df_2AFC_gray$cible1, df_2AFC_gray$cible2)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_gray.cible1,df_2AFC_gray.cible2)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_gray["condID"] <- df_interact1$interact

# check that there is a similar number of trials per condition (this is the case)
df_mean <- df_2AFC_gray %>% 
  group_by(condID) %>%  
  summarise(no_rows = length(slopeBKG_cible1))

mean(df_mean$no_rows)
median(df_mean$no_rows)
sd(df_mean$no_rows)

# how many ppp saw each image? > about 36 participants/image (group_by(stim_name)) and 71 participants/condID_ver (group_by(condID_ver))
pppxcond <- df_2AFC_gray %>%
  group_by(stim_name) %>%
  summarise(no_rows = length(ID_juge))

mean(pppxcond$no_rows)
median(pppxcond$no_rows)
sd(pppxcond$no_rows)

nstim <- length(unique(df_2AFC_gray$condID)) #nstim = 39

df_2AFC_gray <- df_2AFC_gray %>% 
  mutate(experiment_order = if_else(trialnb <= 20, 1, 2))

## Elo ranking of the stimuli
# Create two new columns to determine the preferred stimulus (winner) of a pair (vs loser)
# Using the condID_cibleX to include the background slope value in one pair

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

myratings_gray <- ratings(res1, show = "mean", drawplot = TRUE)

# same but with intracondition variability (5 stimulus versions)
set.seed(123)
res1b <- elochoice(df_2AFC_gray$winner_ver, df_2AFC_gray$loser_ver, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res1b)

myratings <- ratings(res1b, show = "original", drawplot = FALSE)
ratings(res1b, show = "mean", drawplot = TRUE)


# Plot the average ranking per condition (averaged across stim versions)
temp <- res1b$ratmat
df_elopercondver <- data.frame(colMeans(temp), apply(temp, 2, sd))
names <- rownames(df_elopercondver)
rownames(df_elopercondver) <- NULL
df_elopercondver <- cbind(names,df_elopercondver)
colnames(df_elopercondver) <- c("condID_ver", "ranking", "sd")

df_elopercondver["stim_ver"] <- substr(df_elopercondver$condID_ver, nchar(df_elopercondver$condID_ver), nchar(df_elopercondver$condID_ver))
df_elopercondver["condID"] <- substr(df_elopercondver$condID_ver, 1, nchar(df_elopercondver$condID_ver)-2)

# PLOT FIGURE 3C:
ggplot(data = df_elopercondver, aes(x = condID, y = ranking)) + 
  geom_boxplot(color = "#CC79A7", fill = "#CC79A7", alpha=0.7, width = .5, outlier.shape = NA) + 
  geom_point(color = "#CC79A7", fill = "#CC79A7", size = 2, alpha = .7, position = position_jitter(seed = 1, width = .2)) +
  geom_smooth(aes(group=1), color="#CC79A7", method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 0.5, linetype = 2) +
  #ggtitle("Élő Ratings (average across 1000 iterations) for 2-AFC task with gray BKG and their different stimuli") +
  xlab("Target slope") + ylab("Ranking (Élő score)") +
  #geom_vline(xintercept = 7.39, linetype="dashed", color = "black") +
  geom_vline(xintercept = 7.36, linetype="dashed", color = "#CC79A7") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none") +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme(axis.ticks.length = unit(-0.15, "cm"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

df_elopercondver %>%
  group_by(condID) %>%
  summarise_at(vars(ranking), list(name = mean))


# save the df for the stat models
df_gray_lmer <- df_elopercondver


# 6/ Experiment data: Preference tasks - noisy BKG ---------------------------

## Élő score: ranking of the images of the preference tasks

# Create a sub dataset for the noisy 2AFC task
df_2AFC_noisy <- subset(sub_res_data, BKG_type=="noisy" & rt != 'NULL')

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

# how many participants saw each image? > about 36 participants/image (group_by(stim_name)) and 71 participants/condID_ver (group_by(condID_ver))
pppxcond <- df_2AFC_noisy %>%
  group_by(stim_name) %>%
  summarise(no_rows = length(ID_juge))

mean(pppxcond$no_rows)
median(pppxcond$no_rows)
sd(pppxcond$no_rows)

# formula: elochoice(winner, loser, kval = 100, startvalue = 0, runs = 1, normprob = FALSE)
nstim <- length(unique(df_2AFC_noisy$condID)) #nstim = 45

# Create two new columns to determine the preferred stimulus (winner) of a pair (vs loser)
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

# (takes a few minutes to run)
set.seed(123)
res0 <- elochoice(df_2AFC_noisy$winner, df_2AFC_noisy$loser, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res0)

myratings <- ratings(res0, show = "original", drawplot = FALSE)
ratings(res0, show = "mean", drawplot = TRUE)

# same but with the 5 stimulus versions of each condition (takes a few minutes to run)
set.seed(123)
res0b <- elochoice(df_2AFC_noisy$winner_ver, df_2AFC_noisy$loser_ver, kval = 100, startvalue = 0, runs = 1000, normprob = FALSE)
summary(res0b)

myratings <- ratings(res0b, show = "original", drawplot = FALSE)
ratings(res0b, show = "mean", drawplot = TRUE)



# PLOT FIGURE 4B: average ranking per condition 
# with 1000 iterations and averaged across 5 stimulus versions
rm(df_elopercondver)
temp <- res0b$ratmat
df_elopercondver <- data.frame(colMeans(temp), apply(temp, 2, sd))
names <- rownames(df_elopercondver)
rownames(df_elopercondver) <- NULL
df_elopercondver <- cbind(names,df_elopercondver)
colnames(df_elopercondver) <- c("condID_ver", "ranking", "sd")

df_elopercondver["stim_ver"] <- substr(df_elopercondver$condID_ver, nchar(df_elopercondver$condID_ver), nchar(df_elopercondver$condID_ver))
df_elopercondver["condID"] <- substr(df_elopercondver$condID_ver, 1, nchar(df_elopercondver$condID_ver)-2)

df_elopercondver["BKG"] <- c(rep(-1,6*5), rep(-2,6*5), rep(-3,6*5))
df_elopercondver["slope_target"] <- c( rep(-0.25, 5), rep(-0.50,5), rep(-0.75,5), rep(-1.25,5), rep(-1.50,5), rep(-1.75,5),
                                       rep(-1.25,5), rep(-1.50,5), rep(-1.75,5), rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), 
                                       rep(-2.25,5), rep(-2.50,5), rep(-2.75,5), rep(-3.25,5), rep(-3.50,5), rep(-3.75,5))
df_elopercondver["detectionRT"] <- medianRT_perVer$no_rows
df_elopercondver['slope_diff'] <- df_elopercondver$BKG-(df_elopercondver$slope_target)


ggplot(data=df_elopercondver, aes(x= condID, y=ranking, colour=factor(BKG))) +
  scale_colour_manual(values=cbPalette) +
  geom_boxplot(alpha=0.5, width = .5, outlier.shape = NA) + 
  geom_point(size = 2, alpha = .5, position = position_jitter(seed = 1, width = .2)) +
  geom_smooth(aes(group=BKG), method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 0.3, linetype = 2) +
  geom_vline(xintercept = c(6.5,12.5), linetype="dashed", color = "grey") +
  annotate('rect', ymin=-Inf, ymax=Inf, xmin=6.5, xmax=12.5, alpha=.1, fill='grey') +
  #ggtitle("Élő Ratings for 2-AFC task with noisy BKG and their different stimuli") +
  xlab("Target slope") + ylab("Ranking (Élő score)") +
  scale_x_discrete(labels=slope_Tar1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none") +
  theme(axis.text.x = element_text(angle=30, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme(axis.ticks.length = unit(-0.15, "cm"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# save the df for the stat models
df_noisy_lmer <- df_elopercondver

# 7/ Statistical analyses ----------------------------------------------------

## Task 1 - Statistical analyses (patterned "noisy" background) ----

# > Recoding slope diff into 2 variables:
#slope_diff_sign: 1, -1 (pos vs neg) 
#slope_diff_value: 0.25, 0.5, 0.75 

df_noisy_lmer['slope_diff_sign'] <- df_noisy_lmer$slope_diff/df_noisy_lmer$slope_diff
df_noisy_lmer$slope_diff_sign[which(df_noisy_lmer$slope_diff<=0)] = -1
df_noisy_lmer$slope_diff_sign_f <- factor(df_noisy_lmer$slope_diff_sign)

df_noisy_lmer['slope_diff_value'] <- abs(df_noisy_lmer$slope_diff)
df_noisy_lmer$slope_diff_value_f <- as.factor(df_noisy_lmer$slope_diff_value)

# First, we need to test whether the ranking~slope_target relationship is different for the
# three background types (i.e. the BKG:slope_target interaction)
lm1 <- lm(ranking~ as.factor(BKG) * poly(slope_target, degree=2, raw = T), data = df_noisy_lmer )
lmy <- lm(ranking~ slope_diff_value_f + slope_diff_sign_f + relevel(factor(BKG), ref = "-2") * poly(slope_target, degree=2, raw = F), data = df_noisy_lmer )

anova(lm1)
anova(lmy)
# The interaction is significant, so this justifies looking at each background separately.

# First we need a function to calculate the maximum of the ranking~target_slope curve:
maximum <- function(b1, b2){
  # b1 and b2 are the linear and quadratic coeficients respectively
  # If y = a + b1.​x + b2.​x^2 then dy/dx = b1​ + 2.b2​.x, 
  # so at maxmimum, b1​ + 2.b2​.x = 0, or x = -b1 / (2.b2)
  x <- -b1 / (2*b2)
  return(x)
}

# Run linear quadratic models (Tables S10, S11, S12) and compute maximum and CIs for each background:
for(i in c(-1, -2, -3)){
  lm2 <- lm(ranking~ poly(slope_target, degree=2, raw = T),
            data = subset(df_noisy_lmer, BKG == i))
  print( summary(lm2) )
  #print( anova(lm2) )
  
  b1 <- coef(lm2)[2]
  names(b1) <- NULL
  b2 <- coef(lm2)[3]
  names(b2) <- NULL
  x <- maximum(b1, b2)
  # 95% confidence intervals for coefficients
  cis <- confint(lm2)
  b1.lwr <- cis[2, 1]
  b2.lwr <- cis[3, 1]
  x.lwr <- maximum(b1.lwr, b2.lwr)
  b1.uppr <- cis[2, 2]
  b2.uppr <- cis[3, 2]
  x.uppr <- maximum(b1.uppr, b2.uppr)
  print(paste0("For background ", i, " the maximum is ", round(x, 2), " with 95% c.i.s ", round(x.lwr, 2), " to ", round(x.uppr, 2)))
  print("-----------------------------------------------------")
  rm(lm2)
}


# Run linear mixed-effect models for each background separately > Table S7, S8, S9

lmer_bkg1 <- lmer(ranking~ slope_diff_value * slope_diff_sign_f  + (1|stim_ver), data = df_noisy_lmer[df_noisy_lmer$BKG==-1,], REML = FALSE)
summary(lmer_bkg1)

lmer_bkg2 <- lmer(ranking~ slope_diff_value + slope_diff_sign_f  + (1|stim_ver), data = df_noisy_lmer[df_noisy_lmer$BKG==-2,], REML = FALSE)
summary(lmer_bkg2)

lmer_bkg3 <- lmer(ranking~ slope_diff_value * slope_diff_sign_f  + (1|stim_ver), data = df_noisy_lmer[df_noisy_lmer$BKG==-3,], REML = FALSE)
summary(lmer_bkg3)



## Task 2 - Statistical analyses (gray background) ----

df_gray_lmer["slope"] <- as.numeric(df_gray_lmer$condID)
df_gray_lmer["slope2"] <- df_gray_lmer$slope^2


lmer_gray <- lmer(ranking ~ slope + slope2 + (1|stim_ver), data = df_gray_lmer, REML = FALSE)
# Table S6:
summary(lmer_gray)


# 8/ How many raters are needed to achieve stability in ratings? -------------
# !! takes quite a lot of time
length(unique(df_2AFC_gray$ID_juge))
length(unique(df_2AFC_noisy$ID_juge))
set.seed(123)
# figures S1 and S2
res2_ver <- raterprog(df_2AFC_gray$winner_ver, df_2AFC_gray$loser_ver, df_2AFC_gray$ID_juge, progbar = TRUE, ratershuffle = 10) ## takes time!
res1_ver <- raterprog(df_2AFC_noisy$winner_ver, df_2AFC_noisy$loser_ver, df_2AFC_noisy$ID_juge, progbar = TRUE, ratershuffle = 10) ## takes time!


# 9/ Binomial analyses ----

# 1. Define the choice variable: if left image chosen then 1, if right image then 0
df_2AFC_noisy <- subset(sub_res_data, BKG_type=="noisy" & rt != 'NULL')
df_2AFC_noisy$bino_click <- NA
df_2AFC_noisy$bino_click[df_2AFC_noisy$click_half=='left'] = 1
df_2AFC_noisy$bino_click[df_2AFC_noisy$click_half=='right'] = 0

df_2AFC_gray <- subset(sub_res_data, BKG_type=="gray" & rt != 'NULL')
df_2AFC_gray$bino_click <- NA
df_2AFC_gray$bino_click[df_2AFC_gray$click_half=='left'] = 1
df_2AFC_gray$bino_click[df_2AFC_gray$click_half=='right'] = 0

# 2. Select participants information for the model (identifier + gender + age) 
ppp_data <- ppp_data3 %>% select(ID_juge, year, gender)
ppp_data$age <- 2023 - ppp_data$year

df2add <- df_2AFC_noisy
df2add <- df2add %>%
  left_join(ppp_data, by = c("ID_juge" = "ID_juge")) %>%
  select(ppp_age = age, ppp_gender = gender)
df_2AFC_noisy <- cbind(df_2AFC_noisy, df2add)

df2add <- df_2AFC_gray
df2add <- df2add %>%
  left_join(ppp_data, by = c("ID_juge" = "ID_juge")) %>%
  select(ppp_age = age, ppp_gender = gender)
df_2AFC_gray <- cbind(df_2AFC_gray, df2add)

# 3. Define and centre the quantitative variables of interest

df_2AFC_noisy['slope_target_diff'] <- as.numeric(df_2AFC_noisy$cible1)-as.numeric(df_2AFC_noisy$cible2)
df_2AFC_gray['slope_target_diff'] <- as.numeric(df_2AFC_gray$cible1)-as.numeric(df_2AFC_gray$cible2)

df_2AFC_noisy['slope_target_diff_scaled'] <- scale(df_2AFC_noisy$slope_target_diff)
df_2AFC_gray['slope_target_diff_scaled'] <- scale(df_2AFC_gray$slope_target_diff)


## Task 3 - Binomial models ----

df_2AFC_noisy['abs_diff_diff'] <- abs(df_2AFC_noisy$slopeBKG_cible1)-abs(df_2AFC_noisy$slopeBKG_cible2)
df_2AFC_noisy['abs_diff_diff_scaled'] <- scale(df_2AFC_noisy$abs_diff_diff)
# NB: abs_diff_diff_scaled corresponds to [abs(slope_targetL-slope_background) - abs(slope_targetR-slope_background)] 

df_2AFC_noisy['target_diff_sign'] <- NA
df_2AFC_noisy$target_diff_sign[df_2AFC_noisy$slopeBKG_cible1>0 & df_2AFC_noisy$slopeBKG_cible2>0] = 'oo'
df_2AFC_noisy$target_diff_sign[df_2AFC_noisy$slopeBKG_cible1<0 & df_2AFC_noisy$slopeBKG_cible2<0] = 'oo'
df_2AFC_noisy$target_diff_sign[df_2AFC_noisy$slopeBKG_cible1>0 & df_2AFC_noisy$slopeBKG_cible2<0] = '+-'
df_2AFC_noisy$target_diff_sign[df_2AFC_noisy$slopeBKG_cible1<0 & df_2AFC_noisy$slopeBKG_cible2>0] = '-+'

df <- data.frame(df_2AFC_noisy$cible1, df_2AFC_noisy$cible2, df_2AFC_noisy$stim_pente_BKG, df_2AFC_noisy$stim_ver)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_noisy.cible1, df_2AFC_noisy.cible2, df_2AFC_noisy.stim_pente_BKG)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_noisy["condID"] <- df_interact1$interact # n=45


# Model with reference value for intermediate preferences (Table S3)
ref_val <- -2.39 
df_2AFC_noisy['abs_diff_diff_ref'] <- abs(as.numeric(df_2AFC_noisy$cible1)-ref_val)-abs(as.numeric(df_2AFC_noisy$cible2)-ref_val)
# NB: abs_diff_diff_ref corresponds to: [abs(slope_targetL-2.39) - abs(slope_targetR-2.39)] 

bino_ref <- glmer(bino_click ~ abs_diff_diff_scaled + abs_diff_diff_ref + abs_diff_diff_ref:relevel(factor(stim_pente_BKG), ref = "-2") + 
                     target_diff_sign + (1|stim_name) + (1|condID) + (1|ID_juge),
                   data = df_2AFC_noisy, family = binomial, control = glmerControl(optimizer="nloptwrap", calc.derivs = FALSE)) 

# Table S3: 
summary(bino_ref)
car::Anova(bino_ref, type = 3)


## Task 2 - Binomial models ----

df_2AFC_gray['target_diff_sign'] <- NA
df_2AFC_gray$target_diff_sign[df_2AFC_gray$slopeBKG_cible1>0 & df_2AFC_gray$slopeBKG_cible2>0] = '++'
df_2AFC_gray$target_diff_sign[df_2AFC_gray$slopeBKG_cible1<0 & df_2AFC_gray$slopeBKG_cible2<0] = '--'
df_2AFC_gray$target_diff_sign[df_2AFC_gray$slopeBKG_cible1>0 & df_2AFC_gray$slopeBKG_cible2<0] = '+-'
df_2AFC_gray$target_diff_sign[df_2AFC_gray$slopeBKG_cible1<0 & df_2AFC_gray$slopeBKG_cible2>0] = '-+'

df <- data.frame(df_2AFC_gray$cible1, df_2AFC_gray$cible2)
df_interact1 <- df %>%
  mutate_if(is.factor, as.character) %>%
  rowwise() %>% 
  mutate(interact = sort(c(df_2AFC_gray.cible1,df_2AFC_gray.cible2)) %>% paste(., collapse = ".")) %>%
  ungroup()
df_2AFC_gray["condID"] <- df_interact1$interact

ref_val <- -2.39

df_2AFC_gray['abs_diff_diff_ref'] <- abs(as.numeric(df_2AFC_gray$cible1)-ref_val)-abs(as.numeric(df_2AFC_gray$cible2)-ref_val)


bino_gray <- glmer(bino_click ~ slope_target_diff_scaled 
                   + (1|stim_name) + (1|condID) + (1|ID_juge),
                   data = df_2AFC_gray, family = binomial, control = glmerControl(optimizer="nloptwrap", calc.derivs = FALSE)) 
# Table S2: 
summary(bino_gray)
car::Anova(bino_gray, type = 3)


# Simulation -> Figure S3 (!takes some time!)
ref_val_range <- seq(-4, 0, by=0.05)
AIC.score <- matrix(data=NA,nrow=length(ref_val_range),ncol=2)
count <- 1

for (ndx in ref_val_range) {
  print(ndx)
  df_2AFC_gray['abs_diff_diff_ref'] <- abs(as.numeric(df_2AFC_gray$cible1)-ndx)-abs(as.numeric(df_2AFC_gray$cible2)-ndx)
  
  bino_gray_ref <-  glmer(bino_click ~ abs_diff_diff_ref 
                          + (1|stim_name) + (1|condID) + (1|ID_juge),
                          data = df_2AFC_gray, family = binomial, control = glmerControl(optimizer="nloptwrap", calc.derivs = FALSE)) 
  
  AIC.score[count,1] <- summary(bino_gray_ref)$AIC[1]
  AIC.score[count,2] <- ndx
  count<- count +1
  print(count)
}

plot(AIC.score[,2], AIC.score[,1], xlab="reference background value", ylab="AIC score")
hist(AIC.score)

