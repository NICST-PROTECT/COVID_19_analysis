library(ggplot2)
library(tidyverse)
library(dplyr)
library(XML)
library(reshape2)
library(data.table)
library(ggthemes)
library(lubridate)
library(gridExtra)
library(VennDiagram)
library(RColorBrewer)
library(grid)    
library(gridExtra)
library(gtable)  
library(patchwork)
library(lubridate)
library(eulerr)
library("survival")
library("survminer")
library(finalfit)
library(glue)

all_data <- read.csv("admission_data_13_04_2020_17_06_2020.csv", stringsAsFactors = FALSE, na.strings = c(""))


# remove test units
all_data <- all_data[!(all_data$unitId %in% c("5d7a15bd3b60eb002d29f8f4", 
                                              "5d9c65687ee019002626fcd3", 
                                              "5de62f0df045lf00lb2b793l", 
                                              "53002ba326328d00lb69a563")),]

# add a covid status column 
all_data <- all_data %>% 
  mutate(CovidStatus = case_when((((sariPreDischarge.influenza_diagnosis == 'Yes- confirmed') &
                                     ((sariPreDischarge.influenza1 == 'COVID-19') |
                                        sariPreDischarge.influenza2 == 'COVID-19')) | 
                                    ((sariPreDischarge.sari_diagnosis == 'Yes- confirmed') &
                                       (sariPreDischarge.sari1 == 'COVID-19'))) ~ 'Confirmed',
                                 (((sariPreDischarge.influenza_diagnosis == 'Yes- probable') &
                                     ((sariPreDischarge.influenza1 == 'COVID-19') |
                                        sariPreDischarge.influenza2 == 'COVID-19')) | 
                                    ((sariPreDischarge.sari_diagnosis == 'Yes- probable') &
                                       (sariPreDischarge.sari1 == 'COVID-19'))) ~ 'Suspected',
                                 admission.sari == 'Confirmed' ~ 'Confirmed',
                                 admission.sari == 'Suspected' ~ 'Suspected')) %>% 
  mutate(sari_form = case_when((((sariPreDischarge.influenza_diagnosis == 'Yes- confirmed') &
                                   ((sariPreDischarge.influenza1 == 'COVID-19') |
                                      sariPreDischarge.influenza2 == 'COVID-19')) | 
                                  ((sariPreDischarge.sari_diagnosis == 'Yes- confirmed') &
                                     (sariPreDischarge.sari1 == 'COVID-19'))) ~ 'Yes',
                               admission.sari == 'Confirmed' ~ 'No'))



# select covid data only
covid_data_s_c <- all_data %>% 
  filter(CovidStatus == 'Confirmed' | CovidStatus == 'Suspected')

# numbejr of hospitals 
hospital_count <- n_distinct(covid_data_s_c$hospitalId)

# get unitId list
unit_list <- covid_data_s_c %>% 
  distinct(unitId)


# get number of patients admitted to these units
all_admission <- all_data %>% 
  nrow()


# number of suspected and convifred cases 
covid_count = nrow(covid_data_s_c)

# filter to include confired cases only 
covid_data <- covid_data_s_c %>% 
  filter(CovidStatus == 'Confirmed')


# number of hostitals 

# number of units 



# convert string discharge times to datetime
hm_data <- hm(covid_data$discharge.time_of_discharge) 
hms_data <- hms(covid_data$discharge.time_of_discharge) 

# if missing data in hm format replace with date from hms format
hms_data[is.na(hms_data)] <- hm_data[is.na(hms_data)] 
covid_data$discharge_datatime <- ymd(covid_data$discharge.date_of_discharge) + hms_data   

# convert string admission times to datetime
hm_data <- hm(covid_data$admission.time_of_admission) 
hms_data <- hms(covid_data$admission.time_of_admission)

# if missing data in hm format replace with date from hms format
hms_data[is.na(hms_data)] <- hm_data[is.na(hms_data)] 
covid_data$admission_datatime <- ymd(covid_data$admission.date_of_admission) + hms_data   

# get lenght of stay and round to number of days
covid_data <- covid_data %>%  
  mutate(length_of_stay = as.numeric(difftime(discharge_datatime, admission_datatime, units = "days")))


# add discharge hr time if na dischare date is max dischage date
covid_data <- covid_data %>% 
  mutate(discharge_date_hr = case_when(is.na(discharge.date_of_discharge) ~ '2020-06-17',
                                       TRUE ~ discharge.date_of_discharge)) %>% 
  mutate(discharge_time_hr = case_when(is.na(discharge.time_of_discharge) ~ '23-59-59',
                                       TRUE ~ discharge.time_of_discharge))


# convert string discharge_hr times to datetime
hm_data <- hm(covid_data$discharge_time_hr) 
hms_data <- hms(covid_data$discharge_time_hr) 

# if missing data in hm format replace with date from hms format
hms_data[is.na(hms_data)] <- hm_data[is.na(hms_data)] 
covid_data$discharge_datatime_hr <- ymd(covid_data$discharge_date_hr) + hms_data   

# convert string admission times to datetime
hm_data <- hm(covid_data$admission.time_of_admission) 
hms_data <- hms(covid_data$admission.time_of_admission)

# if missing data in hm format replace with date from hms format
hms_data[is.na(hms_data)] <- hm_data[is.na(hms_data)] 
covid_data$admission_datatime <- ymd(covid_data$admission.date_of_admission) + hms_data   

# get lenght of stay and round to number of days
covid_data <- covid_data %>%  
  mutate(length_of_stay_hr = as.numeric(difftime(discharge_datatime_hr, admission_datatime, units = "days")))



covid_data <- covid_data %>% 
  # convert date columns to datetime
  mutate(date_of_admission = ymd(admission.date_of_admission)) %>%
  mutate(date_of_discharge = ymd(discharge.date_of_discharge)) %>%
  mutate(symptom_date = ymd(covid_data$sariAdmissionAssessment.symptom_date)) %>% 
  mutate(time_to_onset = as.numeric(difftime(ymd(admission.date_of_admission_hospital), symptom_date, units = "days"))) %>% 
  mutate(time_to_icu_onset = as.numeric(difftime(date_of_admission, symptom_date, units = "days"))) %>% 
  # add outcome column with In ICU option
  mutate(outcome = case_when(discharge.discharge_status == 'Alive' ~ 'Discharged', 
                             discharge.discharge_status == 'Dead' ~ 'Died',
                             TRUE ~ 'In ICU')) %>% 
  # add timef for getting hazard ratio
  mutate(timef = case_when(outcome == 'Discharged' ~ max(length_of_stay_hr),
                           outcome == 'Died' ~ length_of_stay_hr,
                           outcome == 'In ICU' ~ max(length_of_stay_hr)))


# create new column named age_group by grouping age by every 5 years
age_break = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120)
age_labels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '< 90')
covid_data$admission.age <- as.numeric(covid_data$admission.age)
covid_data$age_group <- cut(covid_data$admission.age, breaks= age_break, right = FALSE, labels = age_labels)




#############################################
### Number of admission by age and gender ###
#############################################



# group admission date by age gender and outcome for outcome by age and gender
age_data <- covid_data %>% 
  drop_na(age_group) %>% 
  # set male values to negative so we can have a split axis
  mutate(Number_of_pateitns = case_when(admission.gender == 'Male' ~ -num, admission.gender == 'Female' ~ num)) %>% 
  group_by(age_group, admission.gender, outcome) %>% 
  summarise(patients = sum(Number_of_pateitns))

# get number of female patients
female_count = covid_data %>%
  drop_na(age_group) %>%
  filter(admission.gender == 'Female') %>% 
  nrow()

# get number of male patients
male_count <- covid_data %>% 
  drop_na(age_group) %>%
  filter(admission.gender == 'Male') %>% 
  nrow()

# get percentage of male and female
all_patients <- female_count + male_count

per_female = round((female_count / all_patients) * 100, 1)
per_male = round((male_count / all_patients) * 100, 1)

# create a label for pyramid chart
female_label = paste(female_count, '/', all_patients, ' (', per_female, '%)')
male_label = paste(male_count, '/', all_patients, ' (', per_male, '%)')



# get other age and gender stats 
age_stats <- summary(covid_data$admission.age)
under_18 <- nrow(covid_data[covid_data$admission.age < 18,])
per_under_18 <- round(under_18 / all_patients * 100, 1)

pregnent <- nrow(covid_data[na.omit(covid_data$sariAdmissionAssessment.pregnant) == "Yes",])
eligible_pregnent <- nrow(covid_data[(na.omit(covid_data$admission.gender) == 'Female') & (covid_data$admission.age > 18),])
per_pregnent <- round(pregnent / eligible_pregnent * 100, 1)







pyramid <- age_data %>% 
  ggplot(aes(x = age_group, y = patients, fill = factor(outcome, levels=c("Died", "In ICU", "Discharged")))) +   # Fill column
  geom_bar(stat = "identity", width = .6) + 
  scale_y_continuous(labels=abs, breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_manual(values = c("#52307c","#ffbb00",'#e75480')) +
  coord_flip(clip = 'off') +
  geom_hline(yintercept = 0) +
  xlab("Age (years)") +
  ylab("Number of patients") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text = element_text(size= 14),
        axis.title.x = element_text(face="bold", hjust = 1, size = 14),
        axis.title.y = element_text(face="bold", hjust = 1, size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.1), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "#ffffffaa", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(60, 8, 2, 2, unit = "pt"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))   # Centre plot title

# add labels and save plot to image
pyramid + 
  geom_text(label = 'Male patients', 
            x = 18.8,
            y = -4,
            hjust = 1,
            size = 5,
            fontface ="bold",
            check_overlap = TRUE) +
  geom_text(label = 'Female patients', 
            x = 18.8,
            y = 4,
            hjust = 0,
            size = 5,
            fontface ="bold",
            check_overlap = TRUE) +
  geom_text(label = male_label, 
            x = 18.2,
            y = -4,
            hjust = 1,
            size = 5,
            check_overlap = TRUE) +
  geom_text(label = female_label,
            x = 18.2,
            y = 4,
            hjust = 0,
            size = 5,
            check_overlap = TRUE) +
  ggsave("age_sex_covid_v2.png", width = 25, height = 20, units = "cm")




#####################################
### Number of admission by gender ###
#####################################


# groupby gender and create a cumliative count
admission_gender_df = covid_data %>% 
  group_by(admission.gender) %>% 
  arrange(date_of_admission) %>% 
  mutate(cumilative_count = cumsum(num))


admission_gender_df %>% 
  ggplot(aes(x=date_of_admission, y=cumilative_count, group = admission.gender, color= admission.gender)) +
  geom_line(stat = "identity", size = 1) +
  xlab("Admission date") +
  ylab("Number of patients")  +
  scale_y_continuous(expand = c(0, 1.5)) +
  theme(plot.title = element_text(hjust = .5), 
        axis.title.x = element_text(face="bold", hjust = 1, size = 14),
        axis.title.y = element_text(face="bold", hjust = 1, size = 14),
        axis.text = element_text(size= 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "#ffffffaa", colour = NA),
        legend.direction="horizontal",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  ggsave("sex_date_line_chart_covid_v2.png", width = 20, height = 10, units = "cm")

#############################################
###                Set up                 ###
#############################################


# import data 









################################################
### Percentage of patients with each symptom ###
################################################


# input all symptom columns to convert_to_long and get -
# a list of symptoms and a dataframe with each sypmot as a new column
symptoms_cols <- c("sariAdmissionAssessment.signs", "sariAdmissionAssessment.signs1", "sariAdmissionAssessment.signs2", "sariAdmissionAssessment.signs3", "sariAdmissionAssessment.signs4", "sariAdmissionAssessment.signs5", "sariAdmissionAssessment.signs6")
symptom_info <- convert_to_long_sym(covid_data, symptoms_cols)
symptoms_conditional_string <- append(symptom_info$list, 'patient_id')

# select patients symptom columns
symptom_data <- symptom_info$data %>% 
  select(all_of(symptoms_conditional_string))


# get number of patients with na symptom

na_count = rowSums(symptom_data[symptom_info$list] == 'Unknown')
na_sypmtoms_count <- length(na_count[na_count == length(symptom_info$list)])
per_na_symptom <- round(na_sypmtoms_count/ nrow(symptom_data) * 100, 1)

# remove unknown symptoms patients
symptom_data <- symptom_data %>% 
  filter_at(symptom_info$list, all_vars(. != 'Unknow'))


# get percentage of patients without each sypmtom
symptom_per_df <- data.frame(Yes = colMeans(symptom_data[symptom_info$list] == 'Yes') * 100,
                             No = colMeans(symptom_data[symptom_info$list] == 'No') * 100,
                             count_yes = colSums(symptom_data[symptom_info$list] == 'Yes'))

#create a duplicate dataframe to look at the couunts
symptom_count_df <- symptom_per_df

# add rownames as a column and delete rowname
symptom_per_df$sypmtom <- rownames(symptom_per_df)
row.names(symptom_per_df) <- NULL

# convert data to long format
symptom_per_df <- reshape2::melt(symptom_per_df, id.vars = c("sypmtom"), measure.vars = c('Yes', 'No'), na.rm = TRUE, order = FALSE)

# convert Yes/No to factor so it has order
symptom_per_df$variable = factor(symptom_per_df$variable, c('No', 'Yes'), ordered = TRUE)

# get ordered data frame by yes
symptom_order <- symptom_per_df %>% 
  filter(variable == 'Yes') %>% 
  arrange((value))

# get the ordered list of sypmtoms
symptom_order <- symptom_order$sypmtom 

# convert sypmtoms to factor so that it is ordred
symptom_per_df$sypmtom <- factor(symptom_per_df$sypmtom, symptom_order, ordered = TRUE)

symptoms_bar <- symptom_per_df %>% 
  ggplot(aes(fill=variable, order = variable, y=value, x=sypmtom)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Symptoms") +
  ylab("Proportion of patients with symptom (%)") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0.2)) +
  scale_fill_manual(values = c("#ffbb00","#52307c")) +
  theme(plot.title = element_text(hjust = .5, size = 14), 
        axis.title.x = element_text(face="bold", vjust = 1, size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        # legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 8, 53, 2, unit = "pt"),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

symptoms_bar +
  ggsave("symptom_per_v2.png", width = 20, height = 20, units = "cm")





##############################################
### overlap of top 3 sypmtoms venn diagram ###
##############################################

# colours for venn diagram 
myCol <- brewer.pal(3, "Pastel2")

# get list of patient ids for each symptom so that we can see the overlap
list_Shortness_of_breath <- symptom_data %>% 
  filter(symptom_data$'Shortness of breath' == 'Yes')
list_Shortness_of_breath <- list_Shortness_of_breath$patient_id

list_History_of_fever <- symptom_data %>% 
  filter(symptom_data$'History of fever' == 'Yes')
list_History_of_fever <- list_History_of_fever$patient_id

list_Cough_with_sputum <- symptom_data %>% 
  filter(symptom_data$'Sore throat' == 'Yes')
list_Cough_with_sputum <- list_Cough_with_sputum$patient_id

symptom_venn_data = list(
  "Shortness of breath" = list_Shortness_of_breath,
  "History of fever" = list_History_of_fever,
  "Sore throat" = list_Cough_with_sputum
)

symptom_venn_data = euler(symptom_venn_data, shape = "ellipse")


symptoms_venn <- plot(symptom_venn_data, 
                      quantities = TRUE, 
                      legend = list(labels = c("Shortness of breath", "History of fever", "Sore throat"), cex = 1.3),
                      fills = list(fill =c("#f9ccff", "#fdffbf", "#bbb3ff")),
                      mar = c(2, 5, 2, 5))


png("symptom_venn.png")
symptoms_venn
dev.off()



## join both symptom charts

ggsave(file = "symptoms_charts_v2.png", grid.arrange(symptoms_bar, symptoms_venn,
                                                  ncol = 1,
                                                  heights = c(1.8, 0.85)), 
       width = 20, 
       height = 28, 
       units = 'cm')






#####################################################
### Percentage of patients with each comobiditess ###
#####################################################


# input all comobidites columns to convert_to_long and get -
# a list of comobiditess and a dataframe with each sypmot as a new column
comobiditess_cols <- c("admission.comorbid_conditions", "admission.comorbid_conditions2", "admission.comorbid_conditions3", "admission.comorbid_conditions4")
comobidites_info <- convert_to_long(covid_data, comobiditess_cols, 'F')
comobiditess_conditional_string <- append(comobidites_info$list, 'patient_id')
comobiditess_list = comobidites_info$list[comobidites_info$list != "None"]

# select patients comobidites columns
comobidites_data <- comobidites_info$data %>% 
  select(all_of(comobiditess_conditional_string))

# replace unknown with nan for all columns so we can get the 
comobidites_data[] <- lapply(comobidites_data, function(x) gsub("[{}]+", "", replace(x, grepl("Unknown", x), NA)))

# get number of patients with no symptom
na_count = rowSums(is.na(comobidites_data[comobiditess_list]))
no_comobidites_count <- length(na_count[na_count == length(comobiditess_list)])
per_no_comobidites <- round(no_comobidites_count/ nrow(comobidites_data) * 100, 1)

# get percentage of patients without each comorbidity
comobidites_per_df <- data.frame(No = colMeans(is.na(comobidites_data[comobiditess_list])) * 100,
                                 Yes = colMeans(!is.na(comobidites_data[comobiditess_list])) * 100,
                                 count_yes = colSums(!is.na(comobidites_data[comobiditess_list]))) 


# create duplicate dataframe to look at the counts for 
comobidites_count_df <- comobidites_per_df


# add rownames as a column and delete rowname
comobidites_per_df$comorbidity <- rownames(comobidites_per_df)
row.names(comobidites_per_df) <- NULL

# arrange so we can 
comobidites_per_df <- comobidites_per_df %>% 
  arrange(desc(Yes))

# convert data to long format
comobidites_per_df <- reshape2::melt(comobidites_per_df, id.vars = c("comorbidity"), measure.vars = c('Yes', 'No'), na.rm = TRUE, order = FALSE)

# convert Yes/No to factor so it has order
comobidites_per_df$variable = factor(comobidites_per_df$variable, c('No', 'Yes'), ordered = TRUE)

# get ordered data frame by yes
comobidites_order <- comobidites_per_df %>% 
  filter(variable == 'Yes') %>% 
  arrange((value))

# get the ordered list of comorbiditys
comobidites_order <- comobidites_order$comorbidity 

# convert comorbiditys to factor so that it is ordred
comobidites_per_df$comorbidity <- factor(comobidites_per_df$comorbidity, comobidites_order, ordered = TRUE)


comobidites_bar <- comobidites_per_df %>% 
  ggplot(aes(fill=variable, ordered = variable, y=value, x=comorbidity)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Comobidites") +
  ylab("Proportion of patients with comorbidity (%)") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0.2)) +
  scale_fill_manual(values = c("#ffbb00","#52307c")) + 
  theme(plot.title = element_text(hjust = .5, size = 14), 
        axis.title.x = element_text(face="bold", vjust = 1, size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        # legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 8, 53, 2, unit = "pt"),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

comobidites_bar +
  ggsave("comobidites_per_v2.png", width = 20, height = 20, units = "cm")





#################################################
### overlap of top 3 comobidites venn diagram ###
#################################################
list_Hypertension <- comobidites_data %>% 
  filter(comobidites_data$'Hypertension' == 'Hypertension')
list_Hypertension <- list_Hypertension$patient_id

list_Diabetes <- comobidites_data %>% 
  filter(comobidites_data$'Diabetes' == 'Diabetes')
list_Diabetes <- list_Diabetes$patient_id

list_Cardiovascular_diseases <- comobidites_data %>% 
  filter(comobidites_data$'Cardiovascular diseases' == 'Cardiovascular diseases')
list_Cardiovascular_diseases <- list_Cardiovascular_diseases$patient_id


comobidites_venn_data = list(
  "Hypertension" = list_Hypertension,
  "Diabetes" = list_Diabetes,
  "Cardiovascular diseases" = list_Cardiovascular_diseases
)

comobidites_venn_data = euler(comobidites_venn_data, shape = "ellipse")

comobidites_venn <- plot(comobidites_venn_data, 
                         quantities = TRUE, 
                         legend = list(labels = c("Hypertension" , "Diabetes" , "Cardiovascular diseases"), cex = 1.3),
                         fills = list(fill =c("#f9ccff", "#fdffbf", "#bbb3ff"))) 

png("comobidites_venn.png")
comobidites_venn 
dev.off()




## join both symptom charts

ggsave(file = "comobidites_charts_v2.png", grid.arrange(comobidites_bar, comobidites_venn,
                                                     ncol = 1,
                                                     heights = c(1.8, 0.85)), 
       width = 20, 
       height = 28, 
       units = 'cm')







################################
### Level of care barcharts  ###
################################


# add yes no columns for the variable we want to show admission for
level_of_care_df = covid_data %>% 
  mutate("Invasive ventilation" = case_when(admissionAssessment.mechanically_ventilated == 'mechanical_vent' ~ 'Yes', 
                                            TRUE ~ 'No')) %>% 
  mutate("Non-invasive ventilation" = case_when(admissionAssessment.mechanically_ventilated == 'self_vent' ~ 'Yes',
                                                TRUE ~ 'No')) %>% 
  mutate("High oxygen requirement" = case_when(admissionAssessment.fraction_inspired_oxygen > 0.6 ~ 'Yes', 
                                               admissionAssessment.fraction_inspired_oxygen <= 0.6 ~ 'No')) %>% 
  mutate('Vasoactive drugs' = admissionAssessment.cardiovascular_support) %>% 
  mutate('Renal replacement therapy' = admissionAssessment.renal_replacement)

# convert data to long with age as a constant
level_of_care_df_long <- reshape2::melt(level_of_care_df, id.vars = c('admission.age'), measure.vars = c("Invasive ventilation", 'Non-invasive ventilation', 'High oxygen requirement', 'Vasoactive drugs', 'Renal replacement therapy'), na.rm = TRUE, order = FALSE)
# round the age to the nearest 3
level_of_care_df_long$age <- round(level_of_care_df_long$admission.age/3)*3

# get yes count for each varaible
get_yes <- function(data){
  x <- data[grepl("Yes", data)]
  count <- length(x)
  return(count)
}


level_of_care_df_long <- level_of_care_df_long %>% 
  group_by(variable) %>% 
  mutate(count_variable = n()) %>% 
  mutate(count_variable_yes = get_yes(value)) %>% 
  group_by(variable, value, age) %>% 
  mutate(count_yes_by_age = n()) %>% 
  ungroup()


# get max y axis for each variable so we can place the label on the corect position
level_of_care_df_long <- level_of_care_df_long %>% 
  mutate(count_yes_by_age = max(level_of_care_df_long$count_yes_by_age))


level_of_care_df_long$variable <- factor(level_of_care_df_long$variable,levels= c("Invasive ventilation", 'Non-invasive ventilation', '', 'High oxygen requirement', 'Vasoactive drugs', 'Renal replacement therapy'))


# creat the label column
level_of_care_df_long <- level_of_care_df_long %>% 
  mutate(per = round((count_variable_yes / count_variable) * 100, 1)) %>% 
  mutate(per_label = paste0(count_variable_yes, "/", count_variable, ' (', per, '%)', sep = ''))

# create the barchart
level_of_care_chart <- level_of_care_df_long %>% 
  ggplot(aes(fill=value, x= age)) + 
  geom_histogram(position= position_dodge2(width = 1, preserve = "single"), stat="count") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(limits=c(0, 100), expand = c(0, 1)) +
  scale_fill_manual(values = c("#ffbb00","#52307c")) + 
  xlab("Age (years)") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = .5, size = 20), 
        axis.title.x = element_text(face="bold", hjust = 0.5, size = 12),
        axis.title.y = element_text(face="bold", hjust = 0.5, size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        # legend.position = "bottom",
        plot.margin = margin(2, 15, 20, 10, unit = "pt"),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


# add per labels 

level_of_care_chart <- level_of_care_chart + 
  geom_text(label = level_of_care_df_long$per_label, 
            x = 0,
            y = level_of_care_df_long$count_yes_by_age * 0.97,
            hjust = 0,
            size = 3,
            check_overlap = TRUE)


# facet wrap the original chart and add the themes
p <- level_of_care_chart +
  facet_wrap(~variable, ncol = 3, nrow = 2,drop=FALSE) +
  theme(strip.text.x = element_text(size=14, margin = margin( b = 5 , t = 0, l = 0, r =0), face="bold"),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text = element_text(hjust = 0),
        legend.position = c(0.83, 0.75), # c(0,0) bottom left, c(1,1) top-right.
        legend.background = element_rect(fill = "#ffffffaa", colour = NA))








g <- ggplotGrob(p)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-2-2")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]
## move axis closer to panel
grid.newpage()
grid.draw(g)


ggsave("level_of_care_v2.png", plot = g, width = 20, height = 20, units = "cm")






#########################
### Outcome barcharts ###
#########################

# add variables we want to show
outcome_df <- covid_data %>% 
  drop_na(outcome) %>% 
  mutate(invasive_mec = case_when(admissionAssessment.mechanically_ventilated == 'mechanical_vent' ~ 'Yes')) %>% 
  mutate(non_invasive_mec = case_when(admissionAssessment.mechanically_ventilated == 'self_vent' ~ 'Yes')) %>% 
  mutate(all = 'Yes')

# convert data to long format age and outcome as constants
outcome_data_long <- reshape2::melt(outcome_df, id.vars = c('admission.age', 'outcome'), measure.vars = c("all", "invasive_mec", 'non_invasive_mec'), na.rm = TRUE, order = FALSE)

# round age to the nearest 3
outcome_data_long$age <- round(outcome_data_long$admission.age/3)*3

# if outcome is Died then add a label becuase it is on the left of the chart
outcome_data_long <- outcome_data_long %>% 
  mutate(label = case_when(
    outcome == 'Died' & variable == 'invasive_mec' ~ 'Invasive mechanical ventilation',
    outcome == 'Died' & variable == 'non_invasive_mec' ~ 'Non invasive mechanical ventilation',
    outcome == 'Died' & variable == 'all' ~ 'All patients',
    TRUE ~ ''
  )) %>% 
  # get n values for labels
  group_by(variable) %>% 
  mutate(count_variable = n()) %>% 
  group_by(outcome, variable) %>% 
  mutate(count_n = n()) %>% 
  group_by(age,outcome, variable) %>% 
  mutate(count_by_age = n())

# get max y axis for each variable so we can place the label on the corect position
outcome_data_long <- outcome_data_long %>% 
  group_by(variable) %>% 
  mutate(count_by_age = max(count_by_age))

# creat the label column
outcome_data_long <- outcome_data_long %>% 
  mutate(per = round((count_n / count_variable) * 100, 1)) %>% 
  mutate(per_label = paste0(count_n, "/", count_variable, ' (', per, '%)', sep = ''))


outcome_data_long$outcome = factor(outcome_data_long$outcome, c('Died', 'In ICU', 'Discharged'), ordered = TRUE)
# outcome_data_long$age = as.numeric(outcome_data_long$age)

outcome_chart <- outcome_data_long %>% 
  ggplot(aes(fill=outcome, x= age)) + 
  geom_bar(position= position_dodge2(width = 0.9, preserve = "single"), stat="count") +
  scale_y_continuous(expand = c(0, 0.2)) +
  scale_x_continuous(limits=c(0, 100), expand = c(0, 1)) +
  scale_fill_manual(values = c("#52307c","#ffbb00",'#e75480')) + 
  ylab("Number of patients") +
  xlab("Age (years)") +
  coord_cartesian(clip = 'off')  +
  theme(plot.title = element_text(hjust = .5), 
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.ticks = element_blank(),
        legend.position="none",
        plot.margin = margin(30, 2, 2, 2, unit = "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))




outcome_chart <- outcome_chart +
  geom_text(label = outcome_data_long$label, 
            x = -1,
            y = outcome_data_long$count_by_age * 1.1,
            hjust = 0,
            size = 4,
            check_overlap = TRUE,
            fontface ="bold",
            inherit.blank = TRUE) +
  geom_text(label = outcome_data_long$per_label, 
            x = 0,
            y = outcome_data_long$count_by_age * 0.95,
            hjust = 0,
            size = 3,
            check_overlap = TRUE,
            inherit.blank = TRUE)


outcome_chart  +
  facet_grid(variable~outcome, switch="y", scales = "free") +
  theme(strip.text.x = element_text(size=14, margin = margin( b = 20 , t = 0, l = 0, r = 0), face="bold"),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(2, "lines")) +
  ggsave("outcome_v2.png", width = 20, height = 20, units = "cm")




###########################
### Discharged to table ###
###########################


discharged_df <- covid_data %>% 
  filter(dischargeStatus == 'true') %>% 
  mutate(discharge_location = case_when(outcome == 'Died' ~ 'Died in ICU',
                                        TRUE ~ discharge.discharge_destination))

count_discharged <- nrow(discharged_df)

discharge_location_df <- discharged_df %>% 
  count(discharge_location) %>% 
  arrange(desc(n)) %>% 
  mutate(per = round((n / count_discharged) * 100, 1)) %>% 
  mutate('Number of patients (%)' = paste0(n, ' (', per, '%)', sep = ''))







#########################
### Outcome age stats ###
#########################

age_break = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120)
age_labels = c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '< 90')
comobiditess_cols <- c("admission.comorbid_conditions", "admission.comorbid_conditions2", "admission.comorbid_conditions3", "admission.comorbid_conditions4")


alive_patients <- covid_data %>% 
  filter(outcome == 'Discharged') 

alive_patients$age_group <- cut(alive_patients$admission.age, breaks= age_break, right = FALSE, labels = age_labels)
plot(aggregate(alive_patients[, "length_of_stay"], list(alive_patients$age_group), mean))


dead_patients <- covid_data %>% 
  filter(outcome == 'Died')

comobidites_info <- convert_to_long(dead_patients, comobiditess_cols, 'F')
comobidites_list <- comobidites_info$list[comobidites_info$list != "None"]
dead_patients[comobidites_list] <- lapply(comobidites_info$data[comobidites_list], function(x) gsub("[{}]+", "", replace(x, grepl("Unknown", x), NA)))
x <- rowSums(!is.na(dead_patients[comobidites_list]))
x <- ifelse(x > 0, 1, 0)
count_dead_comobidity <- length(x) - sum(x)
per_dead_comobidty <- 100 -mean(x) * 100




dead_patients$age_group <- cut(dead_patients$admission.age, breaks= age_break, right = FALSE, labels = age_labels)
lines(aggregate(dead_patients[, "length_of_stay"], list(dead_patients$age_group), mean))
scatter.smooth(covid_data$admission.age,covid_data$length_of_stay)
age_stat_dead <- summary(dead_patients$admission.age)


mec_vent_discharge_data <- covid_data %>% 
  filter(outcome == 'Died' & admissionAssessment.mechanically_ventilated == 'mechanical_vent')


age_stat_mec_dead <- summary(mec_vent_discharge_data$admission.age)


mec_vent_data <- covid_data %>% 
  filter(admissionAssessment.mechanically_ventilated == 'mechanical_vent')


self_vent_data <- covid_data %>% 
  filter(admissionAssessment.mechanically_ventilated == 'self_vent')

non_mec_vent_data <- covid_data %>% 
  filter(is.na(admissionAssessment.mechanically_ventilated))

t.test(mec_vent_data$admission.age[!is.na(mec_vent_data$admission.age)], non_mec_vent_data$admission.age[!is.na(non_mec_vent_data$admission.age)])
t.test(mec_vent_data$length_of_stay[!is.na(mec_vent_data$length_of_stay)], 4.8)

x <- covid_data %>% 
  filter(length_of_stay > 0 & length_of_stay < 100) %>% 
  select(length_of_stay)

y <- self_vent_data %>% 
  filter(length_of_stay > 0 & length_of_stay < 30) %>% 
  select(length_of_stay)

summary(y)

library("ggpubr")

clean_c_df <- covid_data %>% 
  select(admission.age, length_of_stay) %>%
  drop_na(length_of_stay) %>% 
  filter(!(abs(length_of_stay - median(length_of_stay)) > 2*sd(length_of_stay)))

ggscatter(clean_c_df, x = 'admission.age', y = "length_of_stay", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


#####################################
###              Table            ###
#####################################

library(arsenal)
library(mgsub)

comobiditess_cols <- c("admission.comorbid_conditions", "admission.comorbid_conditions2", "admission.comorbid_conditions3", "admission.comorbid_conditions4")
comobidites_info <- convert_to_long(covid_data, comobiditess_cols, 'F')
comobiditess_conditional_string <- append(comobidites_info$list, 'patient_id')
comobiditess_list = comobidites_info$list[comobidites_info$list != "None"]

table_df <- comobidites_info$data %>% 
  mutate_all(funs(str_replace(., "Unknown", "No"))) 

comobidites_order_as <- rev(comobidites_order)
comobidites_order_as <- comobidites_order_as[!comobidites_order_as %in% c('Diabetes with complications')]
comobidites_order_full <- append(comobidites_order_as, c('Diabetes with complications'))
d_pos <- match(c('Diabetes'), comobidites_order_as)
diabetes_pos <- c(d_pos[1])
idx <- c( seq_along(comobidites_order_as), diabetes_pos+0.5 )
comobidites_order_as <- comobidites_order_full[order(idx)]



table_df<- table_df %>% 
  select(append(c('admission.gender', 'admission.age', 'age_group'), comobidites_order_as))

table_df <- data.frame(lapply(table_df, function(x) {
  mgsub::mgsub(x, comobiditess_list, rep('Yes', length(comobiditess_list)))
}), check.names = FALSE)


# create new column named age_group by grouping age by every 5 years
age_break = c(0, 18, 40, 50, 60, 70, 80, 120)
age_labels = c('0-18', '19-39', '40-49', '50-59', '60-69', '70-79', '> 80')
table_df$admission.age <- as.numeric(levels(table_df$admission.age))[table_df$admission.age]
table_df$age_group <- cut(table_df$admission.age, breaks= age_break, right = FALSE, labels = age_labels)

my_labels <- list(
  admission.age = "Age at admission",
  age_group = "Age"
)

b <- tableby(admission.gender ~ ., data=table_df,
             numeric.stats=c("medianq1q3"), numeric.test="kwt", cat.simplify = TRUE)

bb <- data.frame(summary(b, labelTranslations = my_labels)) %>% 
  mutate_all(funs(str_replace_all(., "&nbsp;", "    "))) %>% 
  mutate_all(funs(str_replace_all(., "\\*", "")))


# tab3 <- tableby(arm ~ sex + age, data=mockstudy, test=FALSE, total=FALSE, 
#                 numeric.stats=c("median","q1q3"), numeric.test="kwt")















#################################################
###             Assosiations hazard          ###
#################################################


# new age groupings 
age_break = c(0, 55, 65, 75, 120)
age_labels = c('< 55', '55-64', '65-74', '>= 75')


# convert comobidties to long format
comobiditess_cols <- c("admission.comorbid_conditions", "admission.comorbid_conditions2", "admission.comorbid_conditions3", "admission.comorbid_conditions4")



covid_data_as <- covid_data %>% 
  mutate_at(comobiditess_cols, list(~if_else(. %in% c('Renal failure, Mild', 'Renal failure, Moderate to severe'), 'Renal failure', ., .))) %>% 
  mutate_at(comobiditess_cols, list(~if_else(. %in% c('Respiratory disease, Mild', 'Respiratory disease, Severe moderate'), 'Respiratory disease', ., .)))


comobidites_info <- convert_to_long(covid_data_as, comobiditess_cols, 'F')
comobidites_list <- comobidites_info$list[comobidites_info$list != "None"]


assosiation_df <- comobidites_info$data

assosiation_df <- assosiation_df %>% 
  mutate_at(comobidites_list, list(~if_else(. %in% comobidites_list, 'Yes', 'No', 'No'))) %>% 
  mutate_at(comobidites_list, list(~factor(., levels = c('No', 'Yes'))))


# Add column outcome_binary
assosiation_df <- assosiation_df %>% 
  drop_na(outcome) %>% 
  mutate(outcome_binary = case_when(outcome == 'Died' ~ 1,
                                    outcome == 'Discharged' ~ 0,
                                    outcome == 'In ICU' ~ 0)) %>% 
  mutate("Age_at_admission_years" = cut(admission.age, breaks= age_break, right = FALSE, labels = age_labels)) %>% 
  mutate('Sex_at_birth' = fct_rev(factor(admission.gender))) %>%
  mutate('Mechanically_ventilated' = factor(case_when(admissionAssessment.mechanically_ventilated == 'mechanical_vent' ~ 'Yes',
                                                      TRUE ~ 'No'))) %>% 
  mutate(Cardiovascular_support = factor(admissionAssessment.cardiovascular_support)) %>% 
  mutate('One_or_more_comobidites' = factor(ifelse(rowSums(assosiation_df[comobidites_list] == 'Yes') > 0, 'Yes', 'No')))


com_count = data.frame(Yes = colSums((assosiation_df[comobidites_list]) == 'Yes'))
com_count$comorbidity <- rownames(com_count)


comobidites_as <-  com_count %>% 
                arrange(desc(Yes)) %>% 
                filter(Yes > 4) %>% 
                select(comorbidity)

dependent <- 'Surv(time = assosiation_df$timef, event = assosiation_df$outcome_binary)'
explanatory <- append(c('Age_at_admission_years', 'Sex_at_birth'), comobidites_as$comorbidity)















# Top ten como and mec vent 
# Number in dataframe = 760, Number in model = 760, Missing = 0, Number of events = 261, Concordance = 0.677 (SE = 0.016), R-squared = 0.123( Max possible = 0.988), Likelihood ratio test = 99.921 (df = 16, p = 0.000)

# NO mec vent top ten como
# Number in dataframe = 760, Number in model = 760, Missing = 0, Number of events = 261, Concordance = 0.590 (SE = 0.018), R-squared = 0.042( Max possible = 0.988), Likelihood ratio test = 32.296 (df = 15, p = 0.006)




######################
### functions used ###
######################



hr_plot = function(.data, dependent, explanatory, factorlist=NULL, coxfit=NULL,
                   remove_ref = FALSE,
                   breaks=NULL, column_space=c(-0.5, 0, 0.5),
                   dependent_label = "Survival", 
                   prefix = "", suffix = ": HR (95% CI, p-value)",
                   table_text_size = 5,
                   title_text_size = 18,
                   plot_opts = NULL, table_opts = NULL, ...){
  
  .data <- assosiation_df
  dependent <- dependent
  explanatory <- explanatory
  factorlist=NULL
  coxfit=NULL
  remove_ref = FALSE
  breaks=NULL
  column_space=c(-0.5, 0, 0.5)
  dependent_label = "Survival"
  prefix = ""
  suffix = ": HR (95% CI, p-value)"
  table_text_size = 7.5
  title_text_size = 18
  plot_opts = NULL
  table_opts = NULL
  
  names(.data) <- gsub(x = names(.data), pattern = " ", replacement = "_")  
  explanatory <- gsub(x = explanatory, pattern = " ", replacement = "_")  
  
  names(.data) <- gsub(x = names(.data), pattern = ",", replacement = "")  
  explanatory <- gsub(x = explanatory, pattern = ",", replacement = "")  
  
  
  .data <- .data %>% 
    select(append(explanatory, c('timef', 'outcome_binary')))
  

  explanatory_str = paste(explanatory, collapse = ' + ')
  
  full_str = paste(dependent ,' ~ ' ,exp_str, sep = '')
  
  base_model = coxph(eval(parse(text = full_str)), data = .data)
  
  
  for (var in explanatory){
    exp = explanatory[explanatory != var]
    exp_str = paste(exp, collapse = ' + ')
    dep = dependent
    
    full_str = paste(dep ,' ~ ' ,exp_str, sep = '')
    
    temp_model = coxph(eval(parse(text = full_str)), data = .data)
    
    print(var)
    print((anova(base_model, temp_model, test = "LRT"))$`P(>|Chi|)`)
    
    
  }
  
  
  
  v<-.data %>% 
    finalfit(dependent, explanatory, metrics = TRUE)
  
  
  requireNamespace("ggplot2")
  
  # Generate or format factorlist object
  if(!is.null(factorlist)){
    if(is.null(factorlist$fit_id)) stop("summary_factorlist function must include fit_id=TRUE")
  }
  
  if(is.null(factorlist)){
    factorlist = summary_factorlist(.data, dependent, explanatory, fit_id=TRUE)
  }
  
  if(remove_ref){
    factorlist = factorlist %>%  
      dplyr::mutate(label = ifelse(label == "", NA, label)) %>% 
      tidyr::fill(label) %>% 
      dplyr::group_by(label) %>%
      dplyr::filter(dplyr::row_number() != 1 | 
                      dplyr::n() > 2 |
                      levels %in% c("Mean (SD)", "Median (IQR)")
      )%>% 
      rm_duplicate_labels()
  }
  
  # Specify breaks if provided
  if(is.null(breaks)){
    breaks = scales::pretty_breaks()
  }
  
  # Extract totals (this is CPH specific due to how summary_factorlist works)
  factorlist$Total = as.numeric(stringr::str_extract(as.character(factorlist$all), "^[:digit:]*"))
  
  # Fill in total for continuous variables
  factorlist$Total[factorlist$levels == "Mean (SD)" | factorlist$levels == "Median (IQR)"] = dim(.data)[1]
  
  # For continuous variables, remove level label
  drop = grepl("Mean \\(SD\\)|Median \\(IQR\\)", factorlist$levels)
  factorlist$levels[drop] = "-"
  
  factorlist$all = NULL
  
  # Generate or format glm
  if(is.null(coxfit)){
    coxfit = coxphmulti(.data, dependent, explanatory)
  }
  coxfit_df_c = fit2df(coxfit, condense = TRUE, estimate_suffix = " (multivariable)", 
                       estimate_name = "HR", exp = TRUE)
  coxfit_df = fit2df(coxfit, condense = FALSE, 
                     estimate_name = "HR", exp = TRUE)
  
  # Merge
  df.out = finalfit_merge(factorlist, coxfit_df_c)
  df.out = finalfit_merge(df.out, coxfit_df, ref_symbol = "1.0")
  
  # Remove unwanted lines, where there are more variables in model than wish to display.
  # Note merge function in summarizer merge is now `all` rather than `all.x` as wish to preserve interactions
  # These not named in factorlist, creating this problem. Interactions don't show on plot.
  if (any(
    is.na(df.out$label)
  )
  ){
    remove_rows = which(is.na(df.out$label)) # This row doesn't work when is.na == FALSE, hence if()
    df.out = df.out[-remove_rows,]
  } else {
    df.out
  }
  
  # Fix order
  df.out$levels = as.character(df.out$levels)
  df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])
  
  
  
  df.out[df.out==""]<-NA
  
  
  # add label columns
  df.out <- df.out %>% 
    mutate(hr_ci = glue('{round(as.numeric(df.out$HR), 2)} ({round(df.out$L95, 2)} to {round(df.out$U95, 2)})')) %>% 
    mutate(p_label = case_when(p < 0.001 ~ '< 0.001',
                               TRUE ~ as.character(round(p, 3)))) %>% 
    mutate(variable = str_replace_all(label, '_', ' ')) %>% 
    fill(variable, variable, .direction = 'down')
  
  # drop reference values 
  df.out <- df.out %>% 
    drop_na(p)
  
  c_x = ''
  new_var = c()
  
  for (x in df.out$variable) {
    if (x == c_x) {
      print(x)
      x = ''
    } else {
      c_x = x
    }
    new_var <- append(new_var, x)
  }
  
  df.out$variable = new_var
  
  # Plot
  g1 = ggplot(df.out, aes(x = as.numeric(HR), xmin = as.numeric(L95), xmax  = as.numeric(U95),
                          y = fit_id))+
    geom_point(aes(size = Total), shape=22, fill="darkblue")+
    geom_errorbarh(height=0.2) +
    geom_vline(xintercept = 1, colour = "black")+
    scale_x_continuous(trans="log10", breaks= breaks)+
    xlab("")+ 
    theme_classic(14)+
    theme(axis.title.x = element_text(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 18),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="none")
  
  t1 = ggplot(df.out, aes(x = as.numeric(HR), y = fit_id))+
    annotate("text", x = column_space[1], y =  df.out$fit_id, label=df.out$variable, hjust=0, size=table_text_size)+
    annotate("text", x = column_space[2], y =  df.out$fit_id, label=df.out$levels, hjust=1, size=table_text_size)+
    theme_classic(14)+
    theme(axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())
  
  
  t2 = ggplot(df.out, aes(x = as.numeric(HR), y = fit_id))+
    annotate("text", x = column_space[1], y =  df.out$fit_id, label=df.out$hr_ci, hjust=0.5, size=table_text_size)+
    theme_classic(14)+
    theme(axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())
  
  t3 = ggplot(df.out, aes(x = as.numeric(HR), y = fit_id))+
    annotate("text", x = column_space[1], y =  df.out$fit_id, label=df.out$p_label, hjust=0.5, size=table_text_size)+
    theme_classic(14)+
    theme(axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())
  
  # Add optional arguments
  g1 = g1 + plot_opts
  t1 = t1 + table_opts
  t2 = t2 + table_opts
  t3 = t3 + table_opts
  
  # Add dependent name label
  title = 	plot_title(.data = .data, dependent = dependent, dependent_label = dependent_label, prefix = prefix, suffix = suffix)
  
  plt = gridExtra::grid.arrange(arrangeGrob(t1, top = textGrob('\n \n \n', gp=gpar(fontface="bold", fontsize = 24))), 
                                arrangeGrob(g1, top = textGrob('\n Hazard ratio \n (95% CI) \n', gp=gpar(fontface="bold", fontsize = 24))), 
                                arrangeGrob(t2, top = textGrob('\n Hazard ratio \n (95% CI) \n', gp=gpar(fontface="bold", fontsize = 24))), 
                                arrangeGrob(t3, top = textGrob('\n P \n Value \n', gp=gpar(fontface="bold", fontsize = 24))), ncol=4, widths = c(2, 1.5, 1, 1))
  
  
  plt <- plt + grid.roundrect(width = .99,
                              height = .99,
                              gp = gpar(lwd = 2, col = "black", fill = NA),
                              r=unit(0.01, "snpc")) 
  
  plt <- plt + grid.lines(y = c(0.83, 0.83),x = c(0.005, 0.995), 
                          gp = gpar(col = "black", lwd = 2.5))
  
  ggsave(file="plt_v2.png", plt, width = 40, height = 25, units = "cm")
  
  print('Yesssssssssss!!!!!!!!!!!!!!!!!')
}













convert_to_long <- function(data, cols, available = 'T'){
  
  # Create dataframe by converting all cols into long dataframe
  
  symptoms_data <- melt(data, id.vars = c("patient_id"), measure.vars = cols, na.rm = TRUE) %>% 
    mutate(measurement = 'value')
  
  # Get list of cols_values to select coloumns later.
  
  symptoms_list <- symptoms_data %>% 
    distinct(value)
  
  symptoms_list <- as.vector(symptoms_list)
  
  
  print(class(symptoms_list))
  print(symptoms_list)
  
  # Convert into string split by or operator
  
  symptoms_conditional_string <- paste(symptoms_list$value, collapse = '|')
  
  # then convert back to wide with all cols_value as coloumns
  
  symptoms_data <- symptoms_data[, !(names(symptoms_data) %in% c("variable"))] %>%
    distinct()
  symptoms_data <- reshape2::dcast(symptoms_data, patient_id ~ value, value.var="value")
  
  
  
  # merge back into the full data frame using patient_id
  
  data <- merge(data, symptoms_data, all.x = TRUE)
  
  if (available == 'T'){
    
    data[symptoms_list$value][is.na(data[symptoms_list$value])] <- "Unknown"
    
    
  } else {
    
    data[symptoms_list$value][is.na(data[symptoms_list$value])] <- "Unknown"
    
  }
  
  return(list("data" = data, "new_col_list" = symptoms_conditional_string, "list" = symptoms_list$value))
  
}









convert_to_long_sym <- function(data, cols){
  
  # Create dataframe by converting all cols into long dataframe
  
  symptoms_data <- melt(data, id.vars = c("patient_id", 'sari_form'), measure.vars = cols, na.rm = TRUE) %>% 
    mutate(measurement = 'value')
  
  # Get list of cols_values to select coloumns later.
  
  symptoms_list <- symptoms_data %>% 
    distinct(value)
  
  symptoms_list <- as.vector(symptoms_list)
  
  
  print(class(symptoms_list))
  print(symptoms_list)
  
  # Convert into string split by or operator
  
  symptoms_conditional_string <- paste(symptoms_list$value, collapse = '|')
  
  # then convert back to wide with all cols_value as coloumns
  
  symptoms_data <- symptoms_data[, !(names(symptoms_data) %in% c("variable"))] %>%
    distinct()
  symptoms_data <- reshape2::dcast(symptoms_data, patient_id + sari_form ~ value, value.var="value")
  
  
  
  # merge back into the full data frame using patient_id
  
  data <- merge(data, symptoms_data, all.x = TRUE)
  
  data <- data %>% 
    mutate_at(symptoms_list$value, funs(case_when(
      sari_form == 'No' ~ "Unknow",
      is.na(.) ~ "No",
      TRUE ~ 'Yes'
    )))
  # 
  #     
  #   data[symptoms_list$value][is.na(data[symptoms_list$value])] <- "Unknown"
  #   
  #   data[symptoms_list$value][is.na(data[symptoms_list$value])] <- "Unknown"
  
  
  
  return(list("data" = data, "new_col_list" = symptoms_conditional_string, "list" = symptoms_list$value))
}



singal_comobidity <- function(data, cols){
  x <- rowSums(!is.na(data[comobidites_list]))
  x <- ifelse(x > 0, 'Yes', 'No')
  return(x)
}

