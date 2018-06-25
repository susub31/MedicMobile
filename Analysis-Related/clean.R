
## Objective: Integrate and clean data for analyses
rm(list = ls())

#load R packages
library(dplyr)
library(openxlsx)
library(sqldf)

#list directory and file paths for inputs
data <- "C:/Users/ngcou/OneDrive/Medic Mobile project/Input/Data-Latest"
assessment_table = "Assessment-Table.csv"
case_table = "Patient_Case-Table.csv"
trtfu_table = "Treatment_Follow_Up-Table.csv"
patient_table = "Patient_Extended-View.csv"
referral_table = "Referral_Follow_Up-Table.csv"

#set input directory and input tables
setwd(data)
assessment_table <- read.csv(assessment_table, header=TRUE)
case_table <- read.csv(case_table, header=TRUE)
trtfu_table <- read.csv(trtfu_table, header=TRUE)
patient_table <- read.csv(patient_table, header=TRUE)
referral_table <- read.csv(referral_table, header=TRUE)

#rename variables to indicate which sheet each indicator is coming from
colnames(assessment_table) <- paste("a", colnames(assessment_table), sep = ".")
colnames(case_table) <- paste("c", colnames(case_table), sep = ".")
colnames(trtfu_table) <- paste("t", colnames(trtfu_table), sep = ".")
colnames(patient_table) <- paste("p", colnames(patient_table), sep = ".")
colnames(referral_table) <- paste("r", colnames(referral_table), sep = ".")

#rename case id and patient id variables for merging
colnames(assessment_table)[colnames(assessment_table)=="a.case_id"] <- "case_id"
colnames(case_table)[colnames(case_table)=="c.case_id"] <- "case_id"
colnames(trtfu_table)[colnames(trtfu_table)=="t.case_id"] <- "case_id"
colnames(assessment_table)[colnames(assessment_table)=="a.patient_id"] <- "patient_id"
colnames(case_table)[colnames(case_table)=="c.patient_id"] <- "patient_id"
colnames(patient_table)[colnames(patient_table)=="p.patient_id"] <- "patient_id"
colnames(trtfu_table)[colnames(trtfu_table)=="t.patient_id"] <- "patient_id"

## I. Combine data tables

# Step 1: create integrated tables for patients with treatment follow-up (not referral follow-up)
# Combine assessment, case, and patient tables
allcases <- merge(assessment_table, case_table, by=c("case_id","patient_id"), no.dups=TRUE)
allcases <- merge(allcases,patient_table, by="patient_id", all.x=TRUE, no.dups=TRUE)

# add indicator to indicate whether a case is in the treatment follow-up table
allcases$followup[allcases$case_id %in% trtfu_table$case_id] <- 1

# edit variable to identify when case assessed to determine wet or dry season
allcases$a.reported <- substr(allcases$a.reported, 1, 10) 
allcases$a.reported <- as.Date(allcases$a.reported)
allcases$month_assessed <- as.numeric(format(allcases$a.reported, "%m"))
allcases$wetseason <- ifelse(allcases$month_assessed>=5 & allcases$month_assessed<11, 1,0)

# show frequency of assessment cases by month of assessment
month_assess_cases <- as.data.frame(count(allcases, month_assessed))
colnames(month_assess_cases)[which(names(month_assess_cases)=="month_assessed")] <- "month_reported"
colnames(month_assess_cases)[which(names(month_assess_cases)=="n")] <- "month_assess_casecount"
season_assess_cases <- as.data.frame(count(allcases, wetseason))

# show frequency of follow-up cases by month of assessment
month_fu_cases <- as.data.frame(table(allcases$month_assessed, allcases$followup))
season_fu_cases <- as.data.frame(table(allcases$wetseason, allcases$followup))
month_fu_cases <- month_fu_cases[c("Var1", "Freq")]
colnames(month_fu_cases)[which(names(month_fu_cases)=="Var1")] <- "month_reported"
colnames(month_fu_cases)[which(names(month_fu_cases)=="Freq")] <- "month_fu_casecount"

# add % follow-up among all assessed cases by month of assessment
month_cases <- merge(month_assess_cases,month_fu_cases, by=c("month_reported"))
month_cases$month_fu_rate <- month_cases$month_fu_casecount/month_cases$month_assess_casecount

# Combine treatment follow-up data with assessment and other case information
trtfu_cases <- merge(trtfu_table, allcases, by=c("case_id","patient_id"))

write.csv(trtfu_cases, "treatment-cases.csv")

# Step 2: Create data table with patients who have ICCM diagnoses 
trtfu_ICCM <- trtfu_cases[which(trtfu_cases$t.treat_for_malaria=="true" | trtfu_cases$t.treat_for_ari=="true" | trtfu_cases$t.treat_for_diarrhea=="true"),]

# Step 3: only take first event of ICCM cases
trtfu_ICCM_first <- trtfu_ICCM[which(trtfu_ICCM$t.event_number==1),]

# export
write.csv(trtfu_ICCM_first,"treated-analyze.csv")

## II. Clean combined data table

# create a poor outcome variable for "no change" or "aggravated" health outcome
trtfu_ICCM_first$pooroutcome <- ifelse(trtfu_ICCM_first$t.how_disease_progressing=="aggravated" | trtfu_ICCM_first$t.how_disease_progressing=="no_change", 1,
                                       ifelse(trtfu_ICCM_first$t.how_disease_progressing=="cured"| trtfu_ICCM_first$t.how_disease_progressing=="improved",0,trtfu_ICCM_first$t.how_disease_progressing))

# create a variable for "aggravated" health outcome
trtfu_ICCM_first$aggravated <- ifelse(trtfu_ICCM_first$t.how_disease_progressing=="aggravated", 1,
                                     ifelse(is.na(trtfu_ICCM_first$t.how_disease_progressing),NA,0))

# create binary diagnoses indicators
trtfu_ICCM_first$ari <- ifelse(trtfu_ICCM_first$t.treat_for_ari=="true",1,0)
trtfu_ICCM_first$diarrhea <- ifelse(trtfu_ICCM_first$t.treat_for_diarrhea=="true",1,0)
trtfu_ICCM_first$malaria <- ifelse(trtfu_ICCM_first$t.treat_for_malaria=="true",1,0)

# create a variable for counting the number of diagnoses (max 3)
diagnoses <- trtfu_ICCM_first[c("ari","malaria","diarrhea")]
trtfu_ICCM_first$num_diagnoses <- rowSums(diagnoses)

# create female indicator
  trtfu_ICCM_first$female <- ifelse(trtfu_ICCM_first$p.sex=="Femme" | trtfu_ICCM_first$p.sex=="femme", 1,
                                  ifelse(trtfu_ICCM_first$p.sex=="Homme" | trtfu_ICCM_first$p.sex=="homme",0,NA)) 
# create an age variable
trtfu_ICCM_first$t.reported <- as.Date(trtfu_ICCM_first$t.reported)
trtfu_ICCM_first$p.date_of_birth <- as.Date(trtfu_ICCM_first$p.date_of_birth)
trtfu_ICCM_first$age <- (trtfu_ICCM_first$t.reported - trtfu_ICCM_first$p.date_of_birth)/365.25

# create a variable for counting protocol errors
protocol_errors <- trtfu_ICCM_first[c("c.act_based_combination_without_positive_tdr", "c.acute_respiratory_infection_without_24h_follow_up","c.acute_respiratory_infection_without_48h_follow_up",
                                      "c.acute_respiratory_infection_without_5_day_follow_up", "c.amoxicillin_without_pneumonia","c.danger_signs_without_referral",
                                      "c.diarrhea_without_5day_follow_up","c.diarrhea_without_ors","c.diarrhea_without_zinc","c.fever_without_tdr",
                                      "c.incorrect_dosage_of_act_based_combination","c.incorrect_dosage_of_albendazole","c.incorrect_dosage_of_amoxicillin",
                                      "c.incorrect_dosage_of_paracetamol","c.incorrect_dosage_of_vitamin_a","c.incorrect_dosage_of_zinc",
                                      "c.malaria_without_24h_follow_up","c.malaria_without_48h_follow_up","c.malaria_without_72h_follow_up",
                                      "c.muac_yellow_no_malnutrition_follow_up","c.pneumonia_without_amoxicillin","c.severe_diarrhea_without_referral","c.symptoms_of_malaria_without_tdr")]

protocol_errors[] <- lapply(protocol_errors, gsub, pattern = "true", replacement = "1", fixed = TRUE)
protocol_errors[] <- lapply(protocol_errors, gsub, pattern = "false", replacement = "0", fixed = TRUE)
protocol_errors[] <- lapply(protocol_errors,as.numeric)

trtfu_ICCM_first$num_protocolerrors <-rowSums(protocol_errors)    

# create protocol error rate by CHW (number of cases with errors/number of cases per CHW)
# sum protocol errors by CHW
CHW_errors <- aggregate(trtfu_ICCM_first$num_protocolerrors, by=list(Category=trtfu_ICCM_first$t.chw_id), FUN=sum)
colnames(CHW_errors)[which(names(CHW_errors)=="Category")] <- "t.chw_id"
colnames(CHW_errors)[which(names(CHW_errors)=="x")] <- "CHW_num_errors"

# count the number of cases by CHW
CHW_cases <- count(trtfu_ICCM_first, t.chw_id)
colnames(CHW_cases)[which(names(CHW_cases)=="n")] <- "CHW_num_cases"
# calculate rate of CHW by dividing number of errors by number of cases
CHW_error_rate <- merge(CHW_cases,CHW_errors, by="t.chw_id", no.dups=TRUE)
CHW_error_rate$CHW_error_rate <- CHW_error_rate$CHW_num_errors/CHW_error_rate$CHW_num_cases
# merge CHW error rate back into combined data table
trtfu_ICCM_first <- merge(trtfu_ICCM_first, CHW_error_rate, by="t.chw_id")


# create variable to indicate month of case <- as.character(trtfu_ICCM_first$t.reported)
trtfu_ICCM_first$month_reported <- as.numeric(format(trtfu_ICCM_first$t.reported, "%m"))

# create variable to indicate dry or wet season when case seen
trtfu_ICCM_first$wetseason <- ifelse(trtfu_ICCM_first$month_reported>=5 & trtfu_ICCM_first$month_reported<11, 1,0)

# add indicators for caseload assessed and with follow-up by month
trtfu_ICCM_first <- merge(trtfu_ICCM_first, month_cases, by="month_reported")

# create indicator for monthly CHW-specific caseload
CHW_month_cases <- count(trtfu_ICCM_first, t.chw_id,month_reported)
# merge in number of cases by month for each CHW
trtfu_ICCM_first <- merge(trtfu_ICCM_first, CHW_month_cases, by=c("t.chw_id","month_reported"))
colnames(trtfu_ICCM_first)[which(names(trtfu_ICCM_first)=="n")]<- "CHW_monthly_cases"

# make when illness began variable a numeric variable with hour as an unit(~half a day=12 hours)
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_1"] <- 0
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_2"] <- 12
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_3"] <- 24
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_4"] <- 36
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_5"] <- 48
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_6"] <- 60
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_7"] <- 72
trtfu_ICCM_first$illness_onset[trtfu_ICCM_first$a.when_illness_began=="c_when_illness_8"] <- 84

# make a new variable of time since illness onset based on Medic Mobile coding

trtfu_ICCM_first$time_illness_onset[(trtfu_ICCM_first$a.when_illness_began=="c_when_illness_1" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_2" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_3" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_4" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_1" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_2" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_3" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_1" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_2" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_3" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_1" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_2" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_3" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4")] <- "within_24"

trtfu_ICCM_first$time_illness_onset[(trtfu_ICCM_first$a.when_illness_began=="c_when_illness_5" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_6" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_4" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_5" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_4" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_5" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_3" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_5" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") 
                                      ] <- "within_25_to_48"

trtfu_ICCM_first$time_illness_onset[(trtfu_ICCM_first$a.when_illness_began=="c_when_illness_7" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_6" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_7" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_6" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_7" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") | (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_6" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") |
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_7" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") ] <- "within_49_to_72"

trtfu_ICCM_first$time_illness_onset[(trtfu_ICCM_first$a.when_illness_began=="c_when_illness_8" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_1") |                                       
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_8" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_2") | 
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_8" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_3") | 
                                      (trtfu_ICCM_first$a.when_illness_began=="c_when_illness_8" & trtfu_ICCM_first$a.when_assessed=="c_assessment_time_4") ] <- "beyond_72"

# make a new binary variable that flags if a case was seen more than 24 hours after illness onset
trtfu_ICCM_first$illness_onset_beyond24 <- ifelse(trtfu_ICCM_first$time_illness_onset=="within_24", 0, 1)


## patient history analyses
#count number of cases per patient
case_freq <- count(trtfu_ICCM_first, patient_id)
colnames(case_freq)[which(names(case_freq)=="n")] <- "case_freq"
trtfu_ICCM_first <- merge(trtfu_ICCM_first, case_freq, by="patient_id")

## derive protocol error types

# categorize protocol errors by diagnoses
trtfu_ICCM_first$ari_error <- ifelse(trtfu_ICCM_first$c.acute_respiratory_infection_without_24h_follow_up=="true" | trtfu_ICCM_first$c.acute_respiratory_infection_without_48h_follow_up=="true" | trtfu_ICCM_first$c.acute_respiratory_infection_without_5_day_follow_up=="true" |
                                 trtfu_ICCM_first$c.incorrect_dosage_of_amoxicillin=="true" | trtfu_ICCM_first$c.pneumonia_without_amoxicillin=="true", 1, 0)
trtfu_ICCM_first$diarrhea_error <- ifelse(trtfu_ICCM_first$c.diarrhea_without_5day_follow_up=="true" | trtfu_ICCM_first$c.diarrhea_without_ors=="true" | trtfu_ICCM_first$c.diarrhea_without_zinc=="true" |
                                       trtfu_ICCM_first$c.incorrect_dosage_of_zinc=="true" | trtfu_ICCM_first$c.severe_diarrhea_without_referral=="true",1,0)
trtfu_ICCM_first$malaria_error <- ifelse(trtfu_ICCM_first$c.malaria_without_24h_follow_up=="true" | trtfu_ICCM_first$c.malaria_without_48h_follow_up=="true" | trtfu_ICCM_first$c.malaria_without_72h_follow_up=="true" |
                                       trtfu_ICCM_first$c.incorrect_dosage_of_act_based_combination=="true" | trtfu_ICCM_first$c.symptoms_of_malaria_without_tdr=="true", 1, 0)

#categorize protocol error by error type
trtfu_ICCM_first$followup_error <- ifelse(trtfu_ICCM_first$c.acute_respiratory_infection_without_24h_follow_up=="true" | trtfu_ICCM_first$c.acute_respiratory_infection_without_48h_follow_up=="true" | trtfu_ICCM_first$c.acute_respiratory_infection_without_5_day_follow_up=="true"|
                                            trtfu_ICCM_first$c.diarrhea_without_5day_follow_up=="true"|trtfu_ICCM_first$c.malaria_without_24h_follow_up=="true" | trtfu_ICCM_first$c.malaria_without_48h_follow_up=="true" | trtfu_ICCM_first$c.malaria_without_72h_follow_up=="true", 1,0)
trtfu_ICCM_first$missed_treatment <- ifelse(trtfu_ICCM_first$c.diarrhea_without_ors=="true" |trtfu_ICCM_first$c.diarrhea_without_zinc=="true" | trtfu_ICCM_first$c.pneumonia_without_amoxicillin=="true"|
                                                trtfu_ICCM_first$c.severe_diarrhea_without_referral=="true" | trtfu_ICCM_first$c.symptoms_of_malaria_without_tdr=="true",1,0)
trtfu_ICCM_first$dosage_error <- ifelse(trtfu_ICCM_first$c.incorrect_dosage_of_act_based_combination=="true" |trtfu_ICCM_first$c.incorrect_dosage_of_albendazole=="true"|trtfu_ICCM_first$c.incorrect_dosage_of_amoxicillin=="true"|
                                          trtfu_ICCM_first$c.incorrect_dosage_of_paracetamol=="true"| trtfu_ICCM_first$c.incorrect_dosage_of_vitamin_a=="true"| trtfu_ICCM_first$c.incorrect_dosage_of_zinc=="true",1,0)


####################
# export cleaned dataset

## export
write.csv(trtfu_ICCM_first, "treated-analyze-clean-updated.csv")
