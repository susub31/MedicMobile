rm(list = ls())

# download and load packages required
library(broom)
library(miceadds)
library(multiwayvcov)

# set directory
input <- "C:/Users/ngcou/OneDrive/Medic Mobile project/Input/Data-Latest"
setwd(input)

# read in cleaned csv file
treated <- read.csv("treated-analyze-clean-updated.csv", header=TRUE)

## factor variable for logistic regression (do this before running any logistic regression models)
treated$pooroutcome <- factor(treated$pooroutcome)

##########################################################################
####### II. firsst round analyses of basic models ################

#### 1. conduct first round analyses of single variate models #####

### logistic regression analyses on poor health outcome 

# logit regression w/ patient demographic factors
treated$female<-factor(treated$female)
logit_sex <- glm(pooroutcome ~ female, data=treated, family="binomial")
logit_sex_fit <- tidy(logit_sex)

logit_age <- glm(pooroutcome ~ age, data=treated, family="binomial")
logit_age_fit <- tidy(logit_age)

# logit regression w/ diagnoses
treated$ari <- factor(treated$ari)
treated$diarrhea <- factor(treated$diarrhea)
treated$malaria <- factor(treated$malaria)
logit_ari <- glm(pooroutcome ~ ari, data=treated, family="binomial")
logit_ari_fit <- tidy(logit_ari)
logit_diarrhea <- glm(pooroutcome ~ diarrhea, data=treated, family="binomial")
logit_diarrhea_fit <- tidy(logit_diarrhea)
logit_malaria <- glm(pooroutcome ~ malaria, data=treated, family="binomial")
logit_malaria_fit <- tidy(logit_malaria)

# logit regression w/ number of diagnoses
logit_numdiagnosies <- glm(pooroutcome ~ num_diagnoses, data=treated, family="binomial")
logit_numdiagnosies_fit <- tidy(logit_numdiagnosies)

# logit regression w/ nutrition status
treated$a.nutri_color_shakir <- factor(treated$a.nutri_color_shakir)
logit_nutrition <- glm(pooroutcome ~ a.nutri_color_shakir, data=treated, family="binomial")
logit_nutrition_fit <- tidy(logit_nutrition)

# logit regression w/ protocol errors
treated$c.has_protocol_errors <- factor(treated$c.has_protocol_errors)
logit_protocolerror <- glm(pooroutcome ~ c.has_protocol_errors, data=treated, family="binomial")
logit_protocolerror_fit <- tidy(logit_protocolerror)

logit_num_protocolerrors <- glm(pooroutcome ~ num_protocolerrors,data=treated,family="binomial")
logit_num_protocolerrors_fit <- tidy(logit_num_protocolerrors)

# logit regression w/ month of case reported
treated$month_reported <- factor(treated$month_reported)
logit_month <- glm(pooroutcome ~ month_reported,data=treated,family="binomial")
logit_month_fit <- tidy(logit_month)

# logit regression w/ wet season
treated$wetseason <- factor(treated$wetseason)
logit_season <- glm(pooroutcome ~ wetseason,data=treated,family="binomial")
logit_season_fit <- tidy(logit_season)

# logit regression w/ CHW error history
logit_chw_error <- glm(pooroutcome ~ CHW_error_rate, data=treated ,family="binomial")
logit_chw_error_fit <- tidy(logit_chw_error)

# logit regression w/ CHW caseload
logit_chw_caseload <- glm(pooroutcome ~ CHW_num_cases, data=treated ,family="binomial")
logit_chw_caseload_fit <- tidy(logit_chw_caseload)

# logit regression w/ when illness began
treated$a.when_illness_began<-factor(treated$a.when_illness_began)
logit_illnesstime <-glm(pooroutcome ~ a.when_illness_began, data=treated, family="binomial")
logit_illnesstime_fit <- tidy(logit_illnesstime)

# logit regression w/ time illness onset (hourly)
logit_illnessonset <-glm(pooroutcome ~ illness_onset, data=treated, family="binomial")
logit_illnessonset_fit <- tidy(logit_illnessonset)

# logit regression w/ time to illness onset
treated$time_illness_onset <- factor(treated$time_illness_onset, ordered=TRUE, levels = c("within_24","within_25_to_48", "within_49_to_72", "beyond_72"))
logit_time_illnessonset <-glm(pooroutcome ~ time_illness_onset, data=treated, family="binomial")
logit_time_illnessonset_fit <- tidy(logit_time_illnessonset)

# logit regression w/ time to illness onset beyond 24 hours
logit_illnessonsetbeyond24 <-glm(pooroutcome ~ illness_onset_beyond24, data=treated, family="binomial")
logit_illnessonsetbeyond24_fit <- tidy(logit_illnessonsetbeyond24)

# logit regression w/ number of patient case history
logit_casefreq <-glm(pooroutcome ~ case_freq, data=treated, family="binomial")
logit_casefreq_fit <- tidy(logit_casefreq)

treated$case_freq <- factor(treated$case_freq)
logit_casefreq.f <-glm(pooroutcome ~ case_freq, data=treated, family="binomial")
logit_casefreq.f <- tidy(logit_casefreq.f)

# save all univariate model results in one table
single_results <- data.frame()
single_results <- rbind(logit_sex_fit, logit_age_fit, logit_ari_fit, logit_diarrhea_fit, logit_malaria_fit, logit_numdiagnosies_fit,
                        logit_nutrition_fit, logit_protocolerror_fit, logit_num_protocolerrors_fit, logit_month_fit, logit_season_fit,
                        logit_chw_error_fit, logit_chw_caseload_fit, logit_illnesstime_fit, logit_illnessonset_fit, logit_time_illnessonset_fit, logit_casefreq_fit, logit_casefreq_fit)


####### 2. conduct first round analyses of multivariate models #####

# logit regression w/ patient demographic factors
logit_sexage <- glm(pooroutcome ~ female + age, data=treated, family="binomial")
logit_sexage_fit <- tidy(logit_sexage)

# logit regression w/ diagnoses
logit_alldiagnoses <- glm(pooroutcome ~ ari + diarrhea + malaria , data=treated, family="binomial")
logit_alldiagnoses_fit <- tidy(logit_alldiagnoses)

# logit regression w/ CHW error history and caseload
logit_chw_caseloaderror <- glm(pooroutcome ~ CHW_error_rate + CHW_num_cases, data=treated ,family="binomial")
logit_chw_caseloaderror_fit <- tidy(logit_chw_caseloaderror)
logit_chw_caseloaderror2 <- glm(pooroutcome ~ CHW_num_errors + CHW_num_cases, data=treated ,family="binomial")
logit_chw_caseloaderror2_fit <- tidy(logit_chw_caseloaderror2)

# save all basica multivariate models into table
multi_results <- data.frame()
multi_results <- rbind(logit_sexage_fit, logit_alldiagnoses_fit, logit_chw_caseloaderror_fit, logit_chw_caseloaderror2_fit)


####################################################
####### II.drilldown on variables of interest #########

##### a. protocol errors #######

## identify association between caseload and protocol errors

# understand impact of number of CHW cases on error rate 
lin_case_errorrate <- lm(treated$CHW_error_rate ~ treated$CHW_num_cases, data=treated)
lin_case_errorrate_fit <- tidy(lin_case_errorrate)
# understand impact of number of CHW cases on number of errors
lin_case_error <- lm(treated$CHW_num_errors ~ treated$CHW_num_cases, data=treated)
lin_case_error_fit <- tidy(lin_case_error)
# association between CHw cases and numebr of errors clustered by CHW number
lin_case_error_c <- lm.cluster(treated, treated$CHW_error_rate ~ treated$CHW_num_cases, cluster="t.chw_id")
lin_case_error_c_fit <- tidy(lin_case_error_c)
lin_case_errorrate_c <- lm.cluster(treated, treated$CHW_num_errors ~ treated$CHW_num_cases, cluster="t.chw_id")
lin_case_errorrate_c_fit <- tidy(lin_case_errorrate_c)

# save these results in a tab
CHW_error_results<- rbind(lin_case_errorrate_fit, lin_case_error_fit)

## examine protocol error subtypes and correlation with case outcome

# followup error
logit_followup_error1 <- glm(pooroutcome ~ followup_error, data=treated ,family="binomial")
logit_followup_error1_fit <- tidy(logit_followup_error1)
logit_followup_error1_fit <- cbind(logit_followup_error1_fit, aic=logit_followup_error1$aic)
logit_followup_error2 <- glm(pooroutcome ~ followup_error + c.has_protocol_errors, data=treated ,family="binomial")
logit_followup_error2_fit <- tidy(logit_followup_error2)
logit_followup_error2_fit <- cbind(logit_followup_error2_fit, aic=logit_followup_error2$aic)
logit_followup_error3 <- glm(pooroutcome ~ followup_error + num_protocolerrors, data=treated ,family="binomial")
logit_followup_error3_fit <- tidy(logit_followup_error3)
logit_followup_error3_fit <- cbind(logit_followup_error3_fit, aic=logit_followup_error3$aic)

# missing treatment error
logit_trt_error1 <- glm(pooroutcome ~ missed_treatment, data=treated ,family="binomial")
logit_trt_error1_fit <- tidy(logit_trt_error1)
logit_trt_error1_fit <- cbind(logit_trt_error1_fit, aic=logit_trt_error1$aic)
logit_trt_error2 <- glm(pooroutcome ~ missed_treatment + c.has_protocol_errors, data=treated ,family="binomial")
logit_trt_error2_fit <- tidy(logit_trt_error2)
logit_trt_error2_fit <- cbind(logit_trt_error2_fit, aic=logit_trt_error2$aic)
logit_trt_error3 <- glm(pooroutcome ~ missed_treatment + num_protocolerrors, data=treated ,family="binomial")
logit_trt_error3_fit <- tidy(logit_trt_error3)
logit_trt_error3_fit <- cbind(logit_trt_error3_fit, aic=logit_trt_error3$aic)

# incorrect dosage error
logit_dosage_error1 <- glm(pooroutcome ~ dosage_error, data=treated ,family="binomial")
logit_dosage_error1_fit <- tidy(logit_dosage_error1)
logit_dosage_error1_fit <- cbind(logit_dosage_error1_fit, aic=logit_dosage_error1$aic)
logit_dosage_error2 <- glm(pooroutcome ~ dosage_error + c.has_protocol_errors, data=treated ,family="binomial")
logit_dosage_error2_fit <- tidy(logit_dosage_error2)
logit_dosage_error2_fit <- cbind(logit_dosage_error2_fit, aic=logit_dosage_error2$aic)
logit_dosage_error3 <- glm(pooroutcome ~ dosage_error + num_protocolerrors, data=treated ,family="binomial")
logit_dosage_error3_fit <- tidy(logit_dosage_error3)
logit_dosage_error3_fit <- cbind(logit_dosage_error3_fit, aic=logit_dosage_error3$aic)

## examine protocol error subtypes and correlation with case outcome

# pneumonia-related error
logit_ari_error <- glm(pooroutcome ~ ari_error, data=treated ,family="binomial")
logit_ari_error_fit <- tidy(logit_ari_error)
logit_ari_error_fit <- cbind(logit_ari_error_fit, aic=logit_ari_error$aic)

# diarrhea-related error
logit_diarrhea_error <- glm(pooroutcome ~ diarrhea_error, data=treated ,family="binomial")
logit_diarrhea_error_fit <- tidy(logit_diarrhea_error)
logit_diarrhea_error_fit <- cbind(logit_diarrhea_error_fit, aic=logit_diarrhea_error$aic)

# malaria-related error
logit_malaria_error1 <- glm(pooroutcome ~ malaria_error, data=treated ,family="binomial")
logit_malaria_error1_fit <- tidy(logit_malaria_error1)
logit_malaria_error1_fit <- cbind(logit_malaria_error1_fit, aic=logit_malaria_error1$aic)
logit_malaria_error2 <- glm(pooroutcome ~ malaria_error + num_protocolerrors, data=treated ,family="binomial")
logit_malaria_error2_fit <- tidy(logit_malaria_error2)
logit_malaria_error2_fit <- cbind(logit_malaria_error2_fit, aic=logit_malaria_error2$aic)
logit_malaria_error3 <- glm(pooroutcome ~ malaria_error + CHW_error_rate, data=treated ,family="binomial")
logit_malaria_error3_fit <- tidy(logit_malaria_error3)
logit_malaria_error3_fit <- cbind(logit_malaria_error3_fit, aic=logit_malaria_error3$aic)
logit_malaria_error4 <- glm(pooroutcome ~ malaria_error + malaria, data=treated ,family="binomial")
logit_malaria_error4_fit <- tidy(logit_malaria_error4)
logit_malaria_error4_fit <- cbind(logit_malaria_error4_fit, aic=logit_malaria_error4$aic)

## test whether its the CHW problem or error rate within case problem
logit_error_test <- glm(pooroutcome ~ CHW_error_rate + num_protocolerrors, data=treated, family="binomial")
logit_error_test_fit <- tidy(logit_error_test)
logit_error_test_fit <- cbind(logit_error_test_fit, aic=logit_error_test$aic)

# save all results from models of drilldown of protocol error
error_drilldown <- rbind(logit_followup_error1_fit, logit_followup_error2_fit, logit_followup_error3_fit,
                         logit_trt_error1_fit, logit_trt_error2_fit, logit_trt_error3_fit,
                         logit_dosage_error1_fit, logit_dosage_error2_fit, logit_dosage_error3_fit,
                         logit_ari_error_fit, logit_diarrhea_error_fit, logit_malaria_error1_fit, logit_malaria_error2_fit, logit_malaria_error3_fit, logit_malaria_error4_fit, logit_error_test_fit)


## test all protocol errors one by one

# save all protocol error names that have more than 1 factor levels 
protocol_errors <- c("c.acute_respiratory_infection_without_24h_follow_up", "c.acute_respiratory_infection_without_48h_follow_up", "c.acute_respiratory_infection_without_5_day_follow_up" ,
                     "c.diarrhea_without_5day_follow_up" , "c.diarrhea_without_ors", "c.diarrhea_without_zinc", 
                     "c.fever_without_tdr", "c.incorrect_dosage_of_act_based_combination", "c.incorrect_dosage_of_albendazole", "c.incorrect_dosage_of_amoxicillin" , "c.incorrect_dosage_of_paracetamol", 
                     "c.incorrect_dosage_of_vitamin_a", "c.incorrect_dosage_of_zinc", "c.malaria_without_24h_follow_up", "c.malaria_without_48h_follow_up", "c.malaria_without_72h_follow_up",
                     "c.muac_yellow_no_malnutrition_follow_up", "c.pneumonia_without_amoxicillin", "c.symptoms_of_malaria_without_tdr")

# create table with regression results from each model
error_results <- data.frame()
for (i in 1:length(protocol_errors)) {
  logit_error <- glm(pooroutcome ~ get(protocol_errors[i]), data = treated, family="binomial")
  logit_error_fit <- tidy(logit_error)
  logit_error_fit[2,1] <- protocol_errors[i]
  logit_error_fit <- cbind(logit_error_fit, aic=logit_error$aic)
  error_results <- rbind(error_results, logit_error_fit)
}


##### b. seasonality and time of case assessment #######

# malaria and wet season
logit_season_malaria1 <- glm(pooroutcome ~ malaria + wetseason, data= treated, family="binomial")
logit_season_malaria1_fit <- tidy(logit_season_malaria1)
logit_season_malaria1_fit <- cbind(logit_season_malaria1_fit, aic=logit_season_malaria1$aic)
logit_season_malaria2 <- glm(pooroutcome ~ malaria*wetseason + wetseason, data= treated, family="binomial")
logit_season_malaria2_fit <- tidy(logit_season_malaria2)
logit_season_malaria2_fit <- cbind(logit_season_malaria2_fit, aic=logit_season_malaria2$aic)

# CHW case load and wet season
logit_season_caseload1 <- glm(pooroutcome ~ CHW_num_cases + wetseason, data= treated, family="binomial")
logit_season_caseload1_fit <- tidy(logit_season_caseload1)
logit_season_caseload1_fit <- cbind(logit_season_caseload1_fit, aic=logit_season_caseload1$aic)
logit_season_caseload2 <- glm(pooroutcome ~ CHW_num_cases + wetseason + CHW_num_cases*wetseason, data= treated, family="binomial")
logit_season_caseload2_fit <- tidy(logit_season_caseload2)
logit_season_caseload2_fit <- cbind(logit_season_caseload2_fit, aic=logit_season_caseload2$aic)

# monthly follow-up case load and wet season
logit_monthly_caseload1 <- glm(pooroutcome ~ month_fu_casecount, data= treated, family= "binomial")
logit_monthly_caseload1_fit <- tidy(logit_monthly_caseload1)
logit_monthly_caseload1_fit <- cbind(logit_monthly_caseload1_fit, aic=logit_monthly_caseload1$aic)
logit_monthly_caseload2 <- glm(pooroutcome ~ month_fu_casecount + wetseason, data= treated, family= "binomial")
logit_monthly_caseload2_fit <- tidy(logit_monthly_caseload2)
logit_monthly_caseload2_fit <- cbind(logit_monthly_caseload2_fit, aic=logit_monthly_caseload2$aic)
logit_monthly_caseload3 <- glm(pooroutcome ~ month_fu_casecount + wetseason + CHW_num_cases, data= treated, family= "binomial")
logit_monthly_caseload3_fit <- tidy(logit_monthly_caseload3)
logit_monthly_caseload3_fit <- cbind(logit_monthly_caseload3_fit, aic=logit_monthly_caseload3$aic)
logit_monthly_caseload4 <- glm(pooroutcome ~ month_fu_casecount + wetseason + malaria, data= treated, family= "binomial")
logit_monthly_caseload4_fit <- tidy(logit_monthly_caseload4)
logit_monthly_caseload4_fit <- cbind(logit_monthly_caseload4_fit, aic=logit_monthly_caseload4$aic)
logit_monthly_caseload5 <- glm(pooroutcome ~ month_fu_casecount + wetseason + month_fu_casecount*wetseason, data= treated, family= "binomial")
logit_monthly_caseload5_fit <- tidy(logit_monthly_caseload5)
logit_monthly_caseload5_fit <- cbind(logit_monthly_caseload5_fit, aic=logit_monthly_caseload5$aic)

# monthly follow-up case load and month reported
treated$month_reported <- as.factor(treated$month_reported)
logit_month_fu_casecount <- glm(pooroutcome ~ month_fu_casecount + month_reported, data= treated, family= "binomial")
logit_month_fu_casecount_fit <- tidy(logit_month_fu_casecount)
logit_month_fu_casecount_fit <- cbind(logit_month_fu_casecount_fit, aic=logit_month_fu_casecount$aic)
logit_month_caseload <- glm(pooroutcome ~ month_fu_casecount + month_reported + malaria, data= treated, family= "binomial")
logit_month_caseload_fit <- tidy(logit_month_caseload)
logit_month_caseload_fit <- cbind(logit_month_caseload_fit, aic=logit_month_caseload$aic)

# monthly assessment case load
logit_month_assesscaseload1 <- glm(pooroutcome ~ month_assess_casecount, data= treated, family= "binomial")
logit_month_assesscaseload1_fit <- tidy(logit_month_assesscaseload1)
logit_month_assesscaseload1_fit <- cbind(logit_month_assesscaseload1_fit, aic=logit_month_assesscaseload1$aic)
logit_month_assesscaseload2 <- glm(pooroutcome ~ month_assess_casecount + wetseason, data= treated, family= "binomial")
logit_month_assesscaseload2_fit <- tidy(logit_month_assesscaseload2)
logit_month_assesscaseload2_fit <- cbind(logit_month_assesscaseload2_fit, aic=logit_month_assesscaseload2$aic)
logit_month_assesscaseload3<- glm(pooroutcome ~ month_assess_casecount + wetseason + CHW_num_cases, data= treated, family= "binomial")
logit_month_assesscaseload3_fit <- tidy(logit_month_assesscaseload3)
logit_month_assesscaseload3_fit <- cbind(logit_month_assesscaseload3_fit, aic=logit_month_assesscaseload3$aic)
logit_month_assesscaseload4 <- glm(pooroutcome ~ month_assess_casecount + wetseason + malaria, data= treated, family= "binomial")
logit_month_assesscaseload4_fit <- tidy(logit_month_assesscaseload4)
logit_month_assesscaseload4_fit <- cbind(logit_month_assesscaseload4_fit, aic=logit_month_assesscaseload4$aic)
logit_month_assesscaseload5 <- glm(pooroutcome ~ month_assess_casecount + wetseason + month_assess_casecount*wetseason, data= treated, family= "binomial")
logit_month_assesscaseload5_fit <- tidy(logit_month_assesscaseload5)
logit_month_assesscaseload5_fit <- cbind(logit_month_assesscaseload5_fit, aic=logit_month_assesscaseload5$aic)

# monthly assessment cases and month reported (model did not converge)
treated$month_reported <- as.factor(treated$month_reported)
logit_month_assess_casecount <- glm(pooroutcome ~ month_assess_casecount + month_reported, data= treated, family= "binomial")
logit_month_assess_casecount_fit <- tidy(logit_month_assess_casecount)
logit_month_assess_casecount_fit <- cbind(logit_month_assess_casecount_fit, aic=logit_month_assess_casecount$aic)

# rate of follow-up and seasonality
logit_fu_rate1 <- glm(pooroutcome ~ month_fu_rate, data= treated, family= "binomial")
logit_fu_rate1_fit <- tidy(logit_fu_rate1)
logit_fu_rate1_fit <- cbind(logit_fu_rate1_fit, aic=logit_fu_rate1$aic)
logit_fu_rate2 <- glm(pooroutcome ~ month_fu_rate + wetseason, data= treated, family= "binomial")
logit_fu_rate2_fit <- tidy(logit_fu_rate2)
logit_fu_rate2_fit <- cbind(logit_fu_rate2_fit, aic=logit_fu_rate2$aic)
logit_fu_rate3 <- glm(pooroutcome ~ month_fu_rate + wetseason + CHW_num_cases, data= treated, family= "binomial")
logit_fu_rate3_fit <- tidy(logit_fu_rate3)
logit_fu_rate3_fit <- cbind(logit_fu_rate3_fit, aic=logit_fu_rate3$aic)

# CHw monthly caseload and seasonality
logit_CHW_monthlycase1 <- glm(pooroutcome ~ CHW_monthly_cases, data= treated, family= "binomial")
logit_CHW_monthlycase1_fit <- tidy(logit_CHW_monthlycase1)
logit_CHW_monthlycase1_fit <- cbind(logit_CHW_monthlycase1_fit, aic=logit_CHW_monthlycase1$aic)
logit_CHW_monthlycase2 <- glm(pooroutcome ~ CHW_monthly_cases + wetseason, data= treated, family= "binomial")
logit_CHW_monthlycase2_fit <- tidy(logit_CHW_monthlycase2)
logit_CHW_monthlycase2_fit <- cbind(logit_CHW_monthlycase2_fit, aic=logit_CHW_monthlycase2$aic)
logit_CHW_monthlycase3 <- glm(pooroutcome ~ CHW_monthly_cases + wetseason +CHW_num_cases, data= treated, family= "binomial")
logit_CHW_monthlycase3_fit <- tidy(logit_CHW_monthlycase3)
logit_CHW_monthlycase3_fit <- cbind(logit_CHW_monthlycase3_fit, aic=logit_CHW_monthlycase3$aic)
logit_CHW_monthlycase4 <- glm(pooroutcome ~ CHW_monthly_cases + wetseason + month_fu_casecount, data= treated, family= "binomial")
logit_CHW_monthlycase4_fit <- tidy(logit_CHW_monthlycase4)
logit_CHW_monthlycase4_fit <- cbind(logit_CHW_monthlycase4_fit, aic=logit_CHW_monthlycase4$aic)
logit_CHW_monthlycase5 <- glm(pooroutcome ~ CHW_monthly_cases + wetseason + CHW_monthly_cases*wetseason, data= treated, family= "binomial")
logit_CHW_monthlycase5_fit <- tidy(logit_CHW_monthlycase5)
logit_CHW_monthlycase5_fit <- cbind(logit_CHW_monthlycase5_fit, aic=logit_CHW_monthlycase5$aic)

# save all drilldown reasons on seasonality to table
drilldown_season <- rbind(logit_season_malaria1_fit, logit_season_malaria2_fit, 
                           logit_season_caseload1_fit, logit_season_caseload2_fit,
                           logit_monthly_caseload1_fit, logit_monthly_caseload2_fit, logit_monthly_caseload3_fit, logit_monthly_caseload4_fit, logit_monthly_caseload5_fit,
                           logit_month_fu_casecount_fit, logit_month_caseload_fit,
                           logit_month_assesscaseload1_fit, logit_month_assesscaseload2_fit, logit_month_assesscaseload3_fit, logit_month_assesscaseload4_fit, logit_month_assesscaseload5_fit,
                           logit_fu_rate1_fit, logit_fu_rate2_fit, logit_fu_rate3_fit,
                           logit_CHW_monthlycase1_fit, logit_CHW_monthlycase2_fit, logit_CHW_monthlycase3_fit, logit_CHW_monthlycase4_fit, logit_CHW_monthlycase5_fit)


###### c. correlation between caseload and time from treatment onset ######

# CHW caseload and time from treatment onset
logit_CHWcaseload_timeonset <- glm(treated$time_illness_onset ~ treated$CHW_num_cases, data=treated, family="binomial")
logit_CHWcaseload_timeonset_fit <- tidy(logit_CHWcaseload_timeonset)

lin_CHWcaseload_timeonset <- lm(treated$illness_onset ~ treated$CHW_num_cases, data=treated)
lin_CHWcaseload_timeonset_fit <- tidy(lin_CHWcaseload_timeonset)

caseload_illnessonset_results <- rbind(logit_CHWcaseload_timeonset_fit, lin_CHWcaseload_timeonset_fit )

###### d. reason for treatment not given ######

## first look at whether its out of stock

# make variable to indicate if out of stock
treated$ors_stockout <- ifelse(treated$a.diarrhea_why_not_give_ors=="out_of_stock", 1, 0)
treated$zinc_stockout <- ifelse(treated$a.diarrhea_why_not_give_zinc=="out_of_stock", 1, 0)
treated$amox_stockout <- ifelse(treated$a.ari_not_give_amox=="out_of_stock", 1, 0)

# regress and save results
logit_stockout_ors <- glm(pooroutcome ~ diarrhea + ors_stockout, data=treated, family="binomial")
logit_stockout_ors_fit <- tidy(logit_stockout_ors)
logit_stockout_zinc <- glm(pooroutcome ~ diarrhea + zinc_stockout, data=treated, family="binomial")
logit_stockout_zinc_fit <- tidy(logit_stockout_zinc)
logit_stockout_amox <- glm(pooroutcome ~ ari + amox_stockout, data=treated, family="binomial")
logit_stockout_amox_fit <- tidy(logit_stockout_amox)

## look at whether there's an "other reason"

# make variable to indicate if other reason for no treatment
treated$diarrhea_ors_other <- ifelse(treated$a.diarrhea_why_not_give_ors=="other", 1, 0)
treated$diarrhea_zinc_other <- ifelse(treated$a.diarrhea_why_not_give_zinc=="other", 1, 0)
treated$ari_amox_other <- ifelse(treated$a.ari_not_give_amox=="other", 1, 0)

# regress and save results
logit_ors_other <- glm(pooroutcome ~ diarrhea + diarrhea_ors_other, data=treated, family="binomial")
logit_ors_other_fit <- tidy(logit_ors_other)
logit_zinc_other <- glm(pooroutcome ~ diarrhea + diarrhea_zinc_other, data=treated, family="binomial")
logit_zinc_other_fit <- tidy(logit_zinc_other)
logit_amox_other <- glm(pooroutcome ~ ari + ari_amox_other, data=treated, family="binomial")
logit_amox_other_fit <- tidy(logit_amox_other)

notreatmentreasons_results <- rbind(logit_stockout_ors_fit, logit_stockout_zinc_fit, logit_stockout_amox_fit, 
                                    logit_ors_other_fit, logit_zinc_other_fit, logit_amox_other_fit)

###########################################################
########## III. combining all #####################

# all about CHW
logit_all_CHW1 <- glm(pooroutcome ~ CHW_num_cases + num_protocolerrors + missed_treatment, data= treated, family="binomial")
logit_all_CHW1_fit <- tidy(logit_all_CHW1)
logit_all_CHW1_fit <- cbind(logit_all_CHW1_fit, aic=logit_all_CHW1$aic)
logit_all_CHW2 <- glm(pooroutcome ~ CHW_num_cases + num_protocolerrors + missed_treatment + CHW_error_rate, data= treated, family="binomial")
logit_all_CHW2_fit <- tidy(logit_all_CHW2)
logit_all_CHW2_fit <- cbind(logit_all_CHW2_fit, aic=logit_all_CHW2$aic)

# everything
logit_all1 <- glm(pooroutcome ~ CHW_num_cases + missed_treatment + CHW_error_rate + wetseason + malaria, data= treated, family="binomial")
logit_all1_fit <- tidy(logit_all1)
logit_all1_fit <- cbind(logit_all1_fit, aic=logit_all1$aic)
logit_all2 <- glm(pooroutcome ~ CHW_num_cases + missed_treatment + CHW_error_rate + month_fu_casecount + wetseason + malaria + CHW_num_cases*wetseason, data= treated, family="binomial")
logit_all2_fit <- tidy(logit_all2)
logit_all2_fit <- cbind(logit_all2_fit, aic=logit_all2$aic)
logit_all3 <- glm(pooroutcome ~ CHW_monthly_cases + missed_treatment + CHW_error_rate + month_fu_casecount + wetseason + malaria + CHW_monthly_cases*wetseason, data= treated, family="binomial")
logit_all3_fit <- tidy(logit_all3)
logit_all3_fit <- cbind(logit_all3_fit, aic=logit_all3$aic)
logit_all4 <- glm(pooroutcome ~ CHW_num_cases + missed_treatment + CHW_error_rate + month_fu_casecount + wetseason + num_diagnoses + CHW_num_cases*wetseason, data= treated, family="binomial")
logit_all4_fit <- tidy(logit_all4)
logit_all4_fit <- cbind(logit_all4_fit, aic=logit_all4$aic)
logit_all5 <- glm(pooroutcome ~ CHW_monthly_cases + missed_treatment + CHW_error_rate + month_fu_casecount + wetseason + num_diagnoses + malaria + CHW_monthly_cases*wetseason, data= treated, family="binomial")
logit_all5_fit <- tidy(logit_all5)
logit_all5_fit <- cbind(logit_all5_fit, aic=logit_all5$aic)
logit_all6 <- glm(pooroutcome ~ CHW_monthly_cases + missed_treatment + CHW_error_rate + month_fu_casecount + wetseason + num_diagnoses + malaria + illness_onset_beyond24 + CHW_monthly_cases*wetseason, data= treated, family="binomial")
logit_all6_fit <- tidy(logit_all6)
logit_all6_fit <- cbind(logit_all6_fit, aic=logit_all6$aic)
logit_all7 <- glm(pooroutcome ~ CHW_monthly_cases + missed_treatment + CHW_error_rate + month_fu_casecount + wetseason + num_diagnoses + illness_onset_beyond24 + CHW_monthly_cases*wetseason, data= treated, family="binomial")
logit_all7_fit <- tidy(logit_all7)
logit_all7_fit <- cbind(logit_all7_fit, aic=logit_all7$aic)

# save all full model results
all_results <- rbind(logit_all_CHW1_fit, logit_all_CHW2_fit, logit_all1_fit, logit_all2_fit, logit_all3_fit, logit_all4_fit, logit_all5_fit, logit_all6_fit, logit_all7_fit)


######################################
#### output results to csv files #####

# set output directory
output <- "C:/Users/ngcou/OneDrive/Medic Mobile project/Output"
setwd(output)


write.csv(single_results, "single_results.csv")
write.csv(multi_results, "multi_results.csv")
write.csv(CHW_error_results, "CHW_error_results.csv")
write.csv(error_drilldown, "error_drilldown.csv")
write.csv(error_results, "error_results.csv")
write.csv(drilldown_season, "drilldown_season.csv")
write.csv(caseload_illnessonset_results, "caseload_illnessonset_results.csv")
write.csv(notreatmentreasons_results, "notreatmentreasons_results.csv")
write.csv(all_results, "all_results.csv")
          
          
