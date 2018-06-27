WITH treatment_fu_CTE AS
(
    SELECT
        form_source_id,
        bool_or(follow_up_count = '1') AS fu_1,
        bool_or(follow_up_count = '2') AS fu_2,
        bool_or(follow_up_count = '3') AS fu_3,
        bool_or(follow_up_count = '4') AS fu_4,
        bool_or(follow_up_count = '5') AS fu_5  
        
    FROM
        useview_treatment_follow_up
        
    GROUP BY
        form_source_id
        
), malnutrition_fu_CTE AS
(
    SELECT
        form_source_id
        
    FROM
        useview_malnutrition_follow_up
        
    GROUP BY
        form_source_id
)

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Capture fields that would indicate a protocol error
-- "Danger Signs Without Referral: Assessment has danger sign AND (there is no referal to cscom OR not accompanied to cscom)
-- "Symtoms of Malaria without TDR": Assessment has malaraia danger sign AND malaria TDR was not done
-- "Fever without TDR": Assessment shows child temperature with value >= 38 AND malaria TDR was not done
-- "ACT Based Combination without Positive TDR": Assessment for malaria shows ACT medication given AND malaria TDR result is not positive
*/

SELECT
	
	/*
		!!ATTENTION!! If you add any fields to this, you need to also add
		them to has_protocol_errors, number_of_protocol_errors AND the query below those.  
	*/
	assessment.uuid,
	assessment.chw,
	assessment.chw_area,
	assessment.formname,
	assessment.reported,
		        
	assessment.has_danger_sign = 'true' 
		AND (assessment.refer_to_cscom = 'false' OR assessment.accompany_to_cscom = 'false')
			AS danger_signs_without_referral,
	
	assessment.has_mal_danger_sign = 'true'
		AND assessment.s_malaria_tdr_done = 'no'
			AS symptoms_of_malaria_without_tdr,

	assessment.s_child_temperature_pre_chw::decimal >= 38::decimal
		AND assessment.s_malaria_tdr_done = 'no'
        	AS fever_without_tdr,

    assessment.s_malaria_give_act = 'yes'
    	AND assessment.s_malaria_tdr_result != 'pos'
            AS act_based_combination_without_positive_tdr,
	
/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (incorrect_dosage_of_act_based_combination)
-- "Incorrect dosage of ACT medication" if one of the following is TRUE
   * Patient age is between 0 and 23 months AND 'ACT' dosage is NOT empty and is NOT 2 tablets
   * Patient age is between 24 and 59 months AND 'ACT' dosage is NOT empty and is NOT 4 tablets
   * Patient age is between 60 and 71 months AND 'ACT' dosage is NOT empty and is NOT 4 tablets
   * Patient age is between 72 and 131 months AND 'ACT' dosage is NOT empty and is NOT 6 tablets
   * Patient age is >= 132 months AND 'ACT' dosage is NOT empty and is NOT 8 tablets
*/

	( 
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 0 AND 23,false)
                	AND assessment.s_malaria_act_dosage != '2_tablets_act'
                		AND assessment.s_malaria_act_dosage != ''
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 24 AND 59,false)
                	AND assessment.s_malaria_act_dosage != '4_tablets_act'
                    	AND assessment.s_malaria_act_dosage != ''
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 60 AND 71,false)
             		AND assessment.s_malaria_act_dosage != '4_tablets_act' 
                		AND assessment.s_malaria_act_dosage != ''
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 72 AND 131,false)
                	AND assessment.s_malaria_act_dosage != '6_tablets_act'
                    	AND assessment.s_malaria_act_dosage != ''
            )
            OR
            (
                COALESCE(assessment.patient_age_in_months >= 132,false) 
                	AND assessment.s_malaria_act_dosage != '8_tablets_act'
                    	AND assessment.s_malaria_act_dosage != ''
			)
	) AS incorrect_dosage_of_act_based_combination,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (severe_diarrhea_without_referral)
-- "Severe Diarrhea without Referral" if one of the following is TRUE
   * Assessment shows danger sign of Diarrhea more than 14 days AND (not referred to CSCOM OR not accompanied to CSCOM)
   * Assessment shows Danger sign of bloddin stools AND not accompanied to CSCOM
*/

	(
		    (
		        assessment.s_ref_danger_sign_diarrhea_more_than_14_days = 'yes' 
		        	AND (assessment.refer_to_cscom = 'false' OR assessment.accompany_to_cscom = 'false')
		    )
		    OR
		    (
		        assessment.s_ref_danger_sign_blood_in_stools = 'yes'
		        	AND assessment.accompany_to_cscom = 'false'
		    )
	) AS severe_diarrhea_without_referral,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (diarrhea_without_zinc)
-- "Diarrhea without Zinc" if the following is TRUE
   * Assessment shows diarrhea stools in a day AND Zinc medication was not given
*/
	assessment.s_diarrhea_stools_a_day = 'yes'
		AND assessment.s_diarrhea_give_zinc = 'no'
        	AS diarrhea_without_zinc,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (diarrhea_without_ors)
-- "Diarrhea without ORS" if the following is TRUE
   * Assessment shows diarrhea stools in a day AND ORS medication was not given
*/
  	assessment.s_diarrhea_stools_a_day = 'yes'
  		AND assessment.s_diarrhea_give_ors = 'no'
        	AS diarrhea_without_ors,

 /*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (incorrect_dosage_of_zinc)
-- "Incorrect Dosage of Zinc" if the following is TRUE
   * Assessment shows patient age as between 2 and 5 AND Zinc dosage is not empty and not 1
   * Assessment shows patient age as between 6 and 59 AND Zinc dosage is not empty and not 2
   * Assessment shows patient age as between 6 and 59 AND Zinc dosage is not empty and not 3
*/
   (            
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 2 AND 5,false)
                	AND assessment.s_diarrhea_zinc_dosage != 'c_diarrhea_zinc_dosage_1'
                    	AND assessment.s_diarrhea_zinc_dosage != ''
            ) 
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 6 AND 59,false)
                	AND assessment.s_diarrhea_zinc_dosage != 'c_diarrhea_zinc_dosage_2'
                    	AND assessment.s_diarrhea_zinc_dosage != ''
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 6 AND 59,false)
                	AND assessment.s_diarrhea_zinc_dosage != 'c_diarrhea_zinc_dosage_3'
                    	AND assessment.s_diarrhea_zinc_dosage != ''
            )
    ) AS incorrect_dosage_of_zinc,


 /*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (pneumonia_without_amoxicillin)
-- "Pneumonia without Amoxicillin" if the following is TRUE
   * Assessment shows fast breathing signs AND Amoxicillin was not given
*/
   assessment.fast_breathing = 'true'
    	AND assessment.s_ari_give_amox = 'no'
             AS pneumonia_without_amoxicillin,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (amoxicillin_without_pneumonia)
-- "Amoxicillin without Pneumonia" if the following is TRUE
   * Assessment shows no fast breathing signs AND Amoxicillin was given
*/
    assessment.fast_breathing = 'false'
    	AND assessment.s_ari_give_amox = 'yes'
           AS amoxicillin_without_pneumonia,
           
/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (incorrect_dosage_of_amoxicillin)
-- "Incorrect dosage of Amoxicillin" if the following is TRUE
   * Assessment shows patient age as between 2 and 11 months AND amoxicilin dosage given is not blank and it is not 5 ml.
   * Assessment shows patient age as between 12 and 59 months AND amoxicilin dosage given is not blank and it is not 10 ml.
*/
	-- check if s_ari_give_amox_dosage is not blank and check why we need to give amoxicilin
	(
	        (
	        	COALESCE(assessment.patient_age_in_months BETWEEN 2 AND 11,false)
	            	AND assessment.s_ari_give_amox_dosage != '5_ml_amox' 
	                	AND assessment.s_ari_give_amox_dosage != ''
	        ) 
	        OR
	        (
	        	COALESCE(assessment.patient_age_in_months BETWEEN 12 AND 59,false)
	            	AND assessment.s_ari_give_amox_dosage != '10_ml_amox' 
	                	AND assessment.s_ari_give_amox_dosage != ''
	        )
    ) AS incorrect_dosage_of_amoxicillin,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (muac_yellow_no_malnutrition_follow_up)
-- "No Malnutrition Followup" if the following is TRUE
   * Assessment shows color of shakir strip as 'yellow' AND malnutrition form source is NULL
*/
    assessment.s_nutri_color_shakir = 'Yellow' 
    	AND malnutrition_fu.form_source_id IS NULL
           AS muac_yellow_no_malnutrition_follow_up,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (incorrect_dosage_of_vitamin_a)
-- "Incorrect dosage of Vitamin A" if the following is TRUE
   * Assessment shows patient age as between 6 and 11 months AND vitamin A dosage given is not blank and it is not blue capsule vita.
   * Assessment shows patient age as between 12 and 59 months AND vitamin A dosage given is not blank and it is not red capsule vita.
*/
	(
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 6 AND 11,false)
            		AND assessment.s_nutri_give_vitaminA_dosage != 'blue_capsule_vita'
                    	AND assessment.s_nutri_give_vitaminA_dosage != '' 
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 12 AND 59,false)
            	AND assessment.s_nutri_give_vitaminA_dosage != 'red_capsule_vita'
                    AND assessment.s_nutri_give_vitaminA_dosage != '' 
            )
		) AS incorrect_dosage_of_vitamin_a,
    
/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (incorrect_dosage_of_albendazole)
-- "Incorrect dosage of Albendazole" if the following is TRUE
   * Assessment shows patient age as between 6 and 11 months AND Albendazole dosage given is not blank and it is not 200 mg.
   * Assessment shows patient age as between 12 and 59 months AND Albendazole dosage given is not blank and it is not 400 mg.
*/
        ( 
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 6 AND 11,false)
            	AND assessment.s_nutri_give_albendazole_dosage != '200_mg_albendazole'
                    AND assessment.s_nutri_give_albendazole_dosage != '' 
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 12 AND 59,false)
            	AND assessment.s_nutri_give_albendazole_dosage != '400_mg_albendazole'
                    AND assessment.s_nutri_give_albendazole_dosage != '' 
            )
	) AS incorrect_dosage_of_albendazole,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (incorrect_dosage_of_paracetamol)
-- "Incorrect dosage of Paracetamol" if the following is TRUE
   * Assessment shows patient age as between 6 and 11 months AND Paracetamol dosage given is not blank and it is not quarter tablet.
   * Assessment shows patient age as between 12 and 59 months AND Paracetamol dosage given is not blank and it is not half tablet.
   * Assessment shows patient age as between 59 and 71 months AND Paracetamol dosage given is not blank and it is not three quarter tablet.
   * Assessment shows patient age as between 72 and 131 months AND Paracetamol dosage given is not blank and it is not one tablet.
   * Assessment shows patient age as >= 132 months AND Paracetamol dosage given is not blank and it is not two tablets.
*/
    (
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 2 AND 11,false) 
                AND assessment.s_give_paracetamol_dosage != 'quarter_tab_paracetamol' 
                    AND assessment.s_give_paracetamol_dosage != '' 
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 12 AND 59,false)
            	AND assessment.s_give_paracetamol_dosage != 'half_tab_paracetamol' 
                    AND assessment.s_give_paracetamol_dosage != '' 
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 59 AND 71,false)
            	AND assessment.s_give_paracetamol_dosage != 'three_quarter_tab_paracetamol' 
                    AND assessment.s_give_paracetamol_dosage != '' 
            )
            OR
            (
            	COALESCE(assessment.patient_age_in_months BETWEEN 72 AND 131,false) 
                AND assessment.s_give_paracetamol_dosage != 'one_tab_paracetamol' 
                    AND assessment.s_give_paracetamol_dosage != '' 
            )
            OR
            (
                COALESCE(assessment.patient_age_in_months >= 132,false) 
                AND assessment.s_give_paracetamol_dosage != 'two_tab_paracetamol' 
                    AND assessment.s_give_paracetamol_dosage != '' 
            )
	) AS incorrect_dosage_of_paracetamol,


/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (malaria_without_24h_follow_up)
-- "Malaria without 24h Followup" if the following is TRUE
   * Assessment shows Malaria danger sign and follow-up count is not 1
*/
    assessment.has_mal_danger_sign = 'true' 
    	AND NOT COALESCE(treatment_fu.fu_1,false) 
            AS malaria_without_24h_follow_up,
 
/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (malaria_without_48h_follow_up)
-- "Malaria without 48h Followup" if the following is TRUE
   * Assessment shows Malaria danger sign and follow-up count is not 2
*/
    assessment.has_mal_danger_sign = 'true' 
    	AND NOT COALESCE(treatment_fu.fu_2,false)
            AS malaria_without_48h_follow_up,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (malaria_without_72h_follow_up)
-- "Malaria without 72h Followup" if the following is TRUE
   * Assessment shows Malaria danger sign and follow-up count is not 3
*/
    assessment.has_mal_danger_sign = 'true' 
    	AND NOT COALESCE(treatment_fu.fu_3,false)
	        AS malaria_without_72h_follow_up,

--  assessment.has_danger_sign = 'true' 
--		AND NULLIF(assessment_referrals.follow_up_count,'')::int = 1 
--      	AS danger_sign_without_24h_follow_up,
--
--  assessment.has_danger_sign = 'true' 
--		AND NULLIF(assessment_referrals.follow_up_count,'')::int = 2 
--      	AS danger_sign_without_48h_follow_up,
--
--  assessment.has_danger_sign = 'true'
--		AND NULLIF(assessment_referrals.follow_up_count,'')::int = 3 
--      	AS danger_sign_without_72h_follow_up,


/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (acute_respiratory_infection_without_24h_follow_up)
-- "Acute Respiratory Infection without 24h Followup" if the following is TRUE
   * Assessment shows fast breathing  sign and follow-up count is not 1
*/
	assessment.fast_breathing = 'true'
		AND NOT COALESCE(treatment_fu.fu_1,false)
            AS acute_respiratory_infection_without_24h_follow_up,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (acute_respiratory_infection_without_48h_follow_up)
-- "Acute Respiratory Infection without 48h Followup" if the following is TRUE
   * Assessment shows fast breathing  sign and follow-up count is not 2
*/
	assessment.fast_breathing = 'true'
		AND NOT COALESCE(treatment_fu.fu_2,false) 
        	AS acute_respiratory_infection_without_48h_follow_up,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (acute_respiratory_infection_without_5_day_follow_up)
-- "Acute Respiratory Infection without 5 day Followup" if the following is TRUE
   * Assessment shows fast breathing  sign and follow-up count is not 4
*/
	assessment.fast_breathing = 'true'
		AND NOT COALESCE(treatment_fu.fu_4,false) 
        	AS acute_respiratory_infection_without_5_day_follow_up,

/*
-- Added by Sudha on 1/24/2018 (Annotation)
-- Annotation for the field below (diarrhea_without_5day_follow_up)
-- "Diarrhea without 5 day Followup" if the following is TRUE
   * Assessment shows diarrhea stools sign and follow-up count is not 1
*/
	assessment.s_diarrhea_stools_a_day = 'yes'
		AND NOT COALESCE(treatment_fu.fu_1,false)
        AS diarrhea_without_5day_follow_up

FROM
    useview_assessment AS assessment
    LEFT JOIN treatment_fu_CTE AS treatment_fu ON (assessment.uuid = treatment_fu.form_source_id)
    LEFT JOIN malnutrition_fu_CTE AS malnutrition_fu ON (assessment.uuid = malnutrition_fu.form_source_id)
