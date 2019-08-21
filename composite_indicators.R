library(composr)
library(naniar)

recoding_rules <- read.csv("./input/full_comp_updated.csv", stringsAsFactors = F)
# 
# hh_merged %>% 
#   mutate_all(list(str_replace(., "na", NA)))
# 
# hh_merged$agegrp_3_18_sum %<>% as.numeric
# 
# hh_merged$agegrp_3_18_sum %>% table
# hh_merged$children_out_formal %<>% as.numeric
# 
# recoding_rules_1 <- recoding_rules[c(1:10),]
# recoding_rules_2 <- recoding_rules[c(11:20),]

final$real_sex_hoh <- ifelse(final$hoh == "yes", final$gender_resp , 
                                 ifelse(final$hoh == "no",final$gender_hoh,""))

final <- recode_batch(df = final,
                             tos = recoding_rules$weight,
                             wheres = recoding_rules$condition,
                             targets = recoding_rules$component_name,
                             questionnaire = questionnaire)

#no let's have a look at all those evil little indicators

pins <- final %>% select(education_index, foodsec_index,health_index, 
                         protection_index, shelter_index, wash_index, 
                         coping_gap_index,impact_index, earlyrecovery_index)


#Oh HO. Here it seems that education and wash have a lot of na -- let's review

edu <- final %>% select(current_attend_comp, never_attend_comp, children_out_formal, 
                        agegrp_3_18_sum, barrier_educ_comp, educ_nfi_comp,
                        barrier_girl_comp, education_index)

edu<- edu[c(1:200),]
edu %>% write.csv("edu.csv")

edu$current_attend_comp %>% is.na %>% sum
#AHA again, so current_attend_comp is the issue

#WASH
wash <- final %>% select(wash_index, water_consumption_comp, water_consumption_total, water_collect_comp, 
                         latrine_access_comp, improved_water_comp, wash_hands_comp)
wash %>% str

final$water_consumption_total %>% is.na %>% sum
final$water_consumption_comp %>% is.na %>% sum
final$wash_index %>% is.na %>% sum

# IMPACT 
impact <- final %>% select(access_uxo_comp, access_route_comp,
                           people_displacement_comp,
                           people_restriction_comp,
                           services_facilities_comp,
                           services_comms_comp,
                           access_assistance_comp,
                           impact_index, lga, cluster)
impact %>% str

Jada <- impact %>% filter(lga == "NG002008")


Jada %>% write.csv("./output/Jada.csv")

names(analysisplan_comps_lga) <- names(analysisplan_lga)
results_comp_Jada <- map_to_result(data = Jada, dependent.var = "impact_index", case = "CASE_direct_reporting_categorical_",cluster.variable.name = "cluster", weighting = weighting)


results_comp_Jada
devtools::install_github("mabafaba/msni19")

library("msni19")

?msni19::msni #what is this magic function

msni19::msni

final$msni_nga <- msni(final$education_index, final$foodsec_index, final$health_index, 
                 final$protection_index, final$shelter_index, final$wash_index, 
                 final$coping_gap_index,final$impact_index, final$earlyrecovery_index)


overall_with_msni <- final %>% select(education_index, foodsec_index,health_index, 
                 protection_index, shelter_index, wash_index, 
                 coping_gap_index,impact_index, earlyrecovery_index, msni_nga)

overall_with_msni %>% str
