# 1.0 Setup Loading packages
rm(list = ls())


setwd("C:/Users/Eliora Henzler/Documents/GitHub/NGA_msna_2019/")

final <- read.csv("./input/final_recoding.csv", stringsAsFactors = F)

devtools::install_github("mabafaba/hypegrammaR")

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(parallel) # mclapply
library(kobostandards) # check inputs for inconsistencies
#devtools::install_github('mabafaba/kobostandards')
library(xlsformfill) # generate fake data for kobo
#devtools::install_github('mabafaba/xlsformfill')
library(hypegrammaR) # stats 4 complex samples
#devtools::install_github('ellieallien/hypegrammaR',force=TRUE)
library(composr) # horziontal operations
library(naniar) #recoding to NA

#source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans
source("functions/remove_responses_from_sumstat.R")  # generate analysis plans
### source("SOME_NGA_SPECIFIC_FUNCTIONS")s

# load questionnaire inputs
questions <- read.csv("./input/questions.csv",
                      stringsAsFactors=F, check.names = F)

choices <- read.csv("./input/choices.csv",
                    stringsAsFactors=F, check.names = F)



# 02 a little nettoyages to make sure we are all good
to_alphanumeric_lowercase <-
  function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
names(final)<-to_alphanumeric_lowercase(names(final))

final %<>% replace_with_na_all(condition = ~.x == "na") #this takes a while but is important for recoding


# 03 Load all your inputs (questionaire, samplingframe etc.)

#####
## THE DATA (the one and only) should be 'final', the output from the merge.R and then composite_indicators.R file
## It should also have the composite indicators added at this point !
######

sampling.frame <- load_samplingframe(file = "./input/nga_msna_sampling_frame_strata.csv")

weighting <- map_to_weighting(sampling.frame = sampling.frame,
                              data = final,
                              sampling.frame.population.column ="population",
                              sampling.frame.stratum.column = "lga_pcode",
                              data.stratum.column = "lga")


questionnaire <- load_questionnaire(data = final,
                                    questions = questions,
                                    choices = choices)


design <- map_to_design(data =final, cluster_variable_name = "cluster", weighting_function = weighting)


# 04 Getting ready for analysis

final <- final[,-1] # remove an evil little variable that is giving me trouble

final$vulnerability_index %<>% as.character

# 04.1 LGA level - by population group
analysisplan_lga<-make_analysisplan_all_vars(final,
                                             questionnaire,
                                             repeat.for.variable =  "lga",
                                             hypothesis.type = "direct_reporting" )

analysisplan_lga_hoh <-make_analysisplan_all_vars(final,
                                                  questionnaire,
                                                  repeat.for.variable =  "lga",
                                                  independent.variable = "gender_hoh",
                                                  hypothesis.type = "group_difference" )

analysisplan_lga_hoh <- analysisplan_lga_hoh[-c(1,2,3,6,7,8,211),]

analysisplan_lga_vul <-make_analysisplan_all_vars(final,
                                                  questionnaire,
                                                  repeat.for.variable =  "lga",
                                                  independent.variable = "vulnerability_index",
                                                  hypothesis.type = "group_difference" )

analysisplan_lga_vul <- analysisplan_lga_vul[-c(1,2,3,6,7,8,211),]

# 04.2 State level - by population group

final$vulnerability_index %<>% as.character
final$state_group <- paste(final$state, final$group, sep = "_")

results_ugo <- readRDS("C:/Users/Eliora Henzler/Desktop/result.rds")
chmap_to_master_table(results_ugo$results, "./hoh_lga_everything.csv", questionnaire = questionnaire)

### Here you need to load the new sampling frame Monday made, with the pop numbers by state. Ok ? OK !

# make analysisplan including all questions as dependent variable by HH type, repeated for each state:
analysisplan_state_gr <- make_analysisplan_all_vars(final,
                                                    questionnaire,
                                                    independent.variable = "group", ## here you are comparing among groups for each state
                                                    repeat.for.variable = "state",
                                                    hypothesis.type = "group_difference")



analysisplan_state_hohh_gr <- make_analysisplan_all_vars(final,
                                                         questionnaire,
                                                         repeat.for.variable = "state_group",
                                                         independent.variable = "gender_hoh",
                                                         hypothesis.type = "group_difference")


analysisplan_state_vuln_gr <- make_analysisplan_all_vars(final,
                                                         questionnaire,
                                                         repeat.for.variable = "state_group",
                                                         independent.variable = "vulnerability_index",
                                                         hypothesis.type = "group_difference")


analysisplan_state_vuln <- make_analysisplan_all_vars(final,
                                                      questionnaire,
                                                      repeat.for.variable = "state",
                                                      independent.variable = "vulnerability_index",
                                                      hypothesis.type = "group_difference")

analysisplan_state <- make_analysisplan_all_vars(final,
                                                 questionnaire,
                                                 repeat.for.variable = "state",
                                                 hypothesis.type = "direct_reporting")



analysisplan_state_hohh <- make_analysisplan_all_vars(final,
                                                      questionnaire,
                                                      repeat.for.variable = "state",
                                                      independent.variable = "gender_hoh",
                                                      hypothesis.type = "group_difference")


### EXECUTION

##For each analysiplan you've created run "from_analysisplan_map_to_output" replacing the analysiplan argument as you need


results_state_vul_gr <- from_analysisplan_map_to_output(final, analysisplan = analysisplan_state_vuln_gr,
                                                     weighting = weighting,
                                                     cluster_variable_name = "cluster",
                                                     questionnaire = questionnaire)


results_state_vul_gr %<>% map_to_labeled

map_to_master_table(results_state_vul_gr$results, "./output/results_state_vuln_gr.csv")


## And here for the lga
results_lga<- from_analysisplan_map_to_output(final, analysisplan = analysisplan_lga,
                                              weighting = weighting,
                                              cluster_variable_name = "cluster",
                                              questionnaire = questionnaire)


results_lga %<>% map_to_labeled


map_to_master_table(results_$results, "./output/results_lga_no_disag.csv")
### Now we're doing this for the composite indicators

analysisplan_comps_lga <- cbind("none", "none", "lga", NA, NA, c("coping_gap_index", "protection_index", "education_index", "shelter_index", "earlyrecovery_index", "wash_index", "foodsec_index", "health_index", "vulnerability_index", "impact_index", "msni_nga"),
                                "categorical", "direct_reporting") %>% as.data.frame(stringsAsFactors = F)

analysisplan_comps_state <- cbind("none", "none", "state", NA, NA, c("coping_gap_index", "protection_index", "education_index", "shelter_index", "earlyrecovery_index", "wash_index", "foodsec_index", "health_index", "vulnerability_index", "impact_index", "msni_nga"),
                                "categorical", "direct_reporting") %>% as.data.frame(stringsAsFactors = F)


analysisplan_comps_state_group <- cbind("none", "none", "state", "group", "categorical", c("coping_gap_index", "protection_index", "education_index", "shelter_index", "earlyrecovery_index", "wash_index", "foodsec_index", "health_index", "vulnerability_index", "impact_index", "msni_nga"),
                                  "categorical", "group_difference") %>% as.data.frame(stringsAsFactors = F)


analysisplan_comps_full <- rbind(analysisplan_comps_lga, analysisplan_comps_state,analysisplan_comps_state_group)

analysisplan_comps_full %>% filter(dependent.variable == "shelter_index")

names(analysisplan_comps_full) <- names(analysisplan_lga)

results_comp_full <- from_analysisplan_map_to_output(final, analysisplan = analysisplan_comps_full,
                                                    weighting = weighting,
                                                    cluster_variable_name = "cluster",
                                                    questionnaire = questionnaire)

results_comp_full$results %>% map_to_master_table("./output/composite_full.csv")

results_state <- from_analysisplan_map_to_output(final, analysisplan = analysisplan_comps_state,
                                                 weighting = weighting,
                                                 cluster_variable_name = "cluster",
                                                 questionnaire = questionnaire)



results_state$results %>% map_to_master_table("./output/composite_state.csv")

## Once for my MSNI scores

## MSNI STATE
analysisplan_msni_state <- cbind("none", "none", "state", "group", "categorical", "msni_nga",
                                 "categorical", "group_difference") %>% as.data.frame(stringsAsFactors = F)


analysisplan_msni_state_nogroup <- cbind("none", "none", "lga", NA, NA, "state",
                                       "categorical", "direct_reporting") %>% as.data.frame(stringsAsFactors = F)

names(analysisplan_msni_state_nogroup) <- names(analysisplan_lga)

results_state_msni_nogr <- from_analysisplan_map_to_output(final, analysisplan = analysisplan_msni_state_nogroup,
                                                      weighting = weighting,
                                                      cluster_variable_name = "cluster",
                                                      questionnaire = questionnaire)

map_to_master_table(results_state_msni_nogr$results, "./msni_state_no_gr.csv")


## MSNI LGA
analysisplan_msni_lga <-  cbind("none", "none", "lga", NA, NA, "msni_nga",
                                "categorical", "direct_reporting") %>% as.data.frame(stringsAsFactors = F)


analysisplan_msni_lga_vuln <-  cbind("none", "none", "lga", "vulnerability_index", "categorical", "msni_nga",
                                     "categorical", "group_difference") %>% as.data.frame(stringsAsFactors = F)


names(analysisplan_msni_lga) <- names(analysisplan_lga)
names(analysisplan_msni_lga_vuln) <- names(analysisplan_lga)

results_lga_msni <- from_analysisplan_map_to_output(final, analysisplan = analysisplan_msni_lga,
                                                    weighting = weighting,
                                                    cluster_variable_name = "cluster",
                                                    questionnaire = questionnaire)

results_lga_msni_vuln <- from_analysisplan_map_to_output(final, analysisplan = analysisplan_msni_lga_vuln,
                                                         weighting = weighting,
                                                         cluster_variable_name = "cluster",
                                                         questionnaire = questionnaire)

map_to_master_table(results_lga_msni$results, "./msni_lga.csv")
map_to_master_table(results_lga_msni_vuln$results, "./msni_lga_vuln.csv")
# result_labeled <- result$results %>% lapply(map_to_labeled,questionnaire)

# # exporting only small part of results for speed during testing:
# subset_of_results<- rep(FALSE,length(results$results))
# subset_of_results[500:700]<-TRUE
# some_results<-hypegrammaR:::results_subset(results,logical = subset_of_results)
some_results<-results
# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html
hypegrammaR:::map_to_generic_hierarchical_html(results_lga,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "results_lga.html"
)
# browseURL("summary_by_dependent_var_then_by_repeat_var.html")

map_to_summary_table(results$results,"./lga_overall.csv", questionnaire = questionnaire)

map_to_master_table(results$results, "./output/results_lga.csv", questionnaire = questionnaire)

# not sure this is working correctly.. next on agenda (:
# big_table <- hypegrammaR:::map_to_datamerge(results$results, questionnaire = questionnaire, rows = "repeat.var.value")
#install.packages("rmarkdown")
