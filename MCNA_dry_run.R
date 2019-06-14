# setup

library(dplyr) # data manipulation 
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans
source("functions/remove_responses_from_sumstat.R")  # generate analysis plans
### source("SOME_NGA_SPECIFIC_FUNCTIONS")

# load questionnaire inputs
questions <- read.csv("input/questions.csv", 
                      stringsAsFactors=F, check.names = F)

choices <- read.csv("input/choices.csv", 
                    stringsAsFactors=F, check.names = F)


### remove choice une ligne en trop 
choices <- choices[-252,]

# generate data
response <- xlsform_fill(questions,choices,200)

names(response) <- to_alphanumeric_lowercase(names(response))

questionnaire <- load_questionnaire(data = response,
                                    questions = questions,
                                    choices = choices)


# generate samplingframe
samplingframe <- load_samplingframe(file = "./input/nga_msna_sampling_frame_strata.csv")
# samplingframe <- load_samplingframe("./input/Strata_clusters_population.csv")

weighting <- surveyweights::weighting_fun_from_samplingframe(sampling.frame = samplingframe, 
                                                             data.stratum.column = "lga", 
                                                             sampling.frame.population.column = "population", 
                                                             sampling.frame.stratum.column = "lga_pcode", 
                                                             data = response)

design <- map_to_design(data = response, cluster_variable_name = "cluster", weighting_function = weighting)


analysisplan <- make_analysisplan_all_vars(df= response, questionnaire = questionnaire, repeat.for.variable = "state", independent.variable = "group", hypothesis.type = "group_difference")


short_ap <- analysisplan[c(1:15),]


case <- map_to_case("group_difference", "categorical", "categorical")
result <- map_to_result(data = response, dependent.var = "mhm_material", independent.var = "state", case = case, cluster.variable.name = "cluster", weighting = weighting, questionnaire = questionnaire)


final_result <- from_analysisplan_map_to_output(data = response, analysisplan = short_ap, weighting = weighting, cluster_variable_name = "cluster",questionnaire = questionnaire)

result %>% map_to_labeled(., questionnaire = questionnaire) %>% .$summary.statistic  %>% map_to_file("./summary_stats.csv")

result %>% map_to_visualisation

# add cluster ids

# cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
#                          stringsAsFactors=F, check.names=F)
# 
# response_filtered_w_clusterids <- response_filtered %>% 
#   mutate(strata = paste0(lookup_table$district[match(cluster_location_id,cluster_lookup_table$new_ID)],type_hh))


# horizontal operations / recoding
# 
# r <- data %>%
#   new_recoding(source=how_much_debt, target=hh_with_debt_value) %>%
#   recode_to(0.25,where.num.larger.equal = 505000,otherwise.to=0) %>%
# 
#   new_recoding(target=hh_unemployed) %>%
#   recode_to(0 ,where=!(is.na(response_filtered$work) | is.na(response_filtered$actively_seek_work))) %>%
#   recode_to(0.5,where=(work == "no") & (actively_seek_work == "yes")) %>%
# 
#   new_recoding(source=reasons_for_debt, target=hh_unable_basic_needs) %>%
#   recode_to(0.25, where.selected.any = c("health","food","education","basic_hh_expenditure"), otherwise.to=0) %>%
# 
#   end_recoding

# r <- r %>% mutate(score_livelihoods = hh_with_debt_value+hh_unemployed+hh_unable_basic_needs)

# vertical operations / aggregation

# make analysisplan including all questions as dependent variable by HH type, repeated for each governorate:
analysisplan<-make_analysisplan_all_vars(response,
                                         questionnaire
                                         ,independent.variable = "yes_no_host",
                                         repeat.for.variable = "region",
                                         hypothesis.type = "group_difference" 
                                         )


response$strata<-paste0(response$district,"__",response$yes_no_idp)


strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "strata",
                 data.stratum.column = "strata")

response$general_weights <- strata_weight_fun(response)

response$cluster_id <- paste(response$settlement,response$yes_no_idp,sep = "_")

results <- from_analysisplan_map_to_output(response, analysisplan = analysisplan,
                                          weighting = strata_weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire)

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
hypegrammaR:::map_to_generic_hierarchical_html(some_results,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html"
                                               )
browseURL("summary_by_dependent_var_then_by_repeat_var.html")


# not sure this is working correctly.. next on agenda (:
# big_table <- hypegrammaR:::map_to_datamerge(results$results, questionnaire = questionnaire, rows = "repeat.var.value")

