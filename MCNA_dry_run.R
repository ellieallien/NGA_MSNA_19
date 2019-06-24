# setup

library(dplyr) # data manipulation 
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations
library(parallel)
library(knitr)

source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans
source("functions/remove_responses_from_sumstat.R")  # generate analysis plans
### source("SOME_NGA_SPECIFIC_FUNCTIONS")

# load questionnaire inputs
questions <- read.csv("input/questions.csv", 
                      stringsAsFactors=F, check.names = F)

choices <- read.csv("input/choices.csv", 
                    stringsAsFactors=F, check.names = F)



which(choices$name == "cloth_uniform")

### remove choice une ligne en trop 
choices <- choices[-252,]

# generate data
response <- xlsform_fill(questions,choices,400)
response$consent <- "consent"

names(response) <- to_alphanumeric_lowercase(names(response))

questionnaire <- load_questionnaire(data = response,
                                    questions = questions,
                                    choices = choices)


# generate samplingframe
samplingframe <- load_samplingframe(file = "./input/nga_msna_sampling_frame_strata.csv")
# samplingframe <- load_samplingframe("./input/Strata_clusters_population.csv")

weighting <- map_to_weighting(sampling.frame = samplingframe, 
                                                             data.stratum.column = "lga", 
                                                             sampling.frame.population.column = "population", 
                                                             sampling.frame.stratum.column = "lga_pcode", 
                                                             data = response)

design <- map_to_design(data = response, cluster_variable_name = "cluster", weighting_function = weighting)


analysisplan <- make_analysisplan_all_vars(df= response, 
                                           questionnaire = questionnaire, 
                                           repeat.for.variable = "state", 
                                           independent.variable = "group", 
                                           hypothesis.type = "group_difference")


analysisplan_lga <- make_analysisplan_all_vars(df= response, 
                                               questionnaire = questionnaire, 
                                               repeat.for.variable = "lga", 
                                               hypothesis.type = "direct_reporting")


case <- map_to_case("group_difference", "categorical", "categorical")
result <- map_to_result(data = response, 
                        dependent.var = "mhm_material", 
                        independent.var = "state", 
                        case = case, 
                        cluster.variable.name = "cluster", 
                        weighting = weighting, 
                        questionnaire = questionnaire)

# Calculate the final results
final_result <- from_analysisplan_map_to_output(data = response, 
                                                analysisplan = analysisplan, 
                                                weighting = weighting, 
                                                cluster_variable_name = "cluster",
                                                questionnaire = questionnaire)


# Print a massive table with everything (summary stats and p values)
final_result$results %>% map_to_master_table(., filename= "./master_table.csv", questionnaire = questionnaire)


summary.stats <- final_result$results %>% lapply(function(x){map_to_labeled(result = x, questionnaire = questionnaire)}) %>% 
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .) %>% map_to_file("./summary_stats.csv")


final_result %>% map_to_template( questionnaire = questionnaire, dir = "./output", type = "visual", "report.html")

result %>% map_to_visualisation

# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html
hypegrammaR:::map_to_generic_hierarchical_html(final_result,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html")

browseURL("output/summary_by_dependent_var_then_by_repeat_var.html")


# not sure this is working correctly.. next on agenda (:
# big_table <- hypegrammaR:::map_to_datamerge(results$results, questionnaire = questionnaire, rows = "repeat.var.value")

