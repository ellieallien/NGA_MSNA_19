library(composr)
library(magrittr)
names(response) %<>% to_alphanumeric_lowercase()

new_recoding(response, "secu_incident_comp") %>%
  recode_to(source = security_yn, where.selected.exactly = "yes", to = 2) %>%
  recode_to(where.selected.any = c("99", "98"), to = 0)


recoding_rules <- read.csv("./input/recoding.csv", stringsAsFactors = F)

recode_batch(df = response,
             tos = recoding_rules$to_values,
             wheres = recoding_rules$conditions,
             targets = recoding_rules$target_variable,
             questionnaire = questionnaire)

response$num_nodocs
