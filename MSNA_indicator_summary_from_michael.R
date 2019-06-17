#SET WORKING DIRECTORY
setwd("~/Desktop/Nigeria/MSNA/Ready_Data")
#####LOAD DATA#####
main_data <- read_excel("reach_nga_msna_clean_dataset_final.xlsx", sheet = "clean_hh_data")
main_data <- main_data %>% dplyr:: rename(uuid = `_uuid`)

#####LOAD PACKAGES#####
if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(randomForest)) install.packages('randomForest')
library(randomForest)

if (!require(weights)) install.packages('weights')
library(weights)

if (!require(agricolae)) install.packages('agricolae')
library(agricolae)

if (!require(pls)) install.packages('pls')
library(pls)

if (!require(gmodels)) install.packages('gmodels')
library(gmodels)

if (!require(splitstackshape)) install.packages('splitstackshape')
library(splitstackshape)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(scales)) install.packages('scales')
library(scales)

if (!require(tm)) install.packages('tm')
library(tm)

if (!require(SDMTools)) install.packages('SDMTools')
library(SDMTools)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(tibble)) install.packages('tibble')
library(tibble)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(MASS)) install.packages('MASS')
library(MASS)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(foreign)) install.packages('foreign')
library(foreign)

if (!require(sandwich)) install.packages('sandwich')
library(sandwich)

if (!require(lmtest)) install.packages('lmtest')
library(lmtest)

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if (!require(xtable)) install.packages('xtable')
library(xtable)

if (!require(Hmisc)) install.packages('Hmisc')
library(Hmisc)

if (!require(car)) install.packages('car')
library(car)

if (!require(readr)) install.packages('readr')
library(readr)

######################FUNCTIONS######################

#' Make proportions
#'
#'Calculate proportions with or without "dont know"
#'@param data dataframe
#'@param agg_var Name of the geographic aggregation unit(e.g., lga)
#'@param var_name name of variable to be aggregated
#'@param dontknow_action a string, either "exclude" if "dont know" should be excluded OR "included" if "dont know" should included
#'@param dontknow_format IN QUOTATIONS: How "dont know" is spelled in the data
#'@example
#'@export
#'
make_proportions <- function(data,agg_var,var_name, dontknow_action = "exlude", dontknow_format){
  locationz <- as.vector(unique(data[grep(paste0("^",agg_var,"$"),colnames(data))]))
  #locationz <- sort(locationz, decreasing=TRUE)
  data<- add_column(data, onesz = 1)
  
  #dealing with dont know and missing answers
  dontknow_format <- paste0("'",dontknow_format,"'")
  if(dont_denom == "exclude"){
    dont_denom <- FALSE
  } else if (dont_denom == "include") {
    dont_denom <- TRUE
  }
  if(dont_denom == FALSE){
    data  <- data %>%
      dplyr ::filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))])) 
    data  <- dplyr:: filter_(data, paste(var_name,"!=", dontknow_format,sep=" "))
  } else {
    data  <- data %>%
      dplyr ::  filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))]))
  }
  
  #aggregate for each value
  agg_var_indicator <- as.formula(paste0(agg_var,"~",var_name))  
  result <- data %>% dcast(agg_var_indicator, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
  namez <- paste0(names(result)[2:ncol(result)],"_",var_name)
  names(result)[2:ncol(result)] <- namez
  result<- add_column(result, total_respondents = rowSums(result[2:ncol(result)]))
  denom_column <- ncol(result)
  props <- list()
  for(i in 2:(ncol(result)-1)){
    props[[i]]<-  result[i]/result[denom_column]
  }
  props[sapply(props, is.null)] <- NULL
  props <- as.data.frame(props)
  names(props) <- paste0("pr_",names(props))
  result <- data.frame(result, props )
  result<-merge(locationz, result, by=agg_var , all.x=TRUE)
  return(result)
}
#aa<- make_proportions(nonnumeric,lga, 3,TRUE)

############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5; -5 produces bottom 5)
rank_money2 <- function(df, aggunit, toprank) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  if(toprank >= 1){
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  toprank <- abs(toprank)
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = direction),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  return(ordername)
}
#resulttorank <- subset(result, select=c(lga_group,pr_TRUE_b_movement_intentions_b_push_firstreason_push_security,pr_TRUE_b_movement_intentions_b_pull_firstreason_pull_education,pr_TRUE_b_movement_intentions_b_push_firstreason_push_food,pr_TRUE_b_movement_intentions_b_push_firstreason_push_wash,pr_TRUE_b_movement_intentions_b_push_firstreason_push_shelter,pr_TRUE_b_movement_intentions_b_push_firstreason_push_land,pr_TRUE_b_movement_intentions_b_push_firstreason_push_employment_cash))
#aaa <- rank_money2(resulttorank, "lga_group", 3, "highest")

######INDICATOR SUMMARY: Use inside a loop over a whole dataset
#dataa == dataset with indicators to aggregate
#agg_varble == IN QUOTATIONS: The name of the aggregation variable in the dataset
#i == Column index inside the dataset
indicator_summary <- function(dataa,agg_varble, i){
  var_name <- colnames(dataa)[grep(paste0("^",colnames(dataa)[i],"$"),colnames(dataa))]
  agg_and_var <- c(agg_varble, var_name)
  agg_varble_index <- grep(paste0("^",agg_varble,"$"), colnames(dataa))
  if(length(unique(is.na(dataa[,i])) == TRUE)==1){
    aggs <- as.data.frame(unique(dataa[,agg_varble_index]))
    indicator <-  as.data.frame(rep(var_name, length(unique(dataa[,agg_varble_index]))))  
    values <-  as.data.frame(rep(NA, length(unique(dataa[,agg_varble_index]))))
    result <- data.frame(aggs, indicator, values)
    colnames(result) <- c(agg_varble,"indicator","value")
  } else if (is.double(dataa[,i])){
    result <-  dataa %>% dplyr:: select(one_of(agg_and_var)) %>%
      group_by_at(agg_varble) %>%
      dplyr::summarize(Mean = mean(get(var_name), na.rm=TRUE)) %>%
      filter(!is.na(!!sym(agg_varble)))
    colnames(result)[2] <- paste0("avg_",var_name)
  } else if(is.character(dataa[,i])){
    categorical_formula <- as.formula(paste0(agg_varble,"~",var_name)) 
    result <- dataa %>% dplyr::select(one_of(agg_and_var)) %>%
      group_by(!!agg_varble) %>%
      mutate(onesz = 1) %>%
      dcast(categorical_formula, fun.aggregate = sum, value.var= "onesz", na.rm=TRUE) %>%
      dplyr:: select(-c("NA")) %>%
      mutate(total_resp = rowSums(.[2:ncol(.)]))
    total_resp_column <- grep("total_resp", colnames(result))
    colnames(result)[total_resp_column] <- paste0(var_name ,"_",colnames(result)[total_resp_column])
    result_pr <- list() 
     for(k in 2:(total_resp_column-1)){
      colnames(result)[k] <- paste0(var_name ,"_",colnames(result)[k])
      pr_result <- as.data.frame(result[k]/result[total_resp_column])
      colnames(pr_result)<- paste0("pr_", colnames(pr_result))
      result_pr[[k]] <- pr_result
    }
    result_pr[sapply(result_pr, is.null)] <- NULL
    percents <- data.frame(result_pr)
    result <- data.frame(result, percents)
    result <- result %>% filter(!is.na(!!sym(agg_varble)))
  } else {
    print("TROUBLE")
  } 
  return(result)
}
#apple <- indicator_summary(main_data, "lga",j)


######################BEGIN AGGREGATION######################
##REMOVE SPECIAL CHARACTERS AND "consent_yes" FROM HEADERS
names(main_data) <- names(main_data) %<>%
  gsub("/", "_", .) %>%
  gsub("-", "_", .) %>%
  gsub("/", "_", .) %>%
  gsub("\\)", "", .) %>%
  gsub("\\(", "", .) %>%
  gsub(",", "", .) %>%
  gsub("'", "", .) %>%
  gsub("\\?", "", .) %>%
  gsub("’", "", .) %>%
  gsub(" ", "_", .) %>%
  gsub("\\..", "_", .) %>% 
  gsub("\\.", "_", .) %>% 
  gsub("_–_", "_", .) %>%
gsub("___", "_", .) 
names(main_data) <-tolower(names(main_data))
main_data<-as.data.frame(main_data)
#COLUMN OF 1's FOR NATIONAL-LEVEL AGGREGATION 
main_data$oneszz <- 1 
#REMOVE "other_text" INDICATORS
main_data <- main_data %>% dplyr:: select(-contains("_other_text")) 

#CHOOSE LEVEL OF AGGREGATION
agg_varz <- "lga"  #oneszz
#CREATE USABLE POPULATION GROUP COLUMN  
main_data$pop_group_usable <- ifelse(main_data$population_group == "Internally Displaced Persons (IDPs)", "idp", ifelse(main_data$population_group == "Non-displaced","nondisplaced", ifelse(main_data$population_group =="Returnees","returnee",NA )))
#CHOOSE POPULATION GROUP: total; "idp" ; "nondisplaced" ; "returnee" ; NA
popgroup <- "total"

#FILTER POPULATION GROUP
if(popgroup != "total"){
main_data <- main_data %>% dplyr::filter(grepl(popgroup,pop_group_usable))
} else if(popgroup == "total"){
  main_data <- main_data
} else{
  print("NO POPULATION GROUP SELECTED")
}

###FIRST AND LAST VARIABLES TO BE AGGREGATED
first_msna_agg_var <- grep("head_of_household_y_n", colnames(main_data))
last_msna_agg_var <-  grep("what_is_the_third_priority_need_for_your_household_overall", colnames(main_data))
summarized_indicator_all <- list()
for(j in first_msna_agg_var:last_msna_agg_var){
  summarized_indicator <- indicator_summary(main_data, "lga",j)
  summarized_indicator_all[[j]] <-summarized_indicator  %>% 
    gather(indicator, value, -lga)
  print(colnames(summarized_indicator_all[[j]]))
  }
summarized_indicator_all[sapply(summarized_indicator_all, is.null)] <- NULL
finished_indicator_summary  <- do.call(rbind.data.frame, summarized_indicator_all)
finished_indicator_summary <- finished_indicator_summary %>% dplyr::filter(grepl("avg_|pr_",indicator)) %>% dplyr::filter(!grepl("_No|_total_resp|_99",indicator))
write.csv(finished_indicator_summary,"finished_indicator_summary.csv")



