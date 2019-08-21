containers <- read.csv("./GitHub/NGA_msna_2019/input/containers.csv", stringsAsFactors = F)
response <- read.csv("./GitHub/NGA_msna_2019/input/hh_total.csv", stringsAsFactors = FALSE)
hh_member <- read.csv("./GitHub/NGA_msna_2019/input/hh_members.csv", 
                      stringsAsFactors = F)


containers$total <- containers$volume_ct * containers$num_filled_ct

#containers$total <- as.numeric(containers$interview_enter1_interview_enter2_wash_container_volume_ct) * as.numeric(containers$interview_enter1_interview_enter2_wash_container_num_filled_ct)


merged <- koboloops::affect_loop_to_parent(loop = containers, 
                                           parent = response, 
                                           aggregate.function = sum,
                                           variable.to.add = c(Total_water_hh = "total"), 
                                           uuid.name.loop = "X_submission__uuid", 
                                           uuid.name.parent = "X_uuid")


hh_member$out_of_school <- hh_member$hh_members_eligible_3_18_formal_schl_stat %in% c("never_formal", "drop_out_formal") %>% as.numeric
hh_member$never_school <- hh_member$hh_members_eligible_3_18_formal_schl_stat %in% "never_formal" %>% as.numeric


hehe <- cbind(a = c(hh_member$hh_members_eligble_9m_10_measles == "no"), b= c(hh_member$hh_members_eligible_0_5_polio == "no"), c= c(hh_member$hh_members_eligible_0_5_penta == "no")) %>% as.data.frame

hh_member$no_vacc <- Reduce("&", hehe) %>% as.numeric

hh_member$no_vacc %>% table

hh_member$independent <- hh_member$hh_members_age_hh %in% c(15:65)
hh_member$dependent <- !(hh_member$hh_members_age_hh %in% c(15:65))


hh_member$independent %>% sum

hh_merged <- koboloops::affect_loop_to_parent(loop = hh_member, parent = merged, aggregate.function = sum, 
                                              variable.to.add = c(children_out_formal = "out_of_school", 
                                                                  children_never_formal = "never_school",
                                                                  no_vaccine = "no_vacc", 
                                                                  dependents = "dependent",
                                                                  independents = "independent"),
                                                                  uuid.name.loop = "X_submission__uuid",
                                                                  uuid.name.parent = "X_uuid")


hh_merged <- koboloops::affect_loop_to_parent(loop = hh_member, parent = hh_merged, aggregate.function = sum, 
                                              variable.to.add = c(vulnerability_hh_child_hoh = "hh_members_vulnerability_hh_child_hoh",
                                                                  vulnerability_hh_pregnant = "hh_members_vulnerability_hh_pregnant",
                                          vulnerability_hh_breastfeeding = "hh_members_vulnerability_hh_breastfeeding",
                                          vulnerability_hh_chronically_ill = "hh_members_vulnerability_hh_chronicly_ill",
                                          vulnerability_hh_mentally_disabled = "hh_members_vulnerability_hh_mentally_disabled",
                                          vulnerability_hh_physically_disabled = "hh_members_vulnerability_hh_physically_disabled"), 
                                              uuid.name.loop = "X_submission__uuid",
                                              uuid.name.parent = "X_uuid")
