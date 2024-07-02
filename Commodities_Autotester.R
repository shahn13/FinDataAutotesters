#Commodities Auto-tester for input into Shiny app
#Compares commodities dataset to a previous dataset

#Libraries
library(pacman)
p_load(tidyverse,stringr,httr,jsonlite,lubridate,readxl,writexl, waldo, arsenal, 
       dataCompareR, dm, openxlsx, janitor)

#Reading in files, replaced by app inputs
#source <- read_excel("Commodities_Datasets_COP18-23_Q4_Excel.xlsx")
#candidate <- read_excel("test_Commodities_Datasets_COP18-23_20240315.xlsx", 
#                        sheet = "test_Commodities_Dataset") 


#Compare by country/OU function:

OU_Comparison <- function(source, candidate) {
  CandidateOU <- candidate %>% rename(operating_unit = operatingunit) %>% 
    group_by(implementation_year, operating_unit) %>% 
    summarize(OU_total_candidate = sum(total_budget, na.rm=T), 
              OU_item_total_candidate = sum(item_quantity, na.rm = T)) %>% 
    select(implementation_year, operating_unit, OU_total_candidate, OU_item_total_candidate) %>% 
    mutate(datatset = "candidate") 
  
  SourceOU <- source %>% rename(operating_unit = operatingunit) %>%
    group_by(implementation_year, operating_unit) %>% 
    summarize(OU_total_source = sum(total_budget, na.rm=T), 
              OU_item_total_source = sum(item_quantity, na.rm=T))  %>% 
    select(implementation_year, operating_unit, OU_total_source, OU_item_total_source) %>% 
    mutate(datatset = "source")  
  
  OUcombined <- full_join(SourceOU, CandidateOU, 
                          by=c("implementation_year", "operating_unit")) %>% 
    mutate(OU_total_delta = OU_total_source-OU_total_candidate, 
           item_delta = OU_item_total_source - OU_item_total_candidate,
           dataset = ifelse(is.na(OU_total_source), "Candidate", 
                            ifelse(is.na(OU_total_candidate), "Source", "Both"))) %>%
    select(dataset, implementation_year, operating_unit, OU_total_source, OU_total_candidate, 
           OU_total_delta, OU_item_total_source, OU_item_total_candidate, item_delta)
  
  OUcombined
}

#compare by OU and mech function
Mech_comparison <- function(source, candidate) {
  CandidateMech <- candidate %>% rename(operating_unit = operatingunit) %>% 
    group_by(implementation_year, operating_unit, mech_code) %>% 
    summarize(Mech_total_candidate = sum(total_budget, na.rm=T), 
              Mech_item_total_candidate = sum(item_quantity, na.rm=T)) %>% 
    select(implementation_year, operating_unit, mech_code, Mech_total_candidate, Mech_item_total_candidate) %>% 
    mutate(datatset = "candidate") 
  
  SourceMech <- source %>% rename(operating_unit = operatingunit) %>%
    group_by(implementation_year, operating_unit, mech_code) %>% 
    summarize(Mech_total_source = sum(total_budget, na.rm=T), 
              Mech_item_total_source = sum(item_quantity, na.rm=T))  %>% 
    select(implementation_year, operating_unit, mech_code, Mech_total_source, Mech_item_total_source) %>% 
    mutate(datatset = "source")  
  
  Mechcombined <- full_join(SourceMech, CandidateMech, by=c("implementation_year", 
                                                            "operating_unit", "mech_code")) %>% 
    mutate(Mech_total_delta = Mech_total_source-Mech_total_candidate, 
           Mech_item_delta = Mech_item_total_source - Mech_item_total_candidate,
           dataset = ifelse(is.na(Mech_total_source), "Candidate", 
                            ifelse(is.na(Mech_total_candidate), "Source", "Both"))) %>%
    select(dataset, implementation_year, operating_unit, mech_code, Mech_total_source, Mech_total_candidate, 
           Mech_total_delta, Mech_item_total_source, Mech_item_total_candidate, Mech_item_delta)
  
  Mechcombined
}


#Comparing major commodities function

Majorcommodities_comparison <- function(source, candidate) {
  CandidateMajor <- candidate %>% rename(operating_unit = operatingunit) %>% 
    group_by(implementation_year, operating_unit, mech_code, major_category) %>% 
    summarize(Major_total_candidate = sum(total_budget, na.rm=T), 
              Major_item_total_candidate = sum(item_quantity, na.rm=T)) %>% 
    select(implementation_year, operating_unit, mech_code, major_category, 
           Major_total_candidate, Major_item_total_candidate) %>% 
    mutate(datatset = "candidate") 
  
  SourceMajor <- source %>% rename(operating_unit = operatingunit) %>%
    group_by(implementation_year, operating_unit, mech_code, major_category) %>% 
    summarize(Major_total_source = sum(total_budget, na.rm=T), 
              Major_item_total_source = sum(item_quantity, na.rm=T))  %>% 
    select(implementation_year, operating_unit, mech_code, major_category, 
           Major_total_source, Major_item_total_source) %>% 
    mutate(datatset = "source")  
  
  Majorcombined <- full_join(SourceMajor, CandidateMajor, by=c("implementation_year", "operating_unit", "mech_code", "major_category")) %>% 
    mutate(Major_total_delta = Major_total_source-Major_total_candidate,
           Major_item_delta = Major_item_total_source - Major_item_total_candidate,
           dataset = ifelse(is.na(Major_total_source), "Candidate", 
                            ifelse(is.na(Major_total_candidate), "Source", "Both"))) %>% 
    select(dataset, implementation_year, operating_unit, mech_code, major_category, 
           Major_total_source, Major_total_candidate, Major_total_delta, 
           Major_item_total_source, Major_item_total_candidate, Major_item_delta)
  Majorcombined
}


#Comparing minor commodities

Minorcommodities_comparison <- function(source, candidate) {
  CandidateMinor <- candidate %>% mutate(across(where(is.character), tolower)) %>% rename(operating_unit = operatingunit) %>%
    group_by(implementation_year, operating_unit, mech_code, major_category, minor_category) %>% 
    summarize(Minor_total_candidate = sum(total_budget, na.rm = T), 
              Minor_item_total_candidate = sum(item_quantity, na.rm = T)) %>% 
    select(implementation_year, operating_unit, mech_code, major_category, 
           minor_category, Minor_total_candidate, Minor_item_total_candidate) %>% mutate(datatset = "candidate") 
  
  SourceMinor <- source %>% mutate(across(where(is.character), tolower)) %>% rename(operating_unit = operatingunit) %>%
    group_by(implementation_year, operating_unit, mech_code, major_category, minor_category) %>% 
    summarize(Minor_total_source = sum(total_budget, na.rm = T), 
              Minor_item_total_source = sum(item_quantity, na.rm=T))  %>% 
    select(implementation_year, operating_unit, mech_code, major_category, 
           minor_category, Minor_total_source, Minor_item_total_source) %>% mutate(datatset = "source")  
  
  Minorcombined <- full_join(SourceMinor, CandidateMinor, 
                             by=c("implementation_year", "operating_unit", 
                                  "mech_code", "major_category", "minor_category")) %>% 
    mutate(Minor_total_delta = Minor_total_source-Minor_total_candidate, 
           item_delta = Minor_item_total_source-Minor_item_total_candidate,
           dataset = ifelse(is.na(Minor_total_source), "Candidate", 
                            ifelse(is.na(Minor_total_candidate), "Source", "Both"))) %>%
    select(dataset, implementation_year, operating_unit, mech_code, major_category, minor_category, 
           Minor_total_candidate, Minor_total_source, Minor_total_delta, Minor_item_total_source, 
           Minor_item_total_candidate, item_delta )
  Minorcombined
}

#Comparing row-wise - this combined key still does not provide an exact match due to NA values and some
#rounding issues, so this tab's output should be taken with a grain of salt, but would hopefully highlight
#any major changes

Rowwise_comparison <- function(source, candidate) {
  CandidateKey <- candidate %>% mutate(across(where(is.character), tolower)) %>%
    rename(operating_unit = operatingunit) %>%
    mutate(row_id = ifelse(grepl("^[C]", row_id, ignore.case = TRUE),
                           substr(row_id, 2, nchar(row_id)),row_id)) %>%
    mutate(unit_cost = ifelse(is.na(unit_cost), 0, unit_cost)) %>%
    mutate(concatkey = paste(mech_code, beneficiary, sub_program, commodity_item, round(unit_cost, digits=3), row_id, round(total_budget, digits = 0), sep = '_')) %>% 
    select(implementation_year, operating_unit, mech_code, major_category, 
           minor_category, concatkey, total_budget, item_quantity, row_id) %>% 
    rename(key_candidate_budget = total_budget, candidate_quantity = item_quantity) %>% mutate(datatset = "candidate") 
  
  SourceKey <- source %>% mutate(across(where(is.character), tolower)) %>% 
    rename(operating_unit = operatingunit) %>%
    mutate(row_id = ifelse(grepl("^[C]", row_id, ignore.case = TRUE),
                           substr(row_id, 2, nchar(row_id)),row_id)) %>%
    mutate(unit_cost = ifelse(is.na(unit_cost), 0, unit_cost)) %>%
    mutate(concatkey = paste(mech_code, beneficiary, sub_program, commodity_item, round(unit_cost, digits=3), row_id, round(total_budget, digits = 0), sep = '_'))%>% 
    select(implementation_year, operating_unit, mech_code, major_category, 
           minor_category, concatkey, total_budget, item_quantity) %>% rename(key_source_budget = total_budget, source_quantity = item_quantity) %>%
    mutate(datatset = "source")  
  
  Keycombined <- full_join(SourceKey, CandidateKey, by=c("concatkey", "implementation_year", "operating_unit", 
                                                         "mech_code", "major_category", "minor_category")) %>% 
    mutate(key_total_delta = key_source_budget-key_candidate_budget, key_item_delta = source_quantity-candidate_quantity,
           dataset = ifelse(is.na(key_candidate_budget), "Source", 
                            ifelse(is.na(key_source_budget), "Candidate", "Both"))) %>%
    select(dataset, implementation_year, operating_unit, mech_code, major_category, minor_category, 
           concatkey, key_source_budget, key_candidate_budget, key_total_delta, source_quantity, candidate_quantity, key_item_delta)
  Keycombined
}

#Output in a combined excel file
#list_of_datasets <- list("OUcomparison" = OUcombined, "Mechcomparison" = Mechcombined,
#                         "MajorCommoditiescomparison" = Majorcombined, "MinorCommoditiescomparison" = Minorcombined,
#                         "Row-wisecomparison" = Keycombined)
#write.xlsx(list_of_datasets, file = "FullcommoditiesQ4toQ1cleanoutput20240321.xlsx")

