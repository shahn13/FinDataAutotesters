#Allocated Beneficiary Autotester 8/08/2024

#Creating one function to cycle through calculations for allocated beneficiary for FSD
#Based on the MER data

#Starting with reading in files
library(pacman)
p_load(tidyverse,stringr,httr,jsonlite,lubridate,readxl,writexl, waldo, arsenal, dataCompareR, dm, readr, rlang)
setwd("C:/Users/nshah002/OneDrive - Guidehouse Federal/Documents/Allocated Beneficiary Autotester")
FSD <- read_delim("test_FSD_COP23-24_20240604.txt", col_types = cols(`mech_code` = col_character()))
MER <- read_delim("test_MSD_Country_IM_FY24-25_20240604.txt", col_types = cols(`mech_code` = col_character())) 

#Create a test dataframe for both FSD and MER

#Run this again but with a KP selection of FSD, only for FY24
TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% filter(targeted_beneficiary=="Key Populations" & implementation_year==2024)
TestMER<-MER %>% filter(fiscal_year==2024) #need all of MER to iterate through

#initialize the empty calc dataframe
calc = NULL
#Now loop through the rows:
for (j in 1: nrow(TestFSD)) {
  Mermech <- TestMER %>% filter(mech_code==TestFSD[j,]$mech_code)
  if (length(Mermech$mech_code)!=0){
    MERCTKeyPopsCheck <- Mermech %>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus") %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERHTSKeyPopsCheck <- Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERPrEPKeyPopsCheck <- Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERPREVKeyPopsCheck <- Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop") %>% summarise(tot = sum(targets, na.rm = TRUE))
    if(TestFSD[j,]$targeted_beneficiary=="Key Populations" & TestFSD[j,]$program=="C&T" & !is.na(MERCTKeyPopsCheck)) { 
      cat<-pull(unique(Mermech %>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & !is.na(targets)) %>% select(categoryoptioncomboname) %>% 
                         mutate(cat = gsub(",.*$", "", categoryoptioncomboname)))) # creating a unique vector of cats, without negatives
      for (i in 1:length(cat)){
        if(!is_empty(cat[i])){
          if(cat[i]=="MSM"& !is.na(cat[i])){
            df = as.data.frame(NULL)
            df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                        interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Men Having Sex with Men",
                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & 
                                                                         standardizeddisaggregate=="KeyPop/HIVStatus" & 
                                                                         (categoryoptioncomboname=="MSM, Positive" | categoryoptioncomboname=="MSM, Negative")) %>% #I don't think there are negatives in TX_CURR, but adding in case
                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                      (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus") %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
            calc<-rbind(calc, df)
          } else 
            if(cat[i]=="PWID"& !is.na(cat[i])){
              df = as.data.frame(NULL)
              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "People Who Inject Drugs",
                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & 
                                                                           (categoryoptioncomboname=="PWID, Positive" | categoryoptioncomboname=="PWID, Negative")) %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                        (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus") %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
              calc<-rbind(calc,df)
            } else 
              if(cat[i]=="FSW"& !is.na(cat[i])){
                df = as.data.frame(NULL)
                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                  mutate(allocated_beneficiary = "Sex Workers",
                         calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & 
                                                                             (categoryoptioncomboname=="FSW, Positive" | categoryoptioncomboname=="FSW, Negative")) %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                          (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus") %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                calc<-rbind(calc, df)
              } else 
                if(cat[i]=="TG"& !is.na(cat[i])){
                  df = as.data.frame(NULL)
                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                    mutate(allocated_beneficiary = "Transgender",
                           calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & 
                                                                               (categoryoptioncomboname=="TG, Positive" | categoryoptioncomboname=="TG, Negative")) %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                            (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus") %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                  calc<-rbind(calc, df)
                } else 
                  if(cat[i]=="People in prisons and other enclosed settings"& !is.na(cat[i])){
                    df = as.data.frame(NULL)
                    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                      mutate(allocated_beneficiary = "People in Prisons",
                             calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & 
                                                                                 (categoryoptioncomboname=="People in prisons and other enclosed settings, Positive"|categoryoptioncomboname=="People in prisons and other enclosed settings, Negative")) %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE)))/
                                                              (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus") %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                    calc<-rbind(calc, df)
                  } 
        }#end of empty if cat
        else { #keep the old value if no MER data on key populations
          df = as.data.frame(NULL)
          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
            mutate(allocated_beneficiary = "Key Populations",calc_budget = intervention_total)
          calc<-rbind(calc, df)
        } #end of else if empty cat
      }#end of for loop within C&T
    } else #end of C&T if section************************************
      if(TestFSD[j,]$targeted_beneficiary=="Key Populations" & TestFSD[j,]$program=="HTS" & !is.na(MERHTSKeyPopsCheck)) { #beginning of HTS loop
        cat<-pull(unique(Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & !is.na(targets)) %>% select(categoryoptioncomboname) %>% 
                           mutate(cat = gsub(",.*$", "", categoryoptioncomboname)))) # creating a unique vector of cats, without negatives
        for (i in 1:length(cat)){
          if(!is_empty(cat[i])){
            if(cat[i]=="MSM" & !is.na(cat[i])){
              df = as.data.frame(NULL)
              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "Men Having Sex with Men",
                       calc_budget = as.double(round(((Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & 
                                                                            (categoryoptioncomboname=="MSM, Positive"| categoryoptioncomboname=="MSM, Negative")) %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                        (Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
              calc<-rbind(calc, df)
            } else 
              if(cat[i]=="PWID" & !is.na(cat[i])){
                df = as.data.frame(NULL)
                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                  mutate(allocated_beneficiary = "People Who Inject Drugs",
                         calc_budget = as.double(round(((Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & 
                                                                              (categoryoptioncomboname=="PWID, Positive" | categoryoptioncomboname=="PWID, Negative")) %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                          (Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                calc<-rbind(calc, df)
              } else 
                if(cat[i]=="FSW"& !is.na(cat[i])){
                  df = as.data.frame(NULL)
                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                    mutate(allocated_beneficiary = "Sex Workers",
                           calc_budget = as.double(round(((Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & 
                                                                                (categoryoptioncomboname=="FSW, Positive"|categoryoptioncomboname=="FSW, Negative")) %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                            (Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                  calc<-rbind(calc, df)
                } else 
                  if(cat[i]=="TG"& !is.na(cat[i])){
                    df = as.data.frame(NULL)
                    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                      mutate(allocated_beneficiary = "Transgender",
                             calc_budget = as.double(round(((Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & 
                                                                                  (categoryoptioncomboname=="TG, Positive"|categoryoptioncomboname=="TG, Negative")) %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE)))/
                                                              (Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                    calc<-rbind(calc, df)
                  } else 
                    if(cat[i]=="People in prisons and other enclosed settings"& !is.na(cat[i])){
                      df = as.data.frame(NULL)
                      df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                 interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                        mutate(allocated_beneficiary = "People in Prisons",
                               calc_budget = as.double(round(((Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & 
                                                                                    (categoryoptioncomboname=="People in prisons and other enclosed settings, Positive"|categoryoptioncomboname=="People in prisons and other enclosed settings, Negative")) %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                (Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                      calc<-rbind(calc, df)
                    } 
          }#end of empty if cat
          else { #keep the old value if no MER data on key populations
            df = as.data.frame(NULL)
            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Key Populations",calc_budget = intervention_total)
            calc<-rbind(calc, df)
          } #end of else if empty cat
        }#end of for loop within HTS
      } else #end of HTS if section*****************************************
        if(TestFSD[j,]$targeted_beneficiary=="Key Populations" & TestFSD[j,]$sub_program=="PREV: PrEP" & !is.na(MERPrEPKeyPopsCheck)) { #start of PrEP loop
          cat<-pull(unique(Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr" & !is.na(targets)) %>% select(categoryoptioncomboname) %>% 
                             mutate(cat = gsub(",.*$", "", categoryoptioncomboname)))) # creating a unique vector of cats, without negatives
          for (i in 1:length(cat)){
            if(!is_empty(cat[i])){
              if(cat[i]=="MSM" & !is.na(cat[i])){
                df = as.data.frame(NULL)
                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                  mutate(allocated_beneficiary = "Men Having Sex with Men",
                         calc_budget = as.double(round(((Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr" & 
                                                                              (categoryoptioncomboname=="MSM")) %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                          (Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                calc<-rbind(calc, df)
              } else 
                if(cat[i]=="PWID" & !is.na(cat[i])){
                  df = as.data.frame(NULL)
                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                    mutate(allocated_beneficiary = "People Who Inject Drugs",
                           calc_budget = as.double(round(((Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr" & 
                                                                                (categoryoptioncomboname=="PWID")) %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                            (Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                  calc<-rbind(calc, df)
                } else 
                  if(cat[i]=="FSW" & !is.na(cat[i])){
                    df = as.data.frame(NULL)
                    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                      mutate(allocated_beneficiary = "Sex Workers",
                             calc_budget = as.double(round(((Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr" & 
                                                                                  (categoryoptioncomboname=="FSW")) %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE)))/
                                                              (Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                    calc<-rbind(calc, df)
                  } else 
                    if(cat[i]=="TG" & !is.na(cat[i])){
                      df = as.data.frame(NULL)
                      df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                 interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                        mutate(allocated_beneficiary = "Transgender",
                               calc_budget = as.double(round(((Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr" & 
                                                                                    (categoryoptioncomboname=="TG")) %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                (Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                      calc<-rbind(calc, df)
                    } else 
                      if(cat[i]=="People in prisons and other enclosed settings" & !is.na(cat[i])){
                        df = as.data.frame(NULL)
                        df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                   interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                          mutate(allocated_beneficiary = "People in Prisons",
                                 calc_budget = as.double(round(((Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr" & 
                                                                                      (categoryoptioncomboname=="People in prisons and other enclosed settings")) %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                  (Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% 
                                                                     summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                        calc<-rbind(calc,df)
                      } 
            } #end of empty if cat
            else { #keep the old value if no MER data on key populations
              df = as.data.frame(NULL)
              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "Key Populations",calc_budget = intervention_total)
              calc<-rbind(calc, df)
            } #end of else if empty cat
          } #end of for loop within PrEP
        } else #end of PrEP if section******************************************
          if(TestFSD[j,]$targeted_beneficiary=="Key Populations" & TestFSD[j,]$program=="PREV" & 
             (TestFSD[j,]$sub_program!="PREV: PrEP" & TestFSD[j,]$sub_program!="PREV: VMMC") & !is.na(MERPREVKeyPopsCheck)) { 
            cat<-pull(unique(Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop" & !is.na(targets)) %>% select(categoryoptioncomboname) %>% 
                               mutate(cat = gsub(",.*$", "", categoryoptioncomboname)))) # creating a unique vector of cats, without negatives
            for (i in 1:length(cat)){
              if(!is_empty(cat[i])){
                if(cat[i]=="MSM" & !is.na(cat[i])){
                  df = as.data.frame(NULL)
                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                    mutate(allocated_beneficiary = "Men Having Sex with Men",
                           calc_budget = as.double(round(((Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop" & 
                                                                                (categoryoptioncomboname=="MSM")) %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                            (Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop") %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                  calc<-rbind(calc, df)
                } else 
                  if(cat[i]=="PWID" & !is.na(cat[i])){
                    df = as.data.frame(NULL)
                    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                      mutate(allocated_beneficiary = "People Who Inject Drugs",
                             calc_budget = as.double(round(((Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop" & 
                                                                                  (categoryoptioncomboname=="PWID")) %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE)))/
                                                              (Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop") %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                    calc<-rbind(calc, df)
                  } else 
                    if(cat[i]=="FSW" & !is.na(cat[i])){
                      df = as.data.frame(NULL)
                      df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                 interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                        mutate(allocated_beneficiary = "Sex Workers",
                               calc_budget = as.double(round(((Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop" & 
                                                                                    (categoryoptioncomboname=="FSW")) %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                (Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop") %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                      calc<-rbind(calc, df)
                    } else 
                      if(cat[i]=="TG" & !is.na(cat[i])){
                        df = as.data.frame(NULL)
                        df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                   interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                          mutate(allocated_beneficiary = "Transgender",
                                 calc_budget = as.double(round(((Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop" & 
                                                                                      (categoryoptioncomboname=="TG")) %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                  (Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop") %>% 
                                                                     summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                        calc<-rbind(calc, df)
                      } else 
                        if(cat[i]=="People in prisons and other enclosed settings" & !is.na(cat[i])){
                          df = as.data.frame(NULL)
                          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                            mutate(allocated_beneficiary = "People in Prisons",
                                   calc_budget = as.double(round(((Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop" & 
                                                                                        (categoryoptioncomboname=="People in prisons and other enclosed settings")) %>% 
                                                                     summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                    (Mermech %>% filter(indicator=="KP_PREV" & standardizeddisaggregate=="KeyPop") %>% 
                                                                       summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
                          calc<-rbind(calc, df)
                        } 
              } #end of empty if cat
              else { #keep the old value if no MER data on key populations
                df = as.data.frame(NULL)
                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                  mutate(allocated_beneficiary = "Key Populations",calc_budget = intervention_total)
                calc<-rbind(calc, df)
              } #end of else if empty cat
            } #end of for loop for prev other
          } else #end of PREV other if section******************************************
          { #keep the old value if no MER data on key populations
            df = as.data.frame(NULL)
            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Key Populations",calc_budget = intervention_total)
            calc<-rbind(calc, df)
          }
  } else #end of empty MerMech$mech_code
  { #keep the old value if no match mech code in Mer data (this will need to be pulled out and econmpass all targeted)
    df = as.data.frame(NULL)
    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
      mutate(allocated_beneficiary = "Key Populations",calc_budget = intervention_total)
    calc<-rbind(calc, df)
    
  }
}#end of overall for loop through the rows of FSD

#Check
#calc
#FSD %>% filter(operatingunit=="Cameroon" & mech_code==160107 & targeted_beneficiary=="Key Populations" & program=="C&T" & implementation_year==2024 & interaction_type=="Service Delivery") %>% select(allocated_beneficiary, cop_budget_total)

#Join and Create a delta to check/test:

CheckFSD <- FSD %>%  filter(targeted_beneficiary=="Key Populations" & implementation_year==2024) %>% 
  select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account,
         targeted_beneficiary, allocated_beneficiary, cop_budget_total)

cleancalc <- calc %>% select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account, targeted_beneficiary, allocated_beneficiary, calc_budget) %>%
  rename(calc = calc_budget)

delta<- left_join(CheckFSD, cleancalc, by = c("operatingunit", "mech_code", "implementation_year", "program", "sub_program", "interaction_type", "funding_account", "targeted_beneficiary", "allocated_beneficiary")) %>% 
  mutate(delta = cop_budget_total - calc)

write_xlsx(delta, "Targetedcheck2.xlsx")
#key pops looks good, minor rounding errors causing deltas of 1-2, some balnks with 0 cop budget 
MERCheck<-MER %>% filter(operatingunit=="Asia Region" & fiscal_year==2024)
write_xlsx(MERCheck, "MERCheck.xlsx")  

##Before we get to non-targeted populations let's check Children, OVC, Military, AGYW, and pregnant and breastfeeding

###Children

TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% 
  filter(implementation_year==2024, targeted_beneficiary=="Children")
TestMER<- MER %>% filter(fiscal_year==2024) 

#initialize the empty calc dataframe
calc = NULL
#Now loop through the rows:
for (j in 1: nrow(TestFSD)) {
  Mermech <- TestMER %>% filter(mech_code==TestFSD[j,]$mech_code)
  if (length(Mermech$mech_code)!=0){
    MERCTchildrenCheck <- Mermech %>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus") %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERHTSchildrenCheck <- Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result") %>% summarise(tot = sum(targets, na.rm = TRUE))
    
    if(TestFSD[j,]$targeted_beneficiary=="Children" & TestFSD[j,]$program=="C&T" & !is.na(MERCTchildrenCheck)) { 
      cat<-unique(pull(Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus") & !is.na(targets)) %>% select(sex, trendscoarse, standardizeddisaggregate) %>% 
                         mutate(cat = ifelse(standardizeddisaggregate=="Age/Sex/HIVStatus" & sex=="Male" & trendscoarse=="<15", "Boys", 
                                             ifelse(standardizeddisaggregate=="Age/Sex/HIVStatus" & sex=="Female" & trendscoarse=="<15", "Girls","")))))
      for (i in 1:length(cat)){
        if(!is_empty(cat[i])){
          if(cat[i]=="Girls"& !is.na(cat[i])){
            df = as.data.frame(NULL)
            df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                        interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Girls",
                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & 
                                                                         standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                         (sex=="Female" & (ageasentered=="<01"|ageasentered=="01-04"|ageasentered=="05-09"|ageasentered=="01-09"|ageasentered=="10-14"))) %>% 
                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                      (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus"& trendscoarse=="<15") %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
            calc<-rbind(calc, df)
          } else #End of Girls 
            if(cat[i]=="Boys"& !is.na(cat[i])){
              df = as.data.frame(NULL)
              df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                          interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "Boys",
                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & 
                                                                           standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                           (sex=="Male" & (ageasentered=="<01"|ageasentered=="01-04"|ageasentered=="05-09"|ageasentered=="01-09"|ageasentered=="10-14"))) %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                        (Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15") %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
              calc<-rbind(calc, df)
            } #End of Boys 
        } #end of if empty cat
        else { #keep the old value if no MER data on key populations
          df = as.data.frame(NULL)
          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
            mutate(allocated_beneficiary = "Children",calc_budget = intervention_total)
          calc<-rbind(calc, df)
        } #end of else if empty cat
      }#end of for loop for cat
    }#end of if children C&T
    if(TestFSD[j,]$targeted_beneficiary=="Children" & TestFSD[j,]$program=="HTS" & !is.na(MERHTSchildrenCheck)) { 
      cat<-unique(pull(Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result") & !is.na(targets)) %>% select(sex, trendscoarse, standardizeddisaggregate) %>% 
                         mutate(cat = ifelse(standardizeddisaggregate=="Modality/Age/Sex/Result" & sex=="Male" & trendscoarse=="<15", "Boys", 
                                             ifelse(standardizeddisaggregate=="Modality/Age/Sex/Result" & sex=="Female" & trendscoarse=="<15", "Girls","")))))
      for (i in 1:length(cat)){
        if(!is_empty(cat[i])){
          if(cat[i]=="Girls"& !is.na(cat[i])){
            df = as.data.frame(NULL)
            df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                        interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Girls",
                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & 
                                                                         standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                         (sex=="Female" & (ageasentered=="<01"|ageasentered=="01-04"|ageasentered=="05-09"|ageasentered=="01-09"|ageasentered=="10-14"))) %>% 
                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                      (Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result"& trendscoarse=="<15") %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
            calc<-rbind(calc, df)
          } else #End of Girls 
            if(cat[i]=="Boys"& !is.na(cat[i])){
              df = as.data.frame(NULL)
              df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                          interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "Boys",
                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & 
                                                                           standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                           (sex=="Male" & (ageasentered=="<01"|ageasentered=="01-04"|ageasentered=="05-09"|ageasentered=="01-09"|ageasentered=="10-14"))) %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                        (Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15") %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE))) * intervention_total),0)))
              calc<-rbind(calc, df)
            } #End of Boys 
        } #end of if empty cat
        else { #keep the old value if no MER data on key populations
          df = as.data.frame(NULL)
          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
            mutate(allocated_beneficiary = "Children",calc_budget = intervention_total)
          calc<-rbind(calc, df)
        } #end of else if empty cat
      }#end of for loop for cat
    }#end of if children HTS
  }# end of if mech code is empty
}#end of overall for loop


CheckFSD <- FSD %>%  filter(implementation_year==2024, targeted_beneficiary=="Children") %>% 
  select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account,
         targeted_beneficiary, allocated_beneficiary, cop_budget_total)

cleancalc <- calc %>% select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account, targeted_beneficiary, allocated_beneficiary, calc_budget) %>%
  rename(calc = calc_budget)

delta<- left_join(CheckFSD, cleancalc, by = c("operatingunit", "mech_code", "implementation_year", "program", "sub_program", "interaction_type", "funding_account", "targeted_beneficiary", "allocated_beneficiary")) %>% 
  mutate(delta = cop_budget_total - calc)

write_xlsx(delta, "Childrencheck2.xlsx")





###Code for pregnant and breastfeeding, OVC, military, and AGYW


TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% 
  filter(implementation_year==2024, (targeted_beneficiary=="Pregnant & Breastfeeding Women" | targeted_beneficiary=="OVC" |
                                       targeted_beneficiary=="Military" | targeted_beneficiary=="AGYW"))
TestMER<- MER %>% filter(fiscal_year==2024) 

#initialize the empty calc dataframe
calc = NULL
#Now loop through the rows:
for (j in 1: nrow(TestFSD)) {
  Mermech <- TestMER %>% filter(mech_code==TestFSD[j,]$mech_code)
  if (length(Mermech$mech_code)!=0){
    if(TestFSD[j,]$targeted_beneficiary=="Pregnant & Breastfeeding Women") { 
      df = as.data.frame(NULL)
      df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                 interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
        mutate(allocated_beneficiary = "Pregnant & Breastfeeding Women",calc_budget = intervention_total)
      calc<-rbind(calc, df)
    } else #end of PBFW
      if (TestFSD[j,]$targeted_beneficiary=="OVC"){ 
        df = as.data.frame(NULL)
        df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                   interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
          mutate(allocated_beneficiary = "OVC",calc_budget = intervention_total)
        calc<-rbind(calc, df)
      } else #end of OVC
        if (TestFSD[j,]$targeted_beneficiary=="Military"){ 
          df = as.data.frame(NULL)
          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
            mutate(allocated_beneficiary = "Military",calc_budget = intervention_total)
          calc<-rbind(calc, df)
        } else #End of Military
          if (TestFSD[j,]$targeted_beneficiary=="AGYW"){ 
            df = as.data.frame(NULL)
            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "AGYW",calc_budget = intervention_total)
            calc<-rbind(calc, df)
          } #end of if for AGYW
  }#end of if for empty mech_code
}#end of for loop



CheckFSD <- FSD %>%  filter(implementation_year==2024, (targeted_beneficiary=="Pregnant & Breastfeeding Women" | targeted_beneficiary=="OVC" |
                                                          targeted_beneficiary=="Military" | targeted_beneficiary=="AGYW")) %>% 
  select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account,
         targeted_beneficiary, allocated_beneficiary, cop_budget_total)

cleancalc <- calc %>% select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account, targeted_beneficiary, allocated_beneficiary, calc_budget) %>%
  rename(calc = calc_budget)

delta<- left_join(CheckFSD, cleancalc, by = c("operatingunit", "mech_code", "implementation_year", "program", "sub_program", "interaction_type", "funding_account", "targeted_beneficiary", "allocated_beneficiary")) %>% 
  mutate(delta = cop_budget_total - calc)

write_xlsx(delta, "NonAllcheck.xlsx")














#Test non-targeted populations: Adult men and adult women
#Cameroon 81581
#Botswana 84043
TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% filter((program=="C&T" & 
                                                                               implementation_year==2024 & (mech_code==81997) & targeted_beneficiary=="Non-Targeted Populations"))
TestMER<- MER %>% filter((fiscal_year==2024 & (mech_code==81997)))



TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% filter((program=="C&T" & 
                                                                               implementation_year==2024 & targeted_beneficiary=="Non-Targeted Populations"))
TestMER<- MER %>% filter((fiscal_year==2024))

#initialize the empty calc dataframe
calc = NULL
#Now loop through the rows:
#Given that non-targeted is dependent on if Key-pops or children or pregnant and breastfeeding have already been allocated, need to remove that amount from the denominator
#Unique targeted_beneficiary: unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program) %>% select(targeted_beneficiary))), now we just need to do a grepl or select or match on these
#Options are just non-targeted
#nontargeted +children, nontargeted+PBFW+nontargeted+key-pops
#So need a length conditional to determine the aprropriate calculation here
for (j in 1: nrow(TestFSD)) {
  Mermech <- TestMER %>% filter(mech_code==TestFSD[j,]$mech_code)
  if (length(Mermech$mech_code)!=0){
    MERCTNTCheck <- Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator") | (indicator=="TX_CURR" & (standardizeddisaggregate=="Age/Sex/HIVStatus" | standardizeddisaggregate=="KeyPop/HIVStatus"))) %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERHTSNTCheck <- Mermech %>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result") %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERPrEPNTsCheck <- Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% summarise(tot = sum(targets, na.rm = TRUE))
    
    if(TestFSD[j,]$targeted_beneficiary=="Non-Targeted Populations" & TestFSD[j,]$program=="C&T" & !is.na(MERCTNTCheck)) { 
      cat<-unique(pull(Mermech %>% filter(((indicator=="PMTCT_ART" & standardizeddisaggregate=="Total Numerator") |(indicator=="TX_CURR" & (standardizeddisaggregate=="Age/Sex/HIVStatus"|standardizeddisaggregate=="KeyPop/HIVStatus"))) & !is.na(targets)) %>% select(sex, ageasentered, categoryoptioncomboname, standardizeddisaggregate) %>% 
                         mutate(catlong = ifelse(standardizeddisaggregate=="KeyPop/HIVStatus",gsub(",.*$", "", categoryoptioncomboname), paste(sex, ageasentered)),
                                cat = ifelse(catlong=="MSM"|catlong=="FSW"|catlong=="PWID"|catlong=="TG"|catlong=="People in prisons and other enclosed settings", catlong,
                                             ifelse(catlong=="Female <01"|catlong=="Female 01-04"|catlong=="Female 05-09"|catlong=="Female 01-09"|catlong=="Female 10-14", "Girls",
                                                    ifelse(catlong=="Male <01"|catlong=="Male 01-04"|catlong=="Male 05-09"|catlong=="Male 01-09"|catlong=="Male 10-14", "Boys",
                                                           ifelse(catlong=="Female 15-24"|catlong=="Female 15-19"|catlong=="Female 20-24", "AGYW",
                                                                  ifelse(catlong=="Male 15-24"|catlong=="Male 15-19"|catlong=="Male 20-24", "ABYM",
                                                                         ifelse(catlong=="Female 25-29"|catlong=="Female 25-34"|catlong=="Female 30-34"|catlong=="Female 35-49"|catlong=="Female 35-39"|catlong=="Female 40-44"|catlong=="Female 45-49"|catlong=="Female 50+"|catlong=="Female 50-54"|catlong=="Female 55-59"|catlong=="Female 60-64"|catlong=="Female 65+", "Adult Women",
                                                                                ifelse(catlong=="Male 25-29"|catlong=="Male 25-34"|catlong=="Male 30-34"|catlong=="Male 35-49"|catlong=="Male 35-39"|catlong=="Male 40-44"|catlong=="Male 45-49"|catlong=="Male 50+"|catlong=="Male 50-54"|catlong=="Male 55-59"|catlong=="Male 60-64"|catlong=="Male 65+", "Adult Men", "Pregnant & Breastfeeding Women")))))))))) # creating a unique vector of cats, without negatives
      for (i in 1:length(cat)){
        if(!is_empty(cat[i])){
          if(cat[i]=="Adult Women"& !is.na(cat[i])){
            df = as.data.frame(NULL)
            df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                        interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Adult Women",
                     denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                     denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                     denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                     denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & 
                                                                         standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                         (sex=="Female" & (ageasentered=="25-29"|ageasentered=="25-34"|ageasentered=="30-34"|ageasentered=="35-49"|ageasentered=="35-39"|ageasentered=="40-44"|ageasentered=="45-49"|ageasentered=="50+"|ageasentered=="50-54"|ageasentered=="55-59"|ageasentered=="60-64"|ageasentered=="65+"))) %>% 
                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                      ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                          summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
              select(operatingunit, mech_code,implementation_year, program, sub_program,
                     interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
            calc<-rbind(calc, df)
          } else #End of Adult Women  
            if(cat[i]=="Adult Men"& !is.na(cat[i])){
              df = as.data.frame(NULL)
              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "Adult Men",
                       denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                       denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                       denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                       denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                           (sex=="Male" & (ageasentered=="25-29"|ageasentered=="25-34"|ageasentered=="30-34"|ageasentered=="35-49"|ageasentered=="35-39"|ageasentered=="40-44"|ageasentered=="45-49"|ageasentered=="50+"|ageasentered=="50-54"|ageasentered=="55-59"|ageasentered=="60-64"|ageasentered=="65+"))) %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                        ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                            summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                select(operatingunit, mech_code,implementation_year, program, sub_program,
                       interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
              calc<-rbind(calc,df)
            } else #End of Adult Men
              if(cat[i]=="ABYM"& !is.na(cat[i])){
                df = as.data.frame(NULL)
                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                  mutate(allocated_beneficiary = "ABYM",
                         denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                         denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                         denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                         denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                         calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                             (sex=="Male" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24"))) %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                          ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                              summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                  select(operatingunit, mech_code,implementation_year, program, sub_program,
                         interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                calc<-rbind(calc,df)
              } else #End of ABYM
                if(cat[i]=="AGYW"& !is.na(cat[i]) & !"AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                {
                  df = as.data.frame(NULL)
                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                    mutate(allocated_beneficiary = "AGYW",
                           denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                           denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                           denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                           denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                           calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                               (sex=="Female" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24"))) %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                            ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                    select(operatingunit, mech_code,implementation_year, program, sub_program,
                           interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                  calc<-rbind(calc,df)
                } else # end of AGYW
                  if(cat[i]=="Boys"& !is.na(cat[i]) & !"Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                  {
                    df = as.data.frame(NULL)
                    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                      mutate(allocated_beneficiary = "Boys",
                             denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                             denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                             denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                             denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                             calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                                 (sex=="Male" & trendscoarse=="<15")) %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE)))/
                                                              ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                  summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                      select(operatingunit, mech_code,implementation_year, program, sub_program,
                             interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                    calc<-rbind(calc,df)
                  } else # End Boys
                    if(cat[i]=="Girls"& !is.na(cat[i]) & !"Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                    {
                      df = as.data.frame(NULL)
                      df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                 interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                        mutate(allocated_beneficiary = "Girls",
                               denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                               denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                               denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                               denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                               calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & 
                                                                                   (sex=="Female" & trendscoarse=="<15")) %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                    summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                        select(operatingunit, mech_code,implementation_year, program, sub_program,
                               interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                      calc<-rbind(calc,df)
                    } else # End girls
                      if(cat[i]=="MSM"& !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                      {
                        df = as.data.frame(NULL)
                        df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                   interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                          mutate(allocated_beneficiary = "Men Having Sex with Men",
                                 denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                 denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                 denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                 denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                 calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & otherdisaggregate_sub=="MSM") %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                  ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                      summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                          select(operatingunit, mech_code,implementation_year, program, sub_program,
                                 interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                        calc<-rbind(calc,df)
                      } else # End MSM
                        if(cat[i]=="FSW" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                        {
                          df = as.data.frame(NULL)
                          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                            mutate(allocated_beneficiary = "Sex Workers",
                                   denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                   denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                   denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                   denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                   calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & otherdisaggregate_sub=="FSW") %>% 
                                                                     summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                    ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                        summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                            select(operatingunit, mech_code,implementation_year, program, sub_program,
                                   interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                          calc<-rbind(calc,df)
                        } else # End FSW
                          if(cat[i]=="PWID" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                          {
                            df = as.data.frame(NULL)
                            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                              mutate(allocated_beneficiary = "People Who Inject Drugs",
                                     denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                     denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                     denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                     denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & otherdisaggregate_sub=="PWID") %>% 
                                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                      ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                          summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                              select(operatingunit, mech_code,implementation_year, program, sub_program,
                                     interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                            calc<-rbind(calc,df)
                          } else # End PWID
                            if(cat[i]=="TG" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                            {
                              df = as.data.frame(NULL)
                              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                                mutate(allocated_beneficiary = "Transgender",
                                       denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                       denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                       denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                       denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & otherdisaggregate_sub=="TG") %>% 
                                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                        ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                            summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                                select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                              calc<-rbind(calc,df)
                            } else # End TG
                              if(cat[i]=="People in prisons and other enclosed settings" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                              {
                                df = as.data.frame(NULL)
                                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                                  mutate(allocated_beneficiary = "People in Prisons",
                                         denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                         denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                         denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                         denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                         calc_budget = as.double(round(((Mermech%>% filter(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus" & otherdisaggregate_sub=="People in prisons and other enclosed settings") %>% 
                                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                          ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                              summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                                  select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                                calc<-rbind(calc,df)
                              } else # End enclosed
                                if(cat[i]=="Pregnant & Breastfeeding Women" & !is.na(cat[i]) & !"Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                                {
                                  df = as.data.frame(NULL)
                                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                                    mutate(allocated_beneficiary = "Pregnant & Breastfeeding Women",
                                           denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                           denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                           denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                           denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                           calc_budget = as.double(round(((Mermech%>% filter(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator") %>% 
                                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                            ((Mermech%>% filter((indicator=="TX_CURR" & standardizeddisaggregate=="Age/Sex/HIVStatus")|(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")|(indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator")) %>% 
                                                                                summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                                    select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                                  calc<-rbind(calc,df)
                                }# PBFW
          else { #keep the old value if no MER for C&T
            df = as.data.frame(NULL)
            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Non-Targeted Populations",calc_budget = intervention_total)
            calc<-rbind(calc, df)
          }#end of else no MER
        } #end of else if empty cat
      }#end of for loop within C&T
    } #end of C&T if section************************************
  } #end of if checking if mech_code exits in MER
} #end of for loop overall


CheckFSD <- FSD %>%  filter(implementation_year==2024 & program=="C&T" &  targeted_beneficiary=="Non-Targeted Populations") %>% 
  select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account,
         targeted_beneficiary, allocated_beneficiary, cop_budget_total)

cleancalc <- calc %>% select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account, targeted_beneficiary, allocated_beneficiary, calc_budget) %>%
  rename(calc = calc_budget)

delta<- left_join(CheckFSD, cleancalc, by = c("operatingunit", "mech_code", "implementation_year", "program", "sub_program", "interaction_type", "funding_account", "targeted_beneficiary", "allocated_beneficiary")) %>% 
  mutate(delta = cop_budget_total - calc)


#HTS loop is incomplete, calculations are off for adult women, AGYW, girls:

#Test non-targeted populations: HTS

TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% filter((program=="HTS" & 
                                                                               implementation_year==2024 & (mech_code==70108) & targeted_beneficiary=="Non-Targeted Populations"))
TestMER<- MER %>% filter((fiscal_year==2024 & (mech_code==70108)))



TestFSD <- FSD %>% group_by(operatingunit, implementation_year, mech_code, program, sub_program, interaction_type, funding_account, targeted_beneficiary) %>% 
  summarize(intervention_total = sum(cop_budget_total, na.rm=T)) %>% filter((program=="HTS" & 
                                                                               implementation_year==2024 & targeted_beneficiary=="Non-Targeted Populations"))
TestMER<- MER %>% filter((fiscal_year==2024))

#initialize the empty calc dataframe
calc = NULL
#Now loop through the rows:
#Given that non-targeted is dependent on if Key-pops or children or pregnant and breastfeeding have already been allocated, need to remove that amount from the denominator
#Unique targeted_beneficiary: unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program) %>% select(targeted_beneficiary))), now we just need to do a grepl or select or match on these
#Options are just non-targeted
#nontargeted +children, nontargeted+PBFW+nontargeted+key-pops
#So need a length conditional to determine the aprropriate calculation here
for (j in 1: nrow(TestFSD)) {
  Mermech <- TestMER %>% filter(mech_code==TestFSD[j,]$mech_code)
  if (length(Mermech$mech_code)!=0){
    MERCTNTCheck <- Mermech %>% filter((indicator=="PMTCT_ART"& standardizeddisaggregate=="Total Numerator") | (indicator=="TX_CURR" & (standardizeddisaggregate=="Age/Sex/HIVStatus" | standardizeddisaggregate=="KeyPop/HIVStatus"))) %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERHTSNTCheck <- Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))
    MERPrEPNTsCheck <- Mermech %>% filter(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr") %>% summarise(tot = sum(targets, na.rm = TRUE))
    
    if(TestFSD[j,]$targeted_beneficiary=="Non-Targeted Populations" & TestFSD[j,]$program=="HTS" & !is.na(MERHTSNTCheck)) { 
      cat<-unique(pull(Mermech %>% filter(((indicator=="HTS_TST" & (standardizeddisaggregate=="Modality/Age/Sex/Result"|standardizeddisaggregate=="KeyPop/Result"))) & !is.na(targets)) %>% select(sex, ageasentered, categoryoptioncomboname, standardizeddisaggregate) %>% 
                         mutate(catlong = ifelse(standardizeddisaggregate=="KeyPop/Result",gsub(",.*$", "", categoryoptioncomboname), paste(sex, ageasentered)),
                                cat = ifelse(catlong=="MSM"|catlong=="FSW"|catlong=="PWID"|catlong=="TG"|catlong=="People in prisons and other enclosed settings", catlong,
                                             ifelse(catlong=="Female <01"|catlong=="Female 01-04"|catlong=="Female 05-09"|catlong=="Female 01-09"|catlong=="Female 10-14", "Girls",
                                                    ifelse(catlong=="Male <01"|catlong=="Male 01-04"|catlong=="Male 05-09"|catlong=="Male 01-09"|catlong=="Male 10-14", "Boys",
                                                           ifelse(catlong=="Female 15-24"|catlong=="Female 15-19"|catlong=="Female 20-24", "AGYW",
                                                                  ifelse(catlong=="Male 15-24"|catlong=="Male 15-19"|catlong=="Male 20-24", "ABYM",
                                                                         ifelse(catlong=="Female 25-29"|catlong=="Female 25-34"|catlong=="Female 30-34"|catlong=="Female 35-49"|catlong=="Female 35-39"|catlong=="Female 40-44"|catlong=="Female 45-49"|catlong=="Female 50+"|catlong=="Female 50-54"|catlong=="Female 55-59"|catlong=="Female 60-64"|catlong=="Female 65+", "Adult Women",
                                                                                ifelse(catlong=="Male 25-29"|catlong=="Male 25-34"|catlong=="Male 30-34"|catlong=="Male 35-49"|catlong=="Male 35-39"|catlong=="Male 40-44"|catlong=="Male 45-49"|catlong=="Male 50+"|catlong=="Male 50-54"|catlong=="Male 55-59"|catlong=="Male 60-64"|catlong=="Male 65+", "Adult Men", "Pregnant & Breastfeeding Women")))))))))) # creating a unique vector of cats, without negatives
      for (i in 1:length(cat)){
        if(!is_empty(cat[i])){
          if(cat[i]=="Adult Women"& !is.na(cat[i])){
            df = as.data.frame(NULL)
            df<- TestFSD[j,] %>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                        interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Adult Women",
                     denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                     denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                     denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                     denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter(indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & 
                                                                         standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                         (sex=="Female" & (ageasentered=="25-29"|ageasentered=="25-34"|ageasentered=="30-34"|ageasentered=="35-49"|ageasentered=="35-39"|ageasentered=="40-44"|ageasentered=="45-49"|ageasentered=="50+"|ageasentered=="50-54"|ageasentered=="55-59"|ageasentered=="60-64"|ageasentered=="65+"))) %>% 
                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                      ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                          summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
              select(operatingunit, mech_code,implementation_year, program, sub_program,
                     interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
            calc<-rbind(calc, df)
          } else #End of Adult Women  
            if(cat[i]=="Adult Men"& !is.na(cat[i])){
              df = as.data.frame(NULL)
              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                mutate(allocated_beneficiary = "Adult Men",
                       denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                       denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                       denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                       denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                           (sex=="Male" & (ageasentered=="25-29"|ageasentered=="25-34"|ageasentered=="30-34"|ageasentered=="35-49"|ageasentered=="35-39"|ageasentered=="40-44"|ageasentered=="45-49"|ageasentered=="50+"|ageasentered=="50-54"|ageasentered=="55-59"|ageasentered=="60-64"|ageasentered=="65+"))) %>% 
                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                        ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                            summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                select(operatingunit, mech_code,implementation_year, program, sub_program,
                       interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
              calc<-rbind(calc,df)
            } else #End of Adult Men
              if(cat[i]=="ABYM"& !is.na(cat[i])){
                df = as.data.frame(NULL)
                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                  mutate(allocated_beneficiary = "ABYM",
                         denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                         denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                         denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                         denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                         calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                             (sex=="Male" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24"))) %>% 
                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                          ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                              summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                  select(operatingunit, mech_code,implementation_year, program, sub_program,
                         interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                calc<-rbind(calc,df)
              } else #End of ABYM
                if(cat[i]=="AGYW"& !is.na(cat[i]) & !"AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                {
                  df = as.data.frame(NULL)
                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                    mutate(allocated_beneficiary = "AGYW",
                           denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                           denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                           denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                           denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                           calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                               (sex=="Female" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24"))) %>% 
                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                            ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                    select(operatingunit, mech_code,implementation_year, program, sub_program,
                           interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                  calc<-rbind(calc,df)
                } else # end of AGYW
                  if(cat[i]=="Boys"& !is.na(cat[i]) & !"Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                  {
                    df = as.data.frame(NULL)
                    df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                               interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                      mutate(allocated_beneficiary = "Boys",
                             denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                             denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                             denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                             denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                             calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                                 (sex=="Male" & trendscoarse=="<15")) %>% 
                                                               summarise(tot = sum(targets, na.rm = TRUE)))/
                                                              ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                  summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                      select(operatingunit, mech_code,implementation_year, program, sub_program,
                             interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                    calc<-rbind(calc,df)
                  } else # End Boys
                    if(cat[i]=="Girls"& !is.na(cat[i]) & !"Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                    {
                      df = as.data.frame(NULL)
                      df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                 interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                        mutate(allocated_beneficiary = "Girls",
                               denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                               denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                               denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                               denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                               calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & 
                                                                                   (sex=="Female" & trendscoarse=="<15")) %>% 
                                                                 summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                    summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                        select(operatingunit, mech_code,implementation_year, program, sub_program,
                               interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                      calc<-rbind(calc,df)
                    } else # End girls
                      if(cat[i]=="MSM"& !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                      {
                        df = as.data.frame(NULL)
                        df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                   interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                          mutate(allocated_beneficiary = "Men Having Sex with Men",
                                 denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                 denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                 denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                 denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                 calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & otherdisaggregate_sub=="MSM") %>% 
                                                                   summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                  ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                      summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                          select(operatingunit, mech_code,implementation_year, program, sub_program,
                                 interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                        calc<-rbind(calc,df)
                      } else # End MSM
                        if(cat[i]=="FSW" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                        {
                          df = as.data.frame(NULL)
                          df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                     interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                            mutate(allocated_beneficiary = "Sex Workers",
                                   denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                   denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                   denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                   denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                   calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & otherdisaggregate_sub=="FSW") %>% 
                                                                     summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                    ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                        summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                            select(operatingunit, mech_code,implementation_year, program, sub_program,
                                   interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                          calc<-rbind(calc,df)
                        } else # End FSW
                          if(cat[i]=="PWID" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                          {
                            df = as.data.frame(NULL)
                            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                              mutate(allocated_beneficiary = "People Who Inject Drugs",
                                     denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                     denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                     denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                     denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                     calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & otherdisaggregate_sub=="PWID") %>% 
                                                                       summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                      ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                          summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                              select(operatingunit, mech_code,implementation_year, program, sub_program,
                                     interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                            calc<-rbind(calc,df)
                          } else # End PWID
                            if(cat[i]=="TG" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                            {
                              df = as.data.frame(NULL)
                              df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                         interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                                mutate(allocated_beneficiary = "Transgender",
                                       denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                       denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                       denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                       denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                       calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & otherdisaggregate_sub=="TG") %>% 
                                                                         summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                        ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                            summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                                select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                              calc<-rbind(calc,df)
                            } else # End TG
                              if(cat[i]=="People in prisons and other enclosed settings" & !is.na(cat[i]) & !"Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                              {
                                df = as.data.frame(NULL)
                                df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                           interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                                  mutate(allocated_beneficiary = "People in Prisons",
                                         denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                         denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                         denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                         denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                         calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result" & otherdisaggregate_sub=="People in prisons and other enclosed settings") %>% 
                                                                           summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                          ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                              summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                                  select(operatingunit, mech_code,implementation_year, program, sub_program,
                                         interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                                calc<-rbind(calc,df)
                              } else # End enclosed
                                if(cat[i]=="Pregnant & Breastfeeding Women" & !is.na(cat[i]) & !"Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))))
                                {
                                  df = as.data.frame(NULL)
                                  df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                                             interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
                                    mutate(allocated_beneficiary = "Pregnant & Breastfeeding Women",
                                           denomKP = ifelse("Key Populations" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% summarise(tot = sum(targets, na.rm = TRUE))), 0),
                                           denomChildren = ifelse("Children" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & trendscoarse=="<15")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                           denomAGYW = ifelse("AGYW" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result" & (ageasentered=="15-24"|ageasentered=="15-19"|ageasentered=="20-24") & sex=="Female")) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                           denomPBFW = ifelse("Pregnant & Breastfeeding Women" %in% (unique(pull(FSD %>% filter(mech_code==TestFSD[j,]$mech_code & sub_program==TestFSD[j,]$sub_program & implementation_year==2024 & interaction_type==TestFSD[j,]$interaction_type) %>% select(targeted_beneficiary)))), (Mermech %>% filter((indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1"))) %>% summarise(tot = sum(targets, na.rm = TRUE))),0),
                                           calc_budget = as.double(round(((Mermech%>% filter(indicator=="HTS_TST"& (modality_raw=="PMTCT ANC" |modality_raw=="Post ANC1")) %>% 
                                                                             summarise(tot = sum(targets, na.rm = TRUE)))/
                                                                            ((Mermech%>% filter((indicator=="HTS_TST" & standardizeddisaggregate=="Modality/Age/Sex/Result")|(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>% 
                                                                                summarise(tot = sum(targets, na.rm = TRUE)))-denomKP-denomChildren-denomPBFW-denomAGYW) * intervention_total),0))) %>%
                                    select(operatingunit, mech_code,implementation_year, program, sub_program,
                                           interaction_type, funding_account, intervention_total, targeted_beneficiary, allocated_beneficiary, calc_budget)
                                  calc<-rbind(calc,df)
                                }# PBFW
          else { #keep the old value if no MER for HTS
            df = as.data.frame(NULL)
            df<- TestFSD[j,]%>% select(operatingunit, mech_code,implementation_year, program, sub_program,
                                       interaction_type, funding_account, intervention_total, targeted_beneficiary) %>% 
              mutate(allocated_beneficiary = "Non-Targeted Populations",calc_budget = intervention_total)
            calc<-rbind(calc, df)
          }#end of else no MER
        } #end of else if empty cat
      }#end of for loop within HTS
    } #end of HTS if section************************************
  } #end of if checking if mech_code exits in MER
} #end of for loop overall


CheckFSD <- FSD %>%  filter(implementation_year==2024 & program=="HTS" &  targeted_beneficiary=="Non-Targeted Populations" & mech_code==70108) %>% 
  select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account,
         targeted_beneficiary, allocated_beneficiary, cop_budget_total)

cleancalc <- calc %>% select(operatingunit, mech_code, implementation_year, program, sub_program, interaction_type, funding_account, targeted_beneficiary, allocated_beneficiary, calc_budget) %>%
  rename(calc = calc_budget)

delta<- left_join(CheckFSD, cleancalc, by = c("operatingunit", "mech_code", "implementation_year", "program", "sub_program", "interaction_type", "funding_account", "targeted_beneficiary", "allocated_beneficiary")) %>% 
  mutate(delta = cop_budget_total - calc)




