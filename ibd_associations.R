library(rio)
library(tidyverse)
library(here)
library(janitor)

ibd <- import("ibd_associations.xlsx")

#clean up names
ibd <- ibd %>% 
  clean_names() 

cd_dx <- c("555.0", "555.1", "555.2", "555.9")
uc_dx <- c( "556.0", "556.1", "556.2", "556.3", "556.4", "556.5",
          "556.6", "556.7", "556.8", "556.9")
misc_gi <- c("V12.70", "V12.79", "V44.2", "V44.3", "V45.72",  "V55.2","V55.3", "V58.65",
             "537.1", "565.1", "560.81",
             "569.41", "569.60", "569.62", 
             "569.82", "569.69", "569.81", "569.2", "569.9", "569.60", "569.49",
             "579.3", "579.2", "579.2", "863.39", "863.30")

# arrange by OR in concept 1
ibd_hi_1 <-ibd %>% arrange(desc(odds_ratio)) %>% 
  filter(!
           ((concept_1 %in% (cd_dx) | concept_1 %in% (uc_dx) | concept_1 %in%(misc_gi))) &
           (concept_2 %in% (cd_dx) | concept_2 %in% (uc_dx) | concept_2 %in%(misc_gi))) %>% 
  select(concept_1, concept_1_description, concept_2, concept_2_description, odds_ratio) %>% 
  filter(odds_ratio >1.5) %>% 
  arrange(concept_1) 

ibd_hi1_grouped <- ibd_hi_1 %>% 
  group_by(concept_1_description) %>% 
  summarize(count = n(), mean_or = round(mean(odds_ratio), 3)) %>% 
  filter(count>3) %>% 
  arrange(desc(mean_or))

# arrange by OR in concept 2
ibd_hi_2 <-ibd %>% arrange(desc(odds_ratio)) %>% 
  filter(!
           ((concept_1 %in% (cd_dx) | concept_1 %in% (uc_dx) | concept_1 %in%(misc_gi))) &
           (concept_2 %in% (cd_dx) | concept_2 %in% (uc_dx) | concept_2 %in%(misc_gi))) %>% 
  select(concept_1, concept_1_description, concept_2, concept_2_description, odds_ratio) %>% 
  filter(odds_ratio >1.5) %>% 
  arrange(concept_2) 

ibd_hi2_grouped <- ibd_hi_2 %>% 
  group_by(concept_2_description) %>% 
  summarize(count = n(), mean_or = round(mean(odds_ratio), 3)) %>% 
  filter(count>3) %>% 
  arrange(desc(mean_or))
