# clean you environment
rm(list=ls())

############################################################################
# Integrate indicators extracted from global assessments (IPBES, IPCC, GEO) 
# & and other MEA (CBD, SDGs)
############################################################################
# Created by Yanina Sica in January 2023
# Updated May 2025

### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_where_code_is" # complete accordingly

## Source useful functions from folder downloaded from GitHub
source(paste0(your_dir,"/useful_funcs_indic.R"))
source(paste0(your_dir,"/harmonize_indic_func.R"))
source(paste0(your_dir,"/settings.R"))

## Set working directory and install required libraries
settings()

## Read installed libraries

library(stringr)
#library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(googlesheets4)
#library(purrr)

# 1-Append all indicators to classify-----

# Indicators and other metrics extracted using `auto_search_indic.R`
assess_extracted = read_csv('../input/assessments/automated_search/assess_ext_indicators_orig.csv')

# Indicators used in tables in supplementary material in assessments extracted using `tables_extract.R`
assess_tables = read_csv('../input/assessments/tables_extraction/ipcc_ipbes_supp_indicators_orig.csv')
  
# Indicators used in MEAs extracted using `meas_indic_extract.R`
meas = read_csv('../input/meas/meas_indicators_orig.csv') 
  
# Join all indicators and other metrics together (assessments + MEAs)
assess_indicators = rbind(assess_extracted, assess_tables) %>% 
  mutate(policy = 0)

all_indicators = meas %>% 
  mutate(policy = 1) %>% 
  rbind(assess_indicators) %>% 
  # add source
  mutate(source = word(indic_id, 1, sep = '_')) %>% 
  # drop previous harmonization (did not work)
  dplyr::select(-indicator_harmonized)
  
# checks
all_indicators %>% count() #2647
all_indicators %>% group_by(policy) %>% count()
#   policy     n
# 1      0  1841
# 2      1   806
all_indicators %>% group_by(source) %>% count()
#   source     n
# 1 CITES     53
# 2 CMS       26
# 3 GBF      385
# 4 GEO      248
# 5 IPBES   1121
# 6 IPCC     472
# 7 RAMSAR    70
# 8 SDG      258
# 9 UNCCD     14

write_csv(all_indicators, '../input/all_indicators_orig.csv')

# 2-Harmonize indicators----

# Harmonize: adds the column field_harm, one row per extracted name
harm <- harmonize_indic(all_indicators, indicator_orig)

# Collapse: one row per harmonized indicator
all_indicators_harm <- collapse_indic(harm) %>% 
  rename(indicator_harmonized = field_harm)

# checks
all_indicators_harm %>% count() #1691
all_indicators_harm %>% group_by(policy) %>% count()
# policy     n
#      0  1061
#      1   630
all_indicators_harm %>% group_by(source) %>% count()


# 3-Classify indicators----
# This was manual process that happened in multiple iterations (based on the April24, May24 and May25 versions, oct 26)
# Intermediate version were created merging with previously classified indicators
# Latest version was produced based on a few fixes suggested by Claude after second harmonization was implemented:
# manual_classification_15102026.csv

#Intermediate version were created merging with previously classified indicators
classified = read_csv("../output/manual_classification_15102026.csv")
# checks
classified %>% count() #1691
classified %>% filter(is.na(Categories)) %>% count() 
classified %>% filter(is.na(Subcategories)) %>% count() 

# Join classified harmonized indicators with sources and ids 
all_indicators_harm_cl = left_join(all_indicators_harm, classified, by = 'indicator_harmonized')

# checks
## missing indic
anti_join(classified, all_indicators_harm_cl, by = 'indicator_harmonized')#423 from old extraction were classified (no need to add them)
anti_join(all_indicators_harm_cl,classified, by = 'indicator_harmonized')#0 indicator from last extraction were not classified
## duplicates
check_dup(all_indicators_harm_cl,indicator_harmonized)
## missing categories
all_indicators_harm_cl %>% count() #1691
all_indicators_harm_cl %>% group_by(policy) %>% count()
# policy     n
#      0  1061
#      1   630
all_indicators_harm_cl %>% group_by(source) %>% count() %>% View()

# Save
write_csv(all_indicators_harm_cl,'../output/all_indicators_harmonized_classified_15102026.csv')

# re-format table (long table with repetitions)
mea_sources <- c("GBF", "SDG", "CITES", "CMS", "RAMSAR", "UNCCD") 
assessment_sources <- c("IPBES", "IPCC", "GEO")

indic = all_indicators_harm_cl %>% 
  dplyr::mutate(indic_id = strsplit(as.character(indic_id), ";")) %>% 
  unnest(indic_id) %>%
  dplyr::mutate(source = sub("_.*", "", indic_id),
                policy = ifelse(source %in% mea_sources, 1, 0)) %>% 
  dplyr::select(-"n_sources")
names(indic)


indic <- all_indicators_harm_cl %>%
  # 1. one row per extracted indicator (ID); the source is the ID prefix
  tidyr::separate_rows(indic_id, sep = ";") %>%
  dplyr::mutate(indic_id = trimws(indic_id),
                source   = sub("_.*", "", indic_id)) %>%
  # 2. one row per indicator x source
  dplyr::group_by(indicator_harmonized, source) %>%
  dplyr::summarise(
    indic_ids = paste(indic_id, collapse = ";"),        # this source's IDs only
    dplyr::across(c(Categories, Categories_2, Subcategories, Subcategories_2),
                  dplyr::first),
    .groups = "drop") %>%
  # 3. policy flags from each row's own source
  dplyr::mutate(mea = ifelse(source %in% mea_sources, 1, 0),
                assess = ifelse(source %in% assessment_sources, 1, 0)) %>%
  dplyr::select(indicator_harmonized, indic_ids, source,
                Categories, Categories_2, Subcategories, Subcategories_2,
                mea, assess)

write_csv(indic,'../output/all_indicators_harmonized_classified_15102026_lv.csv')

# 4-Summaries----

all_indicators_harm_cl %>% count() #1691 unique indicators
all_indicators_harm_cl %>%  distinct(indicator_harmonized) %>% count() # all unique
all_indicators_harm_cl %>% filter(!is.na(Categories)) %>% distinct(indicator_harmonized) %>% count() # all classified


all = indic %>% count() #2098 indicators --> 1963
unique = indic %>%  distinct(indicator_harmonized) %>% count() # 1829 unique --> 1646
cat('all indicators: ',all$n, '\nunique indicators: ',unique$n)
# all indicators:  1996 
# unique indicators:  1691

# 5.a-Summaries of indicators by source----

# MEAs vs asses
all_mea = indic %>% filter(mea==TRUE) %>% count() %>% arrange(desc(n))
all_assess = indic %>% filter(assess==TRUE) %>% count() %>% arrange(desc(n))

unique_mea = indic %>% filter(mea == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() 
unique_assess = indic %>% filter(assess == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() 

cat('\nAssess indicators: ',all_assess$n, '\nMea indicators: ',all_mea$n)
cat('\nUnique assess indicators: ',unique_assess$n, '\nUnique mea indicators: ',unique_mea$n)
# Assess indicators:  1307 
# Mea indicators:  689
# Unique assess indicators:  1258 
# Unique mea indicators:  630

# sources (extracted with duplicates)
total_by_source = indic %>% group_by(source) %>% count() %>% arrange(desc(n))
# source     n
# 1 IPBES    797
# 2 IPCC     376
# 3 GBF      311
# 4 SDG      240
# 5 GEO      219
# 6 RAMSAR    65
# 7 CITES     52
# 8 CMS       25
# 9 UNCCD     13


# 5.b-Summaries of indicators by category-----

all_indicators_cl %>% distinct(Categories) %>% count() #8

(all_indicators_cl %>% filter(!is.na(Categories_2)) %>% count())/ #409
  (all_indicators_cl %>% filter(!is.na(Categories)) %>% count())  #1829
  
all_indicators_cl %>% 
  group_by(Categories) %>% count() %>% mutate(prop = n/unique$n) %>% arrange(desc(n))
# Categories             n   prop
# 1 ecosystems           527 0.286 
# 2 Governance           259 0.141 
# 3 Direct drivers       223 0.121 
# 4 Knowledge systems    209 0.113 
# 5 biodiversity         192 0.104 
# 6 Human assets         170 0.0923
# 7 Human well-being     140 0.0760
# 8 Ecosystem services   122 0.0662

indic %>% 
  group_by(source,Categories) %>%
  summarize(n = n()) %>% 
  count() %>% arrange(desc(n))
# 1 GBF        8
# 2 GEO        8
# 3 IPBES      8
# 4 IPCC       8
# 5 SDG        8
# 6 RAMSAR     7
# 7 CMS        5
# 8 UNCCD      5
# 9 CITES      4

indic %>% 
  group_by(source,Categories) %>% count() %>% View()

# Subcategories
all_indicators_cl %>% distinct(Subcategories) %>%  count() # 45

all_indicators_cl %>%   
  # summary cat + source
  group_by(Subcategories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  mutate(prop = (n / unique$n)*100) %>% View()

  
#### Tables for Supplementary material A----
indic_categories = all_indicators_cl %>% 
  dplyr::group_by(Categories) %>% 
  dplyr::count() %>% arrange(desc(n)) %>% 
  dplyr::mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  dplyr::mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::mutate(`Proportion of metrics` = (n/unique$n)*100) %>% 
  dplyr::mutate(Elements = factor(Categories, 
                                  levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                           'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  dplyr::arrange(Elements) %>%
  dplyr::select(Elements,`Number of metrics`=n,`Proportion of metrics`) %>% 
  write_csv('../output/sup_material/indic_categories_190525.csv')

indic_categories_bysource = indic %>%   
  # summary cat + source
  dplyr::group_by(source, Categories)  %>% 
  dplyr::count() %>% arrange(desc(n)) %>% 
  # improve viz
  dplyr::mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  dplyr::mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::select("Elements" = Categories, Source = source, 'Counts of metrics' = n) %>% 
  dplyr::mutate(Elements = factor(Elements, 
                             levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                      'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  arrange(Elements) %>%
  dplyr::mutate(Source = factor(Source, 
                         levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD'))) %>%
  arrange(Source) %>% 
  # set wide format
  tidyr::pivot_wider(names_from = Source, values_from = `Counts of metrics`) %>% 
  write_csv('../output/sup_material/indic_categories_bysource_190525.csv')

indic_subcategories_bysource = indic %>%   
  # summary cat + source
  group_by(source, Categories, Subcategories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  # improve viz
  mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::select("Elements" = Categories, "Subelements" = Subcategories, Source = source, 'Counts of metrics' = n) %>% 
  mutate(Elements = factor(Elements, 
                             levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                      'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  arrange(Elements) %>% 
  dplyr::mutate(Source = factor(Source, 
                                levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD'))) %>%
  arrange(Source) %>% 
  # set wide format
  tidyr::pivot_wider(names_from = Source, values_from = `Counts of metrics`) %>% 
  write_csv('../output/sup_material/indic_subcategories_bysource_190525.csv')


