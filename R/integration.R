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
source(paste0(your_dir,"/useful_functions_indic.R"))
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
ipbes_extracted = read_csv('../input/assessments/automated_search/ipbes_extracted_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipbes') %>% 
  dplyr::select(-ipbes_extracted)
ipcc_extracted = read_csv('../input/assessments/automated_search/ipcc_extracted_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipcc') %>% 
  dplyr::select(-ipcc_extracted)
geo_extracted = read_csv('../input/assessments/automated_search/geo_extracted_indicators.csv') %>% 
  dplyr::mutate(sources = 'geo') %>% 
  dplyr::select(-geo_extracted)

# Indicators used in tables in supplementary material in assessments extracted using `tables_extract.R`
ipbes_sup = read_csv('../input/assessments/tables_extraction/ipbes_sup_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipbes') %>% 
  dplyr::select(-ipbes_sup)
ipcc_sup = read_csv('../input/assessments/tables_extraction/ipcc_sup_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipcc') %>% 
  dplyr::select(-ipcc_sup)

# Join all indicators and other metrics used in assessments
assess_metrics = ipbes_extracted %>% 
  rbind(ipcc_extracted,geo_extracted,ipbes_sup,ipcc_sup) %>% 
  # Group_by indicator_harmonized and concatenate indic_ids and sourcces
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_ids, collapse = ";"),
                   sources = paste0(sources, collapse = ";")) %>% 
  dplyr::mutate(sources = toupper(sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPBES[;]IPBES',"IPBES", sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPBES',"IPBES", sources)) %>%
  dplyr::mutate(sources = gsub('IPBES[;]GEO[;]IPBES',"IPBES;GEO", sources)) %>% 
  dplyr::mutate(sources = gsub('GEO[;]IPBES',"IPBES;GEO", sources)) %>% 
  dplyr::mutate(sources = gsub('IPCC[;]IPCC',"IPCC", sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPCC[;]IPBES',"IPBES;IPCC", sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPCC[;]GEO[;]IPBES',"IPBES;IPCC;GEO", sources)) %>% 
  # Save file
  write_csv('../input/assessments/assess_indicators.csv') %>% 
  # Format to merge with policy indicators
  dplyr::mutate(policy = 0) 

# Indicators used in MEAs extracted using `meas_indic_extract.R`
policy_indic = read_csv('../input/meas/policy_indicators.csv') %>% 
  # Concatenate sources in one column
  dplyr::mutate(indic_ids = strsplit(as.character(indic_ids), ";")) %>% 
  tidyr::unnest(indic_ids) %>% 
  dplyr::mutate(source = word(indic_ids,1, sep = '_')) %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(sources = paste0(source, collapse = ";"),indic_ids = paste0(indic_ids, collapse = ";")) %>% 
  dplyr::mutate(sources = gsub('GBF[;]GBF[;]GBF',"GBF", sources)) %>% 
  dplyr::mutate(sources = gsub('GBF[;]GBF',"GBF", sources)) %>%
  dplyr::mutate(sources = gsub('SDG[;]SDG[;]SDG',"SDG", sources)) %>% 
  dplyr::mutate(sources = gsub('SDG[;]SDG',"SDG", sources)) %>% 
  dplyr::mutate(sources = gsub('UNCCD[;]UNCCD',"UNCCD", sources)) %>% 
  dplyr::mutate(sources = gsub('RAMSAR[;]RAMSAR',"RAMSAR", sources)) %>% 
  dplyr::mutate(sources = gsub('CITES[;]CITES',"CITES", sources)) %>% 
  dplyr::mutate(sources = gsub('CMS[;]CMS',"CMS", sources)) %>% 
  dplyr::mutate(sources = gsub('GBF[;]GBF',"GBF", sources)) %>%
  dplyr::mutate(policy = 1)

# Join all indicators and other metrics together (assessments + MEAs)

all_indicators = policy_indic %>% 
  rbind(assess_metrics) %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_ids, collapse = ";"),
                   sources = paste0(sources, collapse = ";"),
                   policy = sum(policy)) 

# checks
all_indicators %>% count() #1829
all_indicators %>% distinct(indicator_harmonized) %>% count() #1829 unique

all_indicators %>% group_by(sources) %>% count()
all_indicators %>% group_by(policy) %>% count()
all_indicators %>% filter(is.na(indic_ids))

#write_csv(all_indicators,'../input/all_indicators_20052025.csv')

# 2-Classify indicators----
# This was manual process that happened in multiple iterations (based on the April24, May24 and May25 versions)
# See May25 version here: https://docs.google.com/spreadsheets/d/1SxRkXLkBcSrQHqKiT3wWrU4XODB7J24MKCdc0tCD2m4/edit?gid=801923522#gid=801923522

#Intermediate version were created merging with previously classified indicators
classified = read_csv("../output/all_indicators_classified_050525_rev14052025.csv") %>%
  # remove identified errors
  filter(error !='remove' | is.na(error)) %>% 
  dplyr::mutate(classif = 'old') %>%
  dplyr::select(indicator_harmonized, Categories,	Categories_2,	Subcategories,	Subcategories_2, classif) %>%
  filter(!is.na(indicator_harmonized)) %>%
  filter(!is.na(Categories))

# Join
all_indicators_cl = left_join(all_indicators, classified, by = 'indicator_harmonized')
all_indicators_cl %>% filter(is.na(Categories)) %>% count()
#all_indicators_cl %>% filter(is.na(Categories)) %>% View()
#all_indicators_to_cl %>% filter(is.na(Categories)) %>% count()

# CHECKS
changes = anti_join(classified, all_indicators, by = 'indicator_harmonized')#423 from old extraction were classified (no need to add them)
changes2 = anti_join(all_indicators,classified, by = 'indicator_harmonized')#0 indicator from last extraction were not classified

all_indicators_cl %>% filter(!is.na(Categories)) %>% count() #1829 classified
all_indicators_cl %>% filter(is.na(Categories)) %>% count() #0 NOT classified yet

# Save
write_csv(dplyr::select(all_indicators_cl, -classif),'../output/all_indicators_classified_20052025.csv')
#all_indicators_cl = read_csv('../output/all_indicators_classified_14052025.csv')

# checks
check_dup(all_indicators_cl,indicator_harmonized)

# re-format table (long table with repetitions)
indic = all_indicators_cl %>% 
  dplyr::mutate(source = strsplit(as.character(sources), ";")) %>% 
  unnest(source) %>%
  # dplyr::mutate(indic_id = strsplit(as.character(indic_ids), ";")) %>% 
  # unnest(indic_id) %>%
  dplyr::mutate(mea = if_else(source %in% c("IPBES","IPCC", "GEO"),
                       true = FALSE,
                       false = TRUE)) %>% 
  dplyr::mutate(assess = if_else(!source %in% c("IPBES","IPCC", "GEO"),
                          true = FALSE,
                          false = TRUE)) %>% 
  dplyr::select("indicator_harmonized", "indic_ids","source",            
               "Categories","Categories_2",        
               "Subcategories","Subcategories_2",             
               "policy","mea","assess")
names(indic)

# Save
write_csv(indic,'../output/all_indicators_classified_20052025_lv.csv')

# Read in
#indic = read_csv('../output/all_indicators_classified_20052025_lv.csv')


# 3-Append all indicators with their original names-----
orig_indic_tables = read_csv('../input/assessments/tables_extraction/ipcc_ipbes_supp_indicators_orig.csv')
orig_indic_meas = read_csv('../input/meas/meas_indicators_orig.csv')
orig_indic_ext = read_csv('../input/assessments/automated_search/assess_ext_indicators_orig.csv')

orig_indic = rbind(orig_indic_tables,orig_indic_meas,orig_indic_ext)
write_csv(orig_indic,'../output/all_orig_indicators.csv')

# 4-Build table for shinny app with verbatim manes and harmonized names----
orig = read_csv('../output/all_orig_indicators.csv')
classified = read_csv('../output/all_indicators_classified_20052025.csv')

assessment_sources <- c("IPBES", "IPCC", "GEO")
mea_sources         <- c("GBF", "SDG", "CITES", "CMS", "RAMSAR", "UNCCD", "ICCWC")

#verbatim_indicator
#Look each id up in all_orig_indicators.csv and collapse the deduplicated original names.
id_to_orig <- setNames(orig$indicator_orig, orig$indic_id)

get_verbatim <- function(ids_string) {
  ids <- trimws(unlist(strsplit(ids_string, ";")))
  ids <- ids[ids != ""]
  origs <- unique(id_to_orig[ids])
  origs <- origs[!is.na(origs)]
  if (length(origs) == 0) return(NA_character_)
  paste(origs, collapse = "; ")
}

classified <- classified %>%
  mutate(verbatim_indicator = vapply(indic_ids, get_verbatim, character(1)))

#Frequency of use (Number of distinct assessments/MEAs the indicator was used in.
get_frequency <- function(sources_string) {
  toks <- trimws(unlist(strsplit(sources_string, ";")))
  toks <- toks[toks != ""]
  length(unique(toks))
}

classified <- classified %>%
  mutate(`Frequency of use` = vapply(sources, get_frequency, integer(1)))

#MEA/assess
get_mea_assess <- function(sources_string) {
  toks <- trimws(unlist(strsplit(sources_string, ";")))
  toks <- toks[toks != ""]
  has_mea    <- any(toks %in% mea_sources)
  has_assess <- any(toks %in% assessment_sources)
  if (has_mea && has_assess) return("both")
  if (has_mea)               return("mea")
  if (has_assess)            return("assessment")
  NA_character_
}

classified <- classified %>%
  mutate(`MEA/assess` = vapply(sources, get_mea_assess, character(1))) 

classified <- classified %>%
  select("Indicators harmonized"="indicator_harmonized","Sources" ="sources",
         "Elements"="Categories","Subelements"="Subcategories",
         "Frequency of use","MEA/assess", "Indicators original name"="verbatim_indicator",
         "Elements_2"="Categories_2","Subelements_2"="Subcategories_2")
names(classified)

# Save prepped table
out_path <- file.path("../output/indicators_prepped_app.csv")

fwrite(classified, out_path)
message("Wrote prepped table (", nrow(classified), " rows) to: ", out_path)



# 5-Summaries----

all_indicators_cl = read_csv('../output/all_indicators_classified_20052025.csv')
indic = read_csv('../output/all_indicators_classified_20052025_lv.csv')

all_indicators_cl %>% count() #1829 unique indicators
all_indicators_cl %>%  distinct(indicator_harmonized) %>% count() # all unique
all_indicators_cl %>% filter(!is.na(Categories)) %>% distinct(indicator_harmonized) %>% count() # all classified


all = indic %>% count() #2098 indicators
unique = indic %>%  distinct(indicator_harmonized) %>% count() # 1829 unique
cat('all indicators: ',all$n, '\nunique indicators: ',unique$n)
# all indicators:  2098 
# unique indicators:  1829

# 5.a-Summaries of indicators by source----

# MEAs vs asses
all_mea = indic %>% filter(mea==TRUE) %>% count() %>% arrange(desc(n))
all_assess = indic %>% filter(assess==TRUE) %>% count() %>% arrange(desc(n))

unique_mea = indic %>% filter(mea == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() 
unique_assess = indic %>% filter(assess == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() 

cat('\nunique assess indicators: ',unique_assess$n, '\nunique mea indicators: ',unique_mea$n)
# unique assess indicators:  1356 
# unique mea indicators:  657

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


