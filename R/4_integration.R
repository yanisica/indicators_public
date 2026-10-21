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
all_indicators_harm %>% count() #1688
all_indicators_harm %>% group_by(policy) %>% count()
# policy     n
#      0  1057
#      1   631
all_indicators_harm %>% group_by(source) %>% count()


# 3-Classify indicators----
# This was manual process that happened in multiple iterations (based on the April24, May24 and May25 versions, oct 26)
# Intermediate version were created merging with previously classified indicators
# Latest version was produced based on a few fixes suggested by Claude after second harmonization was implemented:
# manual_classification_15102026.csv

#Intermediate version were created merging with previously classified indicators
classified = read_csv("../output/manual_classification_20102026.csv") #%>% 
  # # re-encode only the strings that aren't valid UTF-8
  # dplyr::mutate(dplyr::across(dplyr::everything(),
  #                             ~ ifelse(validUTF8(.x), .x, iconv(.x, from = "latin1", to = "UTF-8")))) %>%
  # mutate(indicator_harmonized = indicator_harmonized %>%
  #          gsub("^hallow coral reefs$", "shallow coral reefs", .) %>%
  #          gsub("agricultural outpic prices", "agricultural output prices", ., fixed = TRUE) %>%
  #          gsub("^water- saving devices$", "water-saving devices", .) %>%
  #          gsub("highest interim target of 35 .*g/m3?$", "highest interim target of 35 μg/m3", .) %>%
  #          gsub("reported in national reports$", "reported in national reports)", .) %>%
  #          gsub("^no emissions$", "nitric oxide (no) emissions", .) %>%
  #          gsub("^c pool$", "carbon (c) pool", .) %>%
  #          gsub("^sh monsoons$", "southern hemisphere (sh) monsoons", .) %>%
  #          gsub("nature(´|’|Â´|\\?)s", "nature's", .)) %>% 
  #          gsub("1[-]in[-]100[-]year return period level [(]etwl[)]", "1-in-100-year extreme total water level (etwl)", .) %>%
  #          gsub("global atmospheric co levels", "global atmospheric carbon monoxide (co) levels", .) %>%
  #          gsub("number of studies on ias available per region", "number of studies on invasive alien species (ias) available per region", .) %>%
  #          gsub("penman[-]monteith aed [(]atmospheric evaporative demand[)]", "penman-monteith atmospheric evaporative demand (aed)", .) %>%
  #          gsub("pm 10 emissions", "pm10 emissions", .) %>%
  #          gsub("simple precipitation intensity index [(]sdii[)]", "simple daily intensity index (sdii)", .)) %>%
  #          write_csv("../output/manual_classification_20102026.csv")

# checks
classified %>% count() #1688
classified %>% filter(is.na(Categories)) %>% count() 
classified %>% filter(is.na(Subcategories)) %>% count() 

# Join classified harmonized indicators with sources and ids 
all_indicators_harm_cl = left_join(all_indicators_harm, classified, by = 'indicator_harmonized')

# checks
## missing indic
anti_join(classified, all_indicators_harm_cl, by = 'indicator_harmonized')
anti_join(all_indicators_harm_cl,classified, by = 'indicator_harmonized')
## duplicates
check_dup(all_indicators_harm_cl,indicator_harmonized)
## missing categories
all_indicators_harm_cl %>% count() #1688
all_indicators_harm_cl %>% group_by(policy) %>% count()
# policy     n
#      0  1057
#      1   631
all_indicators_harm_cl %>% group_by(source) %>% count()

# Save
write_csv(all_indicators_harm_cl,'../output/all_indicators_harmonized_classified_20102026.csv')

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

write_csv(indic,'../output/all_indicators_harmonized_classified_20102026_lv.csv')

# 4-Summaries----

#all_indicators_harm_cl %>% count() #1688 unique indicators
#all_indicators_harm_cl %>%  distinct(indicator_harmonized) %>% count() # all unique
#all_indicators_harm_cl %>% filter(!is.na(Categories)) %>% distinct(indicator_harmonized) %>% count() # all classified

all = indic %>% count() 
unique = indic %>%  distinct(indicator_harmonized) %>% count() 
cat('all indicators: ',all$n, '\nunique indicators: ',unique$n)
# all indicators:  1995 
# unique indicators:  1688

# 4.a-Summaries of indicators re-use----

indic_reuse <- indic %>%
  dplyr::add_count(indicator_harmonized, name = "n_sources")   # sources per metric

# Overall re-use (one row per metric)
reuse_overall <- indic_reuse %>%
  dplyr::distinct(indicator_harmonized, n_sources) %>%
  dplyr::summarise(n_total  = dplyr::n(),
                   n_reused = sum(n_sources > 1),
                   pct      = round(100 * n_reused / n_total, 1))
# n_total n_reused   pct
#    1688      234  13.9

all_indicators_harm_cl %>% filter(n_sources == 4) %>% distinct(indicator_harmonized, source) %>% count()# 11
all_indicators_harm_cl %>% filter(n_sources == 4) %>% distinct(indicator_harmonized, source) %>% View()

all_indicators_harm_cl %>% filter(n_sources == 3) %>% distinct(indicator_harmonized, source) %>% count() #51
all_indicators_harm_cl %>% filter(n_sources == 3) %>% distinct(indicator_harmonized, source) %>% group_by(source) %>% count() #51
#all_indicators_harm_cl %>% filter(n_sources == 3) %>% distinct(indicator_harmonized, source) %>% View()

all_indicators_harm_cl %>% filter(n_sources == 2) %>% distinct(indicator_harmonized, source) %>% count() #172
all_indicators_harm_cl %>% filter(n_sources == 2) %>% distinct(indicator_harmonized, source) %>% group_by(source) %>% count() #51
#all_indicators_harm_cl %>% filter(n_sources == 2) %>% distinct(indicator_harmonized, source) %>% View()

# MEAs vs assessments (one row per metric) 
reuse_realm <- indic_reuse %>%
  dplyr::group_by(indicator_harmonized) %>%
  dplyr::summarise(in_mea = any(mea == 1), in_ass = any(assess == 1),
                   n_sources = dplyr::first(n_sources), .groups = "drop") %>%
  dplyr::summarise(n_mea             = sum(in_mea),
                   n_assessment      = sum(in_ass),
                   shared_mea_ass    = sum(in_mea & in_ass),
                   shared_among_meas = sum(in_mea & !in_ass & n_sources > 1),
                   shared_among_ass  = sum(in_ass & !in_mea & n_sources > 1))
# n_mea n_assessment shared_mea_ass shared_among_meas shared_among_ass
#   631         1256            199                 8               27

# Per source: total, unique, shared, % shared
reuse_source <- indic_reuse %>%
  dplyr::group_by(source) %>%
  dplyr::summarise(n_total    = dplyr::n(),
                   n_unique   = sum(n_sources == 1),
                   n_shared   = sum(n_sources > 1),
                   pct_shared = round(100 * n_shared / n_total, 1),
                   .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(n_total))
# source n_total n_unique n_shared pct_shared
# 1 IPBES      749      537      212       28.3
# 2 IPCC       349      334       15        4.3
# 3 GBF        297      191      106       35.7
# 4 SDG        240       87      153       63.8
# 5 GEO        207      159       48       23.2
# 6 RAMSAR      64       64        0        0  
# 7 CITES       52       47        5        9.6
# 8 CMS         24       24        0        0  
# 9 UNCCD       13       11        2       15.4

# Pairwise shared metrics
reuse_pairs <- indic_reuse %>%
  dplyr::filter(n_sources > 1) %>%
  dplyr::select(indicator_harmonized, source) %>%
  dplyr::inner_join(., ., by = "indicator_harmonized", relationship = "many-to-many") %>%
  dplyr::filter(source.x < source.y) %>%
  dplyr::count(source.x, source.y, name = "n_shared") %>%
  dplyr::arrange(dplyr::desc(n_shared))
#    source.x source.y      n_shared
#  1 IPBES    SDG           143
#  2 GBF      IPBES          93
#  3 GBF      SDG            56
#  4 GEO      IPBES          34
#  5 GBF      GEO            21
#  6 GEO      SDG            17
#  7 GEO      IPCC            8
#  8 IPBES    IPCC            8
#  9 CITES    IPBES           4
# 10 GBF      UNCCD           2
# 11 IPBES    UNCCD           2
# 12 SDG      UNCCD           2
# 13 CITES    GBF             1


# 4.b-Summaries of indicators by category-----

all_indicators_harm_cl %>% distinct(Categories) %>% count() #8

all_indicators_harm_cl %>% 
  group_by(Categories) %>% count() %>% mutate(prop = n/unique$n) %>% arrange(desc(n))
# 1 Ecosystems           486 0.288 
# 2 Governance           244 0.145 
# 3 Knowledge systems    197 0.117 
# 4 Direct drivers       190 0.113 
# 5 Biodiversity         168 0.0995
# 6 Human assets         157 0.0930
# 7 Human well-being     133 0.0788
# 8 Ecosystem services   113 0.066

(all_indicators_harm_cl %>% filter(!is.na(Categories_2)) %>% count())/ #462
  (all_indicators_harm_cl %>% filter(!is.na(Categories)) %>% count())  #1688
# 1 0.2736967

indic %>% 
  group_by(source,Categories) %>%
  summarize(n = n()) %>% 
  count() %>% arrange(desc(n))
# 1 GBF        8
# 2 GEO        8
# 3 IPBES      8
# 4 IPCC       8
# 5 SDG        8
# 6 RAMSAR     6
# 7 CMS        5
# 8 UNCCD      5
# 9 CITES      4

indic %>% 
  group_by(source,Categories) %>% count() %>% View()

# Subcategories
all_indicators_harm_cl %>% distinct(Subcategories) %>%  count() # 45

all_indicators_harm_cl %>%   
  # summary cat + source
  group_by(Subcategories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  mutate(prop = (n / unique$n)*100) %>% View()

  
# 5-Tables for Supplementary material A----
indic_categories = all_indicators_harm_cl %>% 
  dplyr::group_by(Categories) %>% 
  dplyr::count() %>% arrange(desc(n)) %>% 
  dplyr::mutate(`Proportion of metrics` = (n/unique$n)*100) %>% 
  dplyr::mutate(Elements = factor(Categories, 
                                  levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                           'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  dplyr::arrange(Elements) %>%
  dplyr::select(Elements,`Number of metrics`=n,`Proportion of metrics`, -Categories) %>% 
  write_csv('../output/sup_material/indic_categories_19102026.csv')

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
  write_csv('../output/sup_material/indic_categories_bysource_19102026.csv')

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
  write_csv('../output/sup_material/indic_subcategories_bysource_19102026.csv')

indic_categories_2 = all_indicators_harm_cl %>% 
  dplyr::filter(!is.na(Categories_2)) %>% 
  dplyr::group_by(Categories_2) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(`Proportion of secondary-tagged metrics (%)` = round(n / sum(n) * 100, 2)) %>% 
  dplyr::mutate(Elements = factor(Categories_2, 
                                  levels = c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                             'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  dplyr::arrange(Elements) %>%
  dplyr::select(Elements, `Number of metrics` = n, `Proportion of secondary-tagged metrics (%)`) %>% 
  write_csv('../output/sup_material/indic_categories_2_19102026.csv')

# 6-Table for Shinny App------
# Frameworks by type (edit here if the set of sources changes)
assess_regex <- "\\b(IPBES|IPCC|GEO)\\b"
mea_regex    <- "\\b(GBF|SDG|RAMSAR|CITES|CMS|UNCCD)\\b"

app_table <- all_indicators_harm_cl %>%
  mutate(
    `MEA/assess` = case_when(
      str_detect(source, assess_regex) & str_detect(source, mea_regex) ~ "both",
      str_detect(source, mea_regex)                                    ~ "mea",
      str_detect(source, assess_regex)                                 ~ "assessment"
    )
  ) %>%
  select(
    `Indicators harmonized`    = indicator_harmonized,
    Sources                    = source,
    Elements                   = Categories,
    Subelements                = Subcategories,
    `Number of sources`        = n_sources,
    `MEA/assess`,
    `Indicators original name` = indicator_orig,
    Elements_2                 = Categories_2,
    Subelements_2              = Subcategories_2
  )
write_csv(app_table, "../output/indicators_prepped_app.csv")
write_csv(app_table, "C:/Users/JLU-SU/Documents/GitHub/indicators-app/EnvironIndicators/indicators_prepped_app.csv")
