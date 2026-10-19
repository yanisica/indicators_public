# clean you environment
rm(list=ls())
graphics.off()

############################################
# Plot all indicators (MEAs and assessments)
############################################
# Created by Yanina Sica in April 2025


### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_where_code_is" # complete accordingly

## Source useful functions from folder downloaded from GitHub
source(paste0(your_dir,"/useful_funcs_indic.R"))
source(paste0(your_dir,"/settings.R"))

## Set working directory and install required libraries
settings()

## Read installed libraries
library(tidyverse)
library(gtools)
#library(dplyr)
#library(tidyr)
#library(readr)
library(data.table)
#library(purrr)
#library(googlesheets4)

#library(ggplot2)
library(circlize)
library(ggalluvial)
library(ggpattern)
library(patchwork)
library(reshape2)

# Load indicators----

all_indicators_harm_cl = read_csv('../output/all_indicators_harmonized_classified_15102026.csv')

# long format with duplication
indic = read_csv('../output/all_indicators_harmonized_classified_15102026_lv.csv')

# Compute number of distinct sources per indicator ONCE, across the full
indic_nsources <- indic |>
  group_by(indicator_harmonized) |>
  mutate(n_sources = n_distinct(source)) |>
  ungroup()

# Define a color scheme for the different sources
grid_color <- c(
  #Assess
  IPBES = "#BAE4B3", IPCC = "#74C476", GEO = "#31A354",
  #MEAs
  GBF = "#DEEBF7", SDG = "#C6DBEF",RAMSAR = "#9ECAE1",
  CITES = "#6BAED6",CMS = "#4292C6",UNCCD = "#2171B5"
)

# library(RColorBrewer)
# display.brewer.all()
# brewer.pal(n=8,"Blues")
## "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#084594"
# "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5"
# brewer.pal(n=5,"Greens")
## "#EDF8E9" "#BAE4B3" "#74C476" "#31A354" "#006D2C"
# "#BAE4B3" "#74C476" "#31A354"

# Define order
mea_order <- c('GBF','SDG','RAMSAR','CITES','CMS','UNCCD')
assess_order <- c('IPBES', 'IPCC', 'GEO')

# Figure 1: Summaries of indicators by source----

## Reformat data

data_hist_meas  <- indic_nsources |>
  filter(mea == TRUE) |>
  group_by(source) |>
  summarise(
    n_total  = n(),
    n_unique = sum(n_sources == 1),
    .groups  = "drop"
  ) |>
  mutate(
    n_shared = n_total - n_unique,
    equal    = n_total == n_unique,
    source   = factor(source, levels = mea_order)
  ) |>
  pivot_longer(cols = c(n_unique, n_shared),
               names_to = "segment", values_to = "value") |>
  mutate(segment = factor(segment, levels = c("n_unique", "n_shared"))) |>
  rename(sources = source)

data_hist_assess <- indic_nsources |>
  filter(mea == FALSE) |>
  group_by(source) |>
  summarise(
    n_total  = n(),
    n_unique = sum(n_sources == 1),
    .groups  = "drop"
  ) |>
  mutate(
    n_shared = n_total - n_unique,
    equal    = n_total == n_unique,
    source   = factor(source, levels = assess_order)
  ) |>
  pivot_longer(cols = c(n_unique, n_shared),
               names_to = "segment", values_to = "value") |>
  mutate(segment = factor(segment, levels = c("n_unique", "n_shared"))) |>
  rename(sources = source)
  
### Plot
hist_meas = ggplot(data_hist_meas,
                   aes(x = sources, y = value, fill = sources, pattern = segment)) +
  geom_bar_pattern(
    stat = "identity", color = "black", position = "stack",
    pattern_colour  = "white",
    pattern_fill    = "white",
    pattern_density = 0.35,
    pattern_spacing = 0.03,
    pattern_angle   = 45
  ) +
  scale_fill_manual(values = grid_color, limits = mea_order, guide = "none") +
  scale_pattern_manual(
    values = c(n_unique = "stripe", n_shared = "none"),
    labels = c("Unique metrics", "Shared with other sources"), 
    name = "") +
  scale_x_discrete(limits = mea_order) +
  scale_y_continuous(limits = c(0, 320), expand = expansion(mult = c(0, .1))) +
  # n_total label above each bar
  geom_text(
    data = ~filter(.x, segment == "n_shared"),
    aes(x = sources, y = n_total, label = n_total),
    inherit.aes = FALSE, vjust = -0.5, size = 2.8, color = "black") +
  # n_unique label inside bottom segment (only when < n_total)
  geom_text(
    data = ~filter(.x, segment == "n_unique", !equal),
    aes(x = sources, y = n_total - value / 2, label = value),
    inherit.aes = FALSE, size = 2.5, color = "black") +
  labs(y = "Number of metrics", x = "", tag = "A") +
  theme_minimal() +
  theme(
    legend.position   = 'none', #c(0.75, 0.88),
    #legend.background = element_rect(fill = "white", color = NA),
    #legend.key.size   = unit(1.3, "lines"),
    #legend.text       = element_text(size = 8),
    panel.background  = element_blank(),
    axis.line         = element_line(colour = "black"),
    panel.grid        = element_blank(),
    panel.border      = element_blank(),
    plot.tag          = element_text(),
    axis.title        = element_text(size = 10)
  )

hist_meas

hist_assess = ggplot(data_hist_assess,
                     aes(x = sources, y = value, fill = sources, pattern = segment)) +
  geom_bar_pattern(
    stat = "identity", color = "black", position = "stack",
    pattern_colour  = "white",
    pattern_fill    = "white",
    pattern_density = 0.35,
    pattern_spacing = 0.03,
    pattern_angle   = 45
  ) +
  scale_fill_manual(values = grid_color, limits = assess_order, guide = "none") +
  scale_pattern_manual(
    values = c(n_unique = "stripe", n_shared = "none"),
    labels = c("Unique metrics", "Shared with other sources"), name = "") +
  scale_x_discrete(limits = assess_order) +
  scale_y_continuous(limits = c(0, 820), expand = expansion(mult = c(0, .1))) +
  # n_total label above each bar
  geom_text(
    data = ~filter(.x, segment == "n_shared"),
    aes(x = sources, y = n_total, label = n_total),
    inherit.aes = FALSE, vjust = -0.5, size = 2.8, color = "black") +
  # n_unique label inside bottom segment (only when < n_total)
  geom_text(
    data = ~filter(.x, segment == "n_unique", !equal),
    aes(x = sources, y = n_total - value / 2, label = value),
    inherit.aes = FALSE, size = 2.5, color = "black") +
  labs(y = "Number of metrics", x = "", tag = "B") +
  theme_minimal() +
  theme(
    legend.position   = c(0.75, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size   = unit(1.2, "lines"),
    legend.text       = element_text(size = 8),
    panel.background  = element_blank(),
    axis.line         = element_line(colour = "black"),
    panel.grid        = element_blank(),
    panel.border      = element_blank(),
    plot.tag          = element_text(),
    axis.title        = element_text(size = 10)
  )

hist_assess

fig1 = hist_meas + hist_assess
fig1

# Save figures
ggsave(file="../output/figures/fig1_oct26.svg", plot=fig1,
       width=7, height=3.5, units = "in", dpi = 300)
ggsave(file="../output/figures/fig1_oct26.png", plot=fig1,
       width=7, height=3.5, units = "in", dpi = 300)

# Figure 2: shared indicators (chord diagram) ----

entity_order <- c("IPBES", "IPCC", "GEO", "UNCCD", "GBF", "SDG", "RAMSAR", "CITES", "CMS")

# Colours: assessments in greens, MEAs in blues (as in the current figure)
grid_color <- c(IPBES = "#C7E9C0", IPCC = "#74C476", GEO = "#31A354",
                GBF = "#DEEBF7", SDG = "#C6DBEF", RAMSAR = "#9ECAE1",
                CITES = "#6BAED6", CMS = "#4292C6", UNCCD = "#2171B5")

## 1. Edge list: number of metrics shared by each pair of sources
long <- all_indicators_harm_cl %>%
  dplyr::select(indicator_harmonized, source) %>%
  tidyr::separate_rows(source, sep = ";") %>%
  dplyr::mutate(source = trimws(source)) %>%
  dplyr::distinct()

stopifnot(all(long$source %in% entity_order))   # no unexpected source names

edges <- long %>%
  dplyr::inner_join(long, by = "indicator_harmonized",
                    relationship = "many-to-many") %>%
  dplyr::filter(match(source.x, entity_order) < match(source.y, entity_order)) %>%
  dplyr::count(from = source.x, to = source.y, name = "value") %>%
  dplyr::arrange(dplyr::desc(value))

## 2. Sanity check against an independent count

# metrics whose source string contains both GBF and SDG
n_gbf_sdg <- sum(grepl("GBF", all_indicators_harm_cl$source) &
                   grepl("SDG", all_indicators_harm_cl$source))
stopifnot(edges$value[edges$from == "GBF" & edges$to == "SDG"] == n_gbf_sdg)
print(edges)   # current data: IPBES-SDG 143, GBF-IPBES 93, GBF-SDG 56, ...

## 3. Plot data: only sectors that share something

# Set > 0 (e.g. 7) to draw small links thicker; if you do, say so in the caption
min_link <- 5

plot_edges <- edges %>%
  dplyr::mutate(value = value + min_link)

present     <- entity_order[entity_order %in% c(plot_edges$from, plot_edges$to)]
not_shared  <- setdiff(entity_order, present)
message("Not in the diagram (no shared metrics): ", paste(not_shared, collapse = ", "))

## 4. Plot 
png(filename = "../output/figures/fig2_19102026.png",
    width = 4, height = 4, units = "in", bg = "white", res = 300)

circos.clear()
circos.par(start.degree = 165, gap.after = 5,
           track.margin = c(0.01, 0.01), cell.padding = c(0, 0, 0, 0))

chordDiagram(
  as.data.frame(plot_edges),
  order             = present,
  grid.col          = grid_color[present],
  transparency      = 0.1,
  annotationTrack   = "grid",
  preAllocateTracks = list(track.height = 0.1)
)

circos.trackPlotRegion(
  
  track.index = 1, bg.border = NA,
  panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                facing = "inside", niceFacing = TRUE, adj = c(0.5, 0), cex = 0.8)
  }
)

circos.clear()
dev.off()


# Matrix of shared indicators-----
library(dplyr)
library(tidyr)
library(stringr)

sources_ordered <- c("IPBES", "GEO", "IPCC",
                     "GBF", "SDG", "CITES", "CMS", "RAMSAR", "UNCCD")

## One row per (indicator, source) pair
long <- all_indicators_harm_cl |>
  select(indicator_harmonized, source) |>
  separate_longer_delim(source, delim = ";") |>
  mutate(source = str_trim(source)) |>
  filter(source %in% sources_ordered) |>
  distinct()                       # guards against duplicate rows per indicator

## Presence/absence matrix: rows = indicators, columns = sources (0/1)
presence <- table(
  factor(long$indicator_harmonized),
  factor(long$source, levels = sources_ordered)
)
presence <- unclass(presence)      # plain matrix

## Co-occurrence: off-diagonal = number of indicators shared by two sources
co_occurrence_matrix <- crossprod(presence)

## Optional: zero the diagonal so it holds only shared counts
## (otherwise the diagonal is each source's total number of metrics)
co_shared <- co_occurrence_matrix
diag(co_shared) <- 0

co_occurrence_matrix

# Figure 3: Summaries of indicators by categories----

indic_categories = indic %>%   
  # summary cat + source
  group_by(Categories,source)  %>% 
  count() %>% arrange(desc(n)) %>% 
  # improve viz
  mutate(Categories = gsub('Human well-being', 'Human\n well-being', Categories)) %>% 
  mutate(Categories = gsub('Knowledge systems', 'Knowledge\n systems', Categories)) %>% 
  mutate(Categories = gsub('Ecosystem services', 'Ecosystem\n services', Categories)) %>% 
  mutate(Categories = gsub('Direct drivers', 'Direct\n drivers', Categories)) %>% 
  mutate(Categories = gsub('Human assets', 'Human\n assets', Categories)) %>% 
  mutate(source = factor(source, 
                                levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD'))) %>% 
  mutate(Categories = factor(Categories, 
                         levels=c('Biodiversity','Ecosystems','Ecosystem\n services','Human\n well-being',
                                  'Direct\n drivers','Human\n assets','Knowledge\n systems','Governance'))) %>% 
  # improve viz for figure
  mutate(n = if_else(source %in% c('CITES','RAMSAR','CMS','UNCCD'),
                     true = n + 8,false = n))

# Plot
fig3 = ggplot(indic_categories,
       aes(y = n,
           axis1 = source, axis2 = Categories)) +
  geom_alluvium(aes(fill = Categories),
                width = 1/6, knot.pos = 0.2, reverse = FALSE) +
  scale_fill_manual(values =c('Governance'="#440154",'Knowledge\n systems'= "#46327e",
                              'Human\n assets'="#365c8d",'Direct\n drivers'="#277f8e",
                              'Human\n well-being'="#1fa187",'Ecosystem\n services'="#4ac16d" ,
                              'Ecosystems'="#a0da39",'Biodiversity'="#fde725")) +
  #scale_x_discrete(limits=c("IPBES","GEO","IPCC","GBF","SDG","CITES","CMS","RAMSAR","UNCCD")) +
  guides(fill = "none") +
  geom_stratum(alpha = 0, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),reverse = FALSE, size = 3) +
  #scale_x_continuous(breaks = 1:2, labels = c("Sources", "Indicator category")) +
  theme_void()

fig3

ggsave(file="../output/figures/fig3_19102026.svg", plot=fig3, 
       width=7, height=8, units = "in", dpi = 300)
ggsave(file="../output/figures/fig3_191020265.png", plot=fig3, 
       width=7, height=8, units = "in", dpi = 300)









