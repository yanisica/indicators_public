# =============================================================================
# harmonize_indicators.R
# One-step harmonisation of indicator names for the indicators paper.
#
# Replaces harmonize_indic1.R (gsub chain) and harmonize_indic2.R (lookup).
# Every decision from those two scripts is kept, EXCEPT where it broke the
# rules below; those were reversed or corrected.
#
# HOW IT WORKS
#   1. clean_indic_names(): typographic clean-up only (lower case, spaces,
#      dashes, apostrophes, "&", dagger, trailing punctuation). It never
#      changes meaning.
#   2. harm_lookup: an exact, whole-name lookup (cleaned original name ->
#      harmonised name). Because it matches whole names, a replacement can
#      never cascade into a longer name (the "extent of extent of ..."
#      problem of gsub chains).
#
# NAMING CONVENTION
#   Harmonised names are written in full, followed by the acronym in
#   brackets whenever any source gives one (e.g. "gross domestic product
#   (gdp)", "soil moisture (sm)"). Units, scope and dataset/model labels in
#   brackets are dropped (e.g. "(tonnes)", "(global)", "(era-5)", "(csar)").
#
# RULES FOR MERGING
#   Merge only names that refer to the SAME metric: spelling, typos,
#   acronyms, units in brackets, "percentage" vs "proportion", number vs
#   proportion of countries, normalised forms of a metric (per capita, per
#   unit area, e.g. "gross domestic product" and "... per capita"),
#   "trends in X" vs "X", reworded versions of the same official indicator.
#   Keep SEPARATE:
#   a) a variable or concept and an indicator/index proposed to measure it
#      (e.g. "food waste" vs "food waste index", "sea level" vs "global mean
#      sea level (gmsl)", "el nino-southern oscillation (enso)" vs its index);
#   b) different metrics: change vs level (incl. rates of change, e.g.
#      "land use change" vs "rate of land use change"), loss vs stock,
#      cumulative vs annual, sector or subset vs total, a different unit of
#      count (Parties vs sites).
#   A name that is not in harm_lookup keeps its (cleaned) original name.
#
# Built and checked against all_indicators_orig.csv (2,647 extracted names).
# =============================================================================

#' Typographic clean-up of indicator names (no change of meaning)
clean_indic_names <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[ \t\r\n]", " ", x)             # odd spaces
  x <- gsub("[‐‑‒–—]", "-", x)               # dashes
  x <- gsub("[‘’]", "'", x)                  # curly apostrophes
  x <- gsub("†", "", x, fixed = TRUE)        # dagger
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("\\(\\s+", "(", x)
  x <- gsub("\\s+\\)", ")", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- gsub("[[:space:].,;:]+$", "", x)      # trailing punctuation
  x <- gsub("[‘’´]", "'", x)                 # curly apostrophes and acute accent
  x
}

# -----------------------------------------------------------------------------
# Lookup: cleaned original name = harmonised name
# Grouped by harmonised name ("# -> ...").
# -----------------------------------------------------------------------------
harm_lookup <- c(
  # -> above-ground biomass
  "aboveground biomass" =
    "above-ground biomass",
  # -> above-ground biomass in forests
  "above-ground biomass stock in forest (tonnes/ha)" =
    "above-ground biomass in forests",
  "aboveground biomass in forests" =
    "above-ground biomass in forests",
  # -> agriculture orientation index for government expenditures
  "the agriculture orientation index for government expenditures" =
    "agriculture orientation index for government expenditures",
  # -> air pollution emissions account
  "air emission accounts" =
    "air pollution emissions account",
  # -> alternative and nuclear energy (percentage of total energy use)
  "alternative and nuclear energy (% of total energy use)" =
    "alternative and nuclear energy (percentage of total energy use)",
  # -> amount of fossil-fuel subsidies per unit of gross domestic product (production and consump
  "amount of fossil-fuel subsidies (production and consumption) per unit of gdp" =
    "amount of fossil-fuel subsidies per unit of gross domestic product (production and consumption)",
  "amount of fossil-fuel subsidies per unit of gdp (production and consumption)" =
    "amount of fossil-fuel subsidies per unit of gross domestic product (production and consumption)",
  # -> annual global surface air temperature (gsat) change
  "annual gsat change" =
    "annual global surface air temperature (gsat) change",
  # -> annual growth rate of real gross domestic product
  "annual growth rate of real gdp per capita" =
    "annual growth rate of real gross domestic product",
  # -> annual growth rate of real gross domestic product per employed person
  "annual growth rate of real gdp per employed person" =
    "annual growth rate of real gross domestic product per employed person",
  # -> annual mean levels of fine particulate matter in cities
  "annual mean levels of fine particulate matter (e.g. pm2.5 and pm10) in cities" =
    "annual mean levels of fine particulate matter in cities",
  "annual mean levels of fine particulate matter (e.g. pm2.5 and pm10) in cities (population weighted)" =
    "annual mean levels of fine particulate matter in cities",
  # -> annual total precipitation when the daily precipitation exceeds the 95th percentile of the
  "annual precipitation from daily precipitation > 95th percentile (r95p)" =
    "annual total precipitation when the daily precipitation exceeds the 95th percentile of the wet-day (>1 mm) precipitation (r95p)",
  "annual total precipitation when the daily precipitation exceeds the 95th percentile of the wet-day (>1 mm) precipitation" =
    "annual total precipitation when the daily precipitation exceeds the 95th percentile of the wet-day (>1 mm) precipitation (r95p)",
  # -> arctic sea ice area (sia)
  "arctic sea ice area" =
    "arctic sea ice area (sia)",
  "mean arctic sea ice area" =
    "arctic sea ice area (sia)",
  # -> area of agricultural land under conservation
  "area of agricultural land under conservation agriculture (thousand ha)" =
    "area of agricultural land under conservation",
  "areas of agricultural land under convervation agriculture" =
    "area of agricultural land under conservation",
  # -> area of agricultural land under organic production
  "agricultural organic area" =
    "area of agricultural land under organic production",
  "area of agricultural land under organic production (million ha)" =
    "area of agricultural land under organic production",
  # -> area of forest under sustainable management
  "area of forest under sustainable management: total forest management certification by forest stewardship council and programme for the endorsement of forest certification" =
    "area of forest under sustainable management",
  "area of forest under sustainable management: total fsc and pefc forest management certification (million ha)" =
    "area of forest under sustainable management",
  # -> area of tree cover loss
  "area of tree cover loss (ha)" =
    "area of tree cover loss",
  # -> atlantic meridional overturning circulation (amoc)
  "amoc" =
    "atlantic meridional overturning circulation (amoc)",
  "the atlantic meridional overturning circulation (amoc)" =
    "atlantic meridional overturning circulation (amoc)",
  # -> atlantic multi-decadal variability (amv)
  "atlantic multi-decadal variability amv" =
    "atlantic multi-decadal variability (amv)",
  # -> atmospheric co2 concentrations
  "atmospheric co2 concentration" =
    "atmospheric co2 concentrations",
  "atmospheric concentrations" =
    "atmospheric co2 concentrations",
  # -> atmospheric dust deposition
  "global dust deposition" =
    "atmospheric dust deposition",
  # -> biodiversity barometer (percentage of respondents giving correct definition of biodiversit
  "biodiversity barometer (% of respondents giving correct definition of biodiversity)" =
    "biodiversity barometer (percentage of respondents giving correct definition of biodiversity)",
  # -> biodiversity barometer (percentage of respondents that have heard of biodiversity)
  "biodiversity barometer (% of respondents that have heard of biodiversity)" =
    "biodiversity barometer (percentage of respondents that have heard of biodiversity)",
  # -> biodiversity habitat index
  "bhi (overall habitat integrity)" =
    "biodiversity habitat index",
  # -> biodiversity habitat index (hotspots)
  "bhi (overall habitat integrity) - hotspots" =
    "biodiversity habitat index (hotspots)",
  # -> biodiversity habitat index (species persisting, hotspots)
  "bhi (species persisting) - hotspots" =
    "biodiversity habitat index (species persisting, hotspots)",
  # -> biodiversity intactness index
  "biodiversity intactness index (overall)" =
    "biodiversity intactness index",
  # -> biodiversity intactness index (tropical forest)
  "tropical forest bii (overall)" =
    "biodiversity intactness index (tropical forest)",
  # -> biodiversity intactness index (tropical forest, hotspots)
  "tropical forest bii (hotspots)" =
    "biodiversity intactness index (tropical forest, hotspots)",
  # -> biodiversity intactness index (tropical forest, indigenous lands)
  "tropical forest bii (indigenous lands)" =
    "biodiversity intactness index (tropical forest, indigenous lands)",
  # -> biomass of prey fish
  "prey fish biomass" =
    "biomass of prey fish",
  # -> biotime species richness indicator
  "biotime-local species richness" =
    "biotime species richness indicator",
  "local species richness (biotime)" =
    "biotime species richness indicator",
  # -> bird species per grid cell
  "bird species per grid cell (csar)" =
    "bird species per grid cell",
  # -> build up area
  "build up area per capita" =
    "build up area",
  # -> carbon sequestration
  "net carbon sequestration" =
    "carbon sequestration",
  # -> carbon uptake by land and ocean
  "land and ocean carbon sinks" =
    "carbon uptake by land and ocean",
  # -> catch per unit effort
  "catch-per-unit effort" =
    "catch per unit effort",
  # -> change in number of droughts
  "change in # droughts" =
    "change in number of droughts",
  # -> change in the extent of water-related ecosystems over time
  "change in the extent of inland water ecosystems over time" =
    "change in the extent of water-related ecosystems over time",
  # -> changes in mean precipitation
  "changes in precipitation" =
    "changes in mean precipitation",
  # -> changes in soil moisture
  "change in # soil moisture" =
    "changes in soil moisture",
  # -> connectivity indicator
  "connectivity indicator (in development)" =
    "connectivity indicator",
  # -> conservation status of species listed in the cites appendices has stabilized or improved
  "the conservation status of species listed on the cites appendices has stabilized or improved" =
    "conservation status of species listed in the cites appendices has stabilized or improved",
  # -> continuous global mangrove forest area
  "area of mangrove forest cover (km2)" =
    "continuous global mangrove forest area",
  "continuous global mangrove forest cover" =
    "continuous global mangrove forest area",
  "global cover of mangroves" =
    "continuous global mangrove forest area",
  "hectares of mangroves" =
    "continuous global mangrove forest area",
  "mangrove forest area" =
    "continuous global mangrove forest area",
  "mangrove forest cover" =
    "continuous global mangrove forest area",
  # -> cooling degree day index
  "cooling degree days" =
    "cooling degree day index",
  "cooling degree days above 22\u00b0c" =
    "cooling degree day index",
  # -> coverage of protected areas and other effective area-based conservation measures and tradi
  "coverage of protected areas and oecms and traditional territories (by governance type)" =
    "coverage of protected areas and other effective area-based conservation measures and traditional territories (by governance type)",
  # -> coverage of wetland dependent bird populations by designated ramsar sites
  "coverage of wetland dependent bird populations by designated ramsar sites. indicator from resolution ix.1 to be developed" =
    "coverage of wetland dependent bird populations by designated ramsar sites",
  # -> coverage of wetland dependent non-avian populations by designated ramsar sites
  "coverage of wetland dependent non-avian populations by designated ramsar sites. indicator from resolution ix.1 to be developed" =
    "coverage of wetland dependent non-avian populations by designated ramsar sites",
  # -> culturally salient species
  "culturally salient wild species populations" =
    "culturally salient species",
  # -> cumulative greenhouse gas emissions
  "cumulative ghg emissions" =
    "cumulative greenhouse gas emissions",
  # -> current distribution of species
  "distribution of species" =
    "current distribution of species",
  "spatial distribution" =
    "current distribution of species",
  "spatial distribution of species" =
    "current distribution of species",
  # -> daily maximum temperature above 35\u00b0c
  "daily maximum tempreature above 35\u00b0c" =
    "daily maximum temperature above 35\u00b0c",
  # -> direct economic loss attributed to disasters in relation to global gross domestic product 
  "direct economic loss in relation to global gdp, damage to critical infrastructure and number of disruptions to basic services, attributed to disasters" =
    "direct economic loss attributed to disasters in relation to global gross domestic product (gdp)",
  # -> distance travelled to harvest resources
  "distance travelled to reach a resource" =
    "distance travelled to harvest resources",
  "time needed or distance travelled to harvest resources" =
    "distance travelled to harvest resources",
  # -> dollar value of financial and technical assistance (including through north-south, south-s
  "financial and technical assistance provided in dollars (including through south-south, north-south and triangular cooperation)" =
    "dollar value of financial and technical assistance (including through north-south, south-south and triangular cooperation) committed to developing countries",
  "united states dollar value of financial and technical assistance, including through north-south, south-south and triangular cooperation, committed to developing countries and countries with economies in transition" =
    "dollar value of financial and technical assistance (including through north-south, south-south and triangular cooperation) committed to developing countries",
  # -> domestic material consumption
  "domestic material consumption per capita" =
    "domestic material consumption",
  "domestic material consumption, domestic material consumption per capita, and domestic material consumption per gdp" =
    "domestic material consumption",
  # -> ecological footprint
  "ecological footprint (number of earths needed to support human society)" =
    "ecological footprint",
  # -> energy intensity measured in terms of primary energy and gross domestic product
  "energy intensity measured in terms of primary energy and gdp" =
    "energy intensity measured in terms of primary energy and gross domestic product",
  # -> evaporative demand drought index (eddi)
  "potential evaporation, evaporative demand drought index" =
    "evaporative demand drought index (eddi)",
  # -> evapotranspiration
  "evapotranspiration (model ensemble)" =
    "evapotranspiration",
  # -> exposure to pm concentrations (population-weighted annual mean concentration of pm2.5)
  "population-weighted annual mean concentration of pm2.5" =
    "exposure to pm concentrations (population-weighted annual mean concentration of pm2.5)",
  "population-weighted pm2.5 concentrations" =
    "exposure to pm concentrations (population-weighted annual mean concentration of pm2.5)",
  # -> extent of agricultural land
  "agricultural land" =
    "extent of agricultural land",
  # -> extent of natural ecosystems
  "extent of natural ecosystems by type" =
    "extent of natural ecosystems",
  # -> extent of suitable habitat index (esh)
  "extent of suitable habitat esh index" =
    "extent of suitable habitat index (esh)",
  # -> extent to which (i) global citizenship education and (ii) education for sustainable develo
  "extent to which (i) global citizenship education and (ii) education for sustainable development are mainstreamed in (a) national education policies; (b) curricula; (c) teacher education; and (d) student assessment" =
    "extent to which (i) global citizenship education and (ii) education for sustainable development, including gender equality and human rights, are mainstreamed at all levels in: (a) national education policies, (b) curricula, (c) teacher education and (d) student assessments",
  "extent to which (i) global citizenship education and (ii) education for sustainable development, including gender equality and human rights, are mainstreamed at all levels in: (a) national education policies, (b) curricula, (c) teacher education and (d) student assessments (sdg 4.7.1)" =
    "extent to which (i) global citizenship education and (ii) education for sustainable development, including gender equality and human rights, are mainstreamed at all levels in: (a) national education policies, (b) curricula, (c) teacher education and (d) student assessments",
  # -> extinction risk
  "total extinction risk" =
    "extinction risk",
  # -> extraction biomass forestry
  "extraction biomass forstry" =
    "extraction biomass forestry",
  # -> extraction fossil-fuel
  "extraction fossil fuel" =
    "extraction fossil-fuel",
  # -> extraction of industrial and construction minerals
  "extraction of ind. and const. minerals" =
    "extraction of industrial and construction minerals",
  # -> fertilizer use
  "fertilizers used per unit area" =
    "fertilizer use",
  # -> first and last days of snow cover
  "the first and last days of snow cover" =
    "first and last days of snow cover",
  # -> fish catch potential
  "maximum fish catch potential" =
    "fish catch potential",
  # -> food deficit (kcal/capit a/day) (3- year average)
  "depth of the food deficit (kcal/capit a/day) (3- year average)" =
    "food deficit (kcal/capit a/day) (3- year average)",
  # -> foreign direct investment (fdi), official development assistance and south-south cooperati
  "foreign direct investment, official development assistance and south-south cooperation as a proportion of gross national income" =
    "foreign direct investment (fdi), official development assistance and south-south cooperation as a proportion of total domestic budget",
  # -> forest area as a percentage of total land area
  "forest area as a percentage of land area" =
    "forest area as a percentage of total land area",
  "forest area as a proportion of total land area" =
    "forest area as a percentage of total land area",
  # -> forest-specialist bird species per grid cell
  "forest-specialist bird species per grid cell (csar)" =
    "forest-specialist bird species per grid cell",
  # -> fossil-fuel energy consumption (percentage of total)
  "fossil fuel energy consumption (% of total)" =
    "fossil-fuel energy consumption (percentage of total)",
  # -> fraction of precipitation falling as snow
  "the fraction of precipitation falling as snow" =
    "fraction of precipitation falling as snow",
  # -> functional intactness (madingley)
  "average functional intactness" =
    "functional intactness (madingley)",
  "functional intactness" =
    "functional intactness (madingley)",
  # -> funding committed to environmental research
  "funding committed to environmental research ($)" =
    "funding committed to environmental research",
  # -> funding provided by the global environment facility
  "funding provided by the global environment facility ($)" =
    "funding provided by the global environment facility",
  # -> funding towards environmental education
  "funding for environmental education" =
    "funding towards environmental education",
  "funding towards environmental education ($)" =
    "funding towards environmental education",
  # -> funding towards environmental impact assessment
  "funding for environmental impact assessments" =
    "funding towards environmental impact assessment",
  "funding towards environmental impact assessment ($)" =
    "funding towards environmental impact assessment",
  # -> funding towards institutional capacity building in fisheries
  "funding towards institutional capacity building in fisheries ($)" =
    "funding towards institutional capacity building in fisheries",
  # -> funding towards nature reserves
  "funding towards nature reserves ($)" =
    "funding towards nature reserves",
  # -> funding towards species protection
  "funding towards species protection ($)" =
    "funding towards species protection",
  # -> genuine progress indicator (gpi)
  "genuine progress indicator" =
    "genuine progress indicator (gpi)",
  # -> glacier mass balance
  "glacial mass balance" =
    "glacier mass balance",
  "glacial mass balance (mm water equivalent)" =
    "glacier mass balance",
  "glaciar mass loss" =
    "glacier mass balance",
  "glacier areal and mass changes/loss rate" =
    "glacier mass balance",
  "glacier mass loss" =
    "glacier mass balance",
  # -> global climate risk index
  "climate risk index" =
    "global climate risk index",
  # -> global effort in bottom-trawling
  "global effort in bottom trawling" =
    "global effort in bottom-trawling",
  "global effort in bottom-trawling (kw sea-days)" =
    "global effort in bottom-trawling",
  # -> global funding committed towards environmental policy, laws, regulations and economic inst
  "global funding committed towards environmental policy, laws, regulations and economic instruments ($)" =
    "global funding committed towards environmental policy, laws, regulations and economic instruments",
  # -> global mean ocean surface ph
  "ocean ph" =
    "global mean ocean surface ph",
  "ph of ocean surface water" =
    "global mean ocean surface ph",
  "ocenan ph" = "global mean ocean surface ph",
  # -> global mean surface temperature (gmst)
  "annual global mean surface temperature" =
    "global mean surface temperature (gmst)",
  "global surface air temperature (gmst)" =
    "global mean surface temperature (gmst)",
  "global surface temperature" =
    "global mean surface temperature (gmst)",
  "observation-based estimate of global mean surface air temperature (gmst)" =
    "global mean surface temperature (gmst)",
  # -> global scale ecosystem carbon storage
  "global sclae ecosystem carbon storage" =
    "global scale ecosystem carbon storage",
  # -> global seagrass extent (seagrass cover and composition)
  "seagrass meadow area" =
    "global seagrass extent (seagrass cover and composition)",
  # -> global surface air temperature (gsat)
  "estimate of gsat (era-5)" =
    "global surface air temperature (gsat)",
  "mean gsat" =
    "global surface air temperature (gsat)",
  # -> global temperature
  "global temperatures" =
    "global temperature",
  # -> green status of species index (pollinators)
  "green status index (pollinators)" =
    "green status of species index (pollinators)",
  # -> greenhouse gas emissions
  "anthropogenic emissions of long-lived greenhouse gases (co2 equivalents)" =
    "greenhouse gas emissions",
  "anthropogenic emissions of long-lived greenhouse gases (ghg emissions)" =
    "greenhouse gas emissions",
  "ghg emissions" =
    "greenhouse gas emissions",
  "ghg emissions (in tonnes co2 eq and tonnes per capita)" =
    "greenhouse gas emissions",
  # -> greenhouse gas emissions change
  "ghg changes" =
    "greenhouse gas emissions change",
  "ghg emissions change" =
    "greenhouse gas emissions change",
  # -> greenhouse gas emissions from agriculture
  "ghg emissions from agriculture" =
    "greenhouse gas emissions from agriculture",
  # -> gross domestic product (gdp)
  "gdp" =
    "gross domestic product (gdp)",
  "gdp per capita" =
    "gross domestic product (gdp)",
  "gross domestic product" =
    "gross domestic product (gdp)",
  "gross domestic product per capita" =
    "gross domestic product (gdp)",
  # -> gross national happiness index
  "gross domestic happiness index" =
    "gross national happiness index",
  "gross national happiness measure" =
    "gross national happiness index",
  # -> growth in human appropriation of net primary productivity (hanpp)
  "growth in human appropriation of net primary production (hanpp)" =
    "growth in human appropriation of net primary productivity (hanpp)",
  # -> growth in marine species occurrence records accessible through ocean biodiversity informat
  "growth in marine species occurrence records accessible through obis" =
    "growth in marine species occurrence records accessible through ocean biodiversity information system (obis)",
  # -> growth rates of household expenditure or income per capita among the bottom 40 percent of 
  "growth rates of household expenditure or income per capita among the bottom 40 per cent of the population and the total population" =
    "growth rates of household expenditure or income per capita among the bottom 40 percent of the population and the total population",
  # -> hazardous waste
  "hazardous waste generated per capita" =
    "hazardous waste",
  "hazardous waste generation" =
    "hazardous waste",
  # -> highest five day precipitation amount (rx5day)
  "maximum five-day precipitation" =
    "highest five day precipitation amount (rx5day)",
  # -> highest one day precipitation amount (rx1day)
  "maximum one-day precipitation" =
    "highest one day precipitation amount (rx1day)",
  # -> human appropriation of net primary productivity (hanpp)
  "human appropriation of net primary production" =
    "human appropriation of net primary productivity (hanpp)",
  "human appropriation of net primary production (hanpp)" =
    "human appropriation of net primary productivity (hanpp)",
  "human appropriation of net primary productivity" =
    "human appropriation of net primary productivity (hanpp)",
  "human appropriation of net primary productivity (pg c)" =
    "human appropriation of net primary productivity (hanpp)",
  # -> human development index (hdi)
  "human development index" =
    "human development index (hdi)",
  # -> human population density
  "population density" =
    "human population density",
  # -> human population size
  "population size" =
    "human population size",
  "total population" =
    "human population size",
  # -> ice core measurements
  "ice core records" =
    "ice core measurements",
  "ice cores" =
    "ice core measurements",
  # -> improved water supply and sanitation
  "access to improved sanitation facilities" =
    "improved water supply and sanitation",
  # -> income
  "measure of income" =
    "income",
  # -> index of coastal eutrophication potential (icep)
  "index of coastal eutrophication" =
    "index of coastal eutrophication potential (icep)",
  "index of coastal eutrophication (icep)" =
    "index of coastal eutrophication potential (icep)",
  "index of coastal eutrophication potential" =
    "index of coastal eutrophication potential (icep)",
  # -> index of sustainable economic welfare (isew)
  "index of sustainable economic welfare" =
    "index of sustainable economic welfare (isew)",
  "sustainable economic welfare (isew)" =
    "index of sustainable economic welfare (isew)",
  # -> indian ocean dipole (iod)
  "indian ocean dipole (ipo)" =
    "indian ocean dipole (iod)",
  # -> installed renewable energy-generating capacity in developing countries (in watts per capit
  "installed renewable energy-generating capacity in developing and developed countries (in watts per capita)" =
    "installed renewable energy-generating capacity in developing countries (in watts per capita)",
  # -> integration of biodiversity into national accounting and reporting systems, defined as imp
  "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting (seea)",
  "number of countries implementing the system of environmental economic accounting" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting (seea)",
  "number of countries that have integrated biodiversity values into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting (seea)" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting (seea)",
  "number of countries with implementation of the system of environmental economic accounting" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting (seea)",
  # -> labour share of gross domestic product
  "labour share of gdp" =
    "labour share of gross domestic product",
  # -> land monsoon precipitation index
  "global land monsoon precipitation index" =
    "land monsoon precipitation index",
  # -> land not cultivated or urban
  "land not cultivated or urban (global)" =
    "land not cultivated or urban",
  # -> land use change
  "land-use change" =
    "land use change",
  # -> leaf area index (lai)
  "leaf area index" =
    "leaf area index (lai)",
  "leaf area index lai" =
    "leaf area index (lai)",
  # -> level of water stress (freshwater withdrawal as a proportion of available freshwater resou
  "freshwater withdrawal as a proportion of available freshwater resources" =
    "level of water stress (freshwater withdrawal as a proportion of available freshwater resources)",
  "level of water stress" =
    "level of water stress (freshwater withdrawal as a proportion of available freshwater resources)",
  "level of water stress: freshwater withdrawal as a proportion of available freshwater resources" =
    "level of water stress (freshwater withdrawal as a proportion of available freshwater resources)",
  # -> life cycle impact assessment (lcia)
  "life cycle impact assessment (lcia) e.g. lime ; lifecycle impact assessment method based on endpoint modelling" =
    "life cycle impact assessment (lcia)",
  # -> life expectancy
  "life expectancy at birth" =
    "life expectancy",
  # -> linguistic diversity index
  "global index of linguistic diversity and language threat level" =
    "linguistic diversity index",
  "index of linguistic diversity" =
    "linguistic diversity index",
  # -> live coral cover
  "hard coral cover and composition" =
    "live coral cover",
  "live coral cover on reefs" =
    "live coral cover",
  "percentage live coral cover" =
    "live coral cover",
  "percentage of live coral cover" =
    "live coral cover",
  # -> living planet index
  "living planet index (overall)" =
    "living planet index",
  # -> living planet index (migratory species)
  "living planet index for migratory species" =
    "living planet index (migratory species)",
  # -> living planet index (used species)
  "living planet index (for used species)" =
    "living planet index (used species)",
  "living planet index for used species" =
    "living planet index (used species)",
  # -> local species richness (predicts)
  "predicts species-richness indicator" =
    "local species richness (predicts)",
  # -> loss of phylogenetic diversity
  "expected loss of phylogenetic diversity" =
    "loss of phylogenetic diversity",
  # -> manufacturing value added as a proportion of gross domestic product and per capita
  "manufacturing value added as a proportion of gdp and per capita" =
    "manufacturing value added as a proportion of gross domestic product and per capita",
  # -> marine sediment cores
  "marine sediments" =
    "marine sediment cores",
  # -> marine stewardship council certified catch
  "marine stewardship council fish catch" =
    "marine stewardship council certified catch",
  # -> marine stewardship council certified fisheries
  "marine stewardship council certified fisheries (tonnes)" =
    "marine stewardship council certified fisheries",
  "msc certified fisheries" =
    "marine stewardship council certified fisheries",
  # -> marine stewardship council engaged fisheries
  "marine stewardship council engaged fisheries (tonnes)" =
    "marine stewardship council engaged fisheries",
  # -> marine trophic index (mti)
  "marine trophic index" =
    "marine trophic index (mti)",
  # -> material flow accounting
  "material flow analysis" =
    "material flow accounting",
  # -> material footprint
  "material footprint per capita" =
    "material footprint",
  "material footprint, material footprint per capita, and material footprint per gdp" =
    "material footprint",
  # -> maximum number of consecutive days with less than 1 mm of precipitation per day (cdd)
  "consecutive dry days (cdd)" =
    "maximum number of consecutive days with less than 1 mm of precipitation per day (cdd)",
  "maximum number of consecutive days with less than 1 mm of precipitation per day" =
    "maximum number of consecutive days with less than 1 mm of precipitation per day (cdd)",
  "number of consecutive dry days (cdd)" =
    "maximum number of consecutive days with less than 1 mm of precipitation per day (cdd)",
  # -> mean annual change in above-ground biomass
  "mean annual change in aboveground biomass" =
    "mean annual change in above-ground biomass",
  # -> mean annual temperature
  "mean air temperature" =
    "mean annual temperature",
  # -> mean polar sea ice extent
  "extent of ice cover" =
    "mean polar sea ice extent",
  "mean polar sea ice extent (million km2)" =
    "mean polar sea ice extent",
  # -> mean species abundance (msa)
  "mean species abundance" =
    "mean species abundance (msa)",
  "mean species abundance index" =
    "mean species abundance (msa)",
  # -> mean species abundance (msa) (hotspots)
  "mean species abundance index (hotspots)" =
    "mean species abundance (msa) (hotspots)",
  # -> mean species abundance (msa) (indigenous lands)
  "mean species abundance index (indigenous lands)" =
    "mean species abundance (msa) (indigenous lands)",
  # -> migratory birds threatened or near threatened on the red list index
  "migratory birds threatened or near threatened on the iucn red list" =
    "migratory birds threatened or near threatened on the red list index",
  # -> monetary benefits received in accordance with applicable internationally agreed access and
  "indicator on monetary benefits received" =
    "monetary benefits received in accordance with applicable internationally agreed access and benefit-sharing instruments",
  # -> mortality rate attributed to household and ambient air pollution
  "mortality rate attributable to household and ambient air pollution" =
    "mortality rate attributed to household and ambient air pollution",
  # -> national oceanic and atmospheric administration annual greenhouse gas index (aggi)
  "greenhouse gas index (national oceanic and atmospheric administration)" =
    "national oceanic and atmospheric administration annual greenhouse gas index (aggi)",
  # -> national recycling rate, tons of material recycled
  "recycling rate" =
    "national recycling rate, tons of material recycled",
  # -> native newcomer species arrive
  "native newcomer species" =
    "native newcomer species arrive",
  # -> natural habitat extent
  "percentage natural habitat extent" =
    "natural habitat extent",
  # -> net primary production
  "npp" =
    "net primary production",
  # -> net primary production remaining in ecosystems
  "npp remaining in ecosystems" =
    "net primary production remaining in ecosystems",
  # -> nitrogen use balance
  "nitrogen use balance (kg/km2)" =
    "nitrogen use balance",
  # -> noaa heat index (hi): number of days above 41\u00b0c threshold
  "heat index hi>41\u00b0c" =
    "noaa heat index (hi): number of days above 41\u00b0c threshold",
  # -> non-co2 fossil-fuel emissions
  "non-co2 fossil fuel emissions" =
    "non-co2 fossil-fuel emissions",
  # -> non-monetary benefits arising from applicable international access and benefit-sharing ins
  "indicator on non-monetary benefits" =
    "non-monetary benefits arising from applicable international access and benefit-sharing instruments",
  # -> normalized difference vegetation index (ndvi)
  "time series of normalized difference vegetation index (ndvi)" =
    "normalized difference vegetation index (ndvi)",
  # -> north atlantic oscillation (nao)
  "north atlantic oscillation nao" =
    "north atlantic oscillation (nao)",
  # -> north atlantic oscillation (nao) index
  "north atlantic oscillation nao index" =
    "north atlantic oscillation (nao) index",
  "summer north atlantic oscilation index" =
    "north atlantic oscillation (nao) index",
  # -> northern annular mode (nam)
  "northern annular mode nam" =
    "northern annular mode (nam)",
  # -> northern annular mode (nam) index
  "northern annular mode nam index" =
    "northern annular mode (nam) index",
  # -> number of 'hits' on scientific and technical guidance pages of the ramsar web-site and ass
  "number of 'hits' on scientific and technical guidance pages of the ramsar web-site and associated subtotals by country and ramsar region of the source of these hits. (data source: ramsar web-site analytics)" =
    "number of 'hits' on scientific and technical guidance pages of the ramsar web-site and associated subtotals by country and ramsar region of the source of these hits",
  # -> number of assessments on the red list index
  "number of assessments on the iucn red list of threatened species" =
    "number of assessments on the red list index",
  # -> number of centres (visitor centres, interpretation centres, education centres) have been e
  "number of centres (visitor centres, interpretation centres, education centres) have been established in ramsar sites. (data source: national reports)" =
    "number of centres (visitor centres, interpretation centres, education centres) have been established in ramsar sites",
  # -> number of centres at other wetlands
  "number of centres at other wetlands. (data source: national reports)" =
    "number of centres at other wetlands",
  # -> number of countries developing, adopting or implementing policy instruments aimed at encou
  "number of countries developing, adopting or implementing policy instruments aimed at supporting the shift to sustainable consumption and production" =
    "number of countries developing, adopting or implementing policy instruments aimed at encouraging and enabling people to make sustainable consumption choices",
  # -> number of countries implementing national legislation, policies or other measures regardin
  "number of countries implementing national legislation, policies or other measures regarding fpic related to conservation would work here for ips (not necessarily lcs), if spatial planning was substituted for conservation" =
    "number of countries implementing national legislation, policies or other measures regarding fpic related to conservation",
  # -> number of countries implementing natural resource accounts, excluding energy, within the s
  "the number of countries implementing natural resource accounts, excluding energy, within the system of environmental economic accounting" =
    "number of countries implementing natural resource accounts, excluding energy, within the system of environmental economic accounting",
  # -> number of countries implementing sustainable public procurement policies and action plans
  "degree of sustainable public procurement policies and action plan implementation" =
    "number of countries implementing sustainable public procurement policies and action plans",
  # -> number of countries that adopt and implement national disaster risk reduction strategies i
  "number of countries that adopt and implement national disaster risk reduction strategies in line with the sendai framework for disaster risk reduction 2015-2030 which include biodiversity" =
    "number of countries that adopt and implement national disaster risk reduction strategies in line with the sendai framework for disaster risk reduction 2015-2030",
  # -> number of countries with biodiversity relevant charges and fees
  "number of countries with biodiversity-relevant charges and fees" =
    "number of countries with biodiversity relevant charges and fees",
  # -> number of countries with biodiversity relevant taxes
  "number of countries with biodiversity-relevant taxes" =
    "number of countries with biodiversity relevant taxes",
  # -> number of countries with biodiversity relevant taxes, charges and fees
  "number of countries with national instruments on biodiversity-relevant taxes, charges and fees" =
    "number of countries with biodiversity relevant taxes, charges and fees",
  # -> number of countries with biodiversity relevant tradable permit schemes
  "number of countries with biodiversity-relevant tradable permit schemes" =
    "number of countries with biodiversity relevant tradable permit schemes",
  "number of countries with national instruments on biodiversity relevant tradable permit schemes" =
    "number of countries with biodiversity relevant tradable permit schemes",
  # -> number of countries with developed or revised nbsaps
  "percentage of countries with revised nbsaps" =
    "number of countries with developed or revised nbsaps",
  # -> number of countries with nationally determined contributions, long-term strategies, nation
  "number of countries with nationally determined contributions, long-term strategies, national adaptation plans, strategies as reported in adaptation communications and national communications" =
    "number of countries with nationally determined contributions, long-term strategies, national adaptation plans and adaptation communications, as reported to the secretariat of the united nations framework convention on climate change",
  # -> number of countries with regulations requiring recovery of depleted species
  "number of countries with regulations requiring recovered of depleted species" =
    "number of countries with regulations requiring recovery of depleted species",
  "presence of regulations requiring recovery of depleted species" =
    "number of countries with regulations requiring recovery of depleted species",
  # -> number of internet references to the ramsar convention
  "number of internet references to the ramsar convention. (data source: internet analysis)" =
    "number of internet references to the ramsar convention",
  # -> number of internet references to world wetland day activities
  "number of internet references to world wetland day activities. (data source: internet analysis)" =
    "number of internet references to world wetland day activities",
  # -> number of invasive alien species introduction events
  "number of invasive alien species introductions" =
    "number of invasive alien species introduction events",
  "trends in the numbers of invasive alien species introduction events" =
    "number of invasive alien species introduction events",
  # -> number of least developed countries and small island developing states with nationally det
  "number of least developed countries and small island developing states with nationally determined contributions, long-term strategies, national adaptation plans, strategies as reported in adaptation communications and national communications" =
    "number of least developed countries and small island developing states with nationally determined contributions, long-term strategies, national adaptation plans and adaptation communications, as reported to the secretariat of the united nations framework convention on climate change",
  # -> number of parties that are in category 1 under the national legislation project
  "percentage of category 1 nations in cites" =
    "number of parties that are in category 1 under the national legislation project",
  "percentage of countries that are category 1 cites parties" =
    "number of parties that are in category 1 under the national legislation project",
  # -> number of parties that have adopted standard procedures for making non-detriment findings 
  "number of cites parties that have adopted standard procedures for non-detrimental findings" =
    "number of parties that have adopted standard procedures for making non-detriment findings (ndfs)",
  # -> number of parties that have implemented relevant reporting under resolutions and decisions
  "cites parties that have implemented relevant resolutions and decisions" =
    "number of parties that have implemented relevant reporting under resolutions and decisions of the conference of the parties and/or standing committee recommendations",
  # -> number of parties which have acceded to the ramsar convention
  "number of parties which have acceded to the ramsar convention. (data source: national reports)" =
    "number of parties which have acceded to the ramsar convention",
  # -> number of people who died or disappeared in the process of migration towards an internatio
  "number of people who died or disappeared in the process of migration towards an international destinationi" =
    "number of people who died or disappeared in the process of migration towards an international destination",
  # -> number of permits or their equivalents for genetic resources (including those related to t
  "total number of permits, or their equivalent, granted for access to genetic resources" =
    "number of permits or their equivalents for genetic resources (including those related to traditional knowledge) by type of permit",
  # -> number of plant and animal genetic resources for food and agriculture secured in either me
  "number of (a) plant and (b) animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities" =
    "number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities",
  "number of plant and animal genetic resources for food and agriculture secured in medium- or longterm conservation facilities" =
    "number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities",
  "number of plant and animal genetic resources secured in medium or long-term conservation facilities" =
    "number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities",
  "number of plant genetic resources for food and agriculture secured in conservation facilities" =
    "number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities",
  # -> number of practical tools and guidance documents for wetland conservation and wise use, an
  "number of practical tools and guidance documents for wetland conservation and wise use, and other key scientific documentation, which has been developed by either strp, parties and others, and is available via the ramsar website. (data source: ramsar web-site)" =
    "number of practical tools and guidance documents for wetland conservation and wise use, and other key scientific documentation, which has been developed by either strp, parties and others, and is available via the ramsar website",
  # -> number of protected areas that have completed a site-level assessment of governance and eq
  "the number of protected areas that have completed a site-level assessment of governance and equity (sage)" =
    "number of protected areas that have completed a site-level assessment of governance and equity (sage)",
  # -> number of ramsar sites designated for the following under represented wetland types: karst
  "number of ramsar sites designated for the following under represented wetland types: karst and other subterranean hydrological systems; coral reefs; wet grasslands; peatland; sea-grass beds; mangroves; temporary pools; bivalve (shellfish) reefs (data source: ramsar sites database)" =
    "number of ramsar sites designated for the following under represented wetland types: karst and other subterranean hydrological systems; coral reefs; wet grasslands; peatland; sea-grass beds; mangroves; temporary pools; bivalve (shellfish) reefs",
  # -> number of ramsar sites removed from the montreux record
  "number of ramsar sites removed from the montreux record. (data source: ramsar site database)" =
    "number of ramsar sites removed from the montreux record",
  # -> number of ramsar sites reported by parties to the ramsar secretariat of negative human-ind
  "number of ramsar sites reported by parties to the ramsar secretariat of negative human-induced change or likely change in the ecological character of ramsar sites pursuant to article 3.2. (data source: national reports)" =
    "number of ramsar sites reported by parties to the ramsar secretariat of negative human-induced change or likely change in the ecological character of ramsar sites pursuant to article 3.2",
  # -> number of ramsar sites that have been designated
  "number of ramsar sites that have been designated. (data source: ramsar sites database)" =
    "number of ramsar sites that have been designated",
  # -> number of ramsar sites that have effective, implemented management plans
  "number of ramsar sites that have effective, implemented management planning. (data source: new national report question)" =
    "number of ramsar sites that have effective, implemented management plans",
  "number of ramsar sites that have effective, implemented management plans. (data source: national report)" =
    "number of ramsar sites that have effective, implemented management plans",
  # -> number of regional initiatives successfully implemented
  "number of regional initiatives successfully implemented. (data source: national reports)" =
    "number of regional initiatives successfully implemented",
  # -> number of relevant ramsar handbooks downloaded from the ramsar web-site and subtotals by c
  "number of relevant ramsar handbooks downloaded from the ramsar web-site and subtotals by country and ramsar region of the source of these downloads. (data source: ramsar web-site analytics)" =
    "number of relevant ramsar handbooks downloaded from the ramsar web-site and subtotals by country and ramsar region of the source of these downloads",
  # -> number of social media links to world wetland day
  "number of social media links to world wetland day. (data source: social media analysis)" =
    "number of social media links to world wetland day",
  # -> number of species extinctions
  "number of extinctions" =
    "number of species extinctions",
  # -> number of strp briefing papers downloaded from the ramsar web site and subtotals by countr
  "number of strp briefing papers downloaded from the ramsar web site and subtotals by country and ramsar region of the source of these downloads. (data source: ramsar web-site analytics)" =
    "number of strp briefing papers downloaded from the ramsar web site and subtotals by country and ramsar region of the source of these downloads",
  # -> number of transboundary ramsar sites that have been designated
  "number of transboundary ramsar sites that have been designated. (data source: ramsar sites database)" =
    "number of transboundary ramsar sites that have been designated",
  "total number of transboundary ramsar sites. (data source: ramsar sites database)" =
    "number of transboundary ramsar sites that have been designated",
  # -> number of world wetland day activities or events reported to the secretariat
  "number of world wetland day activities or events reported to the secretariat. (data source: ramsar cepa program)" =
    "number of world wetland day activities or events reported to the secretariat",
  # -> numbers of scientific publications on biodiversity
  "number of publications" =
    "numbers of scientific publications on biodiversity",
  # -> ocean acidification
  "reduced ocean acidification" =
    "ocean acidification",
  # -> ocean heat content
  "ocean heat content ohc" =
    "ocean heat content",
  "temperature/ocean heat content" =
    "ocean heat content",
  # -> ocean salinity
  "salinity" =
    "ocean salinity",
  # -> oecd better life index
  "better life index" =
    "oecd better life index",
  # -> official development assistance (oda) on conservation and sustainable use of biodiversity
  "international public funding, including official development assistance (oda) for conservation and sustainable use of biodiversity and ecosystems" =
    "official development assistance (oda) on conservation and sustainable use of biodiversity",
  "official development assistance on conservation and sustainable use of biodiversity" =
    "official development assistance (oda) on conservation and sustainable use of biodiversity",
  # -> official development assistance provided in support of the cbd objectives
  "official development assistance provided in support of cbd objectives" =
    "official development assistance provided in support of the cbd objectives",
  "official development assistance provided in support of the cbd objectives ($)" =
    "official development assistance provided in support of the cbd objectives",
  # -> overall species abundance
  "species abundance" =
    "overall species abundance",
  # -> ozone depleting substances and greenhouse gases
  "odss and ghgs" =
    "ozone depleting substances and greenhouse gases",
  # -> palmer drought severity index (pdsi)
  "palmer drought severity index" =
    "palmer drought severity index (pdsi)",
  # -> passenger and freight volumes, by mode of transport
  "passenger and freight volumes by mode of transport" =
    "passenger and freight volumes, by mode of transport",
  # -> people per outlet mcdonals
  "people per ouletlet mcdonals" =
    "people per outlet mcdonals",
  # -> per capita calorie intake
  "caloric intake" =
    "per capita calorie intake",
  "calorie intake" =
    "per capita calorie intake",
  # -> percentage loss of red list indexed species from ramsar sites
  "% loss of iucn red listed species from ramsar sites" =
    "percentage loss of red list indexed species from ramsar sites",
  # -> percentage of contracting parties that have provided additional financial support through 
  "% of contracting parties that have provided additional financial support through voluntary contributions to non-core funded convention activities. (national reports to cop12)" =
    "percentage of contracting parties that have provided additional financial support through voluntary contributions to non-core funded convention activities",
  # -> percentage of countries that have ratified the nagoya protocol
  "number of countries that have ratified the nagoya protocol" =
    "percentage of countries that have ratified the nagoya protocol",
  # -> percentage of cropped landscapes with at least 10 percent natural land
  "percentage of cropped landscapes with at least 10 % natural land" =
    "percentage of cropped landscapes with at least 10 percent natural land",
  # -> percentage of parties having a national inventory of invasive alien species that currently
  "% of parties having a national inventory of invasive alien species that currently or potentially impact the ecological character of wetlands. (data source: national reports)" =
    "percentage of parties having a national inventory of invasive alien species that currently or potentially impact the ecological character of wetlands",
  # -> percentage of parties having national ramsar committees that include both governmental and
  "% of parties having national ramsar committees that include both governmental and non-governmental representation. (data source: new question for national reports)" =
    "percentage of parties having national ramsar committees that include both governmental and non-governmental representation",
  # -> percentage of parties reporting actions taken to implement incentive measures that encoura
  "% of parties reporting actions taken to implement incentive measures that encourage the conservation and wise use of wetlands. (data source: national reports)" =
    "percentage of parties reporting actions taken to implement incentive measures that encourage the conservation and wise use of wetlands",
  # -> percentage of parties reporting actions taken to remove perverse incentive measures that d
  "% of parties reporting actions taken to remove perverse incentive measures that discourage conservation and wise use of wetlands. (data source: national reports)" =
    "percentage of parties reporting actions taken to remove perverse incentive measures that discourage conservation and wise use of wetlands",
  # -> percentage of parties reporting private sector undertaking activities for the conservation
  "% of parties reporting private sector undertaking activities for the conservation, wise use and management of wetlands in general. (data source: national reports)" =
    "percentage of parties reporting private sector undertaking activities for the conservation, wise use and management of wetlands in general",
  # -> percentage of parties reporting to the ramsar secretariat all cases of negative human-indu
  "% of parties reporting to the ramsar secretariat all cases of negative human-induced change or likely change in the ecological character of ramsar sites pursuant to article 3.2. (data source: national reports)" =
    "percentage of parties reporting to the ramsar secretariat all cases of negative human-induced change or likely change in the ecological character of ramsar sites pursuant to article 3.2",
  # -> percentage of parties that consider wetlands as natural water infrastructure integral to w
  "% of parties that consider wetlands as natural water infrastructure integral to water resource management at the scale of river basin. (data source: national reports)" =
    "percentage of parties that consider wetlands as natural water infrastructure integral to water resource management at the scale of river basin",
  # -> percentage of parties that have adopted wetland policies or equivalent instruments that pr
  "% of parties that have adopted wetland policies or equivalent instruments that promote the wise use of their wetlands. (data source: national reports)" =
    "percentage of parties that have adopted wetland policies or equivalent instruments that promote the wise use of their wetlands",
  # -> percentage of parties that have been involved in the development and implementation of a r
  "% of parties that have been involved in the development and implementation of a regional initiative under the framework of the convention. (data source: national reports)" =
    "percentage of parties that have been involved in the development and implementation of a regional initiative under the framework of the convention",
  # -> percentage of parties that have branded world wetlands day activities
  "% of parties that have branded world wetlands day activities. (data source: national reports)" =
    "percentage of parties that have branded world wetlands day activities",
  # -> percentage of parties that have complete national wetland inventories
  "% of parties that have complete national wetland inventories. (data source: national reports)" =
    "percentage of parties that have complete national wetland inventories",
  # -> percentage of parties that have effective cooperative management in place for shared wetla
  "% of parties that have effective cooperative management in place for shared wetland systems (for example in shared river basins and coastal zones). (data source: national reports)" =
    "percentage of parties that have effective cooperative management in place for shared wetland systems (for example in shared river basins and coastal zones)",
  # -> percentage of parties that have established national action plans for wetland cepa
  "% of parties that have established national action plans for wetland cepa. (data source: national reports)" =
    "percentage of parties that have established national action plans for wetland cepa",
  # -> percentage of parties that have established networks including twinning arrangements natio
  "% of parties that have established networks including twinning arrangements nationally or internationally for knowledge sharing and training for wetlands that share common features. (data source: national reports)" =
    "percentage of parties that have established networks including twinning arrangements nationally or internationally for knowledge sharing and training for wetlands that share common features",
  # -> percentage of parties that have established or reviewed national policies or guidelines on
  "% of parties that have established or reviewed national policies or guidelines on invasive wetland species control and management. (data source: national reports)" =
    "percentage of parties that have established or reviewed national policies or guidelines on invasive wetland species control and management",
  # -> percentage of parties that have established restoration plans [or activities] for sites
  "% of parties that have established restoration plans [or activities] for sites. (data source: national reports)" =
    "percentage of parties that have established restoration plans [or activities] for sites",
  # -> percentage of parties that have implemented effective restoration or rehabilitation projec
  "% of parties that have implemented effective restoration or rehabilitation projects. (data source: national reports)" =
    "percentage of parties that have implemented effective restoration or rehabilitation projects",
  # -> percentage of parties that have implemented programmes or projects that contribute to pove
  "% of parties that have implemented programmes or projects that contribute to poverty alleviation objectives or food and water security plans. (data source: national reports)" =
    "percentage of parties that have implemented programmes or projects that contribute to poverty alleviation objectives or food and water security plans",
  # -> percentage of parties that have included wetland issues within national policies or measur
  "% of parties that have included wetland issues within national policies or measures on agriculture. (data source: national reports)" =
    "percentage of parties that have included wetland issues within national policies or measures on agriculture",
  # -> percentage of parties that have included wetland issues within national strategies and pla
  "% of parties that have included wetland issues into national strategies and in the planning processes such as for water resource management and water efficiency plans. (data source: national reports)" =
    "percentage of parties that have included wetland issues within national strategies and planning processes such as water resource management and water efficiency plans",
  "% of parties that have included wetland issues within national strategies and planning processes such as water resource management and water efficiency plans. (data source: national reports)" =
    "percentage of parties that have included wetland issues within national strategies and planning processes such as water resource management and water efficiency plans",
  # -> percentage of parties that have incorporated wetlands issues into poverty eradication stra
  "% of parties that have incorporated wetlands issues into poverty eradication strategies. (data source: national reports)" =
    "percentage of parties that have incorporated wetlands issues into poverty eradication strategies",
  # -> percentage of parties that have made an assessment of national and local training needs fo
  "% of parties that have made an assessment of national and local training needs for the implementation of the convention. (national reports to cop12)" =
    "percentage of parties that have made an assessment of national and local training needs for the implementation of the convention",
  # -> percentage of parties that have made assessment of ecosystem services of ramsar sites
  "% of parties that have made assessment of ecosystem services of ramsar sites. (data source: national reports)" =
    "percentage of parties that have made assessment of ecosystem services of ramsar sites",
  # -> percentage of parties that have made assessments of effective management of ramsar sites
  "% of parties that have made assessments of effective management of ramsar sites. (data source: national reports)" =
    "percentage of parties that have made assessments of effective management of ramsar sites",
  # -> percentage of parties that have received funding support from development assistance agenc
  "% of parties that have received funding support from development assistance agencies for national wetlands conservation and management. (data source: national reports)" =
    "percentage of parties that have received funding support from development assistance agencies for national wetlands conservation and management",
  # -> percentage of parties that have taken actions to address the issues for which ramsar sites
  "% of parties that have taken actions to address the issues for which ramsar sites have been listed on the montreux record. (national reports to cop12)" =
    "percentage of parties that have taken actions to address the issues for which ramsar sites have been listed on the montreux record",
  # -> percentage of parties that have updated their national inventories in the last decade
  "% of parties that have updated their national inventories in the last decade. (data source: new question for national reports)" =
    "percentage of parties that have updated their national inventories in the last decade",
  # -> percentage of parties where co-ordination mechanisms for the implementation of meas exist 
  "% of parties where co-ordination mechanisms for the implementation of meas exist at a national level. (data source: new question for national reports)" =
    "percentage of parties where co-ordination mechanisms for the implementation of meas exist at a national level",
  # -> percentage of parties with a) a governmental cepa national focal point and b) a non-govern
  "% of parties with a) a governmental cepa national focal point and b) a non-governmental national focal point (data source: national reports)" =
    "percentage of parties with a) a governmental cepa national focal point and b) a non-governmental national focal point",
  # -> percentage of ramsar sites that have updated ramsar information sheets
  "% of ramsar sites that have updated ramsar information sheets. (data source: ramsar sites database)" =
    "percentage of ramsar sites that have updated ramsar information sheets",
  # -> percentage of ramsar sites which have improved the sustainability of water use in the cont
  "% of ramsar sites which have improved the sustainability of water use in the context of ecosystem requirements" =
    "percentage of ramsar sites which have improved the sustainability of water use in the context of ecosystem requirements",
  # -> percentage of threatened species that are improving in status
  "percentage of threatened species that are improving in status according to the red list" =
    "percentage of threatened species that are improving in status",
  # -> pesticide use per area of cropland
  "pesticide use" =
    "pesticide use per area of cropland",
  "pesticide use (tonnes)" =
    "pesticide use per area of cropland",
  "pesticides used per unit area" =
    "pesticide use per area of cropland",
  "rate of use of pesticides" =
    "pesticide use per area of cropland",
  "trends in pesticide use" =
    "pesticide use per area of cropland",
  # -> planetary pressures adjusted human development index (hdi)
  "planetary pressures adjusted human development index" =
    "planetary pressures adjusted human development index (hdi)",
  # -> plastic debris density
  "floating debris density" =
    "plastic debris density",
  "floating plastic debris density" =
    "plastic debris density",
  "floating plastic debris density [by micro and macro plastics]" =
    "plastic debris density",
  "floating plastic litter density" =
    "plastic debris density",
  # -> pm2.5 emissions
  "pm 2.5 emissions" =
    "pm2.5 emissions",
  # -> population growth (annual percentage)
  "population growth (annual %)" =
    "population growth (annual percentage)",
  # -> prevalence of moderate or severe food insecurity in the population, based on the food inse
  "prevalence of moderate or severe food insecurity in the population, based on the food insecurity experience scale" =
    "prevalence of moderate or severe food insecurity in the population, based on the food insecurity experience scale (fies)",
  # -> primary production of marine biota
  "marine npp" =
    "primary production of marine biota",
  "marine npp (remote sensing)" =
    "primary production of marine biota",
  # -> proportion of cities with a direct participation structure of civil society in urban plann
  "percentage of cities with direct participation structure of civil society in urban planning and management, which operate regularly and democratically" =
    "proportion of cities with a direct participation structure of civil society in urban planning and management that operate regularly and democratically",
  # -> proportion of countries adopting relevant national legislation and adequately resourcing t
  "percentage of countries with invasive alien species legislation" =
    "proportion of countries adopting relevant national legislation and adequately resourcing the prevention or control of invasive alien species",
  "trends in numbers of countries with national legislation and other policy measures relevant to the prevention and/or control of invasive alien species" =
    "proportion of countries adopting relevant national legislation and adequately resourcing the prevention or control of invasive alien species",
  # -> proportion of countries that (a) have conducted at least one population and housing census
  "proportion of countries that (a) have conducted at least one population and housing census in the last 10 years; and (b) have achieved 100 per cent birth registration and 80 per cent death registration" =
    "proportion of countries that (a) have conducted at least one population and housing census in the last 10 years; and (b) have achieved 100 percent birth registration and 80 percent death registration",
  # -> proportion of countries where the legal framework (including customary law) guarantees wom
  "number of countries where the legal framework (including customary law) guarantees women's equal rights to land ownership and/or control" =
    "proportion of countries where the legal framework (including customary law) guarantees women equal rights to land ownership and/or control",
  "proportion of countries where the legal framework (including customary law) guarantees women's equal rights to land ownership and/or control" =
    "proportion of countries where the legal framework (including customary law) guarantees women equal rights to land ownership and/or control",
  # -> proportion of countries with migration policies that facilitate orderly, safe, regular and
  "number of countries with migration policies that facilitate orderly, safe, regular and responsible migration and mobility of people" =
    "proportion of countries with migration policies that facilitate orderly, safe, regular and responsible migration and mobility of people",
  # -> proportion of countries with systems to track and make public allocations for gender equal
  "number of countries with systems to track and make public allocations for gender equality and women's empowerment" =
    "proportion of countries with systems to track and make public allocations for gender equality and women's empowerment",
  # -> proportion of domestic and industrial wastewater flows safely treated
  "proportion of domestic and industrial wastewater flow safely treated" =
    "proportion of domestic and industrial wastewater flows safely treated",
  # -> proportion of fish stocks within biologically sustainable levels
  "fish stocks biologically sustainable" =
    "proportion of fish stocks within biologically sustainable levels",
  "proportion of fish stocks at or below a target biomass that can undergo recovery" =
    "proportion of fish stocks within biologically sustainable levels",
  "proportion of fish stocks in safe biological limits" =
    "proportion of fish stocks within biologically sustainable levels",
  "proportion of fish stocks within safe biological limits" =
    "proportion of fish stocks within biologically sustainable levels",
  "proportion of fish stocks within sustainable levels" =
    "proportion of fish stocks within biologically sustainable levels",
  "trends in the proportion of fish stocks within biologically sustainable levels" =
    "proportion of fish stocks within biologically sustainable levels",
  # -> proportion of known species assessed through the red list index
  "proportion of known species assessed through the iucn red list" =
    "proportion of known species assessed through the red list index",
  "taxa assessed for the iucn red list" =
    "proportion of known species assessed through the red list index",
  # -> proportion of land that is degraded over total land area
  "proportion of land degraded over total land area" =
    "proportion of land that is degraded over total land area",
  # -> proportion of local breeds classified as being at risk of extinction
  "proportion of local breeds classified as being at risk extinction" =
    "proportion of local breeds classified as being at risk of extinction",
  "proportion of local breeds, classified as being at risk, not-at-risk or unknown level of risk of extinction" =
    "proportion of local breeds classified as being at risk of extinction",
  # -> proportion of municipal solid waste collected and managed in controlled facilities out of 
  "municipal solid waste collected and managed" =
    "proportion of municipal solid waste collected and managed in controlled facilities out of total municipal waste generated, by cities",
  # -> proportion of people living below 50 percent of median income, by sex, age and persons wit
  "proportion of people living below 50 per cent of median income, by sex, age and persons with disabilities" =
    "proportion of people living below 50 percent of median income, by sex, age and persons with disabilities",
  # -> proportion of persons victim of physical or sexual harassment, by sex, age, disability sta
  "proportion of persons victim of non-sexual or sexual harassment, by sex, age, disability status and place of occurrence, in the previous 12 months" =
    "proportion of persons victim of physical or sexual harassment, by sex, age, disability status and place of occurrence, in the previous 12 months",
  # -> proportion of population using safely managed drinking water services
  "percentage of population using safely managed drinking water services" =
    "proportion of population using safely managed drinking water services",
  "proportion of population using an improved drinking water source" =
    "proportion of population using safely managed drinking water services",
  # -> proportion of population using (a) safely managed sanitation services and (b) a hand-washing facility with soap and water
  "proportion of population using safely managed sanitation services, including a hand-washing facility with soap and water" =
    "proportion of population using (a) safely managed sanitation services and (b) a hand-washing facility with soap and water",
  # -> proportion of population who believe decision-making is inclusive and responsive, by sex, 
  "percentage of population who believe decision making is inclusive and responsive, by sex, age, disability and population group" =
    "proportion of population who believe decision-making is inclusive and responsive, by sex, age, disability and population group",
  # -> proportion of populations within species with an effective population size > 500
  "the proportion of populations within species with an effective population size > 500" =
    "proportion of populations within species with an effective population size > 500",
  # -> proportion of terrestrial protected area as a share of the total land area
  "terrestrial protected areas as a percentage of total land area" =
    "proportion of terrestrial protected area as a share of the total land area",
  "the proportion of terrestrial protected area as a share of the total land area expressed as a percentage" =
    "proportion of terrestrial protected area as a share of the total land area",
  # -> proportion of terrestrial, freshwater and marine ecological regions which are conserved by
  "protected area coverage of terrestrial, marine and freshwater ecoregions" =
    "proportion of terrestrial, freshwater and marine ecological regions which are conserved by protected areas or other effective area-based conservation measures",
  # -> proportions of positions in national and local institutions, including (a) the legislature
  "percentage of positions in national and local institutions, including (a) the legislatures; (b) the public service; and (c) the judiciary, compared to national distributions, by sex, age, persons with disabilities and population groups" =
    "proportions of positions in national and local institutions, including (a) the legislatures; (b) the public service; and (c) the judiciary, compared to national distributions, by sex, age, persons with disabilities and population groups",
  # -> protected area connectedness (parc-connectedness) index
  "protected area connectedness index" =
    "protected area connectedness (parc-connectedness) index",
  "protected area connectedness index (parc-connectedness)" =
    "protected area connectedness (parc-connectedness) index",
  # -> protected area coverage
  "percentage of areas covered by protected areas- marine, coastal, terrestrial, inland water" =
    "protected area coverage",
  # -> protected area coverage of important sites for mountain biodiversity
  "coverage by protected areas of important sites for mountain biodiversity" =
    "protected area coverage of important sites for mountain biodiversity",
  "percentage of mountain key biodiversity areas covered by protected areas" =
    "protected area coverage of important sites for mountain biodiversity",
  # -> protected area coverage of important sites for terrestrial and freshwater biodiversity
  "proportion of important sites for terrestrial and freshwater biodiversity that are covered by protected areas, by ecosystem type" =
    "protected area coverage of important sites for terrestrial and freshwater biodiversity",
  # -> protected area coverage of key biodiversity areas
  "percentage of key biodiversity areas covered by protected areas" =
    "protected area coverage of key biodiversity areas",
  "percentage of terrestrial key biodiversity areas covered by protected areas" =
    "protected area coverage of key biodiversity areas",
  "protected area coverage of key biodiversity areas (including important bird and biodiversity areas, alliance for zero extinction sites)" =
    "protected area coverage of key biodiversity areas",
  # -> protected area coverage of marine and coastal areas
  "coverage of marine protected areas" =
    "protected area coverage of marine and coastal areas",
  "coverage of protected areas in relation to marine areas" =
    "protected area coverage of marine and coastal areas",
  "percentage of marine and coastal areas covered by protected areas" =
    "protected area coverage of marine and coastal areas",
  # -> protected area management effectiveness (pame)
  "management effectiveness in national protected areas" =
    "protected area management effectiveness (pame)",
  "protected area management effectiveness" =
    "protected area management effectiveness (pame)",
  # -> protected connected (protconn) index
  "protconn" =
    "protected connected (protconn) index",
  # -> quality of governance index
  "governance index" =
    "quality of governance index",
  # -> rate of invasive species impact and rate of impact
  "trends in numbers and spatial distribution of invasive alien species and their impacts" =
    "rate of invasive species impact and rate of impact",
  "trends in numbers of invasive alien species and their impacts" =
    "rate of invasive species impact and rate of impact",
  # -> rate of wood removal
  "total wood removals" =
    "rate of wood removal",
  # -> ratio of land consumption rate to population growth rate
  "ratio of land consumption rate to urban population growth rate at comparable scale" =
    "ratio of land consumption rate to population growth rate",
  # -> red list index
  "iucn red list" =
    "red list index",
  "iucn red list index" =
    "red list index",
  "iucn red list of threatened species" =
    "red list index",
  "red list index (overall)" =
    "red list index",
  "red list of threatened species" =
    "red list index",
  # -> red list index (birds and mammals)
  "red list index for birds and mammals" =
    "red list index (birds and mammals)",
  # -> red list index (for utilized species)
  "red list index (impacts of utilisation)" =
    "red list index (for utilized species)",
  "red list index (impacts of utilization)" =
    "red list index (for utilized species)",
  "red list index (species used for food and medicine)" =
    "red list index (for utilized species)",
  "red list index for used species" =
    "red list index (for utilized species)",
  "rli (species used in food and medicine)" =
    "red list index (for utilized species)",
  # -> red list index (forest specialists)
  "rli (forest specialists)" =
    "red list index (forest specialists)",
  # -> red list index (impacts of pollution)
  "red list index (impact of pollution)" =
    "red list index (impacts of pollution)",
  # -> red list index (internationally traded birds)
  "rli (internationally traded birds)" =
    "red list index (internationally traded birds)",
  # -> red list index (internationally traded species and migratory species)
  "red list index (for internationally traded species and for migratory species)" =
    "red list index (internationally traded species and migratory species)",
  # -> red list index (internationally traded species)
  "red list index (for internationally traded species)" =
    "red list index (internationally traded species)",
  # -> red list index (migratory species)
  "red list index for migratory species" =
    "red list index (migratory species)",
  # -> red list index (pollinators)
  "red list index (pollinating species)" =
    "red list index (pollinators)",
  "red list index (pollinator species)" =
    "red list index (pollinators)",
  "rli (pollinators)" =
    "red list index (pollinators)",
  # -> red list index (vertebrate pollinators)
  "red list index for vertebrate pollinators" =
    "red list index (vertebrate pollinators)",
  # -> red list index (wild relatives of farmed and domesticated species)
  "red list index (wild relatives of domesticated animals)" =
    "red list index (wild relatives of farmed and domesticated species)",
  "rli (wild relatives)" =
    "red list index (wild relatives of farmed and domesticated species)",
  # -> red list of ecosystems
  "iucn red list of ecosystems" =
    "red list of ecosystems",
  # -> remaining primary vegetation
  "remaining primary vegetations" =
    "remaining primary vegetation",
  # -> renewable electricity consumption (percentage of total electricity output)
  "renewable electricity consumption (% of total electricity output)" =
    "renewable electricity consumption (percentage of total electricity output)",
  # -> research and development expenditure as a proportion of gross domestic product
  "research and development expenditure as a proportion of gdp" =
    "research and development expenditure as a proportion of gross domestic product",
  # -> revenue generated and finance mobilized from biodiversity relevant economic instruments
  "revenue generated and finance mobilized from biodiversity-relevant economic instruments" =
    "revenue generated and finance mobilized from biodiversity relevant economic instruments",
  # -> sea ice extent
  "sea ice extent/area" =
    "sea ice extent",
  # -> sea level rise
  "increase in the global mean sea level" =
    "sea level rise",
  "rate of rslr (relative sea level rise)" =
    "sea level rise",
  "sea level change" =
    "sea level rise",
  # -> sea surface temperature (sst)
  "sea surface temperature" =
    "sea surface temperature (sst)",
  "sea surface temperature (sst index)" =
    "sea surface temperature (sst)",
  "sea surface temperature (sst) index" =
    "sea surface temperature (sst)",
  "sea surface temperature sst" =
    "sea surface temperature (sst)",
  # -> seasonal variations in atmospheric co2 concentrations
  "seasonal variations in atmospheric co2 concentration" =
    "seasonal variations in atmospheric co2 concentrations",
  # -> simple daily intensity index (sdii)
  "simple precipitation intensity index" =
    "simple daily intensity index (sdii)",
  # -> simulated global mean surface air temperature (gsat) anomalies
  "simulated global mean surface air temperature (gsat)" =
    "simulated global mean surface air temperature (gsat) anomalies",
  # -> soil moisture (sm)
  "soil moisture" =
    "soil moisture (sm)",
  "surface soil moisture" =
    "soil moisture (sm)",
  "total soil moisture" =
    "soil moisture (sm)",
  # -> soil moisture anomalies (sma)
  "soil moisture anomalies" =
    "soil moisture anomalies (sma)",
  # -> soil organic carbon (soc) stocks
  "changes in soil organic carbon stocks" =
    "soil organic carbon (soc) stocks",
  "soil organic carbon" =
    "soil organic carbon (soc) stocks",
  "soil organic carbon (correlative model)" =
    "soil organic carbon (soc) stocks",
  "soil organic carbon (mechanistic models)" =
    "soil organic carbon (soc) stocks",
  "trends in soil organic carbon stock" =
    "soil organic carbon (soc) stocks",
  # -> species occurrence records accessible through the global biodiversity information facility
  "growth in species occurrence records accessible through gbif" =
    "species occurrence records accessible through the global biodiversity information facility",
  "growth in species occurrence records accessible through the global biodiversity information facility" =
    "species occurrence records accessible through the global biodiversity information facility",
  "number of species occurrence records in the global biodiversity information facility" =
    "species occurrence records accessible through the global biodiversity information facility",
  "species with data included in the global biodiversity information facility" =
    "species occurrence records accessible through the global biodiversity information facility",
  # -> species richness per grid cell
  "species richness per grid cell (aim)" =
    "species richness per grid cell",
  # -> species status index
  "species status information index" =
    "species status index",
  # -> species used for medicine
  "fraction of species known to be medicinal" =
    "species used for medicine",
  # -> speleothem records
  "speleotherm records" =
    "speleothem records",
  # -> standardized precipitation evapotranspiration index (spei)
  "standardized precipitation evapotranspiration index" =
    "standardized precipitation evapotranspiration index (spei)",
  "standardized precipitation evapotranspiration index spei" =
    "standardized precipitation evapotranspiration index (spei)",
  # -> standardized precipitation index (spi)
  "standardized precipitation index" =
    "standardized precipitation index (spi)",
  # -> standardized runoff index (sri)
  "standardized runoff index" =
    "standardized runoff index (sri)",
  # -> standardized soil moisture index (ssmi)
  "standardized soil moisture index" =
    "standardized soil moisture index (ssmi)",
  # -> standardized streamflow index (ssi)
  "standardized streamflow index" =
    "standardized streamflow index (ssi)",
  # -> statistical capacity indicator for sustainable development goal monitoring
  "statistical capacity indicators" =
    "statistical capacity indicator for sustainable development goal monitoring",
  # -> summer arctic sea ice extent
  "summer arctic sea-ice extent" =
    "summer arctic sea ice extent",
  # -> sustainable development goals index
  "sdg index" =
    "sustainable development goals index",
  # -> sustainable fisheries as a proportion of gross domestic product in small island developing
  "sustainable fisheries as a proportion of gdp in small island developing states, least developed countries and all countries" =
    "sustainable fisheries as a proportion of gross domestic product in small island developing states, least developed countries and all countries",
  # -> terrestrial c sequestration
  "terrestrial c sequestration (model ensemble)" =
    "terrestrial c sequestration",
  # -> terrestrial net primary production
  "terrestrial net primary production npp" =
    "terrestrial net primary production",
  "terrestrial net primary productivity, evapotranspiration" =
    "terrestrial net primary production",
  "terrestrial npp" =
    "terrestrial net primary production",
  "terrestrial npp (model ensemble)" =
    "terrestrial net primary production",
  "terrestrial npp (remote-sensing)" =
    "terrestrial net primary production",
  # -> terrestrial snow cover
  "snow cover" =
    "terrestrial snow cover",
  # -> terrestrial vegetation biomass
  "vegetation biomass" =
    "terrestrial vegetation biomass",
  "vegetation biomass (mechanistic models)" =
    "terrestrial vegetation biomass",
  # -> tide gauge
  "tide gauges" =
    "tide gauge",
  # -> total amount of funding for developing countries to promote the development, transfer, dis
  "finance mobilized for promoting the development, transfer, dissemination and diffusion of technology" =
    "total amount of funding for developing countries to promote the development, transfer, dissemination and diffusion of environmentally sound technologies",
  "total amount of approved funding for developing countries and countries with economies in transition to promote the development, transfer, dissemination and diffusion of environmentally sound technologies" =
    "total amount of funding for developing countries to promote the development, transfer, dissemination and diffusion of environmentally sound technologies",
  "total amount of approved funding for developing countries to promote the development, transfer, dissemination and diffusion of environmentally sound technologies" =
    "total amount of funding for developing countries to promote the development, transfer, dissemination and diffusion of environmentally sound technologies",
  # -> total government revenue as a proportion of gross domestic product, by source
  "total government revenue as a proportion of gdp, by source" =
    "total government revenue as a proportion of gross domestic product, by source",
  # -> total greenhouse gas emissions per year
  "total ghg emissions" =
    "total greenhouse gas emissions per year",
  # -> total hectares of ramsar sites that have been designated
  "total hectares of ramsar sites that have been designated. (data source: ramsar sites database)" =
    "total hectares of ramsar sites that have been designated",
  # -> total number of internationally recognized certificates of compliance published in the abs
  "total number of internationally recognized certificates published in the abs clearing-house" =
    "total number of internationally recognized certificates of compliance published in the abs clearing-house",
  # -> total number of introduced alien species
  "cumulative introduced invasive aliens" =
    "total number of introduced alien species",
  "cumulative number of alien species" =
    "total number of introduced alien species",
  # -> total richness of endemic species
  "endemic species richness" =
    "total richness of endemic species",
  # -> tourism direct gross domestic product as a proportion of total gross domestic product and 
  "tourism direct gdp as a proportion of total gdp and in growth rate" =
    "tourism direct gross domestic product as a proportion of total gross domestic product and in growth rate",
  # -> tree cover
  "area of tree cover" =
    "tree cover",
  # -> tree rings
  "tree ring reconstructions" =
    "tree rings",
  # -> trends in establishment and national adoption of international agreements relevant to the 
  "existence and uptake of multilateral agreements and national legislation relevant to the prevention and control of invasive alien species" =
    "trends in establishment and national adoption of international agreements relevant to the prevention and control of invasive alien species",
  "trends in establishments and national adoption of international agreements relevant to the prevention and control of invasive alien species" =
    "trends in establishment and national adoption of international agreements relevant to the prevention and control of invasive alien species",
  # -> trends in forest extent
  "extent of forested land" =
    "trends in forest extent",
  "extent of forests" =
    "trends in forest extent",
  "trends in forest extent (tree cover)" =
    "trends in forest extent",
  # -> trends in integration of migratory species concerns in national biodiversity strategies an
  "trends in attention to migratory species in national biodiversity strategies and action plans" =
    "trends in integration of migratory species concerns in national biodiversity strategies and action plans",
  # -> trends in nitrogen deposition
  "nitrogenen deposition trends" =
    "trends in nitrogen deposition",
  # -> trends in pathways of introduction and spread
  "spread of alien species by pathway" =
    "trends in pathways of introduction and spread",
  # -> trends in potentially environmentally harmful elements of government support to agricultur
  "trends in potentially harmful elements of government support to agriculture (produced support estimates)" =
    "trends in potentially environmentally harmful elements of government support to agriculture (producer support estimate)",
  # -> trends in successful control and eradications of invasive alien species
  "trends in successful eradications of invasive alien species" =
    "trends in successful control and eradications of invasive alien species",
  # -> trends in the number and value of government fossil-fuel support measures
  "trends in the number and value of government fossil fuel support measures" =
    "trends in the number and value of government fossil-fuel support measures",
  # -> trends in threats to migratory species
  "trends in threats to migratory species (overall)" =
    "trends in threats to migratory species",
  "trends in threats to migratory species (sub-indicators on specific threat types)" =
    "trends in threats to migratory species",
  # -> under-five child mortality rate
  "child mortality rate" =
    "under-five child mortality rate",
  "mortality rate under 5" =
    "under-five child mortality rate",
  "under-5 mortality rate" =
    "under-five child mortality rate",
  "under-five child mortality" =
    "under-five child mortality rate",
  "under-five mortality" =
    "under-five child mortality rate",
  "under-five mortality rate" =
    "under-five child mortality rate",
  # -> united nations framework convention on climate change (unfccc)
  "united nations framework convention on climate change" =
    "united nations framework convention on climate change (unfccc)",
  # -> value of merchandise being exported
  "merchandise exports" =
    "value of merchandise being exported",
  # -> value of subsidies and other incentives harmful to biodiversity
  "value of subsidies and other incentives harmful to biodiversity, that are redirected, repurposed or eliminated" =
    "value of subsidies and other incentives harmful to biodiversity",
  # -> visitation rates to natural terrestrial, coastal, and marine areas
  "daily exposure natural terrestrial, coastal and marine areas" =
    "visitation rates to natural terrestrial, coastal, and marine areas",
  "visitation to natural areas" =
    "visitation rates to natural terrestrial, coastal, and marine areas",
  # -> volcanic eruptions
  "volcanic activity" =
    "volcanic eruptions",
  # -> volume of production per labour unit by classes of farming/pastoral/forestry enterprise si
  "volume of production per labour unit by classes of farming/pastoral/ forestry enterprise size" =
    "volume of production per labour unit by classes of farming/pastoral/forestry enterprise size",
  # -> volume of remittances (in united states dollars) as a proportion of total gross domestic p
  "volume of remittances (in united states dollars) as a proportion of total gdp" =
    "volume of remittances (in united states dollars) as a proportion of total gross domestic product",
  # -> warm spell duration index (wsdi)
  "warm spell duration index: annual count of days with at least six consecutive days when tx >90th percentile" =
    "warm spell duration index (wsdi)",
  # -> water footprint
  "human appropriation of fresh water" =
    "water footprint",
  "human appropriation of fresh water (water footprint) (thousand km3)" =
    "water footprint",
  "water footprint (human appropriation of fresh water)" =
    "water footprint",
  # -> wetland extent trends index
  "westland extent trend index" =
    "wetland extent trends index",
  "wetland extent trend index" =
    "wetland extent trends index",
  # -> wild bird index (farmland birds)
  "farmland bird index" =
    "wild bird index (farmland birds)",
  # -> wild bird index (migratory birds)
  "wild bird index for migratory birds" =
    "wild bird index (migratory birds)",
  # -> willingness to allocate time for nature's contributions to people conservation
  "willingness-to-give-up-time" =
    "willingness to allocate time for nature's contributions to people conservation",
  # -> willingness to pay to protect nature or improve ecosystem services
  "willingness to pay for nature's contributions to people conservation" =
    "willingness to pay to protect nature or improve ecosystem services",
  "willingness-to-pay" =
    "willingness to pay to protect nature or improve ecosystem services",
  # -> world trade organisation greenbox agricultural subsidies
  "world trade organisation green box agricultural subsidies ($)" =
    "world trade organisation greenbox agricultural subsidies",
  # -> zoonotic disease
  "zoonotic disease in wildlife" =
    "zoonotic disease",
  # -> number of parties that are in category 1 under the national legislation project
  "proportion of countries that are category 1 signatories to the convention on international trade in endangered species of wild fauna and flora (cites)" =
    "number of parties that are in category 1 under the national legislation project",
  # -> number of surveys, studies or other analyses undertaken by exporting countries ... non-detriment findings
  "number of surveys of cites listed species undertaken by exporting states to support non-detrimental findings" =
    "number of surveys, studies or other analyses undertaken by exporting countries based on the sources of information cited in resolution conf. 16.7 (rev. cop17) on non-detriment findings related to - the population status of appendix-ii species; - the trends and impact of trade upon appendix-ii species; and - the status of and trend in naturally-occurring appendix i species and the impact of any recovery plans",
  # -> typos
  "hallow coral reefs" = "shallow coral reefs",
  "impacts of agricultural outpic prices on deforestation rates both inside and outside of protected areas" =
    "impacts of agricultural output prices on deforestation rates both inside and outside of protected areas",
  "water- saving devices" = "water-saving devices",
  "percentage of the population exposed to annual mean pm concentrations above the highest interim target of 35 μg/m" =
    "percentage of the population exposed to annual mean pm concentrations above the highest interim target of 35 μg/m3",
  "processes and tools to monitor the implementation of a right to a healthy environment (e.g. included in nbsaps and reported in national reports" =
    "processes and tools to monitor the implementation of a right to a healthy environment (e.g. included in nbsaps and reported in national reports)",
  # acronym-only names -> full name + acronym (your naming convention)
  "no emissions" = "nitric oxide (no) emissions",          # currently reads as "no emissions"
  "c pool" = "carbon (c) pool",
  "sh monsoons" = "southern hemisphere (sh) monsoons",
  # -> 1-in-100-year extreme total water level (etwl)
  "1-in-100-year return period level (etwl)" =
    "1-in-100-year extreme total water level (etwl)",
  # -> penman-monteith atmospheric evaporative demand (aed)
  "penman-monteith aed (atmospheric evaporative demand)" =
    "penman-monteith atmospheric evaporative demand (aed)",
  "number of studies on ias available per region" =
    "number of studies on invasive alien species (ias) available per region",
  # -> global atmospheric carbon monoxide (co) levels
  "global atmospheric co levels" =
    "global atmospheric carbon monoxide (co) levels",
  # -> pm10 emissions
  "pm 10 emissions" =
    "pm10 emissions",
  # -> species threat abatement and restoration (star) metric
  "species threat abatement and restoration metric" =
    "species threat abatement and restoration (star) metric"
  
)


#' Harmonise indicator names
#'
#' @param file_name data frame
#' @param field     column with the original indicator names (unquoted)
#' @param verbose   print a short report
#' @return file_name with a new column field_harm
harmonize_indic <- function(file_name = file_name,
                            field = field,
                            verbose = TRUE) {
  stopifnot(!any(duplicated(names(harm_lookup))))

  harmonized <- file_name %>%
    dplyr::mutate(field_harm = clean_indic_names({{field}}),
                  field_harm = dplyr::coalesce(unname(harm_lookup[field_harm]),
                                               field_harm))
  if (verbose) {
    orig <- clean_indic_names(dplyr::pull(file_name, {{field}}))
    found <- names(harm_lookup) %in% orig
    message(sum(found), " of ", length(harm_lookup),
            " lookup names found in the data; ",
            dplyr::n_distinct(orig), " distinct cleaned names -> ",
            dplyr::n_distinct(harmonized$field_harm), " harmonised names.")
    if (any(!found)) {
      message("Lookup names not found (fine for a subset of the data):\n  ",
              paste(head(names(harm_lookup)[!found], 20), collapse = "\n  "))
    }
  }
  harmonized
}

#' Paste the unique values of a column (split on ";"), dropping NA/empty
paste_unique <- function(x, sep = ";", sort = FALSE) {
  x <- unique(trimws(unlist(strsplit(as.character(x), sep, fixed = TRUE))))
  x <- x[!is.na(x) & x != "" & x != "NA"]
  if (sort) x <- sort(x)
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}

#' Collapse to one row per harmonised indicator
#'
#' Sources are sorted so the same combination is always written the same way.
#' Original names are joined with " | " because some contain ";".
collapse_indic <- function(harmonized) {
  harmonized %>%
    dplyr::group_by(field_harm) %>%
    dplyr::summarise(
      indicator_orig = paste(unique(trimws(indicator_orig)), collapse = " | "),
      indic_id       = paste_unique(indic_id),
      source         = paste_unique(source, sort = TRUE),
      n_sources      = length(unique(unlist(strsplit(source, ";")))),
      policy         = max(policy, na.rm = TRUE),
      .groups = "drop")
}

# Example ---------------------------------------------------------------------
# library(dplyr)
# all_orig  <- read.csv("all_indicators_orig.csv", encoding = "UTF-8")
# harm      <- harmonize_indic(all_orig, indicator_orig)
# harm_coll <- collapse_indic(harm)
