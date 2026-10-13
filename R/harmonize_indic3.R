# ---------------------------------------------------------------------------
# Harmonise indicator names that share the same meaning under different names
# Lookup tables built from the review of all_indicators_classified_20052025.csv
# (see similar_indicators.xlsx). Names on the left are replaced by the name
# on the right. Matching is on the WHOLE name (after trimming and lower-casing),
# so replacements never cascade into longer names (e.g. "agricultural land"
# will not touch "extent of agricultural land" or "area of agricultural land
# under conservation").
#
# Canonical name rule: the name used by most sources; ties go to the wording
# used by an MEA (GBF, SDG, CITES...), then to the longer, more descriptive
# name. A few were set by hand (typos, non-standard hyphens).
# ---------------------------------------------------------------------------

# Same metric, different wording ------------------------------------------
harm_same <- c(
  # -> proportion of population with access to electricity
  "access to electricity" =
    "proportion of population with access to electricity",
  # -> extent of agricultural land
  "agricultural land" =
    "extent of agricultural land",
  # -> proportion of population exposed to air pollution
  "air pollution exposure" =
    "proportion of population exposed to air pollution",
  # -> atlantic meridional overturning circulation (amoc)
  "amoc" =
    "atlantic meridional overturning circulation (amoc)",
  # -> annual total precipitation when the daily precipitation exceeds the 95
  "annual precipitation from daily precipitation > 95th percentile (r95p)" =
    "annual total precipitation when the daily precipitation exceeds the 95th percentile of the wet-day (>1 mm) precipitation",
  # -> total greenhouse gas emissions per year
  "anthropogenic emissions of long-lived greenhouse gases (co2 equivalents)" =
    "total greenhouse gas emissions per year",
  "anthropogenic emissions of long-lived greenhouse gases (ghg emissions)" =
    "total greenhouse gas emissions per year",
  "ghg emissions" =
    "total greenhouse gas emissions per year",
  "greenhouse gas emissions" =
    "total greenhouse gas emissions per year",
  # -> oecd better life index
  "better life index" =
    "oecd better life index",
  # -> biomass of prey fish
  "prey fish biomass" =
    "biomass of prey fish",
  # -> biotime species richness indicator
  "biotime-local species richness" =
    "biotime species richness indicator",
  "local species richness (biotime)" =
    "biotime species richness indicator",
  # -> per capita calorie intake
  "caloric intake" =
    "per capita calorie intake",
  # -> carbon uptake by land and ocean
  "land and ocean carbon sinks" =
    "carbon uptake by land and ocean",
  # -> changing status of evolutionary distinct and globally endangered speci
  "evolutionarily distinct and globally endangered (edge)" =
    "changing status of evolutionary distinct and globally endangered species (edge index)",
  # -> global climate risk index
  "climate risk index" =
    "global climate risk index",
  # -> maximum number of consecutive days with less than 1 mm of precipitatio
  "consecutive dry days (cdd)" =
    "maximum number of consecutive days with less than 1 mm of precipitation per day",
  "number of consecutive dry days (cdd)" =
    "maximum number of consecutive days with less than 1 mm of precipitation per day",
  # -> conservation status of species listed in the cites appendices has stabilized or improved 
  "the conservation status of species listed on the cites appendices has stabilized or improved"=
    "conservation status of species listed in the cites appendices has stabilized or improved",
  # -> cooling degree day index
  "cooling degree days" =
    "cooling degree day index",
  # -> protected area coverage of important sites for mountain biodiversity
  "coverage by protected areas of important sites for mountain biodiversity" =
    "protected area coverage of important sites for mountain biodiversity",
  # -> dollar value of financial and technical assistance (including through 
  "dollar value of financial and technical assistance (including through north-south, south\u2011south and triangular cooperation) committed to developing countries" =
    "dollar value of financial and technical assistance (including through north-south, south-south and triangular cooperation) committed to developing countries",
  "financial and technical assistance provided in dollars (including through south-south, north-south and triangular cooperation)" =
    "dollar value of financial and technical assistance (including through north-south, south-south and triangular cooperation) committed to developing countries",
  "united states dollar value of financial and technical assistance, including through north-south, south-south and triangular cooperation, committed to developing countries and countries with economies in transition" =
    "dollar value of financial and technical assistance (including through north-south, south-south and triangular cooperation) committed to developing countries",
  # -> total richness of endemic species
  "endemic species richness" =
    "total richness of endemic species",
  # -> exposure to pm concentrations (population-weighted annual mean concent
  "population-weighted annual mean concentration of pm2.5" =
    "exposure to pm concentrations (population-weighted annual mean concentration of pm2.5)",
  "population-weighted pm2.5 concentrations" =
    "exposure to pm concentrations (population-weighted annual mean concentration of pm2.5)",
  # -> trends in forest extent
  "extent of forests" =
    "trends in forest extent",
  # -> extent of suitable habitat index (esh)
  "extent of suitable habitat" =
    "extent of suitable habitat index (esh)",
  # -> wild bird index (farmland birds)
  "farmland bird index" =
    "wild bird index (farmland birds)",
  # -> fertilizer use
  "fertilizers used" =
    "fertilizer use",
  # -> level of water stress (freshwater withdrawal as a proportion of availa
  "freshwater withdrawal as a proportion of available freshwater resources" =
    "level of water stress (freshwater withdrawal as a proportion of available freshwater resources)",
  "level of water stress" =
    "level of water stress (freshwater withdrawal as a proportion of available freshwater resources)",
  # -> funding towards environmental education
  "funding for environmental education" =
    "funding towards environmental education",
  # -> funding towards environmental impact assessment
  "funding for environmental impact assessments" =
    "funding towards environmental impact assessment",
  # -> global temperature
  "global temperatures" =
    "global temperature",
  # -> national oceanic and atmospheric administration annual greenhouse gas 
  "greenhouse gas index (national oceanic and atmospheric administration)" =
    "national oceanic and atmospheric administration annual greenhouse gas index (aggi)",
  # -> highest five day precipitation amount (rx5day)
  "maximum five-day precipitation" =
    "highest five day precipitation amount (rx5day)",
  # -> highest one day precipitation amount (rx1day)
  "maximum one-day precipitation" =
    "highest one day precipitation amount (rx1day)",
  # -> human population density
  "population density" =
    "human population density",
  # -> human population size
  "population size" =
    "human population size",
  # -> ice core measurements
  "ice core records" =
    "ice core measurements",
  "ice cores" =
    "ice core measurements",
  # -> income
  "measure of income" =
    "income",
  # -> index of sustainable economic welfare (isew)
  "sustainable economic welfare (isew)" =
    "index of sustainable economic welfare (isew)",
  # -> indian ocean dipole index (iod)
  "indian ocean dipole (ipo)" =
    "indian ocean dipole index (iod)",
  # -> monetary benefits received in accordance with applicable international
  "indicator on monetary benefits received" =
    "monetary benefits received in accordance with applicable internationally agreed access and benefit-sharing instruments",
  # -> primary production of marine biota
  "marine net primary production" =
    "primary production of marine biota",
  # -> value of merchandise being exported
  "merchandise exports" =
    "value of merchandise being exported",
  # -> mortality rate attributed to unintentional poisoning
  "mortality from unintentional poisoning" =
    "mortality rate attributed to unintentional poisoning",
  # -> mortality rate attributed to household and ambient air pollution
  "mortality rate attributable to household and ambient air pollution" =
    "mortality rate attributed to household and ambient air pollution",
  # -> national recycling rate, tons of material recycled
  "recycling rate" =
    "national recycling rate, tons of material recycled",
  # -> trends in nitrogen deposition
  "nitrogenen deposition trends" =
    "trends in nitrogen deposition",
  # -> normalized difference vegetation index (ndvi)
  "time series of normalized difference vegetation index (ndvi)" =
    "normalized difference vegetation index (ndvi)",
  # -> number of parties that have adopted standard procedures for making non
  "number of cites parties that have adopted standard procedures for non-detrimental findings" =
    "number of parties that have adopted standard procedures for making non-detriment findings (ndfs)",
  # -> number of countries with biodiversity relevant tradable permit schemes
  "number of countries with national instruments on biodiversity relevant tradable permit schemes" =
    "number of countries with biodiversity relevant tradable permit schemes",
  # -> number of people who died or disappeared in the process of migration t
  "number of people who died or disappeared in the process of migration towards an international destinationi" =
    "number of people who died or disappeared in the process of migration towards an international destination",
  # -> number of protected areas that have completed a site-level assessment 
  "the number of protected areas that have completed a site-level assessment of governance and equity (sage)" =
    "number of protected areas that have completed a site-level assessment of governance and equity (sage)",
  # -> number of transboundary ramsar sites that have been designated
  "total number of transboundary ramsar sites" =
    "number of transboundary ramsar sites that have been designated",
  # -> ocean heat content
  "ocean heat content ohc" =
    "ocean heat content",
  # -> passenger and freight volumes, by mode of transport
  "passenger and freight volumes by mode of transport" =
    "passenger and freight volumes, by mode of transport",
  # -> proportion of cities with a direct participation structure of civil so
  "percentage of cities with direct participation structure of civil society in urban planning and management, which operate regularly and democratically" =
    "proportion of cities with a direct participation structure of civil society in urban planning and management that operate regularly and democratically",
  # -> proportion of population using safely managed drinking water services
  "percentage of population using safely managed drinking water services" =
    "proportion of population using safely managed drinking water services",
  # -> proportion of population who believe decision-making is inclusive and 
  "percentage of population who believe decision making is inclusive and responsive, by sex, age, disability and population group" =
    "proportion of population who believe decision-making is inclusive and responsive, by sex, age, disability and population group",
  # -> proportions of positions in national and local institutions, including
  "percentage of positions in national and local institutions, including (a) the legislatures; (b) the public service; and (c) the judiciary, compared to national distributions, by sex, age, persons with disabilities and population groups" =
    "proportions of positions in national and local institutions, including (a) the legislatures; (b) the public service; and (c) the judiciary, compared to national distributions, by sex, age, persons with disabilities and population groups",
  # -> pm2.5 emissions
  "pm 2.5 emissions" =
    "pm2.5 emissions",
  # -> proportion of terrestrial, freshwater and marine ecological regions wh
  "protected area coverage of terrestrial, marine and freshwater ecoregions" =
    "proportion of terrestrial, freshwater and marine ecological regions which are conserved by protected areas or other effective area-based conservation measures",
  # -> ratio of land consumption rate to population growth rate
  "ratio of land consumption rate to urban population growth rate at comparable scale" =
    "ratio of land consumption rate to population growth rate",
  # -> simple precipitation intensity index
  "simple daily intensity index (sdii)" =
    "simple precipitation intensity index",
  # -> soil organic carbon (soc) stocks
  "soil organic carbon" =
    "soil organic carbon (soc) stocks",
  # -> speleothem records
  "speleotherm records" =
    "speleothem records",
  # -> trends in integration of migratory species concerns in national biodiv
  "trends in attention to migratory species in national biodiversity strategies and action plans" =
    "trends in integration of migratory species concerns in national biodiversity strategies and action plans",
  # -> trends in potentially environmentally harmful elements of government s
  "trends in potentially harmful elements of government support to agriculture (produced support estimates)" =
    "trends in potentially environmentally harmful elements of government support to agriculture (producer support estimate)"
)

# Near-equivalent (only used with include_near = TRUE) ---------------------
harm_near <- c(
  # -> quality and abundance of food and medicine species
  "abundance of food species" =
    "quality and abundance of food and medicine species",
  # -> access to improved sanitation facilities
  "improved water supply and sanitation" =
    "access to improved sanitation facilities",
  # -> agrobiodiversity index
  "agro-biodiversity" =
    "agrobiodiversity index",
  # -> air pollution emissions account
  "air emission accounts" =
    "air pollution emissions account",
  # -> air quality index
  "air quality" =
    "air quality index",
  # -> alcohol per capita consumption (aged 15 years and older) within a cale
  "alcohol use" =
    "alcohol per capita consumption (aged 15 years and older) within a calendar year in litres of pure alcohol",
  # -> amount of funding provided through the global environment facility and
  "funding provided by the global environment facility" =
    "amount of funding provided through the global environment facility and allocated to the biodiversity focal area",
  # -> global surface air temperature (gsat)
  "annual gsat change" =
    "global surface air temperature (gsat)",
  "estimate of gsat (era-5)" =
    "global surface air temperature (gsat)",
  "mean gsat" =
    "global surface air temperature (gsat)",
  # -> arctic sea ice area
  "mean arctic sea ice area" =
    "arctic sea ice area",
  "shrinking of the arctic ice cover" =
    "arctic sea ice area",
  # -> atmospheric dust deposition
  "global dust deposition" =
    "atmospheric dust deposition",
  # -> penman-monteith aed (atmospheric evaporative demand)
  "atmospheric evaporative demand (aed)" =
    "penman-monteith aed (atmospheric evaporative demand)",
  # -> average share of the built-up area of cities that is open space for pu
  "average share of the built-up area of cities that is green/blue space for public use for all" =
    "average share of the built-up area of cities that is open space for public use for all, by sex, age and persons with disabilities",
  # -> drought severity index
  "change in # droughts" =
    "drought severity index",
  "drought index (di)" =
    "drought severity index",
  "droughts" =
    "drought severity index",
  # -> change in # soil moisture
  "changes in soil moisture" =
    "change in # soil moisture",
  "soil moisture" =
    "change in # soil moisture",
  "total soil moisture" =
    "change in # soil moisture",
  # -> change in the extent of water-related ecosystems over time
  "change in the extent of inland water ecosystems over time" =
    "change in the extent of water-related ecosystems over time",
  # -> changes in mean precipitation
  "changes in precipitation" =
    "changes in mean precipitation",
  "precipitation" =
    "changes in mean precipitation",
  # -> under-five child mortality rate
  "child mortality" =
    "under-five child mortality rate",
  "child mortality rate" =
    "under-five child mortality rate",
  "mortality rate under 5" =
    "under-five child mortality rate",
  # -> number of parties that have implemented relevant reporting under resol
  "cites parties that have implemented relevant resolutions and decisions" =
    "number of parties that have implemented relevant reporting under resolutions and decisions of the conference of the parties and/or standing committee recommendations",
  # -> continuous global mangrove forest area
  "global cover of mangroves" =
    "continuous global mangrove forest area",
  "mangrove forest area" =
    "continuous global mangrove forest area",
  "trends in mangrove extent" =
    "continuous global mangrove forest area",
  # -> extent to which protected areas and other effective area-based conserv
  "coverage of protected areas and other effective area-based conservation measures" =
    "extent to which protected areas and other effective area-based conservation measures cover key biodiversity areas that are important for migratory species",
  "coverage of protected areas and other effective area-based conservation measures and traditional territories (by governance type)" =
    "extent to which protected areas and other effective area-based conservation measures cover key biodiversity areas that are important for migratory species",
  # -> protected area coverage of marine and coastal areas
  "coverage of protected areas in relation to marine areas" =
    "protected area coverage of marine and coastal areas",
  # -> current distribution of species
  "distribution of species" =
    "current distribution of species",
  "spatial distribution of species" =
    "current distribution of species",
  # -> visitation rates to natural terrestrial, coastal, and marine areas
  "daily exposure natural terrestrial, coastal and marine areas" =
    "visitation rates to natural terrestrial, coastal, and marine areas",
  "visitation to natural areas" =
    "visitation rates to natural terrestrial, coastal, and marine areas",
  # -> mortality rate attributed to unsafe water, unsafe sanitation and lack 
  "deaths due to unsafe water" =
    "mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (exposure to unsafe water, sanitation and hygiene for all (wash) services)",
  # -> deforestation rate
  "deforestation" =
    "deforestation rate",
  # -> number of countries implementing sustainable public procurement polici
  "degree of sustainable public procurement policies and action plan implementation" =
    "number of countries implementing sustainable public procurement policies and action plans",
  # -> direct economic loss attributed to disasters in relation to global gro
  "direct economic loss in relation to global gross domestic product, damage to critical infrastructure and number of disruptions to basic services, attributed to disasters" =
    "direct economic loss attributed to disasters in relation to global gross domestic product",
  # -> domestic material consumption, domestic material consumption, and dome
  "domestic material consumption" =
    "domestic material consumption, domestic material consumption, and domestic material consumption per gross domestic product",
  # -> energy intensity measured in terms of primary energy and gross domesti
  "energy-intensity" =
    "energy intensity measured in terms of primary energy and gross domestic product",
  # -> potential evaporation, evaporative demand drought index
  "evaporative demand drought index (eddi)" =
    "potential evaporation, evaporative demand drought index",
  # -> mean polar sea ice extent
  "extent of ice cover" =
    "mean polar sea ice extent",
  "sea ice extent" =
    "mean polar sea ice extent",
  # -> number of species at risk of extinction
  "extinction risk" =
    "number of species at risk of extinction",
  # -> red list index (wild relatives of farmed and domesticated species)
  "extinction risk of wild relatives of domesticated or farmed birds and mammals" =
    "red list index (wild relatives of farmed and domesticated species)",
  # -> total amount of funding for developing countries to promote the develo
  "finance mobilized for promoting the development, transfer, dissemination and diffusion of technology" =
    "total amount of funding for developing countries to promote the development, transfer, dissemination and diffusion of environmentally sound technologies",
  # -> flood index (fi)
  "floods" =
    "flood index (fi)",
  # -> glacier mass balance
  "glacier areal and mass changes/loss rate" =
    "glacier mass balance",
  "glacier mass loss" =
    "glacier mass balance",
  # -> linguistic diversity index
  "global linguistic diversity index and language threat level" =
    "linguistic diversity index",
  # -> global mean ocean surface ph
  "ocean ph" =
    "global mean ocean surface ph",
  "ph of ocean surface water" =
    "global mean ocean surface ph",
  # -> sea level
  "global mean sea level (gmsl)" =
    "sea level",
  "increase in the global mean sea level" =
    "sea level",
  "rate of rslr (relative sea level rise)" =
    "sea level",
  "sea level change" =
    "sea level",
  "sea level rise" =
    "sea level",
  # -> global seagrass extent (seagrass cover and composition)
  "seagrass meadow area" =
    "global seagrass extent (seagrass cover and composition)",
  # -> quality of governance index
  "governance index" =
    "quality of governance index",
  # -> gross national happiness index of bhutan
  "gross national happiness index" =
    "gross national happiness index of bhutan",
  # -> live coral cover
  "hard coral cover and composition" =
    "live coral cover",
  "percentage of live coral cover" =
    "live coral cover",
  # -> integration of biodiversity into national accounting and reporting sys
  "number of countries implementing the system of environmental economic accounting" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting",
  "number of countries that have integrated biodiversity values into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting (seea)" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting",
  "number of countries with implementation of the system of environmental economic accounting" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting",
  "united nations system of environmental economic accounting" =
    "integration of biodiversity into national accounting and reporting systems, defined as implementation of the system of environmental-economic accounting",
  # -> official development assistance on conservation and sustainable use of
  "international public funding, including official development assistance (oda) for conservation and sustainable use of biodiversity and ecosystems" =
    "official development assistance on conservation and sustainable use of biodiversity",
  # -> rate of invasive species impact and rate of impact
  "invasive alien species impact" =
    "rate of invasive species impact and rate of impact",
  "trends in numbers and spatial distribution of invasive alien species and their impacts" =
    "rate of invasive species impact and rate of impact",
  "trends in numbers of invasive alien species and their impacts" =
    "rate of invasive species impact and rate of impact",
  # -> land use change
  "land-use change" =
    "land use change",
  "rate of land use change" =
    "land use change",
  # -> reduced malnutrition
  "malnutrition" =
    "reduced malnutrition",
  # -> marine sediment cores
  "marine sediments" =
    "marine sediment cores",
  # -> marine stewardship council certified catch
  "marine stewardship council certified fisheries" =
    "marine stewardship council certified catch",
  # -> material footprint, material footprint per capita, and material footpr
  "material footprint per capita" =
    "material footprint, material footprint per capita, and material footprint per gross domestic product",
  # -> mean annual temperature
  "mean air temperature" =
    "mean annual temperature",
  # -> proportion of municipal solid waste collected and managed in controlle
  "municipal solid waste collected and managed" =
    "proportion of municipal solid waste collected and managed in controlled facilities out of total municipal waste generated, by cities",
  # -> number of plant and animal genetic resources for food and agriculture 
  "number of (a) plant and (b) animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities" =
    "number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities",
  "number of plant and animal genetic resources secured in medium or long-term conservation facilities" =
    "number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities",
  # -> proportion of countries adopting relevant national legislation and ade
  "number of countries implementing national legislation, policies or other measures regarding fpic related to conservation" =
    "proportion of countries adopting relevant national legislation and adequately resourcing the prevention or control of invasive alien species",
  "percentage of countries with invasive alien species legislation" =
    "proportion of countries adopting relevant national legislation and adequately resourcing the prevention or control of invasive alien species",
  "trends in numbers of countries with national legislation and other policy measures relevant to the prevention and/or control of invasive alien species" =
    "proportion of countries adopting relevant national legislation and adequately resourcing the prevention or control of invasive alien species",
  # -> percentage of countries that have ratified the nagoya protocol
  "number of countries that have ratified the nagoya protocol" =
    "percentage of countries that have ratified the nagoya protocol",
  # -> proportion of countries where the legal framework (including customary
  "number of countries where the legal framework (including customary law) guarantees women's equal rights to land ownership and/or control" =
    "proportion of countries where the legal framework (including customary law) guarantees women equal rights to land ownership and/or control",
  # -> number of countries with biodiversity relevant charges and fees
  "number of countries with biodiversity relevant taxes" =
    "number of countries with biodiversity relevant charges and fees",
  "number of countries with national instruments on biodiversity relevant taxes, charges and fees" =
    "number of countries with biodiversity relevant charges and fees",
  # -> number of countries with developed or revised nbsaps
  "percentage of countries with revised nbsaps" =
    "number of countries with developed or revised nbsaps",
  # -> proportion of countries with migration policies that facilitate orderl
  "number of countries with migration policies that facilitate orderly, safe, regular and responsible migration and mobility of people" =
    "proportion of countries with migration policies that facilitate orderly, safe, regular and responsible migration and mobility of people",
  # -> number of countries with regulations requiring recovered of depleted s
  "presence of regulations requiring recovery of depleted species" =
    "number of countries with regulations requiring recovered of depleted species",
  # -> proportion of countries with systems to track and make public allocati
  "number of countries with systems to track and make public allocations for gender equality and women's empowerment" =
    "proportion of countries with systems to track and make public allocations for gender equality and women\u2019s empowerment",
  # -> number of invasive alien species introduction events
  "trends in the numbers of invasive alien species introduction events" =
    "number of invasive alien species introduction events",
  # -> number of parties that are in category 1 under the national legislatio
  "percentage of countries that are category 1 cites parties" =
    "number of parties that are in category 1 under the national legislation project",
  # -> number of permits or their equivalents for genetic resources (includin
  "total number of permits, or their equivalent, granted for access to genetic resources" =
    "number of permits or their equivalents for genetic resources (including those related to traditional knowledge) by type of permit",
  # -> protected area management effectiveness (pame)
  "number of protected area management effectiveness (pame) assessments" =
    "protected area management effectiveness (pame)",
  "protected area and oecm management effectiveness (mepca) indicator" =
    "protected area management effectiveness (pame)",
  # -> numbers of scientific publications on biodiversity
  "number of publications" =
    "numbers of scientific publications on biodiversity",
  # -> number of ramsar sites reported by parties to the ramsar secretariat o
  "percentage of parties reporting to the ramsar secretariat all cases of negative human-induced change or likely change in the ecological character of ramsar sites pursuant to article 3.2" =
    "number of ramsar sites reported by parties to the ramsar secretariat of negative human-induced change or likely change in the ecological character of ramsar sites pursuant to article 3.2",
  # -> ocean acidification
  "reduced ocean acidification" =
    "ocean acidification",
  # -> ocean salinity
  "salinity" =
    "ocean salinity",
  # -> overall species abundance
  "species abundance" =
    "overall species abundance",
  # -> pesticide use per area of cropland
  "pesticide use" =
    "pesticide use per area of cropland",
  "rate of use of pesticides" =
    "pesticide use per area of cropland",
  "trends in pesticide use" =
    "pesticide use per area of cropland",
  # -> proportion of the population who are refugees, by country of origin
  "refugee population" =
    "proportion of the population who are refugees, by country of origin",
  # -> proportion of urban population
  "urban population" =
    "proportion of urban population",
  # -> rate of wood removal
  "total wood removals" =
    "rate of wood removal",
  # -> simulated global mean surface air temperature (gsat) anomalies
  "simulated global mean surface air temperature (gsat)" =
    "simulated global mean surface air temperature (gsat) anomalies",
  # -> terrestrial snow cover
  "snow cover" =
    "terrestrial snow cover",
  # -> species occurrence records accessible through the global biodiversity 
  "species with data included in the global biodiversity information facility" =
    "species occurrence records accessible through the global biodiversity information facility",
  # -> trends in pathways of introduction and spread
  "spread of alien species by pathway" =
    "trends in pathways of introduction and spread",
  # -> terrestrial vegetation biomass
  "vegetation biomass" =
    "terrestrial vegetation biomass",
  # -> total official international support (official development assistance 
  "total official flows (official development assistance plus other official flows) to the agriculture sector" =
    "total official international support (official development assistance plus other official flows) to infrastructure",
  "trends in international bilateral and multilateral official development assistance" =
    "total official international support (official development assistance plus other official flows) to infrastructure",
  # -> trends in successful control and eradications of invasive alien specie
  "trends in successful eradications of invasive alien species" =
    "trends in successful control and eradications of invasive alien species",
  # -> volcanic eruptions
  "volcanic activity" =
    "volcanic eruptions",
  # -> water quality index (watqi)
  "water quality" =
    "water quality index (watqi)",
  # -> odss and ghgs
  "odss and ghgs" = 
    "ozone depleting substances and greenhouse gases"
)

#' Harmonise indicator names
#'
#' @param file_name     data frame with the indicator names
#' @param field         column with the names (unquoted, as in harmonize_indic2)
#' @param include_near  also merge near-equivalent names (same underlying
#'                      measure, different unit or framing, e.g. number vs
#'                      proportion of countries). FALSE by default.
#' @return file_name with a new column field_harm
harmonize_indic3 <- function(file_name = file_name,
                             field = field,
                             include_near = FALSE,
                             verbose = TRUE){
  lookup <- harm_same
  if (include_near) lookup <- c(lookup, harm_near)
  
  # lower case, trim, and turn any run of spaces (incl. non-breaking spaces
  # and tabs) into a single normal space, on both the data and the lookup
  clean <- function(x) trimws(gsub("[[:space:]\u00a0]+", " ", tolower(as.character(x))))
  names(lookup) <- clean(names(lookup))
  
  harmonized = file_name %>%
    dplyr::mutate(field_harm = clean({{field}}),
                  field_harm = dplyr::coalesce(unname(lookup[field_harm]),
                                               field_harm)) 
    
  # checks
  if (verbose) {
    found <- names(lookup) %in% clean(dplyr::pull(file_name, {{field}}))
    message(sum(found), " of ", length(lookup), " lookup names found and replaced.")
    if (any(!found)) {
      message("Not found in the data (check spelling or hidden characters):\n  ",
              paste(names(lookup)[!found], collapse = "\n  "))
    }
  }
  return(harmonized)
}


# harmonize_indic3 <- function(file_name = file_name,
#                              field = field,
#                              include_near = FALSE){
#   lookup <- harm_same
#   if (include_near) lookup <- c(lookup, harm_near)
# 
#   harmonized = file_name %>%
#     dplyr::mutate(field_harm = tolower(trimws({{field}})),
#                   field_harm = dplyr::coalesce(unname(lookup[field_harm]),
#                                                field_harm)) %>% 
#     dplyr::mutate(field_harm = gsub('ghg', 'greenhouse gas',{{field}}))
#     
#   return(harmonized)
# }


