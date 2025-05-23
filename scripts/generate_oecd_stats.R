library(tidyverse)
library(countrycode)

# WORLD           COAL            1960            INDPROD         KTOE                              ..
# WORLD           COAL            1960            INDPROD         TJ                                ..
# WORLD           COAL            1960            IMPORTS         KTOE                              ..
# WORLD           COAL            1960            IMPORTS         TJ                                ..
# WORLD           COAL            1960            EXPORTS         KTOE                              ..
wb1 <- read.table("~/Downloads/WORLDBIG1.txt",
                 col.names = c("COUNTRY", "PRODUCT", "year",
                               "FLOW", "unit", "value"))
wb2 <- read.table("~/Downloads/WORLDBIG2.txt",
                 col.names = c("COUNTRY", "PRODUCT", "year",
                               "FLOW", "unit", "value"))

wb <- bind_rows(wb1, wb2)

wb <- wb %>%
    filter(unit=="TJ")

# rm(wb1, wb2)
# KJKG            AUSTRALI        HARDCOAL        NAVERAGE        1960                     26000.00000
# KJKG            AUSTRALI        HARDCOAL        NAVERAGE        1961                     26000.00000
# KJKG            AUSTRALI        HARDCOAL        NAVERAGE        1962                     26000.00000
# KJKG            AUSTRALI        HARDCOAL        NAVERAGE        1963                     26000.00000
# KJKG            AUSTRALI        HARDCOAL        NAVERAGE        1964                     26000.00000
conv_raw <- read.table('~/Downloads/CONV.TXT',
                  col.names = c("unit", "COUNTRY", "PRODUCT", "FLOW", "year", "value"))

template <- read_csv('input/energy/OECD_and_NonOECD_E_Stat-template.csv')

# Convert flow
# Doing nothing for now
unique(template$FLOW)
unique(wb$FLOW)
intersect(unique(template$FLOW), unique(wb$FLOW))
setdiff(unique(template$FLOW), unique(wb$FLOW))
setdiff(unique(wb$FLOW), unique(template$FLOW))


# Convert country
unique(template$COUNTRY)
unique(wb$COUNTRY)
corr_country <- read_csv('input/mappings/Master_Country_List.csv')

# Remove those in composite and add composite directly instead
FSU_IEA_composite_name <- "Former Soviet Union (if no detail)"
FYUG_IEA_composite_name <- "Former Yugoslavia (if no detail)"
Other_African_composite_name <- "Other Africa"
Other_Americas_composite_name <- "Other non-OECD Americas"
Other_Asia_composite_name <- "Other non-OECD Asia"
composite_names <- c(
    FSU_IEA_composite_name,
    FYUG_IEA_composite_name,
    Other_African_composite_name,
    Other_Americas_composite_name,
    Other_Asia_composite_name
)
corr_country_single <- corr_country %>%
    filter(!IEAName %in% composite_names)



# From CEDS
wb$iso <- tolower(countrycode(wb$COUNTRY, "country.name", "iso3c",
                              custom_match = c(
                                  "AUSTRALI" = "AUS",
                                  "CZECH" = "CZE",
                                  "DOMINICANR" = "DOM",
                                  "KOREADPR" = "PRK",
                                  "KOSOVO" = "srb (kosovo)",  # Note: Kosovo uses a temporary code
                                  "NETHLAND" = "NLD",
                                  "MMALI" = "MLI",
                                  "MMAURITANI" = "MRT",
                                  "NORTHMACED" = "MKD",
                                  "MCHAD" = "TCD",
                                  "NZ" = "NZL",
                                  "PHILIPPINE" = "PHL",
                                  "SAUDIARABI" = "SAU",
                                  "SOUTHAFRIC" = "ZAF",
                                  "SWITLAND" = "CHE",
                                  "CHINAREG" = NA,
                                  "CONGOREP" = "COG",
                                  "CONGO" = "COD"
                              )))
wb$COUNTRY_FOR_CEDS <- corr_country$IEAName[match(wb$iso, corr_country$iso)]




wb <- wb[!wb$COUNTRY_FOR_CEDS %in% composite_names, ]


"FSUND" = FSU_IEA_composite_name
"YUGOND" = FYUG_IEA_composite_name
"OTHERAFRIC" = Other_African_composite_name
"OTHERLATIN" = Other_Americas_composite_name
"OTHERASIA" = Other_Asia_composite_name

wb$COUNTRY_FOR_CEDS[wb$COUNTRY == "FSUND"] <- FSU_IEA_composite_name
wb$COUNTRY_FOR_CEDS[wb$COUNTRY == "YUGOND"] <- FYUG_IEA_composite_name
wb$COUNTRY_FOR_CEDS[wb$COUNTRY == "OTHERAFRIC"] <- Other_African_composite_name
wb$COUNTRY_FOR_CEDS[wb$COUNTRY == "OTHERLATIN"] <- Other_Americas_composite_name
wb$COUNTRY_FOR_CEDS[wb$COUNTRY == "OTHERASIA"] <- Other_Asia_composite_name

# Composite regions: keep those we're iterested in

wb <- wb %>% filter(
    !is.na(COUNTRY_FOR_CEDS)
)



# Convert product
unique(wb$PRODUCT)
unique(template$PRODUCT)
corr_product <- tribble(
    ~PRODUCT, ~PRODUCT_FOR_CEDS, ~UNIT,
    "HARDCOAL", "Hard coal (if no detail) (kt)", "kt",
    "BROWN", "Brown coal (if no detail) (kt)", "kt",
    "ANTCOAL", "Anthracite (kt)", "kt",
    "COKCOAL", "Coking coal (kt)", "kt",
    "BITCOAL", "Other bituminous coal (kt)", "kt",
    "SUBCOAL", "Sub-bituminous coal (kt)", "kt",
    "LIGNITE", "Lignite (kt)", "kt",
    "PATFUEL", "Patent fuel (kt)", "kt",
    "OVENCOKE", "Coke oven coke (kt)", "kt",
    "GASCOKE", "Gas coke (kt)", "kt",
    "COALTAR", "Coal tar (kt)", "kt",
    "BKB", "BKB (kt)", "kt",
    "GASWKSGS", "Gas works gas (TJ-gross)", "TJ",
    "COKEOVGS", "Coke oven gas (TJ-gross)", "TJ",
    "BLFURGS", "Blast furnace gas (TJ-gross)", "TJ",
    "OGASES", "Other recovered gases (TJ-gross)", "TJ",
    "PEAT", "Peat (kt)", "kt",
    "PEATPROD", "Peat products (kt)", "kt",
    "OILSHALE", "Oil shale and oil sands (kt)", "kt",
    "MANGAS", "Elec/heat output from non-specified manufactured gases", "TJ",
    "INDWASTE", "Industrial waste (TJ-net)", "TJ",
    "MUNWASTER", "Municipal waste (renewable) (TJ-net)", "TJ",
    "MUNWASTEN", "Municipal waste (non-renewable) (TJ-net)", "TJ",
    "PRIMSBIO", "Primary solid biofuels (TJ-net)", "TJ",
    "BIOGASES", "Biogases (TJ-net)", "TJ",
    "BIOGASOL", "Biogasoline (kt)", "kt",
    "BIODIESEL", "Biodiesels (kt)", "kt",
    "OBIOLIQ", "Other liquid biofuels (kt)", "kt",
    "RENEWNS", "Non-specified primary biofuels/waste (TJ-net)", "TJ",
    "CHARCOAL", "Charcoal (kt)", "kt",
    "NATGAS", "Natural gas (TJ-gross)", "TJ",
    "CRNGFEED", "Crude/NGL/feedstocks (if no detail) (kt)", "kt",
    "CRUDEOIL", "Crude oil (kt)", "kt",
    "NGL", "Natural gas liquids (kt)", "kt",
    "REFFEEDS", "Refinery feedstocks (kt)", "kt",
    "ADDITIVE", "Additives/blending components (kt)", "kt",
    "NONCRUDE", "Other hydrocarbons (kt)", "kt",
    "REFINGAS", "Refinery gas (kt)", "kt",
    "ETHANE", "Ethane (kt)", "kt",
    "LPG", "Liquefied petroleum gases (LPG) (kt)", "kt",
    "NONBIOGASO", "Motor gasoline excl. biofuels (kt)", "kt",
    "AVGAS", "Aviation gasoline (kt)", "kt",
    "JETGAS", "Gasoline type jet fuel (kt)", "kt",
    "NONBIOJETK", "Kerosene type jet fuel excl. biofuels (kt)", "kt",
    "OTHKERO", "Other kerosene (kt)", "kt",
    "NONBIODIES", "Gas/diesel oil excl. biofuels (kt)", "kt",
    "RESFUEL", "Fuel oil (kt)", "kt",
    "NAPHTHA", "Naphtha (kt)", "kt",
    "WHITESP", "White spirit & SBP (kt)", "kt",
    "LUBRIC", "Lubricants (kt)", "kt",
    "BITUMEN", "Bitumen (kt)", "kt",
    "PARWAX", "Paraffin waxes (kt)", "kt",
    "PETCOKE", "Petroleum coke (kt)", "kt",
    "ONONSPEC", "Other oil products (kt)", "kt", # Note: This is not in CEDS
    "HEATNS", "Heat output from non-specified combustible fuels", NA,
    "NUCLEAR", "Nuclear", NA,
    "HYDRO", "Hydro", NA,
    "GEOTHERM", "Geothermal (direct use in TJ-net)", NA,
    "SOLARPV", "Solar photovoltaics", NA,
    "SOLARTH", "Solar thermal (direct use in TJ-net)", NA,
    "TIDE", "Tide, wave and ocean", NA,
    "WIND", "Wind", NA,
    "OTHER", "Other sources", NA,
    "ELECTR", "Electricity (GWh)", "GWh",
    "HEAT", "Heat (TJ)", "TJ",
    "TOTAL", NA, NA,
    "MRENEW", NA, NA,
    "BIOJETKERO", NA, NA
)

# Ensure no duplicates in either PRODUCT or PRODUCT_FOR_CEDS coliumns
stopifnot(!any(duplicated(corr_product$PRODUCT)))
stopifnot(!any(duplicated(corr_product$PRODUCT_FOR_CEDS[!is.na(corr_product$PRODUCT_FOR_CEDS)])))

wb <- wb %>%
    left_join(corr_product)

# wb %>%
    # distinct(PRODUCT, PRODUCT_FOR_CEDS)

wb <- wb %>%
    filter(!is.na(PRODUCT_FOR_CEDS))

# Converting to the right unit
ncv <- conv_raw %>%
    mutate(value = as.numeric(value)) %>%
    filter(FLOW=="NAVERAGE",
           unit=="KJKG") %>%
    select(COUNTRY, PRODUCT, year, ncv_kj_kg=value)

TJ_to_GWh <- 1 / 3.6

wb <- wb %>%
    mutate(value = as.numeric(value)) %>%
    left_join(ncv) %>%
    mutate(
        conversion_factor = case_when(
            UNIT == "kt" ~ 1 / ncv_kj_kg * 1e3, # convert from TH to kt
            UNIT == "GWh" ~ TJ_to_GWh,
            UNIT == "TJ" ~ 1
        )
    ) %>%
    mutate(value = as.numeric(value) * conversion_factor)


# Keep only relevant columns
wb <- wb %>%
    select(COUNTRY_FOR_CEDS, PRODUCT_FOR_CEDS, FLOW, year, value)


# Aggregate by groups (CEDS want uses IEA groups??)
# wb <- wb %>%
#     group_by(COUNTRY_FOR_CEDS, PRODUCT_FOR_CEDS, FLOW, year) %>%
#     summarise(value = sum(value, na.rm=T)) %>%
#     ungroup()

# Format as template
# A tibble: 272,136 × 62
# COUNTRY FLOW     PRODUCT    `1960` `1961` `1962` `1963` `1964` `1965` `1966` `1967` `
wb %>%
    pivot_wider(names_from = year, values_from = value) %>%
    select(COUNTRY=COUNTRY_FOR_CEDS, FLOW, PRODUCT=PRODUCT_FOR_CEDS, everything()) %>%
    write_csv("input/energy/OECD_and_NonOECD_E_Stat.csv")

# Check multiple rows
wb %>%
    group_by(COUNTRY_FOR_CEDS, PRODUCT_FOR_CEDS, FLOW, year) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    filter(n>1) -> x


# Conversion factors ------------------------------------------------------
# Write conv
template_conv <- read_csv('input/energy/OECD_and_NonOECD_Conversion_Factors_Full-template.csv')
# # A tibble: 105,952 × 62
# COUNTRY FLOW  PRODUCT X1960 X1961 X1962 X1963 X1964 X1965 X1966 X1967 X1968 X1969 X1970 X1971 X1972 X1973 X1974 X1975 X1976 X1977 X1978 X1979 X1980 X1981 X1982 X1983 X1984 X1985 X1986 X1987 X1988
# <chr>   <chr> <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <dbl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl>
#     1 Albania Aver… Hard c… ..    ..    ..    ..    ..    ..    ..    ..    ..    ..    ..    21600 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA

unique(template_conv$COUNTRY)
corr_country <- read_csv('input/mappings/Master_Country_List.csv')
conv <- conv_raw %>%
    filter(unit=="KJKG")

conv$iso <- tolower(countrycode(conv$COUNTRY, "country.name", "iso3c",
                              custom_match = c(
                                  "CONGO" = "COD",
                                  "CONGOREP" = "COG",
                                  "AUSTRALI" = "AUS",
                                  "CZECH" = "CZE",
                                  "DOMINICANR" = "DOM",
                                  "KOREADPR" = "PRK",
                                  "KOSOVO" = "srb (kosovo)",  # Note: Kosovo uses a temporary code
                                  "NETHLAND" = "NLD",
                                  "MMALI" = "MLI",
                                  "MMAURITANI" = "MRT",
                                  "NORTHMACED" = "MKD",
                                  "MCHAD" = "TCD",
                                  "NZ" = "NZL",
                                  "PHILIPPINE" = "PHL",
                                  "SAUDIARABI" = "SAU",
                                  "SOUTHAFRIC" = "ZAF",
                                  "SWITLAND" = "CHE"
                              )))
conv$COUNTRY_FOR_CEDS <- corr_country$Country_Name[match(conv$iso, corr_country$iso)]
conv <- conv %>%
    filter(
        !is.na(iso)
    )
# Product
unique(conv$PRODUCT)
unique(template_conv$PRODUCT)


corr_conv_units <- tribble(
    ~PRODUCT, ~PRODUCT_FOR_CEDS,
    "HARDCOAL", "Hard coal (if no detail)",
    "BROWN", "Brown coal (if no detail)",
    "ANTCOAL", "Anthracite",
    "COKCOAL", "Coking coal",
    "BITCOAL", "Other bituminous coal",
    "SUBCOAL", "Sub-bituminous coal",
    "LIGNITE", "Lignite",
    "PATFUEL", "Patent fuel",
    "OVENCOKE", "Coke oven coke",
    "GASCOKE", "Gas coke",
    "COALTAR", "Coal tar",
    "BKB", "BKB",
    "PEAT", "Peat",
    "PEATPROD", "Peat products",
    "OILSHALE", "Oil shale and oil sands",
    "CRNGFEED", "Crude/NGL/feedstocks/non-crude (if no detail)",
    "CRUDEOIL", "Crude oil",
    "NGL", "Natural gas liquids",
    "REFFEEDS", "Refinery feedstocks",
    "ADDITIVE", "Additives/blending components",
    "NONCRUDE", "Other hydrocarbons",
    "REFINGAS", "Refinery gas",
    "ETHANE", "Ethane",
    "LPG", "Liquefied petroleum gases (LPG)",
    "NONBIOGASO", "Motor gasoline excl. biofuels",
    "AVGAS", "Aviation gasoline",
    "JETGAS", "Gasoline type jet fuel",
    "NONBIOJETK", "Kerosene type jet fuel excl. biofuels",
    "OTHKERO", "Other kerosene",
    "NONBIODIES", "Gas/diesel oil excl. biofuels",
    "RESFUEL", "Fuel oil",
    "NAPHTHA", "Naphtha",
    "WHITESP", "White spirit & SBP",
    "LUBRIC", "Lubricants",
    "BITUMEN", "Bitumen",
    "PARWAX", "Paraffin waxes",
    "PETCOKE", "Petroleum coke",
    "ONONSPEC", "Other oil products",
    "BIOGASOL", "Biogasoline",
    "BIODIESEL", "Biodiesels",
    "BIOJETKERO", "Bio jet kerosene",
    "OBIOLIQ", "Other liquid biofuels",
    "CHARCOAL", "Charcoal",
    "NATGAS", NA
)


conv <- conv %>%
    left_join(corr_conv_units)

corr_conv_flow <- tribble(
    ~FLOW, ~FLOW_FOR_CEDS,
    "NAVERAGE", "Average net calorific value",
    "NINDPROD", "NCV of production",
    "NOSOURCES", "NCV of other sources",
    "NIMPORTS", "NCV of imports",
    "NEXPORTS", "NCV of exports",
    "NCOKEOVS", "NCV of coke ovens",
    "NBLAST", "NCV of blast furnaces",
    "NMAIN", "NCV in main activity producer electricity plants",
    "NAUTOELEC", "NCV in autoproducer electricity plants",
    "NMAINCHP", "NCV in main activity CHP plants",
    "NAUTOCHP", "NCV in autoproducer CHP plants",
    "NMAINHEAT", "NCV in main activity heat plants",
    "NAUTOHEAT", "NCV in autoproducer heat plants",
    "NIND", "NCV in industry",
    "NOTHER", "NCV for other uses",
    "BBLTONRATIO", "Volume to mass ratio",
    "GINDPROD", NA,
    "GIMPORTS", NA,
    "GEXPORTS", NA,
    "GFINCONS", NA
)

conv <- conv %>%
    left_join(corr_conv_flow)

conv <- conv %>%
    filter(
        !is.na(FLOW_FOR_CEDS),
        !is.na(PRODUCT_FOR_CEDS)
    )

# Select columns and write
conv %>%
    select(COUNTRY=COUNTRY_FOR_CEDS, FLOW=FLOW_FOR_CEDS, PRODUCT=PRODUCT_FOR_CEDS,
           year, value) %>%
    pivot_wider(names_from = year, values_from = value, names_prefix = "X") %>%
    write_csv("input/energy/OECD_and_NonOECD_Conversion_Factors_Full.csv")


conv %>%
    group_by(COUNTRY=COUNTRY_FOR_CEDS, FLOW=FLOW_FOR_CEDS, PRODUCT=PRODUCT_FOR_CEDS,
           year, value) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(desc(n))




#
#
#
#
# # Recode IEA names
# A.IEAfull <- readData( "ENERGY_IN", "OECD_and_NonOECD_E_Stat", ".csv" )
# MCL <- readData( "MAPPINGS", "Master_Country_List" )
# setdiff(unique(A.IEAfull$COUNTRY), MCL$IEAName)
#
#
# setdiff(unique(A.IEAfull$COUNTRY), MCL$IEAName)
# # [1] "Bolivia"                              "China"                                "Congo"
# # [4] "Curacao"                              "Equatorial Guinea"                    "Hong Kong, China"
# # [7] "Republic of Korea"                    "Democratic Peoples Republic of Korea" "Laos"
# # [10] "Moldova"                              "Myanmar (Burma)"                      "Macedonia"
# # [13] "Russia"                               "Slovakia"                             "Syria"
# # [16] "Tanzania"                             "Turkey"                               "Venezuela"
# # [19] "Vietnam"
#
# unique(grep("China|Bolivia|Congo|Lao|Russia|Slovak|Myanmar|Korea|Tanzania|Viet|Curacao|Hong|Moldova|Macedonia|Syri|Venezuela|Tur|Guin", MCL$IEAName, value=T, ignore.case = T))
# # [1] "Plurinational State of Bolivia"        "People's Republic of China"            "Democratic Republic of the Congo"
# # [4] "Democratic Republic of the Congo"      "Republic of the Congo"                 "Curacao/Netherlands Antilles"
# # [7] "Hong Kong (China)"                     "Korea"                                 "Lao People's Democratic Republic"
# # [10] "Republic of Moldova"                   "Republic of Moldova"                   "Republic of North Macedonia"
# # [13] "Myanmar"                               "Democratic People's Republic of Korea" "Russian Federation"
# # [16] "Slovak Republic"                       "Syrian Arab Republic"                  "United Republic of Tanzania"
# # [19] "United Republic of Tanzania"           "Bolivarian Republic of Venezuela"      "Republic of Turkiye"
# # [22] "Viet Nam"
#
# recoder <- list(
#     "Bolivia" = "Plurinational State of Bolivia",
#     "China" = "People's Republic of China",
#     "Congo" = "Democratic Republic of the Congo",
#     "Curacao" = "Curacao/Netherlands Antilles",
#     "Equatorial Guinea" = "Equatorial Guinea",
#     "Hong Kong, China" = "Hong Kong (China)",
#     "Republic of Korea" = "Korea",
#     "Democratic Peoples Republic of Korea" = "Democratic People's Republic of Korea",
#     "Laos" = "Lao People's Democratic Republic",
#     "Moldova" = "Republic of Moldova",
#     "Myanmar (Burma)" = "Myanmar",
#     "Macedonia" = "Republic of North Macedonia",
#     "Russia" = "Russian Federation",
#     "Slovakia" = "Slovak Republic",
#     "Syria" = "Syrian Arab Republic",
#     "Tanzania" = "United Republic of Tanzania",
#     "Turkey" = "Republic of Turkiye",
#     "Venezuela" = "Bolivarian Republic of Venezuela",
#     "Vietnam" = "Viet Nam"
# )
#
#
# A.IEAfull %>%
#     mutate(COUNTRY=recode(COUNTRY, !!!recoder)) %>%
#     pull(COUNTRY) %>%
#     setdiff(MCL$IEAName)
#
# A.IEAfull %>%
#     mutate(COUNTRY=recode(COUNTRY, !!!recoder)) %>%
#     write.csv("energy/OECD_and_NonOECD_E_Stat.csv")
#
#
# IEA_composite_regions <- c( FSU_IEA_composite_name, FYUG_IEA_composite_name,
#                             Other_African_composite_name, Other_Americas_composite_name,
#                             Other_Asia_composite_name )
