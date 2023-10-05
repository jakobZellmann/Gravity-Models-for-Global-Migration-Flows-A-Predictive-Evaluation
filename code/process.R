
# notes -------------------------------------------------------------------

#midclass and age breakdown only from 2000 onwards - we lose 1995 (1/5 of the data)

# source: gravity_validation_v4_2.R (lines <= 133)

# load packages -----------------------------------------------------------

pacman::p_load(reshape2,
               tidyverse,
               worlddataverse,
               zoo,
               readxl,
               openxlsx,
               data.table,
               Metrics,
               stargazer,
               countrycode)

# set paths ---------------------------------------------------------------

# drive path
get_drive_path <- function(){
  dplyr::case_when(
    dir.exists("G:/Geteilte Ablagen") ~ file.path("G:",
                                                  "Geteilte Ablagen",
                                                  "DATA_WDL"),
    dir.exists("G:/Shared Drives") ~ file.path("G:",
                                               "Shared Drives",
                                               "DATA_WDL"),
    dir.exists("/Volumes/GoogleDrive/Geteilte Ablagen") ~
      file.path("/Volumes",
                "GoogleDrive",
                "Geteilte Ablagen",
                "DATA_WDL"))
}

# setup paths
base_path <- worlddataverse::get_wdl_path()
if(is.na(base_path)) {
  base_path = get_drive_path()
}

input_path <- file.path(base_path,'IOM','gravity_model_initial')
old_path <- file.path(base_path,'IOM', "Migration & Economic Growth old", "Data&Codes", "Data", "Datasets", "Data")
#directories for input raw data on migration
outputpath <- get_drive_path()
datadir <- file.path(outputpath,"IOM","data_output")


# part I ------------------------------------------------------------------



# load data ---------------------------------------------------------------


# load combined data from Jesus's model
df_1 = readRDS('input/data5lagged_new.RDS')

# load information on landlocked countries from Jesus's model
df_2 = read.csv2('input/geo_cepii.csv', header = T, sep = ";", dec = ".", na.strings = ".", fileEncoding = "UTF-8-BOM")

# load mpro data for gdp per capita
df_3 = readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
df_3 = df_3$macro.data

# load population data
df_4 = read.csv2('input/WPP2019_TotalPopulationBySex.csv', sep = ",")

# load migrant stock data
df_5 = read_excel('input/undesa_pd_2020_ims_stock_by_sex_destination_and_origin (1).xlsx', sheet = 2, skip = 10)

# load oecd migration flow data
df_6 = read.csv2('input/OECD_migration.csv', sep = ",", fileEncoding = "UTF-8-BOM")

#### add information on landlocked countries to main data set

# match country codes
df_2 = df_2[!duplicated(df_2$iso2),]
df_2$ccode = countrycode(df_2$iso2, "iso2c", "iso3c",
                         custom_match = c("AN" = "ANT", "TP" = "TLS", "YU" = "MNE", "ZR" = "COD"))

# select country codes and information on landlocked countries
df_2 = data.table(df_2)[, .(ccode, landlocked)]

# add serbia
df_2 = bind_rows(df_2, data.frame(ccode = "SRB", landlocked = 1))

# merge data sets
df_1[,1:2] = sapply(df_1[,1:2], as.character)
df_inp = merge(df_1, df_2, by.x = "orig", by.y = "ccode", all.x = T, all.y = F)
df_inp = rename(df_inp, landlocked_orig = landlocked)
df_inp = merge(df_inp, df_2, by.x = "dest", by.y = "ccode", all.x = T, all.y = F)
df_inp = rename(df_inp, landlocked_dest = landlocked)

#### add rows for year0 2020

# get data from one year
df_2020 = subset(df_inp, year0==2015)

# set unkonwn values to NA
df_2020[,7:13] = NA

# set year0 to 2020
df_2020$year0 = 2020

# add data to main input data file
df_inp = rbind(df_inp, df_2020)

#### add gdp per capita averages for 2020-2025

# select variables
df_3 = select(df_3, ccode, year, GDP.PC.PPP)

# select years
df_3 = subset(df_3, year%in%df_inp$year0)

# merge data
df_inp = select(df_inp, -gdp.cap.dest, -gdp.cap.orig)
df_inp = merge(df_inp, df_3, by.x = c("dest", "year0"), by.y = c("ccode", "year"), all.x = T, all.y = F)
df_inp = rename(df_inp, gdp_pc_dest = GDP.PC.PPP)
df_inp = merge(df_inp, df_3, by.x = c("orig", "year0"), by.y = c("ccode", "year"), all.x = T, all.y = F)
df_inp = rename(df_inp, gdp_pc_orig = GDP.PC.PPP)

#### add population data for 2020

# select medium variant
df_4 = data.table(df_4)[Variant=="Medium"]

# select only countries as origin or destination
df_4 = df_4[LocID<900]

# remove the channel islands
df_4 = df_4[LocID!=830]

# get character instead of numeric country codes
df_4$ccode = countrycode(df_4$LocID, "iso3n", "iso3c")

# select variables
df_4 = select(df_4, ccode, Time, PopTotal)

# select years
df_4 = subset(df_4, Time%in%df_inp$year0)

# multiply population by 1000 to get absolute values
df_4$PopTotal = as.numeric(df_4$PopTotal)
df_4$PopTotal = df_4$PopTotal*1000



# merge data
df_inp = select(df_inp, -pop.tot.dest, -pop.tot.orig)
df_inp = merge(x = df_inp, y = df_4, by.x = c("dest", "year0"), by.y = c("ccode", "Time"), all.x = T, all.y = F)
df_inp = rename(df_inp, population_dest = PopTotal)
df_inp = merge(df_inp, df_4, by.x = c("orig", "year0"), by.y = c("ccode", "Time"), all.x = T, all.y = F)
df_inp = rename(df_inp, population_orig = PopTotal)

##### add migrant stock data for 2020

# select only countries as origin or destination
df_5 = data.table(df_5)[`Location code of destination`<900 & `Location code of origin`<900]

# remove the channel islands
df_5 = df_5[`Location code of destination`!=830 & `Location code of origin`!=830]

# select columns and adjust names
df_5 = df_5[,c(4, 7:14)]
names(df_5)[1:2] = c("dest", "orig")

# get character instead of numeric country codes
df_5$dest = countrycode(df_5$dest, "iso3n", "iso3c")
df_5$orig = countrycode(df_5$orig, "iso3n", "iso3c")

# transform data from wide to long format
df_5 = melt(df_5, id.vars = c("dest", "orig"), variable.name = "year0", value.name = "migrant_stock")

# adjust years
df_5$year0 = as.character(df_5$year0)
df_5$year0 = as.numeric(sapply(strsplit(df_5$year0, "\\."), "[", 1))

# add migrant stock data to input data
df_inp = select(df_inp, -migration.stock)
df_inp = merge(df_inp, df_5, by = c("dest", "orig", "year0"), all.x = T, all.y = F)

#### add data on migrant flows from oecd

# select parameter in oecd data
df_6 = data.table(df_6)[Variable=="Inflows of foreign population by nationality"]

# select variables
df_6 = df_6[, .(COU, CO2, Year, Value)]

# rename variables
names(df_6) = c("dest", "orig", "year", "flow_oecd")

# select moldova
df_6 = df_6[orig=="MDA"]

# add missing country year combinations
dest = unique(df_6$dest)
year = 1995:2019
df_6_1 = expand.grid(dest, year)
names(df_6_1) = c("dest", "year")
df_6_1 = data.table(df_6_1)[!paste0(dest, year)%in%paste0(df_6$dest, df_6$year)]
df_6_1$orig = "MDA"
df_6 = bind_rows(df_6, df_6_1)

# order data
df_6 = df_6[order(dest, year)]

# fill in missing values for destinations with just one observation
for (i in unique(df_6$dest)) {
  if(length(unique(df_6$flow_oecd[df_6$dest==i & !is.na(df_6$flow_oecd)])) == 1)
  {df_6$flow_oecd[df_6$dest==i] = unique(df_6$flow_oecd[df_6$dest==i & !is.na(df_6$flow_oecd)])}
}

# fill remaining missing values
df_6 = df_6[, ":="(flow_oecd = round(na.fill(flow_oecd, fill = "extend"))), by = .(dest)]

# sum up data for 5-year periods
df_6$year0 = df_6$year-df_6$year%%5

# sum up flows for 5-year periods
df_6 = df_6[, .(flow_oecd = sum(flow_oecd)), by = .(dest, orig, year0)]

# add oecd flows to input data
df_inp = merge(df_inp, df_6, by = c("dest", "orig", "year0"), all.x = TRUE, all.y = F)

#### replace selected flows by oecd data

# start with year0 = 1995 (like in oecd data)
df_inp = data.table(df_inp)[year0>1990]

# select total population (not by sex)
df_inp = subset(df_inp, sex=="b")

# replace migration from moldova to other countries by oecd numbers
df_inp$flow[df_inp$orig=="MDA"] = df_inp$flow_oecd[df_inp$orig=="MDA"]

#### update lagged flows

df_lagged = copy(df_inp)[, .(dest, orig, year0, flow)]
df_lagged$year0 = df_lagged$year0+5
df_lagged = rename(df_lagged, flow_lagged = flow)
df_inp = select(df_inp, -flow_lagged)
df_inp = merge(df_inp, df_lagged, by = c("dest", "orig", "year0"), all.x = T, all.y = F)

# order and select variables
df_inp = df_inp[,.(dest, orig, year0, orig_code, dest_code, sex, flow, flow_lagged, contig, comlang_off, colony, dist, col45, landlocked_orig, landlocked_dest, gdp_pc_dest, gdp_pc_orig, population_dest, population_orig, migrant_stock)]


# order and select columns
df_inp = select(df_inp, orig, dest, year0, flow, flow_lagged, gdp_pc_orig, gdp_pc_dest, population_orig, population_dest, migrant_stock, contig, comlang_off, colony, dist, landlocked_orig, landlocked_dest)


# part II -----------------------------------------------------------------



# # load combined data from Jesus's model - something is wrong with the data: gravity_input_v3.csv - dimension do not fit
# df_inp <- read.csv(file.path(input_path, "data","gravity_input_v3.csv"))

# load age breakdown
age_breakdown <- read.csv(file.path(input_path,"data","un_shares_15_20_64.csv")) %>%
  select(iso3, year0, age_15_64)

# load middle class percent
midclass <- read.csv(file.path(input_path,"data","mid_class_percent.csv")) %>%
  select(c(2,3,4)) %>%
  rename("year0" = year)

# load education data
education <- read.csv(file.path(input_path,"data","education_postsec.csv")) %>%
  select(c(2,3,4)) %>%
  rename("year0" = year) %>%
  filter(year0 %in% c("1995","2000","2005","2010","2015","2020"))


# # load mpro data on gdp growth
# mpro = readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
# mpro = mpro$macro.data

# load and clean data on unemployment
unemployment_wb <- read.csv(file.path(datadir, "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_3469538.csv"), fileEncoding = 'UTF-8-BOM') %>%
  worlddataverse::destroyX_world_bank()

unemployment_clean <- unemployment_wb %>% 
  rename("country" = 1) %>%
  subset(select = c(1,5:65)) %>%
  mutate("country" = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  pivot_longer(!country, names_to = "year") %>%
  mutate_at(vars("year"), as.numeric) %>%
  drop_na() %>%
  rename("iso3" = 1, "year0" = 2, "unemployment" = 3)

# load and clean data on fertility rates
fertility_wb <- read.csv(file.path(datadir, 'API_SP.DYN.TFRT.IN_DS2_en_csv_v2_3471714.csv'), fileEncoding = "UTF-8-BOM") %>%
  worlddataverse::destroyX_world_bank()

fertility_clean <- fertility_wb %>% 
  rename("country" = 1) %>%
  subset(select = c(1,5:65)) %>%
  mutate("country" = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  pivot_longer(!country, names_to = "year") %>%
  mutate_at(vars("year"), as.numeric) %>%
  drop_na() %>%
  rename("iso3" = 1, "year0" = 2, "fertility" = 3)

# use 2019 fertility data for 2020
fertility_20 = data.table(fertility_clean)[year0==2019]
fertility_20$year0 = 2020
fertility_clean = rbind(fertility_clean, fertility_20)


# merge data --------------------------------------------------------------

# add information on AGE
# df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, age_breakdown, by.x = c("orig", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, age_15_64_orig = age_15_64)
df_inp = merge(df_inp, age_breakdown, by.x = c("dest", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, age_15_64_dest = age_15_64)

# add information on MIDDLE CLASS just for origin
# df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, midclass, by.x = c("orig", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, midclass_orig = midclasspercent)
df_inp = merge(df_inp, midclass, by.x = c("dest", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, midclass_dest = midclasspercent)

# add information on EDUCATION
# df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, education, by.x = c("orig", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, education_orig = post_secondary_education)
df_inp = merge(df_inp, education, by.x = c("dest", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, education_dest = post_secondary_education)

# add information on UNEMPLOYMENT
# df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, unemployment_clean, by.x = c("orig", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, unemployment_orig = unemployment)
df_inp = merge(df_inp, unemployment_clean, by.x = c("dest", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, unemployment_dest = unemployment)

# add information on FERTILITY
# df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, fertility_clean, by.x = c("orig", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, fertility_orig = fertility)
df_inp = merge(df_inp, fertility_clean, by.x = c("dest", "year0"), by.y = c("iso3", "year0"), all.x = T, all.y = F)
df_inp = rename(df_inp, fertility_dest = fertility)

# add dummy for policy intervention in DEU
df_inp$policy_DEU <- ifelse(df_inp$year0 == 2015|2020 & df_inp$dest == "DEU" & df_inp$orig == "MDA",
                            1,
                            0)

subset(df_inp, df_inp$dest == "DEU" & df_inp$orig == "MDA")


# write data --------------------------------------------------------------
# tmp1 <- read.csv('input/flows2020.csv')[,c('orig', 'dest', 'year0', 'da_pb_closed')]
tmp1 <- read.csv('input/flows2020.csv')
tmp1_lagged <- tmp1
tmp1_lagged$year0 <- tmp1_lagged$year0 + 5
tmp2 <- readRDS('output/data.RDS')
df_inp <- left_join(tmp2, tmp1, by = c('orig', 'dest', 'year0'))
df_inp <- left_join(df_inp, tmp1_lagged, by = c('orig', 'dest', 'year0'))
saveRDS(df_inp, 'output/data.RDS')



# replace missing stocks with 0? ------------------------------------------

flow_BIN <- as.factor(ifelse(df_inp$flow==0, 1, 0))
mig_BIN <- ifelse(is.na(df_inp$migrant_stock)|df_inp$migrant_stock==0, 1, 0)
model <- glm(mig_BIN~flow_BIN, family = 'binomial')
summary(model)


flow_BIN <- ifelse(df_inp$flow==0, 0, 1)
mig_BIN <- ifelse(is.na(subset(df_inp, year0 <=2015, select = migrant_stock)), 0, 1)

