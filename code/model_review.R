# load packages

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

#setup paths
base_path <- worlddataverse::get_wdl_path()
if(is.na(base_path)) {
  base_path = get_drive_path()
}

input_path <- file.path(base_path,'IOM','gravity_model_initial')
old_path <- file.path(base_path,'IOM', "Migration & Economic Growth old", "Data&Codes", "Data", "Datasets", "Data")
#directories for input raw data on migration
outputpath <- get_drive_path()
datadir <- file.path(outputpath,"IOM","data_output")

#### load input data

# load combined data from Jesus's model
df_inp <- read.csv(file.path(input_path, "data","gravity_input_v3.csv"))

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

# load mpro data on gdp growth
mpro = readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
mpro = mpro$macro.data

# load and clean data on unemployment
unemployment_wb <- read.csv(file.path(datadir, "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_3469538.csv")) %>%
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

fertility_wb <- read.csv(file.path(datadir, 'API_SP.DYN.TFRT.IN_DS2_en_csv_v2_3471714.csv')) %>%
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

#### put input data together


# add information on AGE
df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, age_breakdown, by.x = c("orig", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, age_15_64.orig = age_15_64)
df_inp = merge(df_inp, age_breakdown, by.x = c("dest", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, age_15_64.dest = age_15_64)

# add information on MIDDLE CLASS just for origin
df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, midclass, by.x = c("orig", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, midclass.orig = midclasspercent)

# add information on EDUCATION
df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, education, by.x = c("orig", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, education.orig = post_secondary_education)
df_inp = merge(df_inp, education, by.x = c("dest", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, education.dest = post_secondary_education)

# add information on UNEMPLOYMENT
df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, unemployment_clean, by.x = c("orig", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, unemployment.orig = unemployment)
df_inp = merge(df_inp, unemployment_clean, by.x = c("dest", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, unemployment.dest = unemployment)

# add information on FERTILITY
df_inp[,1:2] = sapply(df_inp[,1:2], as.character)
df_inp = merge(df_inp, fertility_clean, by.x = c("orig", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, fertility.orig = fertility)
df_inp = merge(df_inp, fertility_clean, by.x = c("dest", "year0"), by.y = c("iso3", "year0"))
df_inp = rename(df_inp, fertility.dest = fertility)

# create origin country dummies
for (i in unique(df_inp$orig)) {
  df_inp[, paste0("orig_", i)] = 0
  df_inp[df_inp$orig==i, paste0("orig_", i)] = 1
}

# remove afghanistan as baseline country
df_inp = select(df_inp, -orig_AFG)

# create destination country dummies
for (i in unique(df_inp$dest)) {
  df_inp[, paste0("dest_", i)] = 0
  df_inp[df_inp$dest==i, paste0("dest_", i)] = 1
}

# remove afghanistan as baseline country
df_inp = select(df_inp, -dest_AFG)

# remove col45
df_inp = select(df_inp, -col45)

# remove colony
df_inp = select(df_inp, -colony)

# select total population (not by sex)
df_inp = subset(df_inp, sex=="b")

# replace missing migration stocks by 0
df_inp$migrant_stock[is.na(df_inp$migrant_stock)] = 0

#### add year dummies

# create year dummies for 2005-2015
for (i in c(2005, 2010, 2015)) {
  df_inp[, paste0("year_", i)] = 0
  df_inp[df_inp$year0==i, paste0("year_", i)] = 1
}

# apply dummy of 2015-2020 for 2020-2025
df_inp$year_2015[df_inp$year0==2020] = 1

# add an interaction terms of orig_MDA, dest_DEU, dest_PRT and year_2015
df_inp$orig_MDA_year_2015 = df_inp$orig_MDA*df_inp$year_2015
df_inp$dest_DEU_year_2015 = df_inp$dest_DEU*df_inp$year_2015
df_inp$dest_PRT_year_2015 = df_inp$dest_PRT*df_inp$year_2015

#### take logs of selected variables

# set migration flows and stocks of 0 to 1 before taking logs
df_inp$flow[df_inp$flow==0] = 1
df_inp$flow_lagged[df_inp$flow_lagged==0] = 1
df_inp$migrant_stock[df_inp$migrant_stock==0] = 1

# take logs
df_inp$flow = log(df_inp$flow)
df_inp$flow_lagged = log(df_inp$flow_lagged)
df_inp$gdp_pc_dest = log(df_inp$gdp_pc_dest)
df_inp$gdp_pc_orig = log(df_inp$gdp_pc_orig)
df_inp$population_dest = log(df_inp$population_dest)
df_inp$population_orig = log(df_inp$population_orig)
df_inp$migrant_stock = log(df_inp$migrant_stock)
df_inp$dist = log(df_inp$dist)

#### estimation

# small model 1
# dist, gdp_pc_dest, gdp_pc_orig, population_dest, population_orig
small_1 = lm(flow ~ ., data = df_inp[,c(7, 11, 14:17)])

# small model 2
# dist, gdp_pc_dest, gdp_pc_orig, population_dest, population_orig, contig, comlang_off, landlocked_orig, landlocked_dest, migrant_stock
small_2 = lm(flow ~ ., data = df_inp[,c(7, 11, 14:17, 9:10, 12:13)])

# full model without country and year fixed effects
full_1 = lm(flow ~ ., data = df_inp[,c(7, 11, 14:17, 9:10, 12:13, 18:27, 8)])

# full model with country and year fixed effects
linear_first = lm(flow ~ ., data = df_inp[,c(7, 11, 14:17, 9:10, 12:13, 18:27, 8, 386:388, 103, 248, 339, 389:391, 28:102, 104:247, 249:338, 340:385)])
summary(linear_first)
summary(linear_first)$adj.r.squared
AIC(linear_first)

# save summary table
stargazer(small_1, small_2, full_1, linear_first, type = "html", out = "table_report_2022-01.htm")

#### prediction

# create data set for predictions
df_pred = df_inp
df_pred$year0 = as.numeric(df_pred$year0)

# make predictions
df_pred = cbind(df_pred[,c(1, 3, 2, 7)], predict(linear_first, df_pred[,8:ncol(df_pred)]))
names(df_pred)[5] = "pred_flow"

# take exponent to reverse logs
df_pred[,4:5] = sapply(df_pred[,4:5], exp)

# round results
df_pred[,4:5] = sapply(df_pred[,4:5], round)

# set 1 to 0 in flow and predictions
df_pred$flow[df_pred$flow==1] = 0
df_pred$pred_flow[df_pred$pred_flow==1] = 0

# add predicted flows as flows in 2020-2025
df_pred$flow[df_pred$year0==2020] = df_pred$pred_flow[df_pred$year0==2020]

# divide flows by 5 to get average annual numbers
df_pred$flow = df_pred$flow/5

#### get annual flows for 2021 to 2025 based on gdp growth

# get sum of migration flows by destination
df_sum = data.table(df_pred)[, .(flow = sum(flow, na.rm = TRUE)), by = .(dest, year0)]

# compute shares of countries of origin in total migration after 2020
df_share = data.table(df_pred)[year0==2020]
df_share = df_share[, ":="(flow_share = flow/sum(flow, na.rm = TRUE)), by = .(dest)]

# prepare gdp data
df_gdp = data.table(mpro)[, .(ccode, year, GDP.PC.PPP)]
# df_gdp = df_gdp[ccode%in%c("DEU", "PRT") & year>1999 & year<2026]
df_gdp = df_gdp[year>1999 & year<2026]
names(df_gdp) = c("dest", "year", "gdp_pc")

# compute gdp growth
df_gdp_copy = copy(df_gdp)
df_gdp_copy$year = df_gdp_copy$year+1
names(df_gdp_copy) = c("dest", "year", "gdp_prev")
df_gdp = merge(df_gdp, df_gdp_copy, by = c("dest", "year"))
df_gdp$gdp_grw = df_gdp$gdp_pc/df_gdp$gdp_prev-1

# compute average gdp growth of 5-year periods
df_gdp$year0 = (df_gdp$year-1)-((df_gdp$year-1)%%5)
df_gdp_avg = copy(df_gdp)[, .(gdp_grw = mean(gdp_grw, na.rm = TRUE)), by = .(dest, year0)]

# combine gdp and migrant flow data
mig_gdp = merge(df_sum, df_gdp_avg, by = c("dest", "year0"))

# create country dummies
for (i in unique(mig_gdp$dest)) {
  mig_gdp[, paste0("dest_", i)] = 0
  mig_gdp[mig_gdp$dest==i, paste0("dest_", i)] = 1
}

# regress migrant flow on GDP growth
mig_gdp$flow[mig_gdp$flow==0] = 1
linear_gdp = lm(log(flow) ~ ., data = mig_gdp[,3:ncol(mig_gdp)])

# get annual gdp growth for 2021 to 2025
gdp_21_25 = copy(df_gdp)[year>2020]
gdp_21_25 = gdp_21_25[, .(dest, year, gdp_grw)]

# create country dummies
for (i in unique(gdp_21_25$dest)) {
  gdp_21_25[, paste0("dest_", i)] = 0
  gdp_21_25[gdp_21_25$dest==i, paste0("dest_", i)] = 1
}

# predict annual migration flows
gdp_21_25 = cbind(gdp_21_25[,1:2], predict(linear_gdp, gdp_21_25[,3:ncol(gdp_21_25)]))
gdp_21_25 = rename(gdp_21_25, flow_gdp = V2)
gdp_21_25$flow_gdp = exp(gdp_21_25$flow_gdp)

# create data frame with total flows from 2021 to 2025
flow_21_25 = copy(df_sum)[year0==2020]
flow_21_25$flow_sum = flow_21_25$flow*5

# combine total flows with distribution of flows over years
flow_21_25 = merge(gdp_21_25, flow_21_25, by = "dest")

# rescale distribution of flows to match total flows from 2021 to 2025
flow_21_25 = flow_21_25[, ":="(flow = round(flow_gdp*(flow_sum/sum(flow_gdp)))), by = .(dest)]

# check result for germany and portugal
test = copy(flow_21_25)[dest%in%c("DEU", "PRT")]
ggplot() +
  geom_col(test, mapping = aes(x = year, y = flow, fill = dest), position = "dodge") +
  labs(x = "", y = "Number of migrants") +
  scale_fill_manual(values = c("#4daf4a", "#377eb8")) +
  theme_bw()

# select columns and combine annual flows with shares of countries of origin
df_share = df_share[, .(dest, orig, flow_share)]
flow_21_25 = flow_21_25[, .(dest, year, flow)]
flow_21_25 = rename(flow_21_25, year0 = year)
flow_21_25 = merge(df_share, flow_21_25, by = "dest", allow.cartesian = TRUE)

# get flows by country of origin and year
flow_21_25$flow = flow_21_25$flow*flow_21_25$flow_share

# select columns and years
flow_21_25 = flow_21_25[, .(dest, orig, year0, flow)]
df_pred = data.table(df_pred)[, .(dest, orig, year0, flow)]
df_pred = df_pred[year0!=2020]

# combine annual predictions for 2021-2025 with 5-year period predictions
pred_comb = rbind(df_pred, flow_21_25)

# round migration flows
pred_comb$flow = round(pred_comb$flow)

# save results
write.csv(pred_comb, file.path(input_path, "data","gravity_prediction_review.csv"), row.names = FALSE)

# check result for germany, portugal and moldova
test = copy(pred_comb)[dest%in%c("DEU", "PRT") & orig=="MDA"]
ggplot() +
  geom_col(test, mapping = aes(x = as.character(year0), y = flow, fill = dest), position = "dodge") +
  labs(x = "", y = "Number of migrants") +
  scale_fill_manual(values = c("#4daf4a", "#377eb8")) +
  theme_bw()





