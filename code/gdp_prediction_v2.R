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
mpro_path <- file.path(base_path,'Mpro_PC','2021_July','02_output_data')
output_path <- file.path(base_path,'IOM', 'gravity_model_initial', "data")

#### load input data

# population age group shares
population_data <- read.csv(file.path(output_path,"population_and_share_un.csv"))

# mpro
mpro <- readRDS(file.path(mpro_path,'X9_3_mc_remerge_mixed_2021-09-06.RDS'))

# migration
df_mig <- read.csv(file.path(output_path,"gravity_prediction_review.csv"))

#### gdp and productivity data

# clean data

gdp_data <- mpro %>%
  pluck("macro.data") %>%
  select(c(1,2,4)) %>%
  filter(year %in% c(2021:2025))

population_data_clean <- population_data %>%
  select(c(1,3,5))

gdp_working_age <- left_join(gdp_data,population_data_clean)

# getting productivity per worker in each country
productivity <- gdp_working_age %>%
  transform(productivity = (GDP.PPP/population_15_64))

# rename and select variables
productivity = rename(productivity, orig = ccode, year0 = year)
productivity = data.table(productivity)[, .(orig, year0, productivity)]

# gdp of countries of destination
gdp_dest <- mpro %>%
  pluck("macro.data") %>%
  select(c(1,2,4)) %>%
  filter(year %in% c(2000:2025))%>%
  rename(dest = ccode, year0 = year, gdp_baseline = GDP.PPP)

#### total gdp added by immigrants

# add productivity data
df_comb = merge(df_mig, productivity, all.x = TRUE, by = c("orig", "year0"))

# select years in the future
df_comb = data.table(df_comb)[year0>2020]

# compute gdp added by migrants
df_comb$gdp_mig = df_comb$flow*df_comb$productivity

# sum up gdp added by destination
df_comb = df_comb[, .(gdp_mig = sum(gdp_mig, na.rm = TRUE)), by = .(dest, year0)]

# order data
df_comb = df_comb[order(dest, year0)]

# compute cumulative sum of gdp added by migrants
df_comb = df_comb[, ":="(cum_gdp_mig = cumsum(gdp_mig)), by = .(dest)]

#### gdp baseline scenario vs. without immigration

# merge data sets
mig_gdp = merge(gdp_dest, df_comb, all.x = TRUE, by = c("dest", "year0"))

# set gdp added by migrants to zero for years before 2021
mig_gdp$gdp_mig[mig_gdp$year0<2021] = 0
mig_gdp$cum_gdp_mig[mig_gdp$year0<2021] = 0

# compute gdp without migration
mig_gdp$gdp_no_mig = mig_gdp$gdp_baseline-mig_gdp$cum_gdp_mig

# save results
write.csv(mig_gdp, file.path(output_path,"gdp_prediction_v2.csv"), row.names = FALSE)



# plot data for germany and portugal
test = copy(mig_gdp)[, .(dest, year0, gdp_baseline, gdp_no_mig)]
test = melt(test, id.vars = c("dest", "year0"), variable.name = "scenario", value.name = "gdp")
ggplot(subset(test, dest=="DEU"), aes(x = year0, y = gdp, color = scenario)) +
  geom_line()
ggplot(subset(test, dest=="PRT"), aes(x = year0, y = gdp, color = scenario)) +
  geom_line()










