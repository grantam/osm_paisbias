## Title:      data_cleaning.R
## Author:     Grant Mitchell
## Email:      mitchell.grant.a@gmail.com 
## Desciption:    Preparing the data for multi-level model and random forest 
## Last updated:    2024-06-13 

## Load packages
library("tidyverse")
library("viridis")
library("h2o")
library("randomizr") # for grouped fold assignment
library("ggrepel")
library("ggforce")
library("ggpubr")
library("ggeffects")
library("patchwork")
library("ggExtra")
library("vdemdata")


## Load the data
## To ensure that the data meet the requirements of other scholar, the data uses the variables in the original analysis

df_vdem <- vdem %>%
  select(country_name, # Country name
         year, # year
         COWcode, # correlates of war code
         v2x_polyarchy, # electoral democracy
         v2ellovtlg, # largest vote share
         v2ellovtsm, # second largest vote share
         v2ellostsl, # largest seat share
         v2ellovttm, # third largest vote share
         v2ellostss, # second largest seat share
         v2ellostts, # third largest seat share
         v2elvotlrg, # largest presidential vote share
         v2elvotsml, # second largest presidential vote share
         v2xex_elecreg, # Executive electoral regime index
         v2xlg_elecreg, # Legislative electoral regime index
         v2x_suffr, # Share of population with suffrage
         v2eltype_0, # Dummy for legislative elections
         v2eltype_6, # Dummy for presidential elections
         v2ellocons, # Legislative elections, consecutive
         v2ellocumul, # Legislative elections, cumulative
         v2elprescons, # Presidential elections, consecutive
         v2elprescumul, # Presidential elections, cumulative
         v2elturnhog, # Head of government turnover
         v2elturnhos, # Head of state turnover
         v2eltvrexo) %>% # Executive turnover
  mutate(v2elturnhog = ifelse((is.na(v2eltype_6) & is.na(v2eltype_0)) | (v2eltype_6 == 0 & v2eltype_0 == 0), 2, v2elturnhog),
         v2elturnhos = ifelse((is.na(v2eltype_6) & is.na(v2eltype_0)) | (v2eltype_6 == 0), 2, v2elturnhos),
         v2eltvrexo = ifelse((is.na(v2eltype_6) & is.na(v2eltype_0)) | (v2eltype_6 == 0 & v2eltype_0 == 0), 3, v2eltvrexo),
         v2elturnhog = as.factor(v2elturnhog),
         v2elturnhos = as.factor(v2elturnhos),
         v2eltvrexo = as.factor(v2eltvrexo)) %>%
  group_by(country_name) %>%
  mutate()






shared_polities <- read_csv("og_rep/shared_polities.csv")
shared_polities <- as.vector(shared_polities$country_text_id)

df_backsliding_all <- 
  read_csv("og_rep/backsliding_data.csv.gz") |> 
  dplyr::filter(year >= 1900)  |> 
  filter(country_text_id %in% shared_polities) |> 
  mutate(e_fh_combined = (((e_fh_pr+e_fh_cl)-14)*-1)/12,
         e_polity2 = ifelse(e_polity2 < -10, NA, e_polity2),
         e_polity2 = (e_polity2+10)/20,
         e_polity2 = ifelse(year > 2018, NA, e_polity2)) |> 
  filter(sovereign == 1) 

## Reduce the dataset to outcome and predictors
df_backsliding <- 
  df_backsliding_all |>
  ungroup() |>
  dplyr::select(country_name, country_id, year, v2x_polyarchy, e_fh_combined, e_polity2, v2ellovtlg, v2ellovtsm, v2ellovttm, v2ellostsl, v2ellostss, v2ellostts, v2elvotlrg, v2elvotsml, v2xex_elecreg, v2xex_elecreg, multi_party_legislative_elections, v2x_suffr, v2eltype_legislative, v2eltype_presidential, top2_difference, top2_combined, top2_monopoly, v2ellocons, v2ellocumul, v2ellocons, v2elprescumul, v2elturnhog, v2elturnhos, v2eltvrexo, turnover_period, turnover_event, two_turnover_period) %>%
  mutate(ctyid = as.factor(country_name))

