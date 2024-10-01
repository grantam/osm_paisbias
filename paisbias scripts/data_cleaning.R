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
         country_id,
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
         v2eltvrexo)

write.csv(df_vdem, file = "~/R projects/osm_paisbias/paisbias scripts/vdem_osm.csv", row.names = F)


%>% # Executive turnover
  mutate(v2elturnhog = ifelse((is.na(v2eltype_6) & is.na(v2eltype_0)) | (v2eltype_6 == 0 & v2eltype_0 == 0), 2, v2elturnhog),
         v2elturnhos = ifelse((is.na(v2eltype_6) & is.na(v2eltype_0)) | (v2eltype_6 == 0), 2, v2elturnhos),
         v2eltvrexo = ifelse((is.na(v2eltype_6) & is.na(v2eltype_0)) | (v2eltype_6 == 0 & v2eltype_0 == 0), 3, v2eltvrexo),
         v2elturnhog = as.factor(v2elturnhog),
         v2elturnhos = as.factor(v2elturnhos),
         v2eltvrexo = as.factor(v2eltvrexo)) %>%
  group_by(country_name) %>%
  mutate()