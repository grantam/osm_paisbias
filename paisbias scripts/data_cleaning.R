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


## Load the data
## To ensure that the data meet the requriements of other scholar, the data is set to the specification of the orignal analysis: "We are only looking at countries since 1900, and need to combine FH and prepare Polity2. We also only care about sovereign countries."

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
  dplyr::select(country_name, country_id, year, v2x_polyarchy, e_fh_combined, e_polity2, v2ellovtlg, v2ellovtsm, v2ellovttm, v2ellostsl, v2ellostss, v2ellostts, v2elvotlrg, v2elvotsml, v2xex_elecreg, v2xex_elecreg, multi_party_legislative_elections, v2x_suffr, v2eltype_legislative, v2eltype_presidential, top2_difference, top2_combined, top2_monopoly, v2ellocons, v2ellocumul, v2ellocons, v2elprescumul, v2elturnhog, v2elturnhos, v2eltvrexo, turnover_period, turnover_event, two_turnover_period)

