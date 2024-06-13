## Title:      06_table.R
## Author:     Daniel Weitzel
## Email:      daniel.weitzel@colostate.edu 
## Changed:    2023-10-31
## Purpose:    This script produces Table 1

## Set the seed
set.seed(1904)

# Libraries
library("tidyverse")

# Load the data
df_polyarchy <- read_csv("data/output/polyarchy_predictions.csv") |> 
  rename(`Polyarchy, obs` = `observed_mean`,
         `Polyarchy, pred` = `prediction_mean`)

df_fh <- read_csv("data/output/fh_predictions.csv")|> 
  rename(`FH, obs` = `observed_mean`,
         `FH, pred` = `prediction_mean`) |> 
  dplyr::select(-year)

df_p2 <- read_csv("data/output/polity2_predictions.csv")|> 
  rename(`Polity2, obs` = `observed_mean`,
         `Polity2, pred` = `prediction_mean`) |> 
  dplyr::select(-year)

df_table <-
  df_polyarchy |> 
  bind_cols(df_p2,df_fh)
