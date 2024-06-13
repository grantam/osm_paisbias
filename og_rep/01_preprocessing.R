## Title:      preprocessing.R
## Author:     Daniel Weitzel
## Email:      daniel.weitzel@colostate.edu 
## Purpose:    Preprocessing file based on the specification of outcome and preds in the individual model scripts
## Changed:    2023-10-31 
## Purpose:    This script pre-processes the data for the folded random forest models in scripts 02-04. 
##             It generates a training and a test set for the H20 implementation. 

## Load the data and preprocess it
## We are only looking at countries since 1900, and need to combine FH and prepare Polity2
## We also only care about sovereign countries 
shared_polities <- read_csv("data/shared_polities.csv") 
shared_polities <- as.vector(shared_polities$country_text_id)

df_backsliding_all <- 
  read_csv("data/backsliding_data.csv.gz") |> 
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
  dplyr::select(all_of(ids), all_of(preds), all_of(outcome)) |>
  # Option below if you want to replace all missing values with a 0, results are identical
  mutate(across(where(is.numeric) & !any_of(outcome), ~replace(., is.na(.), 0))) |>
  drop_na(all_of(outcome)) |>
  dplyr::filter(year <= 2000) 

## Out of sample data set
df_outcome_2000 <- 
  df_backsliding_all |>
  ungroup() |>
  filter(year > 2000)|> 
  dplyr::select(all_of(ids), all_of(preds), all_of(outcome)) |>
  # Option below if you want to replace all missing values with a 0, results are identical
  mutate(across(where(is.numeric) & !any_of(outcome), ~replace(., is.na(.), 0)))

## Training set
df_outcome <-
  df_backsliding |>
  dplyr::select(all_of(outcome), all_of(ids))

## Split the data into training and validation set
backsliding_training <- df_backsliding

## Generate ID data sets 
## These data sets consist only of the country and year identifiers
## Training data
backsliding_training_ids <-
  backsliding_training |>
  dplyr::select(year, country_id, country_name)

## Test data 
backsliding_test_ids <-
  df_outcome_2000 |>
  dplyr::select(year, country_id,country_name)

## Generate h2o data sets
## Assign fold membership to all observations of a country
backsliding_training$folds <- cluster_ra(clusters = backsliding_training$country_id, 
                                         conditions = c("Fold_1", "Fold_2", "Fold_3", "Fold_4", 
                                                        "Fold_5", "Fold_6")) 
## A training set without the identifiers
backsliding_training_h2o <-
  backsliding_training |>
  dplyr::select(-c(year, country_id, country_name))

## A test set without the identifiers
backsliding_test_h2o <-
  df_outcome_2000 |>
  dplyr::select(-c(year, country_id, country_name))

# Cleaning up
rm(shared_polities)
