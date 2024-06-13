## Title:      04_fh.R
## Author:     Daniel Weitzel
## Email:      daniel.weitzel@colostate.edu 
## Changed:    2023-10-31
## Purpose:    This script runs the model that produces the results in Figure 1 (third column),
##             Figure 4, and Table 1 (Columns 5 and 6)

## Set the seed
set.seed(1904)

# Libraries
library("tidyverse")
library("h2o")
library("randomizr") # for grouped fold assignment
library("ggrepel")
library("ggforce")
library("ggpubr")
library("ggeffects")
library("patchwork")
library("ggExtra")

## Outcome
outcome <- "e_fh_combined"

## ID Variables
ids         <- c("country_name", "country_text_id", "country_id", "year")

## Predictors
preds       <- c("v2xlg_elecreg", "v2xex_elecreg", # legislative and presidential regular elections
                 "v2ellovtlg", "v2ellovtsm", "v2ellovttm", # Vote shares legislative elections 
                 "v2ellostsl", "v2ellostss", "v2ellostts", # seat and seat shares legislature 
                 "v2elvotlrg", "v2elvotsml", # presidential elections vote shares 
                 "multi_party_legislative_elections", # are multiparty elections happening
                 "v2x_suffr", # share of population with suffrage 
                 "top2_difference", "top2_combined", "top2_monopoly",
                 "v2ellocons", "v2ellocumul",  # legislative elections consecutive and cumulative 
                 "v2elprescons", "v2elprescumul", # presidential elections consecutive and cumulative 
                 "v2elturnhog", "v2elturnhos", "v2eltvrexo", # hog, hos, and executive turnover
                 "v2svindep", 
                 "turnover_period", "turnover_event", "two_turnover_period")

## Pre-processing of the file 
## This requires that the preds, ids, and outcome vectors above are specified
source("01_preprocessing.R")

## H20
# Random Forest implementation with h20
# configuration uses all cores and 10GB of RAM 
h2o.no_progress()
h2o.init(nthreads=-1, max_mem_size = "10g")
h2o.removeAll()

## set the predictor names
predictors <- setdiff(colnames(backsliding_training_h2o), outcome)
n_features <- length(setdiff(names(backsliding_training_h2o), outcome))
train_h2o  <- as.h2o(backsliding_training_h2o)
test_h2o   <- as.h2o(backsliding_test_h2o)

## Random forest estimation
h2o_rf_fh <- 
  h2o.randomForest(
    model_id = "RF:330*15",
    x = predictors, 
    y = outcome,
    fold_column = "folds",
    training_frame = train_h2o, 
    ntrees = 300,
    mtries = 3,
    col_sample_rate_per_tree = 0.8,
    seed = 1904,                 
    keep_cross_validation_predictions = TRUE)

## VIP
var_importance_tibble <- 
  as_tibble(h2o.varimp(h2o_rf_fh)) |>
  select(variable,scaled_importance) |>
  mutate(variable = case_when(
    variable == "v2ellovtlg" ~ "Lower chamber Vote Share, largest party",
    variable == "v2ellovtsm" ~ "Lower chamber Vote Share, second largest party",
    variable == "v2ellovttm" ~ "Lower chamber Vote Share, third largest party",
    variable =="v2elloseat" ~ "Lower chamber election seats",
    variable == "v2ellostsl" ~ "Lower chamber Seat Share, largest party",
    variable == "v2ellostss" ~ "Lower chamber Seat Share, second largest party",
    variable == "v2ellostts" ~ "Lower chamber Seat Share, third largest party",
    variable == "v2elvotlrg" ~ "Presidential Vote Share, largest party",
    variable == "v2elvotsml" ~ "Presidential Vote Share, second largest party",
    variable == "v2xlg_elecreg" ~ "Electoral Regime Index, Lower chamber",
    variable == "v2xex_elecreg" ~ "Electoral Regime Index, Presidential",
    variable == "v2x_suffr" ~ "Share of population with suffrage ",
    variable == "v2msuffrage" ~ "Share of male population with suffrage ",
    variable == "v2fsuffrage" ~ "Share of female population with suffrage ",
    variable == "multi_party_legislative_elections" ~ "Elections multiparty (LIED)",
    variable == "v2eltype_legislative" ~ "Lower chamber Election",
    variable == "v2eltype_presidential" ~ "Presidential Election",
    variable == "v2elprescons" ~ "Presidential elections consecutive ",
    variable == "v2elprescumul" ~ "Presidential elections cumulative",
    variable == "v2ellocons" ~ "Lower chamber election consecutive",
    variable == "v2ellocumul" ~ "Lower chamber election cumulative",
    variable == "v2elturnhog" ~ "Election HOG turnover ordinal",
    variable == "v2elturnhos" ~ "Election HOS turnover ordinal",
    variable == "v2eltvrexo" ~ "Election executive turnover ordinal",
    variable == "v2svdomaut" ~ "Domestic autonomy ",
    variable == "top2_combined" ~ "Vote Share, two largest parties",
    variable == "top2_difference" ~ "Difference Vote Share, two largest parties",
    variable == "top2_monopoly" ~ "Vote Share Top2 combined >= 60%",
    variable == "v2svindep" ~ "Independent states",
    variable == "turnover_period" ~ "Turnover Period (Source: LIED)", 
    variable == "turnover_event" ~ "Turnover Event (Source: LIED)", 
    variable == "two_turnover_period" ~ "Two turnover periods (Source: LIED)",              
    variable == "sovereign" ~ "Sovereign Polity (Source: LIED)"))

#  VIP plot
ggplot(var_importance_tibble, 
       aes(x = reorder(variable, scaled_importance),
           y = scaled_importance)) + 
  geom_col() + theme_minimal() + coord_flip() + 
  labs(x = " ", y = "Scaled Importance") + 
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

## Export the VIP data frame for the heatmap plot
vip_fh <- var_importance_tibble |> 
  rename('Freedom House' = scaled_importance)

write_csv(vip_fh, "data/output/vip_fh.csv")
rm(vip_fh, var_importance_tibble)

## Add predictions to the training data set 
preds_training                      <- h2o.predict(object = h2o_rf_fh, newdata = train_h2o)
preds_test                          <- h2o.predict(object = h2o_rf_fh, newdata = test_h2o)
backsliding_training$preds_training <- as.vector(preds_training)
df_outcome_2000$preds_test          <- as.vector(preds_test)

## Combine training and test data set to one combined file
df_combined <-  
  backsliding_training |> 
  bind_rows(df_outcome_2000) |> 
  unite("country_year", c("country_text_id", "year"), sep = " ", remove = FALSE) |> 
  mutate(preds_value_fhc = ifelse(!is.na(preds_training), preds_training,
                                  ifelse(!is.na(preds_test), preds_test, NA)),
         preds_type_fhc  = ifelse(!is.na(preds_training), "training",
                                  ifelse(!is.na(preds_test), "test", NA))) |> 
  dplyr::select(country_year, country_id, year, folds:preds_type_fhc)

df_fh <-
  df_backsliding_all |> 
  left_join(df_combined) |> 
  group_by(country_name) |> 
  mutate(country = country_name) |> 
  arrange(country_name, year) |>
  group_by(year) |> 
  mutate(observed_mean = mean(e_fh_combined, na.rm = TRUE),
         observed_median = median(e_fh_combined, na.rm = TRUE),
         observed_sd = sd(e_fh_combined, na.rm = TRUE),
         prediction_mean = mean(preds_value_fhc, na.rm = TRUE),
         prediction_sd = sd(preds_value_fhc, na.rm = TRUE),
         prediction_median = median(preds_value_fhc, na.rm = TRUE)) 

# Cleaning up
rm(df_combined, df_outcome_2000,preds_training, preds_test,
   h2o_rf_fh, train_h2o, test_h2o, backsliding_test_h2o, 
   backsliding_test_ids, backsliding_training, backsliding_training_h2o, 
   backsliding_training_ids, df_backsliding_all, df_backsliding,
   ids, n_features, predictors, preds, outcome, df_outcome)

# Prepare the Predicted and Observed Polyarchy Values for Table 1
df_table_fh <-
  df_fh |> 
  filter(year > 2000) |> 
  dplyr::select(year, observed_mean, prediction_mean) |> 
  group_by(year) |> 
  distinct() |> 
  arrange(year)

## Export the observed and predicted Polyarchy values for Table 1
write_csv(df_table_fh, "data/output/fh_predictions.csv")
rm(df_table_fh)

# Graphs
# Calculate the mean and standard error of the predicted values by year
df_ci <- 
  df_fh |>
  group_by(year) |>
  summarize(prediction_mean = mean(preds_value_fhc, na.rm = TRUE),
            se = sd(preds_value_fhc, na.rm = TRUE) / sqrt(n())) |>
  filter(year > 2000) |>
  mutate(ci = 1.96 * se)

## Line for predicted vs observed in and out of sample 
fig4_panel1 <-
  ggplot(df_fh, aes(x = year)) +
  geom_line(aes(y = observed_mean, color = "Observed Mean")) +
  geom_line(aes(y = prediction_mean, color = "Predicted Mean")) +
  geom_ribbon(data = df_ci, aes(ymin = prediction_mean - ci, 
                                ymax = prediction_mean + ci, 
                                fill = "95% CI"), alpha = 0.3) +
  scale_color_manual(values = c("Predicted Mean" = "orange3", 
                                "Observed Mean" = "dodgerblue4")) +
  scale_fill_manual(values = "orange3") +
  theme_minimal() + ylim(0,1)  + theme_bw() + 
  theme(legend.position = "none") + 
  labs(title = "Global mean for observed and predicted Polyarchy Freedom House",
       y = "Freedom House",
       x = "Year") +
  scale_x_continuous(guide = guide_axis(angle = 45), breaks=seq(1900, 2022, 5), limits = c(1900,2022)) 

## Scatter plot of predicted vs observed for 2022-2020
## Preparing the data set for the graph
df_delta_plot <- 
  df_fh |>
  dplyr::filter(year %in% c(2000,2022)) |> 
  dplyr::select(country_name, country_text_id, year, 
                e_fh_combined, preds_value_fhc) |>
  group_by(country_text_id) |> 
  mutate(delta_fhc = e_fh_combined -lag(e_fh_combined),
         delta_osm = preds_value_fhc-lag(preds_value_fhc)) |> 
  mutate(label = ifelse(abs(delta_fhc) >= 0.10, country_text_id,
                        ifelse(abs(delta_osm) > 0.10, country_text_id, NA_character_)))

## Generating the graph
fig4_panel2 <-
  df_delta_plot |> 
  ggplot(aes(x = delta_osm, y = delta_fhc)) +
  geom_jitter() + ylim(-0.5,0.5) + xlim(-0.5,0.5) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray") +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "gray") +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5, color = "gray") +
  theme_bw() +
  labs(title = "Change scores, 2022-2000",
       y = "Freedom House, 2022-2000",
       x = "OSM, 2022-2000") +
  geom_text_repel(aes(delta_osm, delta_fhc, label = label), 
                  color = "black", check_overlap = TRUE)

# Add the marginal distribution to panel 2
fig4_panel2 <- ggMarginal(fig4_panel2, type = "density", 
                          color = "grey80", fill = "grey80")

# Produce the figure 
fig4_panel1 + fig4_panel2 + plot_layout(widths = c(2, 1))

rm(df_ci, df_polyarchy, df_delta_plot)
