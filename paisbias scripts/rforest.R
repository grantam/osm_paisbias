#### By Grant Mitchell
#### Code last updated on 4/24
#### Data and linear modeling for the 

#### Packages

library(tidyverse)
library(haven)
library(foreign)
library(ggthemes)
library(vdemdata)
library(lme4)
library(readxl)
library(rstanarm)
library(Matrix)
library(zoo)
library(plm)
library(reghelper)
library(LongituRF)

#### functions

neg_binom <- function(bi_var) {
  counts <- numeric(length(bi_var))
  current_count <- 0
  for (i in seq_along(bi_var)) {
    if (bi_var[i] == 0) {
      current_count <- current_count + 1  
      counts[i] <- current_count
    } else {
      current_count <- 0
    }
  }
  
  return(counts)
}


impute <- function(df, var) {
  previous_value <- NA
  for (i in 1:nrow(df)) {
    if (is.na(df[i, var])) {
      df[i, var] <- previous_value
    } else {
      previous_value <- df[i, var]
    }
  }
  return(df)
}

#### Clean data

data <- vdem %>%
  mutate(turnover_test = ifelse(v2elturnhog == 0, 0, 1), COWcode = as.factor(COWcode), election = ifelse(is.na(v2eltype_0), 0, 1)) %>%
  replace_na(replace = list(turnover_test = 0, v2elturnhog = 0)) %>%
  mutate(v2elturnhog = as.factor(v2elturnhog), fix = as.factor(country_name)) %>%
  select(turnover_test, v2x_polyarchy, v2x_libdem, election, COWcode, v2elturnhog, year, fix, v2ellostsl, v2elsuffrage, e_p_polity, v2expathhg, v2ex_elechog, v2ex_elechos, v2exhoshog, ifs = country_text_id, v2expathhg, v2expathhs, v2x_freexp_altinf, v2xel_frefair, country_id) %>%
  group_by(COWcode) %>%
  mutate(last_elect = log(neg_binom(election) + 1), var_elect = var(last_elect), exec_chose = ifelse(v2exhoshog == 1, v2expathhs, v2expathhg-1), parli = ifelse(exec_chose == 6, 1, 0))

#### Lexical Data

lexical <- read_excel("C:/Users/mitchellg/Documents/R projects/osm_paisbias/LIED_6.6.xlsx") %>%
  rename(COWcode = cow) %>%
  mutate(COWcode = as.factor(COWcode))

data <- left_join(data, lexical, by = c("year", "COWcode")) %>%
  mutate(lexical_index = (lexical_index/6), lexical_index_plus = (lexical_index_plus/7))

data <- cbind(data, 
              datawizard::demean(data, 
                                 select = c("last_elect","v2ellostsl", "executive_elections", "legislative_elections", "multi-party_legislative_elections", "turnover_event", "competitive_elections", "v2elsuffrage", "e_p_polity", "political_liberties", "parli", "v2exhoshog", "v2x_freexp_altinf", "v2xel_frefair"),
                                 group = "COWcode"))

data <- data %>%
  ungroup() %>%
  mutate(across(
    starts_with(c(
      "turnover_event_within", 
      "legislative_elections_within", 
      "executive_elections_within", 
      "multi.party_legislative_elections_within", 
      "last_elect_within", 
      "competitive_elections_within",
      "v2elsuffrage_within",
      "political_liberties_within",
      "turnover_event_between", 
      "legislative_elections_between", 
      "executive_elections_between", 
      "multi.party_legislative_elections_between",
      "last_elect_between",
      "competitive_elections_between",
      "v2elsuffrage_between",
      "political_liberties_between",
      "parli_between",
      "parli_within",
      "v2exhoshog_within",
      "v2exhoshog_between",
      "v2x_freexp_altinf_within",
      "v2x_freexp_altinf_between",
      "v2xel_frefair_within",
      "v2xel_frefair_between"
    )),
    ~ (.-mean(., na.rm = TRUE)) / (2*(sd(., na.rm = TRUE)))
  ))

dat <- data %>%
  select(turnover_event,
         legislative_elections,
         executive_elections,
         `multi-party_legislative_elections`,
         last_elect, competitive_elections,
         v2elsuffrage,
         political_liberties,
         v2x_polyarchy, year, COWcode, countryn) %>%
  na.omit()


REmat <- matrix(NA, nrow = 196, ncol = 200)

for (i in 1:200) {
  
  variable_to_select <- dat %>%
  select(-COWcode, -year, -v2x_polyarchy, -countryn) %>%
  colnames()
  
  variables <- sample(variable_to_select, 3, replace = FALSE)
  
  sampled_data <- dat %>%
    group_by(COWcode) %>%
    sample_n(size = n(), replace = TRUE) %>%
    arrange(COWcode, year) %>%
    mutate(n_in_group = row_number()) %>%
    ungroup()
  
  X <- sampled_data %>%
    select(all_of(variables))
  
  list_dat <- list(
    X = as.matrix(X),
    Y = as.numeric(pull(sampled_data, v2x_polyarchy)),
    id = as.numeric(pull(sampled_data, COWcode)),
    Z = as.matrix(rep(1, nrow(sampled_data))),
    time = as.numeric(pull(sampled_data, n_in_group)))


  mert1 <- LongituRF::MERT(
    X = data.frame(list_dat$X),
    Y = list_dat$Y,
    id = list_dat$id,
    Z = list_dat$Z,
    time = list_dat$time,
    sto = 'OrnUhl')

REmat[, i] <- mert1$random_effects

}


REmat <- as.matrix(REmat)

REmat <- t(REmat)

REmat <- as.data.frame(REmat)


data_viz <- REmat %>%
  summarize_all(list(
    lower = ~quantile(., probs = 0.025, na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    upper = ~quantile(., probs = 0.975, na.rm = TRUE)
  ))

# Pivot to long format
data_viz_long <- data_viz %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"),
               names_sep = "_") %>%
  mutate(problem = ifelse(mean > upper | mean < lower, FALSE, TRUE),
         mean = as.numeric(mean),
         lower = as.numeric(lower),
         upper = as.numeric(upper),
         sig = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0), T, F))

uniquecow <- dat %>%
  group_by(COWcode) %>%
  summarise(name = first(countryn))

data_viz_long <- cbind(data_viz_long, uniquecow) %>%
  arrange(lower)  # Sort by mean in ascending order

# Plot
ggplot(data = data_viz_long) +
  geom_linerange(aes(xmin = lower, xmax = upper, y = reorder(name, mean), color = sig)) +  # Reorder variables based on mean
  geom_point(aes(x = mean, y = reorder(name, mean), color = sig)) +  # Reorder variables based on mean
  geom_vline(xintercept = 0, color = "darkblue") +
  theme_bw() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))


