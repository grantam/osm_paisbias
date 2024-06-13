## Title:      05_heatmap.R
## Author:     Daniel Weitzel
## Email:      daniel.weitzel@colostate.edu 
## Changed:    2023-10-31
## Purpose:    This script produces Figure 1

## Set the seed
set.seed(1904)

# Libraries
library("tidyverse")
library("viridis")

# Load the data
vip_polyarchy <- read_csv("data/output/vip_polyarchy.csv")
vip_fh        <- read_csv("data/output/vip_fh.csv")
vip_p2        <- read_csv("data/output/vip_p2.csv")

# Make data frame
df_vip <-
  vip_polyarchy |>  
  left_join(vip_fh) |> 
  left_join(vip_p2) |> 
  pivot_longer(cols = -variable,
               names_to = "Index",
               values_to = "Value") |> 
  rename(Variable = variable) |> 
  mutate(order = ifelse(Index == "Polyarchy", 1,
                        ifelse(Index == "Polity2", 2, 3)))
# Make heatmap
ggplot(df_vip, aes(y = reorder(Variable,Value), 
                   x = reorder(Index, order), 
                   fill = Value)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis") +
  scale_x_discrete(position = "top") +
  theme_bw() + 
  labs(y = "", x = "", fill = "Scaled Importance") +
  theme(legend.position = "bottom") 

rm(vip_polyarchy, vip_fh, vip_p2, df_vip)
