The scripts in this repository are numbered and should be executed based on the numeric order. 

- 01_preprocessing.R loads the main data set and the shared polity csv from the data folder and prepares the data for the random forest. This script is a helper, scripts 02-04 call on this script and provide important input in order for the script to function. Do not run this script independently. Scripts 02-04 use the source() function to make use of 01.
- 02_polyarchy.R predicts Polyarchy scores using a random forest model and produces data for Figure 1, Table 1, and Figure 2. 
- 03_polity2.R predicts Polity2 scores using a random forest model and produces data for Figure 1, Table 1, and Figure 3. 
- 04_polity2.R predicts combined Freedom House scores using a random forest model and produces data for Figure 1, Table 1, and Figure 4. 
- 05_heatmap.R uses the output from the previous scripts to generate a heatmap of VIP scores (Figure 1),
- 06_table.R uses the output from the previous scripts to generate the observed vs predicted columns in Table 1.

Scripts 02-04 require an installation of H20. 

The R libraries used:
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
