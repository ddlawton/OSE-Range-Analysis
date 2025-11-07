# ===========================
# Creating the senegal map show 
# casing regions used in this
# study
# ===========================

library(tidyverse)
library(janitor)
library(rnaturalearth)


# Get senegal data
senegal <- ne_states(country = "Senegal", returnclass = "sf") |> 
    clean_names()

