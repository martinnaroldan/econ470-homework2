# setup.R - Load all required libraries
required_packages <- c("tidyverse", "readr", "dplyr", "ggplot2")

# Install missing packages automatically
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
lapply(required_packages, library, character.only = TRUE)
