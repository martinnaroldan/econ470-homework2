# Load required libraries using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, 
               ggbeeswarm, MatchIt)

# Read data 
hcris.data.v1996 <- readRDS('data/output/HCRIS_Data_v1996.rds')
hcris.data.v2010 <- readRDS('data/output/HCRIS_Data_v2010.rds')
hcris.data <- readRDS('data/output/HCRIS_Data.rds')

# Filter data to include only years 2000-2015
hcris.data <- hcris.data %>%
  filter(year >= 2000, year <= 2015)

# Create summary objects for qmd
version.1996 <- hcris.data.v1996 %>% 
  group_by(year) %>% 
  summarize(count_1996 = n())

version.2010 <- hcris.data.v2010 %>% 
  group_by(year) %>% 
  summarize(count_2010 = n())

# Merge the two datasets on year
version.dat <- full_join(version.1996, version.2010, by = 'year')

# Modify v1996 data: Add missing columns for HVBP and HRRP payments
final.hcris.v1996 <- hcris.data.v1996 %>%
  mutate(hvbp_payment = NA, hrrp_payment = NA)

# Combine v1996 and v2010 HCRIS datasets and sort by provider_number and year
final.hcris <- rbind(final.hcris.v1996, hcris.data.v2010) %>%
  mutate(fy_end = mdy(fy_end), fy_start = mdy(fy_start),
         data_processed = mdy(data_processed), data_created = mdy(data_created),
         tot_discounts = abs(tot_discounts), hrrp_payment = abs(hrrp_payment)) %>%
  mutate(fyear = year(fy_end)) %>%
  arrange(provider_number, fyear) %>%
  select(-year) %>%
  filter(fyear >= 2000, fyear <= 2015)

hcris.data %>% ungroup() %>% summarize(count = n_distinct(provider_number))

# Create objects for qmd
## Count of hospitals (provider_number) by year
dup.count <- final.hcris %>%
  add_count(provider_number, fyear, name="total_reports") %>%
  mutate(dup_report = (total_reports > 1)) %>%
  group_by(fyear) %>%
  summarize(duplicates = sum(dup_report))

fig.dup <- dup.count %>%
  ggplot(aes(x = as.factor(fyear), y = duplicates, group = 1)) + geom_line() +
  labs(
    x = "Year",
    y = "Number of Hospitals",
    title = ""
  ) +
  theme_bw() + scale_y_continuous(limits = c(0, 300)) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

fig.unique <- hcris.data %>% group_by(year) %>%
  summarize(hosp_count = n()) %>%
  ggplot(aes(x = as.factor(year), y = hosp_count, group = 1)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Number of Hospitals",
    title = ""
  ) +
  theme_bw() +
  scale_y_continuous(labels = comma, limits = c(0, 6500))
