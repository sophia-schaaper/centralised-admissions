library(readr)
library(stringr)
library(dplyr)
library(haven)
library(tidyr)
library(cobalt)
library(MatchIt)
library(tidyverse)

setwd("C:/Users/sophi/Documents/LSE/Education/school_segregation/code/matching")

# Read Data ----
## Years 1992-95 ----
years <- 1992:1995

data_files <- c("1992" = "AGENCY92.DAT", "1993" = "AGENCY93.DAT",
                "1994" = "Ccdagn94.dat", "1995" = "Ccdagn95.dat")

for (yr in years) {
  folder <- paste0(yr, "-", substr(yr + 1, 3, 4))
  
  layout_file <- file.path(folder, sprintf("pau%02dlay.txt", yr %% 100))
  data_file <- file.path(folder, data_files[as.character(yr)])
  
  layout_lines <- readLines(layout_file)
  
  layout_df <- str_match(layout_lines,
                         "^\\s*([A-Z0-9\\+]+)\\s+[A-Z]+\\s+(\\d{3})-(\\d{3})"
                         ) %>%as.data.frame()
  
  layout_df <- layout_df[!is.na(layout_df$V1),]
  
  variable <- layout_df$V2
  start <- as.integer(layout_df$V3)
  end <- as.integer(layout_df$V4)
  
  positions <- fwf_positions(start, end, col_names = variable)
  df <- read_fwf(data_file, col_positions = positions)
  message(sprintf("year %d done, %d rows", yr, nrow(df)))
  assign(paste0("ccd_agency_", yr), df)
}


## Years 96-97 ----
years <- 1996:1997

data_files <- c("1996" = "Ccdagn96.dat", "1997" = "Ccdagn97.dat")

for (yr in years) {
  folder <- paste0(yr, "-", substr(yr + 1, 3, 4))
  
  layout_file <- file.path(folder, sprintf("pau%02dlay.txt", yr %% 100))
  data_file <- file.path(folder, data_files[as.character(yr)])
  
  layout <- readLines(layout_file)
  
  var_lines <- grep("^[A-Z0-9\\+]+\\s+[0-9\\*\\+]+\\s+\\d{3}-\\d{3}",
                    layout, value = TRUE)
  
  variable <- str_extract(var_lines, "^[A-Z0-9\\+]+")
  start <- as.integer(str_extract(var_lines, "\\d{3}(?=-)"))
  end <- as.integer(str_extract(var_lines, "(?<=-)\\d{3}"))
  
  positions <- fwf_positions(start, end, col_names = variable)
  df <- read_fwf(data_file, col_positions = positions)
  message(sprintf("year %d done, %d rows", yr, nrow(df)))
  assign(paste0("ccd_agency_", yr), df)}


## Years 98 and 99 ----
ccd_agency_1998 <- read_sas("1998-99/ag981c.sas7bdat")
ccd_agency_1999 <- read_sas("1999-00/ag991b.sas7bdat")

years <- 1992:1999
all_data <- list()

for (yy in years) {
  suffix <- substr(yy, 3, 4)
  vars <- c("LEAID", paste0("NAME", suffix), paste0("TYPE", suffix),
            paste0("MSC", suffix), paste0("SCH", suffix), 
            paste0("TEACH", suffix), paste0("TOTGUI", suffix))
  
  df_name <- paste0("ccd_agency_", yy)
  df <- get(df_name)
  df <- df %>% mutate(LEAID = as.character(LEAID))  # ensure LEAID retains leading zeros
  df_filtered <- df[, vars]
  df_filtered$year <- yy
  
  all_data[[as.character(yy)]] <- df_filtered}

panel_df <- dplyr::bind_rows(all_data)

# ############ AVERAGE ----
#to numeric
numeric_vars_with_suffix <- unlist(lapply(years, function(yy) {
  suffix <- substr(yy, 3, 4)
  paste0(c("SCH", "TEACH", "TOTGUI"), suffix)}))

panel_df[numeric_vars_with_suffix] <- lapply(
  panel_df[numeric_vars_with_suffix],
  function(x) as.numeric(as.character(x)))

#MSC and TYPE to numeric, to avoid onflicts
categorical_vars_with_suffix <- unlist(lapply(years, function(yy) {
  suffix <- substr(yy, 3, 4)
  paste0(c("MSC", "TYPE"), suffix)}))

panel_df[categorical_vars_with_suffix] <- lapply(
  panel_df[categorical_vars_with_suffix],
  function(x) as.numeric(as.character(x)))

#pivot
panel_long <- panel_df %>%
  pivot_longer(cols = matches("MSC|TYPE|SCH|TEACH|TOTGUI"),
               names_to = c(".value", "year_suffix"),
               names_pattern = "([A-Z]+)([0-9]{2})") %>%
  mutate(year = as.numeric(paste0("19", year_suffix))) %>%
  select(LEAID, year, MSC, TYPE, SCH, TEACH, TOTGUI)

#summ if varied per year, choose most common
MSC_mode <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA_real_)
  tab <- table(ux)
  most_common <- names(tab)[tab == max(tab)]
  as.numeric(most_common[1])  # break ties by picking first
}

TYPE_mode <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA_real_)
  tab <- table(ux)
  most_common <- names(tab)[tab == max(tab)]
  as.numeric(most_common[1])  # break ties by picking first
}


district_avg <- panel_long %>%
  group_by(LEAID) %>%
  summarise(MSC  = MSC_mode(MSC),
            TYPE = TYPE_mode(TYPE),
            SCH  = mean(SCH, na.rm = TRUE),
            TEACH = mean(TEACH, na.rm = TRUE),
            TOTGUI = mean(TOTGUI, na.rm = TRUE))

## Assign T status
target_leaids <- c("3411340",  # Newark
                   "1709930",  # Chicago
                   "0803360",  # Denver
                   "2502790",  # Boston
                   "1100030",  # DC
                   "1804770",  # Indianapolis
                   "0634410",  # San Francisco
                   "3620580")  # NYC Citywide (classification pre-2003

district_avg <- district_avg %>%
  mutate(G = ifelse(LEAID %in% target_leaids, 1, 0))

## Now keep only subset - this was to see stats of treatment and 
#choose control accordingly

#district_avg %>%
#  filter(G == 1) %>%
#  select(MSC, TYPE, SCH, TEACH, TOTGUI) %>%
#  summary()

# get SCH/TEACH/TOTGUI ranges from treated
sch_min <- 75

teach_min <- 15000

totgui_min <- 350

# keep only relevant units (treated and relevant controls)
district_avg_sub <- district_avg %>%
  filter(G == 1 | #either treated group
           (MSC == 1 & TYPE == 1 & #or control need meet criteria
              SCH >= sch_min & TEACH >= teach_min & TOTGUI >= totgui_min))


# Check Balance on Covariates ----

balance <- bal.tab(G ~ SCH + TEACH + TOTGUI ,
                   data = district_avg_sub,
                   estimand = "ATT")

love.plot(balance)

#save relevant LEAIDs
leaid_keep <- district_avg_sub %>% 
  pull(LEAID) %>% 
  unique()
saveRDS(leaid_keep, "leaid_keep.rds")

# join to get district names for the LEAIDs you are keeping
# districts with G = 1
leaid_names_treated <- district_avg_sub %>%
  filter(G == 1) %>%
  select(LEAID) %>%
  distinct() %>%
  left_join(ccd_agency_1999 %>% select(LEAID, NAME99), by = "LEAID")

# districts with G = 0
leaid_names_control <- district_avg_sub %>%
  filter(G == 0) %>%
  select(LEAID) %>%
  distinct() %>%
  left_join(ccd_agency_1999 %>% select(LEAID, NAME99), by = "LEAID")

leaid_names <- bind_rows(leaid_names_treated,leaid_names_control) %>%
  arrange(LEAID)

saveRDS(leaid_names, "leaid_names.rds")





# Actual Matching ----
district_avg_clean <- district_avg %>% 
  drop_na(SCH, TEACH, TOTGUI)

district_avg_sub_clean <- district_avg_sub %>% 
  drop_na(SCH, TEACH, TOTGUI)

full_labeled <- district_avg_clean %>% 
  mutate(SampleGroup = factor("Unadjusted"))

subset_labeled <- district_avg_sub_clean %>% 
  mutate(SampleGroup = factor("Adjusted"))

combined_data <- bind_rows(full_labeled, subset_labeled)

balance_combined <- bal.tab(
  G ~ SCH + TEACH + TOTGUI,
  data = combined_data,
  estimand = "ATT",
  subclass = combined_data$SampleGroup,
  un = TRUE  # <-- this ensures unadjusted values are calculated
)

love.plot(balance_combined,
          stat = "mean.diffs",
          threshold = 0.1,
          stars = "std",
          var.order = "unadjusted")


#### Try with actual matching

library(MatchIt)

# 1. Clean data: drop NAs
district_avg_fixed <- district_avg %>%
  mutate(across(c(SCH, TEACH, TOTGUI), 
                ~ ifelse(is.na(.) | is.nan(.), mean(., na.rm = TRUE), .)))

district_avg_clean <- district_avg %>%
  drop_na(SCH, TEACH, TOTGUI)

# 2. Run matching
m.out <- matchit(G ~ SCH + TEACH + TOTGUI,
                 data = district_avg_fixed,
                 method = "nearest",   # nearest neighbor matching
                 ratio = 5)            # 1:1 matching

# 3. Extract matched data
matched_data <- match.data(m.out)



balance_matched <- bal.tab(m.out, estimand = "ATT", un = TRUE)

love.plot(balance_matched,
          stat = "mean.diffs",
          threshold = 0.1,
          stars = "std")





# 4. Check number of LEAIDs retained
matched_leaids <- matched_data$LEAID %>% unique()

length(matched_leaids)  # total number of unique districts kept

# Optionally: get counts by treatment status
table(matched_data$G)

leaid_keep <- matched_data %>%
  pull(LEAID) %>%
  unique()

#saveRDS(leaid_keep, "leaid_keep.rds")


