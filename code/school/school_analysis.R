
library(tidyverse)
library(readxl)
library(janitor)
library(did)
library(purrr)
library(tibble)
library(kableExtra)
library(segregation)
options(dplyr.summarise.inform = FALSE)

setwd("C:/Users/sophi/Documents/LSE/Education/school_segregation/data")

## Calc Measures ----

all_data <- read_csv("school_count_data.csv",
                     col_types = cols(NCESSCH = col_character(), 
                                      LEAID = col_character())) %>% 
  mutate(nonwhite = black + hispanic + asian + other)

### Calc Exp ----

district_exp <- all_data %>% 
  group_by(year, LEAID) %>% 
  summarise(exp_black = sum(black,    na.rm = TRUE) / sum(total, na.rm = TRUE),
            exp_hisp = sum(hispanic, na.rm = TRUE) / sum(total, na.rm = TRUE),
            exp_nonwhite = sum(nonwhite, na.rm = TRUE) / sum(total, na.rm = TRUE),
            .groups = "drop")

### Calc composition & representation ----  
school_panel <- all_data %>% 
  group_by(NCESSCH, LEAID, year, grade) %>% 
  summarise(
    black_share = sum(black,    na.rm = TRUE) / sum(total, na.rm = TRUE),
    hisp_share = sum(hispanic, na.rm = TRUE) / sum(total, na.rm = TRUE),
    nonwhite_share = sum(nonwhite, na.rm = TRUE) / sum(total, na.rm = TRUE),
    white_share = sum(white,    na.rm = TRUE) / sum(total, na.rm = TRUE),
    .groups = "drop") %>% 
  left_join(district_exp, by = c("LEAID", "year")) %>% 
  mutate(# representation ratios
    rep_black = black_share / exp_black,
    rep_hisp = hisp_share / exp_hisp,
    rep_nonwhite = nonwhite_share / exp_nonwhite)

### Calc mutual info local ----
long_counts <- all_data %>% 
  pivot_longer(cols = c(white, black, hispanic, asian, other),
               names_to  = "race",
               values_to = "n") %>% 
  filter(n > 0)

local_ls <- long_counts %>% 
  group_by(LEAID, year, grade) %>% 
  group_modify(~{
    # if only one race present in this district-grade-year, segregation = 0
    if (n_distinct(.x$race) < 2) {
      tibble(NCESSCH = unique(.x$NCESSCH),
             ls = 0)} else {
               mutual_local(.x,
                            group  = "race",
                            unit   = "NCESSCH",
                            weight = "n",
                            wide   = TRUE) %>% 
                 select(NCESSCH, ls)}
  }) %>% 
  ungroup()

school_panel <- school_panel %>% 
  left_join(local_ls, by = c("NCESSCH", "LEAID", "year", "grade"))

### Calc Shannon Entropy (Within-school diversity) ----
school_panel <- school_panel %>% 
  mutate(other_share = pmax(1 - white_share - black_share - hisp_share, 0)) %>% 
  rowwise() %>% 
  mutate(H_diversity = {
    p <- c_across(c(white_share, black_share,
                    hisp_share, other_share))
    p <- p[p > 0]          # drop zero cells
    -sum(p * log(p))       # natural-log entropy
  }) %>% 
  ungroup() %>% 
  select(-other_share)


school_panel <- school_panel %>% # make sure D == 0 if NA
  left_join(all_data %>% distinct(NCESSCH, grade, year, G, D),
            by = c("NCESSCH", "grade", "year")) %>% 
  mutate(D = replace_na(D, 0L))

g_year_lookup <- school_panel %>%  # get G_year, cohort treatment year
  group_by(NCESSCH, grade) %>% 
  summarise(G_year = if (sum(D) == 0) 0L else min(year[D == 1]),
            .groups = "drop")

school_panel <- school_panel %>% # create unit_id
  left_join(g_year_lookup, by = c("NCESSCH", "grade")) %>% 
  mutate(unit_id = as.integer(factor(paste(NCESSCH, grade, sep = "_"))))

write_csv(school_panel, "school_panel_measures.csv")


## Add Covariates ----
setwd("C:/Users/sophi/Documents/LSE/Education/school_segregation/data/clean")

### from cleaned data ----
years <- c(98, 99, 0:23)
years_padded <- sprintf("%02d", years)
df_names <- paste0("sy", years_padded, "_mem")

vars_interest <- c("TYPE", "FTE", "MEMBER", "ULOCAL", "LOCALE", 
                   "FRELCH", "REDLCH", "TOTFRL")

# Helper: robust var selector
get_vars_safe <- function(df, year_suffix, dataset_name) {
  out <- list()
  
  out$NCESSCH <- df$NCESSCH
  out$LEAID <- df$LEAID
  out$year <- if (as.integer(year_suffix) <= 23) 2000 + as.integer(year_suffix) 
  else 1900 + as.integer(year_suffix)
  
  for (var in vars_interest) {
    possible_names <- c(var, paste0(var, year_suffix))
    found_var <- possible_names[possible_names %in% names(df)]
    
    value <- if (length(found_var) > 0) {
      df[[found_var[1]]]
    } else if (var %in% c("ULOCAL", "LOCALE")) {
      # Try fallback between ULOCAL and LOCALE
      alt_var <- setdiff(c("ULOCAL", "LOCALE"), var)
      alt_found <- alt_var[alt_var %in% names(df)]
      if (length(alt_found) > 0) df[[alt_found[1]]] else {
        message(glue::glue("Variable {var} not found in {dataset_name}."))
        NA}
    } else {
      message(glue::glue("Variable {var} not found in {dataset_name}."))
      NA}
    
    if (var %in% c("ULOCAL", "LOCALE", "STATUS", "MIGRNT")) {
      out[[var]] <- as.character(value)
    } else {
      out[[var]] <- suppressWarnings(as.numeric(value))  # safe coercion
    }}
  
  as_tibble(out)
}

# Process and combine datasets
all_covariates <- map2_dfr(df_names, years_padded, function(name, yy) {
  file_path <- paste0("sy", yy, "_mem.csv")  # Adjust if needed
  if (file.exists(file_path)) {
    df <- read_csv(file_path) %>% select(where(~!is.list(.)))
    get_vars_safe(df, yy, name)
  } else {
    message(glue::glue("File {file_path} not found."))
    NULL
  }
})

### from newly downloaded data ----
# Utility: extract year from column names
extract_year <- function(name) {
  str_extract(name, "\\d{4}")
}

#### main covs ----
main_covs_path <- "../cov/schools/ELSI_csv_export_main_covs.csv"
main_covs_raw <- read_csv(main_covs_path, skip = 6) %>%
  clean_names()

main_covs_long <- main_covs_raw %>%
  pivot_longer(
    cols = matches("(school_type|magnet_school|charter_school|shared_time_school|fte|pupil_teacher|total_students).*\\d{4}"),
    names_to = "variable",
    values_to = "value") %>%
  mutate(year = extract_year(variable),
         variable = case_when(
           str_detect(variable, "school_type") ~ "TYPE",
           str_detect(variable, "magnet") ~ "MAGNET",
           str_detect(variable, "charter") ~ "CHARTER",
           str_detect(variable, "shared_time") ~ "SHARED_TIME",
           str_detect(variable, "fte") ~ "FTE",
           str_detect(variable, "pupil_teacher") ~ "PUPTEACH",
           str_detect(variable, "total_students") ~ "MEMBER",
           TRUE ~ NA_character_)) %>%
  filter(!is.na(variable)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(year = as.integer(year)) %>%
  select(ncessch = school_id_12_digit_nces_assigned_public_school_latest_available_year,
         year, TYPE, CHARTER, MAGNET, SHARED_TIME, FTE, PUPTEACH, MEMBER)

#### locale info ----
locale_path <- "../cov/schools/ELSI_csv_export_locale.csv"
locale_raw <- read_csv(locale_path, skip = 6) %>%
  clean_names()

locale_year_cols <- names(locale_raw)[str_detect
                                      (names(locale_raw), 
                                        "locale_public_school_\\d{4}_\\d{2}")]

locale_long <- locale_raw %>%
  pivot_longer(cols = all_of(locale_year_cols),
               names_to = "year", values_to = "LOCALE") %>%
  mutate(year = str_extract(year, "\\d{4}"), year = as.integer(year)) %>%
  select(ncessch = school_id_12_digit_nces_assigned_public_school_latest_available_year,
         year, LOCALE)


#### type/charter/magnet ----
type_path <- "../cov/schools/ELSI_csv_export_type.csv"
type_raw <- read_csv(type_path, skip = 6) %>%
  clean_names()

type_long <- type_raw %>%
  pivot_longer(
    cols = matches("(charter_school|magnet_school|shared_time_school|pupil_teacher).*\\d{4}"),
    names_to = "variable", values_to = "value") %>%
  mutate(year = extract_year(variable),
         variable = case_when(str_detect(variable, "charter") ~ "CHARTER",
                              str_detect(variable, "magnet") ~ "MAGNET",
                              str_detect(variable, "shared_time") ~ "SHARED_TIME",
                              str_detect(variable, "pupil_teacher") ~ "PUPTEACH",
                              TRUE ~ NA_character_)) %>%
  filter(!is.na(variable)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(year = as.integer(year)) %>%
  select(ncessch = school_id_12_digit_nces_assigned_public_school_latest_available_year,
         year, CHARTER, MAGNET, SHARED_TIME, PUPTEACH)

# Merge 
main_covs_long <- main_covs_long %>% filter(!is.na(ncessch))
type_long <- type_long %>% filter(!is.na(ncessch))
locale_long <- locale_long %>% filter(!is.na(ncessch))

combined_covs <- main_covs_long %>%
  full_join(type_long, by = c("ncessch", "year"), 
            suffix = c(".main", ".type")) %>%
  full_join(locale_long, by = c("ncessch", "year"))

extra_covs <- combined_covs %>%
  mutate(CHARTER = coalesce(CHARTER.main, CHARTER.type),
         MAGNET = coalesce(MAGNET.main, MAGNET.type),
         SHARED_TIME = coalesce(SHARED_TIME.main, SHARED_TIME.type),
         PUPTEACH = coalesce(PUPTEACH.main, PUPTEACH.type)) %>%
  select(ncessch, year, TYPE, FTE, MEMBER, CHARTER, MAGNET, 
         SHARED_TIME, PUPTEACH, LOCALE)

all_covariates <- all_covariates %>%
  left_join(extra_covs, by = c("NCESSCH" = "ncessch", "year")) %>%
  mutate(TYPE = coalesce(as.character(TYPE.x), as.character(TYPE.y)),
         FTE = coalesce(as.numeric(FTE.x), as.numeric(FTE.y)),
         MEMBER = coalesce(as.numeric(MEMBER.x), as.numeric(MEMBER.y)),
         LOCALE = coalesce(as.character(LOCALE.x), as.character(LOCALE.y))) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  
  mutate(ulocal_clean = if_else(str_detect(ULOCAL, "^\\d+$"), 
                                ULOCAL, NA_character_),
         locale_clean = if_else(str_detect(LOCALE, "^\\d+$"), 
                                LOCALE, NA_character_),
         locale_final = coalesce(ulocal_clean, locale_clean)) %>%
  select(-ulocal_clean, -locale_clean, -ULOCAL, -LOCALE) %>%
  rename_with(toupper)


#### NEED TO ADD pre 98?? ----

#### clean up ----
all_covariates <- all_covariates %>%
  # Handle special characters as missing
  mutate(across(c(TOTFRL, MEMBER, FRELCH, FTE, PUPTEACH),
                ~ na_if(as.character(.x), "–"))) %>%
  mutate(across(c(TOTFRL, MEMBER, FRELCH, FTE, PUPTEACH),
                ~ na_if(.x, "†"))) %>%
  # Coerce to numeric
  mutate(across(c(TOTFRL, MEMBER, FRELCH, FTE, PUPTEACH),
                ~ as.numeric(.x))) %>%
  
  # Safely impute PUPTEACH only when both FTE and MEMBER are valid and positive
  mutate(needs_impute_pt = ((is.na(PUPTEACH) | PUPTEACH == 0) &
                              !is.na(FTE) & is.finite(FTE) & FTE > 0 &
                              !is.na(MEMBER) & is.finite(MEMBER) & MEMBER > 0),
         PUPTEACH = if_else(needs_impute_pt, MEMBER / FTE, PUPTEACH)) %>%
  mutate(PUPTEACH = if_else(is.finite(PUPTEACH), PUPTEACH, NA_real_)) %>%
  select(-needs_impute_pt) %>%
  mutate(lunch_pct = if_else(!is.na(TOTFRL) & !is.na(MEMBER) & MEMBER > 0,
                             TOTFRL / MEMBER, NA_real_)) %>%
  ## actually we wanna add here if totfrl is missing than add up frlch and redlch
  # Recode CHARTER and MAGNET + missing dummies
  mutate(
    CHARTER = ifelse(CHARTER %in% c("1-Yes", "2-No"), CHARTER, NA_character_),
    MAGNET  = ifelse(MAGNET  %in% c("1-Yes", "2-No"), MAGNET,  NA_character_),
    CHARTER_miss = as.integer(is.na(CHARTER)),
    MAGNET_miss  = as.integer(is.na(MAGNET)),
    CHARTER = replace_na(CHARTER, "2-No"),
    MAGNET  = replace_na(MAGNET,  "2-No")) %>%
  mutate(LOCALE_FINAL = ifelse(str_detect(LOCALE_FINAL, "^\\d+$"),
                               LOCALE_FINAL, NA_character_)) %>%
  
  group_by(NCESSCH) %>%
  arrange(YEAR) %>%
  fill(CHARTER, CHARTER_miss, MAGNET,  MAGNET_miss, LOCALE_FINAL, TYPE,
       .direction = "downup") %>%
  ungroup() %>%
  select(-SHARED_TIME)

#### final Clean Up ----
# Ensure key variables are properly formatted
all_covariates <- all_covariates %>%
  mutate(NCESSCH = as.character(NCESSCH),
         YEAR = as.integer(YEAR)) %>%
  group_by(NCESSCH, YEAR) %>%
  summarise(
    across(where(is.numeric),  ~ max(.x, na.rm = TRUE)),
    across(where(~ !is.numeric(.)), first),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA_real_, .x)))


write_csv(all_covariates, "../all_covariates_school.csv")


school_panel <- school_panel %>%
  mutate(NCESSCH = as.character(NCESSCH),
         year = as.integer(year))

# Merge Cov and Measures ----
school_panel_full <- school_panel %>% 
  left_join(all_covariates,
            by = c("NCESSCH", "year" = "YEAR")) %>%
  mutate(LEAID = coalesce(LEAID.x, LEAID.y)) %>%
  select(-LEAID.x, -LEAID.y) 

school_panel_full <- school_panel_full %>%
  mutate(LOCALE12 = case_when(
    LOCALE_FINAL %in% 1:8 ~ case_when(
      LOCALE_FINAL == 1 ~ 11L, LOCALE_FINAL == 2 ~ 12L,
      LOCALE_FINAL == 3 ~ 21L, LOCALE_FINAL == 4 ~ 22L,
      LOCALE_FINAL == 5 ~ 23L, LOCALE_FINAL == 6 ~ 31L,
      LOCALE_FINAL == 7 ~ 41L, LOCALE_FINAL == 8 ~ 42L
    ),
    LOCALE_FINAL %in% 11:43 ~ as.integer(LOCALE_FINAL),
    TRUE ~ NA_integer_
  )) %>% # add TYPE to the fill
  group_by(NCESSCH) %>%
  arrange(year) %>%
  fill(CHARTER, CHARTER_miss,
       MAGNET,  MAGNET_miss,
       LOCALE_FINAL, TYPE,
       .direction = "downup") %>%
  ungroup()

#colnames(school_panel_full)

write_csv(school_panel_full, "../school_panel_cov.csv")
#school_panel_full <- read.csv("../school_panel_cov.csv")
