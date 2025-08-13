# ---- Data Cleaning Script ----

library(tidyverse)
library(haven)
library(glue)

fix_leaid <- function(df) {
  df %>%
    mutate(
      LEAID = str_pad(as.character(LEAID), 7, pad = "0"))
}

setwd("C:/Users/sophi/Documents/LSE/Education/NCES - CCD")

leaid_keep <- readRDS("../school_segregation/code/matching/leaid_keep.rds")
nyc_known_leaids <- c("3600075",  # NYC Alternative HS District
                      "3600076", "3600077", "3600078", "3600079", "3600081", 
                      "3600083", "3600084","3600085", "3600086", "3600087", 
                      "3600088", "3600090", "3600091", "3600092","3600094", 
                      "3600095", "3600096", "3600097", "3600098", "3600099", 
                      "3600100","3600101", "3600102", "3600103", "3600119", 
                      "3600120", "3600121", "3600122","3600123", "3600135", 
                      "3600151", "3600152", "3600153")

## ---- 1992 - 1997 ----

# here we dont have grade disaggregated info, but its not needed cause 
# its all pre treatment, so we just copy in ecah grade row the school info

pre_years <- read_csv("1992-97/ELSI_csv_export_6388859020809244005165.csv", 
                      skip = 6)

grade12_cols <- grep("^Grade 12 Students \\[Public School\\]", names(pre_years), 
                     value = TRUE)

race_cols <- grep(
  "^(American|Asian|Hispanic|Black|White).*Students \\[Public School\\] \\d{4}-\\d{2}$",
                  names(pre_years), value = TRUE)

clean_pre <- pre_years %>%
  rename(NCESSCH = 
           `School ID (12-digit) - NCES Assigned [Public School] Latest available year`,
         LEAID   = 
           `Agency ID - NCES Assigned [Public School] Latest available year`,
         STATE   = `State Name [Public School] Latest available year`,
         SCHNAME = `School Name`) %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH)) %>%
  mutate(LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                           LEAID == "4700148" ~ "4702940",  # Memphis
                           LEAID == "2601103" ~ "2612000",  # Detroit
                           TRUE ~ LEAID)) %>%

  filter(LEAID %in% leaid_keep) %>% #filter for relevant districts
  mutate(across(all_of(grade12_cols), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  filter(rowSums(across(all_of(grade12_cols), #keep only schools with grade 12
                        ~ replace_na(.x, 0)), na.rm = TRUE) > 0 ) %>%

  pivot_longer(cols = all_of(race_cols), names_to = c("race", "year"),
               names_pattern = "(.*) Students \\[Public School\\] (\\d{4})-\\d{2}",
               values_to = "n", values_drop_na = TRUE) %>%

  mutate(year = as.integer(year) + 1,
         race = str_replace_all(race, c(
           "American Indian/Alaska Native" = "other",
           "Asian or Asian/Pacific Islander" = "asian",
           "Black or African American" = "black",
           "Hispanic" = "hispanic",
           "White" = "white")),                    # standardize race columns
    n = as.numeric(gsub("[^0-9.]", "", n))) %>%

  pivot_wider(id_cols = c(NCESSCH, LEAID, year, SCHNAME, STATE), 
              names_from = race, values_from = n, values_fill = 0)%>%
  fix_leaid() 

clean_panel <- clean_pre %>%
  mutate(total = white + black + hispanic + asian + other) %>%
  crossing(grade = 9:12) %>%
  select(NCESSCH, LEAID, year, grade, white, black, hispanic, 
         asian, other, total)

write_csv(clean_panel, "../school_segregation/data/clean/pre1998_mem.csv")

## ---- 1998 - 2006 ----

years <- 1998:2006 # three datasets per year to aggregate
folders <- paste0(years, "-", substr(years + 1, 3, 4))

# to generate file name to load
suffixes <- sprintf("%02d", years %% 100)  # "98", "99", ..., "06"
base_names <- c("sc981c", "sc991b", "sc001a", "sc011a", "sc021a", 
                "sc031a", "sc041b", "sc051a", "sc061c")
state_codes <- c("ai", "ow", "kn") # datasets per state

# variables that never get suffixes
fixed_vars <- c("LEAID", "NCESSCH")
# variables that get year suffix:
yearly_vars <- c("TYPE", "STATUS", "FTE", "LEVEL", "FRELCH", "REDLCH", 
                 "TOTFRL", "MIGRNT", "MEMBER")


# plus the LOCALE ULOCAL possibilities (with suffix!)
possible_locale_bases <- c("LOCALE", "ULOCAL")

# plus the long race/grade list
race_grade_vars <- c("G09","AM09M","AM09F","AM09U","AS09M","AS09F","AS09U",
                     "HI09M","HI09F","HI09U","BL09M","BL09F","BL09U","WH09M",
                     "WH09F","WH09U","G10","AM10M","AM10F","AM10U","AS10M",
                     "AS10F","AS10U","HI10M","HI10F","HI10U","BL10M","BL10F",
                     "BL10U","WH10M","WH10F","WH10U","G11","AM11M","AM11F",
                     "AM11U","AS11M","AS11F","AS11U","HI11M","HI11F","HI11U",
                     "BL11M","BL11F","BL11U","WH11M","WH11F","WH11U","G12",
                     "AM12M","AM12F","AM12U","AS12M","AS12F","AS12U","HI12M",
                     "HI12F","HI12U","BL12M","BL12F","BL12U","WH12M","WH12F",
                     "WH12U","AMALM","AMALF","AMALU","ASIAN","ASALM","ASALF",
                     "ASALU","HISP","HIALM","HIALF","HIALU","BLACK","BLALM",
                     "BLALF","BLALU","WHITE","WHALM","WHALF","WHALU","TOTETH",
                     "PUPTCH")

full_vars <- c(yearly_vars, race_grade_vars)
yearly_data <- list()

#### ---- process loop ----
for (i in seq_along(years)) {
  
  yr <- years[i]
  folder <- folders[i]
  suffix <- suffixes[i]
  basefile <- base_names[i]
  
  year_vars <- c(fixed_vars, paste0(full_vars, suffix))
  possible_locale_vars <- paste0(possible_locale_bases, suffix)
  
  df_list <- list()
  for (st in state_codes) {
    fn <- file.path(folder, paste0(basefile, st, ".sas7bdat"))
    if (!file.exists(fn)) {warning(glue("File {fn} does not exist, skipping."))
      next}
    
    df <- read_sas(fn)
    if (!"LEAID" %in% names(df)) {
      warning(glue("LEAID missing in {fn}, skipping.")) # for robustness
      next}
    
    nyc_found <- unique(df$LEAID[df$LEAID %in% nyc_known_leaids])
    if (length(nyc_found) > 0) {
      message(glue("NYC subdistricts found and converted in {fn}: 
                   {paste(nyc_found, collapse = ', ')}"))
      df <- df %>%
        mutate(LEAID = if_else(LEAID %in% nyc_known_leaids, 
                               "3620580", LEAID))}
    
    df <- df %>% filter(LEAID %in% leaid_keep) # keep only relevant districts
    
    locale_var <- intersect(possible_locale_vars, names(df))
    if (length(locale_var) == 0) {
      warning(glue("No LOCALE or ULOCAL with suffix found in {fn}. 
                   Setting LOCALE to NA."))
      df$LOCALE <- NA} else {
        df <- df %>% mutate(LOCALE = !!sym(locale_var[1]))}
    
    # for schools that offer at least up to 12th grade
    gshi_var <- paste0("GSHI", suffix)
    if (gshi_var %in% names(df)) {
      df <- df %>% filter(!!sym(gshi_var) >= 12)} else {
        warning(glue("Missing {gshi_var} in {fn}, skipping GSHI filter."))}
    
    missing_vars <- setdiff(year_vars, names(df))
    if (length(missing_vars) > 0) {
      warning(glue("In {fn}, could not find variables: 
                   {paste(missing_vars, collapse=', ')}"))}
    
    keep_vars <- intersect(year_vars, names(df))
    keep_vars <- union(keep_vars, "LOCALE")
    df <- df %>% select(all_of(keep_vars))
    df_list[[st]] <- df}
  
  year_df <- bind_rows(df_list) # bind all states of year
  year_df <- year_df %>% 
    mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH)) %>%
    fix_leaid()
  
  
  ### create grade-by-race totals
  
  grade_levels <- c("09", "10", "11", "12")
  race_prefixes <- c("AM", "AS", "HI", "BL", "WH")
  sex_suffixes <- c("M", "F", "U")
  
  for (grade in grade_levels) {
    for (race in race_prefixes) {
      male_var <- paste0(race, grade, "M", suffix)
      female_var <- paste0(race, grade, "F", suffix)
      unknown_var <- paste0(race, grade, "U", suffix)
      new_var <- paste0(race, grade)
      
      components <- list()
      if (male_var %in% names(year_df)) {
        components[[1]] <- year_df[[male_var]]
      } else { components[[1]] <- 0 }
      if (female_var %in% names(year_df)) {
        components[[2]] <- year_df[[female_var]] 
      } else { components[[2]] <- 0 }
      if (unknown_var %in% names(year_df)) { 
        components[[3]] <- year_df[[unknown_var]] 
      } else { components[[3]] <- 0 }
      
      if (any(c(male_var, female_var, unknown_var) %in% names(year_df))) {
        year_df[[new_var]] <- rowSums(do.call(cbind, components), na.rm = TRUE)
      } else {
        warning(
          glue("No sex-disaggregated fields found for {new_var} in year {yr}."))
      }}}
  
  # rm sex-disaggregated race-grade vars
  
  cols_to_remove <- expand.grid(race = race_prefixes, grade = grade_levels, 
                                sex = sex_suffixes) %>%
    mutate(var = paste0(race, grade, sex, suffix)) %>%
    pull(var)
  
  existing_cols_to_remove <- intersect(cols_to_remove, names(year_df))
  total_race_sex_vars <- expand.grid(
    race = c("AMAL", "ASAL", "HIAL", "BLAL", "WHAL"),
    sex = c("M", "F", "U")) %>%
    mutate(var = paste0(race, sex, suffix)) %>%
    pull(var)
  
  existing_total_race_sex_vars <- intersect(total_race_sex_vars, names(year_df))
  year_df <- year_df %>% select(-all_of(existing_total_race_sex_vars))
  year_df <- year_df %>% select(-all_of(existing_cols_to_remove))
  
  yearly_data[[as.character(yr)]] <- year_df
  assign(glue("sy{suffix}_mem"), year_df, envir = .GlobalEnv)
  write.csv(year_df, 
            file = glue("../school_segregation/data/clean/sy{suffix}_mem.csv"), 
            row.names = FALSE)
  
  
  message(glue("Processed year {yr} with {nrow(year_df)} rows."))}

## ---- 2007 - 2013 ----

# to gen file names
years <- c("2007-08", "2008-09", "2009-10", "2010-11", 
           "2011-12", "2012-13", "2013-14")
files <- c("sc071b", "sc081b", "sc092a", "sc102a", 
           "sc111a_supp", "sc122a", "sc132a")

grade_levels <- c("09", "10", "11", "12")
race_codes <- c("AM", "AS", "HI", "BL", "WH", "HP", "TR")
sex_suffixes <- c("M", "F", "U")

#### ---- process loop ----
for (i in seq_along(years)) {
  year_folder <- years[i]
  filename <- files[i]
  year_suffix <- gsub("-", "", year_folder)
  yy <- substr(year_suffix, 3,4)
  gshi_col <- paste0("GSHI", yy) 
  
  cat("Processing year:", year_folder, "\n")
  
  file_path <- file.path(year_folder, paste0(filename, ".sas7bdat"))
  df <- haven::read_sas(file_path) %>% 
    mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))%>%
    fix_leaid()
  
  nyc_found <- unique(df$LEAID[df$LEAID %in% nyc_known_leaids])
  if (length(nyc_found) > 0) {
    message(glue::glue("NYC subdistricts found and converted in {file_path}: 
                       {paste(nyc_found, collapse = ', ')}"))
    df <- df %>% mutate(LEAID = 
                          if_else(LEAID %in% nyc_known_leaids, 
                                  "3620580", LEAID))}
  
  #deal w memphis merger w shelby county in 2013
  df <- df %>%
    mutate(LEAID = if_else(LEAID == "4700148", "4702940", LEAID))
  if ("4700148" %in% df$LEAID) {
    message(glue::glue("4700148 found in {year_folder}, recoded to 4702940"))}

  df <- df %>% filter(LEAID %in% leaid_keep)

  # handle variation of GSHI col!!
  if (gshi_col %in% names(df)) { 
    message(paste0("Using column: ", gshi_col))
    df <- df %>% mutate(GSHI_num = suppressWarnings
                        (as.numeric(.data[[gshi_col]])))
  } else if ("GSHI" %in% names(df)) {
    message("Using column: GSHI")
    df <- df %>% mutate(GSHI_num = suppressWarnings(as.numeric(GSHI)))
  } else {
    stop(paste0("Neither ", gshi_col, " nor GSHI found in dataframe"))}
  
  # filter high schools
  df <- df %>% filter(GSHI_num >= 12)
  
  var_bases <- c("NCESSCH", "LEAID", "TYPE", "STATUS","ULOCAL", "FTE", "LEVEL", 
                 "FRELCH", "REDLCH", "TOTFRL", "MEMBER",
                 
                 "G09", "G10", "G11", "G12",
                 
                 "AM09M","AM09F","AS09M","AS09F","HI09M","HI09F",
                 "BL09M","BL09F","WH09M","WH09F","HP09M","HP09F","TR09M","TR09F",
                 
                 "AM10M","AM10F","AS10M","AS10F","HI10M","HI10F",
                 "BL10M","BL10F","WH10M","WH10F","HP10M","HP10F","TR10M","TR10F",
                 
                 "AM11M","AM11F","AS11M","AS11F","HI11M","HI11F",
                 "BL11M","BL11F","WH11M","WH11F","HP11M","HP11F","TR11M","TR11F",
                 
                 "AM12M","AM12F","AS12M","AS12F","HI12M","HI12F",
                 "BL12M","BL12F","WH12M","WH12F","HP12M","HP12F","TR12M","TR12F")
  
  sex_vars <- expand.grid(
    race = race_codes,
    grade = grade_levels,
    sex = sex_suffixes) %>%
    mutate(var = paste0(race, grade, sex)) %>%
    pull(var)
  
  vars_to_keep <- c()
  
  for (v in var_bases) {
    if (v %in% sex_vars) {
      # handle both with and w/o suffix variations!
      suffixed <- paste0(v, yy)
      if (v %in% names(df)) {vars_to_keep <- c(vars_to_keep, v)
      } else if (suffixed %in% names(df)) {
        vars_to_keep <- c(vars_to_keep, suffixed)} else {
          message(paste0("Sex var ", v, " or ", suffixed, 
                         " not found in ", year_folder))
        }} else {
          base <- v
          suffixed <- paste0(v, yy)
          
          if (base %in% names(df)) {vars_to_keep <- c(vars_to_keep, base)
          } else if (suffixed %in% names(df)) {
            vars_to_keep <- c(vars_to_keep, suffixed)} else {
              message(paste0("Variable ", base, " or ", suffixed, 
                             " not found in ", year_folder))}}}
  
  df <- df %>% select(all_of(vars_to_keep))
  #rename year-suffixed race-grade-sex vars by stripping only at the END 
  #avoiding problem of stripping 09 also removing from grade
  names(df) <- gsub(paste0("(?<=^[A-Z]{2}\\d{2}[MFU])", yy, "$"), "", 
                    names(df), perl = TRUE)
  
  grade_levels <- c("09", "10", "11", "12")
  race_codes <- c("AM", "AS", "HI", "BL", "WH", "HP", "TR")
  sex_suffixes <- c("M", "F", "U")
  
  # try with suffic first
  suffix_check <- paste0("(", paste(race_codes, collapse = "|"), ")",
                         "(", paste(grade_levels, collapse = "|"), ")",
                         "(", paste(sex_suffixes, collapse = "|"), ")",
                         yy, "$")
  
  # then w/o suffix
  nosuffix_check <- paste0("(", paste(race_codes, collapse = "|"), ")",
                           "(", paste(grade_levels, collapse = "|"), ")",
                           "(", paste(sex_suffixes, collapse = "|"), ")$")
  
  # check for either pattern
  has_sex_vars <- any(grepl(suffix_check, names(df))) || any(grepl
                                                             (nosuffix_check, 
                                                               names(df)))
  
  if (has_sex_vars) {
    message("Aggregating race variables per grade...")
    
    for (grade in grade_levels) {
      for (race in race_codes) {
        with_suffix <- paste0(race, grade, sex_suffixes, yy)
        no_suffix   <- paste0(race, grade, sex_suffixes)
        
        present_vars <- c(
          with_suffix[with_suffix %in% names(df)],
          no_suffix[no_suffix %in% names(df)])
        
        if (length(present_vars) > 0) {
          new_var <- paste0(race, grade)
          df[[new_var]] <- rowSums(df[, present_vars], na.rm = TRUE)
          df <- df %>% select(-all_of(present_vars))}}}
  } else {message("No sex-disaggregated race-grade variables to aggregate.")}
  
  df <- df %>%
    mutate(ASIAN = rowSums(select(., any_of(c("AS09", "AS10", "AS11", "AS12"))), 
                           na.rm = TRUE),
           HISP  = rowSums(select(., any_of(c("HI09", "HI10", "HI11", "HI12"))), 
                           na.rm = TRUE),
           BLACK = rowSums(select(., any_of(c("BL09", "BL10", "BL11", "BL12"))), 
                           na.rm = TRUE),
           WHITE = rowSums(select(., any_of(c("WH09", "WH10", "WH11", "WH12"))), 
                           na.rm = TRUE))
  
  # recode missing values, kinda unneeded cause also do this later
  df <- df %>%
    mutate(across(everything(), ~ if (is.numeric(.)) {
      replace(., . %in% c(-1, -2, -9), NA_real_)} else {
        replace(., . %in% c("M", "N", "-1", "-2", "-9"), NA_character_)}))
  
  # store
  assign(paste0("sy", yy, "_mem"), df)
  write.csv(df, file = paste0("../school_segregation/data/clean/sy", yy, 
                              "_mem.csv"), row.names = FALSE)
  
  
  rm(df)}

## ---- 2014 - 2015 ----

# these kinda diff from 2007-13 and from 2016-23 so gotta do these 2 years sep.
# also datasets now start getting separated into three info:
# directory, membership, and lunch program 

## 2014
dir_1415 <- read_sas("2014-15/ccd_sch_029_1415_w_0216161a.sas7bdat") %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH),
         LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                      LEAID == "4700148" ~ "4702940", TRUE ~ LEAID)) %>%
  filter(GSHI >= 12, LEAID %in% leaid_keep)

memb_1415 <- read_sas("2014-15/ccd_sch_052_1415_w_0216161a.sas7bdat") %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))

lunch_1415 <- read_sas("2014-15/ccd_sch_033_1415_w_0216161a.sas7bdat") %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))

## 2015
dir_1516 <- read_csv("2015-16/ccd_sch_029_1516_w_2a_011717.csv") %>%
  mutate(LEAID = as.character(LEAID),NCESSCH = as.character(NCESSCH),
    LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                      LEAID == "4700148" ~ "4702940", TRUE ~ LEAID)) %>%
  filter(GSHI >= 12, LEAID %in% leaid_keep)

memb_1516 <- read_csv("2015-16/ccd_sch_052_1516_w_2a_011717.csv") %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))

lunch_1516 <- read_csv("2015-16/ccd_sch_033_1516_w_2a_011717.csv") %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))

#### ---- process loop ----
process_year <- function(mem, sch_dir, lunch) {
  
  mem <- mem %>%
    mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH),
           LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                             LEAID == "4700148" ~ "4702940", TRUE ~ LEAID))%>%
    fix_leaid()
  
  lunch <- lunch %>% mutate(LEAID = as.character(LEAID), 
                            NCESSCH = as.character(NCESSCH)) %>%
    mutate(LEAID = if ("LEAID" %in% names(lunch)) 
    case_when(LEAID %in% nyc_known_leaids ~ "3620580",
              LEAID == "4700148" ~ "4702940",
              TRUE ~ LEAID) else LEAID)%>%
    fix_leaid()
  sch_dir <- sch_dir %>% mutate(LEAID = as.character(LEAID),
                                NCESSCH = as.character(NCESSCH))%>%
    fix_leaid()
  
  filtered <- mem %>%
    filter(NCESSCH %in% sch_dir$NCESSCH) %>%
    pivot_longer(cols = matches("^(AM|AS|BL|HI|WH|HP|TR).."),
                 names_to = "group", values_to = "n") %>%
    mutate(race = substr(group, 1, 2), grade = substr(group, 3, 4)) %>%
    filter(grade %in% c("09", "10", "11", "12")) %>%
    #this is stupid cause we recode again later but oh well
    mutate(race_code = case_when( race == "AM" ~ "ai", race == "AS" ~ "as",
                                  race == "BL" ~ "blk", race == "HI" ~ "lat",
                                  race == "HP" ~ "pac", race == "TR" ~ "2mr",
                                  race == "WH" ~ "wht", TRUE ~ "oth"),
           grade_code = paste0("g", as.integer(grade))) %>%
    group_by(NCESSCH, LEAID, grade_code, race_code) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  
  #one row per NCESSCH
  #yeah recode now lol, too lazy to rewrite whole pipeline..
  race_nces <- c("ai" = "AM", "as" = "AS", "blk" = "BL", "lat" = "HI",
                 "pac" = "HP", "2mr" = "TR", "wht" = "WH")
  
  counts_wide <- filtered %>%
    mutate(race_nces = dplyr::recode(race_code, !!!race_nces),
           grade_num = case_when(grade_code == "g9"  ~ "09",
                                 grade_code == "g10" ~ "10",
                                 grade_code == "g11" ~ "11",
                                 grade_code == "g12" ~ "12"),
           varname = paste0(race_nces, grade_num)) %>%
    select(NCESSCH, LEAID, varname, n) %>%
    pivot_wider(names_from = varname, values_from = n, values_fill = 0)
  
  #calc total enrollment per grade
  grade_totals <- filtered %>% group_by(NCESSCH, LEAID, grade_code) %>%
    summarise(grade_total = sum(n, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = grade_code, values_from = grade_total,
                names_prefix = "total_") %>%
    mutate(total_enroll = rowSums(across(starts_with("total_")), na.rm = TRUE))
  
  lunch_clean <- lunch %>% select(NCESSCH, FRELCH, REDLCH) %>%
    mutate(TOTFRL = FRELCH + REDLCH)
  
  final <- counts_wide %>%
    left_join(grade_totals, by = c("NCESSCH", "LEAID")) %>%
    left_join(lunch_clean, by = "NCESSCH") %>%
    #i never actually use this do i? a lot of uselessness here...
    #
    mutate(rate_frelch = ifelse(total_enroll > 0, 
                                FRELCH / total_enroll, NA_real_),
           rate_redlch = ifelse(total_enroll > 0, 
                                REDLCH / total_enroll, NA_real_),
           rate_totfrl = ifelse(total_enroll > 0, 
                                TOTFRL / total_enroll, NA_real_),
           lunch_total = TOTFRL) %>%
    select(LEAID, NCESSCH, everything())
  
  return(final)}


# Process each year
sy14_mem <- process_year(memb_1415, dir_1415, lunch_1415)
sy15_mem <- process_year(memb_1516, dir_1516, lunch_1516)

write.csv(sy14_mem, "../school_segregation/data/clean/sy14_mem.csv", 
          row.names = FALSE)
write.csv(sy15_mem, "../school_segregation/data/clean/sy15_mem.csv", 
          row.names = FALSE)


## ---- 2016 - 2023 ----

years <- 2016:2023
for (y in years) {
  sy_str   <- paste0(y, "-", substr(y+1, 3,4))
  sy_short <- substr(y, 3,4)
  message("Processing ", sy_str)
  
  # Directory file (029)
  dir_folder <- file.path(sy_str)
  dir_file   <- list.files(dir_folder, pattern = paste0("029.*", sy_short), 
                           full.names = TRUE)
  
  if (length(dir_file) == 0) {warning(paste("No 029 directory file found for", 
                                            sy_str)) 
    next}
  
  dir_data <- read_csv(dir_file[1], show_col_types = FALSE) %>%
    mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))%>%
    fix_leaid()
  
  dir_filtered <- dir_data %>%
    mutate(LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                             LEAID == "4700148" ~ "4702940", 
                             # Memphis merger with Shelby County
                             LEAID == "2601103" ~ "2612000",  
                             # Detroit DPSCD → DPS
                             TRUE ~ LEAID))%>%
    filter(LEAID %in% leaid_keep, GSHI >= 12)
  hs_ncessch <- unique(dir_filtered$NCESSCH)
  
  # Membership file (052)
  mem_file <- list.files(dir_folder, pattern = paste0("052.*", sy_short), 
                         full.names = TRUE)
  
  if (length(mem_file) == 0) {warning(paste("No 052 membership file found for", 
                                            sy_str)) 
    next}
  
  mem_data <- read_csv(mem_file[1], show_col_types = FALSE) %>%
    mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))%>%
    fix_leaid()
  
  
  mem_filtered <- mem_data %>%
    mutate(LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                             LEAID == "4700148" ~ "4702940",  
                             # Memphis merger with Shelby County 
                             LEAID == "2601103" ~ "2612000",  
                             # Detroit DPSCD → DPS
                             TRUE ~ LEAID))%>%
    filter(LEAID %in% leaid_keep, NCESSCH %in% hs_ncessch,
           !DMS_FLAG %in% c("Suppressed", "Missing", "Not reported")) %>%
    
    mutate(grade_code = case_when(GRADE == "Grade 9"  ~ "g9",
                                  GRADE == "Grade 10" ~ "g10",
                                  GRADE == "Grade 11" ~ "g11",
                                  GRADE == "Grade 12" ~ "g12",
                                  TRUE ~ NA_character_),
           race_code = case_when(
             #useless cause we recode later!!
             RACE_ETHNICITY == "American Indian or Alaska Native"~ "ai",
             RACE_ETHNICITY == "Asian" ~ "as",
             RACE_ETHNICITY == "Black or African American" ~ "blk",
             RACE_ETHNICITY == "Hispanic/Latino" ~ "lat",
             RACE_ETHNICITY == "Native Hawaiian or Other Pacific Islander" 
             ~ "pac",
             RACE_ETHNICITY == "White" ~ "wht",
             RACE_ETHNICITY == "Two or more races" ~ "2mr", TRUE ~ "oth")) %>%
    filter(!is.na(grade_code))
  
  # Aggregate counts per LEAID × NCESSCH × grade × race
  mem_agg <- mem_filtered %>%
    group_by(LEAID, NCESSCH, grade_code, race_code) %>%
    summarise(n = sum(STUDENT_COUNT, na.rm = TRUE), .groups = "drop")
  
  # Total enrollment per grade + overall
  grade_totals <- mem_agg %>%
    group_by(LEAID, NCESSCH, grade_code) %>%
    summarise(grade_total = sum(n, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = grade_code, values_from = grade_total,
                names_prefix = "total_") %>%
    mutate(total_enroll = rowSums(across(starts_with("total_")), na.rm=TRUE))
  
  # Pivot wide: one row per NCESSCH with race × grade combinations
  race_nces <- c("ai" = "AM", "as" = "AS", "blk" = "BL", "lat" = "HI", 
                 "pac" = "HP", "wht" = "WH", "2mr" = "TR")
  
  mem_counts_wide <- mem_agg %>%
    mutate(race_nces = dplyr::recode(race_code, !!!race_nces),
           grade_num = case_when(grade_code == "g9"  ~ "09",
                                 grade_code == "g10" ~ "10",
                                 grade_code == "g11" ~ "11",
                                 grade_code == "g12" ~ "12"),
           varname = paste0(race_nces, grade_num)) %>%
    select(LEAID, NCESSCH, varname, n) %>%
    pivot_wider(names_from = varname, values_from = n, values_fill = 0)
  
  # Final membership with totals
  mem_final <- mem_counts_wide %>%
    left_join(grade_totals, by = c("LEAID", "NCESSCH"))
  
  # Lunch program file (033)
  lunch_file <- list.files(dir_folder, pattern = paste0("033.*", sy_short), 
                           full.names = TRUE)
  
  if (length(lunch_file) == 0) {
    warning(paste("No 033 lunch file found for", sy_str))
    mem_with_lunch <- mem_final} else {
      lunch_data <- read_csv(lunch_file[1], show_col_types = FALSE) %>%
        mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))
      
      lunch_data <- lunch_data %>%
        fix_leaid() %>%
        mutate(LEAID = case_when(LEAID %in% nyc_known_leaids ~ "3620580",
                                 LEAID == "4700148" ~ "4702940",  
                                 # Memphis merger with Shelby County
                                 LEAID == "2601103" ~ "2612000",  
                                 # Detroit DPSCD → DPS
                                 TRUE ~ LEAID))
      
      # patch LEAID if missing
      if (!"LEAID" %in% names(lunch_data)) {
        lunch_data <- lunch_data %>%
          left_join(dir_filtered %>% 
                      select(NCESSCH, LEAID), by = "NCESSCH") %>%
          fix_leaid()}
      
      lunch_clean <- lunch_data %>%
        filter(DATA_GROUP != "Direct Certification",
               DMS_FLAG != "Not reported") %>%
        pivot_wider(names_from = LUNCH_PROGRAM, values_from = STUDENT_COUNT,
                    values_fill = 0)
      
      edu_totals <- lunch_clean %>%
        filter(TOTAL_INDICATOR == "Education Unit Total") %>%
        select(LEAID, NCESSCH, `No Category Codes`) %>%
        rename(TOTFRL = `No Category Codes`)
      
      lunch_cleaned <- lunch_clean %>%
        filter(TOTAL_INDICATOR == "Category Set A") %>%
        left_join(edu_totals, by = c("LEAID", "NCESSCH")) %>%
        select(-`No Category Codes`) %>%
        rename(FRELCH = `Free lunch qualified`,
               REDLCH = `Reduced-price lunch qualified`)
      
      lunch_counts <- lunch_cleaned %>%
        left_join(grade_totals %>% select(LEAID, NCESSCH, total_enroll),
                  by = c("LEAID", "NCESSCH")) %>%
        mutate(lunch_total = TOTFRL) #so stupid cause we recoded to that!!!
      
      mem_with_lunch <- mem_final %>%
        left_join(lunch_counts %>%select(LEAID, NCESSCH, FRELCH, REDLCH, 
                                         TOTFRL, lunch_total),
                  by = c("LEAID", "NCESSCH"))}
  
  # Save
  assign(paste0("sy", sy_short, "_mem"), mem_with_lunch)
  write.csv(mem_with_lunch,
            file = file.path("..", "school_segregation", "data", "clean", 
                             paste0("sy", sy_short, "_mem.csv")),
            row.names = FALSE)
  
  message("Finished processing ", sy_str)}
message("All years complete.")

# ---- Merge & Pivot Yearly Data ----

years <- c(98, 99, 0:23)
years_padded <- sprintf("%02d", years)
df_names <- paste0("sy", years_padded, "_mem")

grades <- c("09", "10", "11", "12")

race_suffixes <- list(white = "WH", black = "BL", hispanic = "HI", 
                      asian = "AS", other = c("AM", "HP", "TR", "oth"))
# be robust to presence/absence

#if datasets not in global environ:
data_dir <- "C:/Users/sophi/Documents/LSE/Education/school_segregation/data/clean"
files <- list.files(path = data_dir, pattern = ".*_mem\\.csv$", full.names = TRUE)
for (file in files) {
  df_name <- tools::file_path_sans_ext(basename(file))  # strip path and extension
  assign(df_name, read.csv(file, stringsAsFactors = FALSE))
}


process_dataset <- function(df, year_suffix, year_label) {
  
  df <- df %>% select(where(~!is.list(.))) %>%
    mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))
  
  # column renaming to avoid grade loss (ie, losing grade 09 in year 09)
  colnames_orig <- names(df)
  
  pattern_racegrade <- "^[A-Z]{2}\\d{2}$"
  pattern_grade_total <- "^G\\d{2}$"    
  
  general_cols <- colnames_orig[
    grepl(paste0(year_suffix, "$"), colnames_orig) &
      !grepl(pattern_racegrade,   colnames_orig) &
      !grepl(pattern_grade_total, colnames_orig) &
      !grepl(paste0("^[A-Z]{2}\\d{2}[MFU]", year_suffix, "$"), colnames_orig)]
  
  # safe to rm  year suffix
  names(df)[match(general_cols, names(df))] <-
    sub(paste0(year_suffix, "$"), "", general_cols, perl = TRUE)
  
  racegrade_cols <- colnames_orig[
    grepl(paste0("^[A-Z]{2}\\d{2}[MFU]", year_suffix, "$"), colnames_orig)]
  
  names(df)[match(racegrade_cols, names(df))] <- 
    sub(paste0("(?<=^[A-Z]{2}\\d{2}[MFU])", year_suffix, "$"), "", 
        racegrade_cols, perl = TRUE)
  
  df <- df %>%
    mutate(year = if_else(as.integer(year_label) >= 0 & 
                            as.integer(year_label) <= 23,
                          2000 + as.integer(year_label),
                          1900 + as.integer(year_label)))
  
  # aggregate race × grade 
  out <- map_dfr(grades, function(grade) {
    race_counts <- map(race_suffixes, function(suffixes) {
      total <- 0
      for (suffix in suffixes) {
        colname <- paste0(suffix, grade)
        if (colname %in% colnames(df)) {
          col_data <- df[[colname]]
          col_data <- ifelse(col_data < 0, NA, col_data)
          total <- total + coalesce(col_data, 0)}}
      total})
    
    tibble(NCESSCH = df$NCESSCH,
           LEAID = df$LEAID,
           year = df$year,
           grade = as.integer(grade),
           white = race_counts$white,
           black = race_counts$black,
           hispanic = race_counts$hispanic,
           asian = race_counts$asian,
           other = race_counts$other,
           total = race_counts$white + race_counts$black + 
             race_counts$hispanic + race_counts$asian + race_counts$other)})
  
  return(out)}


# Process all datasets correctly from global environment
all_data <- map2_dfr(df_names, years_padded, function(name, yy) {
  if (exists(name, envir = .GlobalEnv)) {
    df <- get(name, envir = .GlobalEnv)
    process_dataset(df, yy, yy)} else {
      warning(paste("Missing dataset:", name))
      NULL}})

pre1998 <- read_csv("../school_segregation/data/clean/pre1998_mem.csv")

all_data <- all_data %>%
  mutate(NCESSCH = as.character(NCESSCH),
         LEAID = as.character(LEAID))%>%
  fix_leaid()
pre1998 <- pre1998 %>%
  mutate(NCESSCH = as.character(NCESSCH),
         LEAID = as.character(LEAID))%>%
  fix_leaid()

all_data <- bind_rows(pre1998, all_data)

#remove these
all_data <- all_data %>%
  filter(total != 0) %>% #when no enrollment listed...
  filter(LEAID != 2201170) #new orleans is a complicated edge case


#### ---- save docu ----
school_counts <- all_data %>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH))%>%
  group_by(LEAID, year) %>%
  summarise(n_schools = n_distinct(NCESSCH), .groups = "drop")

#pivot wide - n schools per district over the years
school_counts_wide <- school_counts %>%
  pivot_wider(names_from = year, values_from = n_schools, values_fill = 0)

write_csv(school_counts_wide, "../school_segregation/data/school_docu.csv")

#### ---- assign status ----
fix_leaid <- function(df) {
  df %>% 
    mutate(
      LEAID = str_pad(as.character(LEAID), 7, pad = "0"))
}
treated_leaids <- c("3411340", "1709930", "0803360", "2502790",
                    "1100030", "1804770", "0634410", "3620580")

all_data <- all_data %>%
  mutate(G = if_else(LEAID %in% treated_leaids, 1, 0))

rollout_years <- list("2502790" = 2000,  # Boston
                      "3620580" = 2004,  # NYC
                      "0634410" = 2011,  # SF
                      "0803360" = 2012,  # Denver
                      "1100030" = 2014,  # DC
                      "3411340" = 2014,  # Newark
                      "1804770" = 2018,  # Indianapolis
                      "1709930" = 2018   # Chicago
)

# determine D value each year
get_treatment_status <- function(leaid, year, grade) {
  leaid_chr <- as.character(leaid)
  if (!(leaid_chr %in% names(rollout_years))) {
    return(0)}
  
  start_year <- rollout_years[[leaid_chr]]
  if (year < start_year) return(0)
  
  grade_num <- as.integer(grade)
  if (is.na(grade_num)) return(0)
  
  # grade rollout: first year => grade 9, next year => 9+10, ...
  if ((year - start_year) >= (grade_num - 9)) {
    return(1)} else {return(0)}}

# apply d 
all_data <- all_data %>%
  rowwise() %>%
  mutate(D = get_treatment_status(LEAID, year, grade)) %>%
  ungroup()

## problem: there are about 4538 duplicated rows 
## (w same ncessch x year x grade) combination as unique "id"

all_data <- all_data %>%
  distinct() #rm literal duplicates of rows

all_data <- all_data %>% 
  group_by(NCESSCH, year, grade) %>%
  mutate(n_rows = n()) %>%
  group_by(NCESSCH, year, grade, total) %>%
  mutate(total_dups = n()) %>%
  ungroup()

identical_totals <- all_data %>%
  group_by(NCESSCH, year, grade) %>%
  filter(n() > 1, n_distinct(total) == 1) %>%
  slice(1) %>%
  ungroup()

differing_totals <- all_data %>%
  group_by(NCESSCH, year, grade) %>%
  filter(n() > 1, n_distinct(total) > 1) %>%
  summarise(across(c(white, black, hispanic, asian, other, total), 
                   \(x) sum(x, na.rm = TRUE)),
            LEAID = first(LEAID),  # assume same
            G = first(G),
            D = first(D),
            .groups = "drop")

unique_rows <- all_data %>%
  group_by(NCESSCH, year, grade) %>%
  filter(n() == 1) %>%
  ungroup()

all_data_cleaned <- bind_rows(unique_rows, identical_totals, differing_totals) %>%
  arrange(NCESSCH, year, grade)

all_data <- all_data_cleaned %>%
  select(-n_rows, -total_dups)
  

# ---- Save Final Data ----


write_csv(all_data, "../school_segregation/data/school_count_data.csv")

# only treated groups (G == 1)
treated_data <- all_data %>%
  filter(G == 1)

write_csv(treated_data, "../school_segregation/data/treated_schools_count.csv")


