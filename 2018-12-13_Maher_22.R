# 2018-12-13_Maher_22.R


# USEFUL PACKAGES ----
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# USEFUL GLOBALS / FUNCTIONS ----
source('~/Box Sync/Documents/R_helpers/config.R')
source('~/Box Sync/Documents/R_helpers/helpers.R')

dedup_vector <- function(x) x[!duplicated(x)]

# GET DATA ----

# _ UDS 3 ----

# Form Head
fields_u3_hd_raw <- 
  c(
    "ptid"
    , "form_date"
    , "dob"
  )

# Form A1
fields_u3_a1_raw <-
  c(
    "sex"
    , "race"
    , "educ"
    , "maristat"
    , "handed"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# Form A4
fields_u3_a4_raw <-
  c(
    "anymeds", 
    paste0("drugid_", 1:50)
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# Form B5
fields_u3_b5_raw <-
  c(
    "npiqinf"
    , "npiqinfx"
    , "del"
    , "delsev"
    , "hall"
    , "hallsev"
    , "agit"
    , "agitsev"
    , "depd"
    , "depdsev"
    , "anx"
    , "anxsev"
    , "elat"
    , "elatsev"
    , "apa"
    , "apasev"
    , "disn"
    , "disnsev"
    , "irr"
    , "irrsev"
    , "mot"
    , "motsev"
    , "nite"
    , "nitesev"
    , "app"
    , "appsev"
    , "npiq_score"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# Form B6
fields_u3_b6_raw <-
  c(
    "nogds"
    , "satis"
    , "dropact"
    , "empty"
    , "bored"
    , "spirits"
    , "afraid"
    , "happy"
    , "helpless"
    , "stayhome"
    , "memprob"
    , "wondrful"
    , "wrthless"
    , "energy"
    , "hopeless"
    , "better"
    , "gds"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# Form C2 --- Requestor wants entire C2 form
fields_u3_c2_raw <-
  c(
    # UDS 2
    readxl::read_excel(paste0("~/Box Sync/Documents/MADC_Data_Unification/",
                              "WIP__translation_dictionary.xlsx")) %>% 
      filter(form_u2 == "form_c1_mmse_and_npsych") %>% 
      pull(field_u2),
    # UDS 3
    readxl::read_excel(paste0("~/Box Sync/Documents/MADC_Data_Unification/",
                              "WIP__translation_dictionary.xlsx")) %>% 
      filter(form_u3n == "ivp_c2") %>% 
      pull(field_u3n)
  ) %>% dedup_vector() %>% c(paste0("fu_", .), paste0("tele_", .))

# Form D1
fields_u3_d1_raw <-
  c(
    "normcog"
    , "demented"
    , "mciamem"
    , "mciaplus"
    , "mcinon1"
    , "mcinon2"
    , "impnomci"
    , "amndem"
    , "pca"
    , "ppasyn"
    , "ftdsyn"
    , "lbdsyn"
    , "namndem"
    , "alzdis"
    , "alzdisif"
    , "psp"
    , "pspif" 
    , "cort"
    , "cortif"
    , "ftldmo"
    , "ftldmoif" 
    , "ftldnos"
    , "ftldnoif"
    , "cvd"
    , "cvdif"
    , "lbdis"
    , "lbdif"
    , "park"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# Form D2
fields_u3_d2_raw <-
  c(
    "cancer"
    , "diabet"
    , "myoinf"
    , "conghrt"
    , "afibrill"
    , "hypert"
    , "angina"
    , "hypchol"
    , "vb12def"
    , "thydis"
    , "arth"
    , "urineinc"
    , "bowlinc"
    , "sleepap"
    , "remdis"
    , "hyposom"
    , "angiocp"
    , "angiopci"
    , "pacemake"
    , "hvalve"
    , "antienc"
    , "othcond"
    , "othcondx"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# iPad NIH Toolbox
fields_u3_tb_raw <-
  c(
    "date",
    tidyr::crossing(
      instr = c("cog_child"
                , "cog_crys"
                , "cog_fluid"
                , "cog_total"
                , "tb_cardsort"
                , "tb_flanker"
                , "tb_listsort"
                , "tb_oral"
                , "tb_pattern"
                , "tb_picseq"
                , "tb_picvocab"
      ),
      measr = c("age_corrected_standard_score"
                , "computed_score"
                , "fully_corrected_t_score"
                , "national_percentile_age_adjusted"
                , "rawscore"
                , "se"
                , "theta"
                , "tscore"
                , "uncorrected_standard_score"
      )
    ) %>% 
      tidyr::unite("instr_measr", instr, measr, sep = "") %>% 
      pull(instr_measr)
  )

# Caregiver Questionnaire
fields_u3_cg_raw <-
  c(
    "relation_1"
    , "livept"
    , "contact_1"
    , "employ_1"
    , "crgiver"
    , "youcare_1"
    , "nhours_1"
    , paste0("cbi_n", 1:24)  # Caregiver Burden Inventory
    , paste0("sps_1n", 1:24) # Social Provisions Scale
  )

fields_u3_raw <-
  c(
    fields_u3_hd_raw
    , fields_u3_a1_raw
    , fields_u3_a4_raw
    , fields_u3_b5_raw
    , fields_u3_b6_raw
    , fields_u3_d1_raw
    , fields_u3_d2_raw
    , fields_u3_tb_raw
    , fields_u3_cg_raw
  )
# rm(list = ls(pattern = "^fields_u3_.._raw$"))
# 
# fields_u3 <- fields_u3_raw %>% paste(collapse = ",")
# 
# forms_u3_raw <- c("ivp_c2", "fvp_c2")
# 
# forms_u3 <- forms_u3_raw %>% paste(collapse = ",")
# 
# json_u3 <-
#   get_rc_data_api(uri    = REDCAP_API_URI,
#                   token  = REDCAP_API_TOKEN_UDS3n,
#                   fields = fields_u3,
#                   forms  = forms_u3,
#                   vp     = FALSE)
# df_u3 <- jsonlite::fromJSON(json_u3) %>% as_tibble() %>% na_if("")

# _ UDS 2 ----

# # Get UDS 2 fields from MADC Data Unification translation dictionary
# df_trans_dict <- 
#   readxl::read_excel(paste0("~/Box Sync/Documents/MADC_Data_Unification/",
#                             "WIP__translation_dictionary.xlsx"))
# 
# # Keep only the rows of the trans dict that appear in our current UDS 3 fields
# df_trans_dict_flt <- df_trans_dict %>% 
#   filter(field_u3n %in% fields_u3_raw)
# 
# # Pull a vector of non-NA UDS 2 fields
# fields_u2_raw <- df_trans_dict_flt %>% 
#   filter(!is.na(field_u2)) %>% 
#   pull(field_u2)
# 
# # Append AD and Parkinson's diagnosis fields
# fields_u2_raw <-
#   c(
#     fields_u2_raw
#     , "probad"
#     , "probadif"
#     , "park"
#     , "parkif"
#   )
# 
# fields_u2 <- fields_u2_raw %>% paste(collapse = ",")
# 
# json_u2 <-
#   get_rc_data_api(uri    = REDCAP_API_URI,
#                   token  = REDCAP_API_TOKEN_UDS2,
#                   fields = fields_u2,
#                   vp     = FALSE)
# df_u2 <- jsonlite::fromJSON(json_u2) %>% as_tibble() %>% na_if("")
# 
# # Rename fields of df_u2 to match corresponding fields of df_u3
# names_df_u2 <- names(df_u2)
# 
# names(df_u2) <-
#   purrr::map_chr(names_df_u2,
#                  function(x) {
#                    u3n_name <- df_trans_dict_flt %>% 
#                      filter(field_u2 == x) %>% 
#                      pull(field_u3n) 
#                    if (length(u3n_name) > 0) {
#                      return(u3n_name)
#                    } else {
#                      return(x)
#                    }
#                  })

fields_u3_u2_raw <- 
  c(
    fields_u3_raw
    , "probad"
    , "probadif"
    # , "park" # already in UDS 3
    , "parkif"
  )

select_if_exists <- function(df, cols) {
  dplyr::select(df, cols[cols %in% names(df)])
}

df_ux <-
  read_csv(paste0("~/Box Sync/Documents/MADC_Data_Unification/",
                  "df_u2_u3a_u3n.csv"),
           col_types = cols(.default = col_character())) %>% 
  select_if_exists(fields_u3_u2_raw)

fields_u3_u2_raw[!(fields_u3_u2_raw %in% names(df_ux))]


# _ MiNDSet Registry ----

# These are needed to fill in missing demographic data for old cohort pts

# Patient Demographic Entry
fields_ms_dm_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "race_value"
    , "ed_level"
    , "handedness"
  )

fields_ms_raw <- 
  c(
    fields_ms_dm_raw
  )

fields_ms <- fields_ms_raw %>% paste(collapse = ",")

json_ms <-
  get_rc_data_api(uri    = REDCAP_API_URI,
                  token  = REDCAP_API_TOKEN_MINDSET,
                  fields = fields_ms,
                  filterLogic = paste0("(",
                                       "[subject_id] >= 'UM00000543' AND ", 
                                       "[subject_id] <= 'UM00009999' AND ",
                                       "[exam_date]  >= '2017-03-01'",
                                       ")"
                  ),
                  vp     = FALSE
  )
df_ms <- jsonlite::fromJSON(json_ms) %>% as_tibble() %>% na_if("")


# PROCESS DATA ----

# _ Clean Data ----

# # UDS 3
# df_u3_cln <- df_u3 %>% 
#   # Deselect useless field(s)
#   select(-redcap_event_name
#          , -starts_with("instructions_")
#          , -ends_with("_complete")) %>% 
#   # Keep only merged records
#   filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
#   # Clean out records missing `form_date`s
#   filter(!is.na(form_date))
# # write_csv(df_u3_cln, "df_u3_cln.csv", na = "")

# UDS X
df_ux_cln <- df_ux %>% 
  # Deselect useless field(s)
  select(-starts_with("instructions_")
         , -ends_with("_complete")) %>% 
  # Keep only merged records
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  # Clean out records missing `form_date`s
  filter(!is.na(form_date))
write_csv(df_ux_cln, "df_ux_cln.csv", na = "")

# MiNDSet Registry

df_ms_cln <- df_ms %>% 
  # Deselect useless field(s)
  select(-redcap_event_name) %>% 
  # Coerce fields to appropriate types
  mutate(educ_ms = as.integer(ed_level)) %>% 
  select(-ed_level)

# _ Mutate Data ----

# # UDS 3
# dx_vars <- fields_u3_d1_raw %>% 
#   str_replace_all("fu_|tele_", NA_character_) %>% 
#   stringi::stri_remove_empty_na()
# 
# rel_fields <- names(df_u3_cln) %>% 
#   str_replace_all("ptid|form_date|dob", NA_character_) %>% 
#   stringi::stri_remove_empty_na()
# 
# df_u3_cln_mut <- df_u3_cln %>%
#   # Get rid of records without any relevant data (likely milestoned pts)
#   get_nonempty_records(rel_fields) %>% 
#   # Coalesce initial/follow-up/telephone visits
#   coalesce_ift_cols() %>% 
#   # Coerce dx vars to integer
#   mutate_at(vars(dx_vars), as.integer) %>% 
#   # Derive MADC Consensus Dx
#   derive_consensus_dx()
# # write_csv(df_u3_cln_mut, "df_u3_cln_mut.csv", na = "")

# UDS X
dx_vars <- fields_u3_d1_raw %>%
  str_replace_all("fu_|tele_", NA_character_) %>%
  stringi::stri_remove_empty_na()

rel_fields <- names(df_ux_cln) %>%
  str_replace_all("ptid|form_date|dob", NA_character_) %>%
  stringi::stri_remove_empty_na()

df_ux_cln_mut <- df_ux_cln %>%
  # Get rid of records without any relevant data (likely milestoned pts)
  get_nonempty_records(rel_fields) %>%
  # Coalesce initial/follow-up/telephone visits
  coalesce_ift_cols() %>%
  # Coerce dx vars to integer
  mutate_at(vars(dx_vars), as.integer) %>%
  # Derive MADC Consensus Dx (UDS 3)
  derive_consensus_dx() %>% 
  # Derive MADC Consensus Dx (UDS 2)
  (function(.) {
    mutate(.data = .,
           madc_dx = case_when(
             normcog == 0 & demented == 1 &
               (probad == 1 & probadif == 1) ~ "AD",
             normcog == 0 & demented == 1 &
               (park == 1 & parkif == 1) ~ "PD",
             TRUE ~ madc_dx
           ))
  })
write_csv(df_ux_cln_mut, "df_ux_cln_mut.csv", na = "")

# MiNDSet Registry

df_ms_cln_mut <- df_ms_cln %>% 
  # Convert `race_value` to match UDS 3 `race`
  mutate(race_ms = case_when(
    race_value == 1 ~ 1L,  # White
    race_value == 2 ~ 2L,  # Black
    race_value == 3 ~ 5L,  # Asian
    race_value == 5 ~ 50L, # Other
    race_value == 6 ~ 99L, # Unknown
    TRUE ~ NA_integer_
  )) %>% 
  select(-race_value) %>% 
  # Convert `handedness___*` to match UDS 3 `handed`
  mutate(handed_ms = case_when(
    handedness___1 == 1 ~ 2L, # R
    handedness___2 == 1 ~ 1L, # L
    handedness___3 == 1 ~ 3L, # A
    TRUE ~ NA_integer_
  )) %>% 
  select(-handedness___1, -handedness___2, -handedness___3)
# write_csv(df_ms_cln_mut, "df_ms_cln_mut.csv", na = "")

# Spackle in missing demographic data
df_ux_ms <- 
  left_join(df_ux_cln_mut, df_ms_cln_mut,
            by = c("ptid" = "subject_id", "form_date" = "exam_date")) %>% 
  mutate_at(vars(starts_with("race"),
                 starts_with("educ"),
                 starts_with("handed")),
            as.integer) %>% 
  mutate(race   = coalesce(race, race_ms),
         educ   = coalesce(educ, educ_ms),
         handed = coalesce(handed, handed_ms)) %>% 
  select(-race_ms, -educ_ms, -handed_ms)

####
## HERE FIX DIFFERENT FORMATS IN `dob` field
##   UDS 2 : mm/dd/yyyy
##   UDS 3 : yyyy-mm-dd
####

# Mutates
df_ux_ms_mut <- df_ux_ms %>% 
  # Calculate age
  calculate_age(dob, form_date) %>% 
  # Derive Parkinson's dx
  mutate(madc_dx = case_when(
    normcog == 0 & demented == 1 & 
      (lbdis == 1 & lbdif == 1 & park == 1) ~ "PD",
    TRUE ~ madc_dx
  )) %>% 
  # Reorder initial fields
  select(ptid, form_date, dob, starts_with("age"), madc_dx, everything()) %>% 
  select(-dob, -age_years, -age_units)

# Filter for MCI, AD, Parkinson's
df_ux_ms_mut_flt <- df_ux_ms_mut %>%
  filter(madc_dx == "MCI" |
           madc_dx == "AD" |
           madc_dx == "PD")
write_csv(df_ux_ms_mut_flt, "df_ux_ms_mut_flt.csv", na = "")
























