# Merge outcome variables with topline dataset
outcome = outcome %>% 
  mutate(any_icu = factor(any_icu, levels = c("No", "Yes")),
         any_invasive = factor(any_invasive, levels = c("No", "Yes")),
         # Rename and reorder factors
         pneumothorax_ceterm = fct_relevel(pneumothorax_ceterm, "NO") %>% 
           fct_recode(No = "NO", Yes = "YES", NULL = "N/A") %>% 
           droplevels()) 

# joing outcome variables to topline dataset
topline = right_join(topline, 
                     outcome %>% 
                       select(subjid, any_icu, any_invasive, pneumothorax_ceterm, any_oxygen, any_noninvasive),
                     by = "subjid")

# coalesce variables and remove columns 
topline = topline %>% 
  mutate(any_icu = coalesce(any_icu.x, any_icu.y),
         any_invasive = coalesce(any_invasive.x, any_invasive.y),
         pneumothorax_ceterm = coalesce(pneumothorax_ceterm.x, pneumothorax_ceterm.y)) %>% 
  select(-any_icu.x, -any_icu.y, 
         -any_invasive.x, -any_invasive.y,
         -pneumothorax_ceterm.x, -pneumothorax_ceterm.y)


# Maximum respiratory support recieved by patients
# create new variable documenting most invasive form of respiratory support 
topline = topline %>% 
  mutate(any_oxygen = factor(any_oxygen, levels = c("No", "Yes")),
         any_noninvasive = factor(any_noninvasive, levels = c("No", "Yes")),
         resp_support = case_when(any_invasive == "Yes" & any_noninvasive == "Yes" ~ "IMV & NIV",
                                  any_invasive == "Yes" & any_noninvasive == "No" ~ "IMV only",
                                  any_invasive == "No" & any_noninvasive == "Yes" ~ "NIV only",
                                    any_oxygen == "Yes" ~ "Oxygen therapy",
                                    any_oxygen == "No" ~ "None",
                                    TRUE ~ NA_character_)) %>%
  mutate(resp_support = factor(resp_support, 
                                 levels = c("None", "Oxygen therapy", "NIV only", "IMV only", "IMV & NIV")))

attr(topline$any_oxygen, 'label')  = "Supplemental oxygen"
attr(topline$any_noninvasive, 'label')  = "Noninvasive mechanival ventilation"
attr(topline$any_invasive, 'label')  = "Invasive mechanival ventilation"
attr(topline$resp_support, 'label')  = "Respiratory support"

## Reorder and rename factors
# pneumothorax_ceterm
topline = topline %>% 
  mutate(pneumothorax_ceterm = fct_relevel(pneumothorax_ceterm, "NO") %>% 
           fct_recode(No = "NO", Yes = "YES", NULL = "N/A") %>% 
           droplevels())
attr(topline$pneumothorax_ceterm, 'label') = "Pneumothorax"

# asthma_mhyn
topline = topline %>% 
  mutate(asthma_mhyn = fct_relevel(asthma_mhyn, "NO") %>% 
           fct_recode(No = "NO", Yes = "YES", NULL = "Unknown") %>% 
           droplevels())
attr(topline$asthma_mhyn, 'label') = "Asthma (physician diagnosed)"

# chronicpul_mhyn
topline = topline %>% 
  mutate(chronicpul_mhyn = fct_relevel(chronicpul_mhyn, "NO") %>% 
           fct_recode(No = "NO", Yes = "YES", NULL = "Unknown") %>% 
           droplevels())
attr(topline$chronicpul_mhyn, 'label') = "Chronic pulmonary disease (not asthma)"

# vulnerable_copd
topline = topline %>% 
  mutate(vulnerable_copd = fct_relevel(vulnerable_copd, "No") %>% 
           fct_recode(NULL = "N/K") %>% 
           droplevels())
attr(topline$vulnerable_copd, 'label') = "Severe respiratory conditions"


# ---------------------------------------
# ICU Admission Dates
# ---------------------------------------
# Spread ICU dates across all rows
ccp_data = ccp_data %>% 
  group_by(subjid) %>%
  fill(icu_hostdat, icu_hoendat, hodur) # spread across all rows

# ICU dates
icu_dates = ccp_data %>% select(subjid, redcap_event_name, icu_hostdat, icu_hoendat,
                                icu_hostdat2, icu_hoendat2, icu_hostdat3, icu_hoendat3,
                                icu_hostillin, hodur) %>%
  filter(redcap_event_name == "Discharge/Death (Arm 1: TIER 0)" |
           redcap_event_name == "Discharge/Death (Arm 2: TIER 1)" |
           redcap_event_name == "Discharge/Death (Arm 3: TIER 2)"   ) %>%
  distinct(subjid, .keep_all = TRUE)

# Clean ICU dates
icu_dates = icu_dates %>% 
  filter(!is.na(icu_hostdat)) %>% # filter out rows without first ICU admission date
  # if admissions before or equal to discharge, keep dates, otherwise swap
  mutate(icu_hostdat = if_else(icu_hostdat <= icu_hoendat, icu_hostdat, icu_hoendat), 
         icu_hoendat = if_else(icu_hostdat <= icu_hoendat, icu_hoendat, icu_hostdat),
         icu_hostdat2 = if_else(icu_hostdat2 <= icu_hoendat2, icu_hostdat2, icu_hoendat2), 
         icu_hoendat2 = if_else(icu_hostdat2 <= icu_hoendat2, icu_hoendat2, icu_hostdat2),
         icu_hostdat3 = if_else(icu_hostdat3 <= icu_hoendat3, icu_hostdat3, icu_hoendat3), 
         icu_hoendat3 = if_else(icu_hostdat3 <= icu_hoendat3, icu_hoendat3, icu_hostdat3)
  ) %>% 
  mutate(icu_days = icu_hoendat - icu_hostdat,         # calculate days in ICU for each admission
         icu_days2 = icu_hoendat2 - icu_hostdat2,
         icu_days3 = icu_hoendat3 - icu_hostdat3) %>%
  rowwise() %>%                      #rowwise will make sure the sum operation will occur on each row
  mutate(icu_days_total = sum(icu_days, icu_days2, icu_days3, na.rm=TRUE),                                   # aggregate icu stays
         icu_days_total = if_else(as.numeric(icu_days_total) > 200, NA_real_ , as.numeric(icu_days_total)),  # NA if greater than 200 days 
         icu_days.factor = case_when(icu_days_total < 4 ~ "Less than 4 days",                                # create factor
                                     icu_days_total <= 7 ~ "4 to 7 days",
                                     icu_days_total <= 14 ~ "8 to 14 days",
                                     icu_days_total > 14 ~ "15 days or more",
                                     TRUE ~ NA_character_)
         )



# Merge with topline
topline = topline %>%
  left_join(icu_dates %>%
               select(subjid, icu_hostdat, icu_hoendat, icu_days,
                      icu_hostdat2, icu_hoendat2, icu_days2,
                      icu_hostdat3, icu_hoendat3, icu_days3,
                      icu_days_total, icu_days.factor),
             by = "subjid")

# coalesce variables and remove columns 
topline = topline %>% 
  mutate(icu_hostdat = coalesce(icu_hostdat.x, icu_hostdat.y),
         icu_hoendat = coalesce(icu_hoendat.x, icu_hoendat.y),
         icu_hostdat2 = coalesce(icu_hostdat2.x, icu_hostdat2.y),
         icu_hoendat2 = coalesce(icu_hoendat2.x, icu_hoendat2.y),
         icu_hostdat3 = coalesce(icu_hostdat3.x, icu_hostdat3.y),
         icu_hoendat3 = coalesce(icu_hoendat3.x, icu_hoendat3.y)) %>% 
  select(-icu_hostdat.x, -icu_hostdat.y, 
         -icu_hoendat.x, -icu_hoendat.y,
         -icu_hostdat2.x, -icu_hostdat2.y, 
         -icu_hoendat2.x, -icu_hoendat2.y,
         -icu_hostdat3.x, -icu_hostdat3.y, 
         -icu_hoendat3.x, -icu_hoendat3.y)

# Days in ICU/HDU factor
topline = topline %>% 
  mutate(icu_days.factor = if_else(any_icu == "No", "None", icu_days.factor) %>% 
           factor(levels = c("None", "Less than 4 days", "4 to 7 days", "8 to 14 days", "15 days or more")))

# Label variables
attr(topline$any_icu, 'label')  = "Admitted to ICU/HDU"
attr(topline$icu_hostdat, 'label')  = "Date of ICU/HDU admission:"
attr(topline$icu_hoendat, 'label')  = "Date of ICU/HDU discharge:"
attr(topline$icu_hostdat2, 'label')  = "Date of ICU/HDU admission 2:"
attr(topline$icu_hoendat2, 'label')  = "Date of ICU/HDU discharge 2:"
attr(topline$icu_hostdat3, 'label')  = "Date of ICU/HDU admission 3:"
attr(topline$icu_hoendat3, 'label')  = "Date of ICU/HDU discharge 3:"
attr(topline$icu_days, 'label')  = "Days days in ICU/HDU 1"
attr(topline$icu_days2, 'label') = "Days days in ICU/HDU 2"
attr(topline$icu_days3, 'label') = "Days days in ICU/HDU 3"
attr(topline$icu_days_total, 'label')  = "Days in ICU/HDU"
attr(topline$icu_days.factor, 'label')  = "Days in ICU/HDU"

# ----------------------------------------------------------------------

# Output topline dataset

save(topline, file = "topline.RData")
