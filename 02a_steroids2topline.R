#datadir = "/home/common/covid/cleaned/full/"
#timestamp = "2020-10-22_0945"
#ccp_data = read_rds(paste0(datadir, paste0("ccp_data_", timestamp, "_full.rds")))
#topline    = read_rds(paste0(datadir, "topline_", timestamp, "_full.rds"))
#treatment  = read_rds(paste0(datadir, "treatment_", timestamp, "_full.rds"))

##############
###steroids###
##############

#cleaning the input data 
#beclAmethasone gets matched to dexamethasone
#and dexTamethasone matches incorrectly
#for corticost_cmtrt
ccp_data = ccp_data %>% 
  mutate(corticost_cmtrt = case_when(
    str_detect(corticost_cmtrt, "(?i)becl") ~ "beclometasone",
    str_detect(corticost_cmtrt, "(?i)dexta") ~ "dexamethasone",
    TRUE ~ corticost_cmtrt)) 

#same for corticost2_cmtrt
ccp_data = ccp_data %>% 
  mutate(corticost2_cmtrt = case_when(
    str_detect(corticost2_cmtrt, "(?i)becl") ~ "beclometasone",
    str_detect(corticost2_cmtrt, "(?i)dexta") ~ "dexamethasone",
    TRUE ~ corticost2_cmtrt))

#same for corticost3_cmtrt
ccp_data = ccp_data %>% 
  mutate(corticost3_cmtrt = case_when(
    str_detect(corticost3_cmtrt, "(?i)becl") ~ "beclometasone",
    str_detect(corticost3_cmtrt, "(?i)dexta") ~ "dexamethasone",
    TRUE ~ corticost3_cmtrt))

#Now select out the appropriate columns - remove anything put in orifices of least interest
#for corticost_cmtrt
steroid_free_text = ccp_data %>% 
  select(subjid, corticost_cmtrt) %>% 
  filter(!is.na(corticost_cmtrt)) %>% 
  filter(!grepl(' cream| gel| ointme|topical', corticost_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('nose|nasal|spray|hale', corticost_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('drops|opthalmo|lacrimal|eye|ear', corticost_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('suppos|enema|foam', corticost_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('%', corticost_cmtrt, ignore.case = T)) %>% 
  separate_rows(corticost_cmtrt, sep = ',') %>% 
  separate_rows(corticost_cmtrt, sep = '/') %>% 
  separate_rows(corticost_cmtrt, sep = ';') %>% 
  separate_rows(corticost_cmtrt, sep = '&') %>% 
  separate_rows(corticost_cmtrt, sep = 'and') %>% 
  mutate(corticost_cmtrt = trimws(tolower(corticost_cmtrt)))

#Now same for steroid 2
steroid2_free_text = ccp_data %>% 
  select(subjid, corticost2_cmtrt) %>% 
  filter(!is.na(corticost2_cmtrt)) %>% 
  filter(!grepl(' cream| gel| ointme|topical', corticost2_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('nose|nasal|spray|hale', corticost2_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('drops|opthalmo|lacrimal|eye|ear', corticost2_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('suppos|enema|foam', corticost2_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('%', corticost2_cmtrt, ignore.case = T)) %>% 
  separate_rows(corticost2_cmtrt, sep = ',') %>% 
  separate_rows(corticost2_cmtrt, sep = '/') %>% 
  separate_rows(corticost2_cmtrt, sep = ';') %>% 
  separate_rows(corticost2_cmtrt, sep = '&') %>% 
  separate_rows(corticost2_cmtrt, sep = 'and') %>% 
  mutate(corticost2_cmtrt = trimws(tolower(corticost2_cmtrt)))


#Now the same for steroid 3
steroid3_free_text = ccp_data %>% 
  select(subjid, corticost3_cmtrt) %>% 
  filter(!is.na(corticost3_cmtrt)) %>% 
  filter(!grepl(' cream| gel| ointme|topical', corticost3_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('nose|nasal|spray|hale', corticost3_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('drops|opthalmo|lacrimal|eye|ear', corticost3_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('suppos|enema|foam', corticost3_cmtrt, ignore.case = T)) %>% 
  filter(!grepl('%', corticost3_cmtrt, ignore.case = T)) %>% 
  separate_rows(corticost3_cmtrt, sep = ',') %>% 
  separate_rows(corticost3_cmtrt, sep = '/') %>% 
  separate_rows(corticost3_cmtrt, sep = ';') %>% 
  separate_rows(corticost3_cmtrt, sep = '&') %>% 
  separate_rows(corticost3_cmtrt, sep = 'and') %>% 
  mutate(corticost3_cmtrt = trimws(tolower(corticost3_cmtrt)))

#Now lets make the drug lookup for corticost_cmtrt
#modifying the drug_to_generic so that it doesn't think all dexamethasone is ophthalmologic
drug_to_generic = drug_to_generic %>% 
  mutate(brand_name = trimws(gsub('\\/', '', brand_name))) %>% 
  filter(class_type_label != 'ANTIINFLAMMATORY AGENTS, OPHTHALMOLOGIC') %>% 
  distinct(brand_name, .keep_all = T)

#Now run through for corticost_cmtrt
#MET
#Now for the antibiotics
ref_steroid = na.omit(drug_to_generic$brand_name)
words_steroid = steroid_free_text %>% 
  distinct(corticost_cmtrt) %>% 
  pull(corticost_cmtrt)
wordlist_steroid = expand.grid(words_steroid = words_steroid, 
                               ref_steroid = ref_steroid, 
                               stringsAsFactors = FALSE)
#matching bit
df_steroid_matches = wordlist_steroid %>% 
  group_by(words_steroid) %>% 
  mutate(match_score = 1 - stringdist(words_steroid, ref_steroid, method='jw')) %>%
  summarise(match = match_score[which.max(match_score)], 
            matched_to = ref_steroid[which.max(match_score)])

#Now merge back into steroid free text
steroid_free_text = steroid_free_text %>% 
  left_join(df_steroid_matches, by = c('corticost_cmtrt' = 'words_steroid')) %>% 
  left_join(drug_to_generic, by = c('matched_to' = 'brand_name'))

#Now filter out the actual steroids of interest
result = steroid_free_text %>%
  filter(system_type_label == 'CORTICOSTEROIDS FOR SYSTEMIC USE') %>% 
  select(subjid, corticost_cmtrt, match, matched_to)

#renaming the matched_to as steroid_name
result = result %>%
  rename(steroid_name = matched_to)

#this result has many rows for one subjid (ie. many steroids per person)
#I will add a row number to enable me to make rows into columns 
result <- result %>% 
  group_by(subjid) %>% 
  mutate(steroid_number = row_number()) 

#pivoting the table to make 1, 2 and 3 columns
result_wider <- result %>% 
  pivot_wider(names_from = steroid_number, values_from = steroid_name) %>% 
  group_by(subjid) %>% 
  fill(`3`, `2`, .direction = "up") %>% 
  fill(`1`, `2`) %>% 
  rename(steroid_name_1 = `1`,
         steroid_name_2 = `2`,
         steroid_name_3 = `3`)
#2 people for whom they have a duplicate entry to corticost_cmtrt, hence "losing" 2 patients

#Now run through for corticost2_cmtrt
#MET
#Now for the antibiotics
ref_steroid = na.omit(drug_to_generic$brand_name)
words_steroid2 = steroid2_free_text %>% 
  distinct(corticost2_cmtrt) %>% 
  pull(corticost2_cmtrt)
wordlist_steroid2 = expand.grid(words_steroid2 = words_steroid2, 
                                ref_steroid = ref_steroid, 
                                stringsAsFactors = FALSE)
#matching bit for corticost2_cmtrt
df_steroid2_matches = wordlist_steroid2 %>% 
  group_by(words_steroid2) %>% 
  mutate(match_score = 1 - stringdist(words_steroid2, ref_steroid, method='jw')) %>%
  summarise(match = match_score[which.max(match_score)], 
            matched_to = ref_steroid[which.max(match_score)])

#Now merge back into steroid free text
steroid2_free_text = steroid2_free_text %>% 
  left_join(df_steroid2_matches, by = c('corticost2_cmtrt' = 'words_steroid2')) %>% 
  left_join(drug_to_generic, by = c('matched_to' = 'brand_name'))

#Now filter out the actual steroids of interest
result2 = steroid2_free_text %>%
  filter(system_type_label == 'CORTICOSTEROIDS FOR SYSTEMIC USE') %>% 
  select(subjid, corticost2_cmtrt, match, matched_to)
result2 = result2 %>%
  rename(steroid2_name = matched_to)

#Now run through for corticost3_cmtrt
#MET
#Now for the antibiotics
ref_steroid = na.omit(drug_to_generic$brand_name)
words_steroid3 = steroid3_free_text %>% 
  distinct(corticost3_cmtrt) %>% 
  pull(corticost3_cmtrt)
wordlist_steroid3 = expand.grid(words_steroid3 = words_steroid3, 
                                ref_steroid = ref_steroid, 
                                stringsAsFactors = FALSE)
#matching bit
df_steroid3_matches = wordlist_steroid3 %>% 
  group_by(words_steroid3) %>% 
  mutate(match_score = 1 - stringdist(words_steroid3, ref_steroid, method='jw')) %>%
  summarise(match = match_score[which.max(match_score)], 
            matched_to = ref_steroid[which.max(match_score)])

#Now merge back into steroid free text
steroid3_free_text = steroid3_free_text %>% 
  left_join(df_steroid3_matches, by = c('corticost3_cmtrt' = 'words_steroid3')) %>% 
  left_join(drug_to_generic, by = c('matched_to' = 'brand_name'))

#Now filter out the actual steroids of interest
result3 = steroid3_free_text %>%
  filter(system_type_label == 'CORTICOSTEROIDS FOR SYSTEMIC USE') %>% 
  select(subjid, corticost3_cmtrt, match, matched_to)
result3 = result3 %>%
  rename(steroid3_name = matched_to)

#let's just check whether there are duplicates in the steroid2 and 3
result2 %>% 
  count(subjid) %>% 
  filter(n >1) #1 duplicate, checked and it is the same steroid twice

result3 %>% 
  count(subjid) %>% 
  filter(n >1) #no duplicates 
#conclusion: no need to do any of the extra stuff that I did for steroid 1

#now joining the tables together
#joining result_wider, result2 and result3
result_all =  result_wider %>% 
  full_join(result2, by = "subjid") %>% 
  full_join(result3, by = "subjid")

#checking how the matching is (ie. is the match score >0.7, an dhow the lower scorers look like)
result_all %>% 
  filter(match.x < 0.7) %>% 
  select(corticost_cmtrt, steroid_name_1)#none under 
result_all %>% 
  filter(match.y < 0.7) #none under
result_all %>% 
  filter(match < 0.7) #none under

#join to ccp_data 
ccp_data = ccp_data %>% 
  left_join(result_all %>% 
              select(subjid, steroid_name_1, 
                     steroid_name_2, steroid_name_3,
                     steroid2_name, steroid3_name), by = "subjid")

#cleaning the steroid_name_1, as the matching of drug names returns a couple of variants
ccp_data = ccp_data %>% 
  mutate(steroid_name_1_clean = case_when(
    steroid_name_1 == "hydrocortisone" |
      steroid_name_1 == "hydrocortisone micros" |
      steroid_name_1 == "hydrocortisone succinate" |
      steroid_name_1 == "hydrocortone" ~ "hydrocortisone",
    steroid_name_1 == "fludrocortisone" |
      steroid_name_1 == "fludrocortisone acetate" |
      steroid_name_1 == "fludrocortisone micros" |
      steroid_name_1 == "fludrocortisone micro" ~ "fludrocortisone",
    steroid_name_1 == "betamethasone" ~ "betamethasone",
    steroid_name_1 == "dexamethasone micro" |
      steroid_name_1 == "dexamethasone" |
      steroid_name_1 == "dexamethasone phosphate" |
      steroid_name_1 == "dexamethasone micros" |
      steroid_name_1 == "dexsol" |
      steroid_name_1 == "dexamethasone  eye drops unit"| #this is checked below and confirmed that the 4 patients with this entry have dexamethasone
      steroid_name_1 == "dexamethasone  solution for injection ampoules  ampoule" ~ "dexamethasone",
    steroid_name_1 == "methylprednisolone succinate" |
      steroid_name_1 == "methylprednisolone" ~ "methylprednisolone",
    steroid_name_1 == "prednisolone dompe" |
      steroid_name_1 == "prednisolone" |
      steroid_name_1 == "prednisone" |
      steroid_name_1 == "prednisolone ml" |
      steroid_name_1 == "prednisolone soluble" ~ "prednisolone",
    steroid_name_1 == "cortisone" ~ "cortisone",
    TRUE ~ NA_character_))

#similarly cleaning the steroid_name_2
ccp_data = ccp_data %>% 
  mutate(steroid_name_2_clean = case_when(
    steroid_name_2 == "hydrocortisone" |
      steroid_name_2 == "hydrocortisone micros" |
      steroid_name_2 == "hydrocortisone succinate" |
      steroid_name_2 == "hydrocortone" ~ "hydrocortisone",
    steroid_name_2 == "fludrocortisone" |
      steroid_name_2 == "fludrocortisone acetate" |
      steroid_name_2 == "fludrocortisone micros" |
      steroid_name_2 == "fludrocortisone micro" ~ "fludrocortisone",
    steroid_name_2 == "betamethasone" ~ "betamethasone",
    steroid_name_2 == "dexamethasone micro" |
      steroid_name_2 == "dexamethasone" |
      steroid_name_2 == "dexamethasone phosphate" |
      steroid_name_2 == "dexamethasone micros" |
      steroid_name_2 == "dexsol" |
      steroid_name_2 == "dexamethasone  eye drops unit"| #this is checked below and confirmed that the 4 patients with this entry have dexamethasone
      steroid_name_2 == "dexamethasone  solution for injection ampoules  ampoule" ~ "dexamethasone",
    steroid_name_2 == "methylprednisolone succinate" |
      steroid_name_2 == "methylprednisolone" ~ "methylprednisolone",
    steroid_name_2 == "prednisolone dompe" |
      steroid_name_2 == "prednisolone" |
      steroid_name_2 == "prednisone" |
      steroid_name_2 == "prednisolone ml" |
      steroid_name_2 == "prednisolone soluble" ~ "prednisolone",
    steroid_name_2 == "cortisone" ~ "cortisone",
    TRUE ~ NA_character_))

#similarly cleaning the steroid_name_3
ccp_data = ccp_data %>% 
  mutate(steroid_name_3_clean = case_when(
    steroid_name_3 == "hydrocortisone" |
      steroid_name_3 == "hydrocortisone micros" |
      steroid_name_3 == "hydrocortisone succinate" |
      steroid_name_3 == "hydrocortone" ~ "hydrocortisone",
    steroid_name_3 == "fludrocortisone" |
      steroid_name_3 == "fludrocortisone acetate" |
      steroid_name_3 == "fludrocortisone micros" |
      steroid_name_3 == "fludrocortisone micro" ~ "fludrocortisone",
    steroid_name_3 == "betamethasone" ~ "betamethasone",
    steroid_name_3 == "dexamethasone micro" |
      steroid_name_3 == "dexamethasone" |
      steroid_name_3 == "dexamethasone phosphate" |
      steroid_name_3 == "dexamethasone micros" |
      steroid_name_3 == "dexsol" |
      steroid_name_3 == "dexamethasone  eye drops unit"| #this is checked below and confirmed that the 4 patients with this entry have dexamethasone
      steroid_name_3 == "dexamethasone  solution for injection ampoules  ampoule" ~ "dexamethasone",
    steroid_name_3 == "methylprednisolone succinate" |
      steroid_name_3 == "methylprednisolone" ~ "methylprednisolone",
    steroid_name_3 == "prednisolone dompe" |
      steroid_name_3 == "prednisolone" |
      steroid_name_3 == "prednisone" |
      steroid_name_3 == "prednisolone ml" |
      steroid_name_3 == "prednisolone soluble" ~ "prednisolone",
    steroid_name_3 == "cortisone" ~ "cortisone",
    TRUE ~ NA_character_))

#similarly cleaning the steroid2_name
ccp_data = ccp_data %>% 
  mutate(steroid2_name_clean = case_when(
    steroid2_name == "hydrocortisone" |
      steroid2_name == "hydrocortisone micros" |
      steroid2_name == "hydrocortisone succinate" |
      steroid2_name == "hydrocortone" ~ "hydrocortisone",
    steroid2_name == "fludrocortisone" |
      steroid2_name == "fludrocortisone acetate" |
      steroid2_name == "fludrocortisone micros" |
      steroid2_name == "fludrocortisone micro" ~ "fludrocortisone",
    steroid2_name == "betamethasone" ~ "betamethasone",
    steroid2_name == "dexamethasone micro" |
      steroid2_name == "dexamethasone" |
      steroid2_name == "dexamethasone phosphate" |
      steroid2_name == "dexamethasone micros" |
      steroid2_name == "dexsol" |
      steroid2_name == "dexamethasone  eye drops unit"| #this is checked below and confirmed that the 4 patients with this entry have dexamethasone
      steroid2_name == "dexamethasone  solution for injection ampoules  ampoule" ~ "dexamethasone",
    steroid2_name == "methylprednisolone succinate" |
      steroid2_name == "methylprednisolone" ~ "methylprednisolone",
    steroid2_name == "prednisolone dompe" |
      steroid2_name == "prednisolone" |
      steroid2_name == "prednisone" |
      steroid2_name == "prednisolone ml" |
      steroid2_name == "prednisolone soluble" ~ "prednisolone",
    steroid2_name == "cortisone" ~ "cortisone",
    TRUE ~ NA_character_))

#similarly cleaning the steroid3_name
ccp_data = ccp_data %>% 
  mutate(steroid3_name_clean = case_when(
    steroid3_name == "hydrocortisone" |
      steroid3_name == "hydrocortisone micros" |
      steroid3_name == "hydrocortisone succinate" |
      steroid3_name == "hydrocortone" ~ "hydrocortisone",
    steroid3_name == "fludrocortisone" |
      steroid3_name == "fludrocortisone acetate" |
      steroid3_name == "fludrocortisone micros" |
      steroid3_name == "fludrocortisone micro" ~ "fludrocortisone",
    steroid3_name == "betamethasone" ~ "betamethasone",
    steroid3_name == "dexamethasone micro" |
      steroid3_name == "dexamethasone" |
      steroid3_name == "dexamethasone phosphate" |
      steroid3_name == "dexamethasone micros" |
      steroid3_name == "dexsol" |
      steroid3_name == "dexamethasone  eye drops unit"| #this is checked below and confirmed that the 4 patients with this entry have dexamethasone
      steroid3_name == "dexamethasone  solution for injection ampoules  ampoule" ~ "dexamethasone",
    steroid3_name == "methylprednisolone succinate" |
      steroid3_name == "methylprednisolone" ~ "methylprednisolone",
    steroid3_name == "prednisolone dompe" |
      steroid3_name == "prednisolone" |
      steroid3_name == "prednisone" |
      steroid3_name == "prednisolone ml" |
      steroid3_name == "prednisolone soluble" ~ "prednisolone",
    steroid3_name == "cortisone" ~ "cortisone",
    TRUE ~ NA_character_))

#any_steroid variable for dex, methylpred, pred and hydrocortisone
#select correct steroid variables to make any_steroid
ccp_data = ccp_data %>% 
  mutate(any_steroid = case_when(
    dexamethasone == "Yes" | 
      steroid_name_1_clean =="dexamethasone"| 
      steroid_name_1_clean =="hydrocortisone" | 
      steroid_name_1_clean =="prednisolone" |
      steroid_name_1_clean =="methylprednisolone"|
      steroid_name_2_clean =="dexamethasone"| 
      steroid_name_2_clean =="hydrocortisone" | 
      steroid_name_2_clean =="prednisolone" |
      steroid_name_2_clean =="methylprednisolone" |
      steroid_name_3_clean =="dexamethasone"| 
      steroid_name_3_clean =="hydrocortisone" | 
      steroid_name_3_clean =="prednisolone" |
      steroid_name_3_clean =="methylprednisolone" |
      steroid2_name_clean =="dexamethasone"| 
      steroid2_name_clean =="hydrocortisone" | 
      steroid2_name_clean =="prednisolone" |
      steroid2_name_clean =="methylprednisolone" | 
      steroid3_name_clean =="dexamethasone"| 
      steroid3_name_clean =="hydrocortisone" | 
      steroid3_name_clean =="prednisolone" |
      steroid3_name_clean =="methylprednisolone"~ "Yes",
    is.na(corticost_cmyn) & is.na(steroid_name_1_clean) & 
      is.na(steroid_name_2_clean) & is.na(steroid_name_3_clean) &
      is.na(steroid2_name_clean)& is.na(steroid3_name_clean) &
      is.na(dexamethasone) ~ NA_character_,
    TRUE ~ "No") %>% 
      factor() %>% 
      ff_label("Any steroid"))

#making a steroids dataset with one row per subjid to enable joining to topline
steroids = ccp_data  %>% 
  filter(redcap_event_name == "Discharge/Death (Arm 1: TIER 0)" | 
           redcap_event_name == "Discharge/Death (Arm 1: TIER 1)" |
           redcap_event_name == "Discharge/Death (Arm 1: TIER 2)") %>% 
  filter(is.na(redcap_repeat_instrument)) %>% 
  select(subjid, dexamethasone, corticost_cmyn,
         corticost_cmtrt, corticost_cmroute, 
         corticost2_cmtrt, corticost2_cmroute,
         corticost3_cmtrt, corticost3_cmroute,
         steroid_name_1, steroid_name_2, 
         steroid_name_3, steroid2_name, 
         steroid3_name, steroid_name_1_clean, 
         steroid_name_2_clean, steroid_name_3_clean,
         steroid2_name_clean, steroid3_name_clean,
         any_steroid, 
         redcap_event_name, redcap_repeat_instrument) 

#join steroid variables to topline 
topline = topline %>% 
  select(-c(dexamethasone, corticost_cmyn, 
            corticost_cmtrt, corticost_cmroute,
            corticost2_cmtrt, corticost2_cmroute,
            corticost3_cmtrt, corticost3_cmroute)) %>%
  left_join(steroids %>% 
              select(subjid, dexamethasone, corticost_cmyn,
                     corticost_cmtrt, corticost_cmroute, 
                     corticost2_cmtrt, corticost2_cmroute,
                     corticost3_cmtrt, corticost3_cmroute,
                     steroid_name_1, steroid_name_2, 
                     steroid_name_3, steroid2_name, 
                     steroid3_name, steroid_name_1_clean, 
                     steroid_name_2_clean, steroid_name_3_clean,
                     steroid2_name_clean, steroid3_name_clean,
                     any_steroid), 
            by = "subjid")