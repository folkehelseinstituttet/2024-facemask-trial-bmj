################################################################################
## Study title: The personal protective effect of wearing surgical face masks in 
##              public spaces on self-reported respiratory symptoms in adults: a
##              pragmatic, randomised superiority trial  
## 
## Authors script: Petter Elstrøm, Runar Barstad Solberg, Ingeborg Hess Elgersma
## Contact: Runar.Solberg@fhi.no
## 
## Data cleaning
################################################################################

set.seed(2604)

## NB! This code is not reproducible as raw data cannot be provided for privacy
##     reasons but the script is provided for transparency




setwd("N:/durable/Code/BMJ")

# R packages
library(rio)
library(tidyverse)
library(janitor)
library(readxl)
library(knitr)
library(flextable)
library(officer)
library(lubridate)
library(rstatix)
library(stats)
library(gtsummary)
library(gt)
library(mice)
library(VIM)


###         1. Establishing sample based on consent + baseline form          ###

# Open consent forms
# Consent forms were generated as three separate forms
# based on how participants signed up (recruited from Respons or Norstat, or 
# directly on the study's website)

respons <- import("N:/durable/Code/BMJ/Data/326674.csv") %>%
  select(!'$forwarded_to_form')
norstat <- import("N:/durable/Code/BMJ/Data/326949.csv") %>%
  select(!'$forwarded_to_form')
webform <- import("N:/durable/Code/BMJ/Data/310568.csv")

# Merging to one file
consent <- full_join(webform, respons, by = "dob")
consent <- full_join(consent, norstat, by = "dob") %>%
  # Joining variables of email address and date
  mutate(mail = case_when(
    !is.na(epost) ~ epost,
    !is.na(epost.x) ~ epost.x,
    !is.na(epost.y) ~ epost.y),
    c_date = as.Date(case_when(
      !is.na(`$created`) ~ `$created`,
      !is.na(`$created.x`) ~ `$created.x`,
      !is.na(`$created.y`) ~ `$created.y`
    ))) %>%
  # Deleting duplicates and those testing the form before start (persons at CEIR)
  distinct(dob, .keep_all = TRUE) %>%
  filter(c_date > "2023-02-09") %>%
  filter(!mail %in% c("runar.barstad.solberg@gmail.com",
                      "Runar.solberg@fhi.no",
                      "ingeborghess.elgersma@fhi.no",
                      "ingeborgelgersma@hotmail.com",
                      "pernille.grondahl@gmail.com",
                      "atle.fretheim@fhi.no",
                      "annlaugselsto@gmail.com",
                      "petter.elstrom@fhi.no",
                      "unni.gopinathan@fhi.no",
                      "unni.gnathan@gmail.com",
                      "matthewoxman@gmail.com",
                      "atle.fretheim@online.no",
                      "annlaug.selsto@fhi.no",
                      "jonas.fretheim@gmail.com",
                      "arnfinn.helleve@fhi.no",
                      "Arnfinnhelleve@gmail.com")) %>%
  
  # Identify those consenting to participate
  mutate(deltager = case_when(
    samtykke.x == 1 | samtykke.y == 1 | samtykke == 1 ~ 1
  )) %>%
  
  # Keeping those who consent, and variables of personal ID-number (Fodselsnummer), email address, date of consenting, and giving consent (deltager == 1)
  filter(deltager == 1) %>%
  rename(Fodselsnummer = dob) %>%
  select(Fodselsnummer, mail, c_date, deltager)



###              2. Merging with baseline form                               ###

# Importing baseline form
baseline <- import("N:/durable/Code/BMJ/Data/306416.csv") %>%
  # Remove those testing the form before the start of the trial
  mutate(b_date = as.Date(`$created`)) %>%
  filter(b_date > "2023-02-09") %>%
  filter(!`e-postadresse` %in% c("runar.barstad.solberg@gmail.com",
                                 "Runar.solberg@fhi.no",
                                 "ingeborghess.elgersma@fhi.no",
                                 "ingeborgelgersma@hotmail.com",
                                 "pernille.grondahl@gmail.com",
                                 "atle.fretheim@fhi.no",
                                 "annlaugselsto@gmail.com",
                                 "petter.elstrom@fhi.no",
                                 "unni.gopinathan@fhi.no",
                                 "unni.gnathan@gmail.com",
                                 "matthewoxman@gmail.com",
                                 "atle.fretheim@online.no",
                                 "annlaug.selsto@fhi.no",
                                 "jonas.fretheim@gmail.com",
                                 "arnfinn.helleve@fhi.no",
                                 "Arnfinnhelleve@gmail.com"))

# Merge the files for consenting and data from baseline form, 
# keeping only those finishing the baseline form
start <- left_join(baseline, consent, by = "Fodselsnummer")


# Identifying persons filling out the baseline form several times
dupl <- start[duplicated(start$Fodselsnummer),]

# Include all registrations of persons filling out multiple times
dupl_obs <- left_join(dupl, start, by = "Fodselsnummer") %>%
  select(Fodselsnummer) %>%
  mutate(dup = 1)

# Sum persons filling out multiple times
dupl_pers <- dupl_obs %>%
  distinct(Fodselsnummer, .keep_all = TRUE)

duplicates <- sum(dupl_pers$dup)


# Unique persons filling out the baseline form, including the first registration 
# when people have filled out multiple times
start <- start %>%
  distinct(Fodselsnummer, .keep_all = TRUE) %>%
  mutate(started = case_when(
    !is.na(`$submission_id`) ~ 1)) %>%
    # Group allocation
  mutate(group = if_else(`$forwarded_to_form` == 309233, 1, 0))

# Number of persons consenting and filling out the baseline form  
started <- sum(start$started)

# Extracting age and sex based on personal ID-code
start <- start %>%
  mutate(fnr = as.character(Fodselsnummer)) %>%
  mutate(fnr = if_else(nchar(fnr) == 10, paste("0",fnr, sep = ""), fnr)) %>%
  #a few people may use a d-number: then a 4 is added to the first digit in the fnr
  mutate(fnr = if_else(as.numeric(substr(fnr, 1, 1))>3, paste0(as.numeric(substr(fnr, 1, 1))-4,substr(fnr, 2, 11)),fnr)) %>%
  mutate(db = as.Date(substr(fnr, 1, 6),format = "%d%m%y")) %>%
  mutate(c_date = if_else(is.na(c_date), b_date, c_date)) %>%
  #Calculate age based on fnr and day of inclusion in study
  mutate(db = if_else(db >= c_date, db - years(100), db)) %>%
  mutate(age_in_years = interval(db, c_date)/years(1)) %>%
  mutate(female = if_else(as.numeric(substr(fnr, 9, 9)) %% 2 == 0, 1, 0)) 


# Identifying persons under 18 years of age
start <- start %>%
  mutate(excl = case_when(
    age_in_years < 18 ~ 1,
    age_in_years >= 18 ~ 0
  ))

excl_18 <- sum(start$excl)


# Remove those who later withdrew the consent
withdrawals <- read_excel("N:/durable/Code/raw/Withdrawals.xlsx")

withdrawals <- withdrawals %>%
  mutate(withdraw = 1) %>%
  rename("mail" = "epost")

start <- left_join(start, withdrawals, by = "mail")

# N of withdrawals after consenting to participate
withdrawals <- start %>%
  filter(withdraw == 1) %>%
  select(Fodselsnummer, withdraw, mail)

# Remove participants < 18 y, consenting/filled out baseline form, and who later withdrew
start <- start %>%
  filter(is.na(withdraw)) %>%
  filter(excl == 0) %>%
  select(!c('$submission_id', '$created', Mobilnummer, '$answer_time_ms', 
            deltager, excl, withdraw))


# Follow-up form
follow_up <- import("N:/durable/Code/BMJ/Data/308985.csv") %>%
  mutate(f_date = as.Date(`$created`)) %>% #f_date - day of follow up
  filter(f_date > "2023-02-27") %>%  # Remove responses from testing of the form
  select(!`$created`)


# Remove withdrawals
final_withdr <- left_join(follow_up, withdrawals, by = "Fodselsnummer")

# Completed the final form
final <- final_withdr %>%
  filter(is.na(withdraw)) %>%
  select(!c(withdraw, mail))

# Joining baseline form, group allocation and final form
final <- left_join(start, final, by = "Fodselsnummer") %>%
  distinct(Fodselsnummer, .keep_all = TRUE) %>% 
  mutate(finish = case_when(
    !is.na(f_date) ~ 1
  )) %>% 
  
  # Rename and make cathegories
  rename(household = Hvormangepersonerihustanden,
         children = under18,
         colleagues = Kollegaerperdag,
         attitude_facemask = Holdningermunnbind,
         facemask_use = Munnbinduteblantfolk,
         public_transport_bl = offentligtransport,
         age = age_in_years) %>%
  mutate(household_size = as.factor(case_when(
    household == 1 ~ "1",
    household == 2 ~ "2",
    household == 3 ~ "3",
    household == 4 ~ "4",
    household > 4 ~ "5+"
  ))) %>%
  mutate(children_in_household = as.factor(case_when(
    is.na(children) ~ "0",
    children == 1 ~ "1",
    children > 1 & children < 4 ~ "2-3",
    children > 3 ~ "4+"
  ))) %>%
  mutate(completed = case_when(
    finish == 1 ~ 1,
    is.na(finish) ~ 0)) %>%
  
  # Delete variables no longer needed
  select(!c(`$forwarded_to_form`, `e-postadresse`, household, 
            children, finish, mail, fnr, db,
            `$submission_id`, `$answer_time`)) %>% 
  
  # Format as categorical variables
  mutate(colleagues = as.factor(colleagues),
         attitude_facemask = as.factor(attitude_facemask),
         facemask_use = as.factor(facemask_use),
         public_transport_bl = as.factor(public_transport_bl),
         group = as.factor(group))

###    3. Adding sysvak (Norwegian Immunisation Registry) data               ###

#Importing the serial number key
key <- import("N:/durable/Code/BMJ/data/merge_file.csv")
sysvak <- import("N:/durable/Code/BMJ/data/uttrekk_Munnbind_20231103.csv") %>%
  left_join(key)%>%
  select(-id_kode) %>%
  mutate(vdate = as.Date(VAKSINASJONSDATO, format="%d.%m.%Y")) %>%
  arrange(Fodselsnummer, vdate) %>%
  group_by(Fodselsnummer) %>%
  mutate(sumvac = n(),
         last_vdate = max(vdate)) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(last_vdate)) %>%
  select(Fodselsnummer, sumvac, last_vdate)


final <- left_join(final, sysvak, by = "Fodselsnummer")
#Definition of immunized: 1 dose more than 14 days before start, or more than 1 doses
final <- final %>%
  mutate(lastdose14days = if_else(last_vdate > (c_date -days(14)),1,0)) %>%
  mutate(covid_vacc_recceived = case_when(is.na(sumvac) | (sumvac==1 & lastdose14days ==1)  ~ "0",
                                          sumvac==1 | (sumvac==2 & lastdose14days ==1) ~ "1",
                                          sumvac==2 | (sumvac==3 & lastdose14days ==1)~ "2",
                                          (sumvac>2 & lastdose14days !=1) | sumvac >3 ~ "\u22653"))
final <- final %>%
  mutate(imm = if_else(covid_vacc_recceived =="0", 0, 1))
  

###                4. Defining respiratory track symptoms                    ###
final <- final %>%
  mutate(airwayssymptoms = Symptomer_3 + Symptomer_7 + Symptomer_8 + Symptomer_9 + 
           Symptomer_13,
         othersymptoms = Symptomer_1 + Symptomer_4 + Symptomer_5 + Symptomer_6 + 
           Symptomer_10 + Symptomer_11 + Symptomer_12 + Symptomer_14) %>%
  rename(fluefeeling = Forskjoletellerhattsymptom,
         covid = Positivcovidtest) %>%
  mutate(ill = case_when(
    covid == 1 & airwayssymptoms > 0 & Symptomer_2 == 1 ~ 1,
    fluefeeling == 1 & airwayssymptoms > 0 & Symptomer_2 == 1 ~ 2,
    fluefeeling == 1 & airwayssymptoms > 0 & othersymptoms > 1 ~ 3),
    infection = case_when(
      ill > 0 ~ 1,
      ill == 0 ~ 0,
      is.na(ill) & completed == 1 ~ 0)) %>%
  sample() %>%
  mutate(id= row_number())





###         5. Adding MSIS (notified with COVID to Norwegian Surveillance    ###
###               System for Communicable Diseases) positives                ###
merge_file <- readRDS("N:/durable/Code/BMJ/data/merge_file.rds") %>%
  rename_with(tolower)

msis_pos <- read_excel("N:/durable/Code/BMJ/data/23-00491_MSIS-data.xls", sheet = "PositivePr\u00F8ver") %>%
  distinct(id_kode, .keep_all = TRUE) %>%
  janitor::clean_names()

msis <- left_join(merge_file, msis_pos, by = "id_kode") %>%
  mutate(msis_case = if_else(is.na(provedato), 0, 1)) %>%
  select(!id_kode)

final <- final %>%
  rename_with(tolower)%>%
  left_join(msis, by = "fodselsnummer")

final <- final%>%
  mutate(msis_case = if_else(as.Date(provedato) %within% interval(b_date, b_date+ days(17)),1,0),
         msis_case = if_else(is.na(msis_case),0, msis_case))



###                6. Multiple imputation                                    ###
data_to_mice <- final %>%
  select(colleagues, attitude_facemask, facemask_use, public_transport_bl,
         group, age, female, household_size, children_in_household, completed,
         infection)

# Missing pattern in the data set
finalmice_aggr <- aggr(data_to_mice, col = mdc(1:2), numbers = TRUE, sortVars = TRUE,
                       labels = names(data_to_mice), cex.axis = .5, gap = 3,
                       ylab = c("Proportion of missingness", "Missingness pattern")) 




# Imputation (MICE)
imp_final <- mice(data_to_mice, m = 50, printFlag = TRUE, maxit = 20, seed = 1234)
#save(imp_final, file= "imp_final.Rdata")
#load("imp_final.Rdata")
# Keeps 1 imputation in order to share at data set with imputed data
iteration1 <- complete(imp_final, action =1L) %>%
  select(mice_infection=infection) %>%
  mutate(id=row_number())
final <- final %>%
  left_join(iteration1, by="id")



###                           7. Table 2                                     ###
final <- final %>%
  mutate(c19 = if_else(covid==1, 1, 0)) %>%
  mutate(c19 = if_else(is.na(c19)&completed==1, 0, c19)) %>%
  mutate(
    household_size = as.character(household_size),
    household_size = ifelse(is.na(household_size), "7", household_size),
    household_size = as.factor(household_size)
  )%>%
  mutate(
    attitude_facemask = as.character(attitude_facemask),
    attitude_facemask = ifelse(is.na(attitude_facemask), "7", attitude_facemask),
    attitude_facemask = as.factor(attitude_facemask)
  )



tbl2 <- final %>%
  select(group, female, age, covid_vacc_recceived, imm, household_size, children_in_household, colleagues,
         attitude_facemask, facemask_use, public_transport_bl)%>%
  tbl_summary(
    by = group,
    type = list(female ~ "dichotomous",
                age ~ "continuous",
                covid_vacc_recceived ~ "categorical",
                imm ~ "categorical",
                household_size ~ "categorical",
                children_in_household ~ "categorical",
                colleagues ~ "categorical",
                attitude_facemask ~ "categorical", 
                facemask_use ~ "categorical", 
                public_transport_bl ~ "categorical"),
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(female ~ "Female sex",
               age ~ "Age, year",
               covid_vacc_recceived ~ "No of COVID-19 vaccine doses received",
               imm ~ "Immune status",
               household_size ~ "Household size",
               children_in_household ~ "Children in the household",
               colleagues ~ "Close contacts at work (per day)",
               attitude_facemask ~ "Expectations and attitude towards face mask\nWearing face masks... ",
               facemask_use ~ "Face masks use in the two weeks before randomisation",
               public_transport_bl ~ "Commute to work using public transport")) %>%
  modify_header(stat_1 = "**Control group**, n={n}" ,
                stat_2 = "**Intervention group**, n={n}") %>%
  modify_caption("Table 2. Baseline charachteristics") %>%
  as_gt()  %>%
  cols_move(columns= stat_1, stat_2)
  
tbl2 %>%
  gt::gtsave("table2.html")

###               8. Analysis primary outcome imputed data                   ###
## To table 3
line2 <- with(data=imp_final, exp = glm(infection ~ group, family = binomial(link = "logit"))) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 label = list(group ~"Self reported respiratory symptoms (MICE)"))

line2RD <- with(data=imp_final, exp = glm(infection ~ group, family = binomial(link = "identity"))) %>%
  tbl_regression(show_single_row= "group",
                 estimate_fun= function(x) style_number(x, digits = 3),
                 label = list(group ~"Self reported respiratory symptoms (MICE)"))
line2 <- tbl_merge(list(line2, line2RD),
                   FALSE)
save(line2, file= "line2.Rdata")
## Adjusted odds ratios
adjOR_mice <- with(data=imp_final, exp = glm(infection ~ group + children_in_household + age + female, family = binomial(link = "logit"))) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 include = "group",
                 label = list(group ~"Self reported respiratory symptoms (MICE)"))

adjOR_mice

adjOR_complete <- glm(infection ~ group + children_in_household + age + female, data=final, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 include = "group",
                 label = list(group ~"Self reported respiratory symptoms (complete cases)"))

adjOR_complete

## To risk ratio table
infectionRR <- glm(infection ~ group, family = binomial(link = "log"), data= final) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 label = list(group ~"Self reported respiratory symptoms (complete cases)"))%>%
  modify_header(estimate = "**Risk ratio (95% CI)**")

infection_aRR <- glm(infection ~ group + female + age + children_in_household, family = binomial(link = "log"), data= final) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 include = "group",
                 label = list(group ~"Self reported respiratory symptoms (complete cases)"))%>%
  modify_header(estimate = "**Adjusted risk ratio (95% CI)**")



miceinfectionRR <- with(data=imp_final, exp = glm(infection ~ group, family = binomial(link = "log"))) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 label = list(group ~"Self reported respiratory symptoms (MICE)"))
save(miceinfectionRR, file= "miceinfectionRR.Rdata")
miceinfection_aRR <- with(data=imp_final, exp = glm(infection ~ group+ female + age + children_in_household, family = binomial(link = "log"))) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 include = "group",
                 label = list(group ~"Self reported respiratory symptoms (MICE)"))

# c19RR <- glm(c19 ~ group, family = binomial(link = "log"), data= final) %>%
#   tbl_regression(exponentiate = T,
#                  show_single_row= "group",
#                  label = list(group ~"Self reported COVID-19 case"))
# c19_aRR<- glm(c19 ~ group+ female + age + children_in_household + covid_vacc_recceived, family = binomial(link = "log"), data= final) %>%
#   tbl_regression(exponentiate = T,
#                  show_single_row= "group",
#                  include = "group",
#                  label = list(group ~"Self reported COVID-19 case"))
# msis_caseRR <- glm(msis_case ~ group, family = binomial(link = "log"), data= final) %>%
#   tbl_regression(exponentiate = T,
#                  show_single_row= "group",
#                  label = list(group ~"Reported COVID-19 case"))
# msis_case_aRR<- glm(msis_case ~ group+ female + age + children_in_household + covid_vacc_recceived, family = binomial(link = "log"), data= final) %>%
#   tbl_regression(exponentiate = T,
#                  show_single_row= "group",
#                  include = "group",
#                  label = list(group ~"Reported COVID-19 case"))
RRs <- tbl_stack(
  tbls = list(infectionRR, miceinfectionRR))

aRR <- tbl_stack(
  tbls = list(infection_aRR, miceinfection_aRR))

riskratios <- tbl_merge(list(RRs, aRR), FALSE) %>%
  modify_table_styling(
    columns = estimate_2,
    rows = tbl_id1_1 == list(1,2),
    footnote = "Adjusted for sex, age and number of children in household"
                 )%>%
  # modify_table_styling(
  #   columns = estimate_2,
  #   rows = tbl_id1_1 == list(3),
  #   footnote = "Adjusted for sex, age, number of children in household and number of COVID-19 vaccines received"
  # ) %>%
  # modify_table_styling(
  #   columns = estimate_2,
  #   rows = tbl_id1_1 == list(4),
  #   footnote = "Model did not converge"
  #) %>%
  as_gt()


# 
# riskratios <- riskratios %>%
#   sub_values(
#     columns = everything(),
#     rows = 4,
#     values = riskratios["_data"]$`_data`$p.value_2[4],
#     replacement = "-",
#     escape = TRUE
#   )
riskratios%>%
  gt::gtsave("Riskratios.html")
                 
###               8. Anonymising dataset                                     ###
symp <- c(
  "headache",
  "fever",
  "runnynose",
  "reducedsmell",
  "reducedtaste",
  "reducedappetite",
  "throat",
  "cough",
  "sneezing",
  "bodyache",
  "muscleache",
  "tired",
  "heavybreathing",
  "stomachache"
)
final <- final %>%
  rename_with(~ symp, all_of(starts_with("Symp"))) %>%
  rename_with(tolower)
final <-final %>%
  rename(public_transport_flw = m4868357.kollektivtransport,
         cultural_flw = m4868357.kulturarrangement,
         resturant_flw = m4868357.resturant) %>%
  mutate(age_cat = case_when(age < 30 ~ "1",
                             age >= 30 & age <= 60 ~ "2",
                             age > 60 ~ "3")) %>%
  rename(facemask_flw = munnbinduteblantfolk)

vars_to_keep <- c(
  "id", 
  "group",
  "completed",
  "infection", 
  "mice_infection",
  #"msis_case", #Due to the low number of cases we have decided not to share
  # this information due to privacy concerns
  "c19",
  symp,
  "age_rounded",
  "age_cat",
  "female",
  "covid_vacc_recceived",
  "imm",
  "household_size",
  "children_in_household",
  "colleagues",
  "attitude_facemask",
  "facemask_use",
  "public_transport_bl",
  "public_transport_flw",
  "facemask_flw",
  "cultural_flw",
  "resturant_flw"
)
mround <- function(x,base){
  base*round(x/base)
}

anonymized_dataset <- final %>%
  mutate(age_rounded = mround(age,5)) %>% 
  select(c(vars_to_keep))

saveRDS(anonymized_dataset,"final.Rds")
write.csv2(anonymized_dataset, file = "final.csv", row.names = F)
rm(list=setdiff(ls(), c("imp_final")))


