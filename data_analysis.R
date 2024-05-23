################################################################################
## Study title: The personal protective effect of wearing surgical face masks in 
##              public spaces on self-reported respiratory symptoms in adults: a
##              pragmatic, randomised superiority trial  
## 
## Authors script: Petter Elstrøm, Runar Barstad Solberg, Ingeborg Hess Elgersma
## Contact: Runar.Solberg@fhi.no
## 
## Analysis
################################################################################

#Set working directory to your directory
setwd("N:/durable/Code/BMJ")
final <- readRDS("final.Rds")
# R packages
library(tidyverse)
library(gtsummary)
library(gt)
library(broom)
library(cowplot)

###                               TABLE 2                                    ###

#Sine age has been rounded to the closest 5 year, results will not be completly
#identical to table 1 in the paper
tbl2 <- final %>%
  select(group, female, age_rounded, covid_vacc_recceived, imm, household_size, children_in_household, colleagues,
         attitude_facemask, facemask_use, public_transport_bl)%>%
  tbl_summary(
    by = group,
    type = list(female ~ "dichotomous",
                age_rounded ~ "continuous",
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
                 age_rounded ~ "Age, year",
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
  gt::gtsave("table2_anonymizeddata.html")


###                               TABLE 3                                    ###
tbl_cases <- final %>%
  select(group, infection, mice_infection, c19) %>%
  tbl_summary(
    by = group,
    type = list(mice_infection ~ "dichotomous",
                infection ~ "dichotomous",
                c19 ~ "dichotomous"),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing= "no",
    label = list(infection ~ "Self reported respiratory symptoms (complete cases)",
                 mice_infection ~ "Self reported respiratory symptoms (MICE)",
                 c19 ~ "Self reported COVID-19 case")) %>%
  modify_header(stat_1 = "**Control group**, n={n}" ,
                stat_2 = "**Intervention group**, n={n}") %>%
  modify_caption("Table 3: Main findings") %>%
  as_gt()  %>%
  cols_move(columns= stat_1, stat_2)
tbl_cases%>%
  gt::gtsave("table3_part1_anonymizeddata.html")

theme_gtsummary_journal("jama")
line1 <- glm(infection ~ group, family = binomial(link = "logit"), data= final) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 label = list(group ~"Self reported respiratory symptoms (complete cases)"))%>%
  modify_header(estimate = "**Marginal odds ratio (95% CI)**")
line1RD <- glm(infection ~ group, family = binomial(link = "identity"), data= final) %>%
  tbl_regression(show_single_row= "group",
                 estimate_fun= function(x) style_number(x, digits = 3),
                 label = list(group ~"Self reported respiratory symptoms (complete cases)"))%>%
  modify_header(estimate = "**Risk difference (95% CI)**")
line1<- tbl_merge(list(line1, line1RD),
          FALSE)
load("line2.Rdata")
# line2 <- with(data=imp_final, exp = glm(infection ~ group, family = binomial(link = "logit"))) %>%
#   tbl_regression(exponentiate = TRUE,
#                  show_single_row= "group",
#                  label = list(group ~"Self reported respiratory symptoms (MICE)"))
# 
# line2RD <- with(data=imp_final, exp = glm(infection ~ group, family = binomial(link = "identity"))) %>%
#   tbl_regression(show_single_row= "group",
#                  estimate_fun= function(x) style_number(x, digits = 3),
#                  label = list(group ~"Self reported respiratory symptoms (MICE)"))
# line2 <- tbl_merge(list(line2, line2RD),
#                   FALSE)

line3 <-glm(c19 ~ group, family = binomial(link = "logit"), data=final) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 label = list(group ~"Self reported COVID-19 case"))

line3RD <- glm(c19 ~ group, family = binomial(link = "identity"), data= final) %>%
  tbl_regression(show_single_row= "group",
                 estimate_fun= function(x) style_number(x, digits = 3),
                 label = list(group ~"Self reported COVID-19 case"))
line3<- tbl_merge(list(line3, line3RD),
                  FALSE)

# This is the code for notified cases which has been removed for privacy reasons
# line4 <- glm(msis_case ~ group, family = binomial(link = "logit"), data= final) %>%
#   tbl_regression(exponentiate = TRUE,
#                  show_single_row= "group",
#                  label = list(group ~"Reported COVID-19 case"))
# line4RD <- glm(msis_case ~ group, family = binomial(link = "identity"), data= final) %>%
#   tbl_regression(show_single_row= "group",
#                  estimate_fun= function(x) style_sigfig(x, digits = 3),
#                  label = list(group ~"Reported COVID-19 case"))
# line4<- tbl_merge(list(line4, line4RD),
#                   FALSE)


tbls_regression <- tbl_stack(
    tbls = list(line1, line2, line3))
tbls_regression %>%
  
  as_gt() %>%
  gt::gtsave("table3_part2_anonymizeddata.html")


###                      TABLE 3: SENSITIVITY ANALYSIS                       ### 

## Manski type bounds
### Estimating a continium of confidence intervals when the number of participants 
### with symptoms among those with missing data ranges from none to all

# Lowest bound: No participants with missing data have symptoms
final0 <- final %>%
  mutate(infection = if_else(completed == 0, 0, infection))

# Highest bound: All participants with missing data have symptoms
final1 <- final %>%
  mutate(infection = if_else(completed == 0, 1, infection))


#Lower Manski bounds
glm(infection ~ group, family = binomial(link = "logit"), data= final0) %>%
  tbl_regression(exponentiate = T,
                 show_single_row= "group",
                 label = list(group ~"Lower Manski bounds"))
#Higher Manski bounds  
glm(infection ~ group, family = binomial(link = "logit"), data= final1) %>%
  tbl_regression(exponentiate = T,
                 show_single_row= "group",
                 label = list(group ~"Higher Manski bounds"))



### Scenario 1-3

## Building data frames for each scenario

### Identify completed cases
final_cc <- final%>%
  filter(!is.na(infection))

### Identify cases with missing outcome
final_miss <- final %>%
  filter(is.na(infection))

# Number of participants with symptoms in each group i scenario 1, 2 and 3

## Control arm, number of non-completed participants with symptoms:
### Same incidence as completed cases: 36 participants with symptoms
incidence_control_1 <- round(mean(final_cc[final_cc$group==0,]$infection)*nrow(final_miss[final_miss$group==0,]))
### 50% lower incidence as completed cases: 18 participants with symptoms
incidence_control_0.5 <- round(mean(final_cc[final_cc$group==0,]$infection)*nrow(final_miss[final_miss$group==0,]))*0.5
## Intervention arm, number of non-completed participants with symptoms:
### Same incidence as completed cases: 43 participants with symptoms
incidence_intervention_1 <- round(mean(final_cc[final_cc$group==1,]$infection)*nrow(final_miss[final_miss$group==1,]))
### 50% higher incidence as completed cases: 64 participants with symptoms
incidence_intervention_1.5 <- round(mean(final_cc[final_cc$group==1,]$infection)*nrow(final_miss[final_miss$group==1,]))*1.5
# Data frame for Scenario 1 (contr. arm: 50% lower incidence, int. arm: same incidence)
final_miss0_1 <- final_miss %>%
  filter(group == 0) %>%
  mutate(infection = if_else(row_number() <= incidence_control_0.5, 1, 0))

final_miss1_1 <- final_miss %>%
  filter(group == 1) %>%
  mutate(infection = if_else(row_number() <= incidence_intervention_1, 1, 0))

# Joining data sets for scenario 1
miss_s1 <- full_join(final_miss0_1, final_miss1_1)
scenario1 <- full_join(final_cc, miss_s1)

# Data frame for Scenario 2 (contr. arm: same incidence, int. arm: 50% higher incidence)
final_miss0_2 <- final_miss %>%
  filter(group == 0) %>%
  mutate(infection = if_else(row_number() <= incidence_control_1, 1, 0))

final_miss1_2 <- final_miss %>%
  filter(group == 1) %>%
  mutate(infection = if_else(row_number() <= incidence_intervention_1.5, 1, 0))

# Joining data sets for scenario 2
miss_s2 <- full_join(final_miss0_2, final_miss1_2)
scenario2 <- full_join(final_cc, miss_s2)

# Data frame for Scenario 3 (contr. arm: 50% lower incidence, int. arm: 50% higher incidence)
miss_s3 <- full_join(final_miss0_1, final_miss1_2)
scenario3 <- full_join(final_cc, miss_s3)



## Testing the scenarios
sc1 <- glm(infection ~ group, family = binomial(link = "logit"), data = scenario1)%>%
  tbl_regression(exponentiate = T,
                 show_single_row= "group",
                 label = list(group ~"Scenario 1"))
sc2 <- glm(infection ~ group, family = binomial(link = "logit"), data = scenario2)%>%
  tbl_regression(exponentiate = T,
                 show_single_row= "group",
                 label = list(group ~"Scenario 2"))
sc3 <- glm(infection ~ group, family = binomial(link = "logit"), data = scenario3)%>%
  tbl_regression(exponentiate = T,
                 show_single_row= "group",
                 label = list(group ~"Scenario 3"))

tbl_stack(list(sc1, sc2, sc3))

###                                  ADHERENCE                               ### 
final %>%
  select(group, facemask_flw,public_transport_flw, cultural_flw, resturant_flw) %>%
  tbl_summary(
    by = group,
    type = list(facemask_flw ~ "categorical",
                public_transport_flw ~"dichotomous",
                cultural_flw ~ "dichotomous",
                resturant_flw ~ "dichotomous" ),
    label = list(facemask_flw ~ "Use of face mask",
                 public_transport_flw ~ "Using public transport",
                 cultural_flw ~ "Attendance cultural events",
                 resturant_flw ~ "Visiting restaurant")) %>%
  add_difference() %>%
  modify_header(stat_1 = "**Control group**, n={n}" ,
                stat_2 = "**Intervention group**, n={n}") %>%
  modify_caption("Table: Adherence") #%>%



###                               SUBGROUP GRAPH                             ### 

#Subgroup function
 subgrouprow <- function(var_nam){
   final <- final %>%
     mutate(subgroup = !!rlang::sym(var_nam))
   mat <-NULL
   raw <- NULL
   
   
   for(j in levels(as.factor(final$subgroup))) {
     subsetdata<- final %>%
       filter(subgroup==j)
     tab <- table(subsetdata$group, subsetdata$infection)
     OR_reg <- glm(infection ~ group, family = binomial(link = "logit"), data = subsetdata)
     OR <-tidy(OR_reg, conf.int = T, conf.level = 0.95, exponentiate = T)
     
     RD <- DescTools::BinomDiffCI(x1 = tab[2,2],
                                  x2 = tab[1,2],
                                  n1 = tab[2,2]+tab[2,1],
                                  n2 = tab[1,2]+tab[1,1],
                                  method = "wald",
                                  sides = "two.sided")
     tabs <- tibble("Variable" = var_nam,
                    "Subgroup" = j,
                    "Intervention group" = glue::glue("{tab[2,2]}/{sum(tab[2,])} ({scales::percent(tab[2,2]/sum(tab[2,]), decimal.mark ='.', accuracy = 0.1)})"),
                    "Control group" = glue::glue("{tab[1,2]}/{sum(tab[1,])} ({scales::percent(tab[1,2]/sum(tab[1,]), decimal.mark ='.', accuracy = 0.1)})"),
                    "OR" = glue::glue("{format(OR[[2,2]], digits = 2,  decimal.mark ='.')} ({format(OR[[2,6]], digits = 2, decimal.mark ='.')} to {format(OR[[2,7]], digits = 2, decimal.mark ='.')})"),
                    "Absolute risk difference" = glue::glue("{scales::percent(RD[1], accuracy = 0.1, decimal.mark ='.')} ({scales::percent(RD[2], accuracy = 0.1, decimal.mark ='.', suffix='')} to {scales::percent(RD[3], accuracy = 0.1, decimal.mark ='.', suffix = '')})")#,
                    # est = RR$measure[2,1],
                    # low = RR$measure[2,2],
                    # hi = RR$measure[2,3]
                    
     )
     tabs2 <- tibble("Variable" = var_nam,
                     "Subgroup" = j,
                     "exposed_int_gr" = tab[2,2],
                     "sum_int_gr" = sum(tab[2,]),
                     "exposed_cont_gr" = tab[1,2],
                     "sum_cont_gr" = sum(tab[1,]),
                     "OR_est" = OR[[2,2]],
                     "OR_lwr" = OR[[2,6]],
                     "OR_upr" = OR[[2,7]],
                     "RD_est" = RD[1],
                     "RD_lwr" = RD[2],
                     "RD_upr" = RD[3]
     )
     
     mat <- dplyr::bind_rows(mat, tabs)
     raw <- dplyr::bind_rows(raw, tabs2)
   }
   return(list(mat, raw))
 }
 
 ## Some changes to the dataset
 final <- final %>%
   mutate(children_yesno = case_when(children_in_household == "0" ~ "1",
                                     TRUE ~ "2",
   ))%>%
   mutate(maskusage= case_when(facemask_use ==1 ~ "1",
                               facemask_use ==2 ~ "1",
                               facemask_use ==3 ~ "1",
                               facemask_use ==4 ~ "2",
                               facemask_use ==5 ~ "2",
                               facemask_use ==6 ~ "2",
   )) %>%
   mutate(attitude= case_when(attitude_facemask ==1 ~ "1",
                              attitude_facemask ==2 ~ "1",
                              attitude_facemask ==3 ~ "2",
                              attitude_facemask ==4 ~ "3",
                              attitude_facemask ==5 ~ "3",
                              attitude_facemask ==6 ~ "2"))
 
 
 # Making the sub group table
 names_subgroup <- c("Male", 
                     "Female", 
                     "Under 30", 
                     "30-60", 
                     "Over 60",
                     "Has Children",
                     "No Children",
                     "Wore face masks < 50%",
                     "Wore face masks > 50%",
                     "Attitude reduce",
                     "Attitude no effect",
                     "Attitude increase")
 
 table_subgroup <- subgrouprow("female")[[1]] %>%
   rbind(subgrouprow("age_cat")[[1]]) %>%
   rbind(subgrouprow("children_yesno")[[1]]) %>%
   rbind(subgrouprow("maskusage")[[1]]) %>%
   rbind(subgrouprow("attitude")[[1]])%>%
   cbind(names_subgroup)
 
 
 
 
 rawtable_subgroup <- subgrouprow("female")[[2]] %>%
   rbind(subgrouprow("age_cat")[[2]]) %>%
   rbind(subgrouprow("children_yesno")[[2]]) %>%
   rbind(subgrouprow("maskusage")[[2]]) %>%
   rbind(subgrouprow("attitude")[[2]]) %>%
   cbind(names_subgroup)
 
 
 #Interaction effect
 int_female <- anova(glm(formula=infection ~ group*female, data = final, family = binomial(link="logit")), test = "Chisq")$`Pr(>Chi)`[4]
 int_age <- anova(glm(formula=infection ~ group*factor(age_cat), data = final, family = binomial(link="logit")), test = "Chisq")$`Pr(>Chi)`[4]
 int_children <- anova(glm(formula=infection ~ group*children_yesno, data = final, family = binomial(link="logit")), test = "Chisq")$`Pr(>Chi)`[4]
 int_maskuse <- anova(glm(formula=infection ~ group*maskusage, data = final, family = binomial(link="logit")), test = "Chisq")$`Pr(>Chi)`[4]
 int_attitude <- anova(glm(formula=infection ~ group*factor(attitude), data = final, family = binomial(link="logit")), test = "Chisq")$`Pr(>Chi)`[4]
 
 subgr_inteffect <- cbind(table_subgroup, "P-value for interaction" = c(rep(glue::glue("{format(int_female, digits = 2,  decimal.mark ='.')}"),2),
                                                                        rep(glue::glue("{format(int_age, digits = 2,  decimal.mark ='.')}"),3),
                                                                        rep(glue::glue("{format(int_children, digits = 2,  decimal.mark ='.')}"),2),
                                                                        rep(glue::glue("{format(int_maskuse, digits = 2,  decimal.mark ='.')}"),2),
                                                                        rep(glue::glue("{format(int_attitude, digits = 2,  decimal.mark ='.')}"),3)))
 
 
 
 
 
 table_subgroup$names_subgroup <- factor(table_subgroup$names_subgroup, levels = rev(names_subgroup))
 rawtable_subgroup$var <- factor(rawtable_subgroup$names_subgroup, levels = rev(names_subgroup))
 rawtable_subgroup$colour <- rep(c("white", "gray95"), length(nrow(rawtable_subgroup)))
 
 tabsubgr <- rawtable_subgroup %>%
   mutate("Intervention group" = glue::glue("{exposed_int_gr}/{sum_int_gr}"),
          "Control group" = glue::glue("{exposed_cont_gr}/{sum_cont_gr}"),
          "OR" = glue::glue("{format(OR_est, digits = 2,  decimal.mark ='.')} ({format(OR_lwr, digits = 2, decimal.mark ='.')} to {format(OR_upr, digits = 2, decimal.mark ='.')})")) %>%
   cbind("int" = c(" ",
                   glue::glue("{format(int_female, digits = 2,  decimal.mark ='.')}"),
                   rep(" ",2),
                   glue::glue("{format(int_age, digits = 2,  decimal.mark ='.')}"),
                   " ",
                   glue::glue("{format(int_children, digits = 2,  decimal.mark ='.')}"),
                   " ",
                   glue::glue("{format(int_maskuse, digits = 2,  decimal.mark ='.')}"),
                   rep(" ",2),
                   glue::glue("{format(int_attitude, digits = 2,  decimal.mark ='.')}"))) %>%
   ggplot(aes(y = var)) +
   geom_hline(aes(yintercept = var, colour = colour), linewidth = 7) +
   geom_text(aes(x = 0, label = var), hjust = 0) +
   geom_text(aes(x = 4, label = `Intervention group`)) +
   geom_text(aes(x = 6, label = `Control group`)) +
   geom_text(aes(x = 8, label = OR)) + 
   geom_text(aes(x = 11, label = int)) + 
   scale_colour_identity() +
   theme_void() + 
   theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
 print(tabsubgr)
 plotsubgr <- rawtable_subgroup %>%
   ggplot(aes(x = var, y = OR_est, color = Variable)) +
   geom_errorbar(aes(ymin = OR_lwr, ymax = OR_upr), width =  .1, linetype = 1, linewidth = 1.3) +
   scale_y_log10(breaks = c(0.25, 0.5, 1, 2, 4), 
                 labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0),
                 name = "Odds ratio\n") +
   geom_point(size = 3) +
   #scale_y_continuous(name = "Odds ratio\n") +
   #scale_x_discrete(name = "") +
   geom_hline(yintercept = 1, linetype = 2, linewidth = 1) +
   theme_classic() +
   coord_flip(ylim = c(0.20,5)) +
   #coord_cartesian()+
   theme(legend.position = "none",
         axis.text.y = element_blank(),
         axis.ticks.y=element_blank(),
         axis.title.y = element_blank(),
         plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
 print(plotsubgr)
 #gridExtra::grid.arrange(tabsubgr, plotsubgr, ncol=2, align = 'v')
 cowplot::plot_grid(plotlist = list(tabsubgr, plotsubgr), ncol=2, align='h', rel_widths = c(2,1))
 
 titplot <- tibble::tibble(
   "var"  = "Subgroup",
   "Intervention group" = "Intervention group",
   "Control group" = "Control group",
   "OR" = "OR (95% CI)",
   "int" = "P value" ) %>%
   ggplot(aes(y = var)) +
   geom_hline(aes(yintercept = var, color = "white"), linewidth = 7) +
   geom_text(aes(x = 0, label = var), hjust = 0, fontface=2) +
   geom_text(aes(x = 4, label = `Intervention group`), fontface=2) +
   geom_text(aes(x = 6, label = `Control group`), fontface=2) +
   geom_text(aes(x = 8, label = OR), fontface=2) + 
   geom_text(aes(x = 11, label = int), fontface=2) + 
   scale_y_discrete(expand = c(0,0)) +
   scale_colour_identity() +
   theme_void() + 
   theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
         text = element_text(face="bold"),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x= element_blank()) +
   coord_cartesian(clip = "on")
 #cowplot::plot_grid(plotlist = list(titplot, tabsubgr), ncol = 1)
 print(titplot)
 
 cowplot::plot_grid(plotlist = list(titplot, NULL, tabsubgr, plotsubgr), ncol=2, align='h', rel_widths = c(2,1), rel_heights = c(1,8)) 
 widths = c(1, 3)
ggsave("subgroupplot.png", dpi = 400)
ggsave("subgroupplot.tiff", dpi = 400)


###                               RISK RATIO                                 ### 
#Not all adjustment variables are available in the anonymized data set, therefore
#Only the non-adjusted Risk ratios are calculated here.


infectionRR <- glm(infection ~ group, family = binomial(link = "log"), data= final) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row= "group",
                 label = list(group ~"Self reported respiratory symptoms (complete cases)"))%>%
  modify_header(estimate = "**Risk ratio (95% CI)**")
load("miceinfectionRR.Rdata")
c19RR <- glm(c19 ~ group, family = binomial(link = "log"), data= final) %>%
  tbl_regression(exponentiate = T,
                 show_single_row= "group",
                 label = list(group ~"Self reported COVID-19 case"))
# msis_caseRR <- glm(msis_case ~ group, family = binomial(link = "log"), data= final) %>%
#   tbl_regression(exponentiate = T,
#                  show_single_row= "group",
#                  label = list(group ~"Reported COVID-19 case"))

tbls_regression_RR <- tbl_stack(
  tbls = list(infectionRR, miceinfectionRR, c19RR))
tbls_regression_RR %>%
  as_gt() %>%
  gt::gtsave("risk_ratio_anonymized.html")