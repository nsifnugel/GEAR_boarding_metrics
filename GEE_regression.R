################################################################################
# analysis.R
# script to analyze data from HW 14d pt2
# 6/10/26
################################################################################

library(readxl)
library(tidyverse)
library(here)
library(readr)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(geepack)
library(multcomp)
library(multcompView)

load(here("data", "combined_df.RData"))

df_final = df_final |>
  filter(ED %in% df_info$ED)

df_total_all = df_total_all |>
  filter(ED %in% df_info$ED)

df_info = df_info |>
  filter(!is.na(teaching_status))

df_edba = df_final |> filter(Measure == 'EDBA')

df = df_info |> 
  left_join({df_final |> filter(Measure != "EDBA")},by=c("Site","ED")) |> 
  rename(ruca = 'ED RUCA Code') |> 
  mutate(ruca = factor(ruca,levels=c("1","2","3","4","5","7","10","10.1"))) |> 
  mutate(teaching_status = ifelse(is.na(teaching_status),"Unknown Teaching Status",teaching_status)) |> 
  mutate(
    ruca_cat = case_when(
      ruca %in% c("1","2","3","10.1") ~ "Urban",
      ruca %in% c("4","5") ~ "Large Rural City/Town",
      ruca %in% c("10","7") ~ "Small and Isolated Small Rural Town",
      TRUE ~ NA_character_
    ),
    ruca_cat = factor(ruca_cat,levels=c("Urban","Large Rural City/Town","Small and Isolated Small Rural Town"))
  )


# checking merge

df |> 
  dplyr::filter(is.na(`ED Name`))


# regression modeling data -----------------------------------------------------

df_model = df |> 
  filter(Population != "Admission from ED Observation") |> 
  filter(!(Age %in% c("18-64","18+ (all adults)"))) |> 
  mutate(Measure = factor(Measure, levels= c("GEAR","ECAT","AFHM","LOCAL")),
         boarding_count = ifelse(is.na(boarding_count),0,boarding_count),
         Population = factor(Population,levels=c("Standard Admission","Admission, Patient Class Observation")),
         teaching_status = factor(teaching_status,levels=c("Major","Minor","Non-Teaching","Unknown"))) |> 
  dplyr::select(Site,ED,Measure,Population,Age,boarding_count,total_admit,ruca_cat,teaching_status)

df_model2 = df_model |>
  filter(!is.na(total_admit), total_admit > 0)

df_model2 = df_model2 |> 
  mutate(
    ED_id = interaction(Site, ED, drop = TRUE)
  ) |> 
  droplevels()

# GEE models -------------------------------------------------------------------

gee_mod <- geeglm(
  boarding_count ~ Measure + Population + teaching_status + ruca_cat + Site +
    offset(log(total_admit)),
  id = ED_id,
  data = df_model2,
  family = poisson(link = "log"),
  corstr = "exchangeable"
)

summary(gee_mod)

gtsummary::tbl_regression(gee_mod, exp = T)

# saving data ------------------------------------------------------------------

save(
  gee_mod,
  df_model2,
  file = here::here("data", "gee_mod_df_model2.RData")
)

