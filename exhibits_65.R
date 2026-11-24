################################################################################
# exhibits.R
# script for consort diagram numbers
# 7/30/26
################################################################################

library(readxl)
library(tidyverse)
library(here)
library(readr)
library(marginaleffects)
library(consort)
library(grid)
library(scales)
library(emmeans)
library(dplyr)
library(stringr)

load(here("data", "gee_mod_df_model2.RData"))
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


# figure 2 flow counts ---------------------------------------------------------

df_total_all2 = df_total_all |> 
  filter(!(Age %in% c("18-64","18+ (all adults)"))) |> 
  mutate(Variable = str_remove(Variable,"_18|_65")) |> 
  group_by(Variable) |> 
  summarize(total = sum(total,na.rm = T)) |> 
  ungroup() |> 
  mutate(Variable2 = ifelse(Variable == "total_ed_obs_adm_psych","total_ed_obs_adm",Variable)) |> 
  group_by(Variable2) |> 
  summarize(total = sum(total,na.rm = T)) |> 
  pivot_wider(names_from = Variable2, values_from = total)

total_ed_enc = df_total_all2$total_ed_enc 

total_adm = sum(df_total_all2 |> dplyr::select(-total_ed_enc))

total_not_adm = total_ed_enc - total_adm

total_adm_psych = df_total_all2$total_adm_psych

total_ed_obs_adm = df_total_all2$total_ed_obs_adm

total_analytic_sample = total_adm - total_adm_psych - total_ed_obs_adm

  # Box layout
boxes <- tibble(
  id = c("start", "not_adm", "adm", "ed_obs", "psych", "final"),
  label = c(
    paste0("ED encounters\nN = ", comma(total_ed_enc)),
    paste0("Excluded: non-admitted ED disposition\nN = ", comma(total_not_adm)),
    paste0("Admitted encounters\nN = ", comma(total_adm)),
    paste0("Excluded: ED observation admissions\nN = ", comma(total_ed_obs_adm)),
    paste0("Excluded: psychiatric admissions\nN = ", comma(total_adm_psych)),
    paste0("Analytic sample\nN = ", comma(total_analytic_sample))
  ),
  x = c(0, 4.6, 0, 4.4, 4.4, 0),
  y = c(10, 8, 8, 6.2, 4.6, 1.7),
  width = c(3.4, 3.8, 3.4, 4.3, 4.3, 3.4),
  height = c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1)
) |>
  mutate(
    xmin = x - width / 2,
    xmax = x + width / 2,
    ymin = y - height / 2,
    ymax = y + height / 2
  )

start   <- boxes |> filter(id == "start")
adm     <- boxes |> filter(id == "adm")
not_adm <- boxes |> filter(id == "not_adm")
ed_obs  <- boxes |> filter(id == "ed_obs")
psych   <- boxes |> filter(id == "psych")
final   <- boxes |> filter(id == "final")

# useful x positions
branch_x1 <- adm$x + 1.4   # where the branch leaves admitted
branch_x2 <- adm$x + 1.9   # vertical trunk for right-side exclusions
branch_x2 <- adm$x + 1.9
branch_y  <- adm$ymin + 0.01   # lower inside admitted box

seg_plain <- tibble(
  x = c(
    start$x,      # ED -> admitted
    start$x,      # ED -> not admitted horizontal
    not_adm$x,    # ED -> not admitted vertical
    adm$x,        # admitted -> analytic
    adm$x,        # admitted -> branch horizontal
    branch_x2,    # trunk down to ED obs
    branch_x2     # trunk down to psych
  ),
  y = c(
    start$ymin,
    start$ymin,
    start$ymin,
    adm$ymin,
    branch_y,
    branch_y,
    ed_obs$y
  ),
  xend = c(
    adm$x,
    not_adm$x,
    not_adm$x,
    final$x,
    branch_x2,
    branch_x2,
    branch_x2
  ),
  yend = c(
    adm$ymax,
    start$ymin,
    not_adm$ymax + 0.18,   # stop above the box
    final$ymax,
    branch_y,
    ed_obs$y,
    psych$y
  )
)

seg_arrow <- tibble(
  x = c(
    not_adm$x,          # not admitted: start above box
    branch_x2,          # ED obs
    branch_x2           # psych
  ),
  y = c(
    not_adm$ymax + 0.18,
    ed_obs$y,
    psych$y
  ),
  xend = c(
    not_adm$x,
    ed_obs$xmin + 0.04,
    psych$xmin + 0.04
  ),
  yend = c(
    not_adm$ymax + 0.02,   # arrow points downward into top of box
    ed_obs$y,
    psych$y
  )
)
p <- ggplot() +
  geom_rect(
    data = boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "white",
    color = "#2F2F2F",
    linewidth = 0.8
  ) +
  geom_segment(
    data = seg_plain,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.7,
    color = "#4A4A4A"
  ) +
  geom_segment(
    data = seg_arrow,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.7,
    color = "#4A4A4A",
    arrow = arrow(length = unit(0.12, "inches"), type = "closed")
  ) +
  geom_text(
    data = boxes,
    aes(x = x, y = y, label = label),
    family = "sans",
    size = 4.1,
    lineheight = 1.05
  ) +
  coord_cartesian(xlim = c(-2.5, 7.8), ylim = c(0.8, 10.8), clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(20, 30, 20, 30),
    plot.background = element_rect(fill = "white", color = NA)
  )

p 

ggsave(
       filename = here("Figure_2.png"),
       plot = p,
       width = 10,
       height = 8,
       units = "in",
       dpi = 600
     )


# Figure 3 raw boarding rates --------------------------------------------------

obs_long <- df_model2 %>%
  group_by(Measure) %>%
  summarise(
    total_admit = sum(total_admit, na.rm = TRUE),
    boarding_count = sum(boarding_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    boarding_rate = if_else(total_admit > 0, boarding_count / total_admit, 0),
    se = if_else(
      total_admit > 0,
      sqrt(boarding_rate * (1 - boarding_rate) / total_admit),
      0
    ),
    lower = pmax(0, boarding_rate - 1.96 * se),
    upper = pmin(1, boarding_rate + 1.96 * se)
  ) |>
  mutate(rate100  = boarding_rate * 100,
         lower100 = lower * 100,
         upper100 = upper * 100) |>
  mutate(Measure = factor(Measure,
                          levels = c("AFHM", "GEAR", "ECAT", "LOCAL"),
                          labels = c("AFHM", "GEARdef", "ECAT", "LOCAL")))

# GEAR-like palette (names must match the new labels)
gear_cols <- c(
  "AFHM"    = "#0B6B61",
  "GEARdef" = "#F47C20",
  "ECAT"    = "#2CA89A",
  "LOCAL"   = "#8CC63F"
)

f2 <- ggplot(obs_long, aes(x = Measure, y = rate100, color = Measure)) +
  geom_line(
    aes(group = 1),
    linetype = "dashed",
    color = "grey",
    linewidth = 0.8
  ) +
  geom_errorbar(
    aes(ymin = lower100, ymax = upper100),
    width = 0.15,
    linewidth = 1
  ) +
  geom_point(size = 3) +
  geom_text(
    aes(y = upper100,
        label = sprintf("%.1f (%.1f-%.1f)", rate100, lower100, upper100)),
    vjust = -1,
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = gear_cols) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  scale_y_continuous(
    limits = c(30, 55),
    breaks = seq(30, 55, by = 5),
    expand = expansion(mult = 0.02)
  ) +
  labs(
    x = "Boarding definition",
    y = "Observed boarding rate\nper 100 admitted patients"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    plot.margin = margin(8, 12, 8, 8)
  )

f2

ggsave(
  filename = here("Figure_3.png"),
  plot = f2,
  width = 10,
  height = 8,
  units = "in",
  dpi = 600
)

# Values for the text ----------------------------------------------------------

  # descriptive results 

ed_description = df_model2 |> 
  dplyr::select(ED,ruca_cat,teaching_status) |> 
  distinct() 

table1::table1(~ ruca_cat + teaching_status, data = ed_description)

pop_description = df_model2 |> 
  dplyr::select(ED,Age,Population,total_admit) |> 
  distinct()

pop_description |> 
  summarize(sum(total_admit))

pop_description |> 
  group_by(Population) |> 
  summarize(sum(total_admit))

  # boarding results 

obs_long <- df_model2 %>%
  group_by(Measure) %>%
  summarise(
    total_admit = sum(total_admit, na.rm = TRUE),
    boarding_count = sum(boarding_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Measure = factor(Measure, levels = c("AFHM", "GEAR", "ECAT", "LOCAL")),
    boarding_rate = if_else(total_admit > 0, boarding_count / total_admit, 0),
    se = if_else(
      total_admit > 0,
      sqrt(boarding_rate * (1 - boarding_rate) / total_admit),
      0
    ),
    lower = pmax(0, boarding_rate - 1.96 * se),
    upper = pmin(1, boarding_rate + 1.96 * se),
    rate100 = boarding_rate * 100,
    lower100 = lower * 100,
    upper100 = upper * 100,
    raw_rate_ci = sprintf("%.1f (%.1f, %.1f)", rate100, lower100, upper100),
    section = "Observed boarding rate per 100",
    Measure = as.character(Measure),
    rr_ci = NA_character_
  ) %>%
  dplyr::select(section, Measure, raw_rate_ci, rr_ci)


nd <- df_model2 %>%
  mutate(total_admit = 1)

comp_overall <- avg_comparisons(
  gee_mod,
  newdata = nd,
  variables = list(Measure = "pairwise"),
  type = "response",
  comparison = "ratio"
) %>%
  as.data.frame()

comp_overall_afhm_denom <- comp_overall %>%
  mutate(
    lower_old = conf.low,
    upper_old = conf.high,
    flip_afhm = contrast %in% c(
      "mean(AFHM) / mean(ECAT)",
      "mean(AFHM) / mean(GEAR)",
      "mean(AFHM) / mean(LOCAL)"
    ),
    contrast = case_when(
      contrast == "mean(AFHM) / mean(ECAT)" ~ "mean(ECAT) / mean(AFHM)",
      contrast == "mean(AFHM) / mean(GEAR)" ~ "mean(GEAR) / mean(AFHM)",
      contrast == "mean(AFHM) / mean(LOCAL)" ~ "mean(LOCAL) / mean(AFHM)",
      TRUE ~ contrast
    ),
    estimate = if_else(flip_afhm, 1 / estimate, estimate),
    conf.low = if_else(flip_afhm, 1 / upper_old, conf.low),
    conf.high = if_else(flip_afhm, 1 / lower_old, conf.high)
  ) %>%
  dplyr::select(-lower_old, -upper_old, -flip_afhm) %>%
  mutate(
    rr_ci = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
    Measure = contrast,
    section = "Pairwise comparisons",
    raw_rate_ci = NA_character_
  ) %>%
  dplyr::select(
    section,
    raw_rate_ci,
    Measure, rr_ci
  ) 

final_tbl <- bind_rows(
  obs_long,
  comp_overall_afhm_denom
) 

final_tbl
