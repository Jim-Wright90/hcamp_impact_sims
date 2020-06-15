## load impact data 

library(tidyverse)
library(here)
library(rio)
library(fs)
library(colorblindr)
library(gghighlight)
library(forcats)
library(ggrepel)
library(gt)
library(knitr)
library(kableExtra)
library(reactable)
library(plotly)
library(stringr)
library(glue)
library(scales)

files <- dir_ls(here::here("ImPACT_data"), glob = "*.csv")

d <- map_df(files, read_csv, .id = "file",
                 col_types = cols(.default = "c")) %>% 
  modify(~parse_guess(.x)) %>% 
  janitor::clean_names() %>% 
  mutate(year = parse_number(file)) %>% 
  mutate(year = as.factor(year))

d$year

lapply(d, is.double)

lapply(d, function(x) {
  if(is.double(x)) {
    return(mean(x))
  }
  else if(is.character(x) | 
          is.factor(x)) {
    return(table(x))
  }
})

lapply(d, mean, na.rm = TRUE)

by_year <- split(impact, impact$year)
str(by_year)

lapply(by_year, function(x) mean(x$user_age))

lapply(by_year, function(x) {
  ggplot(x, aes(user_memory_composite_score_verbal, user_memory_composite_score_visual)) +
    geom_point() +
    geom_smooth()
})

#6.10.20 trying to change remaining columns to appropriate vector type 
str(impact)

impact$city

impact2 <- d %>% 
  mutate(user_gender = as.factor(user_gender),
         user_handedness = as.factor(user_handedness),
         user_height = as.factor(user_height),
         user_weight = as.factor(user_weight),
         user_country = as.factor(user_country),
         user_first_language = as.factor(user_first_language),
         user_second_language = as.factor(user_second_language),
         user_education_level = as.factor(user_education_level),
         user_special_ed1 = as.factor(user_special_ed1),
         user_special_ed2 = as.factor(user_special_ed2),
         user_special_ed3 = as.factor(user_special_ed3),
         user_special_ed4 = as.factor(user_special_ed4),
         user_special_ed5 = as.factor(user_special_ed5),
         add_adhd = as.factor(add_adhd),
         dyslexia = as.factor(dyslexia),
         autism = as.factor(autism),
         user_treatment_received1 = as.factor(user_treatment_received1),
         user_treatment_received2 = as.factor(user_treatment_received2),
         user_treatment_received3 = as.factor(user_treatment_received3),
         user_treatment_received4 = as.factor(user_treatment_received4),
         user_treatment_received5 = as.factor(user_treatment_received5),
         user_treatment_received6 = as.factor(user_treatment_received6),
         user_treatment_received7 = as.factor(user_treatment_received7),
         user_current_sport = as.factor(user_current_sport),
         user_primary_position = as.factor(user_primary_position),
         user_level_of_participation = as.factor(user_level_of_participation),
         user_years_playing = as.factor(user_years_playing),
         user_number_of_concussions = as.factor(user_number_of_concussions),
         user_concussion_type1 = as.factor(user_concussion_type1),
         user_concussion_type2 = as.factor(user_concussion_type2),
         user_concussion_type3 = as.factor(user_concussion_type3),
         user_concussion_type4 = as.factor(user_concussion_type4),
         user_total_games_missed = as.factor(user_total_games_missed),
         user_concussion_history = as.factor(user_concussion_history),
         user_last_concussion_date = as.factor(user_last_concussion_date),
         test_type = as.factor(test_type),
         test_date = as.factor(test_date),
         exam_language = as.factor(exam_language),
         test_version = as.factor(test_version),
         word_memory_lp = as.factor(word_memory_lp),
         word_memory_dm_correct = as.factor(word_memory_dm_correct),
         word_memory_total_percent_correct = as.factor(word_memory_total_percent_correct),
         design_memory_lp = as.factor(design_memory_lp),
         design_memory_dm_correct = as.factor(design_memory_dm_correct),
         design_memory_total_percent_correct = as.factor(design_memory_total_percent_correct),
         x_oaverage_incorrect = as.factor(x_oaverage_incorrect),
         three_letters_percentage_letters_correct = as.factor(three_letters_percentage_letters_correct))

str(impact2)

impact3 <- impact2 %>% 
  mutate(user_height = as.numeric(user_height),
         user_weight = as.numeric(user_weight),
         user_education_level = as.numeric(user_education_level),
         user_years_playing = as.numeric(user_years_playing),
         user_number_of_concussions = as.numeric(user_number_of_concussions),
         user_concussion_type1 = as.numeric(user_concussion_type1),
         user_concussion_type2 = as.numeric(user_concussion_type2),
         user_concussion_type3 = as.numeric(user_concussion_type3),
         user_concussion_type4 = as.numeric(user_concussion_type4),
         user_total_games_missed = as.numeric(user_total_games_missed),
         word_memory_lp = as.numeric(word_memory_lp),
         word_memory_dm_correct = as.numeric(word_memory_dm_correct),
         word_memory_total_percent_correct = as.numeric(word_memory_total_percent_correct),
         design_memory_lp = as.numeric(design_memory_lp),
         design_memory_dm_correct = as.numeric(design_memory_dm_correct),
         design_memory_total_percent_correct = as.numeric(design_memory_total_percent_correct),
         x_oaverage_incorrect = as.numeric(x_oaverage_incorrect),
         three_letters_percentage_letters_correct = as.numeric(three_letters_percentage_letters_correct))

str(impact3)

impact <- impact3

str(impact) # all necessary columns are changed to either factor or numeric 

impact$year


names(impact)

ggplot(impact, aes(user_age)) +
  geom_histogram(fill = "#56B4E9",
                 color = "white", 
                 alpha = 0.9,
                 bins = 15) +
  facet_wrap(~year) +
  scale_x_continuous("Student Age", limits = c(10, 20),
                     breaks = c(10, 12, 14, 16, 18, 20)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray80")) +
  labs(x = "Student Age",
       y = "Number of Concussions", 
       title = "Distribution of Concussions by Age") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title=element_text(size=10))
  
mean_2 <- function(x) {
  z <- na.omit(x)
  sum(z) / length(z)
}

mean_2(impact$user_age)

mean_2(impact$user_number_of_concussions)

impact %>% 
  count(user_gender)

sport <- impact %>% 
  count(user_current_sport)


# Meeting with Troy

# What is the percent increase in composite score from first baseline to second baseline?
# What is the duration of time between baseline test administrations? 

# Look at baseline and then at individual's post-test 1 - what are changes from baseline to post-test1

## Typical baseline time is freshman and junior year 

## word_memory_hits - look at with other composite test variables 

## which symptoms are correlated with specific composite and subtest scores 

### user symptom delayed variables are collected after testing 

names(impact)

impact$test_type

# Baseline with 2 plus signs are considered invalid 

impact %>% 
  count(test_type)

impact_final %>% 
  count(test_date)

impact$passport_id
impact$first_name

impact %>% 
  count(passport_id)

names(impact)

test_time <- impact %>% 
  select(year,
         passport_id, 
         user_gender, 
         user_age, 
         test_type, 
         test_date, 
         test_version, 
         user_current_sport,
         user_number_of_concussions,
         user_education_level,
         user_memory_composite_score_verbal,
         user_memory_composite_score_visual,
         user_impulse_control_composite_score,
         user_reaction_time_composite_score,
         user_impulse_control_composite_score,
         user_visual_motor_composite_score) 

str(test_time)

test_time$test_type

test_time <- test_time %>% 
  mutate_all(funs(str_replace(., "Post-Injury 1", "Post_Injury_1")))

test_time <- test_time %>% 
  mutate_all(funs(str_replace(., "Post-Injury 2", "Post_Injury_2")))

test_time <- test_time %>% 
  mutate_all(funs(str_replace(., "Post-Injury 3", "Post_Injury_3")))

test_time <- test_time %>% 
  mutate_all(funs(str_replace(., "Post-Injury 4", "Post_Injury_4")))

test_time <- test_time %>% 
  mutate_all(funs(str_replace(., "Baseline ++", "Baseline_inv")))

test_time$test_type

str(test_time)


test_time <- test_time %>% 
  filter(test_type != "Baseline_inv++")

test_time <- test_time %>% 
  filter(test_type != "Post_Injury_2")

test_time <- test_time %>% 
  filter(test_type != "Post_Injury_3")

test_time <- test_time %>% 
  filter(test_type != "Post_Injury_4")

test_time

test_time <- test_time %>% 
  mutate(year = as.factor(year),
         user_gender = as.factor(user_gender),
         test_type = as.factor(test_type),
         test_date = as.factor(test_date),
         test_version = as.factor(test_version),
         user_current_sport = as.factor(user_current_sport),
         user_number_of_concussions = as.factor(user_number_of_concussions),
         user_education_level = as.factor(user_education_level),
         user_memory_composite_score_verbal = as.factor(user_memory_composite_score_verbal),
         user_memory_composite_score_visual = as.factor(user_memory_composite_score_visual),
         user_impulse_control_composite_score = as.factor(user_impulse_control_composite_score),
         user_reaction_time_composite_score = as.factor(user_reaction_time_composite_score),
         user_visual_motor_composite_score = as.factor(user_visual_motor_composite_score))

test_time <- test_time %>% 
  mutate(user_number_of_concussions = as.numeric(user_number_of_concussions),
         user_education_level = as.numeric(user_education_level),
         user_memory_composite_score_verbal = as.numeric(user_memory_composite_score_verbal),
         user_memory_composite_score_visual = as.numeric(user_memory_composite_score_visual),
         user_impulse_control_composite_score = as.numeric(user_impulse_control_composite_score),
         user_reaction_time_composite_score = as.numeric(user_reaction_time_composite_score),
         user_visual_motor_composite_score = as.numeric(user_visual_motor_composite_score))

str(test_time)

test_time %>% 
  arrange(passport_id)

str(test_time)

baseline <- test_time %>% 
  filter(test_type == "Baseline")

str(baseline)

baseline_tidy <- baseline %>% 
  pivot_longer(
    11:15,
    names_to = "composite",
    names_prefix = "user_",
    values_to = "score")

baseline2 <- baseline %>% 
  group_by(passport_id) %>% 
  mutate(baseline_number = row_number()) %>% 
  spread(baseline_number, test_type, sep = "_")

baseline2_tidy <- baseline_tidy %>% 
  group_by(passport_id) %>% 
  mutate(baseline_number = row_number()) %>% 
  spread(baseline_number, test_type, sep = "_")


baseline3 <- baseline %>% 
  group_by(passport_id) %>% 
  filter(n() > 1) %>% 
  mutate(baseline_number = row_number()) %>% 
  spread(baseline_number, test_type, sep = "_") %>% 
  arrange(passport_id)

baseline3 <- baseline3[, -c(17:23)]

str(baseline3)

baseline3 %>% 
  count(passport_id)



# 6.12.20 This code below is what's needed for markdown file
head(baseline3)

baseline3$test_date <- as.Date(baseline3$test_date, format = "%m/%d/%Y")

mem_verbal <- baseline3 %>% 
  group_by(passport_id) %>% 
  mutate(number = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = passport_id,
    names_from = number,
    values_from = c(test_date, user_memory_composite_score_verbal),
    names_glue = "{.value}_{number}"
  ) %>% 
  mutate(time_between = test_date_2 - test_date_1) 

mem_verbal <- mem_verbal %>% 
  select(passport_id, 
         test_date_1, 
         test_date_2, 
         user_memory_composite_score_verbal_1, 
         user_memory_composite_score_verbal_2,
         time_between) %>% 
  mutate(memory_verbal_diff = user_memory_composite_score_verbal_2 - user_memory_composite_score_verbal_1)

mem_verbal <- mem_verbal %>% 
  filter(memory_verbal_diff >= 0)

mem_verbal <- mem_verbal %>% 
  mutate(memory_verbal_percent_inc = memory_verbal_diff / user_memory_composite_score_verbal_1 * 100)

mean_2(mem_verbal$memory_verbal_diff)
mean_2(mem_verbal$time_between)
mean_2(mem_verbal$memory_verbal_percent_inc)

mem_visual <- baseline3 %>% 
  group_by(passport_id) %>% 
  mutate(number = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = passport_id,
    names_from = number,
    values_from = c(test_date, user_memory_composite_score_visual),
    names_glue = "{.value}_{number}"
  ) %>% 
  mutate(time_between = test_date_2 - test_date_1) 

mem_visual <- mem_visual %>% 
  select(passport_id, 
         test_date_1, 
         test_date_2, 
         user_memory_composite_score_visual_1, 
         user_memory_composite_score_visual_2,
         time_between) %>% 
  mutate(memory_visual_diff = user_memory_composite_score_visual_2 - user_memory_composite_score_visual_1)

mem_visual <- mem_visual %>% 
  filter(memory_visual_diff >= 0)

mean_2(mem_visual$memory_visual_diff)
mean_2(mem_visual$time_between)

mem_visual <- mem_visual %>% 
  mutate(memory_visual_percent_inc = memory_visual_diff / user_memory_composite_score_visual_1 * 100)

impulse_control <- baseline3 %>% 
  group_by(passport_id) %>% 
  mutate(number = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = passport_id,
    names_from = number,
    values_from = c(test_date, user_impulse_control_composite_score),
    names_glue = "{.value}_{number}"
  ) %>% 
  mutate(time_between = test_date_2 - test_date_1) 

impulse_control <- impulse_control %>% 
  select(passport_id, 
         test_date_1, 
         test_date_2, 
         user_impulse_control_composite_score_1, 
         user_impulse_control_composite_score_2,
         time_between) %>% 
  mutate(impulse_control_diff = user_impulse_control_composite_score_2 - user_impulse_control_composite_score_1)

impulse_control <- impulse_control %>% 
  filter(impulse_control_diff >= 0)

impulse_control <- impulse_control %>% 
  mutate(impulse_control_percent_inc = impulse_control_diff / user_impulse_control_composite_score_1 * 100)

reaction_time <- baseline3 %>% 
  group_by(passport_id) %>% 
  mutate(number = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = passport_id,
    names_from = number,
    values_from = c(test_date, user_reaction_time_composite_score),
    names_glue = "{.value}_{number}"
  ) %>% 
  mutate(time_between = test_date_2 - test_date_1) 

reaction_time <- reaction_time %>% 
  select(passport_id, 
         test_date_1, 
         test_date_2, 
         user_reaction_time_composite_score_1, 
         user_reaction_time_composite_score_2,
         time_between) %>% 
  mutate(reaction_time_diff = user_reaction_time_composite_score_2 - user_reaction_time_composite_score_1)

reaction_time <- reaction_time %>% 
  filter(reaction_time_diff >= 0)

reaction_time <- reaction_time %>% 
  mutate(reaction_time_percent_inc = reaction_time_diff / user_reaction_time_composite_score_1 * 100)

visual_motor <- baseline3 %>% 
  group_by(passport_id) %>% 
  mutate(number = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = passport_id,
    names_from = number,
    values_from = c(test_date, user_visual_motor_composite_score),
    names_glue = "{.value}_{number}"
  ) %>% 
  mutate(time_between = test_date_2 - test_date_1) 

visual_motor <- visual_motor %>% 
  select(passport_id, 
         test_date_1, 
         test_date_2, 
         user_visual_motor_composite_score_1, 
         user_visual_motor_composite_score_2,
         time_between) %>% 
  mutate(visual_motor_diff = user_visual_motor_composite_score_2 - user_visual_motor_composite_score_1)

visual_motor <- visual_motor %>% 
  filter(visual_motor_diff >= 0)

visual_motor <- visual_motor %>% 
  mutate(visual_motor_percent_inc = visual_motor_diff / user_visual_motor_composite_score_1 * 100)

mem_verbal <- mem_verbal %>% 
  arrange(passport_id)

mem_visual <- mem_visual %>% 
  arrange(passport_id)

impulse_control <- impulse_control %>% 
  arrange(passport_id)

reaction_time <- reaction_time %>% 
  arrange(passport_id)

visual_motor <- visual_motor %>% 
  arrange(passport_id)

test_join <- full_join(mem_verbal, mem_visual)

test_join2 <- full_join(test_join, impulse_control)

test_join3 <- full_join(test_join2, reaction_time)

baseline_final <- full_join(test_join3, visual_motor)

mean_2(impulse_control$impulse_control_diff)
mean_2(baseline_final$impulse_control_diff)

baseline_all_positive <- baseline_final %>% 
  na.omit()

mean_2(baseline_all_positive$time_between)


mem_verbal %>% 
  summarize(Mean = mean(memory_verbal_diff),
            SD = sd(memory_verbal_diff),
            Min = min(memory_verbal_diff),
            Max = max(memory_verbal_diff),
            Total = length(memory_verbal_diff)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  reactable()

create_react <- function(df, var) {
    df %>% 
      summarize(Mean = mean({{var}}),
                SD = sd({{var}}),
                Min = min({{var}}),
                Max = max({{var}}),
                Total = length({{var}})) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      reactable()
}

react_percent <- function(df, var) {
  df %>% 
    summarize(Mean = mean({{var}}),
              SD = sd({{var}}),
              Min = min({{var}}),
              Max = max({{var}}),
              Total = length({{var}})) %>% 
    mutate_if(is.numeric, round, 2) %>% 
    reactable(columns = list(
      Mean = colDef(format = colFormat(percent = TRUE)),
      SD = colDef(format = colFormat(percent = TRUE)),
      Min = colDef(format = colFormat(percent = TRUE)),
      Max = colDef(format = colFormat(percent = TRUE))))
}

create_react(mem_verbal, memory_verbal_diff)

react_percent(mem_verbal, memory_verbal_percent_inc)

create_react(mem_visual, memory_visual_diff)

react_percent(mem_visual, memory_visual_percent_inc)

create_react(impulse_control, impulse_control_diff)

react_percent(impulse_control, impulse_control_percent_inc)

create_react(reaction_time, reaction_time_diff)

react_percent(reaction_time, reaction_time_percent_inc)

create_react(visual_motor, visual_motor_diff)

react_percent(visual_motor, visual_motor_percent_inc)

str(baseline_final$time_between)


time_data <- baseline_final %>% 
  mutate(time_between = as.numeric(time_between))

time_data2 <- baseline_all_positive %>% 
  mutate(time_between = as.numeric(time_between))

create_react(time_data, time_between)
create_react(time_data2, time_between)
