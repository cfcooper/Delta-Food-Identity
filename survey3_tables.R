
# --- 1. Load Libraries -------------------------------------------------------

library(dplyr)
library(tidyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)


rm(list=ls()) # Caution: this clears the Environment

# --- 2. Load Data ------------------------------------------------------------

df <- readRDS("cleaneddata/df_full.rds")

# confirm structure
column_lookup <- data.frame(    
  column_number = 1:ncol(df),
  column_name = names(df))



# --- 3. Breakdown Groups  ----------------------------

# all is just df

group_delta <- df[df$target_state == 1, ]
group_nondelta <- df[df$target_state == 0, ]
group_bnondelta <- group_nondelta[group_nondelta$race_b == 1, ]
group_wdelta <- group_delta[group_delta$race_b == 0, ]



# Define your question columns
q_cols <- c("Q24.w.attn.check_1", "Q24.w.attn.check_2",
            "Q24.w.attn.check_3", "Q24.w.attn.check_5")

# Helper
summarise_vars <- function(data, cols) {
  data %>%
    summarise(across(all_of(cols),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x,   na.rm = TRUE))))
}

# 1. Overall
overall <- df %>%
  summarise_vars(q_cols) %>%
  mutate(Group = "Overall", Parent = NA)

# 2. By region (top-level groups)
by_region <- df %>%
  group_by(target_state) %>%
  summarise_vars(q_cols) %>%
  mutate(Group = as.character(target_state), Parent = NA) %>%
  select(-target_state)

# 3. By subgroup WITHIN each region
by_subgroup <- df %>%
  group_by(target_state, race_b) %>%
  summarise_vars(q_cols) %>%
  mutate(Group    = as.character(race_b),
         Parent   = as.character(target_state)) %>%
  select(-target_state, -race_b)

# 4. Combine in display order: Overall → Region1 → its subs → Region2 → its subs
summary_table <- bind_rows(overall, by_region, by_subgroup) %>%
  arrange(Parent, Group)   # adjust ordering as needed

# 5. Reshape to long
summary_long <- summary_table %>%
  pivot_longer(cols = -c(Group, Parent),
               names_to  = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)") %>%
  mutate(Mean_SD = sprintf("%.2f (%.2f)", mean, sd))


write.csv(summary_long,"PCE_grouped.csv")










# --- 3. Tables: Cultural Connection  ----------------------------

culturalconnection <- df %>%
  group_by(target_state,race_b,Q58) %>%
  summarise(
    n      = n())

write.csv(culturalconnection,"culturalconnection.csv")






# --- 4. Tables: Black Owned Farms  ----------------------------

bl_farms <- df %>%
  group_by(target_state,race_b,Q59) %>%
  summarise(
    n      = n())

write.csv(bl_farms,"bl_farms.csv")

# --- 5. Tables: black owned businesses  ----------------------------

df$Q_83 <- if_else(df$Q83 == "5", 1,0)
df$Q_83 <- if_else(df$Q83 == "4", 2,df$Q_83)
df$Q_83 <- if_else(df$Q83 == "3", 3,df$Q_83)
df$Q_83 <- if_else(df$Q83 == "2", 4,df$Q_83)
df$Q_83 <- if_else(df$Q83 == "1", 5,df$Q_83)

# Define your question columns
q_cols <- c("Q_83")

# Helper
summarise_vars <- function(data, cols) {
  data %>%
    summarise(across(all_of(cols),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x,   na.rm = TRUE))))
}

# 1. Overall
overall <- df %>%
  summarise_vars(q_cols) %>%
  mutate(Group = "Overall", Parent = NA)

# 2. By region (top-level groups)
by_region <- df %>%
  group_by(target_state) %>%
  summarise_vars(q_cols) %>%
  mutate(Group = as.character(target_state), Parent = NA) %>%
  select(-target_state)

# 3. By subgroup WITHIN each region
by_subgroup <- df %>%
  group_by(target_state, race_b) %>%
  summarise_vars(q_cols) %>%
  mutate(Group    = as.character(race_b),
         Parent   = as.character(target_state)) %>%
  select(-target_state, -race_b)

# 4. Combine in display order: Overall → Region1 → its subs → Region2 → its subs
summary_table <- bind_rows(overall, by_region, by_subgroup) %>%
  arrange(Parent, Group)   # adjust ordering as needed

# 5. Reshape to long
summary_long <- summary_table %>%
  pivot_longer(cols = -c(Group, Parent),
               names_to  = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)") %>%
  mutate(Mean_SD = sprintf("%.2f (%.2f)", mean, sd))

write.csv(summary_long,"bl_biz.csv")




