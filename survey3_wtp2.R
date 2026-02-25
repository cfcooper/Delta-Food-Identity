# =============================================================================
# Willingness to Pay (WTP) Analysis - Delta vs Non-Delta Products
# Double-Bounded Dichotomous Choice (DBDC) Logit/Probit Model
# Products: Corn, Tomatoes, Sweet Potatoes, Watermelon, Rice
# Treatments: Black, Delta
# =============================================================================

# --- 1. Load Libraries -------------------------------------------------------

library(tidyverse)
library(survival)    # for interval regression (backup)
library(knitr)       # for summary tables
library(kableExtra)  # for formatted tables

# If not installed, run:
# install.packages(c("tidyverse", "survival", "knitr", "kableExtra"))


rm(list=ls()) # Caution: this clears the Environment

# --- 2. Load Data ------------------------------------------------------------

# Replace with your actual file path
df_bl <- readRDS("cleaneddata/df_black.rds")
df_de <- readRDS("cleaneddata/df_delta.rds")
df <- readRDS("cleaneddata/df_full.rds")


# Confirm structure
glimpse(df)


# --- 3. Define Products & Reshape to Long Format ----------------------------

products <- c("corn", "mater", "tater", "melon", "rice")

# Build long-format dataset with one row per respondent-product-question
long_df <- map_dfr(products, function(prod) {
  
  # Column names for this product
  c1_col  <- paste0(prod, "c1")
  c2_col  <- paste0(prod, "c2")
  p1_col  <- paste0(prod, "p1")
  p2_col  <- paste0(prod, "p2")
  
  df %>%
    select(
      respondent_id  = 1,              # assumes first column is ID; adjust if needed
      info_treatment,
      target_state,
      black,
      choice1  = all_of(c1_col),
      choice2  = all_of(c2_col),
      price1   = all_of(p1_col),
      price2   = all_of(p2_col)
    ) %>%
    mutate(product = prod)
})


# --- 4. Derive DBDC Bid Bounds -----------------------------------------------
# In DBDC, WTP is bounded by the two bids depending on the response pattern:
#   (1, 1) → chose Delta both times → WTP >= price2  (both yes)
#   (1, 0) → chose Delta Q1, not Q2 → price1 <= WTP < price2  (yes-no)
#   (0, 1) → didn't Q1, chose Q2    → price2 <= WTP < price1  (no-yes)
#   (0, 0) → didn't choose Delta either time → WTP < price2  (both no)

long_df <- long_df %>%
  mutate(
    response_pattern = case_when(
      choice1 == 1 & choice2 == 1 ~ "YY",
      choice1 == 1 & choice2 == 0 ~ "YN",
      choice1 == 0 & choice2 == 1 ~ "NY",
      choice1 == 0 & choice2 == 0 ~ "NN"
    ),
    # Lower and upper WTP bounds for interval regression
    wtp_lower = case_when(
      response_pattern == "YY" ~ price2,
      response_pattern == "YN" ~ price1,
      response_pattern == "NY" ~ price2,
      response_pattern == "NN" ~ 0
    ),
    wtp_upper = case_when(
      response_pattern == "YY" ~ Inf,
      response_pattern == "YN" ~ price2,
      response_pattern == "NY" ~ price1,
      response_pattern == "NN" ~ price2
    )
  )


# --- 5. Core WTP Helper Function --------------------------------------------
# Fits a DBDC logit model on a subset of data for one product.
# Returns mean WTP and delta-method SE.

fit_wtp <- function(data) {
  q1 <- data %>% transmute(choice = choice1, bid = price1)
  q2 <- data %>% transmute(choice = choice2, bid = price2)
  stacked <- bind_rows(q1, q2)
  
  model <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
  
  coefs    <- coef(model)
  a        <- coefs["(Intercept)"]
  b        <- coefs["bid"]
  vcov_mat <- vcov(model)
  
  mean_wtp <- -a / b
  se_wtp   <- sqrt(
    (vcov_mat["(Intercept)", "(Intercept)"] / b^2) +
      (a^2 * vcov_mat["bid", "bid"] / b^4) -
      (2 * a * vcov_mat["(Intercept)", "bid"] / b^3)
  )
  
  tibble(
    mean_wtp = round(mean_wtp, 3),
    se_wtp   = round(se_wtp, 3),
    ci_lower = round(mean_wtp - 1.96 * se_wtp, 3),
    ci_upper = round(mean_wtp + 1.96 * se_wtp, 3)
  )
}

# Generic grouped WTP runner: takes data + grouping vars, returns WTP per group x product
run_wtp_by_groups <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(c("product", group_vars)))) %>%
    group_modify(~ fit_wtp(.x)) %>%
    ungroup()
}



# --- 6. WTP Estimates: All Groupings ----------------------------------------

# Overall by product (no subgroups)
wtp_overall <- run_wtp_by_groups(long_df, character(0))

# By info treatment
wtp_by_treatment <- run_wtp_by_groups(long_df, "info_treatment")

# By target_state (Delta region vs. non-Delta region)
wtp_by_region <- run_wtp_by_groups(long_df, "target_state")

# By race (Black vs. non-Black)
wtp_by_race <- run_wtp_by_groups(long_df, "black")

# By all four combinations of target_state x black (ignoring info treatment)
wtp_by_region_race <- run_wtp_by_groups(long_df, c("target_state", "black"))


# --- 7. Per-Respondent Interval Midpoint WTP --------------------------------

respondent_wtp <- long_df %>%
  mutate(
    wtp_estimate = case_when(
      response_pattern == "YY" ~ wtp_lower,
      response_pattern == "YN" ~ (wtp_lower + wtp_upper) / 2,
      response_pattern == "NY" ~ (wtp_lower + wtp_upper) / 2,
      response_pattern == "NN" ~ 0
    )
  ) %>%
  select(respondent_id, info_treatment, target_state, black,
         product, response_pattern, price1, price2, wtp_estimate)


# --- 8. z-Test Helper: Pairwise WTP Difference ------------------------------

z_test_wtp <- function(wtp_df, group_col) {
  groups <- unique(wtp_df[[group_col]])
  if (length(groups) != 2) {
    warning("z-test requires exactly 2 groups; skipping.")
    return(NULL)
  }
  g1 <- groups[1]; g2 <- groups[2]
  
  wtp_df %>%
    select(product, all_of(group_col), mean_wtp, se_wtp) %>%
    pivot_wider(names_from = all_of(group_col),
                values_from = c(mean_wtp, se_wtp),
                names_sep = "_") %>%
    mutate(
      wtp_diff = .data[[paste0("mean_wtp_", g1)]] - .data[[paste0("mean_wtp_", g2)]],
      se_diff  = sqrt(.data[[paste0("se_wtp_", g1)]]^2 + .data[[paste0("se_wtp_", g2)]]^2),
      z_stat   = round(wtp_diff / se_diff, 3),
      p_value  = round(2 * (1 - pnorm(abs(z_stat))), 4),
      sig      = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE           ~ ""
      )
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}


# --- 9. Summary Tables -------------------------------------------------------

fmt <- function(df) kable(df, format = "simple")

# ---- Table 1: Overall Mean WTP per Product ----------------------------------
table1 <- wtp_overall %>%
  mutate(Product = str_to_title(product)) %>%
  select(Product, Mean_WTP = mean_wtp, SE = se_wtp,
         CI_Lower = ci_lower, CI_Upper = ci_upper)

cat("\n========================================\n")
cat("Table 1: Overall Mean WTP per Product\n")
cat("========================================\n")
print(fmt(table1))


# ---- Table 2: WTP by Product x Info Treatment + Difference Test -------------
table2 <- wtp_by_treatment %>%
  mutate(Product = str_to_title(product), Treatment = info_treatment) %>%
  select(Product, Treatment, Mean_WTP = mean_wtp, SE = se_wtp,
         CI_Lower = ci_lower, CI_Upper = ci_upper)

table2_diff <- z_test_wtp(wtp_by_treatment, "info_treatment") %>%
  mutate(Product = str_to_title(product)) %>%
  select(Product, everything(), -product)

cat("\n=================================================\n")
cat("Table 2: Mean WTP by Product and Info Treatment\n")
cat("=================================================\n")
print(fmt(table2))
cat("\n--- Table 2b: Treatment Group Difference Test ---\n")
print(fmt(table2_diff))
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 3: Per-Respondent WTP Summary ------------------------------------
table3_respondent <- respondent_wtp %>%
  group_by(respondent_id, info_treatment, target_state, black) %>%
  summarise(mean_wtp = round(mean(wtp_estimate, na.rm = TRUE), 3), .groups = "drop") %>%
  arrange(respondent_id)

table3_wide <- respondent_wtp %>%
  select(respondent_id, info_treatment, target_state, black, product, wtp_estimate) %>%
  pivot_wider(names_from = product, values_from = wtp_estimate, names_prefix = "wtp_") %>%
  arrange(respondent_id)

cat("\n=============================================\n")
cat("Table 3: Per-Respondent Mean WTP (all products)\n")
cat("=============================================\n")
print(fmt(head(table3_respondent, 20)))
cat("  ... (showing first 20 rows)\n")


# ---- Table 4: WTP by Race x Region Combinations (all 4 cells) ---------------
# Groups: Black x Delta region / Black x non-Delta / non-Black x Delta / non-Black x non-Delta

table4 <- wtp_by_region_race %>%
  mutate(
    Product = str_to_title(product),
    Region  = ifelse(target_state == 1, "Delta Region", "Non-Delta Region"),
    Race    = ifelse(black == 1, "Black", "Non-Black"),
    Group   = paste(Race, "|", Region)
  ) %>%
  select(Product, Group, Mean_WTP = mean_wtp, SE = se_wtp,
         CI_Lower = ci_lower, CI_Upper = ci_upper) %>%
  arrange(Product, Group)

cat("\n=================================================================\n")
cat("Table 4: Mean WTP by Race x Region (4 combinations, per product)\n")
cat("=================================================================\n")
print(fmt(table4))


# ---- Table 5: WTP by Region (Delta vs. non-Delta), pooled + per product -----
# Pooled row: fit WTP model across all products combined for each region

fit_wtp_pooled <- function(data) {
  q1 <- data %>% transmute(choice = choice1, bid = price1)
  q2 <- data %>% transmute(choice = choice2, bid = price2)
  stacked <- bind_rows(q1, q2)
  model <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
  coefs <- coef(model)
  a <- coefs["(Intercept)"]; b <- coefs["bid"]
  vcov_mat <- vcov(model)
  se_wtp <- sqrt(
    (vcov_mat["(Intercept)", "(Intercept)"] / b^2) +
      (a^2 * vcov_mat["bid", "bid"] / b^4) -
      (2 * a * vcov_mat["(Intercept)", "bid"] / b^3)
  )
  tibble(mean_wtp = round(-a/b, 3), se_wtp = round(se_wtp, 3),
         ci_lower = round(-a/b - 1.96*se_wtp, 3),
         ci_upper = round(-a/b + 1.96*se_wtp, 3))
}

wtp_region_pooled <- long_df %>%
  group_by(target_state) %>%
  group_modify(~ fit_wtp_pooled(.x)) %>%
  ungroup() %>%
  mutate(product = "Pooled (All Products)")

table5 <- bind_rows(wtp_region_pooled, wtp_by_region) %>%
  mutate(
    Product = ifelse(product == "Pooled (All Products)", "Pooled (All Products)", str_to_title(product)),
    Region  = ifelse(target_state == 1, "Delta Region", "Non-Delta Region")
  ) %>%
  select(Product, Region, Mean_WTP = mean_wtp, SE = se_wtp,
         CI_Lower = ci_lower, CI_Upper = ci_upper) %>%
  arrange(Product != "Pooled (All Products)", Product, Region)

table5_diff <- z_test_wtp(
  bind_rows(wtp_region_pooled, wtp_by_region) %>% rename(product_label = product) %>%
    mutate(product = ifelse(product_label == "Pooled (All Products)", "Pooled", str_to_title(product_label))),
  "target_state"
) %>%
  rename(
    `Mean WTP (1=Delta)` = mean_wtp_1,
    `Mean WTP (0=Non-Delta)` = mean_wtp_0,
    `SE (Delta)` = se_wtp_1,
    `SE (Non-Delta)` = se_wtp_0
  )

cat("\n==========================================================\n")
cat("Table 5: Mean WTP by Region (Delta vs. Non-Delta), Pooled + Per Product\n")
cat("==========================================================\n")
print(fmt(table5))
cat("\n--- Table 5b: Region Difference Test ---\n")
print(fmt(table5_diff))
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 10. Export Results ------------------------------------------------------

write.csv(table1,             "wtp_table1_overall.csv",               row.names = FALSE)
write.csv(table2,             "wtp_table2_by_treatment.csv",          row.names = FALSE)
write.csv(table2_diff,        "wtp_table2b_treatment_diff_test.csv",  row.names = FALSE)
write.csv(table3_respondent,  "wtp_table3_per_respondent.csv",        row.names = FALSE)
write.csv(table3_wide,        "wtp_table3_per_respondent_wide.csv",   row.names = FALSE)
write.csv(table4,             "wtp_table4_race_x_region.csv",         row.names = FALSE)
write.csv(table5,             "wtp_table5_by_region.csv",             row.names = FALSE)
write.csv(table5_diff,        "wtp_table5b_region_diff_test.csv",     row.names = FALSE)

cat("\n✓ All CSV files exported.\n")
cat("  - wtp_table1_overall.csv\n")
cat("  - wtp_table2_by_treatment.csv\n")
cat("  - wtp_table2b_treatment_diff_test.csv\n")
cat("  - wtp_table3_per_respondent.csv\n")
cat("  - wtp_table3_per_respondent_wide.csv\n")
cat("  - wtp_table4_race_x_region.csv\n")
cat("  - wtp_table5_by_region.csv\n")
cat("  - wtp_table5b_region_diff_test.csv\n")

