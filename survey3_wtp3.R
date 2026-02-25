# =============================================================================
# Willingness to Pay (WTP) Analysis - Delta vs Non-Delta Products
# Double-Bounded Dichotomous Choice (DBDC) Logit/Probit Model
# Products: Corn, Tomatoes, Sweet Potatoes, Watermelon, Rice
# Treatments: Black info treatment, Delta info treatment
# Grouping variables: target_state (Delta region), black (race)
# Outputs: Mean WTP + % premium over base price ((WTP - P1) / P1 * 100)
# Pooled % premium = simple average of the five per-product % premiums
# =============================================================================

# --- 1. Load Libraries -------------------------------------------------------

library(tidyverse)
library(knitr)
library(kableExtra)  # for formatted tables

# install.packages(c("tidyverse", "knitr"))


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

long_df <- map_dfr(products, function(prod) {
  df %>%
    select(
      respondent_id  = 1,        # adjust if your ID column is not first
      info_treatment,
      target_state,              # 1 = Delta region, 0 = non-Delta region
      black,                     # 1 = Black respondent, 0 = non-Black
      choice1 = all_of(paste0(prod, "c1")),
      choice2 = all_of(paste0(prod, "c2")),
      price1  = all_of(paste0(prod, "p1")),
      price2  = all_of(paste0(prod, "p2"))
    ) %>%
    mutate(product = prod)
})


# --- 4. Response Patterns & WTP Bounds --------------------------------------

long_df <- long_df %>%
  mutate(
    response_pattern = case_when(
      choice1 == 1 & choice2 == 1 ~ "YY",
      choice1 == 1 & choice2 == 0 ~ "YN",
      choice1 == 0 & choice2 == 1 ~ "NY",
      choice1 == 0 & choice2 == 0 ~ "NN"
    ),
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


# --- 5. Base Prices (P1) per Product ----------------------------------------
# P1 is constant across respondents; pull once per product for premium calc.

base_prices <- long_df %>%
  group_by(product) %>%
  summarise(base_price = mean(price1, na.rm = TRUE), .groups = "drop")


# --- 6. Core Estimation Helpers ---------------------------------------------

# Fit DBDC logit for a data slice; return mean WTP + delta-method SE
fit_wtp <- function(data) {
  stacked <- bind_rows(
    data %>% transmute(choice = choice1, bid = price1),
    data %>% transmute(choice = choice2, bid = price2)
  )
  model    <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
  coefs    <- coef(model)
  a        <- coefs["(Intercept)"]
  b        <- coefs["bid"]
  v        <- vcov(model)
  mean_wtp <- -a / b
  se_wtp   <- sqrt(v["(Intercept)","(Intercept)"] / b^2 +
                     a^2 * v["bid","bid"] / b^4 -
                     2 * a * v["(Intercept)","bid"] / b^3)
  tibble(
    mean_wtp = round(mean_wtp, 3),
    se_wtp   = round(se_wtp,   3),
    ci_lower = round(mean_wtp - 1.96 * se_wtp, 3),
    ci_upper = round(mean_wtp + 1.96 * se_wtp, 3)
  )
}

# Run fit_wtp() across all products x grouping variables
run_wtp_by_groups <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(c("product", group_vars)))) %>%
    group_modify(~ fit_wtp(.x)) %>%
    ungroup()
}

# Attach % premium = (WTP - P1) / P1 * 100
add_premium <- function(wtp_df) {
  wtp_df %>%
    left_join(base_prices, by = "product") %>%
    mutate(pct_premium = round((mean_wtp - base_price) / base_price * 100, 2)) %>%
    select(-base_price)
}

# z-test for WTP difference between exactly 2 levels of group_col
z_test_wtp <- function(wtp_df, group_col) {
  groups <- sort(unique(wtp_df[[group_col]]))
  if (length(groups) != 2) { warning("z-test requires exactly 2 groups."); return(NULL) }
  g1 <- groups[1]; g2 <- groups[2]
  wtp_df %>%
    select(product, all_of(group_col), mean_wtp, se_wtp, pct_premium) %>%
    pivot_wider(names_from  = all_of(group_col),
                values_from = c(mean_wtp, se_wtp, pct_premium),
                names_sep   = "_") %>%
    mutate(
      wtp_diff = .data[[paste0("mean_wtp_", g1)]] - .data[[paste0("mean_wtp_", g2)]],
      se_diff  = sqrt(.data[[paste0("se_wtp_", g1)]]^2 + .data[[paste0("se_wtp_", g2)]]^2),
      z_stat   = round(wtp_diff / se_diff, 3),
      p_value  = round(2 * (1 - pnorm(abs(z_stat))), 4),
      sig      = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                           p_value < 0.10 ~ "*",   TRUE ~ "")
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}


# --- 7. WTP + Premium: All Groupings ----------------------------------------

wtp_overall        <- run_wtp_by_groups(long_df, character(0))    %>% add_premium()
wtp_by_treatment   <- run_wtp_by_groups(long_df, "info_treatment") %>% add_premium()
wtp_by_region      <- run_wtp_by_groups(long_df, "target_state")   %>% add_premium()
wtp_by_race        <- run_wtp_by_groups(long_df, "black")          %>% add_premium()
wtp_by_region_race <- run_wtp_by_groups(long_df, c("target_state", "black")) %>% add_premium()


# --- 8. Per-Respondent WTP (interval midpoint) ------------------------------

respondent_wtp <- long_df %>%
  mutate(
    wtp_estimate = case_when(
      response_pattern == "YY" ~ wtp_lower,
      response_pattern == "YN" ~ (wtp_lower + wtp_upper) / 2,
      response_pattern == "NY" ~ (wtp_lower + wtp_upper) / 2,
      response_pattern == "NN" ~ 0
    )
  ) %>%
  left_join(base_prices, by = "product") %>%
  mutate(pct_premium = round((wtp_estimate - base_price) / base_price * 100, 2)) %>%
  select(respondent_id, info_treatment, target_state, black,
         product, response_pattern, price1, price2,
         wtp_estimate, pct_premium)


# --- 9. Summary Tables -------------------------------------------------------

fmt <- function(df, ...) kable(df, format = "simple", ...)

# ---- Table 1: Overall Mean WTP + Premium per Product -----------------------

table1 <- wtp_overall %>%
  mutate(Product = str_to_title(product)) %>%
  select(Product,
         `Mean WTP ($)` = mean_wtp, `SE` = se_wtp,
         `95% CI Lower` = ci_lower, `95% CI Upper` = ci_upper,
         `% Premium`    = pct_premium)

cat("\n==========================================\n")
cat("Table 1: Overall Mean WTP + Premium per Product\n")
cat("==========================================\n")
print(fmt(table1))


# ---- Table 2: WTP + Premium by Product x Info Treatment --------------------

table2 <- wtp_by_treatment %>%
  mutate(Product   = str_to_title(product),
         Treatment = info_treatment) %>%
  select(Product, Treatment,
         `Mean WTP ($)` = mean_wtp, `SE` = se_wtp,
         `95% CI Lower` = ci_lower, `95% CI Upper` = ci_upper,
         `% Premium`    = pct_premium)

table2_diff <- z_test_wtp(wtp_by_treatment, "info_treatment") %>%
  mutate(Product = str_to_title(product)) %>%
  select(-product)

cat("\n=====================================================\n")
cat("Table 2: Mean WTP + Premium by Product x Info Treatment\n")
cat("=====================================================\n")
print(fmt(table2))
cat("\n--- Table 2b: Info Treatment Difference Test ---\n")
print(fmt(table2_diff))
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 3: Per-Respondent WTP Summary -----------------------------------

table3_summary <- respondent_wtp %>%
  group_by(respondent_id, info_treatment, target_state, black) %>%
  summarise(
    mean_wtp    = round(mean(wtp_estimate,  na.rm = TRUE), 3),
    pct_premium = round(mean(pct_premium,   na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(respondent_id)

table3_wide <- respondent_wtp %>%
  select(respondent_id, info_treatment, target_state, black,
         product, wtp_estimate, pct_premium) %>%
  pivot_wider(names_from  = product,
              values_from = c(wtp_estimate, pct_premium),
              names_sep   = "_") %>%
  arrange(respondent_id)

cat("\n=================================================\n")
cat("Table 3: Per-Respondent Mean WTP + Premium (first 20 rows)\n")
cat("=================================================\n")
print(fmt(head(table3_summary, 20)))
cat("  ... (showing first 20 rows; full data exported to CSV)\n")


# ---- Table 4: WTP + Premium by Race x Region (4 combinations) --------------

table4 <- wtp_by_region_race %>%
  mutate(
    Product = str_to_title(product),
    Region  = ifelse(target_state == 1, "Delta Region", "Non-Delta Region"),
    Race    = ifelse(black == 1, "Black", "Non-Black"),
    Group   = paste(Race, "|", Region)
  ) %>%
  select(Product, Group,
         `Mean WTP ($)` = mean_wtp, `SE` = se_wtp,
         `95% CI Lower` = ci_lower, `95% CI Upper` = ci_upper,
         `% Premium`    = pct_premium) %>%
  arrange(Product, Group)

cat("\n======================================================================\n")
cat("Table 4: Mean WTP + Premium by Race x Region (4 combinations, per product)\n")
cat("======================================================================\n")
print(fmt(table4))


# ---- Table 5: WTP + Premium by Region, Pooled + Per Product ----------------
# Pooled row: % premium = simple average of the five per-product % premiums

# Per-product region estimates (already computed)
region_per_product <- wtp_by_region %>%
  mutate(level = str_to_title(product))

# Pooled row: average the per-product % premiums within each region group
region_pooled <- wtp_by_region %>%
  group_by(target_state) %>%
  summarise(
    mean_wtp    = round(mean(mean_wtp,    na.rm = TRUE), 3),  # informational only
    se_wtp      = round(mean(se_wtp,      na.rm = TRUE), 3),  # informational only
    ci_lower    = round(mean(ci_lower,    na.rm = TRUE), 3),
    ci_upper    = round(mean(ci_upper,    na.rm = TRUE), 3),
    pct_premium = round(mean(pct_premium, na.rm = TRUE), 2),  # avg of per-product premiums
    .groups = "drop"
  ) %>%
  mutate(product = "pooled", level = "Pooled (All Products)")

table5_data <- bind_rows(
  region_pooled,
  region_per_product
) %>%
  mutate(
    Region = ifelse(target_state == 1, "Delta Region", "Non-Delta Region")
  )

table5 <- table5_data %>%
  select(Product = level, Region,
         `Mean WTP ($)` = mean_wtp, `SE` = se_wtp,
         `95% CI Lower` = ci_lower, `95% CI Upper` = ci_upper,
         `% Premium`    = pct_premium) %>%
  arrange(Product == "Pooled (All Products)" %>% desc(), Product, Region)

# Difference test for region (per product only; pooled row uses averaged premiums)
table5_diff <- z_test_wtp(wtp_by_region, "target_state") %>%
  mutate(Product = str_to_title(product)) %>%
  select(-product)

# Pooled difference row for Table 5b: average per-product premiums per region,
# then compute the difference
pooled_diff <- wtp_by_region %>%
  group_by(target_state) %>%
  summarise(pct_premium = round(mean(pct_premium, na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = target_state, values_from = pct_premium,
              names_prefix = "pct_premium_") %>%
  mutate(
    product = "pooled",
    Product = "Pooled (All Products)",
    pct_premium_diff = round(pct_premium_1 - pct_premium_0, 2)
  )

cat("\n=================================================================\n")
cat("Table 5: Mean WTP + Premium by Region, Pooled + Per Product\n")
cat("=================================================================\n")
print(fmt(table5))
cat("\n--- Table 5b: Region Difference Test (per product) ---\n")
print(fmt(table5_diff))
cat("Note: Pooled % premium = simple average of the five per-product premiums.\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 10. Export Results ------------------------------------------------------

write.csv(table1,          "wtp_table1_overall.csv",               row.names = FALSE)
write.csv(table2,          "wtp_table2_by_treatment.csv",          row.names = FALSE)
write.csv(table2_diff,     "wtp_table2b_treatment_diff_test.csv",  row.names = FALSE)
write.csv(table3_summary,  "wtp_table3_per_respondent.csv",        row.names = FALSE)
write.csv(table3_wide,     "wtp_table3_per_respondent_wide.csv",   row.names = FALSE)
write.csv(table4,          "wtp_table4_race_x_region.csv",         row.names = FALSE)
write.csv(table5,          "wtp_table5_by_region.csv",             row.names = FALSE)
write.csv(table5_diff,     "wtp_table5b_region_diff_test.csv",     row.names = FALSE)

cat("\n✓ All CSV files exported:\n")
cat("  wtp_table1_overall.csv\n")
cat("  wtp_table2_by_treatment.csv\n")
cat("  wtp_table2b_treatment_diff_test.csv\n")
cat("  wtp_table3_per_respondent.csv\n")
cat("  wtp_table3_per_respondent_wide.csv\n")
cat("  wtp_table4_race_x_region.csv\n")
cat("  wtp_table5_by_region.csv\n")
cat("  wtp_table5b_region_diff_test.csv\n")