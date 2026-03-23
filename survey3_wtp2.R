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

# Fit DBDC logit; return mean WTP, delta-method SE, and LR test statistics.
# LR test: compares fitted model (choice ~ bid) to null (choice ~ 1).
# H0: bid coefficient = 0 (price has no effect on choice; model has no power).
# chi-sq = 2 * (logLik(full) - logLik(null)), df = 1, p from chi-sq distribution.

fit_wtp <- function(data) {
  stacked <- bind_rows(
    data %>% transmute(choice = choice1, bid = price1),
    data %>% transmute(choice = choice2, bid = price2)
  )
  
  model_full <- glm(choice ~ bid,  data = stacked, family = binomial(link = "logit"))
  model_null <- glm(choice ~ 1,    data = stacked, family = binomial(link = "logit"))
  
  # WTP and delta-method SE
  coefs    <- coef(model_full)
  a        <- coefs["(Intercept)"]
  b        <- coefs["bid"]
  v        <- vcov(model_full)
  mean_wtp <- -a / b
  se_wtp   <- sqrt(v["(Intercept)","(Intercept)"] / b^2 +
                     a^2 * v["bid","bid"] / b^4 -
                     2 * a * v["(Intercept)","bid"] / b^3)
  
  # LR test
  lr_chi2  <- round(2 * (as.numeric(logLik(model_full)) - as.numeric(logLik(model_null))), 3)
  lr_df    <- 1   # one restriction: bid coefficient = 0
  lr_p     <- round(pchisq(lr_chi2, df = lr_df, lower.tail = FALSE), 4)
  lr_sig   <- case_when(lr_p < 0.01 ~ "***", lr_p < 0.05 ~ "**",
                        lr_p < 0.10 ~ "*",   TRUE ~ "")
  
  tibble(
    mean_wtp = round(mean_wtp, 3),
    se_wtp   = round(se_wtp,   3),
    ci_lower = round(mean_wtp - 1.96 * se_wtp, 3),
    ci_upper = round(mean_wtp + 1.96 * se_wtp, 3),
    lr_chi2  = lr_chi2,
    lr_df    = lr_df,
    lr_p     = lr_p,
    lr_sig   = lr_sig
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

wtp_overall        <- run_wtp_by_groups(long_df, character(0))              %>% add_premium()
wtp_by_treatment   <- run_wtp_by_groups(long_df, "info_treatment")          %>% add_premium()
wtp_by_region      <- run_wtp_by_groups(long_df, "target_state")            %>% add_premium()
wtp_by_race        <- run_wtp_by_groups(long_df, "black")                   %>% add_premium()
wtp_by_region_race <- run_wtp_by_groups(long_df, c("target_state", "black")) %>% add_premium()


# --- 8. Per-Respondent WTP (interval midpoint) ------------------------------
# Note: LR test is a model-level statistic and is not applicable per-respondent.

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

# Shared column rename helper for display
rename_for_display <- function(df) {
  df %>% rename(
    `Mean WTP ($)`  = mean_wtp,
    `SE`            = se_wtp,
    `95% CI Lower`  = ci_lower,
    `95% CI Upper`  = ci_upper,
    `% Premium`     = pct_premium,
    `LR Chi-sq`     = lr_chi2,
    `LR df`         = lr_df,
    `LR p-value`    = lr_p,
    ` `             = lr_sig      # significance stars, no header clutter
  )
}


# ---- Table 1: Overall Mean WTP + Premium + LR Test per Product --------------

table1 <- wtp_overall %>%
  mutate(Product = str_to_title(product)) %>%
  select(Product, mean_wtp, se_wtp, ci_lower, ci_upper,
         pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display()

cat("\n=======================================================\n")
cat("Table 1: Overall Mean WTP, Premium, and LR Test per Product\n")
cat("=======================================================\n")
print(fmt(table1))
cat("LR test H0: bid price has no effect on Delta product choice (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 2: WTP + Premium + LR Test by Product x Info Treatment ----------

table2 <- wtp_by_treatment %>%
  mutate(Product = str_to_title(product), Treatment = info_treatment) %>%
  select(Product, Treatment, mean_wtp, se_wtp, ci_lower, ci_upper,
         pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display()

table2_diff <- z_test_wtp(wtp_by_treatment, "info_treatment") %>%
  mutate(Product = str_to_title(product)) %>%
  select(-product)

cat("\n=============================================================\n")
cat("Table 2: Mean WTP, Premium, and LR Test by Product x Info Treatment\n")
cat("=============================================================\n")
print(fmt(table2))
cat("LR test H0: bid price has no effect on Delta product choice (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")
cat("\n--- Table 2b: Info Treatment WTP Difference Test (z-test) ---\n")
print(fmt(table2_diff))
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 3: Per-Respondent WTP Summary -----------------------------------

table3_summary <- respondent_wtp %>%
  group_by(respondent_id, info_treatment, target_state, black) %>%
  summarise(
    mean_wtp    = round(mean(wtp_estimate, na.rm = TRUE), 3),
    pct_premium = round(mean(pct_premium,  na.rm = TRUE), 2),
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

cat("\n=======================================================\n")
cat("Table 3: Per-Respondent Mean WTP + Premium (first 20 rows)\n")
cat("(LR test is model-level; not applicable per respondent.)\n")
cat("=======================================================\n")
print(fmt(head(table3_summary, 20)))
cat("  ... (showing first 20 rows; full data exported to CSV)\n")


# ---- Table 4: WTP + Premium + LR Test by Race x Region (4 combinations) ----

table4 <- wtp_by_region_race %>%
  mutate(
    Product = str_to_title(product),
    Region  = ifelse(target_state == 1, "Delta Region", "Non-Delta Region"),
    Race    = ifelse(black == 1, "Black", "Non-Black"),
    Group   = paste(Race, "|", Region)
  ) %>%
  select(Product, Group, mean_wtp, se_wtp, ci_lower, ci_upper,
         pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display() %>%
  arrange(Product, Group)

cat("\n==========================================================================\n")
cat("Table 4: Mean WTP, Premium, and LR Test by Race x Region (4 combinations)\n")
cat("==========================================================================\n")
print(fmt(table4))
cat("LR test H0: bid price has no effect on Delta product choice (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 5: WTP + Premium + LR Test by Region, Pooled + Per Product -------
# Pooled row: % premium = simple average of the five per-product % premiums.
# LR chi-sq and p-value are also averaged across products for the pooled row
# and flagged as approximate summaries, not a single model test.

region_per_product <- wtp_by_region %>%
  mutate(level = str_to_title(product))

region_pooled <- wtp_by_region %>%
  group_by(target_state) %>%
  summarise(
    mean_wtp    = round(mean(mean_wtp,    na.rm = TRUE), 3),
    se_wtp      = round(mean(se_wtp,      na.rm = TRUE), 3),
    ci_lower    = round(mean(ci_lower,    na.rm = TRUE), 3),
    ci_upper    = round(mean(ci_upper,    na.rm = TRUE), 3),
    pct_premium = round(mean(pct_premium, na.rm = TRUE), 2),
    lr_chi2     = round(mean(lr_chi2,     na.rm = TRUE), 3),
    lr_df       = 1L,
    lr_p        = round(mean(lr_p,        na.rm = TRUE), 4),
    lr_sig      = "",    # averaged p-value; don't assign stars to avoid misreading
    .groups = "drop"
  ) %>%
  mutate(product = "pooled", level = "Pooled (Avg of Products)")

table5_data <- bind_rows(region_pooled, region_per_product) %>%
  mutate(Region = ifelse(target_state == 1, "Delta Region", "Non-Delta Region"))

table5 <- table5_data %>%
  select(Product = level, Region, mean_wtp, se_wtp, ci_lower, ci_upper,
         pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display() %>%
  arrange(`Product` == "Pooled (Avg of Products)" %>% desc(), `Product`, Region)

table5_diff <- z_test_wtp(wtp_by_region, "target_state") %>%
  mutate(Product = str_to_title(product)) %>%
  select(-product)

cat("\n=====================================================================\n")
cat("Table 5: Mean WTP, Premium, and LR Test by Region, Pooled + Per Product\n")
cat("=====================================================================\n")
print(fmt(table5))
cat("Pooled row: % premium and LR stats are averages across the five products.\n")
cat("Stars suppressed for pooled LR p-value (averaged across models).\n")
cat("LR test H0: bid price has no effect on Delta product choice (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")
cat("\n--- Table 5b: Region WTP Difference Test (z-test, per product) ---\n")
print(fmt(table5_diff))
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 10. Table 6: Nested LR Tests — Region and Race Model Comparisons --------
#
# For each product, tests whether allowing separate WTP functions per group
# significantly improves fit over a single pooled model.
#
# Restricted model:  choice ~ bid               (one intercept, one bid slope)
# Unrestricted model: choice ~ bid + group + bid:group  (separate intercept &
#                     slope per group; equivalent to fitting two separate models)
#
# LR stat = 2 * (logLik(unrestricted) - logLik(restricted)), chi-sq(2)
# df = 2 because two extra parameters are freed: the group intercept shift
#      and the group x bid slope shift.
# H0: the two groups share the same WTP function (intercept and bid slope).
# Rejecting H0 means the groups have statistically different WTP structures.

nested_lr_test <- function(data, group_col, product_name) {
  
  prod_data <- data %>% filter(product == product_name)
  
  stacked <- bind_rows(
    prod_data %>% transmute(choice = choice1, bid = price1,
                            group = .data[[group_col]]),
    prod_data %>% transmute(choice = choice2, bid = price2,
                            group = .data[[group_col]])
  ) %>%
    mutate(group = factor(group))
  
  model_restricted   <- glm(choice ~ bid,             data = stacked,
                            family = binomial(link = "logit"))
  model_unrestricted <- glm(choice ~ bid * group,     data = stacked,
                            family = binomial(link = "logit"))
  
  lr_chi2 <- round(2 * (as.numeric(logLik(model_unrestricted)) -
                          as.numeric(logLik(model_restricted))), 3)
  lr_df   <- 2   # group intercept + group:bid interaction
  lr_p    <- round(pchisq(lr_chi2, df = lr_df, lower.tail = FALSE), 4)
  lr_sig  <- case_when(lr_p < 0.01 ~ "***", lr_p < 0.05 ~ "**",
                       lr_p < 0.10 ~ "*",   TRUE ~ "")
  
  # Log-likelihoods for transparency
  ll_restricted   <- round(as.numeric(logLik(model_restricted)),   3)
  ll_unrestricted <- round(as.numeric(logLik(model_unrestricted)), 3)
  
  tibble(
    Product          = str_to_title(product_name),
    Comparison       = group_col,
    LL_Restricted    = ll_restricted,
    LL_Unrestricted  = ll_unrestricted,
    LR_Chi2          = lr_chi2,
    LR_df            = lr_df,
    LR_p             = lr_p,
    Sig              = lr_sig
  )
}

# Run for both grouping variables x all products
table6_region <- map_dfr(products,
                         ~ nested_lr_test(long_df, "target_state", .x)) %>%
  mutate(Comparison = "Delta Region vs. Non-Delta Region")

table6_race <- map_dfr(products,
                       ~ nested_lr_test(long_df, "black", .x)) %>%
  mutate(Comparison = "Black vs. Non-Black")

table6 <- bind_rows(table6_region, table6_race) %>%
  arrange(Comparison, Product) %>%
  select(Comparison, Product,
         `LL (Restricted)`   = LL_Restricted,
         `LL (Unrestricted)` = LL_Unrestricted,
         `LR Chi-sq`         = LR_Chi2,
         `df`                = LR_df,
         `p-value`           = LR_p,
         ` `                 = Sig)

cat("\n=======================================================================\n")
cat("Table 6: Nested LR Tests — Do Groups Have Different WTP Functions?\n")
cat("=======================================================================\n")
print(fmt(table6))
cat("Restricted model: choice ~ bid (pooled across group).\n")
cat("Unrestricted model: choice ~ bid * group (separate intercept & slope per group).\n")
cat("H0: Both groups share the same WTP function. df = 2.\n")
cat("Rejecting H0 indicates statistically different WTP structures between groups.\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 11. Export Results ------------------------------------------------------

write.csv(table1,         "wtp_table1_overall.csv",               row.names = FALSE)
write.csv(table2,         "wtp_table2_by_treatment.csv",          row.names = FALSE)
write.csv(table2_diff,    "wtp_table2b_treatment_diff_test.csv",  row.names = FALSE)
write.csv(table3_summary, "wtp_table3_per_respondent.csv",        row.names = FALSE)
write.csv(table3_wide,    "wtp_table3_per_respondent_wide.csv",   row.names = FALSE)
write.csv(table4,         "wtp_table4_race_x_region.csv",         row.names = FALSE)
write.csv(table5,         "wtp_table5_by_region.csv",             row.names = FALSE)
write.csv(table5_diff,    "wtp_table5b_region_diff_test.csv",     row.names = FALSE)
write.csv(table6,         "wtp_table6_nested_lr_tests.csv",       row.names = FALSE)

cat("\n✓ All CSV files exported:\n")
cat("  wtp_table1_overall.csv\n")
cat("  wtp_table2_by_treatment.csv\n")
cat("  wtp_table2b_treatment_diff_test.csv\n")
cat("  wtp_table3_per_respondent.csv\n")
cat("  wtp_table3_per_respondent_wide.csv\n")
cat("  wtp_table4_race_x_region.csv\n")
cat("  wtp_table5_by_region.csv\n")
cat("  wtp_table5b_region_diff_test.csv\n")
cat("  wtp_table6_nested_lr_tests.csv\n")