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
library(MASS)

# install.packages(c("tidyverse", "knitr", "MASS"))


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

# --- ID column setup ---------------------------------------------------------
# The script uses the FIRST column of your data as the respondent ID.
# If your ID column has a specific name (e.g. "ResponseId", "id"), replace
# `id_col` below with that name as a string.
id_col <- "ResponseId"   # auto-detects first column; override if needed
# e.g.: id_col <- "ResponseId"

long_df <- map_dfr(products, function(prod) {
  df %>%
    rename(respondent_id = all_of(id_col)) %>%
    dplyr::select(
      respondent_id,
      info_treatment,
      target_state,              # 1 = Delta region, 0 = non-Delta region
      race_b,                     # 1 = Black respondent, 0 = non-Black
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
    dplyr::select(-base_price)
}

# Poe, Giraud & Loomis (2005) complete combinatorial test
# Compares WTP distributions between exactly 2 levels of group_col.
# For each product, fits separate logit models per group, draws n_sim coefficient
# vectors from N(coef, vcov), computes simulated WTP = -alpha/beta for each draw,
# then tests H0: WTP_g1 = WTP_g2 via the proportion of pairwise differences <= 0.
# Two-sided p = 2 * min(Pr(diff <= 0), Pr(diff > 0)).
poe_comparison <- function(data, group_col, n_sim = 10000, seed = 42) {
  set.seed(seed)
  groups <- sort(unique(data[[group_col]]))
  if (length(groups) != 2) { warning("Poe test requires exactly 2 groups."); return(NULL) }
  g1 <- groups[1]; g2 <- groups[2]
  
  map_dfr(unique(data$product), function(prod) {
    prod_data <- data %>% filter(product == prod)
    
    fit_and_draw <- function(grp) {
      d <- prod_data %>% filter(.data[[group_col]] == grp)
      stacked <- bind_rows(
        d %>% transmute(choice = choice1, bid = price1),
        d %>% transmute(choice = choice2, bid = price2)
      )
      m     <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
      draws <- MASS::mvrnorm(n_sim, mu = coef(m), Sigma = vcov(m))
      wtp   <- -draws[, "(Intercept)"] / draws[, "bid"]
      list(wtp = wtp[is.finite(wtp)], mean_wtp = round(-coef(m)["(Intercept)"] / coef(m)["bid"], 3),
           pct_premium = NULL, model = m)
    }
    
    r1 <- fit_and_draw(g1)
    r2 <- fit_and_draw(g2)
    
    # % premiums
    bp <- base_prices %>% filter(product == prod) %>% pull(base_price)
    prem1 <- round((r1$mean_wtp - bp) / bp * 100, 2)
    prem2 <- round((r2$mean_wtp - bp) / bp * 100, 2)
    
    n_use      <- min(length(r1$wtp), length(r2$wtp))
    diffs      <- r1$wtp[1:n_use] - r2$wtp[1:n_use]
    p_one      <- mean(diffs <= 0)
    p_two      <- round(2 * min(p_one, 1 - p_one), 4)
    poe_sig    <- case_when(p_two < 0.01 ~ "***", p_two < 0.05 ~ "**",
                            p_two < 0.10 ~ "*",   TRUE ~ "")
    
    tibble(
      product                        = prod,
      !!paste0("wtp_",    g1)       := r1$mean_wtp,
      !!paste0("prem_",   g1)       := prem1,
      !!paste0("wtp_",    g2)       := r2$mean_wtp,
      !!paste0("prem_",   g2)       := prem2,
      wtp_diff                       = round(r1$mean_wtp - r2$mean_wtp, 3),
      poe_p                          = p_two,
      poe_sig                        = poe_sig
    )
  })
}


# --- 7. WTP + Premium: All Groupings ----------------------------------------

wtp_overall        <- run_wtp_by_groups(long_df, character(0))              %>% add_premium()
wtp_by_treatment   <- run_wtp_by_groups(long_df, "info_treatment")          %>% add_premium()
wtp_by_region      <- run_wtp_by_groups(long_df, "target_state")            %>% add_premium()
wtp_by_race        <- run_wtp_by_groups(long_df, "race_b")                   %>% add_premium()
wtp_by_region_race      <- run_wtp_by_groups(long_df, c("target_state", "race_b"))            %>% add_premium()
wtp_by_treatment_region <- run_wtp_by_groups(long_df, c("info_treatment", "target_state")) %>% add_premium()


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
  dplyr::select(respondent_id, info_treatment, target_state, race_b,
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
  dplyr::select(Product, mean_wtp, se_wtp, ci_lower, ci_upper,
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
  dplyr::select(Product, Treatment, mean_wtp, se_wtp, ci_lower, ci_upper,
                pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display()

cat("\nRunning Poe test for Table 2b (info treatment comparison)...\n")
trts    <- sort(unique(long_df$info_treatment))
trt1    <- trts[1]; trt2 <- trts[2]
table2b_poe <- poe_comparison(long_df, "info_treatment") %>%
  mutate(Product = str_to_title(product)) %>%
  dplyr::select(
    Product,
    !!paste0("WTP: ", trt1, " ($)")     := paste0("wtp_",  trt1),
    !!paste0("% Premium: ", trt1)       := paste0("prem_", trt1),
    !!paste0("WTP: ", trt2, " ($)")     := paste0("wtp_",  trt2),
    !!paste0("% Premium: ", trt2)       := paste0("prem_", trt2),
    `WTP Difference ($)`                 = wtp_diff,
    `Poe p-value (two-sided)`            = poe_p,
    ` `                                  = poe_sig
  )

cat("\n=============================================================\n")
cat("Table 2: Mean WTP, Premium, and LR Test by Product x Info Treatment\n")
cat("=============================================================\n")
print(fmt(table2))
cat("LR test H0: bid price has no effect on Delta product choice (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")
cat("\n--- Table 2b: Info Treatment WTP Difference (Poe Test) ---\n")
print(fmt(table2b_poe))
cat("Poe test (Poe, Giraud & Loomis, 2005): simulated WTP distributions from N(coef, vcov).\n")
cat("H0: WTP is equal across info treatment groups. Two-sided.\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 3: Per-Respondent WTP Summary -----------------------------------

table3_summary <- respondent_wtp %>%
  group_by(respondent_id, info_treatment, target_state, race_b) %>%
  summarise(
    mean_wtp    = round(mean(wtp_estimate, na.rm = TRUE), 3),
    pct_premium = round(mean(pct_premium,  na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(respondent_id)

table3_wide <- respondent_wtp %>%
  dplyr::select(respondent_id, info_treatment, target_state, race_b,
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
    Race    = ifelse(race_b == 1, "Black", "Non-Black"),
    Group   = paste(Race, "|", Region)
  ) %>%
  dplyr::select(Product, Group, mean_wtp, se_wtp, ci_lower, ci_upper,
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
  dplyr::select(Product = level, Region, mean_wtp, se_wtp, ci_lower, ci_upper,
                pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display() %>%
  arrange(`Product` == "Pooled (Avg of Products)" %>% desc(), `Product`, Region)

cat("\nRunning Poe test for Table 5b (region comparison)...\n")
table5b_poe <- poe_comparison(long_df, "target_state") %>%
  mutate(Product = str_to_title(product)) %>%
  dplyr::select(
    Product,
    `WTP: Delta Region ($)`     = wtp_1,
    `% Premium: Delta Region`   = prem_1,
    `WTP: Non-Delta Region ($)` = wtp_0,
    `% Premium: Non-Delta`      = prem_0,
    `WTP Difference ($)`         = wtp_diff,
    `Poe p-value (two-sided)`    = poe_p,
    ` `                          = poe_sig
  )

cat("\n=====================================================================\n")
cat("Table 5: Mean WTP, Premium, and LR Test by Region, Pooled + Per Product\n")
cat("=====================================================================\n")
print(fmt(table5))
cat("Pooled row: % premium and LR stats are averages across the five products.\n")
cat("Stars suppressed for pooled LR p-value (averaged across models).\n")
cat("LR test H0: bid price has no effect on Delta product choice (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")
cat("\n--- Table 5b: Region WTP Difference (Poe Test, per product) ---\n")
print(fmt(table5b_poe))
cat("Poe test (Poe, Giraud & Loomis, 2005): simulated WTP distributions from N(coef, vcov).\n")
cat("H0: WTP is equal across Delta and Non-Delta regions. Two-sided.\n")
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
                       ~ nested_lr_test(long_df, "race_b", .x)) %>%
  mutate(Comparison = "Black vs. Non-Black")

table6 <- bind_rows(table6_region, table6_race) %>%
  arrange(Comparison, Product) %>%
  dplyr::select(Comparison, Product,
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
write.csv(table2b_poe,    "wtp_table2b_treatment_poe_test.csv",   row.names = FALSE)
write.csv(table3_summary, "wtp_table3_per_respondent.csv",        row.names = FALSE)
write.csv(table3_wide,    "wtp_table3_per_respondent_wide.csv",   row.names = FALSE)
write.csv(table4,         "wtp_table4_race_x_region.csv",         row.names = FALSE)
write.csv(table5,         "wtp_table5_by_region.csv",             row.names = FALSE)
write.csv(table5b_poe,    "wtp_table5b_region_poe_test.csv",      row.names = FALSE)
write.csv(table6,         "wtp_table6_nested_lr_tests.csv",       row.names = FALSE)

cat("\n✓ All CSV files exported:\n")
cat("  wtp_table1_overall.csv\n")
cat("  wtp_table2_by_treatment.csv\n")
cat("  wtp_table2b_treatment_poe_test.csv\n")
cat("  wtp_table3_per_respondent.csv\n")
cat("  wtp_table3_per_respondent_wide.csv\n")
cat("  wtp_table4_race_x_region.csv\n")
cat("  wtp_table5_by_region.csv\n")
cat("  wtp_table5b_region_poe_test.csv\n")
cat("  wtp_table6_nested_lr_tests.csv\n")

# --- 12. Table 7: WTP by Info Treatment x Region, + Nested LR Test -----------
#
# For each product, estimates WTP separately for each of the four cells:
#   Black treatment   x Delta region
#   Black treatment   x Non-Delta region
#   Delta treatment   x Delta region
#   Delta treatment   x Non-Delta region
#
# Then within each info treatment, runs a nested LR test comparing the
# Delta-region model vs. the Non-Delta-region model:
#   Restricted:   choice ~ bid           (same WTP function for both regions)
#   Unrestricted: choice ~ bid * target_state  (separate intercept & slope)
#   LR stat ~ chi-sq(2), df = 2

# ---- Table 7a: WTP + Premium per Treatment x Region cell --------------------

table7a <- wtp_by_treatment_region %>%
  mutate(
    Product   = str_to_title(product),
    Treatment = info_treatment,
    Region    = ifelse(target_state == 1, "Delta Region", "Non-Delta Region")
  ) %>%
  dplyr::select(Product, Treatment, Region,
                mean_wtp, se_wtp, ci_lower, ci_upper,
                pct_premium, lr_chi2, lr_df, lr_p, lr_sig) %>%
  rename_for_display() %>%
  arrange(Product, Treatment, Region)

cat("\n===========================================================================\n")
cat("Table 7a: Mean WTP + Premium by Info Treatment x Region, per Product\n")
cat("===========================================================================\n")
print(fmt(table7a))
cat("LR test H0: bid price has no effect on Delta product choice within cell (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# ---- Table 7b: Nested LR Test — Region difference within each treatment -----
# For each product x info treatment, tests whether the two region submodels
# (Delta region vs. Non-Delta region) have statistically different WTP functions.

nested_lr_within_treatment <- function(data, treatment_val, product_name) {
  
  subset_data <- data %>%
    filter(product == product_name, info_treatment == treatment_val)
  
  stacked <- bind_rows(
    subset_data %>% transmute(choice = choice1, bid = price1,
                              group  = factor(target_state)),
    subset_data %>% transmute(choice = choice2, bid = price2,
                              group  = factor(target_state))
  )
  
  model_restricted   <- glm(choice ~ bid,         data = stacked,
                            family = binomial(link = "logit"))
  model_unrestricted <- glm(choice ~ bid * group, data = stacked,
                            family = binomial(link = "logit"))
  
  lr_chi2 <- round(2 * (as.numeric(logLik(model_unrestricted)) -
                          as.numeric(logLik(model_restricted))), 3)
  lr_df   <- 2
  lr_p    <- round(pchisq(lr_chi2, df = lr_df, lower.tail = FALSE), 4)
  lr_sig  <- case_when(lr_p < 0.01 ~ "***", lr_p < 0.05 ~ "**",
                       lr_p < 0.10 ~ "*",   TRUE ~ "")
  
  tibble(
    Product     = str_to_title(product_name),
    Treatment   = treatment_val,
    Comparison  = "Delta Region vs. Non-Delta Region",
    LL_Restr    = round(as.numeric(logLik(model_restricted)),   3),
    LL_Unrestr  = round(as.numeric(logLik(model_unrestricted)), 3),
    LR_Chi2     = lr_chi2,
    LR_df       = lr_df,
    LR_p        = lr_p,
    Sig         = lr_sig
  )
}

treatments <- unique(long_df$info_treatment)

table7b <- map_dfr(treatments, function(trt) {
  map_dfr(products, ~ nested_lr_within_treatment(long_df, trt, .x))
}) %>%
  arrange(Treatment, Product) %>%
  dplyr::select(Treatment, Product, Comparison,
                `LL (Restricted)`   = LL_Restr,
                `LL (Unrestricted)` = LL_Unrestr,
                `LR Chi-sq`         = LR_Chi2,
                `df`                = LR_df,
                `p-value`           = LR_p,
                ` `                 = Sig)

cat("\n===========================================================================\n")
cat("Table 7b: Nested LR Test — Region Differences Within Each Info Treatment\n")
cat("===========================================================================\n")
print(fmt(table7b))
cat("Restricted model:   choice ~ bid (region pooled within treatment).\n")
cat("Unrestricted model: choice ~ bid * region (separate intercept & slope per region).\n")
cat("H0: Delta-region and Non-Delta-region respondents share the same WTP function\n")
cat("    within each info treatment. df = 2.\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 13. Update Exports ------------------------------------------------------

write.csv(table7a, "wtp_table7a_treatment_x_region.csv",          row.names = FALSE)
write.csv(table7b, "wtp_table7b_nested_lr_within_treatment.csv",  row.names = FALSE)

cat("\n✓ Additional CSV files exported:\n")
cat("  wtp_table7a_treatment_x_region.csv\n")
cat("  wtp_table7b_nested_lr_within_treatment.csv\n")

# --- 14. Table 8: Poe Test — Region WTP Differences Within Each Treatment ----
#
# The Poe, Giraud & Loomis (2005) complete combinatorial test compares two
# WTP distributions by simulation rather than relying on the delta method.
#
# Procedure for each product x info treatment:
#   1. Draw N random coefficient vectors from each group's estimated
#      multivariate normal distribution: N(coef, vcov).
#   2. Compute a simulated WTP for each draw: WTP_sim = -alpha_sim / beta_sim.
#   3. Compute all pairwise differences: delta_WTP = WTP_sim_1 - WTP_sim_2.
#   4. p-value (one-sided) = proportion of differences <= 0.
#   5. Two-sided p-value = 2 * min(p_one_sided, 1 - p_one_sided).
#
# H0: WTP_DeltaRegion = WTP_NonDeltaRegion within each info treatment.
# This test makes no distributional assumption about the WTP ratio -alpha/beta.

poe_test <- function(data, treatment_val, product_name, n_sim = 10000, seed = 42) {
  
  set.seed(seed)
  
  # Filter to product x treatment, then split by region
  base <- data %>%
    filter(product == product_name, info_treatment == treatment_val)
  
  fit_group_model <- function(region_val) {
    d <- base %>% filter(target_state == region_val)
    stacked <- bind_rows(
      d %>% transmute(choice = choice1, bid = price1),
      d %>% transmute(choice = choice2, bid = price2)
    )
    glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
  }
  
  model_delta     <- fit_group_model(1)   # Delta region
  model_nondelta  <- fit_group_model(0)   # Non-Delta region
  
  # Simulate coefficient draws from multivariate normal
  draw_wtp <- function(model, n) {
    mu    <- coef(model)
    sigma <- vcov(model)
    draws <- MASS::mvrnorm(n, mu = mu, Sigma = sigma)
    # WTP = -intercept / bid; drop draws where bid coef is near zero (numerical safety)
    wtp   <- -draws[, "(Intercept)"] / draws[, "bid"]
    wtp[is.finite(wtp)]   # remove Inf/-Inf from near-zero bid draws
  }
  
  wtp_delta    <- draw_wtp(model_delta,    n_sim)
  wtp_nondelta <- draw_wtp(model_nondelta, n_sim)
  
  # Trim to equal length in case any draws were dropped
  n_use <- min(length(wtp_delta), length(wtp_nondelta))
  wtp_delta    <- wtp_delta[1:n_use]
  wtp_nondelta <- wtp_nondelta[1:n_use]
  
  # Complete combinatorial: proportion of pairwise differences <= 0
  # For large n_sim, use random sampling of pairs (exact combinatorial is n^2)
  diffs      <- wtp_delta - wtp_nondelta
  p_onesided <- mean(diffs <= 0)
  p_twosided <- round(2 * min(p_onesided, 1 - p_onesided), 4)
  poe_sig    <- case_when(p_twosided < 0.01 ~ "***", p_twosided < 0.05 ~ "**",
                          p_twosided < 0.10 ~ "*",   TRUE ~ "")
  
  # Point WTP estimates for reference
  coefs_d  <- coef(model_delta);    wtp_d  <- round(-coefs_d["(Intercept)"]  / coefs_d["bid"],  3)
  coefs_nd <- coef(model_nondelta); wtp_nd <- round(-coefs_nd["(Intercept)"] / coefs_nd["bid"], 3)
  
  tibble(
    Product          = str_to_title(product_name),
    Treatment        = treatment_val,
    WTP_Delta_Region = wtp_d,
    WTP_NonDelta     = wtp_nd,
    WTP_Diff         = round(wtp_d - wtp_nd, 3),
    N_Sim            = n_use,
    p_twosided       = p_twosided,
    Sig              = poe_sig
  )
}

cat("\nRunning Poe tests (10,000 simulations per cell)... this may take a moment.\n")

table8 <- map_dfr(treatments, function(trt) {
  map_dfr(products, ~ poe_test(long_df, trt, .x, n_sim = 10000))
}) %>%
  arrange(Treatment, Product) %>%
  dplyr::select(
    Treatment,
    Product,
    `WTP: Delta Region ($)`     = WTP_Delta_Region,
    `WTP: Non-Delta Region ($)` = WTP_NonDelta,
    `WTP Difference ($)`        = WTP_Diff,
    `N Simulations`             = N_Sim,
    `Poe p-value (two-sided)`   = p_twosided,
    ` `                         = Sig
  )

cat("\n===========================================================================\n")
cat("Table 8: Poe Test — Region WTP Differences Within Each Info Treatment\n")
cat("===========================================================================\n")
print(fmt(table8))
cat("Poe test (Poe, Giraud & Loomis, 2005): complete combinatorial comparison\n")
cat("of simulated WTP distributions drawn from N(coef, vcov) of each logit model.\n")
cat("H0: WTP(Delta Region) = WTP(Non-Delta Region) within each info treatment.\n")
cat("Two-sided p-value = 2 * min(Pr(diff <= 0), Pr(diff > 0)).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 15. Final Export --------------------------------------------------------

write.csv(table8, "wtp_table8_poe_test_region_within_treatment.csv", row.names = FALSE)

cat("\n✓ Additional CSV exported:\n")
cat("  wtp_table8_poe_test_region_within_treatment.csv\n")



# =============================================================================
# --- 16. Table 9: WTP + Poe Test — Delta Region vs. Black Non-Delta ----------
#         Run separately within each info treatment, per product
#
# Groups compared within each info treatment:
#   Group A: Delta region residents        (target_state == 1, any race)
#   Group B: Black, Non-Delta residents    (black == 1 & target_state == 0)
#
# Table 9a: WTP + % premium for each group x product x treatment
# Table 9b: Poe test comparing Group A vs. Group B within each treatment
# =============================================================================

# Helper: fit logit on a pre-filtered slice, return model + stacked data
fit_logit_slice <- function(data) {
  stacked <- bind_rows(
    data %>% transmute(choice = choice1, bid = price1),
    data %>% transmute(choice = choice2, bid = price2)
  )
  model <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
  list(model = model, stacked = stacked)
}

# Helper: extract WTP, SE, CI, LR test, and % premium from a fitted model + data
extract_wtp_stats <- function(fit, base_price) {
  model    <- fit$model
  stacked  <- fit$stacked
  coefs    <- coef(model)
  a        <- coefs["(Intercept)"]
  b        <- coefs["bid"]
  v        <- vcov(model)
  mean_wtp <- -a / b
  se_wtp   <- sqrt(v["(Intercept)","(Intercept)"] / b^2 +
                     a^2 * v["bid","bid"] / b^4 -
                     2 * a * v["(Intercept)","bid"] / b^3)
  # Fit null model directly on the same stacked data — avoids update() scoping issue
  null_mod <- glm(choice ~ 1, data = stacked, family = binomial(link = "logit"))
  lr_chi2  <- round(2 * (as.numeric(logLik(model)) - as.numeric(logLik(null_mod))), 3)
  lr_p     <- round(pchisq(lr_chi2, df = 1, lower.tail = FALSE), 4)
  lr_sig   <- case_when(lr_p < 0.01 ~ "***", lr_p < 0.05 ~ "**",
                        lr_p < 0.10 ~ "*",   TRUE ~ "")
  pct_prem <- round((mean_wtp - base_price) / base_price * 100, 2)
  
  tibble(
    mean_wtp    = round(mean_wtp, 3),
    se_wtp      = round(se_wtp, 3),
    ci_lower    = round(mean_wtp - 1.96 * se_wtp, 3),
    ci_upper    = round(mean_wtp + 1.96 * se_wtp, 3),
    pct_premium = pct_prem,
    lr_chi2     = lr_chi2,
    lr_df       = 1L,
    lr_p        = lr_p,
    lr_sig      = lr_sig
  )
}

# Helper: Poe test between two fitted models
poe_two_models <- function(model_a, model_b, n_sim = 10000, seed = 42) {
  set.seed(seed)
  
  draw_wtp <- function(m) {
    draws <- MASS::mvrnorm(n_sim, mu = coef(m), Sigma = vcov(m))
    wtp   <- -draws[, "(Intercept)"] / draws[, "bid"]
    wtp[is.finite(wtp)]
  }
  
  wtp_a <- draw_wtp(model_a)
  wtp_b <- draw_wtp(model_b)
  n_use <- min(length(wtp_a), length(wtp_b))
  diffs <- wtp_a[1:n_use] - wtp_b[1:n_use]
  
  p_one <- mean(diffs <= 0)
  p_two <- round(2 * min(p_one, 1 - p_one), 4)
  list(
    wtp_diff = round(mean(wtp_a) - mean(wtp_b), 3),
    poe_p    = p_two,
    poe_sig  = case_when(p_two < 0.01 ~ "***", p_two < 0.05 ~ "**",
                         p_two < 0.10 ~ "*",   TRUE ~ "")
  )
}

# --- Build Table 9a and 9b ---------------------------------------------------

treatments <- sort(unique(long_df$info_treatment))

table9_rows_a  <- list()   # WTP stats per group
table9_rows_b  <- list()   # Poe test results

cat("\nBuilding Table 9 (Delta Region vs. Black Non-Delta, within each treatment)...\n")

for (trt in treatments) {
  for (prod in products) {
    
    bp <- base_prices %>% filter(product == prod) %>% pull(base_price)
    
    # Subset to this treatment x product
    base_data <- long_df %>%
      filter(info_treatment == trt, product == prod)
    
    # Group A: Delta region (target_state == 1, any race)
    data_a <- base_data %>% filter(target_state == 1)
    
    # Group B: Black, Non-Delta region (black == 1, target_state == 0)
    data_b <- base_data %>% filter(race_b == 1, target_state == 0)
    
    # Skip if either group is too small to fit a model (need >1 unique bid)
    enough_a <- length(unique(c(data_a$price1, data_a$price2))) > 1 & nrow(data_a) > 2
    enough_b <- length(unique(c(data_b$price1, data_b$price2))) > 1 & nrow(data_b) > 2
    
    if (!enough_a || !enough_b) {
      warning(paste("Insufficient data for", prod, "in treatment", trt, "— skipping."))
      next
    }
    
    fit_a   <- fit_logit_slice(data_a)
    fit_b   <- fit_logit_slice(data_b)
    
    stats_a <- extract_wtp_stats(fit_a, bp)
    stats_b <- extract_wtp_stats(fit_b, bp)
    poe     <- poe_two_models(fit_a$model, fit_b$model)
    
    # Table 9a rows
    table9_rows_a[[length(table9_rows_a) + 1]] <- tibble(
      Treatment = trt,
      Product   = str_to_title(prod),
      Group     = "Delta Region",
      n         = nrow(data_a)
    ) %>% bind_cols(stats_a)
    
    table9_rows_a[[length(table9_rows_a) + 1]] <- tibble(
      Treatment = trt,
      Product   = str_to_title(prod),
      Group     = "Black Non-Delta",
      n         = nrow(data_b)
    ) %>% bind_cols(stats_b)
    
    # Table 9b rows
    table9_rows_b[[length(table9_rows_b) + 1]] <- tibble(
      Treatment                   = trt,
      Product                     = str_to_title(prod),
      `WTP: Delta Region ($)`     = stats_a$mean_wtp,
      `% Premium: Delta Region`   = stats_a$pct_premium,
      `WTP: Black Non-Delta ($)`  = stats_b$mean_wtp,
      `% Premium: Black Non-Delta`= stats_b$pct_premium,
      `WTP Difference ($)`        = poe$wtp_diff,
      `Poe p-value (two-sided)`   = poe$poe_p,
      ` `                         = poe$poe_sig
    )
  }
}

table9a <- bind_rows(table9_rows_a) %>%
  dplyr::select(Treatment, Product, Group, n,
                `Mean WTP ($)`  = mean_wtp,
                `SE`            = se_wtp,
                `95% CI Lower`  = ci_lower,
                `95% CI Upper`  = ci_upper,
                `% Premium`     = pct_premium,
                `LR Chi-sq`     = lr_chi2,
                `LR df`         = lr_df,
                `LR p-value`    = lr_p,
                ` `             = lr_sig) %>%
  arrange(Treatment, Product, Group)

table9b <- bind_rows(table9_rows_b) %>%
  arrange(Treatment, Product)

cat("\n===========================================================================\n")
cat("Table 9a: WTP + Premium by Group — Delta Region vs. Black Non-Delta\n")
cat("          Estimated separately within each info treatment, per product\n")
cat("===========================================================================\n")
print(fmt(table9a))
cat("Group A: Delta region residents (target_state == 1, any race).\n")
cat("Group B: Black, Non-Delta region residents (black == 1, target_state == 0).\n")
cat("LR test H0: bid price has no effect on Delta product choice within group (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")

cat("\n===========================================================================\n")
cat("Table 9b: Poe Test — Delta Region vs. Black Non-Delta WTP Comparison\n")
cat("          Within each info treatment, per product\n")
cat("===========================================================================\n")
print(fmt(table9b))
cat("Poe test (Poe, Giraud & Loomis, 2005): 10,000 simulated WTP draws per model\n")
cat("from N(coef, vcov). H0: WTP(Delta Region) = WTP(Black Non-Delta Region).\n")
cat("Two-sided p-value = 2 * min(Pr(diff <= 0), Pr(diff > 0)).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 17. Export Table 9 ------------------------------------------------------

write.csv(table9a, "wtp_table9a_delta_vs_blacknondelta_wtp.csv",      row.names = FALSE)
write.csv(table9b, "wtp_table9b_delta_vs_blacknondelta_poe_test.csv", row.names = FALSE)

cat("\n✓ Table 9 CSVs exported:\n")
cat("  wtp_table9a_delta_vs_blacknondelta_wtp.csv\n")
cat("  wtp_table9b_delta_vs_blacknondelta_poe_test.csv\n")




# =============================================================================
# --- 18. Table 10: Delta Info Treatment x Delta Region Only ------------------
#         Black vs. Non-Black residents — WTP + Poe Test, per product
#
# Sample: info_treatment == "delta" & target_state == 1
# Group A: Black residents        (race_b == 1)
# Group B: Non-Black residents    (race_b == 0)
# =============================================================================

table10_rows_a <- list()   # WTP stats per group
table10_rows_b <- list()   # Poe test results

cat("\nBuilding Table 10 (Delta treatment, Delta region: Black vs. Non-Black)...\n")

for (prod in products) {
  
  bp <- base_prices %>% filter(product == prod) %>% pull(base_price)
  
  # Filter to Delta info treatment + Delta region only
  base_data <- long_df %>%
    filter(info_treatment == "delta", target_state == 1, product == prod)
  
  # Group A: Black residents
  data_a <- base_data %>% filter(race_b == 1)
  
  # Group B: Non-Black residents
  data_b <- base_data %>% filter(race_b == 0)
  
  # Check sufficient data in each group
  enough_a <- length(unique(c(data_a$price1, data_a$price2))) > 1 & nrow(data_a) > 2
  enough_b <- length(unique(c(data_b$price1, data_b$price2))) > 1 & nrow(data_b) > 2
  
  if (!enough_a || !enough_b) {
    cat(sprintf(
      "  SKIPPED: product=%-15s  n_black=%d  n_nonblack=%d  enough_a=%s  enough_b=%s\n",
      prod, nrow(data_a), nrow(data_b), enough_a, enough_b
    ))
    next
  }
  
  fit_a   <- fit_logit_slice(data_a)
  fit_b   <- fit_logit_slice(data_b)
  
  stats_a <- extract_wtp_stats(fit_a, bp)
  stats_b <- extract_wtp_stats(fit_b, bp)
  poe     <- poe_two_models(fit_a$model, fit_b$model)
  
  # Table 10a rows — WTP per group
  table10_rows_a[[length(table10_rows_a) + 1]] <- tibble(
    Product = str_to_title(prod),
    Group   = "Black",
    n       = nrow(data_a)
  ) %>% bind_cols(stats_a)
  
  table10_rows_a[[length(table10_rows_a) + 1]] <- tibble(
    Product = str_to_title(prod),
    Group   = "Non-Black",
    n       = nrow(data_b)
  ) %>% bind_cols(stats_b)
  
  # Table 10b rows — Poe test
  table10_rows_b[[length(table10_rows_b) + 1]] <- tibble(
    Product                      = str_to_title(prod),
    `WTP: Black ($)`             = stats_a$mean_wtp,
    `% Premium: Black`           = stats_a$pct_premium,
    `WTP: Non-Black ($)`         = stats_b$mean_wtp,
    `% Premium: Non-Black`       = stats_b$pct_premium,
    `WTP Difference ($)`         = poe$wtp_diff,
    `Poe p-value (two-sided)`    = poe$poe_p,
    ` `                          = poe$poe_sig
  )
}

table10a <- bind_rows(table10_rows_a) %>%
  dplyr::select(
    Product,
    Group,
    n,
    `Mean WTP ($)`  = mean_wtp,
    `SE`            = se_wtp,
    `95% CI Lower`  = ci_lower,
    `95% CI Upper`  = ci_upper,
    `% Premium`     = pct_premium,
    `LR Chi-sq`     = lr_chi2,
    `LR df`         = lr_df,
    `LR p-value`    = lr_p,
    ` `             = lr_sig
  ) %>%
  arrange(Product, Group)

table10b <- bind_rows(table10_rows_b) %>%
  arrange(Product)

cat("\n===========================================================================\n")
cat("Table 10a: WTP + Premium — Black vs. Non-Black, Delta Treatment x Delta Region\n")
cat("===========================================================================\n")
print(fmt(table10a))
cat("Sample: Delta info treatment recipients living in the Delta region.\n")
cat("LR test H0: bid price has no effect on Delta product choice within group (df = 1).\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")

cat("\n===========================================================================\n")
cat("Table 10b: Poe Test — Black vs. Non-Black WTP Comparison\n")
cat("           Delta Info Treatment, Delta Region Only\n")
cat("===========================================================================\n")
print(fmt(table10b))
cat("Poe test (Poe, Giraud & Loomis, 2005): 10,000 simulated WTP draws per model\n")
cat("from N(coef, vcov). H0: WTP(Black) = WTP(Non-Black) among Delta region\n")
cat("residents who received the Delta info treatment. Two-sided.\n")
cat("Significance: *** p<0.01  ** p<0.05  * p<0.10\n")


# --- 19. Export Table 10 -----------------------------------------------------

write.csv(table10a, "wtp_table10a_delta_treatment_region_black_vs_nonblack.csv", row.names = FALSE)
write.csv(table10b, "wtp_table10b_delta_treatment_region_poe_test.csv",          row.names = FALSE)

cat("\n✓ Table 10 CSVs exported:\n")
cat("  wtp_table10a_delta_treatment_region_black_vs_nonblack.csv\n")
cat("  wtp_table10b_delta_treatment_region_poe_test.csv\n")


# --- 20. Tables for Figures --------------------------------------------------

# all

inforeg_sum <- df %>% group_by(target_state,info_treatment) %>%
  summarise(count = n())


# rice
rice_inforeg_sum <- df %>% group_by(target_state,info_treatment,ricec1,ricec2) %>%
  summarise(count = n())
write.csv(rice_inforeg_sum, "figures_inforeg_rice.csv",          row.names = FALSE)

# tomato
tomato_inforeg_sum <- df %>% group_by(target_state,info_treatment,materc1,materc2) %>%
  summarise(count = n())
write.csv(tomato_inforeg_sum, "figures_inforeg_tomato.csv",          row.names = FALSE)

# sweet potato
swpotato_inforeg_sum <- df %>% group_by(target_state,info_treatment,taterc1,taterc2) %>%
  summarise(count = n())
write.csv(swpotato_inforeg_sum, "figures_inforeg_swpotato.csv",          row.names = FALSE)

# melon
melon_inforeg_sum <- df %>% group_by(target_state,info_treatment,melonc1,melonc2) %>%
  summarise(count = n())
write.csv(melon_inforeg_sum, "figures_inforeg_melon.csv",          row.names = FALSE)

# corn
corn_inforeg_sum <- df %>% group_by(target_state,info_treatment,cornc1,cornc2) %>%
  summarise(count = n())
write.csv(corn_inforeg_sum, "figures_inforeg_corn.csv",          row.names = FALSE)
