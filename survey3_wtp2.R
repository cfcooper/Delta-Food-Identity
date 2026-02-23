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


# --- 2. Load Data ------------------------------------------------------------

# Replace with your actual file path
df <- read.csv("your_data.csv")

# Confirm structure
glimpse(df)


# --- 3. Define Products & Reshape to Long Format ----------------------------

products <- c("corn", "tomatoes", "sweetpotatoes", "watermelon", "rice")

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


# --- 5. DBDC Logit Model per Product ----------------------------------------
# We stack Q1 and Q2 as separate binary observations per respondent,
# with the bid (price offered) as the key covariate.
# WTP = -intercept / price_coef  (from logit model)

run_dbdc_logit <- function(data, product_name) {
  
  prod_data <- data %>% filter(product == product_name)
  
  # Stack Q1 and Q2 as separate rows
  q1 <- prod_data %>%
    transmute(respondent_id, info_treatment, choice = choice1, bid = price1, product)
  
  q2 <- prod_data %>%
    transmute(respondent_id, info_treatment, choice = choice2, bid = price2, product)
  
  stacked <- bind_rows(q1, q2)
  
  # Logit model: choice ~ bid
  model <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
  
  # Mean WTP = -intercept / bid_coefficient
  coefs     <- coef(model)
  mean_wtp  <- -coefs["(Intercept)"] / coefs["bid"]
  
  # Delta method standard error for WTP
  vcov_mat  <- vcov(model)
  a         <- coefs["(Intercept)"]
  b         <- coefs["bid"]
  se_wtp    <- sqrt((vcov_mat["(Intercept)", "(Intercept)"] / b^2) +
                      (a^2 * vcov_mat["bid", "bid"] / b^4) -
                      (2 * a * vcov_mat["(Intercept)", "bid"] / b^3))
  
  list(model = model, mean_wtp = mean_wtp, se_wtp = se_wtp, data = stacked)
}

# Run for all products
product_models <- set_names(
  map(products, ~ run_dbdc_logit(long_df, .x)),
  products
)


# --- 6. WTP by Treatment Group (per product) ---------------------------------

run_dbdc_logit_by_treatment <- function(data, product_name) {
  
  prod_data <- data %>% filter(product == product_name)
  treatments <- unique(prod_data$info_treatment)
  
  map_dfr(treatments, function(trt) {
    
    trt_data <- prod_data %>% filter(info_treatment == trt)
    
    q1 <- trt_data %>% transmute(respondent_id, info_treatment, choice = choice1, bid = price1)
    q2 <- trt_data %>% transmute(respondent_id, info_treatment, choice = choice2, bid = price2)
    stacked <- bind_rows(q1, q2)
    
    model  <- glm(choice ~ bid, data = stacked, family = binomial(link = "logit"))
    coefs  <- coef(model)
    a <- coefs["(Intercept)"]
    b <- coefs["bid"]
    
    mean_wtp <- -a / b
    
    vcov_mat <- vcov(model)
    se_wtp   <- sqrt((vcov_mat["(Intercept)", "(Intercept)"] / b^2) +
                       (a^2 * vcov_mat["bid", "bid"] / b^4) -
                       (2 * a * vcov_mat["(Intercept)", "bid"] / b^3))
    
    tibble(
      product        = product_name,
      info_treatment = trt,
      mean_wtp       = round(mean_wtp, 3),
      se_wtp         = round(se_wtp, 3),
      ci_lower       = round(mean_wtp - 1.96 * se_wtp, 3),
      ci_upper       = round(mean_wtp + 1.96 * se_wtp, 3)
    )
  })
}

wtp_by_treatment <- map_dfr(products, ~ run_dbdc_logit_by_treatment(long_df, .x))


# --- 7. Per-Respondent WTP Estimates ----------------------------------------
# Estimated as -( intercept + 0 ) / bid_coef is a population-level estimate.
# Per-respondent WTP is derived from their individual bid-response pattern
# using the midpoint of their WTP interval as a practical approximation.

respondent_wtp <- long_df %>%
  mutate(
    wtp_estimate = case_when(
      response_pattern == "YY" ~ wtp_lower,               # WTP >= upper bid; use upper bid as lower bound
      response_pattern == "YN" ~ (wtp_lower + wtp_upper) / 2,  # midpoint of [p1, p2]
      response_pattern == "NY" ~ (wtp_lower + wtp_upper) / 2,  # midpoint of [p2, p1]
      response_pattern == "NN" ~ 0                         # WTP < lower bid; conservative = 0
    )
  ) %>%
  select(respondent_id, info_treatment, product, response_pattern,
         price1, price2, wtp_estimate)


# --- 8. Summary Tables -------------------------------------------------------

# --- Table 1: Overall Mean WTP per Product ---
table1_overall <- map_dfr(products, function(prod) {
  m <- product_models[[prod]]
  tibble(
    Product    = str_to_title(prod),
    Mean_WTP   = round(m$mean_wtp, 3),
    Std_Error  = round(m$se_wtp, 3),
    CI_Lower   = round(m$mean_wtp - 1.96 * m$se_wtp, 3),
    CI_Upper   = round(m$mean_wtp + 1.96 * m$se_wtp, 3)
  )
})

cat("\n========================================\n")
cat("Table 1: Overall Mean WTP per Product\n")
cat("========================================\n")
print(kable(table1_overall, format = "simple",
            col.names = c("Product", "Mean WTP ($)", "Std Error", "95% CI Lower", "95% CI Upper")))


# --- Table 2: Mean WTP by Product and Treatment Group ---
table2_by_treatment <- wtp_by_treatment %>%
  mutate(
    Product   = str_to_title(product),
    Treatment = info_treatment,
    WTP_CI    = paste0(mean_wtp, " [", ci_lower, ", ", ci_upper, "]")
  ) %>%
  select(Product, Treatment, Mean_WTP = mean_wtp, Std_Error = se_wtp,
         CI_Lower = ci_lower, CI_Upper = ci_upper)

cat("\n=================================================\n")
cat("Table 2: Mean WTP by Product and Treatment Group\n")
cat("=================================================\n")
print(kable(table2_by_treatment, format = "simple",
            col.names = c("Product", "Treatment", "Mean WTP ($)", "Std Error", "95% CI Lower", "95% CI Upper")))


# --- Table 3: Per-Respondent WTP Summary (mean WTP per respondent across products) ---
table3_respondent <- respondent_wtp %>%
  group_by(respondent_id, info_treatment) %>%
  summarise(
    mean_wtp_across_products = round(mean(wtp_estimate, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(respondent_id)

cat("\n=============================================\n")
cat("Table 3: Per-Respondent Mean WTP (all products)\n")
cat("=============================================\n")
print(kable(head(table3_respondent, 20), format = "simple",
            col.names = c("Respondent ID", "Treatment", "Mean WTP ($)")))
cat("  ... (showing first 20 rows)\n")


# --- Table 4: Per-Respondent WTP by Product (wide format) ---
table4_respondent_wide <- respondent_wtp %>%
  select(respondent_id, info_treatment, product, wtp_estimate) %>%
  pivot_wider(names_from = product, values_from = wtp_estimate,
              names_prefix = "wtp_") %>%
  arrange(respondent_id)

cat("\n=============================================\n")
cat("Table 4: Per-Respondent WTP by Product (wide)\n")
cat("=============================================\n")
print(kable(head(table4_respondent_wide, 20), format = "simple"))
cat("  ... (showing first 20 rows)\n")


# --- 9. Treatment Comparison: WTP Difference Test ----------------------------
# For each product, test whether mean WTP differs significantly between treatments

cat("\n=============================================\n")
cat("Table 5: Treatment Difference in WTP (per product)\n")
cat("=============================================\n")

treatment_diff <- wtp_by_treatment %>%
  select(product, info_treatment, mean_wtp, se_wtp) %>%
  pivot_wider(names_from = info_treatment,
              values_from = c(mean_wtp, se_wtp)) %>%
  rename_with(~ str_replace_all(.x, " ", "_")) %>%
  # Compute z-test for difference between two treatments
  # Assumes treatment column names are mean_wtp_Black and mean_wtp_Delta (adjust if different)
  mutate(across(everything(), ~ replace_na(.x, NA)))

# Dynamic: grab the two treatment names
trt_names <- unique(wtp_by_treatment$info_treatment)

if (length(trt_names) == 2) {
  t1 <- trt_names[1]; t2 <- trt_names[2]
  
  diff_table <- wtp_by_treatment %>%
    select(product, info_treatment, mean_wtp, se_wtp) %>%
    pivot_wider(names_from = info_treatment, values_from = c(mean_wtp, se_wtp)) %>%
    mutate(
      wtp_diff  = .data[[paste0("mean_wtp_", t1)]] - .data[[paste0("mean_wtp_", t2)]],
      se_diff   = sqrt(.data[[paste0("se_wtp_", t1)]]^2 + .data[[paste0("se_wtp_", t2)]]^2),
      z_stat    = round(wtp_diff / se_diff, 3),
      p_value   = round(2 * (1 - pnorm(abs(z_stat))), 4),
      sig       = case_when(p_value < 0.01 ~ "***",
                            p_value < 0.05 ~ "**",
                            p_value < 0.10 ~ "*",
                            TRUE ~ "")
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    select(product, starts_with("mean_wtp"), wtp_diff, se_diff, z_stat, p_value, sig)
  
  print(kable(diff_table, format = "simple"))
  cat("  Significance: *** p<0.01  ** p<0.05  * p<0.10\n")
} else {
  cat("  More than 2 treatment groups detected — manual pairwise comparison recommended.\n")
}


# --- 10. Export Results ------------------------------------------------------

write.csv(table1_overall,        "wtp_overall_by_product.csv",      row.names = FALSE)
write.csv(table2_by_treatment,   "wtp_by_product_treatment.csv",    row.names = FALSE)
write.csv(table4_respondent_wide,"wtp_per_respondent_wide.csv",     row.names = FALSE)
write.csv(table3_respondent,     "wtp_per_respondent_summary.csv",  row.names = FALSE)

cat("\n✓ All CSV files exported to working directory.\n")
cat("  - wtp_overall_by_product.csv\n")
cat("  - wtp_by_product_treatment.csv\n")
cat("  - wtp_per_respondent_wide.csv\n")
cat("  - wtp_per_respondent_summary.csv\n")