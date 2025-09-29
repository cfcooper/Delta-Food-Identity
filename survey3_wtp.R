## new code (translated from STATA)




# Install Icens from Bioconductor (required for DCchoice)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Icens")

# Install DCchoice if needed
if (!require("DCchoice", quietly = TRUE))
  install.packages("DCchoice")


rm(list=ls()) # Caution: this clears the Environment

# Load required packages
library(DCchoice)
library(dplyr)

# Load data
delta <- read.csv("rawdata/DWTP_3.csv")

# Summary statistics
cat("===========================================\n")
cat("RAW DATA SUMMARY\n")
cat("===========================================\n")
cat("Total rows:", nrow(delta), "\n")
cat("\nBlack variable distribution:\n")
print(table(delta$black, useNA = "always"))
cat("\nSample of corn variables (first 10 rows):\n")
print(delta[1:10, c("black", "cornd", "cornd_high", "cornd_low", "cornb", "cornb_high", "cornb_low", "corn_h", "corn_l")])
cat("\n")

summary(delta)

# Create group variable
delta <- delta %>%
  mutate(
    group = case_when(
      black == 0 & target_state == 0 ~ 1,  # Not in south, not black info
      black == 1 & target_state == 0 ~ 2,  # Not in south, black info
      black == 0 & target_state == 1 ~ 3,  # In south, not black info
      black == 1 & target_state == 1 ~ 4   # In south, black info
    )
  )

# Function to prepare double-bounded data for DCchoice
# treatment: "d" for delta, "b" for black
prepare_db_data <- function(data, product, treatment, base_price) {
  
  # Filter data based on treatment
  # Delta treatment: groups 1 and 3 (black == 0)
  # Black treatment: groups 2 and 4 (black == 1)
  if (treatment == "d") {
    data <- data %>% filter(black == 0)
  } else if (treatment == "b") {
    data <- data %>% filter(black == 1)
  }
  
  # Build column names based on treatment
  first_q <- paste0(product, treatment)
  high_q <- paste0(product, treatment, "_high")
  low_q <- paste0(product, treatment, "_low")
  high_price <- paste0(product, "_h")  # e.g., corn_h
  low_price <- paste0(product, "_l")   # e.g., corn_l
  
  # Create the response variables
  # In your data: 1 = chose non-delta/non-black product, 2 = chose delta/black product
  # For DCchoice: 1 = accepted the bid (chose delta/black), 0 = rejected (chose non-delta/non-black)
  result <- data %>%
    mutate(
      # R1: First question response (2 = chose delta/black = accepted bid = 1)
      R1 = ifelse(get(first_q) == 2, 1, 0),
      
      # R2: Second question response
      # If R1=1 (accepted first bid), look at high question
      # If R1=0 (rejected first bid), look at low question
      R2 = case_when(
        get(first_q) == 2 ~ ifelse(get(high_q) == 2, 1, 0),  # Accepted first, check high
        get(first_q) == 1 ~ ifelse(get(low_q) == 2, 1, 0),   # Rejected first, check low
        TRUE ~ NA_real_
      ),
      
      # bid1: Initial price (same for everyone)
      bid1 = base_price,
      
      # bid2: Follow-up price
      bid2 = case_when(
        get(first_q) == 2 ~ get(high_price),  # Accepted first, show high price
        get(first_q) == 1 ~ get(low_price),   # Rejected first, show low price
        TRUE ~ NA_real_
      )
    ) %>%
    select(R1, R2, bid1, bid2) %>%
    filter(!is.na(R1) & !is.na(R2) & !is.na(bid1) & !is.na(bid2))
  
  return(result)
}

# Function to prepare percent change data
prepare_db_data_pct <- function(data, product, treatment, base_price) {
  
  # Filter data based on treatment
  if (treatment == "d") {
    data <- data %>% filter(black == 0)
  } else if (treatment == "b") {
    data <- data %>% filter(black == 1)
  }
  
  first_q <- paste0(product, treatment)
  high_q <- paste0(product, treatment, "_high")
  low_q <- paste0(product, treatment, "_low")
  high_price <- paste0(product, "_h")
  low_price <- paste0(product, "_l")
  
  result <- data %>%
    mutate(
      # R1: First question (2 = chose delta/black = accepted = 1)
      R1 = ifelse(get(first_q) == 2, 1, 0),
      
      # R2: Second question
      R2 = case_when(
        get(first_q) == 2 ~ ifelse(get(high_q) == 2, 1, 0),
        get(first_q) == 1 ~ ifelse(get(low_q) == 2, 1, 0),
        TRUE ~ NA_real_
      ),
      
      bid1 = 0,  # Base is 0% change
      bid2 = case_when(
        get(first_q) == 2 ~ (get(high_price) - base_price) / base_price,
        get(first_q) == 1 ~ (get(low_price) - base_price) / base_price,
        TRUE ~ NA_real_
      )
    ) %>%
    select(R1, R2, bid1, bid2) %>%
    filter(!is.na(R1) & !is.na(R2) & !is.na(bid1) & !is.na(bid2))
  
  return(result)
}

# Function to estimate model
estimate_db_model <- function(data, product, treatment, base_price, use_pct_change = FALSE) {
  
  treatment_name <- ifelse(treatment == "d", "Delta", "Black")
  
  if (use_pct_change) {
    db_data <- prepare_db_data_pct(data, product, treatment, base_price)
    model_type <- "percent change"
  } else {
    db_data <- prepare_db_data(data, product, treatment, base_price)
    model_type <- "price"
  }
  
  # Print diagnostics
  cat("\n===========================================\n")
  cat(paste0("Product: ", toupper(product), " | Treatment: ", treatment_name, " (", model_type, " model)\n"))
  cat("===========================================\n")
  
  # Show filtering
  if (treatment == "d") {
    cat("Filtering for Delta treatment (black == 0)\n")
  } else {
    cat("Filtering for Black treatment (black == 1)\n")
  }
  
  cat(paste0("Rows after filtering: ", nrow(db_data), "\n"))
  
  if (nrow(db_data) == 0) {
    cat("\nERROR: No data after filtering!\n")
    return(list(model = NULL, wtp = NULL, n = 0, error = "No data after filtering"))
  }
  
  # Check data quality
  cat("\nData diagnostics:\n")
  cat("Response patterns:\n")
  print(table(db_data$R1, db_data$R2, dnn = c("R1", "R2")))
  cat("\nBid summary:\n")
  cat("bid1 - min:", min(db_data$bid1), "max:", max(db_data$bid1), "\n")
  cat("bid2 - min:", min(db_data$bid2), "max:", max(db_data$bid2), "\n")
  cat("bid2 unique values:", length(unique(db_data$bid2)), "\n")
  cat("\nFirst few rows of prepared data:\n")
  print(head(db_data, 10))
  cat("\n")
  
  # Check for problems
  if (nrow(db_data) < 10) {
    cat("\nWARNING: Very few observations (n < 10). Skipping estimation.\n")
    return(list(model = NULL, wtp = NULL, n = nrow(db_data), error = "Too few observations"))
  }
  
  if (var(db_data$R1) == 0) {
    cat("\nWARNING: No variation in R1 (all same response). Skipping estimation.\n")
    return(list(model = NULL, wtp = NULL, n = nrow(db_data), error = "No variation in R1"))
  }
  
  if (length(unique(db_data$bid2)) < 2) {
    cat("\nWARNING: No variation in bid2 prices. Skipping estimation.\n")
    return(list(model = NULL, wtp = NULL, n = nrow(db_data), error = "No variation in bid2"))
  }
  
  # Try to estimate model
  model <- tryCatch({
    dbchoice(
      R1 + R2 ~ 1 | bid1 + bid2,
      data = db_data,
      dist = "logistic"
    )
  }, error = function(e) {
    cat("\nERROR in model estimation:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(model)) {
    return(list(model = NULL, wtp = NULL, n = nrow(db_data), error = "Estimation failed"))
  }
  
  # Print results
  print(summary(model))
  
  # Calculate and display WTP
  wtp_est <- tryCatch({
    krCI(model, individual = NULL)
  }, error = function(e) {
    cat("\nWARNING: Could not calculate WTP confidence intervals:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(wtp_est)) {
    cat("\nWillingness to Pay Estimates:\n")
    print(wtp_est)
  }
  
  return(list(model = model, wtp = wtp_est, n = nrow(db_data)))
}

# ============================================================================
# CORN - Base price = $0.33
# ============================================================================
cat("\n\n############### CORN ###############\n")
corn_delta_price <- estimate_db_model(delta, "corn", "d", 0.33, use_pct_change = FALSE)
corn_delta_pct <- estimate_db_model(delta, "corn", "d", 0.33, use_pct_change = TRUE)
corn_black_price <- estimate_db_model(delta, "corn", "b", 0.33, use_pct_change = FALSE)
corn_black_pct <- estimate_db_model(delta, "corn", "b", 0.33, use_pct_change = TRUE)

# ============================================================================
# MELON - Base price = $5
# ============================================================================
cat("\n\n############### MELON ###############\n")
melon_delta_price <- estimate_db_model(delta, "melon", "d", 5, use_pct_change = FALSE)
melon_delta_pct <- estimate_db_model(delta, "melon", "d", 5, use_pct_change = TRUE)
melon_black_price <- estimate_db_model(delta, "melon", "b", 5, use_pct_change = FALSE)
melon_black_pct <- estimate_db_model(delta, "melon", "b", 5, use_pct_change = TRUE)

# ============================================================================
# TATER - Base price = $1
# ============================================================================
cat("\n\n############### TATER ###############\n")
tater_delta_price <- estimate_db_model(delta, "tater", "d", 1, use_pct_change = FALSE)
tater_delta_pct <- estimate_db_model(delta, "tater", "d", 1, use_pct_change = TRUE)
tater_black_price <- estimate_db_model(delta, "tater", "b", 1, use_pct_change = FALSE)
tater_black_pct <- estimate_db_model(delta, "tater", "b", 1, use_pct_change = TRUE)

# ============================================================================
# MATER - Base price = $1
# ============================================================================
cat("\n\n############### MATER ###############\n")
mater_delta_price <- estimate_db_model(delta, "mater", "d", 1, use_pct_change = FALSE)
mater_delta_pct <- estimate_db_model(delta, "mater", "d", 1, use_pct_change = TRUE)
mater_black_price <- estimate_db_model(delta, "mater", "b", 1, use_pct_change = FALSE)
mater_black_pct <- estimate_db_model(delta, "mater", "b", 1, use_pct_change = TRUE)

# ============================================================================
# RICE - Base price = $1.75
# ============================================================================
cat("\n\n############### RICE ###############\n")
rice_delta_price <- estimate_db_model(delta, "rice", "d", 1.75, use_pct_change = FALSE)
rice_delta_pct <- estimate_db_model(delta, "rice", "d", 1.75, use_pct_change = TRUE)
rice_black_price <- estimate_db_model(delta, "rice", "b", 1.75, use_pct_change = FALSE)
rice_black_pct <- estimate_db_model(delta, "rice", "b", 1.75, use_pct_change = TRUE)

# ============================================================================
# OPTIONAL: Filter by group
# ============================================================================
# Uncomment to run models on specific groups:
# delta_filtered <- delta %>% filter(group %in% c(1, 3))  # Exclude black info
# delta_filtered <- delta %>% filter(group %in% c(2, 4))  # Only black info
# delta_filtered <- delta %>% filter(target_state == 0)   # Not in south
# delta_filtered <- delta %>% filter(target_state == 1)   # In south
# Then re-run estimate_db_model() with delta_filtered

# ============================================================================
# SUMMARY TABLE
# ============================================================================
extract_wtp <- function(wtp_obj) {
  if (is.null(wtp_obj)) {
    return(data.frame(
      mean = NA,
      median = NA,
      lower_ci = NA,
      upper_ci = NA
    ))
  }
  data.frame(
    mean = wtp_obj$out$mean[1],
    median = wtp_obj$out$median[1],
    lower_ci = wtp_obj$out$lower[1],
    upper_ci = wtp_obj$out$upper[1]
  )
}

wtp_summary <- data.frame(
  Product = rep(c("Corn", "Melon", "Tater", "Mater", "Rice"), each = 4),
  Treatment = rep(rep(c("Delta", "Black"), each = 2), 5),
  Model = rep(c("Price", "Pct Change"), 10),
  N = c(
    corn_delta_price$n, corn_delta_pct$n,
    corn_black_price$n, corn_black_pct$n,
    melon_delta_price$n, melon_delta_pct$n,
    melon_black_price$n, melon_black_pct$n,
    tater_delta_price$n, tater_delta_pct$n,
    tater_black_price$n, tater_black_pct$n,
    mater_delta_price$n, mater_delta_pct$n,
    mater_black_price$n, mater_black_pct$n,
    rice_delta_price$n, rice_delta_pct$n,
    rice_black_price$n, rice_black_pct$n
  ),
  rbind(
    extract_wtp(corn_delta_price$wtp),
    extract_wtp(corn_delta_pct$wtp),
    extract_wtp(corn_black_price$wtp),
    extract_wtp(corn_black_pct$wtp),
    extract_wtp(melon_delta_price$wtp),
    extract_wtp(melon_delta_pct$wtp),
    extract_wtp(melon_black_price$wtp),
    extract_wtp(melon_black_pct$wtp),
    extract_wtp(tater_delta_price$wtp),
    extract_wtp(tater_delta_pct$wtp),
    extract_wtp(tater_black_price$wtp),
    extract_wtp(tater_black_pct$wtp),
    extract_wtp(mater_delta_price$wtp),
    extract_wtp(mater_delta_pct$wtp),
    extract_wtp(mater_black_price$wtp),
    extract_wtp(mater_black_pct$wtp),
    extract_wtp(rice_delta_price$wtp),
    extract_wtp(rice_delta_pct$wtp),
    extract_wtp(rice_black_price$wtp),
    extract_wtp(rice_black_pct$wtp)
  )
)

cat("\n\n===========================================\n")
cat("SUMMARY OF WTP ESTIMATES\n")
cat("===========================================\n")
print(wtp_summary)

# Save results to CSV
write.csv(wtp_summary, "wtp_summary.csv", row.names = FALSE)
cat("\nResults saved to wtp_summary.csv\n")

# Show which models failed
failed_models <- wtp_summary %>% filter(is.na(mean))
if (nrow(failed_models) > 0) {
  cat("\n===========================================\n")
  cat("MODELS THAT FAILED TO ESTIMATE:\n")
  cat("===========================================\n")
  print(failed_models[, c("Product", "Treatment", "Model", "N")])
}