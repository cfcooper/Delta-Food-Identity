## new code (translated from STATA)



# Install Icens from Bioconductor (required for DCchoice)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Icens")

library(DCchoice)
library(dplyr)


# Clear workspace
rm(list = ls())


# read in dataframes

df_bl <- readRDS("cleaneddata/df_black.rds")
df_de <- readRDS("cleaneddata/df_delta.rds")
df <- readRDS("cleaneddata/df_full.rds")

# run models ------------

library(mlogit)


# rice
library(mlogit)

# Step 1: Create long format for DCE
df_long <- data.frame(
  chid = rep(1:nrow(df), each = 2),
  alternative = rep(c("non_delta", "delta"), nrow(df)),
  chosen = c(rbind(1 - df$ricec1, df$ricec1)),
  price = c(rbind(df$ricep1, df$ricep2))
)

# Add treatment variable (constant across alternatives for each choice)
df_long$info_treatment <- rep(df$info_treatment, each = 2)

# Step 2: Prepare data for mlogit
df_mlogit <- mlogit.data(df_long, 
                         choice = "chosen",
                         shape = "long",
                         alt.var = "alternative",
                         chid.var = "chid")

# Step 3: Basic conditional logit model
# Price varies across alternatives, delta dummy in alternative-specific part
model_basic <- mlogit(chosen ~ price | 0, data = df_mlogit)
summary(model_basic)

# Step 4: Model with delta-specific constant (MAIN MODEL)
model_delta <- mlogit(chosen ~ price | alternative, data = df_mlogit)
summary(model_delta)

# Calculate WTP premium for delta
delta_coef <- coef(model_delta)["alternativedelta"]
price_coef <- coef(model_delta)["price"]

wtp_premium <- -delta_coef / price_coef
print(paste("WTP premium for delta: $", round(wtp_premium, 2)))

# Step 5: Model with treatment effects
# Need to create treatment interaction manually in long format
df_long$delta_treat <- ifelse(df_long$alternative == "delta", 
                              df_long$info_treatment, 0)

# Re-prepare mlogit data
df_mlogit2 <- mlogit.data(df_long, 
                          choice = "chosen",
                          shape = "long",
                          alt.var = "alternative",
                          chid.var = "chid")

# Model with treatment interaction
model_treatment <- mlogit(chosen ~ price + delta_treat | alternative, 
                          data = df_mlogit2)
summary(model_treatment)

# Calculate WTP by treatment group
delta_coef <- coef(model_treatment)["alternativedelta"]
treatment_coef <- coef(model_treatment)["delta_treat"]
price_coef <- coef(model_treatment)["price"]

wtp_control <- -delta_coef / price_coef
wtp_info <- -(delta_coef + treatment_coef) / price_coef

print(paste("WTP premium - Control: $", round(wtp_control, 2)))
print(paste("WTP premium - Info: $", round(wtp_info, 2)))
print(paste("Treatment effect: $", round(wtp_info - wtp_control, 2)))

# Step 6: Get standard errors for WTP using delta method
library(car)

# For basic model
wtp_se_basic <- deltaMethod(model_delta, "-alternativedelta/price", 
                            parameterNames = names(coef(model_delta)))
print(wtp_se_basic)

# For treatment model - control group
wtp_se_control <- deltaMethod(model_treatment, "-alternativedelta/price",
                              parameterNames = names(coef(model_treatment)))
print(wtp_se_control)

# For treatment model - info group
wtp_se_info <- deltaMethod(model_treatment, 
                           "-(alternativedelta + delta_treat)/price",
                           parameterNames = names(coef(model_treatment)))
print(wtp_se_info)

