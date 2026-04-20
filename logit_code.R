

library(tidyverse)
library(apollo)
library(writexl)

# import data
#data<-read_csv('data.csv')



# Notes:
# separate row for each respondent/choice task combo
# "choice" is the alternative actually selected
# "av_1"..."av_9" are required for Apollo; binary indicator that the alternative was available
# "p_1"..."p_9" are prices; "p_9" always zero because it is the opt-out option

# MELON CODE -------------------------------------------------------------------

data <- melon_price_b

apollo_initialise()
apollo_control<-list(
  modelName='rpl',
  indivID='ResponseId', # set id variable (required if respondents have multiple choice tasks)
  nCores=4 # set  number of cores
)
# set data
database=data
# set starting values of parameters
apollo_beta=c(mu_asc_1=0, # mean of random utility for alt-spec constant 1
              mu_asc_2=0,
              sigma_asc_1=0, # std dev around mean utility for alt-spec constant 1
              sigma_asc_2=0,
              asc_9=0, # normalize to zero
              b_p=0 # single, linear price term
)
# fix alternative specific constant of opt-out at zero (dropped dummy)
apollo_fixed=c("asc_9") # hold at zero
apollo_draws=list(
  interDrawsType="mlhs", # modified latin hypercube sampling draws
  interNDraws=1000, # number of draws
  interUnifDraws=c(),
  interNormDraws=c("draws_asc_1","draws_asc_2"), # terms to simulate, pulling from normal distribution (for interindividual heterogeneity)
  intraDrawsType="",
  intraNDraws=0,
  intraUnifDraws=c(),
  intraNormDraws=c()
)
apollo_randCoeff=function(apollo_beta,apollo_inputs){
  randcoeff=list()
  randcoeff[["asc_1"]]=mu_asc_1+sigma_asc_1*draws_asc_1 # define random terms (mean + std dev)
  randcoeff[["asc_2"]]=mu_asc_2+sigma_asc_2*draws_asc_2
  return(randcoeff)
}
apollo_inputs=apollo_validateInputs()
# modeling function
apollo_probabilities=function(apollo_beta,apollo_inputs,functionality="estimate"){
  apollo_attach(apollo_beta,apollo_inputs)
  on.exit(apollo_detach(apollo_beta,apollo_inputs))
  P=list()
  # utility equations
  V=list()
  V[["1"]]=asc_1+b_p*p_1 # define indirect utility functions ([random] alt-spec constant plus price)
  V[["2"]]=asc_2+b_p*p_2
  
  mnl_settings=list(
    alternatives=c('1'=1,'2'=2),
    avail=list('1'=av_1,'2'=av_2),
    choiceVar=choice, # set choice variable
    utilities=V
  )
  P[["model"]]=apollo_mnl(mnl_settings,functionality)
  P=apollo_panelProd(P,apollo_inputs,functionality)
  P=apollo_avgInterDraws(P,apollo_inputs,functionality)
  P=apollo_prepareProb(P,apollo_inputs,functionality)
  return(P)
}
rpl=apollo_estimate(apollo_beta,apollo_fixed,apollo_probabilities,apollo_inputs,
                    estimate_settings=list(maxIterations=500))
apollo_modelOutput(rpl)


# parameter estimates
beta<-rpl$betaStop

# Calculate WTP directly from estimated parameters
wtp_alt1 <- -beta["mu_asc_1"] / beta["b_p"]
wtp_alt2 <- -beta["mu_asc_2"] / beta["b_p"]

# Print results
cat("WTP for Alternative 1: $", round(wtp_alt1, 2), "\n")
cat("WTP for Alternative 2: $", round(wtp_alt2, 2), "\n")





# wtp for product 1
wtp1=-beta[1]/beta[17] # can calculate WTP a few ways (this is simplest but more appropriate in MNL designs)


# melon attempt 2 ---------------------
data <- melon_price_b
apollo_initialise()

apollo_control <- list(
  modelName = 'rpl',
  indivID = 'ResponseId',
  nCores = 4
)

database = data

apollo_beta = c(
  mu_asc_1 = 0,     # Mean ASC for non-local (includes $5 price)
  mu_asc_2 = 0,     # Mean ASC for local
  sigma_asc_1 = 0,  # Std dev for non-local
  sigma_asc_2 = 0,  # Std dev for local
  b_p = 0           # Price coefficient
)

apollo_fixed = c()  # Don't fix any parameters

apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc_1", "draws_asc_2"),
  intraDrawsType = "",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  randcoeff[["asc_1"]] = mu_asc_1 + sigma_asc_1 * draws_asc_1
  randcoeff[["asc_2"]] = mu_asc_2 + sigma_asc_2 * draws_asc_2
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  V = list()
  V[["1"]] = asc_1 + b_p * p_1
  V[["2"]] = asc_2 + b_p * p_2
  
  mnl_settings = list(
    alternatives = c('1' = 1, '2' = 2),
    avail = list('1' = av_1, '2' = av_2),
    choiceVar = choice,
    utilities = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Estimate model
rpl = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                      estimate_settings = list(maxIterations = 500))

apollo_modelOutput(rpl)

# Extract parameters - try different methods
beta <- rpl$estimate
if(is.null(beta)) beta <- rpl$betaStop

# Print to check
print("Estimated parameters:")
print(beta)

# Extract price coefficient (handle different formats)
b_p_value <- as.numeric(beta["b_p"])
if(is.na(b_p_value)) {
  b_p_value <- as.numeric(beta[names(beta) == "b_p"])
}

cat("\nPrice coefficient (b_p):", b_p_value, "\n")

# ============ INDIVIDUAL WTP CALCULATION ============

# Get conditional individual-level parameters
conditionals <- apollo_conditionals(rpl, apollo_probabilities, apollo_inputs)

# Check structure
cat("\nConditionals structure:\n")
print(str(conditionals))

# Extract individual ASCs - conditionals returns a list with post.mean
individual_asc1 <- conditionals[["asc_1"]][["post.mean"]]
individual_asc2 <- conditionals[["asc_2"]][["post.mean"]]

# If that doesn't work, try direct access
if(is.null(individual_asc1)) {
  individual_asc1 <- unlist(conditionals[["asc_1"]])
  individual_asc2 <- unlist(conditionals[["asc_2"]])
}

# Calculate individual WTP
# WTP = price where utility of local = utility of non-local
# Solving: asc_1 + b_p*5 = asc_2 + b_p*WTP
# WTP = 5 + (asc_2 - asc_1)/b_p

wtp_individual <- data.frame(
  ResponseId = unique(data$ResponseId),
  wtp_local = 5 + (individual_asc2 - individual_asc1) / b_p_value,
  wtp_premium = (individual_asc2 - individual_asc1) / b_p_value
)

# Population-level WTP
mu_asc_1 <- as.numeric(beta["mu_asc_1"])
mu_asc_2 <- as.numeric(beta["mu_asc_2"])

wtp_population <- 5 + (mu_asc_2 - mu_asc_1) / b_p_value
wtp_premium_pop <- (mu_asc_2 - mu_asc_1) / b_p_value

cat("\n============ WILLINGNESS TO PAY RESULTS ============\n")
cat("Population WTP for local melons: $", round(wtp_population, 2), "\n")
cat("Premium over $5 non-local: $", round(wtp_premium_pop, 2), "\n\n")

# Summary statistics
cat("Individual WTP Summary:\n")
print(summary(wtp_individual$wtp_local))
cat("\nStandard Deviation: $", round(sd(wtp_individual$wtp_local), 2), "\n")

# Merge with demographic data if available
# Assuming your original data has demographics (one row per person)
demo_data <- data %>%
  select(ResponseId, matches("age|income|gender|education")) %>%
  distinct()

wtp_with_demo <- merge(wtp_individual, demo_data, by = "ResponseId", all.x = TRUE)

# Export results
write.csv(wtp_individual, "individual_wtp.csv", row.names = FALSE)
write.csv(wtp_with_demo, "wtp_with_demographics.csv", row.names = FALSE)

cat("\nFiles saved: individual_wtp.csv and wtp_with_demographics.csv\n")

# Example: Calculate mean WTP by demographic groups
# Uncomment and adjust variable names as needed:
# library(dplyr)
# wtp_by_group <- wtp_with_demo %>%
#   group_by(gender) %>%  # Change to your demographic variable
#   summarise(
#     n = n(),
#     mean_wtp = mean(wtp_local, na.rm = TRUE),
#     sd_wtp = sd(wtp_local, na.rm = TRUE),
#     median_wtp = median(wtp_local, na.rm = TRUE)
#   )
# print(wtp_by_group)


# RICE CODE -------------------------------------------------------------------


