

library(tidyverse)
library(apollo)
library(writexl)

# import data
data<-read_csv('data.csv')

# Notes:
# separate row for each respondent/choice task combo
# "choice" is the alternative actually selected
# "av_1"..."av_9" are required for Apollo; binary indicator that the alternative was available
# "p_1"..."p_9" are prices; "p_9" always zero because it is the opt-out option

# MELON CODE -------------------------------------------------------------------

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
  V[["3"]]=asc_3+b_p*p_3
  V[["4"]]=asc_4+b_p*p_4
  V[["5"]]=asc_5+b_p*p_5
  V[["6"]]=asc_6+b_p*p_6
  V[["7"]]=asc_7+b_p*p_7
  V[["8"]]=asc_8+b_p*p_8
  V[["9"]]=asc_9+b_p*p_9 # asc_9 will be 0 (dropped)
  mnl_settings=list(
    alternatives=c('1'=1,'2'=2,'3'=3,'4'=4,'5'=5,'6'=6,'7'=7,'8'=8,'9'=9),
    avail=list('1'=av_1,'2'=av_2,'3'=av_3,'4'=av_4,'5'=av_5,'6'=av_6,'7'=av_7,'8'=av_8,'9'=av_9),
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

# wtp for product 1
wtp1=-beta[1]/beta[17] # can calculate WTP a few ways (this is simplest but more appropriate in MNL designs)


# RICE CODE -------------------------------------------------------------------


