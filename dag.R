library(rethinking)
d <- read.csv("https://www.torkar.se/data-slc.csv")

d$GENDER <- as.integer(ifelse(d$GENDER == "M" , 2 , 1))
d$LUNG_CANCER <- as.integer(ifelse(d$LUNG_CANCER == "YES" , 1 , 0))
d$AGE <- (d$AGE - mean(d$AGE)) / sd(d$AGE)
colnames(d)[colnames(d) %in% c("CHRONIC.DISEASE", "ALCOHOL.CONSUMING", "SHORTNESS.OF.BREATH", "SWALLOWING.DIFFICULTY", "CHEST.PAIN")] <- c("CHRONIC_DISEASE", "ALCOHOL_CONSUMING", "SHORTNESS_OF_BREATH", "SWALLOWING_DIFFICULTY", "CHEST_PAIN")

# m_1 <- quap( 
#     alist(
#     LUNG_CANCER ~ dbinom(1, p),
#     logit(p) <- a ,
#     a ~ dnorm(0, 100)
#     ),
#     data = d,
# )
# 
# # prior <- extract.prior(m_1, n=1e4)
# # p1 <- sapply(1:2, function(k) inv_logit(prior$a))
# # dens(p1)
# 
# m_2 <- quap(
#   alist(
#     LUNG_CANCER ~ dbinom(1, p),
#     logit(p) <- a,
#     a ~ dnorm(0, 1.5)
#   ),
#   data = d
# )
# 
# prior_1 <- extract.prior(m_1, n=1e4)  # Model 1
# prior_2 <- extract.prior(m_2, n=1e4)  # Model 2
# 
# p_1 <- inv_logit(prior_1$a)
# p_2 <- inv_logit(prior_2$a)
# 
# # Plot the first prior density
# dens(p_1, col="blue", xlim=c(0, 1), ylim=c(0, 6), xlab="Probability", main="Prior Comparison")
# # Add the second prior density to the same plot
# dens(p_2, col="red", add=TRUE)
# 
# # Add a legend to distinguish the two priors
# legend("topright", legend=c("Prior 1 (alpha=100)", "Prior 2 (alpha=1.5)"), col=c("blue", "red"), lty=1)
# 






# Model 1: Prior for b_gender ~ dnorm(0, 0.5)
m_1 <- quap(
  alist(
    LUNG_CANCER ~ dbinom(1, p),
    logit(p) <- a + b_gender[GENDER],
    a ~ dnorm(0, 1.5),  # Prior for intercept
    b_gender[GENDER] ~ dnorm(0, 10)  # Prior for beta
  ),
  data = d
)

# Model 2: Prior for b_gender ~ dnorm(0, 1.0)
m_2 <- quap(
  alist(
    LUNG_CANCER ~ dbinom(1, p),
    logit(p) <- a + b_gender[GENDER],
    a ~ dnorm(0, 1.5),  # Same prior for intercept
    b_gender[GENDER] ~ dnorm(0, 0.5)  # Different prior for beta
  ),
  data = d
)

# Check the dimensions of prior$a and prior$b_gender
prior_1 <- extract.prior(m_1, n=1e4)  # Extract prior samples for model 1
prior_2 <- extract.prior(m_2, n=1e4)  # Extract prior samples for model 2

# Ensure b_gender has two dimensions for two categories (e.g., Male and Female)
if (is.matrix(prior_1$b_gender) & is.matrix(prior_2$b_gender)) {
  
  # Apply inv_logit to a + b_gender for both models
  p1 <- sapply(1:2, function(k) inv_logit(prior_1$a + prior_1$b_gender[,k]))
  p2 <- sapply(1:2, function(k) inv_logit(prior_2$a + prior_2$b_gender[,k]))

  # Calculate the absolute difference between gender categories
  b_1 <- abs(p1[,1] - p1[,2])  # For Model 1
  b_2 <- abs(p2[,1] - p2[,2])  # For Model 2

  # Plot the density of beta for Model 1
  dens(b_1, col="blue", xlim=c(0, 1), ylim=c(0, 10), xlab="Beta (Gender)", main="Prior Comparison for Beta (Gender)")

  # Overlay the density of beta for Model 2
  dens(b_2, col="red", add=TRUE)

  # Add a legend to distinguish the two priors
  legend("topright", legend=c("Prior 1 (beta ~ dnorm(0, 10))", "Prior 2 (beta ~ dnorm(0, 0.5))"), col=c("blue", "red"), lty=1)
  
} else {
  print("prior$b_gender is not in matrix form. Ensure it has two columns for gender categories.")
}


# # 
# # m_All <- ulam( 
# #     alist( 
# #     LUNG_CANCER ~ dbinom( 1 , p ) , 
# #     logit(p) <- a + b_gender[GENDER] + b_age*AGE + b_smoking[SMOKING] + 
# #                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
# #                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
# #                 b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + 
# #                 b_alcohol[ALCOHOL_CONSUMING] + b_coughing[COUGHING] + 
# #                 b_breath[SHORTNESS_OF_BREATH] + b_swallow[SWALLOWING_DIFFICULTY] + 
# #                 b_pain[CHEST_PAIN],
# #     a ~ dnorm( 0 , 1.5 )  ,
# #     b_gender[GENDER] ~ dnorm( 0, 0.5),
# #     b_age ~ dnorm( 0 , 0.5),
# #     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
# #     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
# #     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
# #     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
# #     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
# #     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
# #     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
# #     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
# #     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
# #     b_coughing[COUGHING] ~ dnorm(0, 0.5),
# #     b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
# #     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
# #     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
# #     ) ,
# #     data=d ,chains = 4,cores = 4, log_lik = TRUE)
# #     print("finish running m_ll")
#     # print(precis(m_All, depth = 2))
#     # plot(precis(m_All, depth = 2))
#      
# # m_1 <- ulam(                              # removing intercept a
# #     alist( 
# #     LUNG_CANCER ~ dbinom( 1 , p ) , 
# #     logit(p) <- b_gender[GENDER] + b_age*AGE + b_smoking[SMOKING] + 
# #                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
# #                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
# #                 b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + 
# #                 b_alcohol[ALCOHOL_CONSUMING] + b_coughing[COUGHING] + 
# #                 b_breath[SHORTNESS_OF_BREATH] + b_swallow[SWALLOWING_DIFFICULTY] + 
# #                 b_pain[CHEST_PAIN],
# #     b_gender[GENDER] ~ dnorm( 0, 0.5),
# #     b_age ~ dnorm( 0 , 0.5),
# #     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
# #     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
# #     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
# #     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
# #     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
# #     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
# #     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
# #     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
# #     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
# #     b_coughing[COUGHING] ~ dnorm(0, 0.5),
# #     b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
# #     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
# #     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
# #     ) ,
# #     data=d ,chains = 4,cores = 4, log_lik = TRUE )
# #     print("fininsh running m_1")
# #     # print(precis(m_1, depth = 2))
# #     # plot(precis(m_1, depth = 2))
# #      
# # m_2<- ulam(                                 # removed Smoking and intercept
# #     alist( 
# #     LUNG_CANCER ~ dbinom( 1 , p ) , 
# #     logit(p) <- b_gender[GENDER] + b_age*AGE + 
# #                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
# #                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
# #                 b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + 
# #                 b_alcohol[ALCOHOL_CONSUMING] + b_coughing[COUGHING] + 
# #                 b_breath[SHORTNESS_OF_BREATH] + b_swallow[SWALLOWING_DIFFICULTY] + 
# #                 b_pain[CHEST_PAIN],
# #     b_gender[GENDER] ~ dnorm( 0, 0.5),
# #     b_age ~ dnorm( 0 , 0.5),
# #     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
# #     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
# #     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
# #     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
# #     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
# #     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
# #     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
# #     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
# #     b_coughing[COUGHING] ~ dnorm(0, 0.5),
# #     b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
# #     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
# #     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
# #     ) ,
# #     data=d,chains = 4,cores = 4, log_lik = TRUE )
# #     print("finish running m_2")
# #     # print(precis(m_2, depth = 2))
# #     # plot(precis(m_2, depth = 2))
# # 
# # m_3 <- ulam(                           # removed age and intecept
# #     alist( 
# #     LUNG_CANCER ~ dbinom( 1 , p ) , 
# #     logit(p) <- b_gender[GENDER] + b_smoking[SMOKING] + 
# #                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
# #                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
# #                 b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + 
# #                 b_alcohol[ALCOHOL_CONSUMING] + b_coughing[COUGHING] + 
# #                 b_breath[SHORTNESS_OF_BREATH] + b_swallow[SWALLOWING_DIFFICULTY] + 
# #                 b_pain[CHEST_PAIN],
# #     b_gender[GENDER] ~ dnorm( 0, 0.5),
# #     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
# #     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
# #     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
# #     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
# #     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
# #     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
# #     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
# #     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
# #     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
# #     b_coughing[COUGHING] ~ dnorm(0, 0.5),
# #     b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
# #     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
# #     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
# #     ) ,
# #     data=d,chains = 4,cores = 4, log_lik = TRUE )
# #     print("finish running m_3")
# m_4 <- ulam(                                    # removed age and gender and intercept and allergy
#     alist( 
#     LUNG_CANCER ~ dbinom( 1 , p ) , 
#     logit(p) <- b_smoking[SMOKING] + 
#                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
#                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
#                 b_fatigue[FATIGUE] + b_wheezing[WHEEZING] + b_allergy[ALLERGY] +
#                 b_alcohol[ALCOHOL_CONSUMING] + 
#                 # b_coughing[COUGHING]  
#                 b_breath[SHORTNESS_OF_BREATH] + b_swallow[SWALLOWING_DIFFICULTY] + 
#                 b_pain[CHEST_PAIN],
#     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
#     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
#     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
#     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
#     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
#     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
#     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
#     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
#     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
#     # b_coughing[COUGHING] ~ dnorm(0, 0.5),
#     b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
#     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
#     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
#     ) ,
#     data=d,chains = 4,cores = 4, log_lik = TRUE )
#     print("fininsh running m_4")
#     print(precis(m_4, depth = 2))
#     plot(precis(m_4, depth = 2))
# 
# # m_5 <- ulam(                            # removed anxiety, gender ,age and intercept
# #     alist( 
# #     LUNG_CANCER ~ dbinom( 1 , p ) , 
# #     logit(p) <- b_smoking[SMOKING] + 
# #                 b_yellow[YELLOW_FINGERS] + b_wheezing[WHEEZING] + b_pressure[PEER_PRESSURE] +
# #                 b_chronic[CHRONIC_DISEASE] + b_allergy[ALLERGY] +
# #                 b_fatigue[FATIGUE] + b_alcohol[ALCOHOL_CONSUMING] + b_coughing[COUGHING] + 
# #                 b_breath[SHORTNESS_OF_BREATH] + b_swallow[SWALLOWING_DIFFICULTY] + 
# #                 b_pain[CHEST_PAIN],
# #     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
# #     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
# #     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
# #     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
# #     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
# #     b_allergy[ALLERGY] ~ dnorm(0,0.5),
# #     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
# #     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
# #     b_coughing[COUGHING] ~ dnorm(0, 0.5),
# #     b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
# #     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
# #     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
# #     ) ,
# #     data=d,chains = 4,cores = 4,log_lik = TRUE )
# #     print("fininsh running m_5")
# # # print(precis(m_5, depth = 2))
# # # plot(precis(m_5, depth = 2))    
# 
# m_6 <- ulam(        # removed age and gender and intercept and shortness of breath and allergy
#     alist( 
#     LUNG_CANCER ~ dbinom( 1 , p ) , 
#     logit(p) <- b_smoking[SMOKING] + 
#                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
#                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + b_allergy[ALLERGY] +
#                 b_fatigue[FATIGUE] + b_wheezing[WHEEZING] + 
#                 b_alcohol[ALCOHOL_CONSUMING] + 
#                 # b_coughing[COUGHING] 
#                 b_swallow[SWALLOWING_DIFFICULTY] + 
#                 b_pain[CHEST_PAIN],        
#     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
#     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
#     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
#     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
#     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
#     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
#     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
#     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
#     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
#     # b_coughing[COUGHING] ~ dnorm(0, 0.5),
#     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
#     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
#     ) ,
#     data=d ,chains = 4,cores = 4,log_lik = TRUE)
#     print("finish running m_6")
# 
# m_7 <- ulam(                                    # shortness of breath and allergy
#     alist( 
#     LUNG_CANCER ~ dbinom( 1 , p ) , 
#     logit(p) <- b_age*AGE + b_smoking[SMOKING] + 
#                 b_yellow[YELLOW_FINGERS] +
#                 b_anxiety[ANXIETY] + 
#                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
#                 b_fatigue[FATIGUE] + b_wheezing[WHEEZING] + b_allergy[ALLERGY] +
#                 b_alcohol[ALCOHOL_CONSUMING] + 
#                 #b_coughing[COUGHING] 
#                  b_swallow[SWALLOWING_DIFFICULTY] + 
#                 b_pain[CHEST_PAIN] ,            
#     b_age ~ dnorm(0,0.1),        
#     b_smoking[SMOKING] ~ dnorm( 0, 0.5),
#     b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
#     b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
#     b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
#     b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
#     b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
#     b_allergy[ALLERGY] ~ dnorm(0, 0.5),
#     b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
#     b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
#     # b_coughing[COUGHING] ~ dnorm(0, 0.5),
#     b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
#     b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
#     ) ,
#     data=d ,chains = 4,cores = 4, log_lik = TRUE)    
#     print("fininsh running m_7")
#     # print(precis(m_7, depth = 2))
# 
#     # plot(precis(m_7, depth = 2))
# 
# print(compare(m_4,m_6,m_7))
# plot(compare(m_4,m_6,m_7))    
# # traceplot(m_7)
# # trankplot(m_7)
# 
#  plot(coeftab(m_4,m_6,m_7), par=c("b_coughing[1]","b_coughing[2]","b_smoking[1]","b_smoking[2]","b_yellow[1]","b_yellow[2]",
#  "b_anxiety[1]","b_anxiety[2]","b_pressure[1]","b_pressure[2]","b_chronic[1]","b_chronic[2]","b_fatigue[1]","b_fatigue[2]",
#  "b_allergy[1]","b_allergy[2]","b_wheezing[1]","b_wheezing[2]","b_alcohol[1]","b_alcohol[2]","b_swallow[1]","b_swallow[2]",
#  "b_pain[1]","b_pain[2]"
#  ))
# 
#      
# 
#      
# 
# 