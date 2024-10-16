library(rethinking)
d <- read.csv("https://www.torkar.se/data-slc.csv")

d <- d[d$AGE >= 25,]
d$GENDER <- as.integer(ifelse(d$GENDER == "M" , 2 , 1))
d$LUNG_CANCER <- as.integer(ifelse(d$LUNG_CANCER == "YES" , 1 , 0))
d$AGE <- standardize(d$AGE)
colnames(d)[colnames(d) %in% c("CHRONIC.DISEASE", "ALCOHOL.CONSUMING",
                               "SHORTNESS.OF.BREATH", "SWALLOWING.DIFFICULTY",
                               "CHEST.PAIN")] <- c("CHRONIC_DISEASE",
                                                   "ALCOHOL_CONSUMING",
                                                   "SHORTNESS_OF_BREATH",
                                                   "SWALLOWING_DIFFICULTY",
                                                   "CHEST_PAIN")

# m_1 <- quap(  # checking only with intercept
#     alist(
#     LUNG_CANCER ~ dbinom(1, p),
#     logit(p) <- a + b_gender[GENDER],
#     a ~ dnorm(0,1.6),
#     b_gender[GENDER]~dnorm(0,0.5)
#     ),
#     data = d,
# )
# print(precis(m_1))
# prior <- extract.prior(m_1, n=1e4)
# p <- sapply(1:2, function(k) inv_logit(prior$a + prior$b_gender[,k]))
# dens(abs(p[,1]-p[,2]))
#----------------------------------------------------------------------------
m_All <- ulam(   # included all variables
    alist( 
    LUNG_CANCER ~ dbinom(1, p),
    logit(p) <- a + b_gender[GENDER] + b_age*AGE + b_smoking[SMOKING] +
                b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] +
                b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] +
                b_fatigue[FATIGUE] + b_allergy[ALLERGY] +
                b_wheezing[WHEEZING] + b_alcohol[ALCOHOL_CONSUMING] +
                b_coughing[COUGHING] + b_breath[SHORTNESS_OF_BREATH] +
                b_swallow[SWALLOWING_DIFFICULTY] +
                b_pain[CHEST_PAIN],
                a ~ dnorm( 0 , 1.4),
                b_gender[GENDER] ~ dnorm( 0, 0.5),
                b_age ~ dnorm( 0 , 0.5),
                b_smoking[SMOKING] ~ dnorm( 0, 0.5),
                b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
                b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
                b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
                b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
                b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
                b_allergy[ALLERGY] ~ dnorm(0, 0.5),
                b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
                b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
                b_coughing[COUGHING] ~ dnorm(0, 0.5),
                b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
                b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
                b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
              ),
              data = d, chains = 4, cores = 4, log_lik = TRUE)

print("finish running m_ll")
print(precis(m_All, depth = 2))
# plot(precis(m_All, depth = 2))


#-----------------------------------------------------------------------------

# m_4 <- ulam(          # removed age,gender,intercept
#     alist(
#               LUNG_CANCER ~ dbinom(1, p),
#               logit(p) <- b_smoking[SMOKING] +
#                 b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] +
#                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] +
#                 b_fatigue[FATIGUE] + b_wheezing[WHEEZING] + b_allergy[ALLERGY] +
#                 b_alcohol[ALCOHOL_CONSUMING] +
#                 b_coughing[COUGHING] +
#                 b_breath[SHORTNESS_OF_BREATH] +
#                 b_swallow[SWALLOWING_DIFFICULTY] +
#                 b_pain[CHEST_PAIN],
#               b_smoking[SMOKING] ~ dnorm(0, 0.5),
#               b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
#               b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
#               b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
#               b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
#               b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
#               b_allergy[ALLERGY] ~ dnorm(0, 0.5),
#               b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
#               b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
#               b_coughing[COUGHING] ~ dnorm(0, 0.5),
#               b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
#               b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
#               b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
#             ),
#             data = d, chains = 4, cores = 4, log_lik = TRUE)
# print("fininsh running m_4")
# print(precis(m_4, depth = 2))
# plot(precis(m_4, depth = 2))

#-----------------------------------------------------------------------------

m_6 <- ulam(   # removed age,gender,intercept,shortness of breath, anxiety, chest pain
            alist( 
              LUNG_CANCER ~ dbinom(1, p),
              logit(p) <- a + b_smoking[SMOKING] +
                b_yellow[YELLOW_FINGERS] + 
                # b_anxiety[ANXIETY] +
                b_pressure[PEER_PRESSURE] +
                b_chronic[CHRONIC_DISEASE] +
                b_allergy[ALLERGY] + b_fatigue[FATIGUE] +
                b_wheezing[WHEEZING] +
                b_alcohol[ALCOHOL_CONSUMING] +
                b_coughing[COUGHING] +
                b_swallow[SWALLOWING_DIFFICULTY],
                # b_pain[CHEST_PAIN],
              a ~ dnorm( 0 , 1.5),
              b_smoking[SMOKING] ~ dnorm( 0, 0.5),
              b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
              # b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
              b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
              b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
              b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
              b_allergy[ALLERGY] ~ dnorm(0, 0.5),
              b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
              b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
              b_coughing[COUGHING] ~ dnorm(0, 0.5),
              b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5)
              # b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
            ) ,
            data = d, chains = 4, cores = 4, log_lik = TRUE)
print("finish running m_6")
# print(PSIS(m_6, pointwise = TRUE))
print(precis(m_6, depth = 2))
# print(WAIC(m_6))
# print(PSIS(m_6))
# plot(precis(m_6, depth = 2))
# print("hello")


#-----------------------------------------------------------------------------

# m_7 <- ulam(           # removed intercept,gender,shortness of breath
#             alist(
#               LUNG_CANCER ~ dbinom(1, p),
#               logit(p) <- b_age * AGE + b_smoking[SMOKING] +
#                 b_yellow[YELLOW_FINGERS] +
#                 b_anxiety[ANXIETY] +
#                 b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] +
#                 b_fatigue[FATIGUE] + b_wheezing[WHEEZING] + b_allergy[ALLERGY] +
#                 b_alcohol[ALCOHOL_CONSUMING] +
#                 b_coughing[COUGHING] +
#                 b_swallow[SWALLOWING_DIFFICULTY] +
#                 b_pain[CHEST_PAIN],
#               b_age ~ dnorm(0, 0.5),
#               b_smoking[SMOKING] ~ dnorm(0, 0.5),
#               b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
#               b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
#               b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
#               b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
#               b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
#               b_allergy[ALLERGY] ~ dnorm(0, 0.5),
#               b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
#               b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5),
#               b_coughing[COUGHING] ~ dnorm(0, 0.5),
#               b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
#               b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
#             ) ,
#             data = d, chains = 4, cores = 4, log_lik = TRUE)
# print("fininsh running m_7")
# print(precis(m_7, depth = 2))
# plot(precis(m_7, depth = 2))

print("compare result")
print(compare(m_6, m_All, func = WAIC))
# # plot(compare(m_6, m_All))
traceplot(m_6)
# trankplot(m_6)
# 
# plot(coeftab(m_All, m_6), par = c("a","b_age",
#                                      "b_coughing[1]", "b_coughing[2]",
#                                      "b_smoking[1]", "b_smoking[2]",
#                                      "b_yellow[1]", "b_yellow[2]",
#                                      "b_anxiety[1]", "b_anxiety[2]",
#                                      "b_pressure[1]", "b_pressure[2]",
#                                      "b_chronic[1]", "b_chronic[2]",
#                                      "b_fatigue[1]", "b_fatigue[2]",
#                                      "b_allergy[1]", "b_allergy[2]",
#                                      "b_wheezing[1]", "b_wheezing[2]",
#                                      "b_alcohol[1]", "b_alcohol[2]",
#                                      "b_swallow[1]", "b_swallow[2]",
#                                      "b_pain[1]", "b_pain[2]"))