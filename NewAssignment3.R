# early symptoms - fatigue , yellow fingers, anxiety, coughing ,
# allergy,chronic disease,wheezing
# advanced symptoms - chest pain, swallowing dificulty,
library(rethinking)
d <- read.csv("https://www.torkar.se/data-slc.csv")
d$GENDER <- as.integer(ifelse(d$GENDER == "M", 2, 1))
d$LUNG_CANCER <- as.integer(ifelse(d$LUNG_CANCER == "YES", 1, 0))
d$AGE <- standardize(d$AGE)
colnames(d)[colnames(d) %in% c("CHRONIC.DISEASE", "ALCOHOL.CONSUMING", "SHORTNESS.OF.BREATH", "SWALLOWING.DIFFICULTY", "CHEST.PAIN")] <- c("CHRONIC_DISEASE", "ALCOHOL_CONSUMING", "SHORTNESS_OF_BREATH", "SWALLOWING_DIFFICULTY", "CHEST_PAIN")

m_early <- ulam(       # age and shortness of breath
    alist( 
    LUNG_CANCER ~ dbinom( 1 , p ) , 
    logit(p) <- a + b_yellow[YELLOW_FINGERS] +
                b_anxiety[ANXIETY] + b_chronic[CHRONIC_DISEASE] + 
                b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + b_coughing[COUGHING] , 
    a ~ dnorm(0,1.5),
    b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
    b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
    b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
    b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
    b_allergy[ALLERGY] ~ dnorm(0, 0.5),
    b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
    b_coughing[COUGHING] ~ dnorm(0, 0.5)
    ) ,
    data=d ,chains = 4,cores = 4, log_lik = TRUE)

m_advanced <- ulam(                     # age and shortness of breath
    alist( 
    LUNG_CANCER ~ dbinom( 1 , p ) , 
    logit(p) <- a + b_swallow[SWALLOWING_DIFFICULTY] +
                       b_pain[CHEST_PAIN], 
                    a ~ dnorm(0, 1.5),
                    b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
                    b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
    ),
    data = d, chains = 4, cores = 4, log_lik = TRUE)

print(compare(m_early,m_advanced))
plot(compare(m_early,m_advanced))

# plot(coeftab(m_earlysym,m_late), par=c("b_swallow[1]","b_swallow[2]",
# "b_pain[1]","b_pain[2]"
# ))