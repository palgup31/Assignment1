library(rethinking)
d <- read.csv("https://www.torkar.se/data-slc.csv")

d$GENDER <- as.integer(ifelse(d$GENDER == "M" , 2 , 1))
d$LUNG_CANCER <- as.integer(ifelse(d$LUNG_CANCER == "YES" , 1 , 0))
d$AGE <- (d$AGE - mean(d$AGE)) / sd(d$AGE)
colnames(d)[colnames(d) %in% c("CHRONIC.DISEASE", "ALCOHOL.CONSUMING", 
"SHORTNESS.OF.BREATH", "SWALLOWING.DIFFICULTY",
"CHEST.PAIN")] <- c("CHRONIC_DISEASE", "ALCOHOL_CONSUMING",
 "SHORTNESS_OF_BREATH", "SWALLOWING_DIFFICULTY", "CHEST_PAIN")
m_lifestyle <- quap( 
alist( 
LUNG_CANCER ~ dbinom( 1 , p ) ,  
logit(p) <- b_smoking[SMOKING] + b_alcohol[ALCOHOL_CONSUMING],
# a ~ dnorm( 0 , 1.5 ),
b_smoking[SMOKING] ~ dnorm( 0, 0.5),
b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5)
) ,
data=d )

m_lifestyle1 <- quap( 
alist( 
LUNG_CANCER ~ dbinom( 1 , p ) ,  
logit(p) <- a + b_smoking[SMOKING] + b_alcohol[ALCOHOL_CONSUMING],
a ~ dnorm( 0 , 1.5 ),
b_smoking[SMOKING] ~ dnorm( 0, 0.5),
b_alcohol[ALCOHOL_CONSUMING] ~ dnorm(0, 0.5)
) ,
data=d )

m_symptoms <- quap(
    alist(
       LUNG_CANCER ~ dbinom(1,p),
       logit(p) ~ b_gender[GENDER] + b_age*AGE + 
                b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
                b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
                b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + b_coughing[COUGHING] + b_swallow[SWALLOWING_DIFFICULTY] + 
                b_pain[CHEST_PAIN], 
    # a ~ dnorm( 0 , 1.5 )  ,
    b_gender[GENDER] ~ dnorm( 0, 0.5),
    b_age ~ dnorm( 0 , 0.5),
    b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
    b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
    b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
    b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
    b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
    b_allergy[ALLERGY] ~ dnorm(0, 0.5),
    b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
    b_coughing[COUGHING] ~ dnorm(0, 0.5),
    # b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
    b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
    b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
    ) , 
    data = d
)

m_symptoms1 <- quap(
    alist(
       LUNG_CANCER ~ dbinom(1,p),
       logit(p) ~ a + b_gender[GENDER] + b_age*AGE + 
                b_yellow[YELLOW_FINGERS] + b_anxiety[ANXIETY] + 
                b_pressure[PEER_PRESSURE] + b_chronic[CHRONIC_DISEASE] + 
                b_fatigue[FATIGUE] + b_allergy[ALLERGY] + b_wheezing[WHEEZING] + b_coughing[COUGHING] + b_swallow[SWALLOWING_DIFFICULTY] + 
                b_pain[CHEST_PAIN], 
    a ~ dnorm( 0 , 1.5 )  ,
    b_gender[GENDER] ~ dnorm( 0, 0.5),
    b_age ~ dnorm( 0 , 0.5),
    b_yellow[YELLOW_FINGERS] ~ dnorm(0, 0.5),
    b_anxiety[ANXIETY] ~ dnorm(0, 0.5),
    b_pressure[PEER_PRESSURE] ~ dnorm(0, 0.5),
    b_chronic[CHRONIC_DISEASE] ~ dnorm(0, 0.5),
    b_fatigue[FATIGUE] ~ dnorm(0, 0.5),
    b_allergy[ALLERGY] ~ dnorm(0, 0.5),
    b_wheezing[WHEEZING] ~ dnorm(0, 0.5),
    b_coughing[COUGHING] ~ dnorm(0, 0.5),
    # b_breath[SHORTNESS_OF_BREATH] ~ dnorm(0, 0.5),
    b_swallow[SWALLOWING_DIFFICULTY] ~ dnorm(0, 0.5),
    b_pain[CHEST_PAIN] ~ dnorm(0, 0.5)
    ) , 
    data = d
)
# print(precis(m_symptoms, depth = 2))
# plot(precis(m_symptoms, depth = 2))
plot(compare(m_lifestyle,m_symptoms,m_lifestyle1,m_symptoms1))