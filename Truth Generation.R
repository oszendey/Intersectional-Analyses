################################################################################
##############                 Truth Generation                 ################
################################################################################




#Set Distribution for Truth Coefficients 
library(truncnorm)
#bimodal dist for coeff
Sample.negative.coeff <- function (n) {
  y0 <- rtruncnorm(n,a=-2.0, b=-0.20, mean=-1, sd = 1)
} 

Sample.positive.coeff <- function (n) {
  y0 <- rtruncnorm(n,a=.20, b=2.0, mean=1, sd = 1)
}

###############################################################################

####Two Categories######

#Set seeds, each seed was randomly generated using: sample(1:10000,6 ) 
s1<-4186 
s2<-2925 
s3<-5333  
s4<-2931 
s5<-3474


#Randomly sample from distributions for truth coefficients 
bA0B0<-0
set.seed(s1)
bA1B0<-Sample.positive.coeff(1)
set.seed(s2)
bA2B0<-Sample.negative.coeff(1)
set.seed(s3)
bA3B0<-Sample.negative.coeff(1)
bA4B0<-0
bA5B0<-0
bA6B0<-0
bA0B1<- 0
set.seed(s4)
bA1B1<-Sample.negative.coeff(1)
set.seed(s5)
bA2B1<-Sample.positive.coeff(1)
bA3B1<-0
bA4B1<-0
bA5B1<-0
bA6B1<-0


#Build Dataset

A<- c(0, 1, 2, 3, 4, 5, 6)
B<- c(0, 1)
L1<-expand.grid(A=A, B=B)
L1$cluster <-factor(100*(1+L1$A)+ 10*(1+L1$B))


L1$A1 <- ifelse(L1$A==1, 1, 0)
L1$A2<- ifelse(L1$A==2, 1, 0)
L1$A3 <- ifelse(L1$A==3, 1, 0)
L1$A4 <- ifelse(L1$A==4, 1, 0)
L1$A5 <- ifelse(L1$A==5, 1, 0)
L1$A6 <- ifelse(L1$A==6, 1, 0)
L1$A0 <- ifelse(L1$A==0,1,0)
L1$B0<-ifelse(L1$B==0, 1, 0)
L1$B1<-ifelse(L1$B==1, 1, 0)

L1$A0B1 <- ifelse(L1$A == 0 & L1$B == 1, 1, 0)
L1$A1B1 <- ifelse(L1$A == 1 & L1$B == 1, 1, 0)
L1$A2B1 <- ifelse(L1$A == 2 & L1$B == 1, 1, 0)
L1$A3B1 <- ifelse(L1$A == 3 & L1$B == 1, 1, 0)
L1$A4B1 <- ifelse(L1$A == 4 & L1$B == 1, 1, 0)
L1$A5B1 <- ifelse(L1$A == 5 & L1$B == 1, 1, 0)
L1$A6B1 <- ifelse(L1$A == 6 & L1$B == 1, 1, 0)
L1$A0B0 <- ifelse(L1$A == 0 & L1$B == 0, 1, 0)
L1$A1B0 <- ifelse(L1$A == 1 & L1$B == 0, 1, 0)
L1$A2B0 <- ifelse(L1$A == 2 & L1$B == 0, 1, 0)
L1$A3B0 <- ifelse(L1$A == 3 & L1$B == 0, 1, 0)
L1$A4B0 <- ifelse(L1$A == 4 & L1$B == 0, 1, 0)
L1$A5B0 <- ifelse(L1$A == 5 & L1$B == 0, 1, 0)
L1$A6B0 <- ifelse(L1$A == 6 & L1$B == 0, 1, 0)


#Build Level two out
# Define the number of schools
n.schools <- 100

# Initialize an empty dataframe to store the results
Truth <- data.frame()

# Loop through each school
for (i in 1:n.schools) {
  
  # Replicate the structure of Truth for the current school
  df <- L1
  df$school_ID <- i
  
  # Add the replicated data to the results dataframe
  Truth <- rbind(Truth, df)
}

#Set the outcome variable



#Sort by school_ID 
Truth<- Truth[order(Truth$school_ID),]

# To obtain ICC around .20 the random intercept variance is set as 0.25 and the residual variance is 1

u<- rep(rnorm(100,0,.5), each = 14)
ei<- rnorm(1400, 0, 1)


#GenerateOutcome Variable 
Truth$y<- bA0B1*Truth$A0B1+ bA1B1*Truth$A1B1+bA2B1*Truth$A2B1+
  bA3B1*Truth$A3B1+ bA4B1*Truth$A4B1+ bA5B1*Truth$A5B1+
  bA6B1*Truth$A6B1 +bA1B0*Truth$A1B0 + bA2B0*Truth$A2B0 +
  bA3B0*Truth$A3B0 +bA4B0*Truth$A4B0+ bA5B0*Truth$A5B0 +
  bA6B0*Truth$A6B0+ u+ ei 

#Determine Truth for Intersectional Strata by computing cluster averages 
Truth$clusteraverage<-ave(Truth$y, Truth$cluster)

#Set up Unique Cluster for Data Generation

Truth$cluster<-as.character(Truth$cluster)
Truth$school_ID<-as.character(Truth$school_ID)

Truth$cluster<-as.numeric(Truth$cluster)
Truth$school_ID<-as.numeric(Truth$school_ID)

Truth$UniqueCluster<-Truth$cluster+Truth$school_ID



#Check Null Model to Check ICC
library(lme4)

NullModel<-lmer(y~ (1 | school_ID), data= Truth)

icc <- VarCorr(NullModel)$school_ID / (VarCorr(NullModel)$school_ID + sigma(NullModel)^2)
icc
################################################################################

####Three Categories######

#Set seeds, each seed was randomly generated using: sample(1:10000,12) 


S1<-8540 
S2<-5048
S3<-591 
S4<-4163 
S5<-7131
S6<-4184
S7<-2770
S8<-5153
S9<-5178
S10<-603
S11<-2422
S12<-4817

#Randomly sample from distributions for truth coefficients 

bA0B0C0<-0
set.seed(S1)
bA1B0C0<-Sample.positive.coeff(1)
set.seed(S2)
bA2B0C0<-Sample.positive.coeff(1)
set.seed(S3)
bA3B0C0<-Sample.negative.coeff(1) 
bA4B0C0<-0
bA5B0C0<-0
bA6B0C0<-0
bA0B1C0<-0
bA1B1C0<-0
set.seed(S4)
bA2B1C0<-Sample.positive.coeff(1)
bA3B1C0<-0
bA4B1C0<-0
bA5B1C0<-0
bA6B1C0<-0
bA0B0C1<-0
bA1B0C1<-0
set.seed(S5)
bA2B0C1<-Sample.positive.coeff(1)
set.seed(S6)
bA3B0C1<-Sample.negative.coeff
bA4B0C1<-0
bA5B0C1<-0
bA6B0C1<-0
bA0B1C1<-0
bA1B1C1<-0
set.seed(S7)
bA2B1C1<-Sample.positive.coeff(1)
bA3B1C1<-0
bA4B1C1<-0
bA5B1C1<-0
set.seed(S8)
bA6B1C1<-Sample.negative.coeff(1)
bA0B0C2<-0
bA1B0C2<-0
bA2B0C2<-0
bA3B0C2<-0
bA4B0C2<-0
set.seed(S9)
bA5B0C2<-Sample.negative.coeff(1)
bA6B0C2<-0
bA0B1C2<-0
set.seed(S10)
bA1B1C2<-Sample.negative.coeff(1)
bA2B1C2<-0
set.seed(S11)
bA3B1C2<-Sample.negative.coeff(1)
bA4B1C2<-0
bA5B1C2<-0
set.seed(S12)
bA6B1C2<-Sample.negative.coeff(1)

#Build Dataset

A<- c(0, 1, 2, 3, 4, 5, 6)
B<- c(0, 1)
C<- c(0,1,2)
L1<-expand.grid(A=A, B=B, C=C)
L1$cluster <-factor(100*(1+L1$A)+ 10*(1+L1$B)+ 1*(1+L1$C))


L1$A1 <- ifelse(L1$A==1, 1, 0)
L1$A2<- ifelse(L1$A==2, 1, 0)
L1$A3 <- ifelse(L1$A==3, 1, 0)
L1$A4 <- ifelse(L1$A==4, 1, 0)
L1$A5 <- ifelse(L1$A==5, 1, 0)
L1$A6 <- ifelse(L1$A==6, 1, 0)
L1$A0 <- ifelse(L1$A==0,1,0)
L1$B0<-ifelse(L1$B==0, 1, 0)
L1$B1<-ifelse(L1$B==1, 1, 0)
L1$C0<-ifelse(L1$C==0, 1, 0)
L1$C1<-ifelse(L1$C==1, 1, 0)
L1$C2<-ifelse(L1$C==2, 1, 0)


L1$A0B0C0 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A1B0C0 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A2B0C0 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A3B0C0 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A4B0C0 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A5B0C0 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A6B0C0 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A0B1C0 <- ifelse(L1$A == 0 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A1B1C0 <- ifelse(L1$A == 1 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A2B1C0 <- ifelse(L1$A == 2 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A3B1C0 <- ifelse(L1$A == 3 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A4B1C0 <- ifelse(L1$A == 4 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A5B1C0 <- ifelse(L1$A == 5 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A6B1C0 <- ifelse(L1$A == 6 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A0B0C0 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A1B0C0 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A2B0C0 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A3B0C0 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A4B0C0 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A5B0C0 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A6B0C0 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 0, 1, 0)


L1$A0B0C1 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A1B0C1 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A2B0C1 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A3B0C1 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A4B0C1 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A5B0C1 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A6B0C1 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A0B1C1 <- ifelse(L1$A == 0 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A1B1C1 <- ifelse(L1$A == 1 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A2B1C1 <- ifelse(L1$A == 2 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A3B1C1 <- ifelse(L1$A == 3 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A4B1C1 <- ifelse(L1$A == 4 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A5B1C1 <- ifelse(L1$A == 5 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A6B1C1 <- ifelse(L1$A == 6 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A0B0C1 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A1B0C1 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A2B0C1 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A3B0C1 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A4B0C1 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A5B0C1 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A6B0C1 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 1, 1, 0)



L1$A0B0C2 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A1B0C2 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A2B0C2 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A3B0C2 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A4B0C2 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A5B0C2 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A6B0C2 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A0B1C2 <- ifelse(L1$A == 0 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A1B1C2 <- ifelse(L1$A == 1 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A2B1C2 <- ifelse(L1$A == 2 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A3B1C2 <- ifelse(L1$A == 3 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A4B1C2 <- ifelse(L1$A == 4 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A5B1C2 <- ifelse(L1$A == 5 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A6B1C2 <- ifelse(L1$A == 6 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A0B0C2 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A1B0C2 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A2B0C2 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A3B0C2 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A4B0C2 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A5B0C2 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A6B0C2 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 2, 1, 0)



#Build Level two out
# Define the number of schools
n.schools <- 100

# Initialize an empty dataframe to store the results
Truth <- data.frame()

# Loop through each school
for (i in 1:n.schools) {
  
  # Replicate the structure of Truth for the current school
  df <- L1
  df$school_ID <- i
  
  # Add the replicated data to the results dataframe
  Truth <- rbind(Truth, df)
}

#Set the outcome variable



#Sort by school_ID 
Truth<- Truth[order(Truth$school_ID),]

# To obtain ICC around .20 the random intercept variance is set as 0.25 and the residual variance is 1

u<- rep(rnorm(100,0,.5), each = 42)
ei<- rnorm(4200, 0, 1)


#GenerateOutcome Variable 
Truth$y<- bA0B1C0*Truth$A0B1C0+ bA1B1C0*Truth$A1B1C0+bA2B1C0*Truth$A2B1C0+
  bA3B1C0*Truth$A3B1C0+ bA4B1C0*Truth$A4B1C0+ bA5B1C0*Truth$A5B1C0+
  bA6B1C0*Truth$A6B1C0 +bA1B0C0*Truth$A1B0C0 + bA2B0C0*Truth$A2B0C0 +
  bA3B0C0*Truth$A3B0C0 +bA4B0C0*Truth$A4B0C0+ bA5B0C0*Truth$A5B0C0 +
  bA6B0C0*Truth$A6B0C0 + 
  bA0B1C1*Truth$A0B1C1+ bA1B1C1*Truth$A1B1C1+bA2B1C1*Truth$A2B1C1+
  bA3B1C1*Truth$A3B1C1+ bA4B1C1*Truth$A4B1C1+ bA5B1C1*Truth$A5B1C1+
  bA6B1C1*Truth$A6B1C1 +bA1B0C1*Truth$A1B0C1 + bA2B0C1*Truth$A2B0C1 +
  bA3B0C1*Truth$A3B0C1 +bA4B0C1*Truth$A4B0C1+ bA5B0C1*Truth$A5B0C1 +
  bA6B0C1*Truth$A6B0C1 + bA0B0C1*Truth$A0B0C1 +
  bA0B1C2*Truth$A0B1C2+ bA1B1C2*Truth$A1B1C2+bA2B1C2*Truth$A2B1C2+
  bA3B1C2*Truth$A3B1C2+ bA4B1C2*Truth$A4B1C2+ bA5B1C2*Truth$A5B1C2+
  bA6B1C2*Truth$A6B1C2 +bA1B0C2*Truth$A1B0C2 + bA2B0C2*Truth$A2B0C2 +
  bA3B0C2*Truth$A3B0C2 +bA4B0C2*Truth$A4B0C2+ bA5B0C2*Truth$A5B0C2 +
  bA6B0C2*Truth$A6B0C2 +bA0B0C2*Truth$A0B0C2 + u+ ei 

#Determine Truth for Intersectional Strata by computing cluster averages 
Truth$clusteraverage<-ave(Truth$y, Truth$cluster)

#Set up Unique Cluster for Data Generation

Truth$cluster<-as.character(Truth$cluster)
Truth$school_ID<-as.character(Truth$school_ID)

Truth$cluster<-as.numeric(Truth$cluster)
Truth$school_ID<-as.numeric(Truth$school_ID)

Truth$UniqueCluster<-Truth$cluster+Truth$school_ID



#Check Null Model to Check ICC
library(lme4)

NullModel<-lmer(y~ (1 | school_ID), data= Truth)
icc <- VarCorr(NullModel)$school_ID / (VarCorr(NullModel)$school_ID + sigma(NullModel)^2)
icc


