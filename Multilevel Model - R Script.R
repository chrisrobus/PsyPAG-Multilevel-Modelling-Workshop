# First, let's download the packages we will need for the workshop (you won't need to do this every time you load R).
install.packages("lme4")
install.packages("curl")

# Then, we'll load them into R Studio so we can use them.
library(lme4)
library(curl)

# Next, we need to load our data from Github.
data<-read.csv(curl("https://raw.githubusercontent.com/chrisrobus/PsyPAG-Multilevel-Modelling-Workshop/main/Multilevel%20Modelling%20-%20Workshop%20Data.csv"))
subj<-data$X.U.FEFF.subj


                          ### Example 1: Mixed-model with one RM fixed factor ###

# Step 1: Test for inclusion of random intercepts in model using LRTs.

lm1<-lmer(Y~(1|subj)+(1|item),data=data,REML=FALSE) #Full intercept model (i.e. both random effects)

lm2<-lmer(Y~(1|subj),data=data,REML=FALSE) # Reduced model: By-items intercept removed

lm3<-lmer(Y~(1|item),data=data,REML=FALSE) # Reduced model: By-subjects intercept removed


anova(lm1,lm2) # Compare full intercept model to reduced (testing by-items intercept)

anova(lm1,lm3) # Compare full intercept model to reduced (testing by-subjects intercept)


# Step 2: Test for inclusion of random slopes in model using LRTs

lm4<-lmer(Y~(1+X|subj),data=data,REML=FALSE) # Inclusion of X-by-subject slope

lm5<-lmer(Y~(1|subj)+(X|item),data=data,REML=FALSE) # Inclusion of X-by-items slope

# If these had converged, we would then use the 'anova' function to compare them to the intercept-only model via LRTs.


# Step 3: Testing fixed effect

lm6<-lmer(Y~X+(1|subj),data=data,REML=FALSE) # Full model with data-driven random effects structure

summary(lm6) # Get a summary of model parameters for the full LMM

anova(lm6,lm2) # Get a p-value for fixed effect by comparing full model to reduced model without the fixed factor.

                          



                          ### Example 2: Factorial designs ###

# Step 1: Test for includion of random intercepts in model using LRTs.

lmf1<-lmer(Y~(1|subj)+(1|item),data=data,REML=FALSE) # Full intercept model

lmf2<-lmer(Y~(1|subj),data=data,REML=FALSE) #Reduced model (removed by-item intercept)

lmf3<-lmer(Y~(1|item),data=data,REML=FALSE) #Reduced model (removed by-subjects intercept)


anova(lmf1,lmf2) # Compare full intercept model to reduced (testing by-items intercept)

anova(lmf1,lmf3) # Compare full intercept model to reduced (testing by-subjects intercept)
  

# Step 2: Test for inclusion of random slopes in model using LRTs

lmf4<-lmer(Y~(1+X1|subj),data=data,REML=FALSE) # X1-by-subjects slope

lmf5<-lmer(Y~(1|subj)+(X1|item),data=data,REML=FALSE) # X1-by-items slope

lmf6<-lmer(Y~(1+X2|subj),data=data,REML=FALSE) # X2-by-subjects slope

lmf7<-lmer(Y~(1|subj)+(X2|item),data=data,REML=FALSE) # X2-by-items slope


# Step 3: Testing fixed effect

lmf8<-lmer(Y~X1*X2+(1|subj),data=data,REML=FALSE) # Full model with interaction effect included

summary(lmf8) # Summary of model parameters for full model

lmf9<-lmer(Y~X2+X1:X2+(1|subj), data=data, REML=FALSE) # Reduced model: Removed X1

anova(lmf8,lmf9) # LRT to test fixed effect of X1

lmf10<-lmer(Y~X1+X1:X2+(1|subj), data=data, REML=FALSE) # Reduced model: Removed X2

anova(lmf8,lmf10) # LRT to test fixed effect of X2

lmf11<-lmer(Y~X1+X2+(1|subj), data=data, REML=FALSE) # Reduced model: Removed X1 x X2 interaction

anova(lmf8,lmf11) # LRT to test interaction effect

                          



                          ### Example 3: Generalised linear mixed model (binomial response) ###


# Step 1: Test for includion of random intercepts in model using LRTs.

glm1<-glmer(Y2~(1|subj)+(1|item),data=data,family=binomial)

glm2<-glmer(Y2~(1|subj),data=data,family=binomial)

glm3<-glmer(Y2~(1|item),data=data,family=binomial) 

anova(glm1,glm2)

anova(glm1,glm3)


# Step 2: Test for inclusion of random slopes in model using LRTs

glm4<-glmer(Y2~(1+X1|subj),data=data,family=binomial)

glm5<-glmer(Y2~(1|subj)+(X1|item),data=data,family=binomial)

glm6<-glmer(Y2~(1+X2|subj),data=data,family=binomial)

glm7<-glmer(Y2~(1|subj)+(X2|item),data=data,family=binomial)

anova(glm4,glm2) #Check if slopes model differs from intercept-only model

anova(glm7,glm2) #Check if slopes model differs from intercept-only model


# Step 3: Testing fixed effect

glm8<-glmer(Y2~X1*X2+(1|subj)+(X2|item),data=data,family=binomial)

glm8<-glmer(Y2~X1*X2+(1|subj)+(X2|item),data=data,family=binomial,
          control=glmerControl(optimizer="bobyqa")) #Introduce an optimizer to see if this resolves issue.

summary(glm8) # Check random structure to find the culprit

glm9<-glmer(Y2~X1*X2+(1|subj),data=data,family=binomial)
summary(glm9)

glm10<-glmer(Y2~X2+X1:X2+(1|subj),data=data,family=binomial)
anova(glm9,glm10)

glm11<-glmer(Y2~X1+X1:X2+(1|subj),data=data,family=binomial)
anova(glm9,glm11)

glm12<-glmer(Y2~X1+X2+(1|subj),data=data,family=binomial)
anova(glm9,glm12)
