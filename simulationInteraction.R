
library(BayesFactor)
library(dplyr)
library (ggplot2)

#I will upload our original data after the paper is accepted, for now let's simulate some fake data:

# Set coefficients
alpha = 1
beta1 = .3
beta2 = -.5
beta3 = -1.1

# Generate 100 trials
A = c(rep(c(0), 50), rep(c(1), 50)) # '0' 100 times, '1' 100 times
B = rep(c(rep(c(0), 25), rep(c(1), 25)), 2) # '0'x50, '1'x50, '0'x50, '1'x50
e = rnorm(100, 0, sd=1) # Random noise, with standard deviation of 1

# Generate your data using the regression equation
y = alpha + beta1*A + beta2*B + beta3*A*B + e

# Join the variables in a data frame
data = data.frame(cbind(A, B, y))

##change names and values to fit my actual data
names(data)<-c("cond", "exp", "dif")
data$cond <- as.character(data$cond )
data$cond [data$cond  == "0"] <- "noun_cond"
data$exp <- as.character(data$exp )
data$exp [data$exp  == "0"] <- "old"
data$cond [data$cond  == "1"] <- "verb_cond"
data$exp [data$exp  == "1"] <- "new"
data$exp <- as.factor(data$exp)
data$cond<-as.factor(data$cond)
summary(data)


##set some empty objects to fill in with simulated results
df = data ##I am saving the original data
data = df ## but creating a duplicate of it. The duplicate will grow while the original data remains untouched.

##creating some empy objects to fill in later
result = data.frame(BF=numeric(), sample = numeric())
interactionBF = data.frame(BF=1, sample = 1)

##here is the lop that is going to resample participants and run analyses on each new sample
repeat {
for (i in 1:20){ #I add 20 batches of new paerticipants, sampled with replacment from my data
  # For each sample I fit a regression. 
  model = generalTestBF(dif ~ cond*exp, data=data) #first time the loop analyzes my original data, later the data is icreases
  model = as.data.frame(as.vector(model)) #convert the BayesFactor object to something readible
  interactionBF$BF = as.numeric((model[4,1]/model[3,1])) # here I only save the BF for the interaction (line 4 divided by line 3)
  interactionBF$sample = as.numeric(nrow(data)) # I save the information about what my N was
  interactionBF = as.data.frame(interactionBF)
  print (interactionBF) # and print to console just to keep track
    result <- rbind(interactionBF, result) # I bind my result with the previous one
    dfA<-df[sample(nrow(df), 8), ] # I sample 8 participants with replacment from my data
    data <- rbind (data, dfA) # and add those to my data
}
  data = df  # I restore data to point zero to run another simulation
  if (length(result$BF) == 5000){ # this will determine how many times the loop will run
    break
  }
}

## I plot this to get the variability of BF in each simulation
result$sample<-as.factor(result$sample)
ggplot(data = result, aes(y=BF, x=sample))+
  geom_jitter(alpha = 0.2)+
  geom_hline(yintercept=0.3, color = "grey", size = 1)+
  geom_hline(yintercept=3, linetype = "dashed", color = "red", size = 1.5)+
  theme_bw()+
  scale_y_continuous(limits = c(-10, 100))+
  theme(text = element_text(size = 20))

## Get some stats from this
result$moderate<-ifelse(result$BF>3, 1, 0) #my threshold was BF = 3. You can set it higher
xtabs(~result$moderate) # how many samples were above this threhold?

largeN<-subset (result, sample>150) ##let's look only at large samples
xtabs(~largeN$moderate) ##now more samples cross the threshold


