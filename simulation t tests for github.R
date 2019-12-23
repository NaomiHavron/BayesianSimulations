
library(BayesFactor)
library(dplyr)
library (ggplot2)

################################
# independent sample t test ####
################################

#I will upload our original data after the paper is accepted, for now let's simulate some fake data:

# Set coefficients
alpha = 1
beta1 = .3

# Generate 50 trials
A = c(rep(c(0), 25), rep(c(1), 25)) # '0' 100 times, '1' 100 times
e = rnorm(50, 0, sd=1) # Random noise, with standard deviation of 1

# Generate your data using the regression equation
y = alpha + beta1*A + e

data = data.frame(cbind(A, y))

##change names and values to fit my actual data
names(data)<-c("cond", "dif")
data$cond <- as.character(data$cond )
data$cond [data$cond  == "0"] <- "noun_cond"
data$cond [data$cond  == "1"] <- "verb_cond"

data$cond<-as.factor(data$cond)
summary(data)

df = data ##I am saving the original data
data = df ## but creating a duplicate of it. The duplicate will grow while the original data remains untouched.
##set some empty objects to fill in with simulated results
result = data.frame(BF=numeric(), sample = numeric())
BF = data.frame(BF=1, sample = 1)

repeat {
for (i in 1:20){ #I add 20 batches of new paerticipants, sampled with replacment from my data
  # Fit a regression
  model = ttestBF(data$dif[data$cond == "noun_cond"],data$dif[data$cond == "verb_cond"], rscale = c( cond = 1 ))
 #first time the loop analizes my data, later the data is icreases
  model = as.data.frame(as.vector(model))
  BF$BF = as.numeric(model[1,1]) # here I only save the BF for the interaction (line 4 divided by line 3)
  BF$sample = as.numeric(nrow(data)) # I save the information about what my N was
  BF = as.data.frame(BF)
  print (BF) # and print to console just to keep track
    result <- rbind(BF, result) # I bind my result with the previous one
    dfA<-df[sample(nrow(df), 8), ] # I sample 8 participants with replacment from my data
    data <- rbind (data, dfA) # and add those to my data
}
  data = df  # I restore data to point zero to run another simulation
  if (length(result$BF) == 2000){ # this will determine how many times the loop will run
    break
  }
}

## I plot this to get the variability of BF in each simulation
result$sample<-as.factor(result$sample)
ggplot(data = result, aes(y=BF, x=sample))+
  geom_jitter()+
  geom_hline(yintercept=0.3, color = "grey", size = 1)+
  geom_hline(yintercept=3, linetype = "dashed", color = "red", size = 1.5)+
  theme_bw()+
  scale_y_continuous(limits = c(-1, 6))+
  theme(text = element_text(size = 20))

## Get some stats from this
result$moderate<-ifelse(result$BF>3, 1, 0) #my threshold was BF = 3. You can set it higher
xtabs(~result$moderate) # how many samples were above this threhold?

result$sample<-as.numeric(as.character(result$sample))
largeN<-subset (result, sample>150) ##let's look only at large samples
xtabs(~largeN$moderate) ##now more samples cross the threshold


########################
# single sample t test #
#######################
noun_cond<-subset (data, cond == "noun_cond")
data = noun_cond
##set some empty objects to fill in with simulated results

result = data.frame(BF=numeric(), sample = numeric())
BF = data.frame(BF=1, sample = 1)

repeat {
  for (i in 1:20){ #I add 20 batches of new paerticipants, sampled with replacment from my data
    # Fit a regression
    model = ttestBF(data$dif, rscale = 1 , mu = 0)
    #first time the loop analizes my data, later the data is icreases
    model = as.data.frame(as.vector(model))
    BF$BF = as.numeric(model[1,1]) # here I only save the BF for the interaction (line 4 divided by line 3)
    BF$sample = as.numeric(nrow(data)) # I save the information about what my N was
    BF = as.data.frame(BF)
    print (BF) # and print to console just to keep track
    result <- rbind(BF, result) # I bind my result with the previous one
    dfA<-df[sample(nrow(df), 4), ] # I sample 8 participants with replacment from my data
    data <- rbind (data, dfA) # and add those to my data
  }
  data = noun_cond  # I restore data to point zero to run another simulation
  if (length(result$BF) == 8000){ # this will determine how many times the loop will run
    break
  }
}

## I plot this to get the variability of BF in each simulation
result$sample<-as.numeric(as.character(result$sample))
ggplotly(ggplot(data = result, aes(y=BF, x=sample))+
           geom_jitter(alpha = 0.8)+
  geom_hline(yintercept=0.3, color = "grey", size = 1)+
  geom_hline(yintercept=3, linetype = "dashed", color = "red", size = 1.5)+
  theme_bw()+
  scale_y_continuous(limits = c(0, 10000))+
  scale_x_continuous(limits = c(29, 345), breaks = seq(29,345, by = 25))+
  theme(text = element_text(size = 20)))
##note that with the beta I simulated results are so high that you cannot see them in this graph unless you set limits to a very high maximum y value. Your own data might be more visualizable. 

## Get some stats from this
result$extreme<-ifelse(result$BF>1000, 1, 0) #I am defining my thershold as exteremly high because all samples are way above 3
xtabs(~result$extreme) # how many samples were above this threhold?

largeN<-subset (result, sample>150) ##let's look only at large samples
xtabs(~largeN$moderate) ##now even more samples cross the threshold

