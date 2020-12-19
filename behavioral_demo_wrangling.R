library(dplyr)
library(magrittr)

##behavioral/demographic data merging

Z1 <- read.csv('merge_enback.csv')
Z2 <- Z1[ , -grep('tfmri', colnames(Z1))]
A1 <- read.csv('SuicideAttempt_interaction_test.csv')
A2 <- read.csv('SuicideAttempt_interaction_train.csv')
A <- rbind(A1, A2)
C <- merge(Z2, A)
nrow(A)
View(Z1)
nrow(Z2)
nrow(C)
names(C)

#factorize, z-transform

apply(C, 2, class)

for (i in 1:ncol(C)){
  if (class(C[,i]) == character){
    
  }
}

C <- scale(C)



#NA wrangling

colSums(is.na(C))
View(C)

#export
write.csv(C, 'behav_demo.csv')




D <- read.csv('behav_short.csv')
