#setwd("C:/Users/Ziv Ru/MACHINE-LEARNINGNG")

###dictionary as vectors for computer to understand 
train <- read.table("FinalTable.txt")

### aggregate the sum of every answer(1-7)
X <- aggregate(train[,c(2:388)], by=list(Category=train$V1), FUN=sum)

### new table to work on
Coeff <- X

### find the total sum. 1 indicates rows
t <- apply(X[,c(2:388)],1,sum)

### find the coefficence for the algorthim, [scale vectors] 
for(i in 2:388)
{
  for(j in 1:7)
  {
    Coeff[j,i] <- X[j,i]/t[j]
  }
}

### testing example
test <- train
test$P1 <- 0
test$P2 <- 0
test$P3 <- 0
test$P4 <- 0
test$P5 <- 0
test$P6 <- 0
test$P7 <- 0
###k as # of answers, i as # of rows, j as # of col
##[making sure that every question suits the results, max = correct answer]
#testing the trainig set - sum the coef[1to7]*question[2to388]
for(k in 1:7)
{
  for(i in 1:383)
  {
    sum <- 0
    for(j in 2:388)
    {
       sum = sum + Coeff[k,j] * train[i,j]
    }
    test[i,388 + k] <- sum
  }
}

write.csv(test,"out.csv")
write.csv(Coeff,"Coefficence2.csv")