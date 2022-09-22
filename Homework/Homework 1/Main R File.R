#load "arules" library
library(arules)

#import the dataset
#txn.df<-read.csv("transactions.csv")
txn.df <- read.csv("C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Homework/Homework 1/transactions.csv")

#remove the first column
txn.df1<-txn.df[,-1]
incid.txn.df<-ifelse(txn.df1>0,1,0)
#as(txn.df1,"transactions")

#convert the dataframe to matrix
txn.mat<-as.matrix(incid.txn.df)
#convert the matrix to transaction database
txn.trans<-as(txn.mat,"transactions")
inspect(txn.trans)

#run apriori function
rules<-apriori(txn.trans,parameter=list(supp=0.01,conf=0.10,target="rules",minlen=2))

#inspect rules
inspect(rules)

#sort by lift in descending order
inspect(sort(rules,by="lift"))

#show top 10 association rules
inspect(head(sort(rules,by="lift"),n=10))
