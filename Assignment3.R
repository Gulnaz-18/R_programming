# This code demonstrates classification, regression and probability decision trees 
# Week 3: R Programming Assignment
# Written by Gulnaz Khabibullina

#load the necessary libraries 
library( rpart )
library( rpart.plot )
library( ROCR )

#declare a variable PATH that stores a path to access to Excel file's folder 
PATH = "C:/Users/gulna/OneDrive/Documents/Trine/Data Science/HMEQ_Scrubbed"

FILE_NAME 	= "HMEQ_Scrubbed.csv"

INFILE = paste( PATH, FILE_NAME, sep="/" )

setwd( PATH )

#Step 1: Read into data
#read the data into R
df = read.csv( FILE_NAME )

#list the structure of the data (str)
str( df )

#execute a summary of the data
summary( df )

#print the first six records
head(df)

#Step 2: Classification Decision Tree
#Use the rpart library to predict the variable TARGET_BAD_FLAG
#Develop two decision trees, one using Gini and the other using Entropy
#All other parameters such as tree depth are up to you.
#Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
#Plot both decision trees
#List the important variables for both trees
#Create a ROC curve for both trees
#Write a brief summary of the decision trees discussing whether or not they make sense. 
#Which tree would you recommend using? What type of person will default on a loan?


#make a copy of original dataset in order to predict TARGET_BAD_FLAG and set TARGET_LOSS_AMT to NULL
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

head (df_flag)


#rpart to partition data
tree_set = rpart.control( maxdepth = 10 )

#use gini split
tree_gini = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tree_set, method="class", parms=list(split='gini') )

#plot the tree
rpart.plot( tree_gini )


#use entropy split
tree_entropy = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tree_set, method="class", parms=list(split='information') )

#plot the tree
rpart.plot( tree_entropy )

#plot important variables for gini split 
tree_gini$variable.importance

#plot important variables for entropy split 
tree_entropy$variable.importance


#gini roc curve 
pGini = predict( tree_gini, df )
pGini2 = prediction( pGini[,2], df$TARGET_BAD_FLAG )
pGini3 = performance( pGini2, "tpr", "fpr" )

#entropy roc curve
pEntropy = predict( tree_entropy, df )
pEntropy2 = prediction( pEntropy[,2], df$TARGET_BAD_FLAG )
pEntropy3 = performance( pEntropy2, "tpr", "fpr" )



#plot the curves 
plot( pGini3, col="blue" )
plot( pEntropy3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY"),col=c("blue","green"), bty="y", lty=1 )

#area under the gini roc curve
aucGini = performance( pGini2, "auc" )@y.values

#area under the entropy roc curve
aucEntropy = performance( pEntropy2, "auc" )@y.values

print( aucGini )
print( aucEntropy )


#predict gini
fGini = predict( tree_gini, df, type="class" )

#predict entropy
fEntropy = predict( tree_entropy, df, type="class" )

#print
table( fGini, df$TARGET_BAD_FLAG )
table( fEntropy, df$TARGET_BAD_FLAG )


#Step 3: Regression Decision Tree
#Use the rpart library to predict the variable TARGET_LOSS_AMT
#Develop two decision trees, one using anova and the other using poisson
#All other parameters such as tree depth are up to you.
#Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
#Plot both decision trees
#List the important variables for both trees
#Calculate the Root Mean Square Error (RMSE) for both trees
#Write a brief summary of the decision trees discussing whether or not they make sense. 
#Which tree would you recommend using? What factors dictate a large loss of money?

#make a copy of original dataset in order to predict TARGET_LOSS_AMT and set TARGET_BAD_FLAG to NULL
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL
head (df_amt)

#mean 
mean( df_amt$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 10 )

#decision tree with method anova 
tree_anova = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( tree_anova )
tree_anova$variable.importance
predict_tree_anova = predict( tree_anova, df )
RMSE1a = sqrt( mean( ( df$TARGET_LOSS_AMT - predict_tree_anova )^2 ) )
print( RMSE1a )

#decision tree with method poisson 
tree_poisson = rpart( data=df_amt, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tree_poisson )
tree_poisson$variable.importance
predict_poisson = predict( tree_poisson, df )
RMSE1p = sqrt( mean( ( df$TARGET_LOSS_AMT - predict_poisson )^2 ) )
print( RMSE1p )

#print
print( RMSE1a )
print( RMSE1p )

#Step 4: Probability / Severity Model Decision Tree (Push Yourself!)
#Use the rpart library to predict the variable TARGET_BAD_FLAG
#Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
#Plot both decision trees
#List the important variables for both trees
#Using your models, predict the probability of default and the loss given default.
#Multiply the two values together for each record.
#Calculate the RMSE value for the Probability / Severity model.
#Comment on how this model compares to using the model from Step 3. Which one would your recommend using?
  

#set TARGET_LOSS_AMT is already set to NULL 

head(df_flag)
target_flag = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set )
rpart.plot( target_flag )
predict_flag = predict( target_flag, df )
target_flag$variable.importance

#predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
df_amount_2 = subset( df, TARGET_BAD_FLAG == 1 )
head(df_amount_2)

#delete the TARGET_BAD_FLAG field
df_amount_2$TARGET_BAD_FLAG = NULL
head(df_amount_2)

#using anova plot the decision tree
target_amount = rpart( data=df_amount_2, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( target_amount )
target_amount$variable.importance

#predict amount
predict_amount = predict( target_amount, df )

#predict the probability and amount to repay
predict_flag_amount = predict_flag * predict_amount
RMSE2 = sqrt( mean( ( df$TARGET_LOSS_AMT - predict_flag_amount )^2 ) )


#print
head(predict_flag)
head (predict_amount)
head(predict_flag_amount)

#print
print( RMSE1a )
print( RMSE2 )


