# This code demonstrates predictive analysis on a loan dataset
# Week 2: R Programming Assignment
# Written by Gulnaz Khabibullina

#declare a variable PATH that stores a path to access to Excel file's folder 
PATH = "C:/Users/gulna/OneDrive/Documents/Trine/Data Science/HMEQ_WK02"

#declare a variable INPUT_FILE to store input file
INPUT_FILE = "HMEQ_Loss.csv"

#declare a variable OUTPUT_FILE to store output file
OUTPUT_FILE = "HMEQ_Loss_Scrubbed.csv"

#set directory of INPUT_FILE to the declared path
setwd( PATH )

#Step 1: Read into data
#read the data into R
df = read.csv( INPUT_FILE )

#list the structure of the data (str)
str( df )

#execute a summary of the data
summary(df)

#print the first six records
head(df)

#Step 2: Box Plot
#Plot a box plot of all the numeric variables split by the TARGET_BAD_FLAG. 
#The plot needs the following:
#The MAIN TITLE of the box plot should be set to my name
#Add color to the boxes
#Comment on whether or not there are any observable differences in the box plots between the two groups.

#Target Loss Amount
boxplot(df$TARGET_LOSS_AMT ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Target Loss Amount")

#Loan Amount
boxplot(df$LOAN ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Loan")

#Mortgage Due
boxplot(df$MORTDUE ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Mortgage Due", 
        ylim=c(0, 400000))

#Value
boxplot(df$VALUE ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Value", 
        ylim=c(0, 855909))

#Years on the Job
boxplot(df$YOJ ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Years on Job")

#Derogatory Loans
boxplot(df$DEROG~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Derogatory Loans")

#Delay in Payment
boxplot(df$DELINQ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Delay in Payment")

#Credit Line Age Months
boxplot(df$CLAGE ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Credit Line Age Months")

#Number of Inquiries
boxplot(df$NINQ ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Number of Inquiries")

#Lines if Credit
boxplot(df$CLNO ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Lines of Credit")

#Debt/Income
boxplot(df$DEBTINC ~ df$TARGET_BAD_FLAG,col=c("green", "blue"),
        main="Gulnaz Khabibullina", xlab="Target Bad Flag", ylab = "Debt/Income",
        ylim=c(0, 50))


#Step 3: Histograms
#Plot a histogram of at least one of the numeric variables
#Manually set the number of breaks to a value that makes sense
#Superimpose a density line to the graph

hist( df$LOAN, breaks = 15, col = "green", xlab="Loan Amount", main = "Histogram")
lines( density( df$LOAN ),col = "blue"  )


hist( df$LOAN, prob = TRUE, col = "green", xlab="Loan Amount", main = "Histogram")
lines( density( df$LOAN ), col = "blue" )


#Step 4: Impute "Fix" all the numeric variables that have missing values
#For the missing Target variables, simply set the missing values to zero
#For the remaining numeric variables with missing values, create two new variables. 
#One variable will have a name beginning with IMP_ and it will contained the imputed value. 
#The second value will have a name beginning with M_ and it will contain a 1 if the record was 
#imputed and a zero if it was not.
#You may impute with any method that makes sense. The median or mean value will be useful in most cases.
#Push yourself! Try one complex imputation like the one described in the lectures.
#Delete the original variable after it has been imputed.
#Run a summary to prove that all the variables have been imputed
#Compute a sum for all the M_ variables to prove that the number of flags is equal to the number of 
#missing values.


# TARGET_BAD_FLAG, set the missing values to 0 
df$TARGET_LOSS_AMT[ is.na( df$TARGET_LOSS_AMT ) ] = 0


# MORTDUE 
median( df$MORTDUE, na.rm=TRUE )
df$IMP_MORTDUE = df$MORTDUE
df$IMP_MORTDUE[ is.na( df$MORTDUE) ] = 65019
df$M_MORTDUE = is.na( df$MORTDUE) + 0
sum( df$M_MORTDUE )
summary( df )
head(df, 15)
df$MORTDUE = NULL
summary( df )

# VALUE
median( df$VALUE, na.rm=TRUE )
df$IMP_VALUE = df$VALUE
df$IMP_VALUE[ is.na( df$VALUE) ] = 89235.5
df$M_VALUE = is.na( df$VALUE) + 0
sum( df$M_VALUE )
summary( df )
df$VALUE = NULL
summary( df )

#YOJ
median( df$YOJ, na.rm=TRUE )
df$IMP_YOJ = df$YOJ
df$IMP_YOJ[ is.na( df$YOJ ) ] = 7
df$M_YOJ = is.na( df$YOJ ) + 0
sum( df$M_YOJ )
summary( df )
df$YOJ = NULL
summary( df )

#DEROG
median( df$DEROG, na.rm=TRUE )
df$IMP_DEROG = df$DEROG
df$IMP_DEROG[ is.na( df$DEROG ) ] = 0
df$M_DEROG = is.na( df$DEROG ) + 0
sum( df$M_DEROG )
summary( df )
df$DEROG = NULL
summary( df )

#DELINQ
median( df$DELINQ, na.rm=TRUE )
df$IMP_DELINQ = df$DELINQ
df$IMP_DELINQ[ is.na( df$DELINQ ) ] = 0
df$M_DELINQ = is.na( df$DELINQ ) + 0
sum( df$M_DELINQ )
summary( df )
df$DELINQ = NULL
summary( df )

#CLAGE
median( df$CLAGE, na.rm=TRUE )
df$IMP_CLAGE = df$CLAGE
df$IMP_CLAGE[ is.na( df$CLAGE) ] = 173.4667
df$M_CLAGE = is.na( df$CLAGE) + 0
sum( df$M_CLAGE)
summary( df )
df$CLAGE = NULL
summary( df )

#NINQ
median( df$NINQ, na.rm=TRUE )
df$IMP_NINQ = df$NINQ
df$IMP_NINQ[ is.na( df$NINQ) ] = 1
df$M_NINQ = is.na( df$NINQ) + 0
sum( df$M_NINQ)
summary( df )
df$NINQ = NULL
summary( df )

#CLNO
a = aggregate( x=df$CLNO, by=list( df$JOB ), na.rm=TRUE, FUN=median )
a = a[ order( a$x, decreasing=TRUE), ]
a

df$IMP_CLNO = df$CLNO
df$IMP_CLNO[ is.na(df$CLNO) & ( df$JOB == "ProfExe" 		) ] = 24
df$IMP_CLNO[ is.na(df$CLNO) & ( df$JOB == "Mgr" 		) ] = 23
df$IMP_CLNO[ is.na(df$CLNO) & ( df$JOB == "Sales" 	) ] = 23
df$IMP_CLNO[ is.na(df$CLNO) & ( df$JOB == "Self" 	) ] = 22
df$IMP_CLNO[ is.na(df$CLNO) & ( df$JOB == "Office" 	) ] = 20
df$IMP_CLNO[ is.na(df$CLNO) & ( df$JOB == "Other" 	) ] = 18

df$IMP_CLNO[ is.na(df$IMP_CLNO)  ] = 15
df$M_CLNO = is.na( df$CLNO ) + 0

sum( df$M_CLNO)
summary( df )

df$CLNO = NULL
summary( df )


#DEBTINC
median( df$DEBTINC, na.rm=TRUE )
df$IMP_DEBTINC = df$DEBTINC
df$IMP_DEBTINC[ is.na( df$DEBTINC) ] = 34.81826
df$M_DEBTINC = is.na( df$DEBTINC) + 0
sum( df$M_DEBTINC)
summary( df )
df$DEBTINC = NULL
summary( df )

#Step 5: One Hot Encoding
#For the character / category variables, perform one hot encoding. For this create a Flag for each categories.
#Delete the original class variable
#Run a summary to show that the category variables have been replaced by Flag variables.

# Reason
table( df$REASON )
df$FLAG.DebtCon = ( df$REASON == "DebtCon" ) + 0
sum( df$FLAG.DebtCon )
df$REASON = NULL
tail( df, 20)

# Job
table( df$JOB )
df$FLAG.Job.Mgr = ( df$JOB == "Mgr"	) + 0 
df$FLAG.Job.Office	= ( df$JOB == "Office" ) + 0 
df$FLAG.Job.Other		= ( df$JOB == "Other"  ) + 0 
df$FLAG.Job.ProfExe	= ( df$JOB == "ProfExe" ) + 0 
df$FLAG.Job.Sales	= ( df$JOB == "Sales" 		) + 0 
df$FLAG.Job.Self		= ( df$JOB == "Self" 	) + 0 

table (df$JOB)
sum( df$FLAG.Job.Mgr )
sum( df$FLAG.Job.Office )
sum( df$FLAG.Job.Other )
sum( df$FLAG.Job.ProfExe )
sum( df$FLAG.Job.Sales )
sum( df$FLAG.Job.Self )

#delete original value
df$JOB = NULL
summary(df)
head (df)
