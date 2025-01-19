# This code demonstrates data analysis for iris dataset
# Week 1: R Programming Assignment
# Written by Gulnaz Khabibullina

# print the structure of the dataset iris
str (iris)

# print the summary of the dataset iris
summary (iris)

# print the first 6 records in the dataset iris 
head (iris)

# Plot a box-whisker plot of the Sepal.Length split by the Species variable. 
# The MAIN TITLE of the box plot is set to my name.
# Add a notch to the boxes.
# Add color to the boxes.
boxplot( iris$Sepal.Length ~ iris$Species,notch=TRUE,col=c("blue","green"),
         xlab="Species", ylab = "Sepal.Length", main="Gulnaz Khabibullina" )

# Plot a histogram of Sepal.Length.
# Manually set the number of breaks to 15.
# Superimpose a density line to the graph.
# Add color or decoration to the graph.
hist( iris$Sepal.Length, breaks = 15, col = "green", xlab="Sepal.Length", main = "Histogram of Sepal.Length")
lines( density( iris$Sepal.Length ),col = "blue"  )

# Since the graph above does not provide clear image of the superimposed
# density line, I added one below with probability equals to TRUE as opposed to frequency
# Add color or decoration to the graph 
hist( iris$Sepal.Length, prob = TRUE, col = "green",xlab="Sepal.Length", main = "Histogram of Sepal.Length")
lines( density( iris$Sepal.Length ), col = "blue" )

# Create a scatter plot of Sepal.Length and Sepal.Width.
# Each group member is in a different color.
# Set the plot character to a value to 16. 
# Add color or decoration to this graph. 
plot( iris$Sepal.Width, iris$Sepal.Length, col=iris$Species, pch=16,
      xlab="Sepal.Width", ylab = "Sepal.Length", main = "Scatterplot of Sepal.Width and Sepal.Length " )
legend(x="topleft",legend = c('setosa','versicolor','virginica'), 
       col = unique(iris$Species), lty = 1)


# Simple Math
# For the Sepal.Length, compute the following statistics:
# Compute mean
mean(iris$Sepal.Length)

# Compute median
median( iris$Sepal.Length )

# Compute min
min( iris$Sepal.Length )

# Compute max 
max( iris$Sepal.Length )

# Compute standard deviation
sd( iris$Sepal.Length )

# Calculate the Median for Sepal.Length for each group member. 
# Sort the result in Descending order.
a = aggregate( x=iris$Sepal.Length, by=list( iris$Species), FUN=median )
a = a[ order( a$x, decreasing=TRUE), ]
a
