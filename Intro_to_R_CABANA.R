# CABANA Workshop, San Jose Costa Rica
# Introduction to R:          R Alcala | University of Florida
#
# R version 3.6.2  # check R.version
#
#-------------------------- Introduction to R Basics ----------------------------------
# You can use R as a simple calculator:
1+1

3*2

8^2

7/2

4+4-2*8

4+(4-2)*8

# Objects
# Setting values to use again as 'objects'. 
# We do this using the assignment operator '<-' (called the 'gets arrow').

a <- 2 + 2
print(a) 
a

b <- a + 2
b

# You can use '<-' for assigment and '=' for parameters

a.vector <- c(4, 3, 2, 1)

parameter1 = .1

par1 = par2 = par3 = NULL

#-------------------------- Relational operators ----------------------------------
# [ <, >, ==, >=, <=, !=]
# Comparing values will return “True” or “False” 
# logical
TRUE == TRUE 
T == F 
FALSE != FALSE 
# binary
FALSE == 0
TRUE <= 1

# loigical numericals
2==2 ## "==" means does equal 
2!=2 ## "!=" means does not equal
2>=3 ## ">=" means greater than or equal to

# lgical strings
apple <-  "apple"
pineapple <-  "pineapple"

apple == pineapple # based on alphabetical order
apple > pineapple
apple < "abcde"


# Create an object with values from 1 to 10
num10 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Which elements are less than 5? 
num5 <- num10 < 5

# Check the elements in num5, these are TRUE / FALSE
num5

# subsetting objects
num10[3:8]
num10[!num5]

#-------------------------- Relational Operators &  Sub-setting 2D object  ----------------------------------
# data() – data available already in R
data(iris) # famous dataset iris 
head(iris) # checking the first rows
tail(iris) # checking the last rows

data("mtcars") # mtcars data   
str(mtcars) # checking the data structure
dim(mtcars) # checking the dimension of data; 32 rows, 11 columns
class(mtcars) #checking the class

# subsetting the data 
mtcars[1:5, ] # display only the first 5 rows [looks like head() !]

# select only the cars with less than 20 mpg tanks
low_mpg <- mtcars[mtcars$mpg < 20, ] # NOTE: syntax use data[row, column]
low_mpg

# logical vector
low_mpg$mpg < 20 # what do you think the result will be?

#-------------------------- Logical Operators ----------------------------------
# [ &, | , ! ]; 
# [AND, OR, NOT]

# select the cars with mileage below 20 AND with 8 cylinders
low_mpgAND8cyl <- mtcars[ c( mtcars$mpg  < 20  & mtcars$cyl == 8), ]
head(low_mpgAND8cyl)

low_mpgAND8cyl2 <- mtcars[ which(mtcars$mpg  < 20  & mtcars$cyl == 8), ]
head(low_mpgAND8cyl2)

# Are they the same?
low_mpgAND8cyl == low_mpgAND8cyl2

# select the cars with mileage below 20 OR with 8 cylinders
low_mpgOR8cyl <- mtcars[c(mtcars$mpg  <  20 | mtcars$cyl == 8), ]


# select the cars that are any cylinders but 8.
mtcars[mtcars$cyl != 8,]


#-------------------------- Vectors ----------------------------------
# Vectors are a way to set a series of data elements as an object.

v1 <- c(1, 2, 3, 4, 5) # numbers
v2 <- c("hello", "world") # characters 
v3 <- c("TRUE", "FALSE", "TRUE") # logical values (also could be "T", "F", "T")


# Lets make a vector with hypothetical ratings of “R expertise” on a scale of 1-10.
WithR <- c(8.5, 6.5, 4, 1, 3, 10, 5, 5, 5, 1, 1, 6, 6)
WithR

#Summary statistics
#We can use the following functions to look at some summary statistics.
summary(WithR)

# If you have a doubt about a function you can call 'help'
?summary
mean(WithR)
sd(WithR) 
median(WithR)
# work inside the vector withinR
vector <- summary(WithR)
vector["Mean"] # accessing the vector for 'Mean'
vector[3] #accessing the vector for position '3'

# basic plot: histogram or data distribution!
hist(WithR, xlab = "Self-Reported R Proficiency")
plot(WithR, xlab = "Self-Reported R Proficiency")

#-------------------------- conditional statemtns  ----------------------------------

# 'if' symntax 
# if (test_condition) {
#     do_my_condition
#   }

i <- 2
if (i < 5) {
	message(i, " is less than 5")
  }

# 'if else' symnyax
# if (test_condition) {
#     do_my_condition
#   } else {
#     do_other_condition}

i <- 20
if (i < 5) { # change '<' to '>' and see what happens
	message(i, " is less than 5")
} else {  
	message(i, " is greater than 5")}

# 'while' symnyax
# while (test_condition) {
#    do_my_condition
#    }

i <- 1
while (i < 10) {
  print(i)
  } # this is gonna run forever so 'kill the taks' ctrl + c or press 'stop' in your console

# fullfiling your condition
while (i < 10) {
  print(i)
  i = i + 1}


#-------------------------- loops  ----------------------------------
# 'for' loop syntax
# for (test_condition) {
#    do_my_condition
# }

for (i in 1:10){
  print(i)  
  i = i + 1
  }

# 'while' loop syntax
# for (test_condition) {
#    do_my_condition
# }

i <- 1
while (i <= 10) {
  print(i)
  i = i + 1
  }

# for loops with if else stament

for (i in 1:10){ 
  if (i <= 5) {
	message(i, " Loops are cool")
  } else {  
	message(i, " Loops are super cool")}
}

