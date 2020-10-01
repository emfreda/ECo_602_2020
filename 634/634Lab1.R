# Template R-script for ECo 634 fall 2020: Lab 01

# Use this template script to do your work.
# ---- Question 1 ----
# Run the following two lines of code in the console and explain the differences in output:
c(1, 2, 3)
"c(1, 2, 3)"


# ---- Question 2 ----
# Run the following two lines of code in the console and consider the differences:
c_1 = c(1, 2, 3)
class(c_1)
c_2 = "c(1, 2, 3)"
class(c_2)

# Q1. Is c_1 a variable, or a function?
# Q2. Is c_2 a variable, or a function?
# Q3. If c_1 and c_2 have different values, why?


# ---- Question 3: Matrices 1 ----
# Create a numeric vector of length 3 called my_vec. It should contain the integers from 1 to 3.
my_vec = c(1,2,3)
  
# Build a matrix using the following code:
mat_1 = matrix(my_vec)
dim(mat_1)
mat_1[3,]

# Q1 (1pt.): What are the dimensions of the matrix (i.e. how many rows and columns)?
# Q2 (2pts.): Write R code to retrieve the element of mat_1 that has a value of 3.
mat2 <- matrix(1:9, nrow=3, byrow=TRUE)
dim(mat2)
mat2[3]



# ---- Question 4: Matrices 2 ----
# You will use my_vec from the previous question again.
# Create a matrix mat_2 that has two rows and three columns using my_vec. Do not use the c() or rep() functions.
# Create a matrix mat_3 that has three rows and two columns using my_vec. Do not use the c() or rep() functions.

# Q1 (1pt.): Paste the code you used to create mat_2.
mat_2 <- matrix(my_vec, nrow=2, ncol=3)
mat_2
# Q2 (1pt.): Paste the code you used to create mat_3.
mat_3 <- matrix(my_vec, nrow=3, ncol=2)
mat_3
# Q3 (1pt.): Did R use rows or columns to recycle the values in my_vec?
# Q4 (1pt.): Create a matrix, mat_4, with a number of elements that is not a multiple of 3 and paste the code into the editor.

mat_4 <- matrix(my_vec, nrow=4, ncol=5)
mat_4
# Q5 (1pt.): How did R handle the recycling of values of my_vec in mat_4?


# ---- Question 5: List subsetting challenge question ----
# Create a list, named my_list_1 with following three elements:
# first element is numeric: 5.2
# second element is a string "five point two"
# third element is a vector of all integers from 0 to 5. Do recall how to do this from the DataCamp course?
# Name the elements in my_list_1:
# “two”
# “one”
# “three”
my_list_1 = list ("two"= 5.2,"one"= "five point two","three"= c(1:5))
my_list_1

# Run the following lines of code.
my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"

# Q1 (8 pts): for each subsetting operation, explain why R produced the corresponding output.
# Identify the type of subsetting operation.
# Explain how the operation chose which element (1, 2, or 3) to return.

# Q2 (2 pts): Hypothesize why some of the lines may have produced errors or NULL outputs.
# Q3 (2 pts): Identify which lines produced the same output and explain why.

#non question related messing around
my_list_1[[as.numeric(1)]]
