#QUESTION 1

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

#create a Boolean vector (called vec_2) 
#whose entries are TRUE if the corresponding entry 
#in vec_1 is 3 and FALSE otherwise

vec_2 <- vec_1 == 3
head(vec_2)

#self test to see if vec_2 retrieves all values of 3 in vec_1
vec_1[vec_2]


#QUESTION 2

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
# check vector length
length(vec_1)
#check how many entries have a value of 3
sum(vec_1 == 3)
#run the following code several times 
#taking note of how many 3 entries appear each time you run it.
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))


#QUESTION 3
#classic for loop that prints the index value for 10 iterations
for (i in 1:10)
{
  print(i)
}
#Modify the code in the body of the loop to print out a message
for (i in 1:10)
{
  print(paste0("This is loop iteration ", i))
}


#QUESTION 4 
#write a for loop that iterates n times, printing each index
n <- 13
for(i in 1:n)
{
  print(i)
}


#QUESTION 5
#create a variable with a value of 17
n <- 17
#create a vector as long as the variable n
#this vector should be made of randomly generated integers from 1-10
vec_1 <- sample(10, n, replace = TRUE)
vec_1
###sample() generates random numbers
###the first argument is the range of values
###the second argument is the length of the vector
###the third argument specifies if you can repeat values or not

#create a loop the executes n times (soft coded length of vec_1)
#and prints out each element of vec_1 with it's index/iteration value
for(i in 1:n)
{
  print(paste0("the element at index ", i, " is ", vec_1[i]))
}


#QUESTION 6
#make a custom function that:
#creates a vector of n random integers 
#between the values of min (1) and max(10); and
#loops through the elements of the vector, 
#printing a message with the index of the element and its value.

create_and_print_vec <- function(n, min = 1, max = 10)
{
  vec <- sample(min:max, n, replace = TRUE)
  for(i in 1:n)
  {
    print(paste0("the element at index ", i, " is ", vec[i]))
  }
}
#test it out with a length of 10, min(100), and max(2000)
create_and_print_vec(10, 100, 2000)
