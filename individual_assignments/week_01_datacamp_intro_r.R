#QUESTION 1
a <- "emily"
b1 <- 45.6
b2 <- "45.6"
c <- c(0, 1, 2, 3)

class(a)
class(b1)
class(b2)
class(c)
sum(b1+b2)
sum(b1+c)


#QUESTION 2
v1 <- c(-2:2)
v1
v2 <- v1 * 3
v2
sum(v2)

#QUESTION 3
my_list_1 <- list("two" = 5.2, "one" = "five point two", "three" = c(0:5))
my_list_1

#QUESTION 4
my_list_1[3]
my_list_1$one

#QUESTION 5
my_vec = rep(1:3, 5)
my_vec
my_bool_vec <- my_vec == 3
my_bool_vec
#set up dataframe that puts the vecs next to each other for easy comparison
data.frame(my_vec, my_bool_vec)

#QUESTION 6
my_vec[my_bool_vec]
