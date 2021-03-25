# The 7 game problem from class
print(mean(rowSums(matrix(runif(700000),100000,7)>0.5)>4))
# OR
print(mean(rbinom(100000,7,0.5)>4))


# The bus seating problems from class
print(mean(rbinom(100000,10,0.9)>8))
