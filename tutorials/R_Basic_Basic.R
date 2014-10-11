######## General Useful Tips ###########
# evaluate by selectivng lines and pressing 'apple + return'
# break command by pressing esc key in mac
# help for a function can be found by typing ?functionName
# arguements of function found by typing args(function Name)
# example of function can be found by typing example(functionName)
# 'ls()' lists variables in workspace 
# 'ls.str' lists variables + attributes
#  

# R does basic arithmatic 
1+3
max(5,1,6)

# variables can be assigned using '<-'
x<-(4+5)/3

# vectors are made using c()
vect<-c(1,5,8,10,3,5)
stringVect<-c('R', 'club', 'is', 'the', 'best')

# cat() can be used to concatonate vectors
stringVect2<-cat(vect, stringVect)  

# rm(variable) deletes data
rm(vect)
vect

# ':' and seq() are used to make sequences
1:10
10:1
seq(from=1, to=10, by=2)

# '[]' is used for indexing
vect<-1:15
vect[5]
vect[4:8]
vect[vect>10]
vect[ abs(vect-mean(vect)) > 1.5*sd(vect) ]


# R has for loops
for(i in 1:10) {
  y[i] <- i*i
}

# R has while loops
i=1
while(i <= 10) {
  y[i] <- i+i
  i <- i + 1
}




