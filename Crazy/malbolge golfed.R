

# Test case. In the submission it will be
# x=scan('stdin')
x<-c(0, 0,
      29524, 29524,
      59048, 59048,
      1131, 11355,
      11355, 1131)



for(i in seq(1,length(x),2)){ # every odd-numbered index
  N=0                         # N will eventually be the result
  a=x[i+0:1]                  # "a" is the i'th pair of numbers
  f=floor                     # set floor function to a letter to save characters
  for(j in 9:0){              # corresponds to 10-digit ternary number
    e=3^j                     # place value of the 10-j digit
    N=N+array(c(1,1,2,0,0,2,0,2,1),c(3,3))[f(a[1]/e)+1,f(a[2]/e)+1]*e
                              # working from the inside out:
#     Inside the brackets, the expression "f(a[1]/e)" finds the 10-j digit of a[1]
#     in ternary. In base 10, the digit in the hudreds place of 456 is floor(456/10^2).
#     Just value divided by place value. The same principle works to find digits of base 3.
#     
#     Adding one to the 10-j ternary digit of a[1] and a[2] returns their row-column
#     indexes on the crazy table, which is described by "array(c(1,1,2,0,0,2,0,2,1),c(3,3))"
#     
#     Multiply this new ternary digit by its place value (*e on the end) to get the
#     value in base 10, and add that value to N
        
    a=a%%e                   # Take remainder to prepare to get next digit
  }
  print(N)
}




# as one solid line:

for(i in seq(1,length(x),2)){N=0;a=x[i+0:1];f=floor;for(j in 9:0){e=3^j;N=N+array(c(1,1,2,0,0,2,0,2,1),c(3,3))[f(a[1]/e)+1,f(a[2]/e)+1]*e;a=a%%e};print(N)}

######################## above is 177 bytes


