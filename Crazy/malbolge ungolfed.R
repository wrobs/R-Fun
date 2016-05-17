
### expected input format
mb<-c("0 0","29524 29524","59048 59048","1131 11355","11355 1131")


### testing with 11355 and 1131,
### convert base10 to base3
b3a<-c()
b3b<-c()
b10a<-11355
b10b<-1131
for(j in 9:0){
      # This divides the current value by the base 3
      # place value, stores the rounded-down result
      # as the base 3 digit, finds the remainder,
      # and begins again on the remander.
  b3a<-c(b3a,floor(b10a/3^j))
  b3b<-c(b3b,floor(b10b/3^j))
  b10a<-b10a%%3^j
  b10b<-b10b%%3^j    
}
b3a
b3b

### Check base-3 digits on matrix and store
### base 3 digits of new number.
crazy<-array(c(1,1,2,0,0,2,0,2,1),c(3,3))
newtrits<-c()
for (j in 9:0){
  newtrits<-c(newtrits, crazy[b3a[10-j]+1,b3b[10-j]+1])
}
newtrits

### ...and back to base 10
b10<-0  
for(j in 9:0){
    # multiplies each base 3 digit by its place value, summing
    # to the final value in base 10
  b10<-b10+newtrits[10-j]*3^j
}
b10

