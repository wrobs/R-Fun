# Todo: strMultiply apparently works fine with leading zeroes,
#       even though it seems like it shouldn't... 
#
#
# online arbitrary precision calculator confirms all results.
# however it works MUCH faster than the strMultiply function.
##############################################################


# Add 2 positive integers of arbitrary length,
# where integers are entered as strings.

strAdd<-function(number1, number2){
  
  
  
    # Order strings by length
  num <- c(number1,number2)
  num <- num[order(nchar(num))]
  
    # Turn strings into numeric vectors and reverse their order
    # (reversing makes place value and carrying simpler).
  a <- rev(as.numeric(strsplit(num[2], split="")[[1]]))
  b <- rev(as.numeric(strsplit(num[1], split="")[[1]]))
  
    # Tack extra zero on "a" in case carrying is needed there
    # and tack enough zeroes on "b" to match length.
  a <- c(a, 0)
  b <- c(b, rep(0, length(a)-length(b)))
  

    # -set carry to initial value zero
    # -for each place value from ones up, add a, b, and "carry"
    # -if sum breaks 10, take last digit and carry one, else
    #  do not carry.
  carry <- 0
  for(i in 1:length(a)){
    a[i] <- a[i] + b[i] + carry
    if(a[i] >= 10){
      a[i] <- a[i] - 10
      carry <- 1
    } else{
      carry <- 0
    }
  }
  
    # Drop "leading" zeroes. (trailing zeroes?
    # since the digit order is reversed.)
  while((length(a) > 1) && (a[length(a)]==0)){
    a <- head(a, -1)
  }
  
    # Reverse vector back to normal order, convert
    # back to character vector, and collapse to
    # one string.
  a <- as.character(rev(a))    
  a <- paste(a, collapse="")
  
  a  
}


strAdd("2","2")
# [1] "4"

strAdd("99","24")
# [1] "123"

strAdd("123456789098765432123456789987654321234567809876542123456789098765432345678909876543",
       "5678909876543345678987654323456789009876543345678987654567898765434567890987654323567898")
# [1] "5679033333332444444419777780246776664197777913488864196691355554533333323333333233444441"







# Multiply 2 positive integers of arbitrary length,
# where integers are entered as strings.


strMultiply<- function(number1, number2){

    # Reverse as before. No need for ordering by length this time.
  a <- rev(as.numeric(strsplit(number1, split="")[[1]]))
  b <- rev(as.numeric(strsplit(number2, split="")[[1]]))
  
    # sum begins at zero and is increased with each pair of digits
    # multiplied. Multiplied digits are given appropriate trailing
    # zeroes for their respective place values.
    # To avoid bit overflow, addends are collapsed to string values
    # and added with strAdd.
  sum<-"0"
  for(j in 1:length(a)){
    for(k in 1: length(b)){
      digitprod <- b[k]*a[j]
      prodchar <- as.character(digitprod)
      zeroes <- paste(rep("0", (j+k)-2), collapse="")
      addend <- paste(prodchar, zeroes, sep="")
      sum <- strAdd(sum, addend)      
    }
  }
  
  sum
}

 
strMultiply("101456789987654334567890987654345678978976544567890789876545678900987",
            "101567898765434567890987654323456789098766789987654356789098765")
# [1] "10304752974532030916456304161130323458934741269340249027374547734573612066334382680922081772118755703959657866736800936227398981055"