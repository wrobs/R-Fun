
# online arbitrary precision calculator confirms all results.
# however it works faster than the strMultiply function.
##############################################################


# Add 2 positive integers of arbitrary length,
# where integers are entered as strings.

strAdd<-function(number1, number2){    
  
    # Order strings by length
  num <- c(number1,number2)
  num <- num[order(nchar(num))]
  
    # Turn strings into numeric vectors
  a <- as.numeric(strsplit(num[2], split="")[[1]])
  b <- as.numeric(strsplit(num[1], split="")[[1]])
  last <- length(b)
  
    # Tack leading zero on "a" (longer number) in case 
    # carrying is needed there
    # and tack enough zeroes on "b" to match length.
  a <- c(0, a)
  b <- c(rep(0, length(a)-length(b)), b)
  

    # -set carry to initial value zero
    # -for each place value from ones up, add a, b, and "carry"
    # -if sum breaks 10, take last digit and carry one, else
    #  do not carry.
    # repeat this for the meaningful digits of b only
  carry <- 0
  len <-length(a)
  for(i in len:(len-last+1)){
    a[i] <- a[i] + b[i] + carry
    if(a[i] >= 10){
      a[i] <- a[i] - 10
      carry <- 1
    } else{
      carry <- 0
    }
  }

  
    # continue carries as needed.
    # (looks redundant but setting it up like this
    # eliminates a lot of unneccesary operations)
  
  k <- len-last
  while(carry == 1){
    a[k] <- a[k] + carry
    if(a[k] >= 10){
      a[k] <- a[k] - 10
      carry <- 1
    } else{
      carry <- 0
    }
    k <- k-1
  }
  
    # Drop leading zeroes.
  while((length(a) > 1) && (a[1]==0)){
    a <- tail(a, -1)
  }
  

    # convert back to character vector, and collapse to
    # one string.
  a <- as.character(a)    
  a <- paste(a, collapse="")
  
  a  
}


strAdd("99","1")

strAdd("9999999","67")

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

    # Split as before. No need for ordering by length this time.
  a <- as.numeric(strsplit(number1, split="")[[1]])
  b <- as.numeric(strsplit(number2, split="")[[1]])
  
    # Declare length variables just to avoid calculating the same
    # thing a million times in loops.
  len.a <- length(a)
  len.b <- length(b)
  len.t <- len.a+len.b
  

    # Sum will be, at most, len.a+len.b digits long.
    # set it as a vector of zeroes that long. It will
    # be added to procedurally.
  sum <-  rep(0, len.t)
  
    # Multiplication is performed the same way we
    # learned in gradeschool. Each digit of the first number
    # is multiplied by each digit of the second number, and
    # the result is aligned and added to its correct place value.
  for(j in len.a:1){
    for(k in len.b:1){
      
        # Digits are multiplied, and result isconverted to vector 
        # of 2 digits for adding.
      digiprod <- a[len.a-j+1]*b[len.b-k+1]
      digiprod <- c(digiprod%/%10, digiprod%%10)
      
        # Addition is performed just like strAdd, with a slight
        # difference: the function starts adding the new addend
        # to "sum" at its proper place value, determined by the
        # place values of the digits in the original 2 numbers.
        # This is what the convoluted-looking "index.t" is doing.
      carry <- 0
      for(i in 0:1){
        index.t <- len.t+2-(j+k)-i
        sum[index.t] <- digiprod[2-i] + sum[index.t] + carry
        if(sum[index.t] >= 10){
          sum[index.t] <- sum[index.t] - 10
          carry <- 1
        } else{
          carry <- 0
        }
      }
      
      
        # Continue carries just like strAdd      
      i <- len.t-(j+k)
      while(carry == 1){
        sum[i] <- sum[i] + carry
        if(sum[i] >= 10){
          sum[i] <- sum[i] - 10
          carry <- 1
        } else{
          carry <- 0
        }
        i <- (i-1)
      }      
    }
  }
  
    # Drop leading zeroes.
  while((length(sum) > 1) && (sum[1]==0)){
    sum <- tail(sum, -1)
  }
  
    # Convert back to character vector, and collapse to
    # one string.
  sum <- as.character(sum)    
  sum <- paste(sum, collapse="")
  
  
  sum
}
 
strMultiply("101456789987654334567890987654345678978976544567890789876545678900987",
            "101567898765434567890987654323456789098766789987654356789098765")
# [1] "10304752974532030916456304161130323458934741269340249027374547734573612066334382680922081772118755703959657866736800936227398981055"





