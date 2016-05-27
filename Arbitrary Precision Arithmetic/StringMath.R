
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
  
    # If highest place value is still zero, drop it.
  if(a[length(a)] == 0){
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

