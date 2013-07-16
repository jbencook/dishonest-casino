#The first die is fair:
die <- "F"
casino <- die

cat("Generating 10,000 rolls... \n\n")
rolls <- NULL
for (i in 1:10000){
   if (die == "F"){
      rollx <- sample(1:6,1)
      y <- runif(1)
      if (y <= 0.05) 
      {
         newdie <- "L"
      } else {newdie <- "F"}
   }
   if (die=="L"){
      x <- runif(1)
      x1 <- floor(x*10)+1
      if(x1 <= 5){
         rollx <- x1
      } else{
     rollx <- 6}
   y <- runif(1)
   if (y <= 0.1) {
     newdie <- "F"
   } else {newdie <- "L"}
}

rolls <- c(rolls,rollx)
casino <- c(casino,die)
die <- newdie
}

casino <- casino[1:10000]

rm(list=ls()[which(!(ls() %in% c("casino","rolls")))])
