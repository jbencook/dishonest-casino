#Number of steps:
T <- length(rolls)

#Initialize transition probability matrix:
A <- matrix(data=NA, nrow=2, ncol=2)
A[1,1] <- .95
A[1,2] <- .05
A[2,1] <- .1
A[2,2] <- .9

##############
#Solve for pi:
##############

#Coefficient matrix:
a <- matrix(data=NA, nrow=2, ncol=2)
a[1,1] <- A[1,1] - 1
a[1,2] <- A[2,1]
a[2,] <- rep(1,2)

#Right hand side of the linear system:
b <- c(0,1)

#Solve for pi:
pi <- solve(a,b)

#Initialize emission probability matrix:
B <- matrix(data=NA, nrow=2, ncol=6)
B[1,1:6] <- (1/6)
B[2,1:5] <- .1
B[2,6] <- .5

###################
#Viterbi Algorithm:
###################

#Initialization:
delta <- matrix(data=NA, nrow=2, ncol=T)
delta[1,1] <- log(pi[1]*B[1,rolls[1]])
delta[2,1] <- log(pi[2]*B[2,rolls[1]])

psi <- matrix(data=NA, nrow=2, ncol=T)

#Recursion:
for(t in 2:T) {
  delta[1,t] <- max(c(delta[1,t-1] + log(A[1,1]), delta[2,t-1] + log(A[2,1]))) + log(B[1,rolls[t]])
  delta[2,t] <- max(c(delta[1,t-1] + log(A[1,2]), delta[2,t-1] + log(A[2,2]))) + log(B[2,rolls[t]])

  psi[1,t] <- ifelse(delta[1,t-1] + log(A[1,1]) > delta[2,t-1] + log(A[2,1]), yes=1, no=2)
  psi[2,t] <- ifelse(delta[1,t-1] + log(A[1,2]) > delta[2,t-1] + log(A[2,2]), yes=1, no=2)
}

#Termination:
qStar <- NULL
qStar[T] <- ifelse(delta[1,T] > delta[2,T], yes=1, no=2)

#Backtracking:
for(t in (T-1):1) {
  qStar[t] <- psi[qStar[t+1],t+1]
}

#Determine number of correct estimates:
qStar[qStar %in% 1] <- "F"
qStar[qStar %in% 2] <- "L"

predicted <- qStar
actual <- casino

print(table(predicted, actual))
cat(paste(100 * sum(qStar == casino)/T, "% correct\n", sep=""))
