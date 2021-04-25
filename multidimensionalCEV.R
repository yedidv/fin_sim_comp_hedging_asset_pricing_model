

set.seed(2546)
# Input paramters:
mu <- matrix(c(0.06,0.06),ncol=1)
print(mu)

vol <- c(0.2,0.3)
print(sigma)

rho <- matrix(c(1,0.5,0.5,1),nrow=2,byrow=T)
print(rho)

# Evaluate Covarance Matrix:
#Sigma <- diag(sigma)%*%rho%*%diag(sigma)
#print(Sigma)

# Cholesky Factorization (note, R produces B such that B'B = Sigma):
#B <- t(chol(Sigma))
#test <- B%*%t(B)
#print(test)

# Define a function to simulate paths:
myCEVD <- function(M,N,d,t,mu,X0,sigma,rho,alpha){
  # Generate 3-dim X array for results:
  S.CEV <- array(NA,dim=c(M,N+1,d))
  #S.Euler <- array(NA,dim=c(M,N+1,d))
  # Set initial values = X0:
  S.CEV[,1,] <- X0
  Sigma <- diag(sigma)%*%rho%*%diag(sigma)
  B <- t(chol(Sigma))
  dt <- t/N
  muMat <- matrix(rep(mu,M),ncol=d,byrow=T)
  # Run the simulation:
  for (i in 1:N){
    Zmat <- matrix(rnorm(d*M),ncol=M)
    # Euler Scheme:
    # GBM :
    #S.Euler[,i+1,] <- S.Euler[,i,] + mu*S.Euler[,i,]*dt +
      #S.Euler[,i,]*sqrt(dt)*t(B%*%Zmat)
    # CEV :
    S.CEV[,i+1,] <- S.CEV[,i,] + muMat*S.CEV[,i,]*dt +
      S.CEV[,i,]^alpha*sqrt(dt)*t(B%*%Zmat)
  }
  return(S.CEV)
}
M <- 10000
N <- 252
d <- 2
t <- 1
X0 <- 100
alpha <- 0.8
sigma <- vol*X0^(1-alpha)
X <- myCEVD(M,N,d,t,mu,X0,sigma,rho,alpha)

