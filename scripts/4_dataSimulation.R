### In this script, I create two datasets, each of which represent observations of some Poisson-distributed focal species
### and a set of multivariate-normal environmental variables.
### In each, only one third of these variables has an effect on the focal speices.
### The difference between datasets is that, in the first, all the environmental variables are independent, whereas,
### in the second, they have some non-zero covariance structure.
require(MASS)
## The foundation:

# n = number of variables
n <- 12
# number of observations:
obs <- 100
# mean of observations; mu = 0 assumes variables are centered
mu <- rep(0, n)
# true beta coefficients for each of n variables:
# (create a vector of 0s, then give 1/3rd of them random values between -1 and 1)
B <- rep(0, n)
B[sample(1:n, round(n/3))] <- runif(round(n/3), -1, 1)

## Dataset 1: independent Xs:

# Sigma = covariance matrix of variables; first we will use all independent variables:
sig.ind <- diag(1, n)

# This creates a matrix of observations of n independent, multivariate normal observations
# If you want them to be literally orthogonal within the sample,
# as opposed to sampled from independent distributions, set empirical = T
X.ind <- mvrnorm(obs, mu, sig.ind, empirical = F)

# Here we simulate observations of a Poisson (i.e. randomly) distributed species
Y.ind <- rpois(obs, lambda = exp(X.ind %*% B))

dat1 <- data.frame(cbind(Y.ind, X.ind))
names(dat1) <- c('y', paste0('x', 1:n))


## Dataset 2: correlated Xs

# First, a random matrix with n columns:
A <- matrix(runif(n^2, min = -2, max = 2), ncol=n)
# Then the covariance of that matrix:
CovA <- t(A) %*% A
# Lastly, scale these to variance = 1 (i.e. a correlation matrix - this will keep our Xs on the unit scale)
CorA <- cov2cor(CovA)

# Now create our environmental variables as before, but with our randomly defined covariance strucure:
X.cor <- mvrnorm(100, rep(0,n), CorA)

# and our observations, as before:
Y.cor <- rpois(obs, lambda = exp(X.cor %*% B))

dat2 <- data.frame(cbind(Y.cor, X.cor))
names(dat2) <- c('y', paste0('x', 1:n))

# Get rid of the pesky intermediates:
rm('A', 'CorA', 'CovA', 'sig.ind', 'X.cor', 'X.ind', 'mu', 'n', 'obs', 'Y.cor', 'Y.ind')
