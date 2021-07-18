set.seed(32) # initialize random generator
m = 100
a = 2.0
b = 1.0 / 3.0
theta = rgamma(n=m, shape = a, rate = b)
hist(theta, freq = FALSE)
curve(dgamma(x=x, shape=a, rate=b),col="blue", add =TRUE)
#To find Monte Carlo approximation to E(theta)

sum(theta)/m
  # mean MC
mean(theta).    
 # mean real 
 a/b
 
 #forlarge no of size
 m=1e4
 theta = rgamma(n=m, shape = a, rate = b)
 mean(theta)
 var(theta)
 # real var
 a / b^2
 # Let approximate theta is less than 5
 ind = theta <  5.0
 mean(ind)
 pgamma(q=5.0, shape=a, rate=b)
 quantile(x=theta, probs=0.9)
 qgamma(p=0.9,shape=a, rate=b)
 
 #MONTE CARLO ERROR
 
 se = sd(theta) / sqrt(m)
 2.0 * se

ind = theta<5.0
se = sd(ind)/sqrt(m)
2.0 * se

# Marginilisation

m = 10e4

y = numeric(m) 
# create the vectors we will fill in with simulations
phi = numeric(m)

for (i in 1:m) {
  phi[i] = rbeta(n=1, shape1=2.0, shape2=2.0)
  y[i] = rbinom(n=1, size=10, prob=phi[i])
}
# which is equivalent to the following 'vectorized' code
phi = rbeta(n=m, shape1=2.0, shape2=2.0)
y = rbinom(n=m, size=10, prob=phi)
mean(y)
plot(prop.table(table(y)), ylab="P(y)", main="Marginal distribution of y")