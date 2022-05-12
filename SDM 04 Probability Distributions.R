# RANDOM DISTRIBUTIONS

# Normal distribution: x ~ Normal(??, ??2)

x <- rnorm(n=1000, mean=0, sd=5)
x <- rnorm(n=100, mean=0, sd=5)
x <- rnorm(n=10000, mean=0, sd=5)

hist(x, col="seagreen")
d <- density(x)                               # Returns the density data
plot(d)                                       # Kernel densityplot

# Binomial distribution: x ~ Binomial(p, n)

x <- rbinom(n=1000, size=100, prob=0.5)
x <- rbinom(n=1000, size=1000, prob=0.5)
x <- rbinom(n=1000, size=10, prob=0.5)

d <- density(x)
plot(d) 

# Poisson distribution: x ~ Poisson(??)

x <- rpois(n=1000, lambda=5)
x <- rpois(n=1000, lambda=10)
x <- rpois(n=1000, lambda=1)

# Beta distribution: x ~ Beta(alpha, beta)

x <- rbeta(n=1000, shape1=10, shape2=10)
x <- rbeta(n=1000, shape1=2, shape2=2)
x <- rbeta(n=1000, shape1=5, shape2=2)
x <- rbeta(n=1000, shape1=0.5, shape2=5)

# Gamma distribution: x ~ Gamma(k, ??)

x <- rgamma(n=1000, shape=1, scale=2)
x <- rgamma(n=1000, shape=5, scale=2)
x <- rgamma(n=1000, shape=5, scale=0.5)
x <- rgamma(n=1000, shape=0.5, scale=2)

# Hypergeometric distribution
