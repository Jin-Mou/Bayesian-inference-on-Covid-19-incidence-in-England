#### Jin Mou  s2103110

### 1. prepare data
# daily hospital deaths with Covid in England for the first 100 days from March 2nd 2020
y <-c(1,2,0,2,2,0,4,4,1,9,14,20,22,27,40,46,66,63,105,103,149,159,204,263,326,353,360,437,498,576,645,647,
      700,778,743,727,813,900,792,740,779,718,699,646,686,639,610,571,524,566,486,502,451,438,386,380,345,341,
      325,313,306,268,251,259,252,266,256,215,203,196,166,183,162,180,172,167,138,160,144,153,150,122,128,116,
      133,139,122,124,117,92,83,94,109,111,83,86,83,80,73,69)
# add 20 days of zero deaths to the start of the data
y <- c(rep(0, 20), y)
N <- length(y)

## build matrix B
B <- matrix(0, N, N)
for (i in 1:N) {
  for (j in 1:N) {
    if(j <= i) {
      B[i, j] <- dlnorm(i-j, 3.235, 0.4147)
    } else B[i, j] <-0
  }
}

# make the same "random" results when the code is run
set.seed(1)

### do jags
# load rjags (and coda)
library(rjags) 
# build model
model <- jags.model("model.jags",data=list(B=B, N=N, y=y))
sample_coda <- coda.samples(model,c("n", "m", "tau"),thin=50, n.iter=10000)


## 4A: 8.3*11.7
pdf("final plot.pdf", width=8.3, height=11.7, paper="a4")

## diagnostics plots
plot(sample_coda[[1]][,c(121, 150,220)])
## trace and density plots 
# first, We select n[1], n[30], n[100] to analyze,
# since n[1] and n[2] have different distributions from distribution of others,
# n[30] is almost a turning point, after which the trajectories of n[i]s become stable 
# and n[100] is the end point of our research.
# (the last 20 n are unstable)
# We can see the trace plot of n[1] is not stable, 
# however, the trace plot of n[30] is almost like white noise,
# and the trace plot of n[100] becomes stable after some iterations.
# Therefore, maybe we can try 3000 as burn-in period and extend our iteration to
# about 40000 to see whether n[1] becomes stable after some iterations.

acfplot(sample_coda[[1]][,c(121, 122, 140, 200)], aspect=1)
## ACF plots
# We select n[1], n[2], n[20], n[80] as targets
# Although we have thinned our data, value of n[i] (i<20, i>=80, a rough region) 
# may still be autocorrelated. 
# To fix it, we can thin the data by 100 steps.




dev.off()

### draw the summary plots
## compute data
# the posterior mean for m trajectory
m_mean <- colMeans(sample_coda[[1]][,1:120])
# the posterior mean for n trajectory
n_mean <- colMeans(sample_coda[[1]][,121:240])
# the upper bound of 95% credible interval for n trajectory
n_upper <- HPDinterval(sample_coda[[1]][,121:240])[,2]
# the lower bound of 95% credible interval for n trajectory
n_lower <- HPDinterval(sample_coda[[1]][,121:240])[,1]
# compute the number of days before sample in 2020
days_bef <- julian(as.Date("2020-3-2"),origin=as.Date("2019-12-31")) - 21
# corresponding days of sample
days_sam <- 1:N + days_bef

## draw
# daily incidences and deaths against days of the year
plot(days_sam[1:100], n_mean[1:100], ylim=c(0,2500), xlim=c(1,200), 
     xlab="Day", ylab="People", pch=1, main="Daily incidences and deaths")
lines(days_sam[21:120], m_mean[21:120], col="green", lty=1)
# draw upper and lower bounds
lines(days_sam[1:100], n_upper[1:100], col="blue", lty=2)
lines(days_sam[1:100], n_lower[1:100], col="red", lty=2)

# draw legends
legend(140, 2400, legend=c("mean(n)", "upper bound(95% n)", "lower bound(95% n)", 
                           "mean(m)"),
       col=c("black","blue", "red", "green"), pch=c(1, NA, NA, NA), lty=c(NA, 2, 2, 1), 
       cex=0.6)

# mark lockdown
lockdown_day <- julian(as.Date("2020-3-24"),origin=as.Date("2019-12-31"))
abline(v=lockdown_day, lty=2)
text(lockdown_day, 2350, labels="UK lockdown", cex=0.6)

