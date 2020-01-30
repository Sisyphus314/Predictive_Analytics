#8200
#HW1
#Jan 27, 2020

#Q1

#1.a
s <- c(rep(0,100),
       10*exp(-(1:100)/20)*cos(2*pi*1:100/4))
s

x <- ts(s + rnorm(200, 0, 1))
x

plot(x)

#1.b
s2 <- c(rep(0,100),
        10*exp(-(1:100)/200)*cos(2*pi*1:100/4))
x2 <- ts(s2 + rnorm(200, 0, 1))

plot(x2)

#1.c
# plot(x) is similar to the explosion series because it has a peak and then
# the waves fade away

# plot(x2) is close to the earthquake since the graphs is has more ups and downs
# and the variations is almost constant

#Q3
rm(list = ls())
par(mfrow=c(2,1))
require(astsa)
?oil;?gas
oil
gas
plot(oil);plot(gas)
#a
#there are not stationary time series because there are trends.

#b
par(mfrow=c(2,1))
change <- function(x){
  x_len = length(x)
  x_per_change = log(x[2:x_len]) - log(oil[1:(x_len-1)])
  plot(ts(x_per_change))
  return(ts(x_per_change))
}

oil_perc_change = change(oil)
gas_perc_change = change(gas)

acf(oil_perc_change,lag = 120)
acf(gas_perc_change, lag = 120)

#Q4

rm(list = ls())
par(mfrow=c(1,1))
sigma_w = 1
wt = rnorm(1000,1,sigma_w^2)
wt_neg = wt[1:998]
wt_pos = wt[3:1000]
wt_0 = wt[2:999]
xt = wt_neg + 2 * wt_0 + wt_pos 
plot(ts(xt))

acf(xt,lag=100)

