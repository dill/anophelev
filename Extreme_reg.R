#install.packages("ismev")               # if not installed!
library(ismev)
library(magrittr)
library(mgcv)
library(tidyverse)

# Read in the data
anoph <- read_csv('Anopheles clean.csv')[,-c(1:2)]
anoph

# Recenter years, and eliminate recent years which are data deficient (relatively)

#anoph %<>% mutate(Year = (Year - 1900))

hist(anoph$Year)
table(anoph$Year)
#anoph %<>% filter(Year < 115)
anoph %<>% filter(Year < 2015)

# Pull out wellcomei and check it out

well <- anoph %>%
  filter(Species == 'wellcomei')

ggplot(well, aes(x = Year, y = Lat)) + #geom_point(alpha = 0.3) +
  stat_binhex() + scale_fill_gradient(low = 'blue', high = 'red')

# Pull out Southern latitudes (reverse sign), one max per year, and run a regression just as a check

well.max <- well %>%
  mutate(AbsLat = -1*Lat) %>%
  group_by(Year) %>%
  slice_max(AbsLat, n = 1, with_ties=FALSE)

well.lm<- lm(AbsLat ~ Year, data = well.max)
summary(well.lm)

plot(AbsLat ~ Year, well.max)
abline(well.lm)

# Now... how to do this as a GAM 

m1 <- gam(list(AbsLat ~ s(Year, k=40),
               ~ s(Year, k=20),
               ~ 1),
          data = well.max, optimizer="efs",
          family = gevlss())

summary(m1)
plot(m1, pages = 1, all.terms = TRUE, scale=0)

# Is this correct? 

mu <- fitted(m1)[,1]
rho <- fitted(m1)[,2]
xi <- fitted(m1)[,3]
fv <- mu + exp(rho)*(gamma(1-xi)-1)/xi

plot(fv ~ well.max$AbsLat)

plot(well.max$AbsLat ~ well.max$Year)
points(fv ~ well.max$Year, col='red')



# simulation code from GAM book 2nd ed
# need to figure this out
b <- m1

Fi.gev <- function(z,mu,sigma,xi) { ## GEV inverse cdf.
  xi[abs(xi)<1e-8] <- 1e-8 ## approximate xi=0, by small xi
  x <- mu + ((-log(z))^-xi-1)*sigma/xi
}
mb <- coef(b);Vb <- vcov(b) ## posterior mean and cov
b1 <- b ## copy fitted model object to modify
n.rep <- 1000; br <- rmvn(n.rep,mb,Vb) ## posterior sim
n <- length(fitted(b))
sim.dat <- cbind(data.frame(rep(0,n*n.rep)), well.max$Year)
for (i in 1:n.rep) {
  b1$coefficients <- br[i,] ## copy sim coefs to gam object
  X <- predict(b1,type="response");ii <- 1:n + (i-1)*n
  sim.dat[ii,1] <- Fi.gev(runif(n),X[,1],exp(X[,2]),X[,3])
}


stm <- tapply(sim.dat[,1],sim.dat[,2],mean)
st98 <- tapply(sim.dat[,1],sim.dat[,2],quantile,probs=.98)

par(mfrow=c(1,2))
plot(well.max$AbsLat, stm)
abline(a=0,b=1)
plot(well.max$AbsLat, st98)
abline(a=0,b=1)
