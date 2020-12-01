#install.packages("ismev")               # if not installed!
#library(ismev)
library(magrittr)
library(mgcv)
library(tidyverse)

# Read in the data
anoph <- read_csv('Anopheles clean.csv')[,-c(1:2)]

# eliminate recent years which are data deficient (relatively)
anoph %<>% filter(Year < 2015)

# per species heatmaps of the observations
# (think the next plot might be more useful?)
ggplot(anoph, aes(x = Year, y = Lat)) +
  stat_bin2d() +
  scale_fill_gradient(low = 'blue', high = 'red') +
  facet_wrap(~Species) +
  theme_minimal()

# year as "years from start"
anoph %<>% transform(Year = Year-min(Year))
# species factor for fs later?
anoph %<>% transform(Species = as.factor(Species))


# get the max values, here I'm just arbitrarily picking 
anoph_all <- anoph %>%
  filter(Lat<=0) %>%
  mutate(AbsLat = abs(Lat)) %>%
  group_by(Year, Species) %>%
  slice_max(AbsLat, n = 1, with_ties=FALSE) %>%
  ungroup()

# can we smoosh together N and S hemispheres?
ggplot(anoph_all) +
  geom_line(aes(x=Year+medianYr, y=abs(Lat),
                group=as.factor(sign(Lat)),
                colour=as.factor(sign(Lat)))) +
  geom_point(aes(x=Year+medianYr, y=abs(Lat),
                 colour=as.factor(sign(Lat))), size=0.1, pch=3) +
  theme_minimal() +
  facet_wrap(~Species)
# from this looks like the following will probably never work:
#  - bwambae
#  - vaneedeni
# others may just be data deficient but might work with a factor-smooth
#  approach?

anoph_all %>%
  group_by(Species) %>%
  count() %>%
  arrange(desc(n))


# Pull out 1 species

anoph_1 <- anoph_all %>%
  filter(Species == 'gambiae.S')
#  filter(Species == 'arabiensis')


# lat vs elevation plot
ggplot(anoph_1) +
  geom_line(aes(x=AbsLat, y=elev)) +
  geom_text(aes(x=AbsLat, y=elev, label=Year)) +
  theme_minimal()


# try a gevlss gam:
m1 <- gam(list(AbsLat ~ s(Year, k=20, bs="bs"),
#te(Year, elev, k=c(10, 5), bs="bs"),
                      ~ 1,#s(Year, k=20, bs="bs"),
                      ~ 1),
          data = anoph_1, optimizer="efs",
          family = gevlss())

summary(m1)
plot(m1, pages = 1, all.terms = TRUE, scale=0)

# work out predictions
mu <- fitted(m1)[,1]
rho <- fitted(m1)[,2]
xi <- fitted(m1)[,3]
fv <- mu + exp(rho)*(gamma(1-xi)-1)/xi

# roughly goodness of fit (if the point follow the line
# we're doing well)
plot(fv ~ anoph_1$AbsLat, xlab="Prediction lat", ylab="Observation lat")
abline(a=0,b=1)

# 
plot(anoph_1$AbsLat ~ anoph_1$Year, xlab="Year", ylab="Latitude")
points(fv ~ anoph_1$Year, col='red')



# simulation code from GAM book 2nd ed
# need to figure this out
b <- m1

## GEV inverse cdf.
Fi.gev <- function(z,mu,sigma,xi) {
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





# model go brrrr
m_fs <- gam(list(AbsLat ~ s(Year, Species, k=20, bs="fs"),
                        ~ Species,
                        ~ 1),
            data = anoph_all, optimizer="efs",
            family = gevlss())

# work out predictions, this is mega-janky
mu <- fitted(m_fs)[,1]
rho <- fitted(m_fs)[,2]
xi <- fitted(m_fs)[,3]
anoph_all$fv <- mu + exp(rho)*(gamma(1-xi)-1)/xi

# plot model results?
ggplot(anoph_all) +
  geom_line(aes(x=Year+medianYr, y=AbsLat)) +
  geom_line(aes(x=Year+medianYr, y=fv), colour="red") +
  facet_wrap(~Species) +
  theme_minimal()


