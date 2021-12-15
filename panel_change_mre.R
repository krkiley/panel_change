
#Settled dispositions model
y_star <- rnorm(1000, 0, 1)
y1 <- y_star + rnorm(1000, 0, .3)
y2 <- y_star + rnorm(1000, 0, .3)
y3 <- y_star + rnorm(1000, 0, .3)

df <- data.frame(y1 = y1, y2 = y2, y3 = y3)

nls1 <- nls(y3 ~ a + b*p*y2 + b*(1-p)*y1,
            data = df,
            start = list(a = 0, b = 1, p = 0))
#p estimate is ~0.5, meaning waves 1 and 2 are equally predictive of wave3


nls2 <- nls(y3 ~ a + b*.5*y2 + b*(.5)*y1,
            data = df,
            start = list(a = 0, b = 1))

#Calculate BICs for each model
bic1 <- BIC(nls1)
bic2 <- BIC(nls2)

#Calculate probability of active updating model
bicdiff=bic2-bic1
or=exp(bicdiff/2) 
prob_aum=or/(or+1)
prob_aum



#Active updating model
#A Random Walk
y_star <- rnorm(1000, 0, 1)
y1 <- y_star + rnorm(1000, 0, .3)
y2 <- y1 + rnorm(1000, 0, .3)
y3 <- y2 + rnorm(1000, 0, .3)

df <- data.frame(y1 = y1, y2 = y2, y3 = y3)

nls1 <- nls(y3 ~ a + b*p*y2 + b*(1-p)*y1,
            data = df,
            start = list(a = 0, b = 1, p = 0))

nls2 <- nls(y3 ~ a + b*.5*y2 + b*(.5)*y1,
            data = df,
            start = list(a = 0, b = 1))
#p estimate is ~1, meaning wave 1 gives no additional information when wave 2 present

bic1 <- BIC(nls1)
bic2 <- BIC(nls2)

#Calculate probability of active updating model
bicdiff=bic2-bic1
or=exp(bicdiff/2) 
prob_aum=or/(or+1)
prob_aum

