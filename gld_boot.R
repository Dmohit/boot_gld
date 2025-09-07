library(quantmod)
library(gld)
library(boot)
#can try MSFT, NVDA, AMD
getSymbols('GME', from = "2024-06-01", to = "2025-06-30",
            periodicity = "daily")

l_returns <- diff(log(GME$GME.Adjusted), 1)
l_returns <- l_returns[-1]

m <- mean(l_returns)
s <- sd(l_returns)

mlr <- fit.fkml(l_returns, method = 'ml')

set.seed(1008)
ml_gld <- boot(l_returns,
                statistic = function(x) fit.fkml(x, method = 'ml')$lambda,
                R = 150,
                sim = 'parametric',
                ran.gen = function(data, y) rgl(length(data), y[1], y[2], y[3], y[4]),
                mle = mlr$lambda)

bl1 <- boot.ci(ml_gld, type = 'perc', index = 1, conf = 1-0.05/4)
bl2 <- boot.ci(ml_gld, type = 'perc', index = 2, conf = 1-0.05/4)
bl3 <- boot.ci(ml_gld, type = 'perc', index = 3, conf = 1-0.05/4)
bl4 <- boot.ci(ml_gld, type = 'perc', index = 4, conf = 1-0.05/4)

par(mfrow= c(2,2))
for(i in 1:4)
        plot(density(ml_gld$t[,i]),
                main = paste('Density ',"\u03BB" ,i, sep = ''))

gld.moments(par = mlr$lambda)
gld.moments(par = c(bl1$percent[4], bl2$percent[4], bl3$percent[4], bl4$percent[4]))
gld.moments(par = c(bl1$percent[5], bl2$percent[5], bl3$percent[5], bl4$percent[5]))

c(lambda1 = bl1$percent[4], lambda2 =  bl2$percent[4],
        lambda3 = bl3$percent[4], lambda4 =  bl4$percent[4])

c(lambda1 = bl1$percent[5], lambda2 =  bl2$percent[5],
        lambda3 = bl3$percent[5], lambda4 =  bl4$percent[5])

dev.new()
hist(l_returns, main = 'Comparing Densities', breaks = 'FD', freq = FALSE,
    ylim = c(0, 32), xlim = c(-0.3, 0.3), col = 'lightyellow')
chosen <- mlr
plotgld(lambda1 = chosen$lambda[1], lambda2 = chosen$lambda[2],
        lambda3 = chosen$lambda[3], lambda4 = chosen$lambda[4],
        param = "fmkl", add = TRUE, lty = 'dashed', lwd = 3.1)
plotgld(lambda1 = bl1$percent[4], lambda2 = bl2$percent[4],
        lambda3 = bl3$percent[4], lambda4 = bl4$percent[4],
        param = "fmkl", add = TRUE, col = 'blue', lty = 'dashed', lwd = 3.1)
plotgld(lambda1 = bl1$percent[5], lambda2 = bl2$percent[5],
        lambda3 = bl3$percent[5], lambda4 = bl4$percent[5],
        param = "fmkl", add = TRUE, col = 'orange', lty = 'dashed', lwd = 3.1)
curve(dnorm(x, mean = m, sd = s), add = TRUE, col = 'green', lwd = 3.1)
x <- seq(-0.3, 0.3, 0.01)
transparent_green <- rgb(0, 170, 12, alpha = 60, maxColorValue = 255)
polygon(x, dnorm(x, mean = m, sd = s), col = transparent_green)
legend('topleft', lwd = 3.1, lty = 'dashed', bty = 'n',
        col = c('black', 'blue', 'orange', 'green'),
        legend = c('ML Estimate', 'Lower Bound', 'Upper Bound', 'Normal Density'))
