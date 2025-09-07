setwd('~/Documents/My Writeups/Analytics/boot_gld/')
png('Compare_Dens.png', 850, 850)
par(cex = 2.2)
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
dev.off()

png('ML Estimates.png', 850, 850)
par(mfrow= c(2,2))
for(i in 1:4)
        plot(density(ml_gld$t[,i]),
                main = paste('Density ',"\u03BB" ,i, sep = ''))
dev.off()
