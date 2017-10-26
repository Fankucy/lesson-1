#Definition of VaR based on the probability density function
curve(dnorm(x),from = -3, to = 3, xlab = 'return', ylab = 'p(x)', las = '1',
      main = 'Definition of VaR based on the probability density function', cex.main = 0.8)
abline(v = qnorm(0.05), lty = 2)
#axis(1, at = qnorm(0.05), labels = '')
abline(v = 0, lty = 1)
text(-2, 0.02, '5%')
arrows(0, 0.15, qnorm(0.05), 0.15, length = 0.15)
text(-0.9, 0.17, 'VaR')

#Definition of VaR based on the cumulative distribution function
curve(pnorm(x),from = -3, to = 3, xlab = 'return', ylab = 'F(x)', las = '1',
      main = 'Definition of VaR based on the cumulative distribution function', cex.main = 0.8)
abline(h = 0.05, lty = 2)
axis(2, at = 0.05, labels = '0.05', las = '1', par(cex = 0.6))
