set.seed(1234) #set.seed for reproducibility
lambda <- 0.2
n <- 40
simno <- 1000

ggplot(simdata, aes(simdata)) + geom_histogram(binwidth=1)

hist(rexp(n*totalsim, lambda))

simmeans = NULL
for (i in 1 : 1000) simmeans = c(simmeans, mean(rexp(n, lambda)))
mean(simmeans)
df <- data.frame(simmeans)

g1 <- ggplot(df, aes(x = simmeans))
g1 + geom_histogram(color="black", fill="white") + labs(title = "Distribution of Average of 40 Exponentials", x="Average of 40 Exponentials", y="") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(aes(xintercept = mean(simmeans)), color="blue", linetype= "dashed", size=1) + geom_vline(aes(xintercept = 1/lambda), color="red", linetype= "solid", size=1)

sumtab <- matrix(c(1/lambda, mean(simmeans), 1/lambda/sqrt(n), sd(simmeans), 1/lambda^2/n, var(simmeans)), nrow = 3, ncol = 2, byrow = TRUE)
colnames(sumtab) <- c("Theoretical", "Sample")
rownames(sumtab) <- c("Mean", "Standard Deviation", "Variance")
sumtab

rexpo <- rexp(simno, lambda)
hist(rexpo, main = "Histogram of Random Exponentials", xlab = "Random Exponentials Value", breaks = 30, xlim = c(0,30))
hist(simmeans, main = "Histogram of Averages of 40 Random Exponentials", xlab = "Averages of 40 Random Exponentials", breaks = 20, xlim = c(2,9))
