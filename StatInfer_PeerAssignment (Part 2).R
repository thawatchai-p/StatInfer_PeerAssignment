library(datasets)
library(ggplot2)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

str(ToothGrowth)

qplot(dose,len,data=ToothGrowth, facets=~supp, main="Tooth growth of guinea pigs affected by supplement type and dosage",xlab="Dosage", ylab="Tooth length") + geom_boxplot(aes(fill=dose)) + theme(plot.title = element_text(hjust = 0.5))

norm.plot <- ggplot(data = ToothGrowth, aes(x=len))
norm.plot + geom_density(color="darkblue", fill="lightblue") + facet_grid(. ~supp) + geom_vline(aes(xintercept=mean(len)), color="blue", linetype="dashed", size=1) + labs(title = "Density plot of tooth length", x = "Tooth length") + theme(plot.title = element_text(hjust = 0.5))

t.test(len ~ supp, data = ToothGrowth, paired = FALSE, var.equal = FALSE)

dose05 <- subset(ToothGrowth, dose == 0.5)
t.test(len ~ supp, data = dose05, paired = FALSE, var.equal = FALSE)
dose10 <- subset(ToothGrowth, dose == 1.0)
t.test(len ~ supp, data = dose10, paired = FALSE, var.equal = FALSE)
dose20 <- subset(ToothGrowth, dose == 2.0)
t.test(len ~ supp, data = dose20, paired = FALSE, var.equal = FALSE)