# setwd('<<YOUR WORKING DIRECTORY HERE>>')

Sys.setlocale('LC_CTYPE', 'pt_BR.UTF-8')
# Load libraries
library(nortest)
library(e1071)

# Read CSV into R
path = getwd()
path = paste(path, 'data', sep='/')
AFSA_cMFDR = unlist(read.csv(file=paste(path, 'afsa_cmfdr.csv', sep='/'), header=TRUE, sep=','))
cMFDR = unlist(read.csv(file=paste(path, 'cmfdr.csv', sep='/'), header=TRUE, sep=','))
AFSA_MFDR = unlist(read.csv(file=paste(path, 'afsa_mfdr.csv', sep='/'), header=TRUE, sep=','))
MFDR = unlist(read.csv(file=paste(path, 'mfdr.csv', sep='/'), header=TRUE, sep=','))
AFSA_MFD = unlist(read.csv(file=paste(path, 'afsa_mfd.csv', sep='/'), header=TRUE, sep=','))
MFD = unlist(read.csv(file=paste(path, 'mfd.csv', sep='/'), header=TRUE, sep=','))

# Descriptive statistics
afsa_cmfdr_mean = mean(AFSA_cMFDR)
afsa_cmfdr_sk = skewness(AFSA_cMFDR)
afsa_cmfdr_median = median(AFSA_cMFDR)

cmfdr_mean = mean(cMFDR)
cmfdr_sk = skewness(cMFDR)
cmfdr_median = median(cMFDR)

afsa_mfdr_mean = mean(AFSA_MFDR)
afsa_mfdr_sk = skewness(AFSA_MFDR)
afsa_mfdr_median = median(AFSA_MFDR)

mfdr_mean = mean(MFDR)
mfdr_sk = skewness(MFDR)
mfdr_median = median(MFDR)

afsa_mfd_mean = mean(AFSA_MFD)
afsa_mfd_sk = skewness(AFSA_MFD)
afsa_mfd_median = median(AFSA_MFD)

mfd_mean = mean(MFD)
mfd_sk = skewness(MFD)
mfd_median = median(MFD)

cat('Descriptive statistics\n')
cat('__________________________________________')
cat('Mean\t\t&Median\t\t&Std dev', '\\\\\n',
afsa_cmfdr_mean, '\t&', afsa_cmfdr_median, '\t&', afsa_cmfdr_sk, '\\\\\n',
cmfdr_mean, '\t&', cmfdr_median, '\t&', cmfdr_sk, '\\\\\n',
afsa_mfdr_mean, '\t&', afsa_mfdr_median, '\t&', afsa_mfdr_sk, '\\\\\n',
mfdr_mean, '\t&', mfdr_median, '\t&', mfdr_sk, '\\\\\n',
afsa_mfd_mean, '\t&', afsa_mfd_median, '\t&', afsa_mfd_sk, '\\\\\n',
mfd_mean, '\t&', mfd_median, '\t&', mfd_sk, '\\\\')
cat('__________________________________________')

files_path = paste(getwd(), '/img/', sep = '', col="blue")

# Draw histograms for samples

pdf(paste(files_path, 'histograms.pdf', sep = ''))
par(mfrow=c(3,2))
hist(AFSA_cMFDR, col="blue", xlab='Micro-F1 (x100)', main='AFSA+cMFDR', breaks=c(80, 80.8, 81.6, 82.4, 83.2, 84))
hist(cMFDR, col="blue", xlab='Micro-F1 (x100)', main='cMFDR', breaks=c(80, 80.7, 81.4, 82.1, 82.8, 83.5))
hist(AFSA_MFDR, col="blue", xlab='Micro-F1 (x100)', main='AFSA+MFDR', breaks=c(78.5, 79.2, 79.9, 80.6, 81.3, 82))
hist(MFDR, col="blue", xlab='Micro-F1 (x100)', main='MFDR', breaks=c(78, 78.8, 79.6, 80.4, 81.2, 82))
hist(AFSA_MFD, col="blue", xlab='Micro-F1 (x100)', main='AFSA+MFD', breaks=c(79.5, 80.2, 80.9, 81.6, 82.3, 83))
hist(MFD, col="blue", xlab='Micro-F1 (x100)', main='MFD', breaks=c(80, 80.7, 81.4, 82.1, 82.8, 83.5))
dev.off()

# Wrap data in a dataframe
data = data.frame(AFSA_cMFDR, cMFDR, AFSA_MFDR, MFDR, AFSA_MFD, MFD)

# Draw boxplots for samples
pdf(paste(files_path, 'boxplot.pdf', sep = ''))
boxplot(data, ylab='Micro-F1 (x100)', main=NULL, cex.axis=0.75)
dev.off()

# Draw normal probability plot for samples
pdf(paste(files_path, 'qqplots.pdf', sep = ''))
par(mfrow=c(3,2))
qqnorm(AFSA_cMFDR, col="blue", pch=16, main='AFSA+cMFDR', ylab = 'Quantis amostrais', xlab = 'Quantis teóricos')
qqline(AFSA_cMFDR, col="green")

qqnorm(cMFDR, col="blue", pch=16, main='cMFDR', ylab = 'Quantis amostrais', xlab = 'Quantis teóricos')
qqline(cMFDR, col="green")

qqnorm(AFSA_MFDR, col="blue", pch=16, main='AFSA+MFDR', ylab = 'Quantis amostrais', xlab = 'Quantis teóricos')
qqline(AFSA_MFDR, col="green")

qqnorm(MFDR, col="blue", pch=16, main='MFDR', ylab = 'Quantis amostrais', xlab = 'Quantis teóricos')
qqline(MFDR, col="green")

qqnorm(AFSA_MFD, col="blue", pch=16, main='AFSA+MFD', ylab = 'Quantis amostrais', xlab = 'Quantis teóricos')
qqline(AFSA_MFD, col="green")

qqnorm(MFD, col="blue", pch=16, main='MFD', ylab = 'Quantis amostrais', xlab = 'Quantis teóricos')
qqline(MFD, col="green")
dev.off()


# Shapiro-Wilk test to verify normality of the samples (default alpha = 0.05)
shapiro.test(AFSA_cMFDR)
shapiro.test(cMFDR)
shapiro.test(AFSA_MFDR)
shapiro.test(MFDR)
shapiro.test(AFSA_MFD)
shapiro.test(MFD)

# Anderson-Darling test
ad.test(AFSA_cMFDR)
ad.test(cMFDR)
ad.test(AFSA_MFDR)
ad.test(MFDR)
ad.test(AFSA_MFD)
ad.test(MFD)

cvm.test(AFSA_cMFDR)
cvm.test(cMFDR)
cvm.test(AFSA_MFDR)
cvm.test(MFDR)
cvm.test(AFSA_MFD)
cvm.test(MFD)


# Hypothesis test for paired samples
wilcox.test(AFSA_cMFDR, cMFDR, paired = TRUE, exact = FALSE, alternative = 'greater')
wilcox.test(AFSA_cMFDR, cMFDR, paired = TRUE, exact = FALSE, alternative = 'less')

wilcox.test(AFSA_MFDR, MFDR, paired = TRUE, exact = FALSE, alternative = 'greater')
wilcox.test(AFSA_MFDR, MFDR, paired = TRUE, exact = FALSE, alternative = 'less')

wilcox.test(AFSA_MFD, MFD, paired = TRUE, exact = FALSE, alternative = 'greater')
wilcox.test(AFSA_MFD, MFD, paired = TRUE, exact = FALSE, alternative = 'less')

