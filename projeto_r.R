# Load library
library(nortest)

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
afsa_cmfdr_sd = sd(AFSA_cMFDR)
afsa_cmfdr_median = median(AFSA_cMFDR)

cmfdr_mean = mean(cMFDR)
cmfdr_sd = sd(cMFDR)
cmfdr_median = median(cMFDR)

afsa_mfdr_mean = mean(AFSA_MFDR)
afsa_mfdr_sd = sd(AFSA_MFDR)
afsa_mfdr_median = median(AFSA_MFDR)

mfdr_mean = mean(MFDR)
mfdr_sd = sd(MFDR)
mfdr_median = median(MFDR)

afsa_mfd_mean = mean(AFSA_MFD)
afsa_mfd_sd = sd(AFSA_MFD)
afsa_mfd_median = median(AFSA_MFD)

mfd_mean = mean(MFD)
mfd_sd = sd(MFD)
mfd_median = median(MFD)

cat('Descriptive statistics\n')
cat('__________________________________________')
cat('Mean\t\t&Median\t\t&Std dev', '\\\\\n',
afsa_cmfdr_mean, '\t&', afsa_cmfdr_median, '\t&', afsa_cmfdr_sd, '\\\\\n',
cmfdr_mean, '\t&', cmfdr_median, '\t&', cmfdr_sd, '\\\\\n',
afsa_mfdr_mean, '\t&', afsa_mfdr_median, '\t&', afsa_mfdr_sd, '\\\\\n',
mfdr_mean, '\t&', mfdr_median, '\t&', mfdr_sd, '\\\\\n',
afsa_mfd_mean, '\t&', afsa_mfd_median, '\t&', afsa_mfd_sd, '\\\\\n',
mfd_mean, '\t&', mfd_median, '\t&', mfd_sd, '\\\\')
cat('__________________________________________')

files_path = paste(getwd(), '/img/', sep = '', col="blue")

# Draw histograms for samples
pdf(paste(files_path, 'hist_afsa_cmfdr.pdf', sep = ''))
hist(AFSA_cMFDR, col="blue", main=NULL, xlab=NULL)
dev.off()

pdf(paste(files_path, 'hist_cmfdr.pdf', sep = ''))
hist(cMFDR, col="blue", main=NULL, xlab=NULL)
dev.off()

pdf(paste(files_path, 'hist_afsa_mfdr.pdf', sep = ''))
hist(AFSA_MFDR, col="blue", main=NULL, xlab=NULL)
dev.off()

pdf(paste(files_path, 'hist_mfdr.pdf', sep = ''))
hist(MFDR, col="blue", main=NULL, xlab=NULL)
dev.off()

pdf(paste(files_path, 'hist_afsa_mfd.pdf', sep = ''))
hist(AFSA_MFD, col="blue", main=NULL, xlab=NULL)
dev.off()

pdf(paste(files_path, 'hist_mfd.pdf', sep = ''))
hist(MFD, col="blue", main=NULL, xlab=NULL)
dev.off()

# Wrap data in a dataframe
data = data.frame(AFSA_cMFDR, cMFDR, AFSA_MFDR, MFDR, AFSA_MFD, MFD)

# Draw boxplots for samples
pdf(paste(files_path, 'boxplot.pdf', sep = ''))
boxplot(data, ylab='Micro-F1 (x100)', main=NULL, cex.axis=0.75)
dev.off()

# Draw normal probability plot for samples
pdf(paste(files_path, 'norm_afsa_cmfdr.pdf', sep = ''))
qqnorm(AFSA_cMFDR, col="blue", pch=16, main=NULL)
qqline(AFSA_cMFDR, col="green")
dev.off()

pdf(paste(files_path, 'norm_cmfdr.pdf', sep = ''))
qqnorm(cMFDR, col="blue", pch=16, main=NULL)
qqline(cMFDR, col="green")
dev.off()

pdf(paste(files_path, 'norm_afsa_mfdr.pdf', sep = ''))
qqnorm(AFSA_MFDR, col="blue", pch=16, main=NULL)
qqline(AFSA_MFDR, col="green")
dev.off()

pdf(paste(files_path, 'norm_mfdr.pdf', sep = ''))
qqnorm(MFDR, col="blue", pch=16, main=NULL)
qqline(MFDR, col="green")
dev.off()

pdf(paste(files_path, 'norm_afsa_mfd.pdf', sep = ''))
qqnorm(AFSA_MFD, col="blue", pch=16, main=NULL)
qqline(AFSA_MFD, col="green")
dev.off()

pdf(paste(files_path, 'norm_mfd.pdf', sep = ''))
qqnorm(MFD, col="blue", pch=16, main=NULL)
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

