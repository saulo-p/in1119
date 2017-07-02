# Read CSV into R


path = getwd()
path = paste(path, 'data', sep='/')
AFSA = unlist(read.csv(file=paste(path, 'afsa.csv', sep='/'), header=TRUE, sep=','))
cMFDR = unlist(read.csv(file=paste(path, 'cmfdr.csv', sep='/'), header=TRUE, sep=','))
MFDR = unlist(read.csv(file=paste(path, 'mfdr.csv', sep='/'), header=TRUE, sep=','))
MFD = unlist(read.csv(file=paste(path, 'mfd.csv', sep='/'), header=TRUE, sep=','))

# Wrap data in a dataframe
data = data.frame(AFSA, cMFDR, MFDR, MFD)

# Descriptive statistics
afsa_mean = mean(AFSA)
afsa_sd = sd(AFSA)
afsa_median = median(AFSA)

cmfdr_mean = mean(cMFDR)
cmfdr_sd = sd(cMFDR)
cmfdr_median = median(cMFDR)

mfdr_mean = mean(MFDR)
mfdr_sd = sd(MFDR)
mfdr_median = median(MFDR)

mfd_mean = mean(MFD)
mfd_sd = sd(MFD)
mfd_median = median(MFD)

cat('Descriptive statistics\n')
cat('__________________________________________')
cat('Mean\t\t|Median\t\t|Std dev')
cat('__________________________________________')
cat(afsa_mean, '\t|', afsa_median, '\t|', afsa_sd)
cat(cmfdr_mean, '\t|', cmfdr_median, '\t|', cmfdr_sd)
cat(mfdr_mean, '\t|', mfdr_median, '\t|', mfdr_sd)
cat(mfd_mean, '\t|', mfd_median, '\t|', mfd_sd)
cat('__________________________________________')

files_path = paste(getwd(), '/img/', sep = '', col="blue")

# Draw histograms for samples
pdf(paste(files_path, 'hist_afsa.pdf', sep = ''))
hist(AFSA, col="blue", main=NULL)
dev.off()

pdf(paste(files_path, 'hist_cmfdr.pdf', sep = ''))
hist(cMFDR, col="blue", main=NULL)
dev.off()

pdf(paste(files_path, 'hist_mfdr.pdf', sep = ''))
hist(MFDR, col="blue")
dev.off()

pdf(paste(files_path, 'hist_mfd.pdf', sep = ''))
hist(MFD, col="blue", main=NULL)
dev.off()

# Draw boxplots for samples
pdf(paste(files_path, 'boxplot.pdf', sep = ''))
boxplot(data, ylab='Micro-F1 (x100)', main=NULL)
dev.off()

# Draw normal probability plot for samples
pdf(paste(files_path, 'norm_afsa.pdf', sep = ''))
qqnorm(AFSA, col="blue", pch=16, main=NULL)
qqline(AFSA, col="green")
dev.off()

pdf(paste(files_path, 'norm_cmfdr.pdf', sep = ''))
qqnorm(cMFDR, col="blue", pch=16, main=NULL)
qqline(cMFDR, col="green")
dev.off()

pdf(paste(files_path, 'norm_mfdr.pdf', sep = ''))
qqnorm(MFDR, col="blue", pch=16, main=NULL)
qqline(MFDR, col="green")
dev.off()

pdf(paste(files_path, 'norm_mfd.pdf', sep = ''))
qqnorm(MFD, col="blue", pch=16, main=NULL)
qqline(MFD, col="green")
dev.off()


# Shapiro-Wilk test to verify normality of the samples (default alpha = 0.05)
shapiro.test(AFSA)
shapiro.test(cMFDR)
shapiro.test(MFDR)
shapiro.test(MFD)

# Kolmogrov-Smirnov test
ks.test(AFSA, pnorm(10, mean(AFSA), sd(AFSA)))
ks.test(cMFDR, pnorm(10, mean(cMFDR), sd(cMFDR)))
ks.test(MFDR, pnorm(10, mean(MFDR), sd(MFDR)))
ks.test(MFD, pnorm(10, mean(MFD), sd(MFD)))


# Hypothesis test for multiple samples


# Hypothesis test for paired samples

