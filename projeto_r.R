# Read CSV into R
path = getwd()
path = paste(path, 'data', sep='/')
afsa = unlist(read.csv(file=paste(path, 'afsa.csv', sep='/'), header=TRUE, sep=','))
cmfdr = unlist(read.csv(file=paste(path, 'cmfdr.csv', sep='/'), header=TRUE, sep=','))
mfdr = unlist(read.csv(file=paste(path, 'mfdr.csv', sep='/'), header=TRUE, sep=','))
mfd = unlist(read.csv(file=paste(path, 'mfd.csv', sep='/'), header=TRUE, sep=','))

# Wrap data in a dataframe
data = data.frame(afsa, cmfdr, mfdr, mfd)

# Descriptive statistics
afsa_mean = mean(afsa)
afsa_sd = sd(afsa)
afsa_median = median(afsa)

cmfdr_mean = mean(cmfdr)
cmfdr_sd = sd(cmfdr)
cmfdr_median = median(cmfdr)

mfdr_mean = mean(mfdr)
mfdr_sd = sd(mfdr)
mfdr_median = median(mfdr)

mfd_mean = mean(mfd)
mfd_sd = sd(mfd)
mfd_median = median(mfd)

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
files_path
# Draw histograms for samples
pdf(paste(files_path, 'hist_afsa.pdf', sep = ''))
hist(afsa, col="blue")
dev.off()

pdf(paste(files_path, 'hist_cmfdr.pdf', sep = ''))
hist(cmfdr, col="blue")
dev.off()

pdf(paste(files_path, 'hist_mfdr.pdf', sep = ''))
hist(mfdr, col="blue")
dev.off()

pdf(paste(files_path, 'hist_mfd.pdf', sep = ''))
hist(mfd, col="blue")
dev.off()

# Draw boxplots for samples
pdf(paste(files_path, 'boxplot.pdf', sep = ''))
boxplot(data)
dev.off()

# Draw normal probability plot for samples
pdf(paste(files_path, 'norm_afsa.pdf', sep = ''))
qqnorm(afsa, col="blue", pch=16)
qqline(afsa, col="green")
dev.off()

pdf(paste(files_path, 'norm_cmfdr.pdf', sep = ''))
qqnorm(cmfdr, col="blue", pch=16)
qqline(cmfdr, col="green")
dev.off()

pdf(paste(files_path, 'norm_mfdr.pdf', sep = ''))
qqnorm(mfdr, col="blue", pch=16)
qqline(mfdr, col="green")
dev.off()

pdf(paste(files_path, 'norm_mfd.pdf', sep = ''))
qqnorm(mfd, col="blue", pch=16)
qqline(mfd, col="green")
dev.off()


# Shapiro-Wilk test to verify normality of the samples (default alpha = 0.05)
shapiro.test(afsa)
shapiro.test(cmfdr)
shapiro.test(mfdr)
shapiro.test(mfd)

# Kolmogrov-Smirnov test
ks.test(afsa, pnorm(10, mean(afsa), sd(afsa)))
ks.test(cmfdr, pnorm(10, mean(cmfdr), sd(cmfdr)))
ks.test(mfdr, pnorm(10, mean(mfdr), sd(mfdr)))
ks.test(mfd, pnorm(10, mean(mfd), sd(mfd)))


# Hypothesis test for multiple samples


# Hypothesis test for paired samples

