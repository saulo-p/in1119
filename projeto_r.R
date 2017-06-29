# Read CSV into R
afsa = unlist(read.csv(file='/Users/rogeriofragoso/Dropbox/PhD/Disciplinas/Estatística/Projeto/data/afsa.csv', header=TRUE, sep=','))
cmfdr = unlist(read.csv(file='/Users/rogeriofragoso/Dropbox/PhD/Disciplinas/Estatística/Projeto/data/cmfdr.csv', header=TRUE, sep=','))
mfdr = unlist(read.csv(file='/Users/rogeriofragoso/Dropbox/PhD/Disciplinas/Estatística/Projeto/data/mfdr.csv', header=TRUE, sep=','))
mfd = unlist(read.csv(file='/Users/rogeriofragoso/Dropbox/PhD/Disciplinas/Estatística/Projeto/data/mfd.csv', header=TRUE, sep=','))
mean(mfd)

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

files_path = paste(getwd(), '/Dropbox/PhD/Disciplinas/Estatística/Projeto/graphs/', sep = '')
files_path
# Draw histograms for samples
pdf(paste(files_path, 'hist_afsa.pdf', sep = ''))
hist(afsa)
dev.off()

pdf(paste(files_path, 'hist_cmfdr.pdf', sep = ''))
hist(cmfdr)
dev.off()

pdf(paste(files_path, 'hist_mfdr.pdf', sep = ''))
hist(mfdr)
dev.off()

pdf(paste(files_path, 'hist_mfd.pdf', sep = ''))
hist(mfd)
dev.off()

# Draw boxplots for samples
pdf("boxplot.pdf")
boxplot(data)
dev.off()

# Draw normal probability plot for samples
qqnorm(afsa)
qqline(afsa)

qqnorm(cmfdr)
qqline(cmfdr)

qqnorm(mfdr)
qqline(mfdr)

qqnorm(mfd)
qqline(mfd)


# Shapiro-Wilk test to verify normality of the samples
shapiro.test(afsa)
shapiro.test(cmfdr)
shapiro.test(mfdr)
shapiro.test(mfd)

# Kolmogrov-Smirnov test
ks.test(afsa, pnorm(10, mean(afsa), sd(afsa)))
ks.test(cmfdr, pnorm(10, mean(cmfdr), sd(cmfdr)))
ks.test(mfdr, pnorm(10, mean(mfdr), sd(mfdr)))
ks.test(mfd, pnorm(10, mean(mfd), sd(mfd)))



# Hypothesis test for multiple samples (???)


# Hypothesis test for paired samples

