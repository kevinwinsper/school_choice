rm(list=ls())
library(RColorBrewer)
library(reshape)
library(gregmisc)


#error bar function for plotting
error.bar <- function(x, y, upper, lower=upper, length,...)
{
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


pathname <- "~/LocalDoc/GitHub/school_choice/export/"
exp <- "Experiment"

pset <- 3

for(pset in 3:3)
{
  if(pset == 1) check <- seq(from = 0, to = 24, by = 1) #set1
  #if(pset == 1) check <- seq(from = 0, to = 48, by = 2) #set1
  if(pset == 2) check <- seq(from = 25, to = 34, by = 1) #set2 more places 110
  if(pset == 3) check <- seq(from = 35, to = 39, by = 1) #set3 more places 120
  if(pset == 4) check <- seq(from = 51, to = 99, by = 2) #set4
  if(pset == 5) check <- seq(from = 100, to = 148, by = 2) #set5
  if(pset == 6) check <- seq(from = 101, to = 149, by = 2) #set6
  if(pset == 7) check <- seq(from = 150, to = 198, by = 2) #set7
  if(pset == 8) check <- seq(from = 151, to = 199, by = 2) #set8
  
  if(pset == 9) check <- seq(from = 200, to = 224, by = 1) #set9
  if(pset == 10) check <- seq(from = 225, to = 249, by = 1) #set10
  
  i <- check[1]
  
  dat <- read.csv(paste(pathname, exp, i, "/Parents_Data_Exp", i, ".csv", sep =""), skip = 1, header = T)
  
  dat15 <- dat[dat$child.age == 15,]
  dat99 <- dat15[dat15$Tick == 99,]
  
  distance.rounded <- floor(dat99$allocated.distance / 10)
  dat99 <- cbind(dat99, distance.rounded)
  
  dat99.incDist0 <- dat99[dat99$distance.rounded == 0,]
  dat99.incDist1 <- dat99[dat99$distance.rounded == 1,]
  dat99.incDist2 <- dat99[dat99$distance.rounded == 2|dat99$distance.rounded == 3,]
  dat99.incDist3 <- dat99[dat99$distance.rounded == 4|dat99$distance.rounded == 5,]
  dat99.incDist4 <- dat99[dat99$distance.rounded == 6|dat99$distance.rounded == 7,]
  dat99.incDist5 <- dat99[dat99$distance.rounded == 8|dat99$distance.rounded == 9,]
  
  dat99.success <- dat99[dat99$success.rank1 == 1,]
  dat99.successDist0 <- dat99.success[dat99.success$distance.rounded == 0,]
  dat99.successDist1 <- dat99.success[dat99.success$distance.rounded == 1,]
  dat99.successDist2 <- dat99.success[dat99.success$distance.rounded == 2|dat99.success$distance.rounded == 3,]
  dat99.successDist3 <- dat99.success[dat99.success$distance.rounded == 4|dat99.success$distance.rounded == 5,]
  dat99.successDist4 <- dat99.success[dat99.success$distance.rounded == 6|dat99.success$distance.rounded == 7,]
  dat99.successDist5 <- dat99.success[dat99.success$distance.rounded == 8|dat99.success$distance.rounded == 9,]
  
  incBreaks <- c(min(dat99[,c("hhold.income")]),quantile(dat99[,c("hhold.income")], 0.1),quantile(dat99[,c("hhold.income")], 0.2),quantile(dat99[,c("hhold.income")], 0.3),quantile(dat99[,c("hhold.income")], 0.4),quantile(dat99[,c("hhold.income")], 0.5),quantile(dat99[,c("hhold.income")], 0.6),quantile(dat99[,c("hhold.income")], 0.7),quantile(dat99[,c("hhold.income")], 0.8),quantile(dat99[,c("hhold.income")], 0.9),max(dat99[,c("hhold.income")]))
  incBreaks
  
  incDist0.counts <- hist(dat99.incDist0$hhold.income, incBreaks, plot = F)$counts
  incDist1.counts <- hist(dat99.incDist1$hhold.income, incBreaks, plot = F)$counts
  incDist2.counts <- hist(dat99.incDist2$hhold.income, incBreaks, plot = F)$counts
  incDist3.counts <- hist(dat99.incDist3$hhold.income, incBreaks, plot = F)$counts
  incDist4.counts <- hist(dat99.incDist4$hhold.income, incBreaks, plot = F)$counts
  incDist5.counts <- hist(dat99.incDist5$hhold.income, incBreaks, plot = F)$counts
  
  successDist0.counts <- hist(dat99.successDist0$hhold.income, incBreaks, plot = F)$counts
  successDist1.counts <- hist(dat99.successDist1$hhold.income, incBreaks, plot = F)$counts
  successDist2.counts <- hist(dat99.successDist2$hhold.income, incBreaks, plot = F)$counts
  successDist3.counts <- hist(dat99.successDist3$hhold.income, incBreaks, plot = F)$counts
  successDist4.counts <- hist(dat99.successDist4$hhold.income, incBreaks, plot = F)$counts
  successDist5.counts <- hist(dat99.successDist5$hhold.income, incBreaks, plot = F)$counts
  
  
  rm(dat)
  
  
  for(j in 2:length(check))
  {
    i <- check[j]
    
    dat <- read.csv(paste(pathname, exp, i, "/Parents_Data_Exp", i, ".csv", sep =""), skip = 1, header = T)
    
    dat15 <- dat[dat$child.age == 15,]
    dat99 <- dat15[dat15$Tick == 99,]
    
    distance.rounded <- floor(dat99$allocated.distance / 10)
    dat99 <- cbind(dat99, distance.rounded)
    
    dat99.incDist0 <- dat99[dat99$distance.rounded == 0,]
    dat99.incDist1 <- dat99[dat99$distance.rounded == 1,]
    dat99.incDist2 <- dat99[dat99$distance.rounded == 2|dat99$distance.rounded == 3,]
    dat99.incDist3 <- dat99[dat99$distance.rounded == 4|dat99$distance.rounded == 5,]
    dat99.incDist4 <- dat99[dat99$distance.rounded == 6|dat99$distance.rounded == 7,]
    dat99.incDist5 <- dat99[dat99$distance.rounded == 8|dat99$distance.rounded == 9,]
    
    dat99.success <- dat99[dat99$success.rank1 == 1,]
    dat99.successDist0 <- dat99.success[dat99.success$distance.rounded == 0,]
    dat99.successDist1 <- dat99.success[dat99.success$distance.rounded == 1,]
    dat99.successDist2 <- dat99.success[dat99.success$distance.rounded == 2|dat99.success$distance.rounded == 3,]
    dat99.successDist3 <- dat99.success[dat99.success$distance.rounded == 4|dat99.success$distance.rounded == 5,]
    dat99.successDist4 <- dat99.success[dat99.success$distance.rounded == 6|dat99.success$distance.rounded == 7,]
    dat99.successDist5 <- dat99.success[dat99.success$distance.rounded == 8|dat99.success$distance.rounded == 9,]
    
    incBreaks <- c(min(dat99[,c("hhold.income")]),quantile(dat99[,c("hhold.income")], 0.1),quantile(dat99[,c("hhold.income")], 0.2),quantile(dat99[,c("hhold.income")], 0.3),quantile(dat99[,c("hhold.income")], 0.4),quantile(dat99[,c("hhold.income")], 0.5),quantile(dat99[,c("hhold.income")], 0.6),quantile(dat99[,c("hhold.income")], 0.7),quantile(dat99[,c("hhold.income")], 0.8),quantile(dat99[,c("hhold.income")], 0.9),max(dat99[,c("hhold.income")]))
    
    incBreaks
    
    incDist0.counts <- cbind(incDist0.counts, hist(dat99.incDist0$hhold.income, incBreaks, plot = F)$counts)
    incDist1.counts <- cbind(incDist1.counts, hist(dat99.incDist1$hhold.income, incBreaks, plot = F)$counts)
    incDist2.counts <- cbind(incDist2.counts, hist(dat99.incDist2$hhold.income, incBreaks, plot = F)$counts)
    incDist3.counts <- cbind(incDist3.counts, hist(dat99.incDist3$hhold.income, incBreaks, plot = F)$counts)
    incDist4.counts <- cbind(incDist4.counts, hist(dat99.incDist4$hhold.income, incBreaks, plot = F)$counts)
    incDist5.counts <- cbind(incDist5.counts, hist(dat99.incDist5$hhold.income, incBreaks, plot = F)$counts)
    
    successDist0.counts <- cbind(successDist0.counts, hist(dat99.successDist0$hhold.income, incBreaks, plot = F)$counts)
    successDist1.counts <- cbind(successDist1.counts, hist(dat99.successDist1$hhold.income, incBreaks, plot = F)$counts)
    successDist2.counts <- cbind(successDist2.counts, hist(dat99.successDist2$hhold.income, incBreaks, plot = F)$counts)
    successDist3.counts <- cbind(successDist3.counts, hist(dat99.successDist3$hhold.income, incBreaks, plot = F)$counts)
    successDist4.counts <- cbind(successDist4.counts, hist(dat99.successDist4$hhold.income, incBreaks, plot = F)$counts)
    successDist5.counts <- cbind(successDist5.counts, hist(dat99.successDist5$hhold.income, incBreaks, plot = F)$counts)
    
    rm(dat)
    
  }
  
  incDist0.means <- apply(incDist0.counts, 1, mean, na.rm = T)
  incDist1.means <- apply(incDist1.counts, 1, mean, na.rm = T)
  incDist2.means <- apply(incDist2.counts, 1, mean, na.rm = T)
  incDist3.means <- apply(incDist3.counts, 1, mean, na.rm = T)
  incDist4.means <- apply(incDist4.counts, 1, mean, na.rm = T)
  incDist5.means <- apply(incDist5.counts, 1, mean, na.rm = T)
  
  incDist0.upper <- apply(incDist0.counts, 1, quantile, probs = 0.975, na.rm = T)
  incDist1.upper <- apply(incDist1.counts, 1, quantile, probs = 0.975, na.rm = T)
  incDist2.upper <- apply(incDist2.counts, 1, quantile, probs = 0.975, na.rm = T)
  incDist3.upper <- apply(incDist3.counts, 1, quantile, probs = 0.975, na.rm = T)
  incDist4.upper <- apply(incDist4.counts, 1, quantile, probs = 0.975, na.rm = T)
  incDist5.upper <- apply(incDist5.counts, 1, quantile, probs = 0.975, na.rm = T)
  
  incDist0.lower <- apply(incDist0.counts, 1, quantile, probs = 0.025, na.rm = T)
  incDist1.lower <- apply(incDist1.counts, 1, quantile, probs = 0.025, na.rm = T)
  incDist2.lower <- apply(incDist2.counts, 1, quantile, probs = 0.025, na.rm = T)
  incDist3.lower <- apply(incDist3.counts, 1, quantile, probs = 0.025, na.rm = T)
  incDist4.lower <- apply(incDist4.counts, 1, quantile, probs = 0.025, na.rm = T)
  incDist5.lower <- apply(incDist5.counts, 1, quantile, probs = 0.025, na.rm = T)
  
  successDist0.means <- apply(successDist0.counts, 1, mean, na.rm = T)
  successDist1.means <- apply(successDist1.counts, 1, mean, na.rm = T)
  successDist2.means <- apply(successDist2.counts, 1, mean, na.rm = T)
  successDist3.means <- apply(successDist3.counts, 1, mean, na.rm = T)
  successDist4.means <- apply(successDist4.counts, 1, mean, na.rm = T)
  successDist5.means <- apply(successDist5.counts, 1, mean, na.rm = T)
  
  
  dists <- seq(0, 5, 1)
  
  barcols <- brewer.pal(6, "BuPu")
  dist.dat <- rbind(incDist0.means, incDist1.means, incDist2.means, incDist3.means, incDist4.means, incDist5.means)
  success.dat <- rbind(successDist0.means, successDist1.means, successDist2.means, successDist3.means, successDist4.means, successDist5.means)
  
  inc.sums <- apply(dist.dat, 2, sum)
  inc.props <- t(apply(dist.dat, 1, "/", inc.sums))
  
  #  success.props <- success.dat / strat.sums
  success.props <- t(apply(success.dat, 1, "/", inc.sums))
  
  max.count <- max(inc.props)
  max.countR <- max(pretty(c(0, max.count)))
  
  
  png(paste(pathname, "More_Places_ParameterSet", pset, "_HHoldIncomeVsDistanceVsSuccess_Bars.png", sep = ""), width = 3500, height = 2000)
  par(cex = 3, lwd = 2)
  barplot(inc.props, col = barcols, beside = T, axes = F, ylim = c(0, max.countR), ylab = "Proportion (of Income class)", xlab = "Household Income")
  barplot(inc.props, col = "black", density = 10, angle = 45, add = T, beside = T, axes = F, ylim = c(0, max.countR), ylab = "", xlab = "")
  barplot(success.props, col = barcols, add = T, beside = T, axes = F)
  
  # error.bar(seq(1.5, 64.5, 7), incDist0.means, upper = (incDist0.upper - incDist0.means), lower = (incDist0.means - incDist0.lower), length = 0)
  # error.bar(seq(2.5, 65.5, 7), incDist1.means, upper = (incDist1.upper - incDist1.means), lower = (incDist1.means - incDist1.lower), length = 0)
  # error.bar(seq(3.5, 66.5, 7), incDist2.means, upper = (incDist2.upper - incDist2.means), lower = (incDist2.means - incDist2.lower), length = 0)
  # error.bar(seq(4.5, 67.5, 7), incDist3.means, upper = (incDist3.upper - incDist3.means), lower = (incDist3.means - incDist3.lower), length = 0)
  # error.bar(seq(5.5, 68.5, 7), incDist4.means, upper = (incDist4.upper - incDist4.means), lower = (incDist4.means - incDist4.lower), length = 0)
  # error.bar(seq(6.5, 69.5, 7), incDist5.means, upper = (incDist5.upper - incDist5.means), lower = (incDist5.means - incDist5.lower), length = 0)
  
  
  axis(2, at = pretty(c(0, max.count)))
  axis(1, at = seq(4, 68, 7), labels = c(min(dat99[,c("hhold.income")]),quantile(dat99[,c("hhold.income")], 0.1),quantile(dat99[,c("hhold.income")], 0.2),quantile(dat99[,c("hhold.income")], 0.3),quantile(dat99[,c("hhold.income")], 0.4),quantile(dat99[,c("hhold.income")], 0.5),quantile(dat99[,c("hhold.income")], 0.6),quantile(dat99[,c("hhold.income")], 0.7),quantile(dat99[,c("hhold.income")], 0.8),quantile(dat99[,c("hhold.income")], 0.9)), tick = F)
  
  legend("topright", title = "Distance", c("0-10", "10-20", "20-40", "40-60", "60-80", "80-100"), fill = barcols)
  text(70, 0.45, "Hatching = Fail")
  
  dev.off()
  
}