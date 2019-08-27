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

pset <- 1

for(pset in 1:1)
{
  if(pset == 1) check <- seq(from = 0, to = 24, by = 1) #set1
  #if(pset == 1) check <- seq(from = 0, to = 48, by = 2) #set1
  if(pset == 2) check <- seq(from = 1, to = 49, by = 2) #set2
  if(pset == 3) check <- seq(from = 50, to = 98, by = 2) #set3
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
  
  #aspiration <- floor(dat99$allocated.distance / 10)
  #dat99 <- cbind(dat99, aspiration)
  
  dat99.incAsp0 <- dat99[dat99$aspiration >= 0&&dat99$aspiration <= 20,]
  dat99.incAsp1 <- dat99[dat99$aspiration > 20&&dat99$aspiration <= 40,]
  dat99.incAsp2 <- dat99[dat99$aspiration > 40&&dat99$aspiration <= 60,]
  dat99.incAsp3 <- dat99[dat99$aspiration > 60&&dat99$aspiration <= 80,]
  dat99.incAsp4 <- dat99[dat99$aspiration > 80&&dat99$aspiration <= 100,]
  #dat99.incAsp5 <- dat99[dat99$aspiration >= 70&&dat99$aspiration <= 100,]
  
  dat99.moved <- dat99[dat99$have.moved == 0,]
  dat99.movedAsp0 <- dat99.moved[dat99.moved$aspiration >= 0&&dat99$aspiration <= 20,]
  dat99.movedAsp1 <- dat99.moved[dat99.moved$aspiration > 20&&dat99$aspiration <= 40,]
  dat99.movedAsp2 <- dat99.moved[dat99.moved$aspiration > 40&&dat99$aspiration <= 60,]
  dat99.movedAsp3 <- dat99.moved[dat99.moved$aspiration > 60&&dat99$aspiration <= 80,]
  dat99.movedAsp4 <- dat99.moved[dat99.moved$aspiration > 80&&dat99$aspiration <= 100,]
  #dat99.movedAsp5 <- dat99.moved[dat99.moved$aspiration > 70&&dat99$aspiration <= 100,]
  
  incBreaks <- c(min(dat99[,c("hhold.income")]),quantile(dat99[,c("hhold.income")], 0.1),quantile(dat99[,c("hhold.income")], 0.2),quantile(dat99[,c("hhold.income")], 0.3),quantile(dat99[,c("hhold.income")], 0.4),quantile(dat99[,c("hhold.income")], 0.5),quantile(dat99[,c("hhold.income")], 0.6),quantile(dat99[,c("hhold.income")], 0.7),quantile(dat99[,c("hhold.income")], 0.8),quantile(dat99[,c("hhold.income")], 0.9),max(dat99[,c("hhold.income")]))
  incBreaks
  
  incAsp0.counts <- hist(dat99.incAsp0$hhold.income, incBreaks, plot = F)$counts
  incAsp1.counts <- hist(dat99.incAsp1$hhold.income, incBreaks, plot = F)$counts
  incAsp2.counts <- hist(dat99.incAsp2$hhold.income, incBreaks, plot = F)$counts
  incAsp3.counts <- hist(dat99.incAsp3$hhold.income, incBreaks, plot = F)$counts
  incAsp4.counts <- hist(dat99.incAsp4$hhold.income, incBreaks, plot = F)$counts
  #incAsp5.counts <- hist(dat99.incAsp5$hhold.income, incBreaks, plot = F)$counts
  
  movedAsp0.counts <- hist(dat99.movedAsp0$hhold.income, incBreaks, plot = F)$counts
  movedAsp1.counts <- hist(dat99.movedAsp1$hhold.income, incBreaks, plot = F)$counts
  movedAsp2.counts <- hist(dat99.movedAsp2$hhold.income, incBreaks, plot = F)$counts
  movedAsp3.counts <- hist(dat99.movedAsp3$hhold.income, incBreaks, plot = F)$counts
  movedAsp4.counts <- hist(dat99.movedAsp4$hhold.income, incBreaks, plot = F)$counts
  #movedAsp5.counts <- hist(dat99.movedAsp5$hhold.income, incBreaks, plot = F)$counts
  
  
  rm(dat)
  
  
  for(j in 2:length(check))
  {
    i <- check[j]
    
    dat <- read.csv(paste(pathname, exp, i, "/Parents_Data_Exp", i, ".csv", sep =""), skip = 1, header = T)
    
    dat15 <- dat[dat$child.age == 15,]
    dat99 <- dat15[dat15$Tick == 99,]
    
    #aspiration <- floor(dat99$allocated.distance / 10)
    #dat99 <- cbind(dat99, aspiration)
    
    dat99.incAsp0 <- dat99[dat99$aspiration >= 0&&dat99$aspiration <= 20,]
    dat99.incAsp1 <- dat99[dat99$aspiration > 20&&dat99$aspiration <= 40,]
    dat99.incAsp2 <- dat99[dat99$aspiration > 40&&dat99$aspiration <= 60,]
    dat99.incAsp3 <- dat99[dat99$aspiration > 60&&dat99$aspiration <= 80,]
    dat99.incAsp4 <- dat99[dat99$aspiration > 80&&dat99$aspiration <= 100,]
    #dat99.incAsp5 <- dat99[dat99$aspiration > 70&&dat99$aspiration <= 100,]
    
    dat99.moved <- dat99[dat99$have.moved == 0,]
    dat99.movedAsp0 <- dat99.moved[dat99.moved$aspiration >= 0&&dat99$aspiration <= 20,]
    dat99.movedAsp1 <- dat99.moved[dat99.moved$aspiration > 20&&dat99$aspiration <= 40,]
    dat99.movedAsp2 <- dat99.moved[dat99.moved$aspiration > 40&&dat99$aspiration <= 60,]
    dat99.movedAsp3 <- dat99.moved[dat99.moved$aspiration > 60&&dat99$aspiration <= 80,]
    dat99.movedAsp4 <- dat99.moved[dat99.moved$aspiration > 80&&dat99$aspiration <= 100,]
    #dat99.movedAsp5 <- dat99.moved[dat99.moved$aspiration > 70&&dat99$aspiration <= 100,]
    
    incBreaks <- c(min(dat99[,c("hhold.income")]),quantile(dat99[,c("hhold.income")], 0.1),quantile(dat99[,c("hhold.income")], 0.2),quantile(dat99[,c("hhold.income")], 0.3),quantile(dat99[,c("hhold.income")], 0.4),quantile(dat99[,c("hhold.income")], 0.5),quantile(dat99[,c("hhold.income")], 0.6),quantile(dat99[,c("hhold.income")], 0.7),quantile(dat99[,c("hhold.income")], 0.8),quantile(dat99[,c("hhold.income")], 0.9),max(dat99[,c("hhold.income")]))
    
    incBreaks
    
    incAsp0.counts <- cbind(incAsp0.counts, hist(dat99.incAsp0$hhold.income, incBreaks, plot = F)$counts)
    incAsp1.counts <- cbind(incAsp1.counts, hist(dat99.incAsp1$hhold.income, incBreaks, plot = F)$counts)
    incAsp2.counts <- cbind(incAsp2.counts, hist(dat99.incAsp2$hhold.income, incBreaks, plot = F)$counts)
    incAsp3.counts <- cbind(incAsp3.counts, hist(dat99.incAsp3$hhold.income, incBreaks, plot = F)$counts)
    incAsp4.counts <- cbind(incAsp4.counts, hist(dat99.incAsp4$hhold.income, incBreaks, plot = F)$counts)
    #incAsp5.counts <- cbind(incAsp5.counts, hist(dat99.incAsp5$hhold.income, incBreaks, plot = F)$counts)
    
    movedAsp0.counts <- cbind(movedAsp0.counts, hist(dat99.movedAsp0$hhold.income, incBreaks, plot = F)$counts)
    movedAsp1.counts <- cbind(movedAsp1.counts, hist(dat99.movedAsp1$hhold.income, incBreaks, plot = F)$counts)
    movedAsp2.counts <- cbind(movedAsp2.counts, hist(dat99.movedAsp2$hhold.income, incBreaks, plot = F)$counts)
    movedAsp3.counts <- cbind(movedAsp3.counts, hist(dat99.movedAsp3$hhold.income, incBreaks, plot = F)$counts)
    movedAsp4.counts <- cbind(movedAsp4.counts, hist(dat99.movedAsp4$hhold.income, incBreaks, plot = F)$counts)
    #movedAsp5.counts <- cbind(movedAsp5.counts, hist(dat99.movedAsp5$hhold.income, incBreaks, plot = F)$counts)
    
    rm(dat)
    
  }
  
  incAsp0.means <- apply(incAsp0.counts, 1, mean, na.rm = T)
  incAsp1.means <- apply(incAsp1.counts, 1, mean, na.rm = T)
  incAsp2.means <- apply(incAsp2.counts, 1, mean, na.rm = T)
  incAsp3.means <- apply(incAsp3.counts, 1, mean, na.rm = T)
  incAsp4.means <- apply(incAsp4.counts, 1, mean, na.rm = T)
  #incAsp5.means <- apply(incAsp5.counts, 1, mean, na.rm = T)
  
  incAsp0.upper <- apply(incAsp0.counts, 1, quantile, probs = 0.975, na.rm = T)
  incAsp1.upper <- apply(incAsp1.counts, 1, quantile, probs = 0.975, na.rm = T)
  incAsp2.upper <- apply(incAsp2.counts, 1, quantile, probs = 0.975, na.rm = T)
  incAsp3.upper <- apply(incAsp3.counts, 1, quantile, probs = 0.975, na.rm = T)
  incAsp4.upper <- apply(incAsp4.counts, 1, quantile, probs = 0.975, na.rm = T)
  #incAsp5.upper <- apply(incAsp5.counts, 1, quantile, probs = 0.975, na.rm = T)
  
  incAsp0.lower <- apply(incAsp0.counts, 1, quantile, probs = 0.025, na.rm = T)
  incAsp1.lower <- apply(incAsp1.counts, 1, quantile, probs = 0.025, na.rm = T)
  incAsp2.lower <- apply(incAsp2.counts, 1, quantile, probs = 0.025, na.rm = T)
  incAsp3.lower <- apply(incAsp3.counts, 1, quantile, probs = 0.025, na.rm = T)
  incAsp4.lower <- apply(incAsp4.counts, 1, quantile, probs = 0.025, na.rm = T)
  #incAsp5.lower <- apply(incAsp5.counts, 1, quantile, probs = 0.025, na.rm = T)
  
  movedAsp0.means <- apply(movedAsp0.counts, 1, mean, na.rm = T)
  movedAsp1.means <- apply(movedAsp1.counts, 1, mean, na.rm = T)
  movedAsp2.means <- apply(movedAsp2.counts, 1, mean, na.rm = T)
  movedAsp3.means <- apply(movedAsp3.counts, 1, mean, na.rm = T)
  movedAsp4.means <- apply(movedAsp4.counts, 1, mean, na.rm = T)
  #movedAsp5.means <- apply(movedAsp5.counts, 1, mean, na.rm = T)
  
  
  dists <- seq(0, 5, 1)
  
  barcols <- brewer.pal(5, "OrRd")
  asp.dat <- rbind(incAsp0.means, incAsp1.means, incAsp2.means, incAsp3.means, incAsp4.means)
  moved.dat <- rbind(movedAsp0.means, movedAsp1.means, movedAsp2.means, movedAsp3.means, movedAsp4.means)
  
  inc.sums <- apply(asp.dat, 2, sum)
  inc.props <- t(apply(asp.dat, 1, "/", inc.sums))
  
  #  moved.props <- moved.dat / strat.sums
  moved.props <- t(apply(moved.dat, 1, "/", inc.sums))
  
  max.count <- max(inc.props)
  max.countR <- max(pretty(c(0, max.count)))
  
  
  png(paste(pathname, "ParameterSet", pset, "_HHoldIncomeVsAspirationVsMoved_Bars.png", sep = ""), width = 3500, height = 2000)
  par(cex = 3, lwd = 2)
  barplot(inc.props, col = barcols, beside = T, axes = F, ylim = c(0, max.countR), ylab = "Proportion (of Income class)", xlab = "Household Income")
  barplot(inc.props, col = "black", density = 10, angle = 45, add = T, beside = T, axes = F, ylim = c(0, max.countR), ylab = "", xlab = "")
  barplot(moved.props, col = barcols, add = T, beside = T, axes = F)
  
  # error.bar(seq(1.5, 64.5, 7), incAsp0.means, upper = (incAsp0.upper - incAsp0.means), lower = (incAsp0.means - incAsp0.lower), length = 0)
  # error.bar(seq(2.5, 65.5, 7), incAsp1.means, upper = (incAsp1.upper - incAsp1.means), lower = (incAsp1.means - incAsp1.lower), length = 0)
  # error.bar(seq(3.5, 66.5, 7), incAsp2.means, upper = (incAsp2.upper - incAsp2.means), lower = (incAsp2.means - incAsp2.lower), length = 0)
  # error.bar(seq(4.5, 67.5, 7), incAsp3.means, upper = (incAsp3.upper - incAsp3.means), lower = (incAsp3.means - incAsp3.lower), length = 0)
  # error.bar(seq(5.5, 68.5, 7), incAsp4.means, upper = (incAsp4.upper - incAsp4.means), lower = (incAsp4.means - incAsp4.lower), length = 0)
  # error.bar(seq(6.5, 69.5, 7), incAsp5.means, upper = (incAsp5.upper - incAsp5.means), lower = (incAsp5.means - incAsp5.lower), length = 0)
  
  
  axis(2, at = pretty(c(0, max.count)))
  axis(1, at = seq(4, 68, 7), labels = c(min(dat99[,c("hhold.income")]),quantile(dat99[,c("hhold.income")], 0.1),quantile(dat99[,c("hhold.income")], 0.2),quantile(dat99[,c("hhold.income")], 0.3),quantile(dat99[,c("hhold.income")], 0.4),quantile(dat99[,c("hhold.income")], 0.5),quantile(dat99[,c("hhold.income")], 0.6),quantile(dat99[,c("hhold.income")], 0.7),quantile(dat99[,c("hhold.income")], 0.8),quantile(dat99[,c("hhold.income")], 0.9)), tick = F)
  
  legend("topright", title = "Aspiration", c("0-20", "20-40", "40-60", "60-80", "80-100"), fill = barcols)
  #text(60, 0.52, "Hatching = Moved")
  
  dev.off()
  
}