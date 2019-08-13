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


#pathname <- "C:/Users/James/Dropbox/Research/OngoingProjects/SchoolChoice/"
#pathname <- "J:/SchoolChoice/"
pathname <- "~/Documents/GitHub/school_choice"
exp <- "Experiment"

#win.pathname <- "C:\\Users\\James\\Dropbox\\Research\\OngoingProjects\\SchoolChoice\\"
#win.pathname <- "J:\\SchoolChoice\\"

pset <- 1

#for(pset in 1:1)
#{
  #if(pset == 1) check <- seq(from = 0, to = 8, by = 2) #set1
  if(pset == 1) check <- seq(from = 0, to = 48, by = 2) #set1
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
  
  #shell(paste("\"C:\\Program Files\\WinRAR\\WinRAR.exe\"", "E -ibck -o+", paste(win.pathname, exp, i, "\\Parents_Data.rar", sep =""), paste(win.pathname, exp, i, sep ="")), wait = T)
  #dat <- read.csv(paste(win.pathname, exp, i, "\\Parents_Data_Exp", i, ".csv", sep =""), skip = 1, header = T)
  dat <- read.csv(paste(pathname,"Parents_Data_Exp", i, ".csv", sep =""), skip = 1, header = T)
  #shell(paste("DEL /Q ", paste(win.pathname, exp, i, "\\Parents_Data_Exp", i, ".csv", sep ="")))
  
  dat15 <- dat[dat$child.age == 15,]
  dat15 <- dat15[dat15$Tick > 20,]
  dat.final <- dat15[dat15$Tick = 99,]
  
  attainment.delta <- dat15$child.attainment - dat15$aspiration
  dat15 <- cbind(dat15, attainment.delta)
  
  aspiration.rounded <- floor(dat15$aspiration / 20)
  dat15 <- cbind(dat15, aspiration.rounded)
  
  dat15.attAsp0 <- dat15[dat15$aspiration.rounded == 0,]
  dat15.attAsp1 <- dat15[dat15$aspiration.rounded == 1,]
  dat15.attAsp2 <- dat15[dat15$aspiration.rounded == 2,]
  dat15.attAsp3 <- dat15[dat15$aspiration.rounded == 3,]
  dat15.attAsp4 <- dat15[dat15$aspiration.rounded == 4,]

  
  dat15.success <- dat15[dat15$success.rank1 == 1,]
  dat15.successAsp0 <- dat15.success[dat15.success$aspiration.rounded == 0,]
  dat15.successAsp1 <- dat15.success[dat15.success$aspiration.rounded == 1,]
  dat15.successAsp2 <- dat15.success[dat15.success$aspiration.rounded == 2,]
  dat15.successAsp3 <- dat15.success[dat15.success$aspiration.rounded == 3,]
  dat15.successAsp4 <- dat15.success[dat15.success$aspiration.rounded == 4,]
  
 # dat15.sD0_lowAtt <- dat15.success[dat15.success$distance.rounded == 0 & dat15.success$attainment.delta < -10,]
  
  
  attBreaks <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
  
  attAsp0.counts <- hist(dat15.attAsp0$attainment.delta, attBreaks, plot = F)$counts
  attAsp1.counts <- hist(dat15.attAsp1$attainment.delta, attBreaks, plot = F)$counts
  attAsp2.counts <- hist(dat15.attAsp2$attainment.delta, attBreaks, plot = F)$counts
  attAsp3.counts <- hist(dat15.attAsp3$attainment.delta, attBreaks, plot = F)$counts
  attAsp4.counts <- hist(dat15.attAsp4$attainment.delta, attBreaks, plot = F)$counts
  
  successAsp0.counts <- hist(dat15.successAsp0$attainment.delta, attBreaks, plot = F)$counts
  successAsp1.counts <- hist(dat15.successAsp1$attainment.delta, attBreaks, plot = F)$counts
  successAsp2.counts <- hist(dat15.successAsp2$attainment.delta, attBreaks, plot = F)$counts
  successAsp3.counts <- hist(dat15.successAsp3$attainment.delta, attBreaks, plot = F)$counts
  successAsp4.counts <- hist(dat15.successAsp4$attainment.delta, attBreaks, plot = F)$counts
  
  rm(dat)

  
  for(j in 2:length(check))
  {
    i <- check[j]
    
    shell(paste("\"C:\\Program Files\\WinRAR\\WinRAR.exe\"", "E -ibck -o+", paste(win.pathname, exp, i, "\\Parents_Data.rar", sep =""), paste(win.pathname, exp, i, sep ="")), wait = T)
    dat <- read.csv(paste(win.pathname, exp, i, "\\Parents_Data_Exp", i, ".csv", sep =""), skip = 1, header = T)
    shell(paste("DEL /Q ", paste(win.pathname, exp, i, "\\Parents_Data_Exp", i, ".csv", sep ="")))
    
    dat15 <- dat[dat$child.age == 15,]
    dat15 <- dat15[dat15$Tick > 20,]
    
    attainment.delta <- dat15$child.attainment - dat15$aspiration
    dat15 <- cbind(dat15, attainment.delta)
    
    aspiration.rounded <- floor(dat15$aspiration / 20)
    dat15 <- cbind(dat15, aspiration.rounded)
    
    dat15.attAsp0 <- dat15[dat15$aspiration.rounded == 0,]
    dat15.attAsp1 <- dat15[dat15$aspiration.rounded == 1,]
    dat15.attAsp2 <- dat15[dat15$aspiration.rounded == 2,]
    dat15.attAsp3 <- dat15[dat15$aspiration.rounded == 3,]
    dat15.attAsp4 <- dat15[dat15$aspiration.rounded == 4,]
    
    
    dat15.success <- dat15[dat15$success.rank1 == 1,]
    dat15.successAsp0 <- dat15.success[dat15.success$aspiration.rounded == 0,]
    dat15.successAsp1 <- dat15.success[dat15.success$aspiration.rounded == 1,]
    dat15.successAsp2 <- dat15.success[dat15.success$aspiration.rounded == 2,]
    dat15.successAsp3 <- dat15.success[dat15.success$aspiration.rounded == 3,]
    dat15.successAsp4 <- dat15.success[dat15.success$aspiration.rounded == 4,]
    
    # dat15.sD0_lowAtt <- dat15.success[dat15.success$distance.rounded == 0 & dat15.success$attainment.delta < -10,]
    
    attBreaks <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
    
    attAsp0.counts <- cbind(attAsp0.counts, hist(dat15.attAsp0$attainment.delta, attBreaks, plot = F)$counts)
    attAsp1.counts <- cbind(attAsp1.counts, hist(dat15.attAsp1$attainment.delta, attBreaks, plot = F)$counts)
    attAsp2.counts <- cbind(attAsp2.counts, hist(dat15.attAsp2$attainment.delta, attBreaks, plot = F)$counts)
    attAsp3.counts <- cbind(attAsp3.counts, hist(dat15.attAsp3$attainment.delta, attBreaks, plot = F)$counts)
    attAsp4.counts <- cbind(attAsp4.counts, hist(dat15.attAsp4$attainment.delta, attBreaks, plot = F)$counts)
    
    successAsp0.counts <- cbind(successAsp0.counts, hist(dat15.successAsp0$attainment.delta, attBreaks, plot = F)$counts)
    successAsp1.counts <- cbind(successAsp1.counts, hist(dat15.successAsp1$attainment.delta, attBreaks, plot = F)$counts)
    successAsp2.counts <- cbind(successAsp2.counts, hist(dat15.successAsp2$attainment.delta, attBreaks, plot = F)$counts)
    successAsp3.counts <- cbind(successAsp3.counts, hist(dat15.successAsp3$attainment.delta, attBreaks, plot = F)$counts)
    successAsp4.counts <- cbind(successAsp4.counts, hist(dat15.successAsp4$attainment.delta, attBreaks, plot = F)$counts)
    
    rm(dat)
    
  }
      
  attAsp0.means <- apply(attAsp0.counts, 1, mean, na.rm = T)
  attAsp1.means <- apply(attAsp1.counts, 1, mean, na.rm = T)
  attAsp2.means <- apply(attAsp2.counts, 1, mean, na.rm = T)
  attAsp3.means <- apply(attAsp3.counts, 1, mean, na.rm = T)
  attAsp4.means <- apply(attAsp4.counts, 1, mean, na.rm = T)
  
  attAsp0.upper <- apply(attAsp0.counts, 1, quantile, probs = 0.975, na.rm = T)
  attAsp1.upper <- apply(attAsp1.counts, 1, quantile, probs = 0.975, na.rm = T)
  attAsp2.upper <- apply(attAsp2.counts, 1, quantile, probs = 0.975, na.rm = T)
  attAsp3.upper <- apply(attAsp3.counts, 1, quantile, probs = 0.975, na.rm = T)
  attAsp4.upper <- apply(attAsp4.counts, 1, quantile, probs = 0.975, na.rm = T)
  
  attAsp0.lower <- apply(attAsp0.counts, 1, quantile, probs = 0.025, na.rm = T)
  attAsp1.lower <- apply(attAsp1.counts, 1, quantile, probs = 0.025, na.rm = T)
  attAsp2.lower <- apply(attAsp2.counts, 1, quantile, probs = 0.025, na.rm = T)
  attAsp3.lower <- apply(attAsp3.counts, 1, quantile, probs = 0.025, na.rm = T)
  attAsp4.lower <- apply(attAsp4.counts, 1, quantile, probs = 0.025, na.rm = T)
  
  successAsp0.means <- apply(successAsp0.counts, 1, mean, na.rm = T)
  successAsp1.means <- apply(successAsp1.counts, 1, mean, na.rm = T)
  successAsp2.means <- apply(successAsp2.counts, 1, mean, na.rm = T)
  successAsp3.means <- apply(successAsp3.counts, 1, mean, na.rm = T)
  successAsp4.means <- apply(successAsp4.counts, 1, mean, na.rm = T)

  dists <- seq(0, 5, 1)

  barcols <- brewer.pal(5, "Oranges")
  asp.dat <- rbind(attAsp0.means, attAsp1.means, attAsp2.means, attAsp3.means, attAsp4.means)
  success.dat <- rbind(successAsp0.means, successAsp1.means, successAsp2.means, successAsp3.means, successAsp4.means)
  
  max.count <- max(asp.dat)
  max.countR <- max(pretty(c(0, max.count)))
  
  
  png(paste(pathname, "ParameterSet", pset, "_AttainmentVsAspirationVsSuccess_Bars.png", sep = ""), width = 2000, height = 2000)
  par(cex = 3, lwd = 2)
  barplot(asp.dat, col = barcols, beside = T, axes = F, ylim = c(0, max.countR), ylab = "Counts", xlab = "Attainment Change")
  barplot(asp.dat, col = "black", density = 10, angle = 45, add = T, beside = T, axes = F)
  barplot(success.dat, col = barcols, add = T, beside = T, axes = F)
  
  error.bar(seq(1.5, 55.5, 6), attAsp0.means, upper = (attAsp0.upper - attAsp0.means), lower = (attAsp0.means - attAsp0.lower), length = 0)
  error.bar(seq(2.5, 56.5, 6), attAsp1.means, upper = (attAsp1.upper - attAsp1.means), lower = (attAsp1.means - attAsp1.lower), length = 0)
  error.bar(seq(3.5, 57.5, 6), attAsp2.means, upper = (attAsp2.upper - attAsp2.means), lower = (attAsp2.means - attAsp2.lower), length = 0)
  error.bar(seq(4.5, 58.5, 6), attAsp3.means, upper = (attAsp3.upper - attAsp3.means), lower = (attAsp3.means - attAsp3.lower), length = 0)
  error.bar(seq(5.5, 59.5, 6), attAsp4.means, upper = (attAsp4.upper - attAsp4.means), lower = (attAsp4.means - attAsp4.lower), length = 0)
  
  axis(2, at = pretty(c(0, max.count)))
  axis(1, at = seq(3.5, 60.5, 6), labels = c("-50--40", "-40--30", "-30--20", "-20--10", "-10-0", "0-10", "10-20", "20-30", "30-40", "40-50"), tick = F)
  
  legend("topright", title = "Aspiration", c("0-20", "20-40", "40-60", "60-80", "80-100"), fill = barcols)
  text(54, 10000, "Hatching = Fail")
  
  dev.off()
  
  
  
  pdf(paste(pathname, "ParameterSet", pset, "_AttainmentVsAspirationVsSuccess_Bars.pdf", sep = ""), width = 10, height = 8)
  par(cex = 1, lwd = 1)
  barplot(asp.dat, col = barcols, beside = T, axes = F, ylim = c(0, max.countR), ylab = "Counts", xlab = "Attainment Change")
  barplot(asp.dat, col = "black", density = 20, angle = 45, add = T, beside = T, axes = F)
  barplot(success.dat, col = barcols, add = T, beside = T, axes = F)
  
  error.bar(seq(1.5, 55.5, 6), attAsp0.means, upper = (attAsp0.upper - attAsp0.means), lower = (attAsp0.means - attAsp0.lower), length = 0)
  error.bar(seq(2.5, 56.5, 6), attAsp1.means, upper = (attAsp1.upper - attAsp1.means), lower = (attAsp1.means - attAsp1.lower), length = 0)
  error.bar(seq(3.5, 57.5, 6), attAsp2.means, upper = (attAsp2.upper - attAsp2.means), lower = (attAsp2.means - attAsp2.lower), length = 0)
  error.bar(seq(4.5, 58.5, 6), attAsp3.means, upper = (attAsp3.upper - attAsp3.means), lower = (attAsp3.means - attAsp3.lower), length = 0)
  error.bar(seq(5.5, 59.5, 6), attAsp4.means, upper = (attAsp4.upper - attAsp4.means), lower = (attAsp4.means - attAsp4.lower), length = 0)
  
  axis(2, at = pretty(c(0, max.count)))
  axis(1, at = seq(3.5, 60.5, 6), labels = c("-50--40", "-40--30", "-30--20", "-20--10", "-10-0", "0-10", "10-20", "20-30", "30-40", "40-50"), tick = F)
  
  legend("topright", title = "Aspiration", c("0-20", "20-40", "40-60", "60-80", "80-100"), fill = barcols)
  text(54, 10000, "Hatching = Fail")
  
  dev.off()
  
#}


