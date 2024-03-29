rm(list=ls())

pathname <- "~/LocalDoc/GitHub/school_choice/export/"
exp <- "Experiment"
coeff <- c()
coeff2 <- c()
coeff3 <- c()
rsq <- c()
rsq2 <- c()
rsq3 <- c()
largePVal.count = 0
largePVal.count2 = 0
largePVal.count3 = 0

check <- seq(from = 105, to = 114, by = 1)

for(i in check[1]:check[10])
{
  dat <- read.csv(paste(pathname, exp, i, "/Schools_SummaryData_Exp", i, ".csv", sep =""), skip = 16, header = T)
  for (t in 20:99)
  {
    subdat <- dat[dat$Tick == t, c("Tick","School_id","GCSE.score","App.ratio","max.distance_y7","max.distance_y11")]
    
    lm <- lm(subdat$GCSE.score~subdat$App.ratio)
    if(summary(lm)$coefficients[2,4] > 0.05){
      largePVal.count = largePVal.count + 1
    }
    rsq <- c(rsq,summary(lm)$r.squared)
    coeff <- c(coeff,summary(lm)$coefficients[2,1])
    
    lm2 <- lm(subdat$GCSE.score~subdat$max.distance_y7)
    if(summary(lm2)$coefficients[2,4] > 0.05){
      largePVal.count2 = largePVal.count + 1
    }
    rsq2 <- c(rsq2,summary(lm2)$r.squared)
    coeff2 <- c(coeff2,summary(lm2)$coefficients[2,1])
    
    lm3 <- lm(subdat$App.ratio~subdat$max.distance_y7)
    if(summary(lm3)$coefficients[2,4] > 0.05){
      largePVal.count3 = largePVal.count + 1
    }
    rsq3 <- c(rsq3,summary(lm3)$r.squared)
    coeff3 <- c(coeff3,summary(lm3)$coefficients[2,1])
  }
}
mean(rsq)
mean(coeff)
largePVal.count/25

mean(rsq2)
mean(coeff2)
largePVal.count2/25

mean(rsq3)
mean(coeff3)
largePVal.count3/25