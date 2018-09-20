library(tidyverse)
library(ggthemes)
library(haven)
exacevents_wide <- read_sas("H:/Projects/Personalized Exacerbation Prediction/COPD-Merged-Trials/sasdata/exacevents_wide.sas7bdat", 
                            NULL)
View(exacevents_wide)

eclipse2yr <- read_sas("H:/Projects/Personalized Exacerbation Prediction/ECLIPSE/2018-07-17/eclipse2yr.sas7bdat", 
                       NULL)
View(eclipse2yr)

plotHist <- function(dataset, variable, multipliedByFactor = 1, dataBin = 20, dataFormat = "bin"){
  
  var <- select(dataset, variable)
  var <- var * multipliedByFactor;
  p <- ggplot(gather(var), aes(value)) + 
    geom_histogram(bins = dataBin, stat = dataFormat) + 
    facet_wrap(~key, scales = 'free_x') + theme_tufte() + 
    theme(axis.text=element_text(size=25), axis.title=element_text(size=25,face="bold")) +
    ylab ("") + xlab("")
  return(p)
}

plotHist (exacevents_wide, "age")
plotHist (exacevents_wide, "bmi10", 10)
plotHist (exacevents_wide, "stgtotalc00")
plotHist (exacevents_wide, "fev1pre")
plotHist (exacevents_wide, "Days_In_Study")

plotHist (eclipse2yr, "age10", 10)
plotHist (eclipse2yr, "BMI10", 10)
plotHist (eclipse2yr, "sgrq10", 10)
plotHist (eclipse2yr, "FEV1")
plotHist (eclipse2yr, "OBSTIME")
plotHist (eclipse2yr, "obsExac_yr2", dataFormat = "count")

