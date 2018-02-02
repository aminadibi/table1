# load data (in long format)
data(AT_Soccer, package = "sparkTable")
# first three observations to see the structure of the data

# prepare content
content <- list(
     function(x) { sum(x) },
     function(x) { round(sum(x), 2) },
     function(x) { round(sum(x), 2) },
     sparkTable::newSparkLine(lineWidth = 2, pointWidth = 6), sparkTable::newSparkBar())

names(content) <- c("Points", "ShotGoal", "GetGoal", "GoalDiff", "WinLose")

# set variables
vars <- c("points", "shotgoal", "getgoal", "goaldiff", "wl")

# create the sparkTable object
stab <- sparkTable::newSparkTable(dataObj = AT_Soccer, tableContent = content, varType = vars)


statcope <- haven::read_sas("H:/SAS Projects/Exacerbation Three Trials/3DataSets/STATCOPE/statcope.sas7bdat",
                     NULL)
age <- statcope$staage
sline <- sparkTable::newSparkLine (value=age)
slineIQR <- sparkTable::newSparkLine (value = age, showIQR = TRUE)
sbar <- sparkTable::newSparkBar (value = age, barSpacingPerc = 5)
sbox <- sparkTable::newSparkBox (value = age)
shist <- sparkTable::newSparkHist(value = age)

sparkTable::plot(shist)
sparkTable::export(sbox, outputType = "png", filename = "./figures/test")
getwd()

