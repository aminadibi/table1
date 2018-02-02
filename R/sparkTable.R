# load data (in long format)
data(AT_Soccer, package = "sparkTable")
# first three observations to see the structure of the data
head(AT_Soccer, 3)

# prepare content
content <- list(
     function(x) { sum(x) },
     function(x) { round(sum(x), 2) },
     function(x) { round(sum(x), 2) },
     newSparkLine(lineWidth = 2, pointWidth = 6), newSparkBar())

names(content) <- c("Points", "ShotGoal", "GetGoal", "GoalDiff", "WinLose")

# set variables
vars <- c("points", "shotgoal", "getgoal", "goaldiff", "wl")

# create the sparkTable object
stab <- newSparkTable(dataObj = AT_Soccer, tableContent = content, varType = vars)


statcope <- read_sas("H:/SAS Projects/Exacerbation Three Trials/3DataSets/STATCOPE/statcope.sas7bdat",
                     NULL)
age <- statcope$staage
sline <- newSparkLine (value=age)
slineIQR <- newSparkLine (value = age, showIQR = TRUE)
sbar <- newSparkBar (value = age, barSpacingPerc = 5)
sbox <- newSparkBox (value = age)
shist <- newSparkHist(value = age)

#plot(shist)
export(sbox, outputType = "png", filename = "./test")


