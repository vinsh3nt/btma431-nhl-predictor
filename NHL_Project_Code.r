rm(list = ls())
library(dplyr)
library(car)
library(jsonlite)
library(ggplot2)

## Web Scraping ##
## Create a vector of link names. These links are to json files of our data
## Use fromJSON() to scrape the data, and merge it all together in one data frame using seasonID and teamFullName

linknames <- c('https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=teamsummary&sort=[{%22property%22:%22points%22,%22direction%22:%22DESC%22},{%22property%22:%22wins%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=penalties&sort=[{%22property%22:%22penaltyMinutesPerGame%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=realtime&sort=[{%22property%22:%22hits%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=leadingtrailing&sort=[{%22property%22:%22winsAfterLead1p%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=outshootoutshotby&sort=[{%22property%22:%22winsOutshotOpponent%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=goalsbyperiod&sort=[{%22property%22:%22goalsForInPer1%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=false&reportName=scoretrailfirst&sort=[{%22property%22:%22winsScore1st%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=shooting&isGame=false&reportName=teampercentages&sort=[{%22property%22:%22shotAttemptsPctg%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019',
               'https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=core&isGame=false&reportName=faceoffsbyzone&sort=[{%22property%22:%22faceoffWinPctgOffensiveZone%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameTypeId=2%20and%20seasonId%3E=20092010%20and%20seasonId%3C=20182019'
)
url <- linknames[1]
document <- fromJSON(txt = url)
document <- document$data
for(i in 2:length(linknames)){
  url <- linknames[i]
  newdoc <- fromJSON(txt = url)
  document <- merge(x = document, y = newdoc$data, by = c("seasonId", "teamFullName"))
}
View(document)

## Remove the '.x' and '.y' that R added to the end of some variable names

names(document) <- gsub(pattern = ".x",
                        replacement = "", 
                        x = names(document))
names(document) <- gsub(pattern = ".y",
                        replacement = "", 
                        x = names(document))

## Remove duplicate columns

document <- document[, !duplicated(colnames(document))]
View(document)


## Calculate Composite Fields ##
## The following code calculates:
## winPctg:              the percentage of games a team wins (our response variable)
## leadAfter1Pctg:       the percentage of games a team leads after 1 period
## leadAfter1Pctg:       the percentage of games a team leads after 2 periods
## outshootOpponentPctg: the percentage of games a team outshoots their opponent
## score1stPctg:         the percentage of games a team scores the first goal of the game


document$outshootOpponentPctg <- (document$winsOutshotOpponent + document$lossOutshotOpponent + document$otLossOutshotOpponent) / document$gamesPled
document$score1stPctg <- (document$winsScore1st + document$lossScore1st + document$otLossScore1st) / document$gamesPled
document$winPctg <- document$wins / document$gamesPled


## Variable Selection ##
## First, eliminate meaningless variables (variables that a team does not have influence over)
## Next, eliminate variables that are not in percentage form
## Then, check for aliased coefficients, and remove them

meaningless <- c('shootingPlusSavePctg', 'leadAfter2Pctg', 'shootingPctg', 'leadAfter1Pctg', 'points', 'regPlusOtWins', 'goalsFor', 'teamFullName', 'teamId', 'teamAbbrev', 'losses', 'otLosses', 'wins', 'seasonId', 'pointPctg', 'winPctgAfterTrail1p', 'winPctgAfterTrail2p', 'goalsAgainst', 'goalsAgainstPerGame', 'goalsForPerGame', 'evGoalsFor', 'lossEvenShots', 'winsEvenShots', 'goalsAgainstInOt', 'goalsAgainstInPer1', 'goalsAgainstInPer2', 'goalsAgainstInPer3', 'goalsForInPer1', 'goalsForInPer2', 'goalsForInPer3', 'ppGoalsFor', 'lossOpponentScore1st', 'otLossOpponentScore1st', 'winsOpponentScore1st', 'winPctgAfterLead1p', 'winPctgAfterLead2p', 'winsAfterTrail1p', 'winsAfterTrail2p', 'lossAfterLead1p', 'lossAfterLead2p', 'lossAfterTrail1p', 'lossAfterTrail2p', 'otLossAfterLead1p', 'otLossAfterLead2p', 'otLossEvenShots', 'otLossAfterTrail1p', 'otLossAfterTrail2p', 'winPctgOpponentScore1st', 'winPctgAfterLead1p', 'winPctgAfterLead2p', 'winPctgScore1st', 'tiesScore1st', 'winsScore1st', 'lossScore1st', 'otLossScore1st', 'shootoutGamesWon', 'goalsForInOt', 'winsAfterLead1p', 'winsAfterLead2p', 'lossAfterLead1p', 'lossAfterLead2p', 'otLossAfterLead1p', 'otLossAfterLead2p', 'winsOutshotOpponent', 'lossOutshotOpponent' ,'otLossOutshotOpponent', 'ties', 'tiesEvenShots', 'tiesOpponentScore1st', 'tiesOutshotOpponent', 'tiesScore1st', 'shGoalsFor', 'winsScore1st')
nonPctg <- c('faceoffsLost', 'faceoffLoss', 'faceoffLossDefensiveZone', 'faceoffLossNeutralZone', 'faceoffLossOffensiveZone', 'faceoffLossWhenAhead', 'faceoffLossWhenBehind', 'faceoffLossWhenClose', 'faceoffs', 'faceoffsLost', 'faceoffsTaken', 'faceoffsWon', 'faceoffWins', 'faceoffWinsDefensiveZone', 'faceoffWinsNeutralZone', 'faceoffWinsOffensiveZone', 'faceoffWinsWhenBehind', 'faceoffWinsWhenAhead', 'faceoffWinsWhenClose')

doc1 <- select(document, -one_of(c(nonPctg, meaningless)))

## Use VIF() to eliminate collinear variables

while (TRUE) {
  modl <- lm(winPctg ~ ., data = doc1)
  dfVif <- modl %>%
    vif() %>%
    as.data.frame()
  ind <- which.max(dfVif$.)
  if (dfVif$.[ind] > 10){
    doc1 <- doc1[,-ind]
  } else {break}
}

str(doc1)

## Use step() to select the most statistically significant variables

nullModel <- lm(winPctg ~ 1, data = doc1)
fullModel <- lm(winPctg ~ ., data = doc1)
stepModel <- step(nullModel, 
                  scope = list(lower = nullModel, upper = fullModel),
                  direction = "both")

summary(stepModel)

## Eliminate non-significant variables

modelData <- select(doc1, -one_of('unblockedShotAttemptsPctgAhead', 'penaltiesMatch'))

nullModel <- lm(winPctg ~ 1, data = modelData)
fullModel <- lm(winPctg ~ ., data = modelData)
stepModel2 <- step(nullModel, 
                  scope = list(lower = nullModel, upper = fullModel),
                  direction = "both")

summary(stepModel2)


## Model Evaluation
## Test the model's ability to predict winning percentage.

preds <- predict(stepModel2, newdata = modelData)

plotdata <- data.frame(winpctg = document$winPctg, preds) %>%
  arrange(winpctg)
plotdata <- data.frame(ind = 1:nrow(document), plotdata)

## Plot the true winning percentage (black) and the predicted winning percentage (red)

attach(plotdata)
ggplot(plotdata, aes(x = ind, y = preds)) +
  geom_point(aes(col = I("red"))) +
  geom_line(aes(y = winpctg, size = I(1))) +
  ggtitle("Win Percentage, Predicted vs Actual") +  
  xlab("Index") +
  ylab("Win Percentage") +
  theme(plot.title = element_text(hjust = 0.5))


## Root Mean Squared Error (RMSE):: Measures the Standard Deviation of Errors
sqrt(sum(((plotdata$winpctg - plotdata$preds)**2) / nrow(plotdata)))

