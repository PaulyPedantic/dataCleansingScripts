##This script was written to clean the data retrieved manually from https://www.dol.gov/whd/state/stateMinWageHis.htm
##On 1/14/2019 and combine it with the January 2019 week three makeover monday data set to use for a visualization
##The goal of the visualization is to contextualize Bureau of Labor Statistics and Department of Labor data to answer:
##Do state minimum wages affect the percentage of workers paid at or below the federal minimum wage?

##Author: Pauly Russ
##Date: 1/14/2019


##load required libraries
library(dplyr)
library(stringr)
library(tidyr)

##set working directory and load data files
setwd("C:\\Users\\pruss\\Documents\\My Tableau Repository\\WIP")
StateMinWages <- read.csv("State Minimum Wage.csv", stringsAsFactors = FALSE)
FedMinWages <- read.csv("Minimum Wage.csv", stringsAsFactors = FALSE)

##clean up state minimum wage data

  ##first pull the header names and clean those
stateminwagenames <- colnames(StateMinWages)

  ##I saw three patterns: first year colnames started with X, state colname had junk char + .., one state had junk char + .
stateminwagenames <- str_replace_all(stateminwagenames, "(ï\\.\\.)|(X)|(Â\\.)", "")

  ##second clean data. four strings need to be removed
    ##1. [\c] - some values have a note indicated by an alpha character wrapped in square brackets
    ##2. (\c) - some values have a note indicated by an alpha character wrapped in parenthesis
    ##3. Â - a few instances of this junk character appear in the data set
    ##4. Some entries have two values listed separated by a hyphen. I read through the notes and decided to drop the second (higher) value
      ## the lower value is indicative of the state's true 'MINIMUM' that can be paid
StateMinWages <- lapply(StateMinWages, str_replace_all, pattern = "(\\[[A-z]\\])|(\\([A-z]\\))|(Â)|( *\\- *\\d*\\.\\d*)", replacement = "")

StateMinWages[2:20] <- lapply(StateMinWages[2:20], str_replace_all, pattern = "\\h", replacement = "")

  ##states with no state minimum were indicated by three dots, replace with 0 so we can use math
StateMinWages <- lapply(StateMinWages, str_replace_all, pattern = "\\.\\.\\.", replacement = "0")

  ##lapply breaks data frame, put back into 51 row data frame
StateMinWages <- data.frame(matrix(unlist(StateMinWages), nrow=51, byrow=F),stringsAsFactors=FALSE)


  ##reapply column names
colnames(StateMinWages) <- stateminwagenames

## now we prep and combine for final set and calculate some columns

## first, pivot the state minimum wages into a long format
pivotStateMinWages <- reshape(StateMinWages, direction = "long", varying = list(colnames(StateMinWages[2:20])))

## I think the pivot treated years as factors, but they were sequential, so math to fix year values
pivotStateMinWages$time = pivotStateMinWages$time + 1999

## rename some columns
colnames(pivotStateMinWages)[colnames(pivotStateMinWages)=="time"] <- "year"
colnames(pivotStateMinWages)[colnames(pivotStateMinWages)=="State.or.other"] <- "state"
colnames(pivotStateMinWages)[colnames(pivotStateMinWages)=="2000"] <- "minwage"

## convert the minimum wage to number and make year/state into factors
pivotStateMinWages$minwage <- as.numeric(pivotStateMinWages$minwage)
pivotStateMinWages$year <- as.factor(as.character(pivotStateMinWages$year))
pivotStateMinWages$state <- as.factor(as.character(pivotStateMinWages$state))

## rename some columns
colnames(FedMinWages)[1] <- "year"
colnames(FedMinWages)[2] <- "state"
colnames(FedMinWages) <- tolower(colnames(FedMinWages))

## the federal minimum wage data set had a 'total' set of rows, drop them and convert to factor
FedMinWages <- FedMinWages[-grep('Total', FedMinWages$state),]

## make state and year into factors
FedMinWages$year <- as.factor(as.character(FedMinWages$year))
FedMinWages$state <- as.factor(as.character(FedMinWages$state))


## combine minimum wage table with pivoted state minimums
finalset <- merge(FedMinWages, pivotStateMinWages)

##rename some columns
colnames(finalset)[6] <- "state.minimum.wage"
colnames(finalset)[5] <- "pcnt.below.fed.min"
colnames(finalset)[4] <- "pcnt.at.fed.min"


## forgot fed minimum wage changed over observed time, below adds that based on the chart at https://www.dol.gov/whd/minwage/chart.htm
fedminovertime <- data.frame(year = unique(finalset$year))
fedminovertime$fed.minimum.wage <- 5.15

fedminovertime$year <- as.numeric(as.character(fedminovertime$year))
fedminovertime$fed.minimum.wage <- if_else(fedminovertime$year == 2007, fedminovertime$fed.minimum.wage + 0.7, if_else(fedminovertime$year == 2008, fedminovertime$fed.minimum.wage + 1.4, if_else(fedminovertime$year >= 2009, fedminovertime$fed.minimum.wage + 2.1, fedminovertime$fed.minimum.wage)))

## merge the federal minimum wage over time on to the final set as a column
finalset <- merge(finalset, fedminovertime)


## create a column that quickly indicates whether a state minimum is higher, equal or lower than federal min
finalset <- finalset%>%
  mutate(state.min.vs.fed.min = case_when(
  state.minimum.wage < fed.minimum.wage & state.minimum.wage != 0 ~ "lt",
  state.minimum.wage == fed.minimum.wage | state.minimum.wage == 0 ~ "eq",
  state.minimum.wage > fed.minimum.wage ~ "gt"))

write.csv(finalset, "enhancedWageData.csv", sep = ",", row.names = FALSE, fileEncoding = "UTF-8")
