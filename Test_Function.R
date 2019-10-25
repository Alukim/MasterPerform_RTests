test_algoritm <- function(path, moneyToSpendForWeek, moneyForOneMachineForHour) {
  library(forecast)
  
  firstWeekPath = paste(path, "/first.csv", sep = "")
  secondWeekPath = paste(path, "/second.csv", sep = "")
  thirdWeekPath = paste(path, "/third.csv", sep = "")
  fourthWeekPath = paste(path, "/fourth.csv", sep = "")
  testWeekPath = paste(path, "/test.csv", sep = "")
  firstWeek = read.csv(firstWeekPath, header = TRUE, sep = ";")
  secondWeek = read.csv(secondWeekPath, header = TRUE, sep = ";")
  thirdWeek = read.csv(thirdWeekPath, header = TRUE, sep = ";")
  fourthWeek = read.csv(fourthWeekPath, header = TRUE, sep = ";")
  testWeek = read.csv(testWeekPath, header = TRUE, sep = ";")
  restOfMoney <- moneyToSpendForWeek
  
  par(mfrow=c(3,2))
  plot(x = firstWeek[,1], y = firstWeek[,2], type = "l", col = "red", xlab = "Hour in week", ylab = "Number of requests", main = "Number of requests in hour - First week")
  plot(x = secondWeek[,1], y = secondWeek[,2], type = "l", col = "red", xlab = "Hour in week", ylab = "Number of requests", main = "Number of requests in hour - Second week")
  plot(x = thirdWeek[,1], y = thirdWeek[,2], type = "l", col = "red", xlab = "Hour in week", ylab = "Number of requests", main = "Number of requests in hour - Third week")
  plot(x = fourthWeek[,1], y = fourthWeek[,2], type = "l", col = "red", xlab = "Hour in week", ylab = "Number of requests", main = "Number of requests in hour - Fourth week")
  
  par(mfrow=c(1,1))
  plot(x = testWeek[,1], y = testWeek[,2], type = "l", col = "red", xlab = "Hour in week", ylab = "Number of requests", main = "Number of requests in hour - Test week")
  
  par(mfrow=c(1,1))
  month = c(firstWeek[,2], secondWeek[,2], thirdWeek[,2], fourthWeek[,2]);
  plot(month, type = "l", col = "red", main = "Number of requests per seconds in month", xlab = 'Hour in month', ylab = 'Number of requests')
  
  month.machine = c(firstWeek[,3], secondWeek[,3], thirdWeek[,3], fourthWeek[,3]);
  plot(month.machine, type = "l", col = "red", main = "Count of active machine per hours in month", xlab = "Hour in month", ylab = "Count of active machines")
  cat("\n Number of machines in first week: ", sum(firstWeek[,3]))
  
  month.ts <- ts(month, frequency=24*7)
  month.machine.ts <- ts(month.machine, frequency=24*7)
  
  cat("\n Money to spend for week: ", moneyToSpendForWeek)
  cat("\n Cost of one hour active machine: ", moneyForOneMachineForHour)
  cat("\n")
  
  acf(month.ts, main = "ACF for Month - Number of requests per hours")
  acf(month.ts, main = "ACF for Month - Number of machine per hours")
  
  month.decompose <- decompose(month.ts)
  month.machine.decompose <- decompose(month.machine.ts)
  
  plot(month.decompose)
  title("Month - decomposition")
  
  plot(month.machine.decompose)
  title("Month - machines - decomposition")
  
  month.ts.learn <- window(month.ts, end = c(3, 168))
  month.ts.test <- window(month.ts, start = c(4, 1))
 
  plot(month.ts.test)
  
  print("TEST")
  
  month.arima <- auto.arima(month.ts.learn)
  month.arima.forecast <- forecast(month.arima, h = length(month.ts.test))
  
  plot(month.arima.forecast, ylab = "Number of request per hours.")
  lines(month.ts.test, col = "red")
  
  machines.high <- 0
  distinctions.high <- 0
  
  machines.low <- 0
  distinctions.low <- 0
  
  machines.center <- 0
  distinctions.center <- 0
  
  upper.high <- month.arima.forecast$upper[,2]
  lower.high <- month.arima.forecast$upper[,1]
  
  upper.low <- month.arima.forecast$lower[,2]
  lower.low <- month.arima.forecast$lower[,1]
  
  for(i in 1:168) {
    machines.high[i] <- getMachineCount(upper.high[i])
    distinctions.high[i] <- upper.high[i] - upper.low[i]
    
    machines.low[i] <- getMachineCount(lower.high[i])
    distinctions.low[i] <- lower.high[i] - lower.low[i]
    
    machines.center[i] <- getMachineCount(month.arima.forecast$mean[i])
  }
  
  machines.predictions <- 0
  distinctions.predictions <- 0
  restCost <- restOfMoney
  
  machines.sum.high <- sum(machines.high)
  cat("\n 95%: ", machines.sum.high * moneyForOneMachineForHour)
  if(machines.sum.high * moneyForOneMachineForHour > restOfMoney) {
    machines.sum.low <- sum(machines.low)
    cat("\n 80%: ", machines.sum.low * moneyForOneMachineForHour)
    if(machines.sum.low * moneyForOneMachineForHour > restOfMoney) {
      machines.sum.center <- sum(machines.center)
      cat("\n Mean: ", machines.sum.center * moneyForOneMachineForHour)
      restCost <- restOfMoney - machines.sum.center
      machines.predictions <- machines.center
    } else {
      restCost <- restOfMoney - machines.sum.low
      machines.predictions <- machines.low
    }
  }
  else {
    restCost <- restOfMoney - machines.sum.high
    machines.predictions <- machines.high
  }
  
  restMachines
  if()

}

forecastWithArima <- function(learn, test) {
  
}

forecastWithEts <- function(learn, test) {
  
}

getMachineCount <- function(value) {
  if (value < 36000)
    return(1)
  
  if (value < 57600)
    return(2)
  
  if (value < 64800)
    return(3)
  
  if(value < 75600)
    return(4)
  
  return(5)
}