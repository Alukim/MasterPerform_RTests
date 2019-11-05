autoscaleSimulation <- function(path, moneyToSpendForWeek, moneyForOneMachineForHour, pathToSave, q) {
  library(forecast)
  
  data = loadData(path)

  month.ts <- ts(data$month, frequency=24*7)
  month.machine.ts <- ts(data$machine, frequency=24*7)
  
  cat("\n Money to spend for week: ", moneyToSpendForWeek)
  cat("\n Cost of one hour of active machine: ", moneyForOneMachineForHour)
  cat("\n")
  
  acf(month.ts, main = "ACF for Month - Number of requests per hours")
  acf(month.machine.ts, main = "ACF for Month - Number of machine per hours")
  
  month.decompose <- decompose(month.ts)
  month.machine.decompose <- decompose(month.machine.ts)
  
  plot(month.decompose)
  
  plot(month.machine.decompose)
  
  month.ts.learn <- window(month.ts, end = c(4,168))
  month.ts.test <- window(month.ts, start = c(5,1))
  
  first <- TRUE
  test.prediction.values <- 0
  test.prediction.machines <- 0
  test.prediction.machines.before <- 0
  test.prediction.costForHour <- 0
  test.prediction.restCostInHour <- 0
  
  dir.create(pathToSave);
  
  for(i in 1:168) {
    cat("\nIteration: ", i)
    
    if(first == TRUE) {
      month.ts.learn <- window(month.ts, end = c(4,168))
      month.ts.test <- window(month.ts, start = c(5,1))
      first <- FALSE
    } else {
      month.ts.learn <- window(month.ts, end = c(5,(i-1)))
      month.ts.test <- window(month.ts, start = c(5,i))
    }
    
    month.arima <- auto.arima(month.ts.learn)
    month.arima.forecast <- forecast(month.arima, h = length(month.ts.test))
    
    plotsPathEnumeration = paste(pathToSave, i, sep = "")
    dir.create(plotsPathEnumeration);
    
    forecastPath = paste(plotsPathEnumeration, "\\forecast.png", sep = "")
    png(filename = forecastPath, width = 1200, height = 500)
    
    par(mfrow=c(1,1))
    plot(month.arima.forecast, ylab = "Number of request per hours.", xlab = "Numer of weeks")
    lines(month.ts.test, col = "red")
    dev.off()
    
    prediction.high <- 0
    machines.high <- 0
    distinctions.high <- 0
    
    prediction.low <- 0
    machines.low <- 0
    distinctions.low <- 0
    
    machines.center <- 0
    
    upper.high <- month.arima.forecast$upper[,2]
    lower.high <- month.arima.forecast$upper[,1]
    
    upper.low <- month.arima.forecast$lower[,2]
    lower.low <- month.arima.forecast$lower[,1]
    
    for(j in 1:length(month.ts.test)) {
      prediction.high[j] <- upper.high[j]
      machines.high[j] <- getMachineCount(upper.high[j])
      distinctions.high[j] <- upper.high[j] - upper.low[j]
      
      prediction.low[j] <- lower.high[j]
      machines.low[j] <- getMachineCount(lower.high[j])
      distinctions.low[j] <- lower.high[j] - lower.low[j]
      
      machines.center[j] <- getMachineCount(month.arima.forecast$mean[j])
    }
    
    machines.predictions <- machines.high
    distinctions.predictions <- distinctions.high
    value.prediction <- prediction.high
    
    machines.sum.high <- sum(machines.high)
    restCost <- moneyToSpendForWeek - (machines.sum.high * moneyForOneMachineForHour)
    cat(" ,95%: ", machines.sum.high * moneyForOneMachineForHour)
    if(machines.sum.high * moneyForOneMachineForHour > moneyToSpendForWeek) {
      machines.sum.low <- sum(machines.low)
      cat("\n 80%: ", machines.sum.low * moneyForOneMachineForHour)
      if(machines.sum.low * moneyForOneMachineForHour > moneyToSpendForWeek) {
        machines.sum.center <- sum(machines.center)
        cat("\n Mean: ", machines.sum.center * moneyForOneMachineForHour)
        restCost <- moneyToSpendForWeek - (machines.sum.center * moneyForOneMachineForHour)
        machines.predictions <- machines.center
        value.prediction <- month.arima.forecast$mean
      } else {
        restCost <- moneyToSpendForWeek - (machines.sum.low * moneyForOneMachineForHour)
        machines.predictions <- machines.low
        distinctions.predictions <- distinctions.low
        value.prediction <- prediction.low
      }
    }
    
    machines.predictions.before <- machines.predictions
    
    if(restCost >= moneyForOneMachineForHour) {
      restMachinesCount <- restCost %/% moneyForOneMachineForHour
      sortedDistincions <- sort(distinctions.predictions, index.return=TRUE, decreasing=TRUE)
      sortedDistincions.length <- length(sortedDistincions$x)
      threshold <- max(distinctions.predictions) * 0.6
      division <- restMachinesCount%/%sortedDistincions.length
      additionalMachine = 1
      restMachinesToDistribute <- restMachinesCount
      
      if(division > 2) {
        additionalMachine = division %/% q
      }
      
      if(division >= 1) {
        for(f in 1:sortedDistincions.length) {
          machines.predictions[f] <- machines.predictions[f] + additionalMachine 
        }
        
        restMachinesToDistribute <- restMachinesCount - (additionalMachine * sortedDistincions.length)
      }
      
      filterDistincionsLenght <- length(Filter(function(z) {z > threshold}, sortedDistincions$x))
      division <- restMachinesToDistribute%/%filterDistincionsLenght
      
      if(division >= 1) {
        for(g in 1:filterDistincionsLenght) {
          index <- sortedDistincions$ix[g]
          machines.predictions[index] <- machines.predictions[index] + division
        }
      }
      
      restMachinesToDistribute <- restMachinesToDistribute - (division * filterDistincionsLenght)
      
      if(restMachinesToDistribute > 0) {
        for(r in 1:restMachinesToDistribute) {
          index <- sortedDistincions$ix[r]
          machines.predictions[index] <- machines.predictions[index] + 1
        }
      }
    }
    
    moneyForMachineForThisHour <- machines.predictions[1] * moneyForOneMachineForHour
    moneyToSpendForWeek = moneyToSpendForWeek - moneyForMachineForThisHour
    
    ylimMax <- max(machines.predictions)
    machinesPath = paste(plotsPathEnumeration, "\\machines.png", sep = "")
    png(filename = machinesPath, width = 1200, height = 500)
    
    par(mfrow=c(1,1))
    plot(machines.predictions, col = "red", xlab = "Hour in week", ylab = "Number of machines", main = "Number of machines per hours", type = "l", ylim = c(0, ylimMax))
    lines(machines.predictions.before)
    dev.off()
    
    test.prediction.values[i] <- value.prediction[1]
    test.prediction.machines[i] <- machines.predictions[1]
    test.prediction.machines.before[i] <- machines.predictions.before[1]
    test.prediction.costForHour[i] <- moneyForMachineForThisHour
    test.prediction.restCostInHour[i] <- moneyToSpendForWeek
  }
  
  requeusts = paste(pathToSave, "requests.png", sep = "")
  png(filename = requeusts, width = 1200, height = 500)
  par(mfrow=(c(1,1)))
  plot(test.prediction.values, col = "red", xlab = "Hour in week", ylab = "Number of requests", main = "Number of request per hours", type = "l")
  lines(data$testWeek[,2])
  dev.off()
  
  machinesPerHours = paste(pathToSave, "machines.png", sep = "")
  png(filename = machinesPerHours, width = 1200, height = 500)
  ylimMax <- max(test.prediction.machines) + 2
  plot(test.prediction.machines, col = "red", xlab = "Hour in week", ylab = "Number of machines", main = "Number of machines per hours", type = "l", ylim = c(0, ylimMax))
  lines(test.prediction.machines.before)
  dev.off()
  
  costs = paste(pathToSave, "costPerHour.png", sep = "")
  png(filename = costs, width = 1200, height = 500)
  plot(test.prediction.costForHour, xlab = "Hour in week", ylab = "Cost", main = "Cost of active machines in hour", type = "l")
  dev.off()
  
  restCost = paste(pathToSave, "restConstPerHour.png", sep = "")
  png(filename = restCost, width = 1200, height = 500)
  plot(test.prediction.restCostInHour, xlab = "Hour in week", ylab = "Cost", main = "Cost of active machines in hour", type = "l")
  dev.off()
  
  predictedValues = paste(pathToSave, "predictedValues.csv", sep = "")
  write.csv(x = test.prediction.values, file = predictedValues)
  
  predictedMachines = paste(pathToSave, "predictedMachines.csv", sep = "")
  write.csv(x = test.prediction.machines.before, file = predictedMachines)
  
  predicatedMachinesWithAdditionals = paste(pathToSave, "predictedWithAdditionals.csv", sep = "")
  write.csv(x = test.prediction.machines, file = predicatedMachinesWithAdditionals)
  
  predictedCost = paste(pathToSave, "predictedCost.csv", sep = "")
  write.csv(x = test.prediction.costForHour, file = predictedCost)
  
  predictedRestCostInHour = paste(pathToSave, "predictedRestCostInHour.csv", sep = "")
  write.csv(x = test.prediction.restCostInHour, file = predictedRestCostInHour)
}

loadData <- function(path) {
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
  
  par(mfrow=c(2,2))
  plot(x = firstWeek[,1], y = firstWeek[,2], type = "l", xlab = "Godzina w tygodniu", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę")
  plot(x = secondWeek[,1], y = secondWeek[,2], type = "l", xlab = "Godzina w tygodniu", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę")
  plot(x = thirdWeek[,1], y = thirdWeek[,2], type = "l", xlab = "Godzina w tygodniu", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę")
  plot(x = fourthWeek[,1], y = fourthWeek[,2], type = "l", xlab = "Godzina w tygodniu", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę")
  
  par(mfrow=c(1,1))
  plot(x = testWeek[,1], y = testWeek[,2], type = "l", xlab = "Godzina w tygodniu", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę")
  
  month = c(firstWeek[,2], secondWeek[,2], thirdWeek[,2], fourthWeek[,2], testWeek[,2]);
  plot(month, type = "l", main = "Ilość zapytań na godzinę w przeciągu 4 tygodni", xlab = 'Godzina w tygodniach', ylab = 'Ilość zapytań')
  
  month.machine = c(firstWeek[,3], secondWeek[,3], thirdWeek[,3], fourthWeek[,3], testWeek[,3]);
  plot(month.machine, type = "l", main = "Ilość maszyn na godzinę w przeciągu 4 ", xlab = "Godzina w tygodniu", ylab = "Ilość maszyn")

  return(list(month = month, machine = month.machine, test = testWeek))
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

simulateThreeTimes <- function(path, moneyToSpendForWeek, moneyForOneMachineForHour) {
  pathToSaveMain = paste(path, "data_", sep = "")
  
  pathToSaveMain = paste(pathToSaveMain, moneyToSpendForWeek, sep = "")
  dir.create(pathToSaveMain)
  
  pathToSaveMain = paste(pathToSaveMain, "\\data", sep = "")
  
  print("First test")
  pathToSaveFirstTest = paste(pathToSaveMain, "1\\", sep = "")
  autoscaleSimulation(path, moneyToSpendForWeek ,moneyForOneMachineForHour, pathToSaveFirstTest, 2)
  
  print("Second test")
  pathToSaveSecondTest = paste(pathToSaveMain, "2\\", sep = "")
  autoscaleSimulation(path, moneyToSpendForWeek ,moneyForOneMachineForHour, pathToSaveSecondTest, 2)
  
  print("Third test")
  pathToSaveThirdTest = paste(pathToSaveMain, "3\\", sep = "")
  autoscaleSimulation(path, moneyToSpendForWeek ,moneyForOneMachineForHour, pathToSaveThirdTest, 2)
}

simulateDecrease <- function() {
  simulateThreeTimes(".\\Decrease\\", 2500, 5)
  simulateThreeTimes(".\\Decrease\\", 2700, 5)
  simulateThreeTimes(".\\Decrease\\", 3000, 5)
  simulateThreeTimes(".\\Decrease\\", 3700, 5)
}

simulateIncrease <- function() {
  simulateThreeTimes(".\\Increase\\", 1100, 5)
  simulateThreeTimes(".\\Increase\\", 1250, 5)
  simulateThreeTimes(".\\Increase\\", 1600, 5)
  simulateThreeTimes(".\\Increase\\", 2000, 5)
}

simulateMiddle <- function() {
  simulateThreeTimes(".\\Middle\\", 2100, 5)
  simulateThreeTimes(".\\Middle\\", 2300, 5)
  simulateThreeTimes(".\\Middle\\", 2800, 5)
  simulateThreeTimes(".\\Middle\\", 3400, 5)
}