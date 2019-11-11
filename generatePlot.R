generatePlot <- function(path, money1, money2, money3, money4) {
  testWeekPath = paste(path, "/test.csv", sep = "")
  testWeek = read.csv(testWeekPath, header = TRUE, sep = ";")
  testWeekValues = testWeek[,2]
  testWeekMachines = testWeek[,3]
  
  prediction1Path = paste(path, "/data_", sep = "")
  prediction2Path = paste(path, "/data_", sep = "")
  prediction3Path = paste(path, "/data_", sep = "")
  prediction4Path = paste(path, "/data_", sep = "")
  
  prediction1Path = paste(prediction1Path, money1, sep = "")
  prediction2Path = paste(prediction2Path, money2, sep = "")
  prediction3Path = paste(prediction3Path, money3, sep = "")
  prediction4Path = paste(prediction4Path, money4, sep = "")
  
  prediction1PathMachines = paste(prediction1Path, "/data1/predictedMachines.csv", sep = "")
  prediction2PathMachines = paste(prediction2Path, "/data1/predictedMachines.csv", sep = "")
  prediction3PathMachines = paste(prediction3Path, "/data1/predictedMachines.csv", sep = "")
  prediction4PathMachines = paste(prediction4Path, "/data1/predictedMachines.csv", sep = "")
  
  prediction1PathAddMachines = paste(prediction1Path, "/data1/predictedWithAdditionals.csv", sep = "")
  prediction2PathAddMachines = paste(prediction2Path, "/data1/predictedWithAdditionals.csv", sep = "")
  prediction3PathAddMachines = paste(prediction3Path, "/data1/predictedWithAdditionals.csv", sep = "")
  prediction4PathAddMachines = paste(prediction4Path, "/data1/predictedWithAdditionals.csv", sep = "")
  
  prediction1CostPath = paste(prediction1Path, "/data1/predictedCost.csv", sep = "")
  prediction2CostPath = paste(prediction2Path, "/data1/predictedCost.csv", sep = "")
  prediction3CostPath = paste(prediction3Path, "/data1/predictedCost.csv", sep = "")
  prediction4CostPath = paste(prediction4Path, "/data1/predictedCost.csv", sep = "")
  
  prediction1Path = paste(prediction1Path, "/data1/predictedValues.csv", sep = "")
  prediction2Path = paste(prediction2Path, "/data1/predictedValues.csv", sep = "")
  prediction3Path = paste(prediction3Path, "/data1/predictedValues.csv", sep = "")
  prediction4Path = paste(prediction4Path, "/data1/predictedValues.csv", sep = "")
  
  prediction1 = read.csv(prediction1Path, header = TRUE, sep = ",")
  prediction2 = read.csv(prediction2Path, header = TRUE, sep = ",")
  prediction3 = read.csv(prediction3Path, header = TRUE, sep = ",")
  prediction4 = read.csv(prediction4Path, header = TRUE, sep = ",")
  
  prediction1Machines = read.csv(prediction1PathMachines, header = TRUE, sep = ",")
  prediction2Machines = read.csv(prediction2PathMachines, header = TRUE, sep = ",")
  prediction3Machines = read.csv(prediction3PathMachines, header = TRUE, sep = ",")
  prediction4Machines = read.csv(prediction4PathMachines, header = TRUE, sep = ",")
  
  prediction1Add = read.csv(prediction1PathAddMachines, header = TRUE, sep = ",")
  prediction2Add = read.csv(prediction2PathAddMachines, header = TRUE, sep = ",")
  prediction3Add = read.csv(prediction3PathAddMachines, header = TRUE, sep = ",")
  prediction4Add = read.csv(prediction4PathAddMachines, header = TRUE, sep = ",")
  
  prediction1Cost = read.csv(prediction1CostPath, header = TRUE, sep = ",")
  prediction2Cost = read.csv(prediction2CostPath, header = TRUE, sep = ",")
  prediction3Cost = read.csv(prediction3CostPath, header = TRUE, sep = ",")
  prediction4Cost = read.csv(prediction4CostPath, header = TRUE, sep = ",")
  
  par(mfrow=c(1,1))
  ylimMax <- max(prediction1[,2]) + 2
  plot(prediction1[,2], xlab = "Godzina w tygodniu", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(testWeekValues, col = "red")
  legend("bottomleft", legend = c("Ilość zapytań na godzinę z predykcji", "Testowa wartość ilości zapytań na godzinę"), cex=.8, col = c("black", "red"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction1Add[,2]) + 2
  plot(prediction1Machines[,2], xlab = "Godzina w tygodniu", ylab = "Ilość maszyn", main = "Ilość maszyn na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(prediction1Add[,2], col = "red")
  lines(testWeekMachines, col = "blue")
  legend("topleft", legend = c("Ilość maszyn przyznana po predykcji", "Ilość maszyn na godzinę po rozłożeniu pozostałej kwoty", "Ilość maszyn dla okresu testowego"), cex=.8, col = c("black", "red", "blue"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction2[,2]) + 2
  plot(prediction2[,2], xlab = "Godzina", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(testWeekValues, col = "red")
  legend("bottomleft", legend = c("Ilość zapytań na godzinę z predykcji", "Testowa wartość ilości zapytań na godzinę"), cex=.8, col = c("black", "red"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction2Add[,2]) + 2
  plot(prediction2Machines[,2], xlab = "Godzina", ylab = "Ilość maszyn", main = "Ilość maszyn na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(prediction2Add[,2], col = "red")
  lines(testWeekMachines, col = "blue")
  legend("bottomleft", legend = c("Ilość maszyn przyznana po predykcji", "Ilość maszyn na godzinę po rozłożeniu pozostałej kwoty", "Ilość maszyn dla okresu testowego"), cex=.8, col = c("black", "red", "blue"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction3[,2]) + 2
  plot(prediction3[,2], xlab = "Godzina", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(testWeekValues, col = "red")
  legend("bottomleft", legend = c("Ilość zapytań na godzinę z predykcji", "Testowa wartość ilości zapytań na godzinę"), cex=.8, col = c("black", "red"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction3Add[,2]) + 2
  plot(prediction3Machines[,2], xlab = "Godzina", ylab = "Ilość maszyn", main = "Ilość maszyn na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(prediction3Add[,2], col = "red")
  lines(testWeekMachines, col = "blue")
  legend("bottomleft", legend = c("Ilość maszyn przyznana po predykcji", "Ilość maszyn na godzinę po rozłożeniu pozostałej kwoty", "Ilość maszyn dla okresu testowego"), cex=.8, col = c("black", "red", "blue"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction4[,2]) + 2
  plot(prediction4[,2], xlab = "Godzina", ylab = "Ilość zapytań", main = "Ilość zapytań na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(testWeekValues, col = "red")
  legend("bottomleft", legend = c("Ilość zapytań na godzinę z predykcji", "Testowa wartość ilości zapytań na godzinę"), cex=.8, col = c("black", "red"), pch=c(1,2))

  par(mfrow=c(1,1))
  ylimMax <- max(prediction4Add[,2]) + 2
  plot(prediction4Machines[,2], xlab = "Godzina", ylab = "Ilość maszyn", main = "Ilość maszyn na godzinę", type = "l", ylim = c(0, ylimMax))
  lines(prediction4Add[,2], col = "red")
  lines(testWeekMachines, col = "blue")
  legend("topleft", legend = c("Ilość maszyn przyznana po predykcji", "Ilość maszyn na godzinę po rozłożeniu pozostałej kwoty", "Ilość maszyn dla okresu testowego"), cex=.8, col = c("black", "red", "blue"), pch=c(1,2))
  
  par(mfrow=c(1,1))
  plot(prediction1Cost, xlab = "Godzina w tygodniu", ylab = "Koszt", main = "Koszt maszyn na godzinę", type = "l")

  par(mfrow=c(1,1))
  plot(prediction2Cost, xlab = "Godzina w tygodniu", ylab = "Koszt", main = "Koszt maszyn na godzinę", type = "l")

  par(mfrow=c(1,1))
  plot(prediction3Cost, xlab = "Godzina w tygodniu", ylab = "Koszt", main = "Koszt maszyn na godzinę", type = "l")

  par(mfrow=c(1,1))
  plot(prediction4Cost, xlab = "Godzina w tygodniu", ylab = "Koszt", main = "Koszt maszyn na godzinę", type = "l")
}