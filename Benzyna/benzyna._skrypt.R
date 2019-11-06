benzyna_skrypt <- function() {
  # Załadowania dodatkowej biblioteki
  library(forecast)
  
  # Załadowanie pliku z danymi
  load("C:/Projects/Study/mgr/MasterPerform_RTests/Benzyna/benzyna._skrypt.R")
  
  # Wykres szeregu czasowego
  plot(benzyna, main = "Średnie ceny benzyny w Polsce \n w latach 2006-2013")
  
  # Wykonanie obliczenia i wykresu autokorelacji
  acf(benzyna, main = "Średnie ceny benzyny")
  
  # Wykonanie dekompozycji klasycznej
  benzyna.dekomp <- decompose(benzyna)
  
  plot(benzyna.dekomp)
  title("Średnie ceny benzyny - dekompozycja klasyczna")
  
  # Podział szeregu na część uczącą i część testową
  benzyna.learn <- window(benzyna, end = c(2011, 12))
  benzyna.test <- window(benzyna, start = c(2012, 1))
  
  # Zbudowanie modelu ARIMA
  benzyna.arima <- auto.arima(benzyna.learn)
  
  # Wykonanie prognozy
  benzyna.arima.forecast <- forecast(benzyna.arima, h = length(benzyna.test))
  
  # Wykres prognozy
  plot(benzyna.arima.forecast, ylab = "cena benzyny")
  lines(benzyna.test, col = "red")
}
