nz.fun <- function() {
  while(getNext()) {
    model <- readRDS('/nz/export/ae/applications/modelBayes.rds')
    
    daylength <- getInputColumn(0)
    season <- getInputColumn(1)
    
    data <- data.frame(V3 = daylength, V4 = season)
    fcast <- predict(model, data)
	
    setOutputString(0, fcast)
	  outputResult()
  }
}