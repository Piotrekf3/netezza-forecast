nz.fun <- function() {
	install.packages('/nz/export/ae/applications/e1071.tar.gz', repos=NULL, type='source',INSTALL_opts = c('--no-lock'))
	library(e1071)
	model <- readRDS('/nz/export/ae/applications/modelBayes.rds')
	while(getNext()) {
		daylength <- getInputColumn(0)
		season <- getInputColumn(1)
		
		data <- data.frame(V3 = daylength, V4 = season)
		fcast <- predict(model, data)
		
		setOutputString(0, fcast)
		outputResult()
	}

}