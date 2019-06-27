nz.fun <- function() {
	install.packages('/nz/export/ae/applications/nnet.tar.gz', repos=NULL, type='source',INSTALL_opts = c('--no-lock'))
	library(nnet)
	model <- readRDS('/nz/export/ae/applications/modelNNET.rds')
	while(getNext()) {
		daylength <- getInputColumn(0)
		season <- getInputColumn(1)
		
		data <- data.frame(V3 = daylength, V4 = season)
		fcast <- predict(model, data)
		
		setOutputString(0, fcast)
		outputResult()
	}

}