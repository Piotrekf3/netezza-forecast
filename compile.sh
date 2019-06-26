#!/bin/sh

#compile and register

/nz/export/ae/utilities/bin/compile_ae --echo --language r \
--version 3 --template compile --db $1 forecastBayes.r

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecastBayes.r --sig "PUT_FORECAST_BAYES(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecastBayes.r --sig "PUT_FORECAST_BAYES_SEQ(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1 --noparallel

/nz/export/ae/utilities/bin/compile_ae --echo --language r \
--version 3 --template compile --db $1 forecastSVM.r

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecastSVM.r --sig "PUT_FORECAST_SVM(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecastSVM.r --sig "PUT_FORECAST_SVM_SEQ(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1 --noparallel

/nz/export/ae/utilities/bin/compile_ae --echo --language r \
--version 3 --template compile --db $1 forecastNNET.r

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecastNNET.r --sig "PUT_FORECAST_NNET(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecastNNET.r --sig "PUT_FORECAST_NNET_SEQ(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1 --noparallel