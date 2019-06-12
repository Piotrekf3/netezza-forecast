#!/bin/sh
/nz/export/ae/utilities/bin/compile_ae --language r \
--version 3 --template compile --db system forecast.r

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecast.r --sig "PUT_FORECAST_TEST(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db system