#!/bin/sh
/nz/export/ae/utilities/bin/compile_ae --echo --language r \
--version 3 --template compile --db $1 forecast.r

/nz/export/ae/utilities/bin/register_ae --language r --version 3 \
--template udtf --exe forecast.r --sig "PUT_FORECAST(VARARGS)" \
--return "TABLE(predicted_class VARCHAR(1024))" --db $1