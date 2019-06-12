#!/bin/sh
find . -name "*.pyc" -type f -delete
echo "compile_ae"
/nz/export/ae/utilities/bin/compile_ae --db $1 --language r \
     --version 3 --template deploy "./" 
echo "copy whole current project"
export AE_PATH="/nz/export/ae/applications/$1/admin"
if [ ! -d $AE_PATH ]; then
    echo "DB doesn't exists"
    exit
fi
rm -rf . $AE_PATH
echo $AE_PATH
yes | cp -rf . $AE_PATH
echo "register_ae"

/nz/export/ae/utilities/bin/register_ae --db $1 --language r --version 3 \
    --template udtf --exe "./forecast.r" --sig "PUT_PREDICT(VARARGS)" \
    --return "TABLE(predicted_class VARCHAR(1024))" \