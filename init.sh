nzsql -c "CREATE DATABASE PREDYKCJA"
nzsql -c "CREATE TABLE TEST_DATA(DAYLENGTH REAL,SEASON REAL)" -db PREDYKCJA
/nz/export/ae/languages/r/3.0/host64/bin/Rscript /export/home/nz/predykcja/R/classifier.r