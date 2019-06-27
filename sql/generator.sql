DROP TABLE TEST_DATA;
CREATE EXTERNAL TABLE TEST_DATA
(
    DAYLENGTH REAL,
    SEASON REAL
)
USING
(
    DATAOBJECT('/export/home/nz/predykcja/dane/sample.csv')
    DELIMITER ','
    LOGDIR '/tmp'
);
