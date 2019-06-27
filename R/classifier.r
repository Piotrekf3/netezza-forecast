library(e1071)
library(kernlab)
library(nnet)
data<-read.table(file="/export/home/nz/predykcja/dane/daneCWdate.csv" ,sep=";")


fit <- naiveBayes(V5~., data=data[,3:5])
saveRDS(fit, '/nz/export/ae/applications/modelBayes.rds')

fit <- ksvm(V5~., data=data[,3:5])
saveRDS(fit, '/nz/export/ae/applications/modelSVM.rds')

fit <- nnet(V5~., data=data[,3:5], size=4, decay=0.0001, maxit=500)
saveRDS(fit, '/nz/export/ae/applications/modelNNET.rds')