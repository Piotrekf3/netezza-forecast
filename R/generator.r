args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Number of rows must be supplied", call.=FALSE)
}

input <- read.table(file="./dane/daneCWdate.csv" ,sep=";")
input <- input[,3:4]
result <- input[sample(1:nrow(input), args[1]), ]
write.table(result, file = "./dane/sample.csv", sep=",", row.names=FALSE, col.names=FALSE)
