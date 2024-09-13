#analysis
mydata<-readLines("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/mydata.txt, header= TRUE, sep="/t"")
mydata <- read.delim("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/mydata.txt", header = TRUE, sep = "\t")
filtered_data <- mydata[mydata$Filter != 1, ]
remaining_participants <- nrow(filtered_data)
print(remaining_participants)
testosterone_summary <- aggregate(Testosteron ~ Sex, data = mydata, summary)
print(testosterone_summary)