#analysis
mydata<-readLines("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/mydata.txt, header= TRUE, sep="/t"")
mydata <- read.delim("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/mydata.txt", header = TRUE, sep = "\t")
filtered_data <- mydata[mydata$Filter != 1, ]
remaining_participants <- nrow(filtered_data)
print(remaining_participants)
testosterone_summary <- aggregate(Testosteron ~ Sex, data = mydata, summary)
print(testosterone_summary)

#Reliability: 
#Um die Aufgabenstellung zu lösen, führen wir die folgenden Schritte aus:
#1. Berechne die Korrelation zwischen den beiden Gedächtnismessungen: SD (Short Delay) und LD (Long Delay).
#2. Untersuche den Leistungsunterschied zwischen SD und LD.
#3. Prüfe, ob dieser Unterschied sinnvoll ist.
#4. Führe die gleiche Analyse für die Replikationsstichprobe durch.

# Berechne die Korrelation zwischen EM_SD und EM_LD
correlation <- cor(mydata$EM_SD, mydata$EM_LD, use = "complete.obs")
print(paste("Korrelation zwischen EM_SD und EM_LD:", correlation))

# Paired t-test für den Unterschied zwischen EM_SD und EM_LD
t_test_result <- t.test(mydata$EM_SD, mydata$EM_LD, paired = TRUE)
print(t_test_result)

#Falls ein signifikanter Unterschied festgestellt wird, können wir dies interpretieren. Im Allgemeinen ist es sinnvoll, dass die Leistung bei der Gedächtnisaufgabe nach einem kurzen Intervall (SD) besser ist als nach einem längeren Intervall (LD), da die Erinnerung im Laufe der Zeit abnimmt.

#Analyse RepData 
repdata<- read.delim("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/repdata.txt", header = TRUE, sep = "\t")
# Berechne die Korrelation zwischen EM_SD und EM_LD in RepData
correlationrep <- cor(repdata$EM_SD, repdata$EM_LD, use = "complete.obs")
print(paste("Korrelation zwischen EM_SD und EM_LD:", correlationrep))

# Paired t-test für den Unterschied zwischen EM_SD und EM_LD repdata
t_test_resultrep <- t.test(repdata$EM_SD, repdata$EM_LD, paired = TRUE)
print(t_test_resultrep)
