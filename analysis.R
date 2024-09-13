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
correlation <- cor(filtered_data$EM_SD, filtered_data$EM_LD, use = "complete.obs")
print(paste("Korrelation zwischen EM_SD und EM_LD:", correlation))

# Paired t-test für den Unterschied zwischen EM_SD und EM_LD
t_test_result <- t.test(filtered_data$EM_SD, filtered_data$EM_LD, paired = TRUE)
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

#9)	Aggregation: Since the SD and LD memory performance are highly correlated, you can calculate also the average memory performance for the downstream analysis for both samples.
# Berechne den Durchschnitt der Gedächtnisleistungen (SD und LD) für alle Teilnehmer
filtered_data$Memory_Avg <- rowMeans(filtered_data[, c("EM_SD", "EM_LD")], na.rm = TRUE)

# Überprüfe die ersten Zeilen, um den neuen Durchschnittswert zu sehen
head(filtered_data[, c("EM_SD", "EM_LD", "Memory_Avg")])

# Berechne den Durchschnitt der Gedächtnisleistungen (SD und LD) für die Replikationsstichprobe
if (nrow(repdata) > 0) {
  filtered_data$Memory_Avg <- rowMeans(filtered_data[, c("EM_SD", "EM_LD")], na.rm = TRUE)
  
  # Überprüfe die ersten Zeilen der Replikationsstichprobe, um den neuen Durchschnittswert zu sehen
  head(repdata[, c("EM_SD", "EM_LD", "Memory_Avg")])
} else {
  print(repdata)
}
                      
 # Beispiel: fMRI-Daten für Amygdala und Hippocampus
corfMRIpic<-cor(filtered_data$fMRI_amy_neg_neu, filtered_data$fMRI_hipp_neg_neu, use = "complete.obs")
print(corfMRIpic)                      
                    
corfMRIpicrep<-cor(repdata$fMRI_amy_neg_neu, repdata$fMRI_hipp_neg_neu, use = "complete.obs")
print(corfMRIpicrep)

#Hypothese:"Die fMRI-Aktivität in der Amygdala und im Hippocampus ist während der Betrachtung negativer Bilder stärker korreliert als während der Betrachtung neutraler Bilder."

# Beispiel-Daten für Amygdala und Hippocampus unter zwei Bedingungen (negativ und neutral)
fMRI_amy_neg <- c(4.81, 4.56, -0.68, 4.19, 3.90, 2.62, 3.41, 3.38, 3.64, 3.50)  # Amygdala (negativ)
fMRI_hipp_neg <- c(3.96, 6.65, -0.20, 3.01, 3.03, 3.31, 3.99, 4.41, 3.10, 5.34)  # Hippocampus (negativ)

fMRI_amy_neu <- c(3.00, 2.90, 1.75, 2.80, 2.65, 2.50, 3.10, 3.50, 3.20, 3.40)  # Amygdala (neutral)
fMRI_hipp_neu <- c(2.10, 2.50, 1.50, 2.70, 2.45, 2.40, 2.80, 3.00, 3.00, 3.10)  # Hippocampus (neutral)

# Berechne die Korrelationen für negative Bilder
cor_neg <- cor(fMRI_amy_neg, fMRI_hipp_neg, use = "complete.obs")

# Berechne die Korrelationen für neutrale Bilder
cor_neu <- cor(fMRI_amy_neu, fMRI_hipp_neu, use = "complete.obs")

# Ausgabe der Korrelationen
print(paste("Korrelation für negative Bilder:", cor_neg))
print(paste("Korrelation für neutrale Bilder:", cor_neu))

# Optional: Fisher-Z-Test für den Vergleich der beiden Korrelationen
install.packages("psych")
library("psych")
fisher_test <- r.test(n = length(fMRI_amy_neg), r12 = cor_neg, r34 = cor_neu)
print(fisher_test)
