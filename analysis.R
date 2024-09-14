###Power analysis: 
install.packages("pwr")
library(pwr)

#Define the parameters
n <- 800 # Sample size
alpha <- 0.05 # Significance level

#Power calculation for small effect size (r = 0.1)
power_small <- pwr.r.test(n = n, r = 0.1, sig.level = alpha, alternative = "two.sided")

#Power calculation for medium effect size (r = 0.3)
power_medium <- pwr.r.test(n = n, r = 0.3, sig.level = alpha, alternative = "two.sided")

#Power calculation for large effect size (r = 0.5)
power_large <- pwr.r.test(n = n, r = 0.5, sig.level = alpha, alternative = "two.sided")

#Print results
power_small
power_medium
power_large

#2.	You want to run 10 analysis – therefore you have to adjust your alpha level to 0.005. How much power do you have now to detect a significant finding for a small, medium or large effect?
alpha_adj <- 0.005  # Adjusted alpha level

# Power calculation for small effect size (r = 0.1) with adjusted alpha
power_small_adj <- pwr.r.test(n = n, r = 0.1, sig.level = alpha_adj, alternative = "two.sided")

# Power calculation for medium effect size (r = 0.3) with adjusted alpha
power_medium_adj <- pwr.r.test(n = n, r = 0.3, sig.level = alpha_adj, alternative = "two.sided")

# Power calculation for large effect size (r = 0.5) with adjusted alpha
power_large_adj <- pwr.r.test(n = n, r = 0.5, sig.level = alpha_adj, alternative = "two.sided")

# Print results
power_small_adj
power_medium_adj
power_large_adj

# What is the minimum sample size you need to replicate a large, medium, or a small effect with a power of 90%?

# Define the parameters
power_target <- 0.90  # Desired power
alpha <- 0.05         # Significance level

# Sample size calculation for small effect size (r = 0.1)
sample_size_small <- pwr.r.test(r = 0.1, sig.level = alpha, power = power_target, alternative = "two.sided")

# Sample size calculation for medium effect size (r = 0.3)
sample_size_medium <- pwr.r.test(r = 0.3, sig.level = alpha, power = power_target, alternative = "two.sided")

# Sample size calculation for large effect size (r = 0.5)
sample_size_large <- pwr.r.test(r = 0.5, sig.level = alpha, power = power_target, alternative = "two.sided")

# Print results
sample_size_small
sample_size_medium
sample_size_large

# Your replication sample contains about 250 participants with high-quality data. How much power do you have to replicate a significant finding for a small, medium or large effect (alpha=0.05, two-sided) in this sample?
# Define the parameters
n <- 250       # Sample size
alpha <- 0.05  # Significance level

# Power calculation for small effect size (r = 0.1)
power_small_250 <- pwr.r.test(n = n, r = 0.1, sig.level = alpha, alternative = "two.sided")

# Power calculation for medium effect size (r = 0.3)
power_medium_250 <- pwr.r.test(n = n, r = 0.3, sig.level = alpha, alternative = "two.sided")

# Power calculation for large effect size (r = 0.5)
power_large_250 <- pwr.r.test(n = n, r = 0.5, sig.level = alpha, alternative = "two.sided")

# Print results
power_small_250
power_medium_250
power_large_250

### Read in data and quality check = removing of low-quality data
mydata <- read.delim("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/mydata.txt", header = TRUE, sep = "\t")

###Data-cleaning um alle unnötigen Daten zu entfernen 
filtered_data <- mydata[mydata$Filter != 1, ] 
remaining_participants <- nrow(filtered_data) 
print(remaining_participants)  
testosterone_summary <- aggregate(Testosteron ~ Sex, data = filtered_data, summary)


filtered_data_rep <- repdata[repdata$Filter != 1, ] 
remaining_participants_rep <- nrow(filtered_data_rep) 
print(remaining_participants_rep) 

### Reliability, Validation and Aggregation of data (2-6 of exercise 4)

testosterone_summary <- aggregate(Testosteron ~ Sex, data = filtered_data, summary)

testosterone_summary_rep <- aggregate(Testosteron ~ Sex, data = filtered_data_rep, summary)

print(testosterone_summary_rep)

print(testosterone_summary) 

#Reliability: Berechne die Korrelation zwischen EM_SD und EM_LD
correlation <- cor(filtered_data$EM_SD, filtered_data$EM_LD, use = "complete.obs")
print(paste("Korrelation zwischen EM_SD und EM_LD:", correlation))###1. Berechne die Korrelation zwischen den beiden Gedächtnismessungen: SD (Short Delay) und LD (Long Delay).

# Paired t-test für den Unterschied zwischen EM_SD und EM_LD
t_test_result <- t.test(filtered_data$EM_SD, filtered_data$EM_LD, paired = TRUE)
print(t_test_result) ###2. Untersuche den Leistungsunterschied zwischen SD und LD.

#Falls ein signifikanter Unterschied festgestellt wird, können wir dies interpretieren. Im Allgemeinen ist es sinnvoll, dass die Leistung bei der Gedächtnisaufgabe nach einem kurzen Intervall (SD) besser ist als nach einem längeren Intervall (LD), da die Erinnerung im Laufe der Zeit abnimmt.

#Analyse RepData 
repdata<- read.delim("C:/Users/Kerstin/Desktop/gittest/G02_Kerstin/repdata.txt", header = TRUE, sep = "\t")

# Berechne die Korrelation zwischen EM_SD und EM_LD in RepData
correlationrep <- cor(filtered_data_rep$EM_SD, filtered_data_rep$EM_LD, use = "complete.obs")
print(paste("Korrelation zwischen EM_SD und EM_LD:", correlationrep))###4. Führe die gleiche Analyse für die Replikationsstichprobe durch.

# Paired t-test für den Unterschied zwischen EM_SD und EM_LD repdata
t_test_resultrep <- t.test(filtered_data_rep$EM_SD, filtered_data_rep$EM_LD, paired = TRUE)
print(t_test_resultrep)

#9)	Aggregation: Since the SD and LD memory performance are highly correlated, you can calculate also the average memory performance for the downstream analysis for both samples.
# Berechne den Durchschnitt der Gedächtnisleistungen (SD und LD) für alle Teilnehmer
filtered_data$Memory_Avg <- rowMeans(filtered_data[, c("EM_SD", "EM_LD")], na.rm = TRUE)

#Überprüfe die ersten Zeilen, um den neuen Durchschnittswert zu sehen
head(filtered_data[, c("EM_SD", "EM_LD", "Memory_Avg")])

#Berechne den Durchschnitt der Gedächtnisleistungen (SD und LD) für die Replikationsstichprobe
if (nrow(repdata) > 0) {
  filtered_data$Memory_Avg <- rowMeans(filtered_data[, c("EM_SD", "EM_LD")], na.rm = TRUE)
  
#Überprüfe die ersten Zeilen der Replikationsstichprobe, um den neuen Durchschnittswert zu sehen
  head(repdata[, c("EM_SD", "EM_LD", "Memory_Avg")])
} else {
  print(repdata)
}
                      
#Beispiel:fMRI-Daten für Amygdala und Hippocampus
corfMRIpic<-cor(filtered_data$fMRI_amy_neg_neu, filtered_data$fMRI_hipp_neg_neu, use = "complete.obs")
print(corfMRIpic)                      
                    
corfMRIpicrep<-cor(filtered_data_rep$fMRI_amy_neg_neu, filtered_data_rep$fMRI_hipp_neg_neu, use = "complete.obs")
print(corfMRIpicrep)
### Main analysis
#Hypothese:"Die fMRI-Aktivität in der Amygdala und im Hippocampus ist während der Betrachtung negativer Bilder stärker korreliert als während der Betrachtung neutraler Bilder."

# Beispiel-Daten für Amygdala und Hippocampus unter zwei Bedingungen (negativ und neutral)in mydata 

###Part 5 neue Hypothese
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

### Additional analysis

### Replication
