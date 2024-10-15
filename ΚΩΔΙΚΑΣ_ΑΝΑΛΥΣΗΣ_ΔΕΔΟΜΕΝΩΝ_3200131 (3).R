####ΝΙΚΟΛΑΟΣ ΠΑΠΑΓΕΩΡΓΙΟΥ ΕΡΓΑΣΙΑ_1  3200131

####Πρώτα αναλύσαμε το αρχικό dataset εξολοκλήρου (Περιγραφική ανάλυση+ Σχέσεις ανά δύο)
####και μετά αναλύσαμε τα δύο subsets που προέκυψαν από την διάμεσο του Population με τον ίδιο τρόπο
####Και ακολούθησε το γραμμικό υποδειγμα


# Φόρτωση των απαραίτητων πακέτων
library(psych)
library(dplyr)
library(tidyr)
library(nortest)
library(tidyverse)
library(GGally)
library(ggplot2)
library(gplots)
library(corrplot)
library(car)
library(sjPlot)

# Διάβασμα του αρχείου CSV
b <- read.csv("C:\\Users\\NIKOS\\Downloads\\01_Bush.csv", sep = "\t", header = TRUE)

# Χωρισμός της πρώτης στήλης με το / σε ξεχωριστές στήλες
b_separated <- b %>%
  separate(col = state.bush.male.pop.rural.bpovl.clfu.mgt18.pgt65.numgt75, 
           into = c("ID", "State", "Bush", "Male", "Population", "Rural", "Below_Poverty", 
                    "CLFU", "Management_18", "Percentage_65", "Num_75"),
           sep = "/")

# Μετατροπή των στήλων σε κατάλληλους τύπους δεδομένων
b_separated <- b_separated %>%
  mutate(across(c(Bush, Male, Population, Rural, Below_Poverty, CLFU, 
                  Management_18, Percentage_65, Num_75), as.numeric)) %>%
  mutate(ID = as.factor(ID))


b_separated$Num_75<-b_separated$Num_75/b_separated$Population
b_separated$Num_75<-b_separated$Num_75*100

####Στρογγυλοποιηση σε δύο δεκαδικά ψηφία την Bush
b_separated$Bush<-round(b_separated$Bush,2)
###Μετατροπη σε κοινη κλιμακα με τις υπόλοιπες
b_separated$Bush <- b_separated$Bush * 100

# Εμφάνιση του νέου dataset με τις κατάλληλες μετατροπές τύπων δεδομένων
print(b_separated)


str(b_separated)
#Περιγραφικη Αναλυση του σετ δεδομενων
describe(b_separated)
summary(b_separated)




par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
# Δημιουργία ιστογραμμάτων για τις τρεις μεταβλητές που προσεγγιζουν κανονικη κατανομη
hist(b_separated$Below_Poverty, main = "Histogram of Below_Poverty", xlab = "Below_Poverty", col = "blue", border = "black")
hist(b_separated$Male, main = "Histogram of Male", xlab = "Male", col = "green", border = "black")
hist(b_separated$Management_18, main = "Histogram of Management_18", xlab = "Management_18", col = "red", border = "black")
mtext("Histograms", outer = TRUE, cex = 1.5)


par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))

# Δημιουργία qq-plot για τις τρεις μεταβλητές που προσεγγιζουν κανονικη κατανομη

qqnorm(b_separated$Below_Poverty, main = "Q-Q Plot of Below_Poverty")
qqline(b_separated$Below_Poverty, col = "blue")


qqnorm(b_separated$Male, main = "Q-Q Plot of Male")
qqline(b_separated$Male, col = "red")


qqnorm(b_separated$Management_18, main = "Q-Q Plot of Management_18")
qqline(b_separated$Management_18, col = "green")
mtext("QQ-plots", outer = TRUE, cex = 1.5)





####Έλεγχοι κανονικότητας για τις μταβλητες που προσεγγιζουν κανονικη κατανομη
shapiro.test(b_separated$Below_Poverty)
shapiro.test(b_separated$Male)
shapiro.test(b_separated$Management_18)

####Έλεγχοι κανονικότητας για τις μταβλητες που προσεγγιζουν κανονικη κατανομη με lillie test αυτή τη φορά
lillie_result <- lillie.test(b_separated$Below_Poverty)
print(lillie_result)
lillie_result <- lillie.test(b_separated$Male)
print(lillie_result)
lillie_result <- lillie.test(b_separated$Management_18)
print(lillie_result)






par(mfrow = c(1, 3))

# Δημιουργία box plots για όλες τις μεταβλητές του dataset

boxplot(b_separated$Below_Poverty, main = "Boxplot of Below_Poverty", col = "blue")
boxplot(b_separated$Male, main = "Boxplot of Male", col = "green")
boxplot(b_separated$Management_18, main = "Boxplot of Management_18", col = "red")


boxplot(b_separated$Population, main = "Boxplot of Population", col = "blue")
boxplot(b_separated$Bush, main = "Boxplot of Bush", col = "green")
boxplot(b_separated$Rural, main = "Boxplot of Rural", col = "red")


boxplot(b_separated$CLFU, main = "Boxplot of CLFU", col = "blue")
boxplot(b_separated$Percentage_65, main = "Boxplot of Percentage_65", col = "green")
boxplot(b_separated$Num_75, main = "Boxplot of Num_75", col = "red")



##### Σχήμα με τους  δεικτες συσχετισης Pearson ολων των μεταβλητών μεταξύ τους
par(mfrow = c(1, 1))
cor_matrix <- b_separated %>%
  select(Bush, Male, Population, Rural, Below_Poverty, CLFU, Management_18, Percentage_65, Num_75) %>%
  cor()


corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,  # Χρώμα και κλίση ετικετών
         addCoef.col = "black",          # Προσθήκη συντελεστών συσχέτισης
         col = colorRampPalette(c("lightgreen", "white", "lightcoral"))(200))
mtext("ΔΕΙΚΤΗΣ ΣΥΣΧΕΤΙΣΗΣ PEARSON", outer = TRUE, cex = 1.5)



####Τα Scatterplots ανάμεσα στις μεταβλητές που εμφανίζουν υψηλή συσχέτιση και μας αφορούν
par(mfrow = c(1, 4))
plot(b_separated$Rural, b_separated$Bush, main = "Bush vs. Rural", xlab = "Rural (%)", ylab = "Bush (%)")
plot(b_separated$CLFU, b_separated$Bush, main = "Bush vs. CLFU", xlab = "CLFU (%)", ylab = "Bush (%)")
plot(b_separated$Population, b_separated$Rural, main = "Rural vs. Population", xlab = "Population (%)", ylab = "Rural (%)")
plot(b_separated$Num_75, b_separated$Bush, main = "Bush vs. Num_75", xlab = "Num_75 (%)", ylab = "Bush (%)")
mtext("SCATTERPLOTS", outer = TRUE, cex = 1.5)
par(mfrow = c(1, 3))
plot(b_separated$Male, b_separated$Management_18, main = "Management_18 vs. Male", xlab = "Male (%)", ylab = "Management_18 (%)")
plot(b_separated$Rural, b_separated$Num_75, main = "Num_75 vs. Rural", xlab = "Rural (%)", ylab = "Num_75 (%)")
plot(b_separated$Below_Poverty, b_separated$CLFU, main = "CLFU vs. Below_Poverty", xlab = "Below_Poverty(%)", ylab = "CLFU (%)")
mtext("SCATTERPLOTS", outer = TRUE, cex = 1.5)

####Στρογγυλοποιηση σε δύο δεκαδικά ψηφία για όλο το dataset αν χρειαζεται
b_separated[, -(1:3)] <- round(b_separated[, -(1:3)], 2)

####Δημιουργεία μεταβλητής Bush_over που θα μας βοηθήσει με τις εξαρτήσεις. Χωρίζουμε με βάση το ποσοστό του Bush σε κάθε πολιτεία μεγαλύτερο η όχι του 0.5(50%)
b_separated$Bush_over <- ifelse(b_separated$Bush > 50, 1, 0)
b_separated


par(mfrow = c(1, 1))

######Έλεγχοι εξαρτησίας-ανεξαρτησίας ανάμεσα στην Bush_over και τις μεταβλητές CLFU ΚΑΙ RURAL
######ΘΕΩΡΟΥΜΕ INDEPENDENT SAMPLES
######Ελεγχος κανονικοτητας
by(b_separated$CLFU,b_separated$Bush_over,shapiro.test)
by(b_separated$CLFU,b_separated$Bush_over,lillie.test)
###Mikra deigmata
###Μη παραμετρικός έλεγχος
wilcox.test(b_separated$CLFU,b_separated$Bush_over,data=b_separated)
####Yparxei simantiki diafora
boxplot(b_separated$CLFU~b_separated$Bush_over)

######Ελεγχος κανονικοτητας
by(b_separated$CLFU,b_separated$Bush_over,lillie.test)
######Ελεγχος διασπορων
var.test(b_separated$CLFU~b_separated$Bush_over,data=b_separated)
###Παραμετρικός έλεγχος
t.test(b_separated$CLFU~b_separated$Bush_over,data=b_separated,var.equal=T)
plotmeans(b_separated$CLFU~b_separated$Bush_over, connect=F)

######Ελεγχος κανονικοτητας
by(b_separated$Rural,b_separated$Bush_over,shapiro.test)
by(b_separated$Rural,b_separated$Bush_over,lillie.test)

###Mikra deigmata
###Μη παραμετρικός έλεγχος
wilcox.test(b_separated$Rural,b_separated$Bush_over,data=b_separated)
####Yparxei simantiki diafora
boxplot(b_separated$Rural~b_separated$Bush_over)

####Μέσες τιμές των μεταβλητών RURAL,CLFU για τις τιμές της Bush_over
mean_rural <- tapply(b_separated$Rural, b_separated$Bush_over, mean)
mean_clfu <- tapply(b_separated$CLFU, b_separated$Bush_over, mean)
par(mfrow = c(1, 2))

# Δημιουργία barplot για τη μεταβλητή Rural
bp_rural <- barplot(mean_rural, main = "Average Rural by Bush_over", 
                    xlab = "Bush_over", ylab = "Average Rural (%)", col = "skyblue",
                    names.arg = c("Bush_over = 0", "Bush_over = 1"), ylim = c(0, 100),space=0.5)
# Προσθήκη ετικετών πάνω από τις ραβδους της Rural
text(x = bp_rural, y = mean_rural, label = paste0(round(mean_rural, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")

# Δημιουργία barplot για τη μεταβλητή CLFU
bp_clfu <- barplot(mean_clfu, main = "Average CLFU by Bush_over", 
                   xlab = "Bush_over", ylab = "Average CLFU (%)", col = "lightgreen",
                   names.arg = c("Bush_over = 0", "Bush_over = 1"), ylim = c(0, 100),space=0.5) 
# Προσθήκη ετικετών πάνω από τις ραβδους της CLFU
text(x = bp_clfu, y = mean_clfu, label = paste0(round(mean_clfu, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")


####Boxplots ανα τιμη της Bush_over των μεταβλητων Rural και CLFU
par(mfrow = c(1, 2))
boxplot(b_separated$Rural ~ b_separated$Bush_over, col = c("orange", "lightblue"), xlab = "Bush_over", ylab = "Rural")
boxplot(b_separated$CLFU ~ b_separated$Bush_over, col = c("orange", "lightblue"), xlab = "Bush_over", ylab = "CLFU")
mtext("Boxplots ανα τιμη της Bush_over των μεταβλητων Rural και CLFU", outer = TRUE,cex = 1.5,col = "black" ,font = 2)


par(mfrow = c(2, 2))

# Density plots για την μεταβλητή Rural ανά τιμή της Bush_over
for (i in unique(b_separated$Bush_over)) {
  density_plot <- density(b_separated$Rural[b_separated$Bush_over == i])
  plot(density_plot, main = paste("Density Plot of Rural for Bush_over =", i), 
       xlab = "Rural", ylab = "Density", col = "white", lwd = 2)
  polygon(density_plot, col = "skyblue", border = "black")
}

# Density plots για την μεταβλητή CLFU ανά τιμή της Bush_over
for (i in unique(b_separated$Bush_over)) {
  density_plot <- density(b_separated$CLFU[b_separated$Bush_over == i])
  plot(density_plot, main = paste("Density Plot of CLFU for Bush_over =", i), 
       xlab = "CLFU", ylab = "Density", col = "white", lwd = 2)
  polygon(density_plot, col = "skyblue", border = "black")
}

#### Οι νέες δίτιμες κατηγορικές που προκύπτουν από κάθε μεταβλητή αν την χωρίσουμε σε μεγάλα και μικρά ποσοστά σύμφωνα με την διάμεσο τους
#### Με σκοπό την πραγματοποίηση ελέγχων Fisher.
b_separated$Rural_over <- ifelse(b_separated$Rural > 30, 1, 0)
tab<-table(b_separated$Bush_over,b_separated$Rural_over)
###Έλεγχος
fisher.test(tab)

b_separated$Num_75_over <- ifelse(b_separated$Num_75 > 2, 1, 0)
tab2<-table(b_separated$Bush_over,b_separated$Num_75_over)
fisher.test(tab2)

tab3<-table(b_separated$Rural_over,b_separated$Num_75_over)
fisher.test(tab3)


####Διάμεσσος της Population με σκοπό τον χωρισμό του dataset σε δύο υποομάδες
median_pop <- median(b_separated$Population)
b_separated$pop_hl <- ifelse(b_separated$Population > median_pop, "High Pop", "Low Pop")
high_pop <- subset(b_separated, pop_hl == "High Pop")
low_pop <- subset(b_separated, pop_hl == "Low Pop")

####Περιγραφική ανάλυση και για τα δύο
describe(high_pop)
describe(low_pop)


par(mfrow = c(1, 1))
par(mar = c(8, 5, 5, 2) + 0.1)
par(mfrow = c(1, 6))

#####QQ-plots μεταβλητών που πιθανόν να προσεγγίζουν κανονική κατανομή για υποομάδα high_pop
qqnorm(high_pop$Male, main = "Q-Q Plot of Male")
qqline(high_pop$Male, col = "red")

qqnorm(high_pop$Rural, main = "Q-Q Plot of Rural")
qqline(high_pop$Rural, col = "blue")

qqnorm(high_pop$Management_18, main = "Q-Q Plot of Management_18")
qqline(high_pop$Management_18, col = "green")

qqnorm(high_pop$Below_Poverty, main = "Q-Q Plot of Below_Poverty")
qqline(high_pop$Below_Poverty, col = "yellow")

qqnorm(high_pop$Bush, main = "Q-Q Plot of Bush")
qqline(high_pop$Bush, col = "purple")

qqnorm(high_pop$CLFU, main = "Q-Q Plot of CLFU")
qqline(high_pop$CLFU, col = "black")
mtext("QQ-plots μεταβλητών που πιθανόν να προσεγγίζουν την κανονική κατανομή απο high_pop ", outer = TRUE, cex = 1.5)

par(mfrow = c(1, 1))
par(mar = c(8, 5, 5, 2) + 0.1)
par(mfrow = c(1, 6))


######Density plots των παραπάνω μεταβλητών για high_pop

density_plot <- density(high_pop$Male)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Male",xlab = "Male", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")


density_plot <- density(high_pop$Rural)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Rural",xlab = "Rural", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")

density_plot <- density(high_pop$Management_18)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Management_18",xlab = "Management_18", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")


density_plot <- density(high_pop$Below_Poverty)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Below_Poverty",xlab = "Below_Poverty", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")

density_plot <- density(high_pop$Bush)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Bush",xlab = "Bush", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")

density_plot <- density(high_pop$CLFU)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of CLFU",xlab = "CLFU", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")
mtext("DENSITY PLOTS", outer = TRUE, cex = 1.5)



par(mfrow = c(1, 1))

######Density plots των μεταβλητών που προσεγγίζουν κανονική κατανομή για low_pop

par(mfrow = c(1, 3))
density_plot <- density(low_pop$Rural)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Rural",xlab = "Rural", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")


density_plot <- density(low_pop$Management_18)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Management_18",xlab = "Management_18", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")
mtext("DENSITY PLOTS", outer = TRUE, cex = 1.5)

density_plot <- density(low_pop$Male)
# Σχεδίαση του density plot 
plot(density_plot, main = "Density Plot of Male",xlab = "Male", ylab = "Density", col = "white", lwd = 2) 
polygon(density_plot, col = "skyblue", border = "black")


par(mfrow = c(1, 1))
par(mfrow = c(1, 3))

#####QQ-plots μεταβλητών που πιθανόν να προσεγγίζουν κανονική κατανομή για υποομάδα low_pop

qqnorm(low_pop$Rural, main = "Q-Q Plot of Rural")
qqline(low_pop$Rural, col = "purple")

qqnorm(low_pop$Management_18 , main = "Q-Q Plot of Management_18 ")
qqline(low_pop$Management_18 , col = "black")

qqnorm(low_pop$Male , main = "Q-Q Plot of Male ")
qqline(low_pop$Male , col = "red")


par(mfrow = c(1, 1))
par(mfrow = c(1, 3))

#Δημιουργία box plots για τις 6 μεταβλητές της high pop  που πιθανόν να προσεγγίζουν κανονική κατανομή
boxplot(high_pop$Male, main = "Boxplot of Male", col = "green")
boxplot(high_pop$Below_Poverty, main = "Boxplot of Below_Poverty", col = "blue")
boxplot(high_pop$Management_18, main = "Boxplot of Management_18", col = "red")


boxplot(high_pop$Bush, main = "Boxplot of Bush", col = "blue")
boxplot(high_pop$CLFU, main = "Boxplot of CLFU", col = "green")
boxplot(high_pop$Rural, main = "Boxplot of Rural", col = "red")

par(mfrow = c(1, 1))
par(mfrow = c(1, 3))

#Δημιουργία box plots για τις 3 μεταβλητές της low_pop που πιθανόν να προσεγγίζουν κανονική κατανομή
boxplot(low_pop$Rural, main = "Boxplot of Rural", col = "green")
boxplot(low_pop$Management_18, main = "Boxplot of Management_18", col = "blue")
boxplot(low_pop$Male, main = "Boxplot of Male", col = "red")



par(mfrow = c(1, 1))
###Δείκτες συσχετισής για υποομάδα high_pop:
cor_matrix <- high_pop %>%
  select(Bush, Male, Population, Rural, Below_Poverty, CLFU, Management_18, Percentage_65, Num_75) %>%
  cor()


corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,  # Χρώμα και κλίση ετικετών
         addCoef.col = "black",          # Προσθήκη συντελεστών συσχέτισης
         col = colorRampPalette(c("lightgreen", "white", "lightcoral"))(200))
mtext("ΔΕΙΚΤΗΣ ΣΥΣΧΕΤΙΣΗΣ PEARSON ΓΙΑ ΠΟΛΙΤΕΙΕΣ ΜΕ ΜΕΓΑΛΟ ΑΡΙΘΜΟ ΠΛΗΘΥΣΜΟΥ", outer = TRUE, cex = 1.5)




###Δείκτες συσχέτισης για υποομάδα low_pop:
#cor low_pop:
cor_matrix <- low_pop %>%
  select(Bush, Male, Population, Rural, Below_Poverty, CLFU, Management_18, Percentage_65, Num_75) %>%
  cor()

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,  # Χρώμα και κλίση ετικετών
         addCoef.col = "black",          # Προσθήκη συντελεστών συσχέτισης
         col = colorRampPalette(c("lightgreen", "white", "lightcoral"))(200))
mtext("ΔΕΙΚΤΗΣ ΣΥΣΧΕΤΙΣΗΣ PEARSON ΓΙΑ ΠΟΛΙΤΕΙΕΣ ΜΕ ΜΙΚΡΟ ΑΡΙΘΜΟ ΠΛΗΘΥΣΜΟΥ", outer = TRUE, cex = 1.5)

####SCATTERPLOTS ανάμεσα σε μεταβλητές που φαναιρώνουν υψηλή συσχέτιση στην low_pop
par(mfrow = c(1, 5))
plot(low_pop$Rural, low_pop$Bush, main = "Bush vs. Rural", xlab = "Rural (%)", ylab = "Bush (%)")
plot(low_pop$CLFU, low_pop$Below_Poverty, main = "Below_Poverty vs. CLFU", xlab = "CLFU (%)", ylab = "Below_Poverty (%)")
plot(low_pop$Num_75, low_pop$Bush, main = "Bush vs. Num_75", xlab = "Num_75 (%)", ylab = "Bush (%)")
plot(low_pop$Management_18, low_pop$Male, main = "Male vs. Management_18", xlab = "Management_18 (%)", ylab = "Male (%)")
plot(low_pop$Male, low_pop$Bush, main = "Bush vs. Male", xlab = "Male (%)", ylab = "Bush (%)")
mtext("SCATTERPLOTS", outer = TRUE, cex = 1.5)

par(mfrow = c(1, 1))
par(mfrow = c(1, 5))
####SCATTERPLOTS ανάμεσα σε μεταβλητές που φαναιρώνουν υψηλή συσχέτιση στην high_pop
plot(high_pop$Rural, high_pop$Bush, main = "Bush vs. Rural", xlab = "Rural (%)", ylab = "Bush (%)")
plot(high_pop$CLFU, high_pop$Below_Poverty, main = "Below_Poverty vs. CLFU", xlab = "CLFU (%)", ylab = "Below_Poverty (%)")
plot(high_pop$Num_75, high_pop$Bush, main = "Bush vs. Num_75", xlab = "Num_75 (%)", ylab = "Bush (%)")
plot(high_pop$CLFU, high_pop$Below_Poverty, main = "Below_Poverty vs. CLFU", xlab = "CLFU (%)", ylab = "Below_Poverty (%)")
plot(high_pop$Management_18, high_pop$Male, main = "Male vs. Management_18", xlab = "Management_18 (%)", ylab = "Male (%)")
mtext("SCATTERPLOTS", outer = TRUE, cex = 1.5)



par(mfrow = c(1, 1))
par(mfrow = c(1, 2))

######Scatterplots ανάμεσα στις μεταβλητές που μας αφορούν(Bush και Rural,CLFU),για τον υπάρχει εξάρτηση(ελέγχουμε και για τα δύο subsets)

plot(high_pop$Rural, high_pop$Bush, main = "Bush vs. Rural ΓΙΑ ΜΕΓΑΛΟ ΑΡΙΘΜΟ ΠΛΗΘΥΣΜΟΥ", xlab = "Rural (%)", ylab = "Bush (%)")
plot(low_pop$Rural, low_pop$Bush, main = "Bush vs. Rural ΓΙΑ ΜΙΚΡΟ ΑΡΙΘΜΟ ΠΛΗΘΥΣΜΟΥ", xlab = "Rural (%)", ylab = "Bush (%)")
mtext("SCATTERPLOTS BUSH~RURAL ΚΑΙ ΓΙΑ ΤΙΣ ΔΥΟ ΥΠΟΟΜΑΔΕΣ", outer = TRUE, cex = 1.5)

par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plot(high_pop$CLFU, high_pop$Bush, main = "Bush vs. CLFU ΓΙΑ ΜΕΓΑΛΟ ΑΡΙΘΜΟ ΠΛΗΘΥΣΜΟΥ", xlab = "CLFU (%)", ylab = "Bush (%)")
plot(low_pop$CLFU, low_pop$Bush, main = "Bush vs. CLFU ΓΙΑ ΜΙΚΡΟ ΑΡΙΘΜΟ ΠΛΗΘΥΣΜΟΥ", xlab = "CLFU (%)", ylab = "Bush (%)")
mtext("SCATTERPLOTS BUSH~CLFU ΚΑΙ ΓΙΑ ΤΙΣ ΔΥΟ ΥΠΟΟΜΑΔΕΣ", outer = TRUE, cex = 1.5)


######Έλεγχοι εξαρτησίας-ανεξαρτησίας ανάμεσα στην Bush_over και τις μεταβλητές CLFU ΚΑΙ RURAL και στα δύο subsets
######ΘΕΩΡΟΥΜΕ INDEPENDENT SAMPLES

###Gia high_pop
######Ελεγχος κανονικοτητας
by(high_pop$CLFU,high_pop$Bush_over,shapiro.test)
######Ελεγχος διακυμάνσεων
var.test(high_pop$CLFU~high_pop$Bush_over,data=high_pop)
t.test(high_pop$CLFU~high_pop$Bush_over,data=high_pop,var.equal=T)
plotmeans(high_pop$CLFU~high_pop$Bush_over, connect=F)
###den iparxei ejartisi

######Ελεγχος κανονικοτητας
by(high_pop$Rural,high_pop$Bush_over,shapiro.test)
######Ελεγχος διακυμάνσεων
var.test(high_pop$Rural~high_pop$Bush_over,data=high_pop)
###Παραμετρικός έλεγχος
t.test(high_pop$Rural~high_pop$Bush_over,data=high_pop,var.equal=T)
plotmeans(high_pop$Rural~high_pop$Bush_over, connect=F)
###yparxei ejartisi

###Gia low_pop
######Ελεγχος κανονικότητας
by(low_pop$CLFU,low_pop$Bush_over,shapiro.test)
###Mikra deigmata
###Μη παραμετρικός έλεγχος
wilcox.test(low_pop$CLFU,low_pop$Bush_over,data=low_pop)
####Yparxei simantiki diafora
boxplot(low_pop$CLFU~low_pop$Bush_over)
###iparxi ejartisi

######Ελεγχος κανονικότητας
by(low_pop$Rural,low_pop$Bush_over,shapiro.test)
######Ελεγχος διακυμάνσεων
var.test(low_pop$Rural~low_pop$Bush_over,data=low_pop)
###Παραμετρικός έλεγχος
t.test(low_pop$Rural~low_pop$Bush_over,data=low_pop,var.equal=T)
plotmeans(low_pop$Rural~low_pop$Bush_over, connect=F)
###iparxi

####Μέσες τιμές των μεταβλητών RURAL,CLFU για τις τιμές της Bush_over και στις δύο υποομάδες του πληθυσμού
mean_rural_high_pop <- tapply(high_pop$Rural, high_pop$Bush_over, mean)
mean_clfu_high_pop <- tapply(high_pop$CLFU, high_pop$Bush_over, mean)
mean_rural_low_pop <- tapply(low_pop$Rural, low_pop$Bush_over, mean)
mean_clfu_low_pop <- tapply(low_pop$CLFU, low_pop$Bush_over, mean)


par(mfrow = c(1, 2))
# Δημιουργία barplot για τη μεταβλητή Rural σε Πολιτείες με υψηλό πληθυσμό
bp_rural_high <- barplot(mean_rural_high_pop, main = "Average Rural by Bush_over σε Πολιτείες με μεγάλο πληθυσμό", 
                         xlab = "Bush_over", ylab = "Average Rural (%)", col = "skyblue",
                         names.arg = c("Bush_over = 0", "Bush_over = 1"), ylim = c(0, 100),space=0.5)
text(x = bp_rural_high, y = mean_rural_high_pop, label = paste0(round(mean_rural_high_pop, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")

# Δημιουργία barplot για τη μεταβλητή CLFU σε Πολιτείες με υψηλό πληθυσμό
bp_clfu_high <- barplot(mean_clfu_high_pop, main = "Average CLFU by Bush_over σε Πολιτείες με μεγάλο πληθυσμό", 
                        xlab = "Bush_over", ylab = "Average CLFU (%)", col = "lightgreen",
                        names.arg = c("Bush_over = 0", "Bush_over = 1"), ylim = c(0, 100),space=0.5)
text(x = bp_clfu_high, y = mean_clfu_high_pop, label = paste0(round(mean_clfu_high_pop, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")

# Δημιουργία barplot για τη μεταβλητή Rural σε Πολιτείες με μικρό αριθμό πληθυσμού
bp_rural_low <- barplot(mean_rural_low_pop, main = "Average Rural by Bush_over σε Πολιτείες με μικρό αριθμό πληθυσμού", 
                        xlab = "Bush_over", ylab = "Average Rural (%)", col = "skyblue",
                        names.arg = c("Bush_over = 0", "Bush_over = 1"), ylim = c(0, 100),space=0.5)
text(x = bp_rural_low, y = mean_rural_low_pop, label = paste0(round(mean_rural_low_pop, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")

# Δημιουργία barplot για τη μεταβλητή CLFU σε Πολιτείες με μικρό αριθμό πληθυσμού
bp_clfu_low <- barplot(mean_clfu_low_pop, main = "Average CLFU by Bush_over σε Πολιτείες με μικρό αριθμό πληθυσμού", 
                       xlab = "Bush_over", ylab = "Average CLFU (%)", col = "lightgreen",
                       names.arg = c("Bush_over = 0", "Bush_over = 1"), ylim = c(0, 100),space=0.5)
text(x = bp_clfu_low, y = mean_clfu_low_pop, label = paste0(round(mean_clfu_low_pop, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")




par(mfrow = c(1, 2))
###Boxplots ανα τιμη της Bush_over των μεταβλητων Rural και CLFU για Πολιτείες με μεγάλο αριθμό πληθυσμό
boxplot(high_pop$Rural ~ high_pop$Bush_over, col = c("orange", "lightblue"), xlab = "Bush_over", ylab = "Rural")
boxplot(high_pop$CLFU ~ high_pop$Bush_over, col = c("orange", "lightblue"), xlab = "Bush_over", ylab = "CLFU")
mtext("Boxplots ανα τιμη της Bush_over των μεταβλητων Rural και CLFU για Πολιτείες με μεγάλο αριθμό πληθυσμό", outer = TRUE,cex = 1.5,col = "black",font = 2)


par(mfrow = c(1, 2))
###Boxplots ανα τιμη της Bush_over των μεταβλητων Rural και CLFU για Πολιτείες με μικρό αριθμό πληθυσμό
boxplot(low_pop$Rural ~ low_pop$Bush_over, col = c("orange", "lightblue"), xlab = "Bush_over", ylab = "Rural")
boxplot(low_pop$CLFU ~ low_pop$Bush_over, col = c("orange", "lightblue"), xlab = "Bush_over", ylab = "CLFU")
mtext("Boxplots ανα τιμη της Bush_over των μεταβλητων Rural και CLFU για Πολιτείες με μικρό αριθμό πληθυσμό", outer = TRUE,cex = 1.5,col = "black",font = 2)



par(mfrow = c(2, 2))

# Density plots για την μεταβλητή Rural ανά τιμή της Bush_over για Πολιτείες με μεγάλο αριθμό πληθυσμού
for (i in unique(high_pop$Bush_over)) {
  density_plot <- density(high_pop$Rural[high_pop$Bush_over == i])
  plot(density_plot, main = paste("Density Plot of Rural for Bush_over =", i), 
       xlab = "Rural", ylab = "Density", col = "white", lwd = 2)
  polygon(density_plot, col = "skyblue", border = "black")
}

# Density plots για την μεταβλητή CLFU ανά τιμή της Bush_over στο high_pop
for (i in unique(high_pop$Bush_over)) {
  density_plot <- density(high_pop$CLFU[high_pop$Bush_over == i])
  plot(density_plot, main = paste("Density Plot of CLFU for Bush_over =", i), 
       xlab = "CLFU", ylab = "Density", col = "white", lwd = 2)
  polygon(density_plot, col = "skyblue", border = "black")
}
mtext("Density Plot της Rural και της CLFU για τις τιμές της Bush_over για Πολιτείες με μεγάλο αριθμό πληθυσμού ", outer = TRUE,cex = 1.5,col = "black",font = 2)



# Density plots για την μεταβλητή Rural ανά τιμή της Bush_over στο low_pop
for (i in unique(low_pop$Bush_over)) {
  density_plot <- density(low_pop$Rural[low_pop$Bush_over == i])
  plot(density_plot, main = paste("Density Plot of Rural for Bush_over =", i), 
       xlab = "Rural", ylab = "Density", col = "white", lwd = 2)
  polygon(density_plot, col = "skyblue", border = "black")
}

# Density plots για την μεταβλητή CLFU ανά τιμή της Bush_over για Πολιτείες με μικρό αριθμό πληθυσμού
for (i in unique(low_pop$Bush_over)) {
  density_plot <- density(low_pop$CLFU[low_pop$Bush_over == i])
  plot(density_plot, main = paste("Density Plot of CLFU for Bush_over =", i), 
       xlab = "CLFU", ylab = "Density", col = "white", lwd = 2)
  polygon(density_plot, col = "skyblue", border = "black")
}
mtext("Density Plot της Rural και της CLFU για τις τιμές της Bush_over για Πολιτείες με μικρό αριθμό πληθυσμού ", outer = TRUE,cex = 1.5,col = "black",font = 2)





#####Μετατροπή της Population σε κατηγορική για να μας βοηθήσει στην ανάλυση μας με βάση τα τεταρτημόρια boxplot

Q1 <- quantile(b_separated$Population, 0.25)
Q2 <- median(b_separated$Population)
Q3 <- quantile(b_separated$Population, 0.75)


b_separated$Population <- cut(b_separated$Population, 
                              breaks = c(-Inf, Q1, Q3, Inf), 
                              labels = c("low", "medium", "high"))

table(b_separated$Population)



par(mfrow = c(1, 1))

population_table <- table(b_separated$Population)

###Μέσες τιμές της Bush ανα κατηγορια πληθυσμου
mean_bush_low <- mean(b_separated$Bush[b_separated$Population == "low"])
mean_bush_medium <- mean(b_separated$Bush[b_separated$Population == "medium"])
mean_bush_high <- mean(b_separated$Bush[b_separated$Population == "high"])

mean_bush_values <- c(mean_bush_low, mean_bush_medium, mean_bush_high)
###Ετικέτες των κατηγοριών
population_categories <- c("low", "medium", "high")
###barplot για μέσο ποσοστό του Bush ανά κατηγορία πληθυσμού
par(mfrow = c(1, 1))
bp<-barplot(mean_bush_values,names.arg = population_categories,main = "Μέσο ποσοστό του Bush ανά κατηγορία πληθυσμού",
            xlab = "Κατηγορία Πληθυσμού",ylab = "Μέσο ποσοστό του Bush (%)",col = "lightblue",ylim = c(0, 100))
text(x = bp, y = mean_bush_values,labels = paste0(round(mean_bush_values , 1), "%"),pos = 3, cex = 0.8, col = "black")

###Anova για Bush~Population
anova<-aov(Bush~Population,data=b_separated)
###Πραγματοποιήση και των δύο test κανονικότητας
lillie.test(anova$res)
shapiro.test(anova$res)
###Επειδη το δειγμα=51 και κυμαίνεται ανάμεσα σε μικρό η μεσαίο δείγμα αφού απορρίφθηκε η μια από τις δύο θα απορρίψουμε κανονικότητα
####Deigma>50
by(b_separated$Bush,b_separated$Population,describe)
###Αφου βλέπουμε ότι και στις τρείς κατηγορίες έχουμε κοντινές τιμές σε Μέση τιμή και διάμεσο
### και ταυτόχρονα και στις τρείς χαμηλό skew και kurtosis θεωρούμε πως είναι κατάλληλο μέτρο
###Έλεγχος ίσων διακυμάνσεων
leveneTest(anova)
###Απορρίπτεται αρα
###Έλεγχος ισότητας μέσων με άνισες διακυμάνσεις
oneway.test(Bush~Population,data=b_separated,var.equal=FALSE)
###Δεν απορρίπτεται
kruskal.test(Bush~Population,data=b_separated)
####Δεν απορρίπτεται


###Boxplots ανα κατηγορια της Population της μεταβλητης Bush 
boxplot(b_separated$Bush~b_separated$Population, col = c("orange", "lightblue"), xlab = "Population", ylab = "Bush")
mtext("Boxplots ανα κατηγορια της Population της μεταβλητης Bush", outer = TRUE,cex = 1.5,col = "black",font = 2)
plotmeans(b_separated$Bush~b_separated$Population, connect=F,col = c("orange", "lightblue"), xlab = "Population", ylab = "Bush")
mtext("Errorbars ανα κατηγορια της Population της μεταβλητης Bush", outer = TRUE,cex = 1.5,col = "black",font = 2)
pairwise.wilcox.test(b_separated$Bush,b_separated$Population)

###Dataset για το 4ο ερώτημα
b4<-b_separated[,3:11]
b4.2<-b_separated[,3:11]



###Plots για να ελέγξουμε τις υποθέσεις του αρχικού μοντέλου
model <- lm(Bush ~ Male+Population+Rural+Below_Poverty+CLFU+Management_18+Percentage_65+Num_75,data = b4)
summary(model)
par(mfrow=c(2,2))
plot(model, col = "purple")
###Plot για ανεξαρτησία
###Προετοιμασία για ελέγχους
res<-rstandard(model)
fit<-fitted(model)
par(mfrow=c(1,1))
plot(res,type="l")


####ΕΛΕΓΧΟΙ
###Έλεγχος κανονικότητας
qqnorm(res)
qqline(res)
shapiro.test(res)
lillie.test(res)


#Έλεγχος ομοσκεδαστικότητας
par(mfrow=c(2,2))
plot(fit,res)
abline(h=c(-2,2),col=2,lty=2)

plot(fit,sqrt(abs(res)))
abline(h=sqrt(2),col=2,lty=2)

plot(fit,res^2)
abline(h=4,col=2,lty=2)

qfit<-cut(fit,breaks=quantile(fit),include.lowest=T)
library(car)
leveneTest(res~qfit)
boxplot(res~qfit)

#Έλεγχος ανεξαρτησίας
par(mfrow=c(1,2))
plot(res,type="l")
acf(model$res)
dwt(model)
dwt(model$res)
dwt(model,max.lag=7)

vif(model)

###Πληρες μοντελο κανουμε κανονικοποίηση για καλύτερη ερμηνεία σταθεράς.
model <- lm(Bush ~ Male + Population + Rural + Below_Poverty + CLFU + Management_18 + Percentage_65 + Num_75, data = b4)
b4$Male <- scale(b4$Male, center = TRUE, scale = FALSE)
b4$Rural <- scale(b4$Rural, center = TRUE, scale = FALSE)
b4$Below_Poverty <- scale(b4$Below_Poverty, center = TRUE, scale = FALSE)
b4$CLFU <- scale(b4$CLFU, center = TRUE, scale = FALSE)
b4$Management_18 <- scale(b4$Management_18, center = TRUE, scale = FALSE)
b4$Percentage_65 <- scale(b4$Percentage_65, center = TRUE, scale = FALSE)
b4$Num_75 <- scale(b4$Num_75, center = TRUE, scale = FALSE)

###Μοντέλο μετά την κεντροποίηση
model_center <- lm(Bush ~ Male + Population + Rural + Below_Poverty + CLFU + Management_18 + Percentage_65 + Num_75, data = b4)

summary(model_center)
tab_model(model_center, show.ci = FALSE, show.se = TRUE, show.aic = TRUE)


###AIC
full_model <- lm(Bush ~ Male + Population + Rural + Below_Poverty + CLFU + Management_18 + Percentage_65 + Num_75, data = b4)
step(full_model,direction='both')
###BIC
full_model2 <-lm(Bush ~ Male + Population + Rural + Below_Poverty + CLFU + Management_18 + Percentage_65 + Num_75, data = b4)
n<-nrow(b4)
step(full_model2,direction='both',k=log(n))

model_aic<-lm(Bush ~ Male + Population + Rural + CLFU + Management_18 + Percentage_65 + Num_75, data = b4)
model_bic<-lm(Bush ~ Male + Rural + Management_18 + Num_75, data = b4)
###Παρατηρούμε το μοντελο με stepwise-regression AIC
summary(model_aic)
###Παρατηρούμε το μοντελο με stepwise-regression BIC
summary(model_bic)

###Επιλεγουμε αυτό που μας προτείνει το AIC καθώς περιέχει παράπανω μεταβλητές άρα και πληροφορία
###καθώς και την μεταβλητή Population.
###Ελεγχος πολυσυγγραμμικοτητας
vif(model_aic)

###Αφαιρούμε την Male με το μεγαλύτερο διορθώνοντας το πρόβλημα
model_aic<-lm(Bush ~ Population + Rural + CLFU + Management_18 + Percentage_65 + Num_75, data = b4)

summary(model_aic)
###Αφαιρούμε την στατιστικά μη σημαντική Management_18 
model_aic<-lm(Bush ~ Population + Rural + CLFU + Percentage_65 + Num_75, data = b4)
vif(model_aic)
summary(model_aic)
###Όλες εκτός της Populationmedium είναι στατιστικά σημαντικές αλλά δεν θα την αφαιρέσουμε
###Καθώς η Populationlow βρίσκεται στην σταθερά και φαίνεται πολύ σημαντική και σημαντική είναι
###και η PopulationHigh

tab_model(model_aic, show.ci = FALSE, show.se = TRUE, show.aic = TRUE)

par(mfrow=c(2,2))
plot(model_aic, col = "orange")
###Plot για ανεξαρτησία
###Προετοιμασία για ελέγχους
res<-rstandard(model_aic)
fit<-fitted(model_aic)
par(mfrow=c(1,1))
plot(res,type="l")


####ΕΛΕΓΧΟΙ
###Έλεγχος κανονικότητας
qqnorm(res)
qqline(res)
shapiro.test(res)
lillie.test(res)


#Έλεγχος ομοσκεδαστικότητας
par(mfrow=c(2,2))
plot(fit,res)
abline(h=c(-2,2),col=2,lty=2)

plot(fit,sqrt(abs(res)))
abline(h=sqrt(2),col=2,lty=2)

plot(fit,res^2)
abline(h=4,col=2,lty=2)

qfit<-cut(fit,breaks=quantile(fit),include.lowest=T)
library(car)
leveneTest(res~qfit)
boxplot(res~qfit)

#Έλεγχος ανεξαρτησίας
par(mfrow=c(1,2))
plot(res,type="l")
acf(model_aic$res)
dwt(model_aic)
dwt(model_aic$res)
dwt(model_aic,max.lag=7)

vif(model_aic)


### Χρησιμοποιουμε το dataset πριν την κεντροποιηση
b4.2$log_Num_75 <- log(b4.2$Num_75)

###(Μετατροπή σε λογάριθμο της Num_75)
model_Metasximatismou<-lm(Bush ~ Population + Rural + CLFU + Percentage_65 + log_Num_75, data = b4.2)
par(mfrow=c(2,2))
plot(model_Metasximatismou, col = "red")

tab_model(model_Metasximatismou, show.ci = FALSE, show.se = TRUE, show.aic = TRUE)
###Plot για ανεξαρτησία
###Προετοιμασία για ελέγχους
res<-rstandard(model_Metasximatismou)
fit<-fitted(model_Metasximatismou)
par(mfrow=c(1,1))
plot(res,type="l")


####ΕΛΕΓΧΟΙ
###Έλεγχος κανονικότητας καταλοίπων
qqnorm(res)
qqline(res)
shapiro.test(res)
lillie.test(res)
###Δεν απορρίπτεται

#Έλεγχος ομοσκεδαστικότητας
par(mfrow=c(2,2))
plot(fit,res)
abline(h=c(-2,2),col=2,lty=2)

plot(fit,sqrt(abs(res)))
abline(h=sqrt(2),col=2,lty=2)

plot(fit,res^2)
abline(h=4,col=2,lty=2)

qfit<-cut(fit,breaks=quantile(fit),include.lowest=T)
library(car)
leveneTest(res~qfit)
###Δεν απορρίπτεται
boxplot(res~qfit)

#Έλεγχος ανεξαρτησίας
par(mfrow=c(1,2))
plot(res,type="l")
acf(model_Metasximatismou$res)
dwt(model_Metasximatismou)
dwt(model_Metasximatismou$res)
dwt(model_Metasximatismou,max.lag=7)
###Δεν απορρίπτεται

###Ελ. πολυσυγγραμμικότητας
vif(model_Metasximatismou)
###Δεν απορρίπτεται
###Το AIC του μοντέλου
AIC(model_Metasximatismou)

###Κεντροποίηση για καλύτερη ερμηνεία του μοντέλου
b4.2$Rural_c <- b4.2$Rural - mean(b4.2$Rural)
b4.2$CLFU_c <- b4.2$CLFU - mean(b4.2$CLFU)
b4.2$Percentage_65_c <- b4.2$Percentage_65 - mean(b4.2$Percentage_65)
b4.2$log_Num_75_c <- b4.2$log_Num_75 - mean(b4.2$log_Num_75)

model_Metasximatismou2 <- lm(Bush ~ Population + Rural_c + CLFU_c + Percentage_65_c + log_Num_75_c, data = b4.2)

summary(model_Metasximatismou2)
###Το μοντέλο μετά την κεντροποίηση.
tab_model(model_Metasximatismou2, show.ci = FALSE, show.se = TRUE, show.aic = TRUE)




