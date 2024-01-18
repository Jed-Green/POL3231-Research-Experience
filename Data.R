library(haven)
library(gmodels)
library(rcompanion)
library(chisq.posthoc.test)
library(stargazer)
library(ggplot2)
library(tidyverse)
library(xtable)
library(AICcmodavg)
library(rmarkdown)


df<-read_sav("C19PRC_UKW1W2_archive_final.sav")


### Descriptive statistics:

round((prop.table(table(df$W1_Voted_GenElection))*100),digits = 2) 
round((prop.table(table(df$W1_EURef))*100),digits = 2)
table(df$W1_Depression_Total,df$W1_Voted_GenElection)
table(df$W1_Voted_GenElection_YorN)
table(df$W1_Depression_Total)
table(df$W1_Voted_GenElection)
table(df$W1_Overall_Trust)
table(df$W1_GAD_Total)
table(df$W1_Dep_9)

hist(df$W1_Depression_Total,
     breaks = 30,
     xlim = c(0,30),
     ylim = c(0,800),
     xlab = "PHQ-9 Total Test Score",
     main = "Histogram of PHQ-9 Total Test Score")

barplot(prop.table(table(df$W1_Voted_GenElection_YorN)),
        ylim = c(0,1),
        ylab = "Percentage",
        xlab = "Voted",
        main = "Barplot showing Total Number 
        of People Who Said They Voted in 
        The Last General Election")

table(mdf4$W1_Dep_Severity)


hist(df$W1_Overall_Trust,
     breaks = 35,
     xlim = c(7,35),
     ylim = c(0,250),
     xlab = "Trust Total Score",
     main = "Histogram of Trust Total Score")

table(mdf4$W1_Dep_Severity)
data <- c(378, 227, 154, 1199, 67)
categories <- c("Mild", "Moderate", "Moderately Severe", "None minimal", "Severe")

# Create a data frame
BP1 <- data.frame(Category = categories, Count = data)

# Sort the data frame by Count in descending order
BP1 <- BP1[order(-BP1$Count), ]

# Create the bar plot
barplot(BP1$Count, names.arg = BP1$Category, main = "Bar Plot with Descending Bars",
        xlab = "Category", ylab = "Count")

hist(df$W1_GAD_Total,
     breaks = 21,
     xlim = c(0,21),
     ylim = c(0,800),
     xlab = "GAD-7 Total Test Score",
     main = "Histogram of GAD-7 Total Test Score")

hist(df$W1_Political_Scale,
     breaks = 10,
     xlim = c(1,10),
     ylim = c(0,800),
     xlab = "Poltical scale",
     main = "Histogram of Poltical Scale")

hist(mdf4$W1_Paranoia_Total,
    breaks = 20,
     xlim = c(5,30),
     ylim = c(0,400),
     xlab = "Paranoia Total",
     main = "Histogram of Paranoia Total")

hist(mdf4$W1_Conscientiousness_Total,
     breaks = 8,
     xlim = c(2,10),
     ylim = c(0,500),
     xlab = " Conscientiousness Total",
     main = "Histogram of Conscientiousness Total")

plot(df$W1_Overall_Trust,df$W1_Depression_Total)
abline(lm(df$W1_Depression_Total ~ df$W1_Overall_Trust), col = "red")

plot(df$W1_GAD_Total,df$W1_Depression_Total)
abline(lm(df$W1_GAD_Total~df$W1_Depression_Total), col = "red")
cor(df$W1_GAD_Total,df$W1_Depression_Total)

# correlation Matrix
cor.matrix<-cor(df[c("W1_Depression_Total", "W1_GAD_Total", "W1_Overall_Trust","W1_Voted_GenElection_YorN", "W1_Political_Scale" )], use = "complete.obs")
write.csv(cor.matrix, file = "correlation_matrix.csv", row.names = TRUE)
?xtable
### Chi-Squared Test

# Re-coding variable
df$W1_Dep_Severity <- NA
df$W1_Dep_Severity[df$W1_Depression_Total <= 4 ] <- "None minimal"
df$W1_Dep_Severity[df$W1_Depression_Total >= 5 & df$W1_Depression_Total <= 9 ] <- "Mild"
df$W1_Dep_Severity[df$W1_Depression_Total >= 10 & df$W1_Depression_Total <= 14] <- "Moderate"
df$W1_Dep_Severity[df$W1_Depression_Total >= 15 & df$W1_Depression_Total <= 19] <- "Moderately Severe"
df$W1_Dep_Severity[df$W1_Depression_Total >= 20 & df$W1_Depression_Total <= 27] <- "Severe"
table(df$W1_Dep_Severity,df$W1_Voted_GenElection)


# Making Frequency table
freqs<- matrix(c(1061,116,310,56,186,33,129,23,45,17), ncol = 5, byrow = F)
rownames(freqs) = c("Voted", "Did not Vote")
colnames(freqs) = c("None minimal","Mild","Moderate","Moderately Severe","Severe")
freqs.table = as.table(freqs)
print(freqs.table)

# Expected value table
CrossTable(freqs.table, expected = T, prop.r = F, prop.c = F, prop.t = F)

# Chisq square test 
chisq.test(freqs.table, correct = F)

# Effect size
cohenW(freqs.table)

# Post-hoc test
chisq.posthoc.test(freqs.table, method = 'bonferroni')

# Critical value calculation
chisq.test(freqs.table, correct = F)$stdres
alpha<- 0.05
alpha_adj<-alpha/(nrow(freqs.table)*ncol(freqs.table))
qnorm(alpha_adj/2)

### Linear Regression

# Coding binary variable
df$W1_Voted_GenElection_YorN <- NA
df$W1_Voted_GenElection_YorN[df$W1_Voted_GenElection == 1 ] <- 1
df$W1_Voted_GenElection_YorN[df$W1_Voted_GenElection == 2]<- 0
table(df$W1_Voted_GenElection_YorN)

# Coding categorical variable
df$W1_Overall_Trust<-df$W1_Trust_Body1+df$W1_Trust_Body2+df$W1_Trust_Body3+df$W1_Trust_Body4+df$W1_Trust_Body5+df$W1_Trust_Body6+df$W1_Trust_Body7
table(df$W1_Overall_Trust)

### Linear regression models

# Depression and its effect on Turnout

m1<-lm(df$W1_Voted_GenElection_YorN~df$W1_Depression_Total)
summary(m1)
stargazer(m1, type="text",out="M1.txt", df=FALSE)

plot(y = df$W1_Voted_GenElection_YorN, 
    x = df$W1_Depression_Total,
    main = "Scatterplot Voting and Depression Total",
    xlab = "Depression Total"  ,
    ylab = "Vote",
    pch = 20,
    ylim = c(-0.4, 1.4),
    xlim = c(0,27),
    cex.main = 0.8)
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(25, 0.9, cex = 0.8, "Voted")
text(25, -0.1, cex= 0.8, "Did Not Vote")
abline(m1,
       lwd = 1.8, 
       col = "steelblue")

# Anxiety and its effect on turnout
m5<-lm(df$W1_Voted_GenElection_YorN~df$W1_GAD_Total)
summary(m5)
stargazer(m5, type="text",out="M5.txt", df=FALSE)

plot(y = df$W1_Voted_GenElection_YorN, 
     x = df$W1_GAD_Total,
     main = "Scatterplot Voting and Anxiety Total",
     xlab = "Anxiety Total"  ,
     ylab = "Vote",
     pch = 20,
     ylim = c(-0.4, 1.4),
     xlim = c(0,21),
     cex.main = 0.8)
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(15, 0.9, cex = 0.8, "Voted")
text(15, -0.1, cex= 0.8, "Did Not Vote")
abline(m5,
       lwd = 1.8, 
       col = "steelblue")

# Trust and its effect on turnout

m2<-lm(df$W1_Voted_GenElection_YorN~df$W1_Overall_Trust)
summary(m2)
stargazer(m2, type="text",out="M2.txt", df=FALSE)

plot(y = df$W1_Voted_GenElection_YorN, 
     x = df$W1_Overall_Trust,
     xlab = "Trust Total"  ,
     main = "Scatterplot Voting and Trust Total",
     ylab = "Vote",
     pch = 20,
     ylim = c(-0.4, 1.4),
     xlim = c(7,35),
     cex.main = 0.8)
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(25, 0.9, cex = 0.8, "Voted")
text(25, -0.1, cex= 0.8, "Did Not Vote")
abline(m2,
       lwd = 1.8, 
       col = "steelblue")

# Depression and its effect on trust

m3<-lm(df$W1_Overall_Trust ~ df$W1_Depression_Total)
summary(m3)
stargazer(m3, type="text",out="M3.txt", df=FALSE)

plot(x= df$W1_Depression_Total,
     y=df$W1_Overall_Trust,
     ylab = "Overall Trust",
     xlab = "Depression Total",
     main = "Scattergraph of Depression and its effect on Trust")
abline(m3,
       lwd = 1.8, 
       col = "steelblue")



# Depression and its effect on Poltical scale

m4<- lm(df$W1_Political_Scale~df$W1_Depression_Total)
summary(m4)
stargazer(m4, type="text",out="M4.txt", df=FALSE)

plot(y= df$W1_Political_Scale,
     x=df$W1_Depression_Total,
     ylab = "Poltical scale",
     xlab = "Depression Total",
     main = "Scattergraph of Depression and 
     its effect on the Poltical scale")
abline(m4,
       lwd = 1.8, 
       col = "steelblue")

### Wave attrition code
table(df$W1_Present)
table(df$W2_Present)

# re-coding variable 
df$W2_Present[is.na(df$W2_Present)] <- 0

# creating freq tables
table(df$W1_Dep_Severity)
table(df$W1_Dep_Severity[df$W2_Present == 0])
prop.table(table(df$W1_Dep_Severity))*100
prop.table(table(df$W1_Dep_Severity[df$W2_Present == 0]))*100

tab<-prop.table(table(df$W1_Dep_Severity))*100
tab<-as.data.frame(tab)

tab1<-prop.table(table(df$W1_Dep_Severity[df$W2_Present == 0]))*100
tab1<-as.data.frame(tab1)

#Graphs for just wave 1

pie(prop.table(table(df$W1_Dep_Severity))*100,
    main = "Pie chart showing percentage of Wave 1 respondants and their PHQ-9 category")

G1 <- tab %>%
  mutate(Var1 = fct_relevel(Var1, 
                            "None minimal", "Mild", "Moderate", 
                            "Moderately Severe", "Severe")) %>%
  ggplot(aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity") +
  xlab("PHQ-9 Depression Scale") +
  ylim(0,65)
G1

#Graphs for Wave 2 Non-respondents 

pie(prop.table(table(df$W1_Dep_Severity[df$W2_Present == 0]))*100,
    main = "Pie chart showing percentage of Wave 1 respondants,
    who didn't respond in wave 2 and their PHQ-9 category")

G2 <- tab1 %>%
  mutate(Var1 = fct_relevel(Var1, 
                            "None minimal", "Mild", "Moderate", 
                            "Moderately Severe", "Severe")) %>%
  ggplot(aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity") +
  xlab("PHQ-9 Depression Scale") +
  ylim(0,65)
G2

### 

m6<-lm(df$W1_Voted_GenElection_YorN~df$W1_Depression_Total + df$W1_Overall_Trust + df$W1_Political_Scale) 
summary(m6)
stargazer(m6, type="text",out="M6.txt", df=FALSE)

###########

### Wave attrition code:
df$W2_present_New<-ifelse(df$W2_Present == 1, 0,1)
table(df$W2_present_New)

###
m7<-lm(df$W2_Present~df$W1_Depression_Total)
summary(m7)
stargazer(m7, type="text",out="M7.txt", df=FALSE)

plot(y = df$W2_Present, 
     x = df$W1_Depression_Total,
     xlab = "Depression Total"  ,
     main = "Scatterplot present and depression Total",
     ylab = "present",
     pch = 20,
     ylim = c(-0.4, 1.4),
     xlim = c(0,27),
     cex.main = 0.8)
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(20, 0.9, cex = 0.8, "Present")
text(20, -0.1, cex= 0.8, "Not present")
abline(m7,
       lwd = 1.8, 
       col = "steelblue")

bm1<-glm(df$W2_Present~df$W1_Depression_Total)
summary(bm1)


#####################################################################################################################################


###Attrition code:

### Reading in all databases:

df1<-read_sav("C19PRC_UK_W3_archive_final.sav")
df2<-read_sav("C19PRC_UK_W4_archive_final.sav")
df3<-read_sav("C19PRC_UKW5_archive_final.sav")
df4<-read_sav("C19PRC_UK_W6_archive_final.sav")

### Making the databases contain only variables needed:

ldf1<-as.data.frame(cbind(df1$pid,df1$W3_Type))
colnames(ldf1)<-c("pid","W3_Type")

ldf2<-as.data.frame(cbind(df2$pid,df2$W4_Type))
colnames(ldf2)<-c("pid","W4_Type")

ldf3<-as.data.frame(cbind(df3$pid,df3$W5_Type))
colnames(ldf3)<-c("pid","W5_Type")

ldf4<-as.data.frame(cbind(df4$pid,df4$W6_Type))
colnames(ldf4)<-c("pid","W6_Type")


### Merging datasets:

mdf1<-merge.data.frame(df,ldf1, by ="pid", all = T)
mdf2<-merge.data.frame(mdf1,ldf2, by ="pid", all = T)
mdf3<-merge.data.frame(mdf2,ldf3, by ="pid", all = T)
mdf4<-merge.data.frame(mdf3,ldf4, by ="pid", all = T)

### Re-coding variable: answering waves one after another in order:

table(mdf4$W1_Present)
mdf4$presentwave1_2 <- ifelse(mdf4$W1_Present == 1 & mdf4$W2_Present == 1,1,0)
mdf4$presentwave1_2_3 <-ifelse(mdf4$W1_Present == 1 & mdf4$W2_Present == 1 & mdf4$W3_Type == 0,1,0)
mdf4$presentwave1_2_3_4<-ifelse(mdf4$W1_Present == 1 & mdf4$W2_Present == 1 & mdf4$W3_Type == 0 & mdf4$W4_Type == 1,1,0)
mdf4$presentwave1_2_3_4_5<-ifelse(mdf4$W1_Present == 1 & mdf4$W2_Present == 1 & mdf4$W3_Type == 0 & mdf4$W4_Type == 1 & mdf4$W5_Type == 1,1,0)
mdf4$presentwave1_2_3_4_5_6<-ifelse(mdf4$W1_Present == 1 & mdf4$W2_Present == 1 & mdf4$W3_Type == 0 & mdf4$W4_Type 
      == 1 & mdf4$W5_Type == 1 & mdf4$W6_Type == 1,1,0)

mdf4$W1_Dep_Severity <- NA

mdf4$W1_Dep_Severity[mdf4$W1_Depression_Total >= 0 &mdf4$W1_Depression_Total <= 4 ] <- "None minimal"
mdf4$W1_Dep_Severity[mdf4$W1_Depression_Total >= 5 & mdf4$W1_Depression_Total <= 9 ] <- "Mild"
mdf4$W1_Dep_Severity[mdf4$W1_Depression_Total >= 10 & mdf4$W1_Depression_Total <= 14] <- "Moderate"
mdf4$W1_Dep_Severity[mdf4$W1_Depression_Total >= 15 & mdf4$W1_Depression_Total <= 19] <- "Moderately Severe"
mdf4$W1_Dep_Severity[mdf4$W1_Depression_Total >= 20 & mdf4$W1_Depression_Total <= 27] <- "Severe"
table(mdf4$W1_Dep_Severity)

### Putting previous variable into a matrix

tab <- matrix(c(2025,1406, 950, 771, 677, 582), ncol=1, byrow=TRUE)
rownames(tab) <- c('Wave 1','Wave 2','Wave 3','Wave 4','Wave 5','Wave 6')
colnames(tab) <- c('Present')
tabdf<-as.data.frame(tab)
tabdf$Waves <- rownames(tabdf)

### Plots for Attrition between waves:

ggplot(tabdf, aes(x = Waves, y = Present, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(title = "Frequency by Waves", x = "Waves", y = "Present") +
  theme_minimal()

tabdf$Percentage <- (tabdf$Present / tabdf$Present[1]) * 100
ggplot(tabdf, aes(x = Waves, y = Percentage, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(title = "Frequency by Waves", x = "Waves", y = "Percentage of Wave 1") +
  theme_minimal()

### Depression and wave attrition
table(mdf4$W1_Present, mdf4$W1_Dep_Severity)
table(mdf4$presentwave1_2, mdf4$W1_Dep_Severity)
table(mdf4$presentwave1_2_3, mdf4$W1_Dep_Severity)
table(mdf4$presentwave1_2_3_4, mdf4$W1_Dep_Severity)
table(mdf4$presentwave1_2_3_4_5,mdf4$W1_Dep_Severity)
table(mdf4$presentwave1_2_3_4_5_6,mdf4$W1_Dep_Severity)

### Creating the data frame from the table

wave_data <- data.frame(
  Severity = c("None minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderately Severe (15-19)", "Severe (20-27)"),
  Wave_1 = c(1199,  378, 227, 154, 67),
  Wave_2 = c(896 , 243, 138, 95, 34),
  Wave_3 = c(637, 163, 78, 59, 13),
  Wave_4 = c(536, 124, 55, 47, 9),
  Wave_5 = c(474, 107, 51, 37, 8),
  Wave_6 = c(414, 91, 41, 28, 8)
)
 
### Making the graphs:

wave_data_long <- pivot_longer(wave_data, cols = -Severity, names_to = "Wave_Number", values_to = "Frequency")

custom_colors <- c("None minimal (0-4)" = "darkgreen", "Mild (5-9)"= "lightgreen","Moderate (10-14)" = "orange",
                   "Moderately Severe (15-19)" = "red", "Severe (20-27)" = "darkred")
legend_order <- c("None minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderately Severe (15-19)", "Severe (20-27)")

### Plotting the line graph for Frequencies:

ggplot(wave_data_long, aes(x = Wave_Number, y = Frequency, color = Severity, group = Severity)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors
  labs(title = "Frequency of Depression Severity Levels Across Waves",
       x = "Wave Number", y = "Frequency") +
  theme_minimal()

### Calculating percentages relative to Wave 1 for each severity level:

wave_data_long <- wave_data_long %>%
  group_by(Severity) %>%
  mutate(Percentage = Frequency / Frequency[Wave_Number == "Wave_1"] * 100)

###Plotting the line graph with percentages relative to Wave 1 on the y-axis:

ggplot(wave_data_long, aes(x = Wave_Number, y = Percentage, color = Severity, group = Severity)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of Depression Severity Levels and those who answered consecutive waves 
       Relative to Wave 1",
       x = "Wave Number", y = "Percentage") +
  theme_minimal() +
  ylim(0,100)


### ANOVA of above data

two.way<-aov(wave_data_long$Percentage~wave_data_long$Severity+ wave_data_long$Wave_Number)
summary(two.way)

TukeyHSD(two.way)

### OCD and wave attrition

### Recoding variable:

mdf4$presentwave2 <- ifelse(mdf4$W2_Present == 1,1,0)
mdf4$presentwave2_3 <-ifelse(mdf4$W2_Present == 1 & mdf4$W3_Type == 0,1,0)
mdf4$presentwave2_3_4<-ifelse(mdf4$W2_Present == 1 & mdf4$W3_Type == 0 & mdf4$W4_Type == 1,1,0)
mdf4$presentwave2_3_4_5<-ifelse(mdf4$W2_Present == 1 & mdf4$W3_Type == 0 & mdf4$W4_Type == 1 & mdf4$W5_Type == 1,1,0)
mdf4$presentwave2_3_4_5_6<-ifelse(mdf4$W2_Present == 1 & mdf4$W3_Type == 0 & mdf4$W4_Type 
                                    == 1 & mdf4$W5_Type == 1 & mdf4$W6_Type == 1,1,0)

m8<-lm(mdf4$presentwave2_3_4_5_6~mdf4$W2_OCI_Total)
summary(m8)
plot(y = mdf4$presentwave2_3_4_5_6, 
     x = mdf4$W2_OCI_Total,
     xlab = "OCD Total"  ,
     main = "Scatterplot present and depression Total",
     ylab = "present",
     pch = 20,
     ylim = c(-0.4, 1.4),
     xlim = c(0,72),
     cex.main = 0.8)
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(25, 0.9, cex = 0.8, "present")
text(25, -0.1, cex= 0.8, "not present")
abline(m8,
       lwd = 1.8, 
       col = "steelblue")


table(mdf4$presentwave2, mdf4$W2_OCI_Cat)
table(mdf4$presentwave2_3, mdf4$W2_OCI_Cat)
table(mdf4$presentwave2_3_4, mdf4$W2_OCI_Cat)
table(mdf4$presentwave2_3_4_5,mdf4$W2_OCI_Cat)
table(mdf4$presentwave2_3_4_5_6,mdf4$W2_OCI_Cat)

OCD_data <- data.frame(
  OCD = c("Under OCD Threshold", "Over OCD Threshold"),
  Wave_2 = c(1103, 303),
  Wave_3 = c(776, 174),
  Wave_4 = c(664, 127),
  Wave_5 = c(568, 109),
  Wave_6 = c(491, 91)
)

OCD_data_long <- pivot_longer(OCD_data, cols = -OCD, names_to = "Wave_Number", values_to = "Frequency")

custom_colors <- c("Over OCD Threshold" = "darkgreen", "Under OCD Threshold" = "darkred")
legend_order <- c("Over OCD Threshold", "Under OCD Threshold")

# Plotting the line graph using ggplot2 with explicit grouping and custom colors
ggplot(OCD_data_long, aes(x = Wave_Number, y = Frequency, color = OCD, group = OCD)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors
  labs(title = "Frequency of OCD Levels Across Waves",
       x = "Wave Number", y = "Frequency") +
  theme_minimal()

# Calculating percentages relative to Wave 1 for each severity level
OCD_data_long <- OCD_data_long %>%
  group_by(OCD) %>%
  mutate(Percentage = Frequency / Frequency[Wave_Number == "Wave_2"] * 100)

# Plotting the line graph with percentages relative to Wave 1 on the y-axis
ggplot(OCD_data_long, aes(x = Wave_Number, y = Percentage, color = OCD, group = OCD)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of OCD Levels Relative to Wave 1 Across Waves",
       x = "Wave Number", y = "Percentage") +
  theme_minimal()

### ANOVA

two.way2<-aov(OCD_data_long$Percentage~OCD_data_long$OCD+ OCD_data_long$Wave_Number)
summary(two.way2)

TukeyHSD(two.way2)

### Anxiety
mdf4$W1_Anx_Severity <- NA
mdf4$W1_Anx_Severity[mdf4$W1_GAD_Total <= 4 ] <- "Minimal"
mdf4$W1_Anx_Severity[mdf4$W1_GAD_Total >= 5 & mdf4$W1_GAD_Total <= 9 ] <- "Mild"
mdf4$W1_Anx_Severity[mdf4$W1_GAD_Total >= 10 & mdf4$W1_GAD_Total <= 14] <- "Moderate"
mdf4$W1_Anx_Severity[mdf4$W1_GAD_Total >= 15 ] <- "Severe"
table(mdf4$W1_Anx_Severity,mdf4$W1_Voted_GenElection)


table(mdf4$W1_Present, mdf4$W1_Anx_Severity)
table(mdf4$presentwave1_2, mdf4$W1_Anx_Severity)
table(mdf4$presentwave1_2_3, mdf4$W1_Anx_Severity)
table(mdf4$presentwave1_2_3_4, mdf4$W1_Anx_Severity)
table(mdf4$presentwave1_2_3_4_5,mdf4$W1_Anx_Severity)
table(mdf4$presentwave1_2_3_4_5_6,mdf4$W1_Anx_Severity)

Anx_data <- data.frame(
  Severity = c("Minimal", "Mild", "Moderate", "Severe"),
  Wave_1 = c(1145, 442, 266, 172),
  Wave_2 = c(841, 302, 156, 107),
  Wave_3 = c(595, 194, 106, 55),
  Wave_4 = c(499, 149, 78 , 45),
  Wave_5 = c(441, 130, 69, 37),
  Wave_6 = c(382, 110, 57, 33)
)

Anx_data_long <- pivot_longer(Anx_data, cols = -Severity, names_to = "Wave_Number", values_to = "Frequency")

custom_colors <- c("Minimal" = "green", "Mild" = "orange", "Moderate" = "red","Severe" = "darkred")
legend_order <- c("Minimal", "Mild", "Moderate", "Severe")

# Plotting the line graph using ggplot2 with explicit grouping and custom colors
ggplot(Anx_data_long, aes(x = Wave_Number, y = Frequency, color = Severity, group = Severity)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors
  labs(title = "Frequency of Anxiety Severity Levels Across Waves",
       x = "Wave Number", y = "Frequency") +
  theme_minimal()

# Calculating percentages relative to Wave 1 for each severity level
Anx_data_long <- Anx_data_long %>%
  group_by(Severity) %>%
  mutate(Percentage = Frequency / Frequency[Wave_Number == "Wave_1"] * 100)

# Plotting the line graph with percentages relative to Wave 1 on the y-axis
ggplot(Anx_data_long, aes(x = Wave_Number, y = Percentage, color = Severity, group = Severity)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of Anxiety Severity Levels Relative to Wave 1 Across Waves",
       x = "Wave Number", y = "Percentage") +
  theme_minimal()

two.way3<-aov(Anx_data_long$Percentage~Anx_data_long$Severity+ Anx_data_long$Wave_Number)
summary(two.way3)

TukeyHSD(two.way3)


### Number of waves completed and level of depression

mdf4$W3_Type[is.na(mdf4$W3_Type)]<--99
mdf4$W4_Type[is.na(mdf4$W4_Type)]<--99
mdf4$W5_Type[is.na(mdf4$W5_Type)]<--99
mdf4$W6_Type[is.na(mdf4$W6_Type)]<--99

mdf4$Waves_attended<-0
mdf4$Waves_attended <- ifelse(mdf4$W1_Present == 1, mdf4$Waves_attended + 1, mdf4$Waves_attended)
mdf4$Waves_attended <- ifelse(mdf4$W2_Present == 1, mdf4$Waves_attended + 1, mdf4$Waves_attended)
mdf4$Waves_attended <- ifelse(mdf4$W3_Type == 0, mdf4$Waves_attended + 1, mdf4$Waves_attended)
mdf4$Waves_attended <- ifelse(mdf4$W4_Type == 1, mdf4$Waves_attended + 1, mdf4$Waves_attended)
mdf4$Waves_attended <- ifelse(mdf4$W5_Type == 1, mdf4$Waves_attended + 1, mdf4$Waves_attended)
mdf4$Waves_attended <- ifelse(mdf4$W6_Type == 1, mdf4$Waves_attended + 1, mdf4$Waves_attended)
table(mdf4$Waves_attended)
table(mdf4$Waves_attended,mdf4$W1_Dep_Severity)
table(mdf4$W1_Present, mdf4$W1_Dep_Severity)



wave_data1 <- data.frame(
  Severity = c("None minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderately Severe (15-19)", "Severe (20-27)"),
  At_Least_One_Wave = c(1199, 378, 227, 154, 67),
  At_Least_Two_Waves = c(1072, 309, 178, 125, 43),
  At_Least_Three_Waves = c(912, 215, 130, 91, 32),
  At_Least_Four_Waves = c(754, 187, 97, 63, 23),
  At_Least_Five_Waves = c(636, 157, 78, 53, 18),
  All_Six_Waves = c(414, 91, 41, 28, 8)
)

custom_colors <- c("None minimal (0-4)" = "darkgreen", "Mild (5-9)"= "lightgreen","Moderate (10-14)" = "orange",
                   "Moderately Severe (15-19)" = "red", "Severe (20-27)" = "darkred")
legend_order <- c("None minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Moderately Severe (15-19)", "Severe (20-27)")


wave_data1_long <- wave_data1 %>%
  pivot_longer(cols = -Severity, names_to = "Waves", values_to = "Counts") %>%
  mutate(Waves = factor(Waves, levels = c("At_Least_One_Wave", "At_Least_Two_Waves", "At_Least_Three_Waves", "At_Least_Four_Waves", "At_Least_Five_Waves", "All_Six_Waves")))

# Plotting
ggplot(wave_data1_long, aes(x = Waves, y = Counts, group = Severity, color = Severity)) +
  geom_line() +
  labs(title = "Depression Severity Counts Across Waves Completed",
       x = "Waves",
       y = "Count") +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # You can change the palette if needed
  theme_minimal()

wave_data1_long <- wave_data1_long %>%
  group_by(Severity) %>%
  mutate(Percentage = Counts / Counts[Waves == "At_Least_One_Wave"] * 100)

# Plotting the line graph with percentages relative to Wave 1 on the y-axis
ggplot(wave_data1_long, aes(x = Waves, y = Percentage, color = Severity, group = Severity)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of waves completed per person",
       x = "Number of waves completed", y = "Percentage") +
  theme_minimal()

### Barplot of total number of waves completed by each person

barplot(table(mdf4$Waves_attended),
ylim = c(0,600),
ylab = "Count",
xlab = "Total Waves attended",
main = "Barplot showing Total Number of Waves each Person completed 
(Assuming they Attended Wave 1 and were not a Top-up)")

### Creating cumulative table

# Get the initial table
initial_table <- table(mdf4$Waves_attended)

# Calculate cumulative sums
cumulative_counts <- cumsum(rev(initial_table))

# Display cumulative counts
cumulative_counts

reversed_counts <- rev(cumulative_counts)


### Counts of waves attended cumulative graph

### Create labels for the bars
labels <- c('At least one wave', 'At least two waves', 'At least three waves', 'At least four waves', 'At least five waves', 'All six waves')

barplot(reversed_counts, 
        names.arg = labels,
        ylim = c(0,2250),
        xlab = 'Waves Attended', 
        ylab = 'Number of Individuals',
        main = 'Number of Individuals Attending at Least a Certain Number of Waves',
        col = 'skyblue',
        border = 'black')
      
cumulative_percentages <- 100 * cumsum(rev(initial_table)) / sum(initial_table)


### Percentage of waves attended cumulative graph

reversed_percentages <- rev(cumulative_percentages)
barplot(reversed_percentages, 
        names.arg = labels, 
        xlab = 'Waves Attended', 
        ylab = 'Percentage of Individuals (%)',
        main = 'Percentage of Individuals Attending at Least a Certain Number of Waves',
        col = 'lightblue',
        border = 'black') # horizontal bar plot



m9<-lm(mdf4$Waves_attended ~mdf4$W1_Depression_Total + mdf4$W1_Overall_Trust +mdf4$W1_Paranoia_Total + mdf4$W1_GAD_Total + mdf4$W1_PTSD_Total + mdf4$W1_ReligiousBelief_Total+ mdf4$W1_Humanity_Total + mdf4$W1_Loneliness_Total+ mdf4$W1_Extraversion_Total+ mdf4$W1_Agreeableness_Total + mdf4$W1_Conscientiousness_Total + mdf4$W1_Neuroticism_Total+mdf4$W1_Openness_Total+ mdf4$W1_Resilience_Total + mdf4$W1_Death_Anxiety_Total +mdf4$W1_Nationalism_Total + mdf4$W1_National_Pride_Total+ mdf4$W1_Conspiracy_Total)
summary(m9)
stargazer(m9, type="text",out="M9.txt", df=FALSE)



selected_columns <- c(
  "Waves_attended",
  "W1_Depression_Total",
  "W1_Overall_Trust",
  "W1_Paranoia_Total",
  "W1_GAD_Total",
  "W1_PTSD_Total",
  "W1_ReligiousBelief_Total",
  "W1_Humanity_Total",
  "W1_Loneliness_Total",
  "W1_Extraversion_Total",
  "W1_Agreeableness_Total",
  "W1_Conscientiousness_Total",
  "W1_Neuroticism_Total",
  "W1_Openness_Total",
  "W1_Resilience_Total",
  "W1_Death_Anxiety_Total",
  "W1_Nationalism_Total",
  "W1_National_Pride_Total",
  "W1_Conspiracy_Total"
)

# Create a subset of your dataframe with the selected columns
selected_data <- mdf4[, selected_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(selected_data, use = "pairwise.complete.obs")

# Display correlation table
print(correlation_matrix)

###

mdf4$W1_Conscientiousness_Cat <- NA
mdf4$W1_Conscientiousness_Cat[mdf4$W1_Conscientiousness_Total >= 2 & mdf4$W1_Conscientiousness_Total <= 4  ] <- "Low"
mdf4$W1_Conscientiousness_Cat[mdf4$W1_Conscientiousness_Total >= 5 & mdf4$W1_Conscientiousness_Total <= 7 ] <- "Medium"
mdf4$W1_Conscientiousness_Cat[mdf4$W1_Conscientiousness_Total >= 8 & mdf4$W1_Conscientiousness_Total <= 10] <- "High"
table(mdf4$Waves_attended, mdf4$W1_Conscientiousness_Cat)
table(mdf4$W1_Present,mdf4$W1_Conscientiousness_Cat)

wave_data2 <- data.frame(
  Conscientiousness_level = c("Low (2-4)", "Medium (5-7)", "High (8-10)"),
  At_Least_One_Waves = c(74, 953, 998),
  At_Least_Two_Waves = c(60, 803, 894),
  At_Least_Three_Waves = c(48, 653, 781),
  At_Least_Four_Waves = c(40, 522, 643),
  At_Least_Five_Waves = c(30, 421, 521),
  All_Six_Waves = c(16, 239, 327)
)

custom_colors <- c("Low (2-4)" = "green", "Medium (5-7)"= "orange","High (8-10)" = "red")
legend_order <- c("Low (2-4)", "Medium (5-7)","High (8-10)")


wave_data2_long <- wave_data2 %>%
  pivot_longer(cols = -Conscientiousness_level, names_to = "Waves", values_to = "Counts") %>%
  mutate(Waves = factor(Waves, levels = c("At_Least_One_Waves", "At_Least_Two_Waves", "At_Least_Three_Waves", "At_Least_Four_Waves", "At_Least_Five_Waves", "All_Six_Waves")))

# Plotting
ggplot(wave_data2_long, aes(x = Waves, y = Counts, group = Conscientiousness_level, color = Conscientiousness_level)) +
  geom_line() +
  labs(title = "Depression Conscientiousness_level Counts Across Waves Completed",
       x = "Waves",
       y = "Count") +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # You can change the palette if needed
  theme_minimal()

wave_data2_long <- wave_data2_long %>%
  group_by(Conscientiousness_level) %>%
  mutate(Percentage = Counts / Counts[Waves == "At_Least_One_Waves"] * 100)

# Plotting the line graph with percentages relative to Wave 1 on the y-axis
ggplot(wave_data2_long, aes(x = Waves, y = Percentage, color = Conscientiousness_level, group = Conscientiousness_level)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of waves completed per person",
       x = "Number of waves completed", y = "Percentage") +
  theme_minimal()


### Paranoia Levels

mdf4$W1_Paranoia_Cat <- NA
mdf4$W1_Paranoia_Cat[mdf4$W1_Paranoia_Total >= 5 & mdf4$W1_Paranoia_Total <= 11  ] <- "Low"
mdf4$W1_Paranoia_Cat[mdf4$W1_Paranoia_Total >= 12 & mdf4$W1_Paranoia_Total <= 18 ] <- "Medium"
mdf4$W1_Paranoia_Cat[mdf4$W1_Paranoia_Total >= 19 & mdf4$W1_Paranoia_Total <= 25] <- "High"
table(mdf4$Waves_attended, mdf4$W1_Paranoia_Cat)
table(mdf4$W1_Present,mdf4$W1_Paranoia_Cat)

wave_data3 <- data.frame(
  Paranoia_level = c("Low (5-11)", "Medium (12-18)", "High (19-25)"),
  At_Least_One_Waves = c(899, 883, 243),
  At_Least_Two_Waves = c(821, 756, 180),
  At_Least_Three_Waves = c(727, 618, 137),
  At_Least_Four_Waves = c(615, 482, 108),
  At_Least_Five_Waves = c(499, 392, 81),
  All_Six_Waves = c(316, 221, 45)
)

custom_colors <- c("Low (5-11)" = "green", "Medium (12-18)"= "orange","High (19-25)" = "red")
legend_order <- c("Low (5-11)", "Medium (12-18)","High (19-25)")


wave_data3_long <- wave_data3 %>%
  pivot_longer(cols = -Paranoia_level, names_to = "Waves", values_to = "Counts") %>%
  mutate(Waves = factor(Waves, levels = c("At_Least_One_Waves", "At_Least_Two_Waves", "At_Least_Three_Waves", "At_Least_Four_Waves", "At_Least_Five_Waves", "All_Six_Waves")))

# Plotting
ggplot(wave_data3_long, aes(x = Waves, y = Counts, group = Paranoia_level, color = Paranoia_level)) +
  geom_line() +
  labs(title = "Depression Conscientiousness_level Counts Across Waves Completed",
       x = "Waves",
       y = "Count") +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # You can change the palette if needed
  theme_minimal()

wave_data3_long <- wave_data3_long %>%
  group_by(Paranoia_level) %>%
  mutate(Percentage = Counts / Counts[Waves == "At_Least_One_Waves"] * 100)

# Plotting the line graph with percentages relative to Wave 1 on the y-axis
ggplot(wave_data3_long, aes(x = Waves, y = Percentage, color = Paranoia_level, group = Paranoia_level)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of waves completed per person",
       x = "Number of waves completed", y = "Percentage") +
  theme_minimal()

### Extraversion

mdf4$W1_Extraversion_Cat <- NA
mdf4$W1_Extraversion_Cat[mdf4$W1_Extraversion_Total >= 2 & mdf4$W1_Extraversion_Total <= 4  ] <- "Low"
mdf4$W1_Extraversion_Cat[mdf4$W1_Extraversion_Total >= 5 & mdf4$W1_Extraversion_Total <= 7 ] <- "Medium"
mdf4$W1_Extraversion_Cat[mdf4$W1_Extraversion_Total >= 8 & mdf4$W1_Extraversion_Total <= 10] <- "High"
table(mdf4$Waves_attended, mdf4$W1_Extraversion_Cat)
table(mdf4$W1_Present,mdf4$W1_Extraversion_Cat)

wave_data4 <- data.frame(
  Extraversion_level = c("Low (2-4)", "Medium (5-7)", "High (8-10)"),
  At_Least_One_Waves = c(492, 1155, 378),
  At_Least_Two_Waves = c(433, 1004, 320),
  At_Least_Three_Waves = c(371, 834, 277),
  At_Least_Four_Waves = c(314, 664, 227),
  At_Least_Five_Waves = c(251, 594, 172),
  All_Six_Waves = c(159, 319, 104)
)

custom_colors <- c("Low (2-4)" = "green", "Medium (5-7)"= "orange","High (8-10)" = "red")
legend_order <- c("Low (2-4)", "Medium (5-7)","High (8-10)")


wave_data4_long <- wave_data4 %>%
  pivot_longer(cols = -Extraversion_level, names_to = "Waves", values_to = "Counts") %>%
  mutate(Waves = factor(Waves, levels = c("At_Least_One_Waves", "At_Least_Two_Waves", "At_Least_Three_Waves", "At_Least_Four_Waves", "At_Least_Five_Waves", "All_Six_Waves")))

# Plotting
ggplot(wave_data4_long, aes(x = Waves, y = Counts, group = Extraversion_level, color = Extraversion_level)) +
  geom_line() +
  labs(title = "Depression Conscientiousness_level Counts Across Waves Completed",
       x = "Waves",
       y = "Count") +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # You can change the palette if needed
  theme_minimal()

wave_data4_long <- wave_data4_long %>%
  group_by(Extraversion_level) %>%
  mutate(Percentage = Counts / Counts[Waves == "At_Least_One_Waves"] * 100)

# Plotting the line graph with percentages relative to Wave 1 on the y-axis
ggplot(wave_data4_long, aes(x = Waves, y = Percentage, color = Extraversion_level, group = Extraversion_level)) +
  geom_line() +
  scale_color_manual(values = custom_colors, breaks = legend_order) +  # Assigning custom colors and order
  labs(title = "Percentage of waves completed per person",
       x = "Number of waves completed", y = "Percentage") +
  theme_minimal()

m10<-lm(mdf4$Waves_attended ~ mdf4$W1_Dep_Severity)
stargazer(m10, type="text",out="M10.txt", df=FALSE)
table(mdf4$W1_Dep_Severity)
summary(m10)

m11<-lm(mdf4$Waves_attended ~ mdf4$W1_Depression_Total)
summary(m11)

plot(y = mdf4$Waves_attended, 
     x = mdf4$W1_Depression_Total,
     xlab = "Depression Total"  ,
     main = "Scatterplot of Total Number of Waves Present and depression Total",
     ylab = "Total Number of Waves Present",
     xlim = c(0,27),
     cex.main = 0.7)
abline(m11,
       lwd = 1.8, 
       col = "steelblue")

m12<-lm(mdf4$Waves_attended ~ mdf4$W1_Conscientiousness_Total)
summary(m12)


plot(y = mdf4$Waves_attended, 
     x = mdf4$W1_Conscientiousness_Total,
     xlab = "Conscientiousness Total"  ,
     main = "Scatterplot of Total Number of Waves Present and Conscientiousness Total",
     ylab = "Total Number of Waves Present",
     xlim = c(2,10),
     cex.main = 0.7)
abline(m12,
       lwd = 1.8, 
       col = "steelblue")


library(xtable)
colnames(wave_data1) <- gsub("_", " ", colnames(wave_data1))

# Convert the dataframe into a LaTeX-formatted table
latex_table <- xtable(wave_data1, caption = "Occurrences of Waves by Severity Level", label = "tab:wave_data")

# Print the LaTeX table
print(latex_table, caption.placement = "top")

wave_table <- xtable(wave_data)

# Print the LaTeX code
print(wave_table, caption = "Summary of Severity Levels by Waves", label = "tab:severity_waves", caption.placement = "top")

