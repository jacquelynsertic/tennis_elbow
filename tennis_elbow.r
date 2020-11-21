# Importing tennis_elbow data from excel

library('readxl')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('dslabs')
library('ggthemes')


# TABLE OF CONTENTS
# 1) PRELIMINARY DATA VISUALIZATION
## 1a) SCATTERPLOTS
## 1b) BAR GRAPHS
## 1c) PRE TO POST BOXPLOTS
## 1d) PRE TO POST LINE PLOTS
# 2) OUTILERS - BOXPLOTS
# 3) NORMALITY - DENSITY PLOTS
# 4) HISTOGRAMS
# 5) CORRELATIONS
## 5a) PRELIMINARY SCATTERPLOTS
## 5B) BASELINE AND WEEK 10 CORRELATIONS


#load the data into R from excel
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') #load excel sheet into R


##############################################
############################################## 
#### 1) Preliminary Data Visualization #######
##############################################
##############################################


TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

#### 1A) Scatterplots

#Grip Strength

TE %>% ggplot(aes(GripStrength0, GripStrength10)) + 
  geom_point() + #creates scatterplot
  scale_x_continuous(name = "Grip Strength Baseline Scores (lbs)", #names the continuous x-axis
                     limit = c(1,160), #sets the limits of the x-axis
                     breaks = c(0, 40, 80, 120, 160)) + #sets the tick marks for the axis
  scale_y_continuous(name = "Grip Strength Week 10 Scores (lbs)", #names the continuous y-axis
                     limit = c(1,160), 
                     breaks = c(0, 40, 80, 120, 160)) +
  geom_abline(slope=1, intercept = 0) + # adds line of equality
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=158,y=1,label="A") #Adds the text to label the graph

#UEFI Score

TE %>% ggplot(aes(UEFI_Score0, UEFI_Score10)) + 
  geom_point() +
  scale_x_continuous(name = "Baseline UEFI Scores",
                     limit = c(1,80)) + 
  scale_y_continuous(name = "Week 10 UEFI Scores",
                     limit = c(1,80)) +
  geom_abline(slope=1, intercept = 0) + # adds line of equality
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=80,y=1,label="A")
  
#Pain Scores

TE %>% ggplot(aes(Pain0, Pain10)) + 
  geom_point() +
  scale_x_continuous(name = "Baseline Pain Scores",
                     limit = c(1,10)) + 
  scale_y_continuous(name = "Week 10 Pain Scores",
                     limit = c(1,10)) +
  geom_abline(slope=1, intercept = 0) + # adds line of equality
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=10,y=1,label="A")



#### 1b) Bar Graphs

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') #load excel sheet into R

#Grip Strength

TE %>% ggplot() + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") + 
  scale_y_continuous(name = "Week 10 Scores (lbs)",
                     limit = c(1,160)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))



######### 1c) pre to post boxplots 

# Grip Strength 

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') 
#load excel sheet into R

TE %>% ggplot(aes(x = variable, y = Score)) + 
  geom_boxplot(alpha = 0.5, #creates boxplot with transparency 
               color = "black", #sets surrounding lines to black
               fill = c(Baseline = "#800000", Week10 = "#FFD700")) + #fill colors
  scale_x_discrete(name = "Grip Strength") +
  scale_y_continuous(name = "Score (lbs)",
                     breaks = c(0, 40, 80, 120, 160)) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank()) + #removes legend titles
  annotate(geom="text",x=2.5,y=10,label="B")


# UEFI Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI') 
#load excel sheet into R

TE %>% ggplot(aes(x = variable, y = Score)) + 
  geom_boxplot(alpha = 0.5, 
               color = "black", 
               fill = c(Baseline = "#800000", Week10 = "#FFD700")) + #) +
  scale_x_discrete(name = "UEFI Scores") +
  scale_y_continuous(name = "Score") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank()) + #removes legend titles
  annotate(geom="text",x=2.5,y=10,label="B")

# Pain Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain') 
#load excel sheet into R

TE %>% ggplot(aes(x = variable, y = Score)) + 
  geom_boxplot(alpha = 0.5, 
               color = "black", 
               fill = c(Baseline = "#800000", Week10 = "#FFD700")) + 
  scale_x_discrete(name = "Pain Scores") +
  scale_y_continuous(name = "Score") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank()) + #removes legend titles
  annotate(geom="text",x=2.5,y=1,label="B")



####### 1d) line plots of PRE AND POST

# Grip Strength 

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') 
#load excel sheet into R

TE %>% ggplot(aes(x = Score, y = ..count.., fill = variable)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(Baseline = "#800000", Week10 = "#FFD700")) + 
  scale_x_continuous(name = "Grip Strength Scores (lbs)") +
  scale_y_continuous(name = "Frequency") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank())  #removes legend titles

# UEFI Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI') 
#load excel sheet into R

TE %>% ggplot(aes(x = Score, y = ..count.., fill = variable)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(Baseline = "#800000", Week10 = "#FFD700")) + 
  scale_x_continuous(name = "UEFI Scores") +
  scale_y_continuous(name = "Frequency") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank())  #removes legend titles

# Pain Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain') 
#load excel sheet into R

TE %>% ggplot(aes(x = Score, y = ..count.., fill = variable)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(Baseline = "#800000", Week10 = "#FFD700")) + 
  scale_x_discrete(name = "Pain Scores",
                   breaks = c(0:10),
                   limit = c(0:10)) +
  scale_y_continuous(name = "Frequency") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.title=element_blank())  #removes legend titles


###############################################
############################################### 
##### 2) TESTS FOR OUTLIERS - BOXPLOTS ########
############################################### 
###############################################

# Boxplot of difference in GS

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GS_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
} #adds the outliers

TE %>% 
  filter(!is.na(Score)) %>% #filters out NANs
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>% #sets outliers
  ggplot(aes(x = GripStrength, y = Score, order = factor(GripStrength))) + #creates the ggplot
  geom_boxplot(aes(fill= GripStrength),
               width = .8, #adjusts spaces between boxplots
               color = "black", fill = "#800000", #colors lines, fills with maroon
               alpha = 0.5, #transparent boxplot
               outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  scale_y_continuous(name = "Difference in Grip Strength Scores (lbs): Week 10 - Baseline ") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_x_discrete(name = "Grip Strength")

# Boxplot of difference in UEFI Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 2 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

TE %>% 
  filter(!is.na(Score)) %>%
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>%
  ggplot(aes(x = UEFI_Score, y = Score, order = factor(UEFI_Score))) + #creates the ggplot
  geom_boxplot(aes(fill= UEFI_Score),
               width = .8, #adjusts spaces between boxplots
               color = "black", fill = "#800000",
               alpha = 0.5, #transparent boxplot
               outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  theme(axis.title.x = element_blank()) +  #removes x-axis label
  scale_y_continuous(name = "Difference in UEFI Scores (Week 10 - Baseline)",
                     limit = c(0,80),
                     breaks = c(0,20,40,60,80)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_x_discrete(name = "UEFI Scores")

# Boxplot of difference in Pain Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

TE %>% 
  filter(!is.na(Score)) %>%
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>%
  ggplot(aes(x = Pain_Score, y = Score, order = factor(Pain_Score))) + #creates the ggplot
  geom_boxplot(aes(fill= Pain_Score),
               width = .8, #adjusts spaces between boxplots
               color = "black", fill = "#800000",
               alpha = 0.5, #transparent boxplot
               outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  scale_y_continuous(name = "Difference in Pain Scores (Week 10 - Baseline)",
                     limit = c(-10,10),
                     breaks = c(-10:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_x_discrete(name = "Pain Scores")


############################################### 
###############################################
######## 3) DENSITY - LINE PLOTS ##############
###############################################
###############################################

# Non parametrics: Wilcoxon signed-rank test
# line plot for normality 

# Grip Strength

GS_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GS_diff') #load excel sheet into R

GS_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
      color = "#800000",
      fill = "#800000",
      alpha = 0.5) + 
  scale_x_continuous(name = "Grip Strength Scores (lbs)",
      limit = c(0,100)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"))
  
shapiro.test(GS_norm$Score) # Shapiro test for normality 


# UEFI score - paired samples t-test

UEFI_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI_diff') #load excel sheet into R
  
UEFI_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
      color = "#800000",
      fill = "#800000",
      alpha = 0.5) + 
  scale_x_continuous("UEFI Scores", 
      limit = c(-35,85),
      breaks = c(-35, -20, -5, 10, 25, 40, 55, 70, 85)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      anel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"))
  
shapiro.test(UEFI_norm$Score) # Shapiro test for normality 
  
  
# Pain Scores

Pain_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain_diff') #load excel sheet into R
  
Pain_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
               color = "#800000",
               fill = "#800000",
               alpha = 0.5) + 
  scale_x_continuous("Pain Scores", 
                     limit = c(-12,5),
                     breaks = c(-12:5)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

shapiro.test(Pain_norm$Score) # Shapiro test for normality 



###############################################
###############################################
############### 4) HISTOGRAMS #################
###############################################
###############################################

# Grip Strength histogram 

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

TE %>% ggplot(aes(x = gshist, y = ..count.., fill = POS)) + 
  geom_histogram(binwidth = 9, 
                 color = "white") +
  scale_y_continuous(name="Frequency", #names the y-axis
                     breaks = c(0:10), #adds tick marks for each value
                     limit = c(0,10)) + #set the min and max values for y-axis
  scale_x_continuous(name="Grip Strength Scores: Week 10 - baseline ") +
  theme_bw() + 
  theme(legend.title=element_blank()) +  #removes legend titles
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c(Negative = "#FFD700", Positive = "#800000"))

# UEFI Score histogram 

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

TE %>% ggplot(aes(x = gshist, y = ..count.., fill = POSUEFI)) + 
  geom_histogram(binwidth = 12, 
                 color = "white",
                 position="dodge") +
  scale_y_continuous(name="Frequency", #names the y-axis
                     breaks = c(0:10), #adds tick marks for each value
                     limit = c(0,7)) + #set the min and max values for y-axis
  scale_x_continuous(name="UEFI Scores: Week 10 - baseline ") +
  theme_bw() + 
  theme(legend.title=element_blank()) +  #removes legend titles
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c(Negative = "#FFD700", Positive = "#800000"))

# Pain Score histogram 

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

TE %>% ggplot(aes(x = gshist, y = ..count.., fill = POSPAIN)) + 
  geom_histogram(binwidth = 12, 
                 color = "white",
                 position="dodge") +
  scale_y_continuous(name="Frequency", #names the y-axis
                     breaks = c(0:10), #adds tick marks for each value
                     limit = c(0,8)) + #set the min and max values for y-axis
  scale_x_continuous(name="Pain Scores: Week 10 - baseline ") +
  theme_bw() + 
  theme(legend.title=element_blank()) +  #removes legend titles
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c(Negative = "#FFD700", Positive = "#800000"))



###############################################
###############################################
############## 5) CORRELATIONS ################
###############################################
###############################################

###### 5a) preliminary scatterplots
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R
library("gridExtra")
library("cowplot")

a <- TE %>% ggplot(aes(GripStrength0, UEFI_Score0)) +
  geom_point(size = 3) + #changes the size of the dots
  scale_x_continuous(name = "Baseline Grip Strength Scores (lbs)", #names x-axis
                     limit = c(1,160),
                     breaks = c(0, 40, 80, 120, 160)) + 
  scale_y_continuous(name = "Baseline UEFI Scores",
                     limit = c(1,80),
                     breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+ 
  annotate(geom="text",x=1,y=75,label="A")
  
b <- TE %>% ggplot(aes(GripStrength0, Pain0)) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Baseline Grip Strength Scores (lbs)",
                     limit = c(1,160),
                     breaks = c(0, 40, 80, 120, 160)) + 
  scale_y_continuous(name = "Baseline Pain Scores",
                     limit = c(1,10),
                     breaks = c(0:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+ 
  annotate(geom="text",x=1,y=10,label="B")

c <- TE %>% ggplot(aes(UEFI_Score0, Pain0)) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Baseline UEFI Scores",
                     limit = c(1,80),
                     breaks = c(0, 20, 40, 60, 80)) + 
  scale_y_continuous(name = "Baseline Pain Scores",
                     limit = c(1,10),
                     breaks = c(0:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=1,y=10,label="C")

d <- TE %>% ggplot(aes(GripStrength10, UEFI_Score10)) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Week 10 Grip Strength Scores (lbs)",
                     limit = c(1,160),
                     breaks = c(0, 40, 80, 120, 160)) + 
  scale_y_continuous(name = "Week 10 UEFI Scores",
                     limit = c(1,80),
                     breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=1,y=80,label="D")

e <- TE %>% ggplot(aes(GripStrength10, Pain10)) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Week 10 Grip Strength Scores (lbs)",
                     limit = c(1,160),
                     breaks = c(0, 40, 80, 120, 160)) + 
  scale_y_continuous(name = "Week 10 Pain Scores",
                     limit = c(1,10),
                     breaks = c(0:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=1,y=10,label="E")
  
f <- TE %>% ggplot(aes(UEFI_Score10, Pain10)) +
  geom_point(size = 3) +
  scale_x_continuous(name = "Week 10 UEFI Scores",
                     limit = c(1,80),
                     breaks = c(0, 20, 40, 60, 80)) + 
  scale_y_continuous(name = "Week 10 Pain Scores",
                     limit = c(1,10),
                     breaks = c(0:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=1,y=10,label="F")

plot_grid(a, b, c, d, e, f, ncol = 3, nrow = 2) #plots all graphs in the same figure



###### 5b) significant correlations

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

# Baseline

fit <- 0.293
rsq_label <- paste('R^2 == ', fit)

TE %>% ggplot(aes(UEFI_Score0, GripStrength0)) +
  geom_point(shape = 21) +
  geom_smooth(method=lm, se = FALSE, color = "black") +
  scale_x_continuous(name = "Baseline UEFI Scores") +
  scale_y_continuous(name = "Baseline Grip Strength Scores (lbs)") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=70,y=80,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE)


# Week 10 

fit <- 0.471
rsq_label <- paste('R^2 == ', fit)

TE %>% ggplot(aes(Pain10, UEFI_Score10)) +
  geom_point(shape = 21) +
  geom_smooth(method=lm, se = FALSE, color = "black") +
  scale_x_discrete(name = "Week 10 Pain Scores",
                   breaks = c(0:10),
                   limit = c(0:10)) +
  scale_y_continuous(name = "Week 10 UEFI Scores") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  annotate(geom="text",x=9,y=58,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE)
