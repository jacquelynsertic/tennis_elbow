# Importing tennis_elbow data from excel

library('readxl')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('dslabs')
library('ggthemes')
library("gridExtra")
library("cowplot")
library("showtext")
library("gridExtra")

## TABLE OF CONTENTS
## 1) PRELIMINARY DATA VISUALIZATION
#### 1a) SCATTERPLOTS
#### 1b) BAR GRAPHS
#### 1c) PRE TO POST BOXPLOTS
#### 1d) PRE TO POST LINE PLOTS
## 2) OUTILERS - BOXPLOTS
## 3) NORMALITY - DENSITY PLOTS
## 4) HISTOGRAMS
## 5) CORRELATIONS
#### 5a) PRELIMINARY SCATTERPLOTS
#### 5B) BASELINE AND WEEK 10 CORRELATIONS

#load the data into R from excel
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') #load excel sheet into R

##############################################
############################################## 
#### 1) Preliminary Data Visualization #######
##############################################
##############################################

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

####### 
####### 1A) SCATTERPLOTS

#Grip Strength

a <- TE %>% ggplot(aes(GripStrength0, GripStrength10)) + #ggplot with x and y 
  geom_point(size = 3) + #creates scatterplot
  labs(tag = "A")+
  scale_x_continuous(name = "Grip Strength at Baseline (lbs)", #names the continuous x-axis
       limit = c(1,160), #sets the limits of the x-axis
       breaks = c(0, 40, 80, 120, 160)) + #sets the tick marks for the axis
  scale_y_continuous(name = "Grip Strength at Week 10 (lbs)", #names the continuous y-axis
       limit = c(1,160), 
       breaks = c(0, 40, 80, 120, 160)) +
  geom_abline(slope=1, intercept = 0, color = "red", size = 1.2) + # adds line of equality
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), 
       axis.line = element_line(colour = "black"))+
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15)) +
  annotate(geom="text",x=9,y=157,label="     increase in\nstrength") +
  annotate(geom="text",x=145,y=5,label= "decrease in\n     strength")

#UEFI Score

(c<-TE %>% ggplot(aes(UEFI_Score0, UEFI_Score10)) + 
  geom_point(size = 3) +
  labs(tag = "A")+
  scale_x_continuous(name = "UEFI Scores at Baseline",
       limit = c(1,80)) + 
  scale_y_continuous(name = "UEFI Scores at Week 10",
       limit = c(1,80)) +
  geom_abline(slope=1, intercept = 0, color = "red", size = 1.2) + # adds line of equality
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15)) +
  annotate(geom="text",x=7,y=79,label="increase in\n  UEFI scores") +
  annotate(geom="text",x=72,y=3,label= "  decrease in\n  UEFI scores"))
  
#Pain Scores

e <- TE %>% ggplot(aes(Pain0, Pain10)) + 
  geom_point(size = 3) +
  labs(tag = "A")+
  scale_x_continuous(name = "Pain Scores at Baseline",
       limit = c(1,10)) + 
  scale_y_continuous(name = "Pain Scores at Week 10",
       limit = c(1,10)) +
  geom_abline(slope=1, intercept = 0, color = "red", size = 1.2) + # adds line of equality
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15)) +
  annotate(geom="text",x=1,y=10,label="         increase\n      in pain") +
  annotate(geom="text",x=9.5,y=1.2,label= "decrease\n    in pain")

#######
####### 1c) PRE TO POST BOXPLOTS

# Grip Strength 

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') 
#load excel sheet into R

(b <- TE %>% ggplot(aes(x = variable, y = Score)) + 
  geom_boxplot(alpha = 0.5, #creates boxplot with transparency 
       color = "black", #sets surrounding lines to black
       fill = c(Baseline = "#800000", Week10 = "#FFD700")) + #fill colors
  scale_y_continuous(name = "Grip Strength (lbs)",
       breaks = c(0, 40, 80, 120, 160)) +
  labs(x = " Baseline            Week 10", 
    tag = "B")+ 
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(), # removes the automatically generated title from the data
        axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15)))

# UEFI Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI') 
#load excel sheet into R

d <- TE %>% ggplot(aes(x = variable, y = Score)) + 
  geom_boxplot(alpha = 0.5, 
       color = "black", 
       fill = c(Baseline = "#800000", Week10 = "#FFD700")) + 
  scale_y_continuous(name = "UEFI Scores") +
  labs(x = " Baseline             Week 10", 
       tag = "B")+
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(), # removes the automatically generated title from the data
        axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15))

# Pain Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain') 
#load excel sheet into R

f <- TE %>% ggplot(aes(x = variable, y = Score)) + 
  geom_boxplot(alpha = 0.5, 
       color = "black", 
       fill = c(Baseline = "#800000", Week10 = "#FFD700")) + 
  scale_y_continuous(name = "Pain Scores") +
  labs(x = " Baseline             Week 10", 
       tag = "B")+
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(), # removes the automatically generated title from the data
        axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15))

### combining scatterplots and pre-post boxplots
plot_grid(a,b, ncol = 2, nrow = 1) #grip strength
plot_grid(c,d, ncol = 2, nrow = 1) #uefi
plot_grid(e,f, ncol = 2, nrow = 1) #pain

#######
####### 1d) PRE AND POST LINE PLOTS

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

# BOXPLOT OF DIFFERENCE IN GS

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GS_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
} #adds the outliers

g <- TE %>% 
  filter(!is.na(Score)) %>% #filters out NANs
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>% #sets outliers
  ggplot(aes(x = GripStrength, y = Score, order = factor(GripStrength))) + #creates the ggplot
  geom_boxplot(aes(fill= GripStrength),
       width = .8, #adjusts spaces between boxplots
       color = "black", fill = "#800000", #colors lines, fills with maroon
       alpha = 0.5, #transparent boxplot
       outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  scale_y_continuous(name = "Change in Grip Strength (lbs)") +
  scale_x_discrete(name = "test")+
  labs(#x = " test   ", 
       tag = "B")+
  theme_bw() +
  theme(panel.border = element_blank(), 
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), 
       axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(), # removes the automatically generated title from the data
        axis.title.y = element_text(vjust = 2, size = 15), 
                axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "white", vjust = 2, size = 30))
  
# BOXPLOT OF DIFFERENCE IN UEFI Scores

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

# BOXPLOT OF DIFFERENCE IN Pain Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

i <- TE %>% 
  filter(!is.na(Score)) %>%
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>%
  ggplot(aes(x = Pain_Score, y = Score, order = factor(Pain_Score))) + #creates the ggplot
  geom_boxplot(aes(fill= Pain_Score),
      width = .8, #adjusts spaces between boxplots
      color = "black", fill = "#800000",
      alpha = 0.5, #transparent boxplot
      outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  scale_y_continuous(name = "Change in Pain Scores",
      limit = c(-10,10),
      breaks = c(-10:10)) +
  labs(x = "", 
       tag = "B")+
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(), # removes the automatically generated title from the data
        axis.title.y = element_text(vjust = 0, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "white", vjust = 2, size = 30))
  

############################################### 
###############################################
######## 3) DENSITY - LINE PLOTS ##############
###############################################
###############################################

# NON-PARAMETRICS: WILCOXON SIGNED-RANK TEST

#######
####### LINE PLOT FOR NORMALITY

# Grip Strength

GS_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GS_diff') #load excel sheet into R

h <- GS_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
      color = "#800000",
      fill = "#800000",
      alpha = 0.5) + 
  labs(y = "Density",
       tag = "A") +
  scale_x_continuous(name = "Change in Grip Strength (lbs)",
      limit = c(0,100)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15),
        axis.title.x = element_text(vjust = 0, size = 15),
        axis.text = element_text(color = "black", size = 12))
  
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
  
j <- Pain_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
       color = "#800000",
       fill = "#800000",
       alpha = 0.5) + 
  labs(y = "Density",
       tag = "A") +
  scale_x_continuous("Change in Pain Scores", 
       limit = c(-12,5),
       breaks = c(-12:5)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15),
        axis.title.x = element_text(vjust = 0, size = 15),
        axis.text = element_text(color = "black", size = 12))

shapiro.test(Pain_norm$Score) # Shapiro test for normality 

#### combine density and difference box plots

plot_grid(h,g, ncol = 2, nrow = 1) #grip strength boxplot and density plots
plot_grid(j,i, ncol = 2, nrow = 1) #pain boxplot and density plots

###############################################
###############################################
############### 4) HISTOGRAMS #################
###############################################
###############################################

# GRIP STRENGTH

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

# UEFI SCORE

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

# PAIN SCORE

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

####### 
####### 5a) PRELIMINARY SCATTERPLOTS

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

(m <- TE %>% ggplot(aes(UEFI_Score0, GripStrength0)) +
  geom_point(size = 3,
             fill = "#800000",
             color = "#800000") + #changes the size of the dots
  geom_smooth(method=lm, se = FALSE, color = "black") +
    scale_y_continuous(name = "Baseline Grip Strength Scores (lbs)", #names x-axis
      limit = c(1,160),
      breaks = c(0, 40, 80, 120, 160)) + 
  scale_x_continuous(name = "Baseline UEFI Scores",
      limit = c(1,80),
      breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"))+
  labs(tag = "A")+
  annotate(geom="text",x=3,y=53,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE) +
  theme(axis.title.y = element_text(vjust = 2, size = 14), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 14)))


(n <- TE %>% ggplot(aes(GripStrength0, Pain0)) +
  geom_point(size = 3,
               fill = "#800000",
               color = "#800000") + #changes the size of the dots
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
  labs(tag = "B") +
  theme(axis.title.y = element_text(vjust = 2, size = 14), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 14)))

o <- TE %>% ggplot(aes(Pain0, UEFI_Score0)) +
  geom_point(size = 3,
             fill = "#800000",
             color = "#800000") + #changes the size of the dots
  scale_y_continuous(name = "Baseline UEFI Scores",
      limit = c(1,80),
      breaks = c(0, 20, 40, 60, 80)) + 
  scale_x_continuous(name = "Baseline Pain Scores",
      limit = c(1,10),
      breaks = c(0:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black")) + 
  labs(tag ="C") +
  theme(axis.title.y = element_text(vjust = 2, size = 14), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 14))

p <- TE %>% ggplot(aes(UEFI_Score10, GripStrength10)) +
  geom_point(size = 3,
             fill = "#FFD700",
             color = "#FFD700") + #changes the size of the dots
  scale_y_continuous(name = "Week 10 Grip Strength Scores (lbs)",
      limit = c(1,160),
      breaks = c(0, 40, 80, 120, 160)) + 
  scale_x_continuous(name = "Week 10 UEFI Scores",
      limit = c(1,80),
      breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black")) + 
  labs(tag = "D") +
  theme(axis.title.y = element_text(vjust = 2, size = 14), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 14))

q <- TE %>% ggplot(aes(GripStrength10, Pain10)) +
  geom_point(size = 3,
             fill = "#FFD700",
             color = "#FFD700") + #changes the size of the dots
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
  labs(tag = "E")+
  theme(axis.title.y = element_text(vjust = 2, size = 14), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 14))
  
r <- TE %>% ggplot(aes(Pain10, UEFI_Score10)) +
  geom_point(size = 3,
             fill = "#FFD700",
             color = "#FFD700") + #changes the size of the dots
  geom_smooth(method=lm, se = FALSE, color = "black") +
  scale_y_continuous(name = "Week 10 UEFI Scores",
      limit = c(1,80),
      breaks = c(0, 20, 40, 60, 80)) + 
  scale_x_continuous(name = "Week 10 Pain Scores",
      limit = c(1,10),
      breaks = c(0:10)) +
  theme_bw() +
  theme(panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black")) + 
  labs(tag = "F")+
  annotate(geom="text",x=6.5,y=73,label= rsq_label, hjust = 0, vjust = 1, parse = TRUE) +
  theme(axis.title.y = element_text(vjust = 2, size = 14), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 14))

grid.arrange(m, n, o, ncol = 3, widths =c(3.5, 3.5, 3.5)) #baseline
grid.arrange(p, q, r, ncol = 3, widths =c(3.5, 3.5, 3.5)) #Week 10 plots

###### 5b) SIGNIFICANT CORRELATIONS

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

###############################################
###############################################
############## 6) SCATTERPLOTS ################
###############################################
###############################################

####### 
####### SCATTERPLOTS

#Grip Strength

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') #load excel sheet into R

(t <- TE %>% ggplot(aes(Score, Subject, color = variable)) + #ggplot with x and y 
  geom_point(size = 3) + #creates scatterplot
  labs(x = "",
       tag = "A")+
  scale_x_continuous(name = "Grip Strength Scores (lbs)", #names the continuous x-axis
                     limit = c(0, 160), #sets the limits of the x-axis
                     breaks = c(0, 40, 80, 120, 160)) + #sets the tick marks for the axis
  labs(y = "")+
  theme_bw() + #sets the theme to plane
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(axis.title.y = element_text(vjust = 2, size = 15), 
        axis.text = element_text(color = "black", size = 12), 
        axis.title.x = element_text(color = "black", vjust = 0, size = 15),
        legend.title = element_blank()) +
   scale_color_manual(values = c(Baseline = "#800000", Week10 = "#FFD700"), 
        labels = c("Baseline", "Week 10")) +
   theme(legend.text = element_text(size = 15),
         legend.position = "none"))

#UEFI
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI') #load excel sheet into R

(u <- TE %>% ggplot(aes(Score, Subject, color = variable)) + #ggplot with x and y 
    geom_point(size = 3) + #creates scatterplot
    labs(x = "",
         tag = "B")+
    scale_x_continuous(name = "UEFI Scores", #names the continuous x-axis
                       limit = c(0, 80), #sets the limits of the x-axis
                       breaks = c(0, 20, 40, 60, 80)) + #sets the tick marks for the axis
    labs(y = "")+
    theme_bw() + #sets the theme to plane
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    theme(axis.title.y = element_text(vjust = 2, size = 15), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 15),
          legend.title = element_blank()) +
    scale_color_manual(values = c(Baseline = "#800000", Week10 = "#FFD700"), 
                       labels = c("Baseline", "Week 10")) +
    theme(legend.text = element_text(size = 15),
          legend.position = "none"))

#Pain
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain') #load excel sheet into R

(v <- TE %>% ggplot(aes(Score, Subject, color = variable)) + #ggplot with x and y 
    geom_point(size = 3) + #creates scatterplot
    labs(x = "",
         tag = "C")+
    scale_x_continuous(name = "Pain Scores", #names the continuous x-axis
          limit = c(0, 10), #sets the limits of the x-axis
          breaks = c(0:10)) + #sets the tick marks for the axis
    labs(y = "")+
    theme_bw() + #sets the theme to plane
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))+
    theme(axis.title.y = element_text(vjust = 2, size = 15), 
          axis.text = element_text(color = "black", size = 12), 
          axis.title.x = element_text(color = "black", vjust = 0, size = 15),
          legend.title = element_blank()) +
    scale_color_manual(values = c(Baseline = "#800000", Week10 = "#FFD700"), 
                       labels = c("Baseline", "Week 10")) +
    theme(legend.text = element_text(size = 15),
          legend.position = "top"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(v) #creates the variable legend with the plot legends

v <- v + theme(legend.position = "none") #sets the plot back to having no legend

grid.arrange(t, u, v, legend, ncol = 3, nrow = 2, #creates the layout of figures
             layout_matrix = rbind(c(1,2,3), c(NA,4,NA)), #creates the matrix. Each number corresponds to the number in the plot
             widths = c(3,3,3), heights = c(3, 0.7))