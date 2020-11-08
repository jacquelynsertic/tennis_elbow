# Importing tennis_elbow data from excel

library('readxl')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('dslabs')
library('ggthemes')

#load the data into R from excel
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GripStrength') #load excel sheet into R


# create a line graph, separated into the weeks, of the grip strength, pain, and UEIF scores to determine whether they are normally distributed

################## TESTS FOR OUTLIERS
################## BOXPLOTS 


# Boxplot of difference in GS

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GS_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

TE %>% 
  filter(!is.na(Score)) %>%
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>%
  ggplot(aes(x = GripStrength, y = Score, order = factor(GripStrength))) + #creates the ggplot
  geom_boxplot(aes(fill= GripStrength),
               width = .8, #adjusts spaces between boxplots
               color = "#800000", fill = "#800000",
               alpha = 0.5, #transparent boxplot
               outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  ggtitle("Grip Strength: Week 10 - Baseline scores") + #labels title
  theme(axis.title.x = element_blank()) +  #removes x-axis label
  scale_x_discrete(name = element_blank()) +
  theme_bw()


# Boxplot of difference in UEFI Scores

TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI_diff') #load excel sheet into R

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

TE %>% 
  filter(!is.na(Score)) %>%
  mutate(outlier = ifelse(is_outlier(Score), Score, as.numeric(NA))) %>%
  ggplot(aes(x = UEFI_Score, y = Score, order = factor(UEFI_Score))) + #creates the ggplot
  geom_boxplot(aes(fill= UEFI_Score),
               width = .8, #adjusts spaces between boxplots
               color = "#800000", fill = "#800000",
               alpha = 0.5, #transparent boxplot
               outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  ggtitle("UEFI Scores: Week 10 - Baseline scores") + #labels title
  theme(axis.title.x = element_blank()) +  #removes x-axis label
  scale_x_discrete(name = element_blank()) +
  theme_bw()


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
               color = "#800000", fill = "#800000",
               alpha = 0.5, #transparent boxplot
               outlier.colour = "black", outlier.fill = "black", outlier.shape = 22, outlier.size = 2) + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.35) + 
  ggtitle("Pain Scores: Week 10 - Baseline scores") + #labels title
  theme(axis.title.x = element_blank()) +  #removes x-axis label
  scale_x_discrete(name = element_blank()) +
  theme_bw()



#############################################################
# Non parametrics: Wilcoxon signed-rank test
# line plot for normality 

# Grip Strength

GS_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'GS_diff') #load excel sheet into R

GS_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
               color = "#800000",
               fill = "#800000",
               alpha = 0.5) + 
  scale_x_continuous("Grip Strength Scores") +
  ggtitle("Grip Strength differences") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"))
  
shapiro.test(GS_norm$Score) # Shapiro test for normality 


# UEFI score

UEFI_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'UEFI_diff') #load excel sheet into R
  
UEFI_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
                 color = "#800000",
                 fill = "#800000",
                 alpha = 0.5) + 
  scale_x_continuous("UEFI Scores") +
  ggtitle("UEFI Score differences") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
  
shapiro.test(UEFI_norm$Score) # Shapiro test for normality 
  
  
# Pain Scores

Pain_norm <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'Pain_diff') #load excel sheet into R
  
Pain_norm %>% ggplot() + 
  geom_density(aes(x = Score), 
               color = "#800000",
               fill = "#800000",
               alpha = 0.5) + 
  scale_x_continuous("Pain Scores") +
  ggtitle("Pain Score differences") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

shapiro.test(Pain_norm$Score) # Shapiro test for normality 


################# create histograms of differences 

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



############## correlations
TE <- read_excel('C:\\Users\\jacqu\\Documents\\minnesota\\stats\\Porth Phase 1 Results 2020-deidentified.xlsx', sheet = 'tennisElbow') #load excel sheet into R

# Baseline

TE %>% ggplot() +
  geom_point(aes(UEFI_Score0, GripStrength0)) +
  geom_abline()

cor(TE$GripStrength0, TE$UEFI_Score0)

# Week 10 
TE %>% ggplot() +
  geom_point(aes(Pain10, UEFI_Score10)) +
  geom_abline()
