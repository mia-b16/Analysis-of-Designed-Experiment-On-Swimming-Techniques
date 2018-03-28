#####Analysis of Designed Experiment
#####LOADING DATA
# Note : Change the path
swimming_data <- read.csv("C:/Users/pmutreja/Desktop/swimming.csv", header = T, sep = '\t') 
head(swimming_data)

#####Checking Design is balanced or not
table(swimming_data$Style,swimming_data$Food_intake) ##We have a Balanced design

#####VISUALIZATION

#install.packages("ggpubr")
library("ggpubr")

##Boxplot
ggboxplot(swimming_data, x = "Style", y = "Time", color = "Food_intake",
          palette = c("#00AFBB", "#E7B800"))

##Interaction plot
ggline(swimming_data, x = "Style", y = "Time", color = "Food_intake",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

####Computing Anova with interaction effect
res <- aov(Time~Style*Food_intake, data = swimming_data) 
summary(res)
##We can see that both main effects Style,Food_intake and their interaction are statistically significant at 0.05 level of significance.
##Also F value is quite large, hence its a good model.
##Final equation : Time = Style + Food_intake + Style*Food_intake


####Summary statistics
#install.packages('dplyr')
require("dplyr")
group_by(swimming_data, Style, Food_intake) %>%
  summarise(
    count = n(),
    mean = mean(Time, na.rm = TRUE),
    sd = sd(Time, na.rm = TRUE)
  )

model.tables(res, type="means", se = TRUE)

####Tukey's Test
tuk <- TukeyHSD(res,alpha =0.05,p.adj="none")
tuk
## p val > 0.05 same mean 
## All styles have different mean as p-value < 0.05
## Food intake methods also have differnt mean (as per Anova also)
## Following interactions have same mean : 1. Back stroke - food_intake(not much effect)
# 2. Butterfly stroke - food_intake(not much effect)

##ANOVA Assumptions
#1.variance among groups are homogeneous
plot(res,1)
##Levene test for checking homogeneity of variance
library(car)
leveneTest(Time~Style*Food_intake, data = swimming_data) 



#2.data are normally distributed
#res1 <- aov(Time~Style+Food_intake, data = swimming_data)
plot(res,2)
shapiro.test(res$residuals) 
##normally distributed


####Mean Effects Analyis
plot.design(Time~Style*Food_intake, data= swimming_data)
##Butterfly stroke is more time consuming and free style consumes the least time.
##Swimming after meal requires more time as compared to empty stomach


####Regression Analysis
summary(lm(Time~Style*Food_intake, data= swimming_data))

##R Sq = 0.9897 and Adj R Sq = 0.9812 with F-Stats 115.6 
##Hence interaction model is a really good model.
##Final regression equation : Time = 58+8.6(Butterfly Style)-6.55(Free Style)-6.10(Empty Stomach Free Style)

##Without interaction
reg <- lm(Time~Style+Food_intake, data= swimming_data)
summary(reg)
