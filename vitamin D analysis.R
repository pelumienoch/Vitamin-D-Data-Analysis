# This is a project to determine the levels of serum Vitamin D and assess its relationship with osteoporosis in premenopausal obese subjects

# first we import the dataset

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio)

# CSV

rio_csv <- import ("C:/Users/User/Documents/STATISTIX/HAFSAT/VitaminD_Data.csv")
head(rio_csv)

# Now we want to determine the serum vitamin D levels in premenopausal women with obesity compared with premenopausal women with normal body weight, using correlation and t-test
rio_csv <- vitaminD_dataset
rio_csv$Vit.DngmL
rio_csv$BMIKgm2

x<-rio_csv$Vit.DngmL
y<-rio_csv$BMIKgm2
mean(x)
median(x)
mean(y)
median(y)

# what is the correlation between vitamin D and BMI???
cor(x,y, method = "pearson")  
# the correlation coefficient is -0.56, this is nice!!!

plot(x,y)

# What is the difference in the vitamin D level across the groups of participants
# t-test, where 1= normal weight, 2= mildly obese, 3= moderately obese, 4= severely obese
data1 <- subset(rio_csv$Vit.DngmL, rio_csv$Group == "1")
data2 <- subset(rio_csv$Vit.DngmL, rio_csv$Group == "2")
data3 <- subset(rio_csv$Vit.DngmL, rio_csv$Group == "3")
data4 <- subset(rio_csv$Vit.DngmL, rio_csv$Group == "4")

summary(data1)
summary(data2)
summary(data3)
summary(data4)

# Between normal weight and mildly obese
t.test(data1, data2)

# Between normal weight and moderately obese
t.test(data1, data3)

# Between normal weight and severely obese
t.test(data1, data4)

# Across all the groups
# ANOVA to the rescue!!!
anova.model <- aov(rio_csv$Vit.DngmL~ rio_csv$Group, data= rio_csv)
summary(anova.model)
vitamin_D <- rio_csv$Vit.DngmL
BMI <- rio_csv$BMIKgm2
plot(vitamin_D, BMI)

