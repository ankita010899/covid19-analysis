# covid19 data analysis
rm(list = ls()) # to remove all previously stored variables

# loading Hmisc library
library(Hmisc)

data <- read.csv("C:\\Users\\HP\\Documents\\R data analysis\\Covid 19\\COVID19_line_list_data.csv")
describe(data)

# fixing death column as it has NA values
data$actual_death <- as.integer(data$death!=0)
unique(data$actual_death)

# death rate 
(sum(data$actual_death) / nrow(data)) * 100

# Age factor wrt death
# claim 1 : Older ppl have higer death rate than younger ones
dead <- subset(data, actual_death == 1)
alive <- subset(data, actual_death == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
t.test(dead$age, alive$age, alternative = "two.sided", conf.level = 0.99)
# p-value ~ 0 hence claim 1 is true

# Gender wrt death
# claim 2 : Males have higer death rate than females
men <- subset(data, gender == "male")
women <- subset(data, gender == "female")
mean(men$actual_death, na.rm = TRUE)
mean(women$actual_death, na.rm = TRUE)
t.test(men$actual_death, women$actual_death, alternative = "two.sided", conf.level = 0.99)
# p-value ~ 0 hence claim 2 is also true

# visualize claim 1 and claim 2 data
ggplot(dead, aes(x=gender, y=age))+
  geom_boxplot(fill="red")+
  xlab("Gender")+
  ylab("Age")+
  ggtitle("Age-Gender Plot")+
  facet_grid(~actual_death)
