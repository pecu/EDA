library(ggplot2)
library(dplyr)
rm(list=ls(all=TRUE))
load("population.RData")

getcity <- function(str)
{
  substr(str, start=1, stop=3)
}
city = sapply(population$site_id, getcity)
city = data.frame(city)
population = cbind(city, population)
population$site_id = as.factor(population$site_id)
population$village = as.factor(population$village)
population$age = as.factor(population$age)
population$sex = as.factor(population$sex)
View(population)
View(summary(population))

group_by(population, age) %>%
  summarise(count = sum(count)) %>%
  arrange(age) %>%
  ggplot(aes(x = age, y = count)) +
  geom_point() + geom_line()

# 各年齡層的人數？
ageAns <- group_by(population, age) %>%
          summarise(count = sum(count)) %>%
          arrange(age)
View(summary(ageAns))
ggplot(data = ageAns, 
       aes(x = age, y = count)) +
  geom_point() + geom_line()

# 各行政區的性別人數
villSexAns <- group_by(population, city, sex) %>%
              summarise(count = sum(count))
View(summary(villSexAns))
ggplot(data = villSexAns,
       aes(x = city, y = count, fill = sex)) +
geom_bar(position = "dodge", stat = "identity")

# 各行政區的性別&年齡人數
villSexAge <- group_by(population, city, sex, age) %>%
              summarise(count = sum(count))
View(summary(villSexAge))
ggplot(data = villSexAge,
       aes(x = age, y = count)) +
       geom_line(aes(color = sex), stat = "identity") +
       facet_wrap(~ city)

NewTaipei <- filter(population, age >= 25, 
                    city == "新北市") %>%
             group_by(site_id, age, sex) %>%
             summarise(all = sum(count))

