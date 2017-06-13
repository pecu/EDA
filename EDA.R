library(ggplot2)
library(dplyr)
raw = read.csv("Project(Part 1).csv", 
               header = TRUE, sep = ',')
# 統計敘述
result = summary(raw)

# 颱風修復工程代碼
ans <- filter(raw, raw$颱風 == "97年辛樂克及薔蜜") %>%
       group_by(機關名稱, 工程代碼) %>%
       summarise(price = sum(審議經費.千元.))
highPrice <- filter(ans, price > 500000)

ans2 <- filter(raw, raw$颱風 == "97年辛樂克及薔蜜") %>%
        group_by(機關名稱, 工程代碼)
  ggplot(ans2, aes(x = 工程代碼)) +
    geom_bar(position = "dodge",
             aes(fill = 機關名稱))

work <- filter(raw, raw$工程代碼 == "C1") %>%
        group_by(機關名稱, 鄉.鎮市.) %>%
        summarise(price = sum(審議經費.千元.))

ggplot(data = ans, aes(x = 工程代碼, 
                       y = price,
                       group = 機關名稱)) +
         geom_bar(position = "dodge", 
                  stat = "identity",
                  aes(fill = 機關名稱))

ggplot(data = ans, aes(x = 工程代碼, 
                       group = 機關名稱)) +
  geom_bar(position = "dodge", 
           aes(fill = 機關名稱))

# 機關名稱申請工程
ans <- filter(raw, raw$颱風 == "97年辛樂克及薔蜜") %>%
  group_by(機關名稱, 工程代碼) %>%
  summarise(price = sum(審議經費.千元.))
ggplot(data = ans, aes(x = 機關名稱, 
                       y = price,
                       fill = 工程代碼)) +
  geom_bar(stat = "identity")

# 各單位通過的審議經費？
out <- group_by(raw, 機關名稱, 颱風, 工程代碼) %>%
  summarise(price = sum(審議經費.千元.))
ggplot(data = out, aes(x = 機關名稱,
                       y = price)) +
    geom_boxplot()