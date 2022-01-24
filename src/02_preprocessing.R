#rm(list = ls())
source("./src/01_dataLoad.R", encoding = "UTF-8")


test$target = NA

train = cbind(division = "train" ,train)
test = cbind(division = "test" ,test)

data = rbind(train, test)


data$Exter.Qual %>% unique
data$Kitchen.Qual %>% unique
data$Bsmt.Qual %>% unique

data1 = data %>%
  mutate(Exter.Qual = recode(Exter.Qual , 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)) %>% 
  mutate(Kitchen.Qual = recode(Kitchen.Qual , 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)) %>% 
  mutate(Bsmt.Qual = recode(Bsmt.Qual , 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5))

data1 %>% str
colSums(is.na(data1)) # 결측치 미존재


write.csv(data1, "./data/prep/data1.csv", row.names = FALSE)
