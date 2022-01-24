
#############

source("./src/02_preprocessing.R", encoding = "UTF-8")

data2 = read.csv("./data/prep/data1.csv")

data2$id = as.character(data2$id)
data2 %>% str

train2 = data2 %>% filter(division == "train")
test2 = data2 %>% filter(division == "test")

train2$division = NULL
test2$division = NULL

############
