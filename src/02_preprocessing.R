#rm(list = ls())
source("./src/01_dataLoad.R", encoding = "UTF-8")

sub_ver = "sub6"

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


colSums(is.na(data1)) # 결측치 미존재

data1 = data1 %>% mutate(Garage.Area.Cars = Garage.Area / Garage.Cars)

data1 %>% filter(Garage.Yr.Blt == max(Garage.Yr.Blt)) # 2007 년을 2207로 오기재 했다고 가정
data1$Garage.Yr.Blt[data1$Garage.Yr.Blt == 2207] = 2007

data1 %>% head

# 파생변수 추가
data1 = data1 %>% mutate(comb.Qual = Overall.Qual + Exter.Qual + Kitchen.Qual + Bsmt.Qual)

data1 = data1 %>% mutate(Garage.Yr.Blt_cal = 2011 - Garage.Yr.Blt,
                         Year.Remod.Add_cal = 2011 - Year.Remod.Add,
                         Year.Built_cal = 2011 - Year.Built)

data1 = data1 %>% mutate(comb.Built = Year.Built_cal + Year.Remod.Add_cal + Garage.Yr.Blt_cal)
# data1 = data1 %>% mutate(comb.Area = Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF)
# data1 = data1 %>% mutate(comb.roomCount = Garage.Cars + Full.Bath)
# data1 = data1 %>% mutate(comb.plusAll = abs(comb.Qual) * (abs(comb.Area) + abs(comb.roomCount)) - (abs(comb.Qual) * abs(comb.Built)))


# data1$Year.Remod.Add = NULL
# data1$Garage.Yr.Blt = NULL
# data1$Year.Built = NULL
# data1$Year.Remod.Add_cal = NULL
# data1$Garage.Yr.Blt_cal = NULL
# data1$Year.Built_cal = NULL



# 정규화 - log
data1 %>% head
log(data1[,3:ncol(data1)]+1)
data1 = data1 %>% relocate(target, .after = last_col())
data1 = cbind(data1$division,
              data1$id,
              as.data.frame(log(data1[,3:(ncol(data1))]+1)))



colnames(data1)[1] = "division"
colnames(data1)[2] = "id"
colnames(data1)[ncol(data1)] = "target"



data1 = data1 %>% relocate(target, .after = last_col())

cor(data1[!is.na(data$target) ,3:ncol(data1)]) %>% tail


write.csv(data1, paste0("./data/prep/",sub_ver,".csv"), row.names = FALSE)
