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
# year 계산
max(data1$Year.Built)
max(data1$Year.Remod.Add)
max(data1$Garage.Yr.Blt)

data1 %>% filter(Garage.Yr.Blt == max(Garage.Yr.Blt)) # 2007 년을 2207로 오기재 했다고 가정

data1$Garage.Yr.Blt[data1$Garage.Yr.Blt == 2207] = 2007


data1 = data1 %>% mutate(Garage.Yr.Blt_cal = 2010 - Garage.Yr.Blt,
                         Year.Remod.Add_cal = 2010 - Year.Remod.Add,
                         Year.Built_cal = 2010 - Year.Built)


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

data1$Year.Built_cal %>% unique %>% sort
data1$Year.Remod.Add_cal %>% unique %>% sort
data1$Garage.Yr.Blt_cal %>% unique %>% sort



data1 = data1 %>% relocate(target, .after = last_col())

write.csv(data1, paste0("./data/prep/",sub_ver,".csv"), row.names = FALSE)
