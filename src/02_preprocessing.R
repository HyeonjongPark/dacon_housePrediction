#rm(list = ls())
source("./src/01_dataLoad.R", encoding = "UTF-8")

sub_ver = "sub2"

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

data1 %>% head

# data1 = data1 %>% mutate(Garage.Area.Cars = Garage.Area / Garage.Cars)
# 
# # year 계산
# data1 = data1 %>% mutate(Garage.Yr.Blt_cal = Garage.Yr.Blt - Year.Built,
#                          Year.Remod.Add_cal = Year.Remod.Add - Year.Built,
#                          Year.Built_cal = 2022 - Year.Built)
# 
# 
# data1$Year.Built_cal %>% unique %>% sort
# data1$Year.Remod.Add_cal %>% unique %>% sort
# data1$Garage.Yr.Blt_cal %>% unique %>% sort

# data1 = data1 %>% mutate(Year.Remod.Add_cal = ifelse(Year.Remod.Add_cal <= 0, Year.Built_cal, Year.Remod.Add_cal))
# data1 = data1 %>% mutate(Garage.Yr.Blt_cal = ifelse(Garage.Yr.Blt_cal <= 0, Year.Built_cal, Garage.Yr.Blt_cal))
# 
# data1 = data1 %>% mutate(Year.Built_cal = cut(Year.Built_cal, 5),
#                          Year.Remod.Add_cal = cut(Year.Remod.Add_cal, 5),
#                          Garage.Yr.Blt_cal = cut(Garage.Yr.Blt_cal, 5))
# 
# 
# 
# data1 = data1 %>%
#   mutate(Year.Built_cal = Convert_Numeric(Year.Built_cal),
#          Year.Remod.Add_cal = Convert_Numeric(Year.Remod.Add_cal),
#          Garage.Yr.Blt_cal = Convert_Numeric(Garage.Yr.Blt_cal))
# 
# 
# 
# 정규화
# data1 %>% head
# data1 = cbind(data1$division,
#               data1$id,
#               as.data.frame(scale(data1[,3:15])),
#               data1$target)
# 
# colnames(data1)[1] = "division"
# colnames(data1)[2] = "id"
# colnames(data1)[16] = "target"


data1 = data1 %>% relocate(target, .after = last_col())

write.csv(data1, paste0("./data/prep/",sub_ver,".csv"), row.names = FALSE)
