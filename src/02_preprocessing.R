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
  mutate(Exter.Qual = dplyr::recode(Exter.Qual , 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)) %>%
  mutate(Kitchen.Qual = dplyr::recode(Kitchen.Qual , 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)) %>%
  mutate(Bsmt.Qual = dplyr::recode(Bsmt.Qual , 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5))


colSums(is.na(data1)) # 결측치 미존재

# data1 = data1 %>% mutate(Garage.Area.Cars = Garage.Area / Garage.Cars)


# 이상치 검출
data1 %>% filter(Garage.Yr.Blt == max(Garage.Yr.Blt)) # 2007 년을 2207로 오기재 했다고 가정
data1$Garage.Yr.Blt[data1$Garage.Yr.Blt == 2207] = 2007
data1 %>% head


# 파생변수 추가
data1 = data1 %>% mutate(comb.Qual = Exter.Qual + Kitchen.Qual + Bsmt.Qual)

data1 = data1 %>% mutate(Garage.Yr.Blt_cal = 2011 - Garage.Yr.Blt,
                         Year.Remod.Add_cal = 2011 - Year.Remod.Add,
                         Year.Built_cal = 2011 - Year.Built)

data1 %>% head

data1 = data1 %>% mutate(comb.Built = Year.Built_cal + Year.Remod.Add_cal)
data1 = data1 %>% mutate(comb.Area = Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF)
data1 = data1 %>% mutate(total.Price.Index = Overall.Qual * comb.Qual * comb.Area)

#data1 = data1 %>% mutate(comb.plusAll = abs(comb.Qual) * (abs(comb.Area)) - (abs(comb.Qual) * abs(comb.Built)))


data1$Year.Remod.Add = NULL
data1$Garage.Yr.Blt = NULL
# data1$Year.Built = NULL

# data1$Year.Remod.Add_cal = NULL
# data1$Garage.Yr.Blt_cal = NULL
# data1$Year.Built_cal = NULL

# data1$Exter.Qual = NULL
# data1$Kitchen.Qual = NULL
# data1$Bsmt.Qual = NULL # 0.0944
# 
# data1$Overall.Qual = NULL
# data1$comb.Qual = NULL
# 
# data1$comb.Area = NULL




# 시각화
ggplot(data = data1[!is.na(data1$target),], aes(x = total.Price.Index, y = target)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1))





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

# 이상치 제거
library(car)

outlierTest_lm = lm(target ~ total.Price.Index, data=data1)
outlierTest(outlierTest_lm)
olt = outlierTest(outlierTest_lm) 
olt = as.numeric(row.names(as.data.frame(olt$rstudent)))

data1 = data1[!(as.numeric(rownames(data1)) %in% olt),]



ggplot(data = data1[!is.na(data1$target),], aes(x = total.Price.Index, y = target)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1))






data1 = data1 %>% relocate(target, .after = last_col())

# 상관 계수 확인 후 - 상관성 높은 변수 drop
cor(data1[!is.na(data1$target) ,3:ncol(data1)]) %>% tail
DataExplorer::plot_correlation(data1[!is.na(data1$target) ,3:ncol(data1)])


write.csv(data1, paste0("./data/prep/",sub_ver,".csv"), row.names = FALSE)
