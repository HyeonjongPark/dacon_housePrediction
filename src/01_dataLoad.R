
source("./src/00_libs.R", encoding = "UTF-8")

library(dplyr)
train = read.csv("./data/raw/housing/train.csv")
test = read.csv("./data/raw/housing/test.csv")
submission = read.csv("./data/raw/housing/sample_submission.csv")

# id : 데이터 고유 id
# OverallQual : 전반적 재료와 마감 품질
# YearBuilt : 완공 연도
# YearRemodAdd : 리모델링 연도
# ExterQual : 외관 재료 품질
# BsmtQual : 지하실 높이
# TotalBsmtSF : 지하실 면적 
# 1stFlrSF : 1층 면적 
# GrLivArea : 지상층 생활 면적
# FullBath : 지상층 화장실 개수 
# KitchenQual : 부억 품질 
# GarageYrBlt : 차고 완공 연도
# GarageCars: 차고 자리 개수
# GarageArea: 차고 면적 
# target : 집값(달러 단위)

train %>% head
test %>% head
submission %>% head

train %>% dim
test %>% dim


