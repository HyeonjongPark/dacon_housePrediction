


import pandas as pd
import numpy as np

import warnings
warnings.filterwarnings('ignore')

import os
print(os.getcwd())

train = pd.read_csv('../data/prep/train3.csv')
test = pd.read_csv('../data/prep/test3.csv')
submission = pd.read_csv('../data/raw/housing/sample_submission.csv')

train = train.iloc[:, 1:]
test = test.iloc[:, 1:]

X = train.drop('target', axis = 1)
#y = np.log1p(train.target)
y = train.target
target = test[X.columns]

from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from catboost import CatBoostRegressor, Pool
from ngboost import NGBRegressor
from sklearn.metrics import make_scorer
from sklearn.model_selection import KFold

def NMAE(true, pred) -> float:
    mae = np.mean(np.abs(true - pred))
    score = mae / np.mean(np.abs(true))
    return score
    
nmae_score = make_scorer(NMAE, greater_is_better=False)

kf = KFold(n_splits = 10, random_state = 42, shuffle = True)

# ngboost
ngb_pred = np.zeros(target.shape[0])
ngb_val = []
for n, (tr_idx, val_idx) in enumerate(kf.split(X, y)) :
    print(f'{n + 1} FOLD Training.....')
    tr_x, tr_y = X.iloc[tr_idx], y.iloc[tr_idx]
    val_x, val_y = X.iloc[val_idx], np.expm1(y.iloc[val_idx])
    
    ngb = NGBRegressor(random_state = 42, n_estimators = 1000, verbose = 0, learning_rate = 0.03)
    ngb.fit(tr_x, tr_y, val_x, val_y, early_stopping_rounds = 300)
    
    val_pred = np.expm1(ngb.predict(val_x))
    val_nmae = NMAE(val_y, val_pred)
    ngb_val.append(val_nmae)
    print(f'{n + 1} FOLD NMAE = {val_nmae}\n')
    
    target_data = Pool(data = target, label = None)
    fold_pred = ngb.predict(target) / 10
    ngb_pred += fold_pred
print(f'10FOLD Mean of NMAE = {np.mean(ngb_val)} & std = {np.std(ngb_val)}')

submission['target'] = ngb_pred

submission.to_csv("../out/ngb/ngb1.csv", header = True, index = False)
