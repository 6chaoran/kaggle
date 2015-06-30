# -*- coding: utf-8 -*-
"""
Created on Thu Jun 25 16:14:59 2015

@author: chaoranliu
"""
import pandas as pd
import numpy as np
file_path='/Users/chaoranliu/Dropbox/kaggle/titanic/'
train=pd.read_csv(file_path+'train.csv',na_values=[' ','NA'])
test=pd.read_csv(file_path+'test.csv',na_values=[' ','NA'])
test['Cat']='test'
train['Cat']='train'
test['Survived']=None
full=pd.concat([train,test])
_=full.fillna({'Age':full.Age.median(),'Fare':full.Fare.median(),'Embarked':'S'},inplace=True)
full['FamilySize']=full.SibSp+full.Parch+1

train=full[full.Cat=='train']
test=full[full.Cat=='test']