#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul  1 17:56:05 2021

@author: Zack-work
"""


import pandas as pd


path = 'data/variant_pre_march/raw/Region'

seq = range(1, 10, 1)

for i in seq:
    
    print(path + str(i) + '.csv')
    
    df = pd.read_csv(path + str(i) + '.csv', sep = '\t', encoding = 'UTF-16 LE')
    
    df['Region'] = 'Region ' + str(i)
    
    df.to_csv('data/variant_pre_march/clean/Region' + str(i) + '.csv')
