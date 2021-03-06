# -*- coding: utf-8 -*-
"""
Created on Thu Sep 20 21:49:15 2018

@author: SiqueiraCampos2
"""

import pandas as pd
import numpy as np
import itertools

from dask import dataframe as dd
from dask.multiprocessing import get
from multiprocessing import cpu_count

nCores = cpu_count()

import os
os.chdir('c:\\Users\SiqueiraCampos2\Documents\Python_scripts')

# read df_day2 data frame

source='df_day2_PI.csv'

df = pd.read_csv(source, sep=',')

# Row generators, all combinations of df row  
rows_l=list(itertools.combinations(range(0,len(df.index)),2))



# Extract number generator

t1=[i[0] for i in rows_l]
t2=[i[1] for i in rows_l]

# Dask processing
dd.from_pandas(df,npartitions=2*nCores)
resultado1=[sum(np.logical_and(df.loc[a,]==df.loc[b,],df.loc[a,]==5)) for a,b in zip(t1, t2)]
resultado2=[sum(np.logical_and(df.loc[a,]==df.loc[b,],df.loc[a,]!=5)) for a,b in zip(t1, t2)]
dd.compute(get=get)

# Output dataframe
df_output=pd.concat([pd.DataFrame(t1),pd.DataFrame(t2),pd.DataFrame(resultado1),pd.DataFrame(resultado2)], axis=1)
df_output.columns=['row_1', 'row_2', 'correct','incorrect']

# Sort top 20 incorrect and correct
df1=df_output.sort_values('correct', ascending=False).head(20)
df2=df_output.sort_values('incorrect', ascending=False).head(20)

# Stack data frame
df1=df1.append(df2)

# add new column for R index
df1["newrow1"]=df1["row_1"]+1
df1["newrow2"]=df1["row_2"]+1

# remove big file
del(df_output)

# New name to a file
new="df_out"+source[2:]

# Save as csv file
df1.to_csv(new)



