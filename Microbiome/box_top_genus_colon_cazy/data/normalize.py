import pandas as pd

datadf = pd.read_excel("CAZy Family丰度表2019-01-04.xls", na_values="-")
numberdf = datadf.iloc[:,2:]
numberdf = numberdf / numberdf.sum()
numberdf = numberdf * 100

outputdf = pd.concat([datadf.iloc[:,0:2], numberdf], axis = 1)
outputdf = outputdf.sort_values(by=['Total'], ascending=False)
outputdf.to_excel("ColonCAZy.xls", index = False)