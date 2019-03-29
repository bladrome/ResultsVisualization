import pandas as pd

datadf = pd.read_excel("enzyme丰度表2019-01-04.xls", na_values = '-')
enzymenumber = ["1.2.1.2", "1.2.99.5", "2.3.1.101",
                "3.5.4.27", "1.5.98.1", "1.5.98.2", "2.1.1.86", "2.8.4.1"]

numberdf = datadf.iloc[:,2:]
numberdf = numberdf / numberdf.sum()
numberdf = numberdf * 100
datadf = pd.concat([datadf.iloc[:,0:2], numberdf], axis = 1)

Enzymedf = datadf[datadf.Enzyme.isin(enzymenumber)]
Enzymedf.to_excel("RumenKeggEnzyme.xls", index = False) 