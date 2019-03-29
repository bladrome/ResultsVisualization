import pandas as pd
import numpy as np

k = 30

datadf = pd.read_excel("level丰度表2019-01-04.xls")
datadf = datadf.iloc[:,1:]
datadf.rename(columns={"Level2":"Pathway", "Level3":"Description"}, inplace=True)

topkdf = datadf.sort_values(by=['Total'], ascending=False)

#seleceteddf = topkdf[0:k]
seleceteddf = topkdf

seleceteddf.to_csv("siglevel" + str(k) + ".csv")

#seleceteddf.to_csv("Pathway_" + str(k) + ".csv")

colnameslist = [i for i in datadf.columns]

Fcondmusk = ['F_' in i for i in colnameslist]
Scondmusk = ['S_' in i for i in colnameslist]

Fconddf = datadf.loc[:, Fcondmusk]
Sconddf = datadf.loc[:, Scondmusk]

LWmusk = [True, True]
MCmusk = [True, True]
JCmusk = [True, True]

LWmusk.extend(['LW' in i for i in colnameslist[2:]])
MCmusk.extend(['MC' in i for i in colnameslist[2:]])
JCmusk.extend(['JC' in i for i in colnameslist[2:]])

LWdf = datadf.loc[:, LWmusk]
MCdf = datadf.loc[:, MCmusk]
JCdf = datadf.loc[:, JCmusk]

LWdf['Total'] = LWdf.sum(axis=1)
LWdf = LWdf.sort_values(by=['Total'], ascending=False)
numberdf = LWdf.iloc[:,2:]
numberdf = numberdf / numberdf.sum()
numberdf = numberdf * 100
LWdf = pd.concat([LWdf.iloc[:,0:2], numberdf], axis=1)

MCdf['Total'] = MCdf.sum(axis=1)
MCdf = MCdf.sort_values(by=['Total'], ascending=False)
numberdf = MCdf.iloc[:,2:]
numberdf = numberdf / numberdf.sum()
numberdf = numberdf * 100
MCdf = pd.concat([MCdf.iloc[:,0:2], numberdf],axis=1)


JCdf['Total'] = JCdf.sum(axis=1)
JCdf = JCdf.sort_values(by=['Total'], ascending=False)
numberdf = JCdf.iloc[:,2:]
numberdf = numberdf / numberdf.sum()
numberdf = numberdf * 100
JCdf = pd.concat([JCdf.iloc[:,0:2], numberdf], axis=1)


LWdf.to_csv(
    "Pathway_rumen.csv",
    index=False,
    columns=['Pathway', 'Description'] + sorted(LWdf.columns[2:12]) + ['Total'])
MCdf.to_csv(
    "Pathway_cecum.csv",
    index=False,
    columns=['Pathway', 'Description'] + sorted(MCdf.columns[2:12]) +  ['Total'])
JCdf.to_csv(
    "Pathway_colon.csv",
    index=False,
    columns=['Pathway', 'Description'] + sorted(JCdf.columns[2:12]) + ['Total'])
