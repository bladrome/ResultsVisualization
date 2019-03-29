import pandas as pd

datadf = pd.read_excel("NR物种注释结果表2018-12-21_Species.xls")
genusdf = datadf.groupby(['Genus']).sum()


rumenbac = ["g__Alistipes", "g__Methanobrevibacter"]
cecumbac = ["g__Clostridium", "g__Prevotella", "g__unclassified_d__Bacteria"]
colonbac = ["g__Clostridium", "g__Prevotella"]

rumencols = [str(i) + str(j) for i in ["F_LW", "S_LW"] for j in range(1, 6)]
cecumcols = [str(i) + str(j) for i in ["F_MC", "S_MC"] for j in range(1, 6)]
coloncols = [str(i) + str(j) for i in ["F_JC", "S_JC"] for j in range(1, 6)]

rumencols.append("Total")
cecumcols.append("Total")
coloncols.append("Total")

rumenbacdf = genusdf.loc[rumenbac]
cecumbacdf = genusdf.loc[cecumbac]
colonbacdf = genusdf.loc[colonbac]

rumenbacdf = rumenbacdf[rumencols]
cecumbacdf = cecumbacdf[cecumcols]
colonbacdf = colonbacdf[coloncols]

rumenbacdf.to_csv("rumenbac.csv")
cecumbacdf.to_csv("cecumbac.csv")
colonbacdf.to_csv("colonbac.csv")


## Top 30
k = 30
topkdf = genusdf.sort_values(by=['Total'], ascending=False)[0:k]
topkdf.to_csv("genustop_" + str(k) + ".csv")
