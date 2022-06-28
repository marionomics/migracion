import pandas as pd

df = pd.read_csv("data/remesas-mx.csv", encoding="latin")

df = df.drop([0, 1])
print(df.head())
