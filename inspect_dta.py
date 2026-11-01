import pandas as pd

dta1 = pd.read_stata(r"Replication\Data\Analysis_Annual.dta")
print("Analysis_Annual.dta shape:", dta1.shape)
print("Analysis_Annual.dta columns:", dta1.columns.tolist()[:30])

dta2 = pd.read_stata(r"Replication\Data\Auxi\SCM_9_Annual.dta")
print("\nSCM_9_Annual.dta shape:", dta2.shape)
print("SCM_9_Annual.dta columns:", dta2.columns.tolist()[:30])