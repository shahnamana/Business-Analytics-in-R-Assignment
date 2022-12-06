from datetime import date
import pandas as pd


df = pd.read_csv(
    "C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTest1.csv")
df['dob'] = pd.to_datetime(df['dob'], format="%d-%m-%Y")


def calculateAge(birthDate):
    today = date.today()
    age = today.year - birthDate.year -  ((today.month, today.day) <  (birthDate.month, birthDate.day))

    return age



df['age'] = df['dob'].apply(lambda x: calculateAge(x))
df.to_csv('C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTest11.csv', index=False)
print(calculateAge(date(1997, 2, 3)), "years")
