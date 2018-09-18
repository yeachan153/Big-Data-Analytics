import os
import pandas as pd

path = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Text Analytics Project/youtube-personality/transcripts"
os.chdir(path)

filenames = os.listdir()
dict1 = {}

for current_file in filenames:
    with open(current_file) as f:
        current_text = f.read().splitlines()
        dict1[current_file] = "".join([each.lower().strip() for each in current_text])

transcript = pd.DataFrame.from_dict(dict1, orient = "index")
transcript.reset_index(inplace = True)

transcript.to_csv("/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Text Analytics Project/youtube-personality/transcripts.csv", index = False)
