Classification project
----------------------
In this project I've used various text processing procedures to clean answers that were given to an open-ended question (What is your occupation?).

01_cleansing.R, 02_spacy.ipynb, 03_cleansing2.R

used R packages: data.table, readxl, readr, stringdist, stringr, stringi, reticulate, tidyverse, plyr

Main steps of data cleansing:
-----------------------------
- 1, standardization: lower case, change punctuation to space, change foreign letters to Latin, change multiple spaces to one 
- 2, deduplication
- 3, omit stopwords (in R with reticulate using nltk python package)
- 4, define a dictionary to correct misspellings: the dictionary is based on word frequencies of the official ISCO occupations
- 5, correct misspelled words if Levenshtein distance is max. 1: change these words to the most frequent word in dictionary
- 6, lemmatize words & part of speech analysis (in python, spacy, pandas)
- 7, compile a thematic dictionary based on the Hungarian webcorpus and the lemmas from source and ISCO occupations
- 8, decompose composite/compound words in several ways with the help of the compiled dictionary

Steps of classification (text similarity):
-----------------------------------------
- 1, joining raw data
- 2, joining phrases after correction of words (max 1 Levensthein distance)
- 3, joining based on highest equality of lemmas in words (max 1 lemma difference)
