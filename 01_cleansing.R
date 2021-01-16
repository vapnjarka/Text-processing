rm(list=ls())


setwd("c:/Users/Asus/Rlabel/_coding2")

install.packages("stringdist") 

library(readxl)
library(stringr)
library(stringi)
#install.packages("RecordLinkage") this is very old -> stringdist!
#library(RecordLinkage)
library(stringdist)
library("data.table")


##########################################
#1, MC FOGL

#import file as a data fame
mcfogl <- as.data.frame(read_excel("2016_fogl_csakdolgozok.xlsx"))


#lower case to variable names
names(mcfogl)<- tolower(names(mcfogl))


#lower case to variable content
mcfogl$fogl<-tolower(mcfogl$fogl)

#before any correction (only lowercase), what are the most frequent phrases?
freq_zero<-data.frame(foglalkozas=names(summary(as.factor(mcfogl$fogl))),gyakorisag=summary(as.factor(mcfogl$fogl)), row.names=NULL) 



#list of characters of fogl
split_letters_mc <- unlist(strsplit(mcfogl$fogl, split = "")) #first split, then unique
sort(unique(split_letters_mc))

#freq to these chars
freq<-data.frame(karakter=names(summary(as.factor(split_letters_mc))), gyakorisag=summary(as.factor(split_letters_mc)), row.names=NULL)
write.table(freq, "fogl_freq.csv", sep = ";", row.names = F)
#its not really ok, because there is ; in the text as well

#phrases with ô and with their freq
freq3<-data.frame(szo=names(summary(as.factor(str_subset(mcfogl$fogl, "ô")))), gyakorisag=summary(as.factor(str_subset(mcfogl$fogl, "ô"))), row.names=NULL)
#somehow only top 100 word

#phrases with ? and with their freq
freq2<-data.frame(szo=names(summary(as.factor(str_subset(mcfogl$fogl, "\\?")))), gyakorisag=summary(as.factor(str_subset(mcfogl$fogl, "\\?"))), row.names=NULL)
#somehow only top 100 word

#â
str_subset(mcfogl$fogl, "â")



#where is char "\177" (it means DEL)
subset(mcfogl,mcfogl$fogl %in% str_subset(mcfogl$fogl, "\177"),select=)





#change "-", "&", "(", ")", ",", ".", "/", ":", ";", "_" to space 
mcfogl<-cbind(mcfogl,fogl2=str_replace_all(mcfogl$fogl, "[-&(),./:;_]", " "), stringsAsFactors=F)
#cbind makes automatically factors, it's needed to avoid it with "stringsAsFactors=F"


#keep all the alphabetical chars, numbers (alnum: letters&numbers) and space
mcfogl$fogl2<-str_replace_all(mcfogl$fogl2, "[^[:alnum:] [:space:]]", "")


#change foreign letters to latin
mcfogl<-cbind(mcfogl,fogl3=str_replace_all(mcfogl$fogl2, c('à' = 'a', 'ă' = 'a', 'â' = 'a', 
'å' = 'a', 'ã' = 'a', 'ą' = 'a', 'ā' = 'a', 'ä' = 'a', 'ḃ' = 'b', 'ć' = 'c', 
'ĉ' = 'c', 'č' = 'c', 'ċ' = 'c', 'ç' = 'c', 'ď' = 'd', 'ḋ' = 'd', 'đ' = 'd', 
'è' = 'e', 'ĕ' = 'e', 'ê' = 'e', 'ě' = 'e', 'ë' = 'e', 'ė' = 'e', 'ę' = 'e', 'ē' = 'e', 
'ḟ' = 'f', 'ƒ' = 'f', 'ğ' = 'g', 'ĝ' = 'g', 'ġ' = 'g', 'ģ' = 'g', 'ĥ' = 'h', 'ħ' = 'h', 
'ì' = 'i', 'î' = 'i', 'ï' = 'i', 'ĩ' = 'i', 'į' = 'i', 'ī' = 'i', 'ĵ' = 'j', 'ķ' = 'k', 
'ĺ' = 'l', 'ľ' = 'l', 'ļ' = 'l', 'ł' = 'l', 'ṁ' = 'm', 'ń' = 'n', 'ň' = 'n', 'ñ' = 'n', 
'ņ' = 'n', 'ò' = 'o', 'ô' = 'o', 'õ' = 'o', 'ō' = 'o', 'ơ' = 'o', 'ṗ' = 'p', 
'ŕ' = 'r', 'ř' = 'r', 'ŗ' = 'r', 'ś' = 's', 'ŝ' = 's', 'š' = 's', 'ṡ' = 's', 'ş' = 's', 
'ș' = 's', 'ť' = 't', 'ṫ' = 't', 'ţ' = 't', 'ț' = 't', 'ŧ' = 't', 'ù' = 'u', 'ŭ' = 'u', 
'û' = 'u', 'ů' = 'u', 'ũ' = 'u', 'ų' = 'u', 'ū' = 'u', 'ư' = 'u', 'ẃ' = 'w', 'ẁ' = 'w', 
'ŵ' = 'w', 'ẅ' = 'w', 'ý' = 'y', 'ỳ' = 'y', 'ŷ' = 'y', 'ÿ' = 'y', 'ź' = 'z', 'ž' = 'z', 
'ż' = 'z', 'µ' = 'u')), stringsAsFactors=F)

#there were some warnings
warnings()

#check letters
split_letters_mc2 <- unlist(strsplit(mcfogl$fogl3, split = "")) #first split, then unique
sort(unique(split_letters_mc2))



#change multiple spaces to one (str(squish))
mcfogl<-cbind(mcfogl,fogl4=str_squish(mcfogl$fogl3), stringsAsFactors=F)

#where is shorter words than original
tmp<-subset(mcfogl, nchar(mcfogl$fogl4)<nchar(mcfogl$fogl),select=)

#check again list of characters, after manipulation
split_letters_mc2 <- unlist(strsplit(mcfogl$fogl4, split = "")) #first split, then unique
sort(unique(split_letters_mc2))
#left only Hungarian alphabet and numbers, currently we keep them


#whether there are NAs? NO
subset(mcfogl,is.na(mcfogl$fogl4)==T, select=c(sorsz,fogl2,fogl4))

#save file before deduplications
write.table(mcfogl, "fogl_full.csv", sep = ";", row.names = F)



#whether there are duplicates? 285.587!!!
mcfogl$fogl4[duplicated(mcfogl$fogl4)]
dup<-subset(mcfogl, mcfogl$fogl4 %in% mcfogl$fogl4[duplicated(mcfogl$fogl4)], select=c(sorsz,fogl2,fogl4) )
dup<-dup[order(dup$fogl4),]


#omit duplicates
mcfogl<-mcfogl[!duplicated(mcfogl$fogl4), ] #363.182 -> 94.430!!!


write.table(mcfogl, "fogl.csv", sep = ";", row.names = F)

####################
#2, FEOR munkakörök


#import 
feor <- as.data.frame(read_excel("feor-08_munkaköri_lista.xlsx"))

#lower case to variable names
names(feor)<- tolower(names(feor))

#lower case to variable content
feor$munkk<-tolower(feor$munkk)

#list of characters of feor
split_letters_feor <- unlist(strsplit(feor$munkk, split = ""))
sort(unique(split_letters_feor))

#9: mikrobuszvezető (9 fő fölött)
str_subset(feor$munkk, "9")

#change [-&(),./:;_] to space 
feor<-cbind(feor, munkk2=str_replace_all(feor$munkk, "[-&(),./:;_]", " "), stringsAsFactors=F)

#keep all the alpabetical chars, numbers (alnum: letters&numbers) and space
feor<-cbind(feor,munkk3=str_replace_all(feor$munkk2, "[^[:alnum:] [:space:]]", ""), stringsAsFactors=F)

#change multiple spaces to one (str(squish))
feor<-cbind(feor,munkk4=str_squish(feor$munkk3), stringsAsFactors=F)

#check again list of characters, after manipulation
split_letters_feor2 <- unlist(strsplit(feor$munkk4, split = ""))
sort(unique(split_letters_feor2))


#whether there are duplicates? 74
feor$munkk4[duplicated(feor$munkk4)]
dup<-subset(feor, feor$munkk4 %in% feor$munkk4[duplicated(feor$munkk4)], select= )
dup<-dup[order(dup$munkk),]

#deduplication
feor<-feor[!duplicated(feor$munkk4), ]

#whether there are NAs? yes 1
subset(feor,is.na(feor$munkk4)==T, select=)
#this was originally empty!!!


#drop Nas
feor<-subset(feor,is.na(feor$munkk4)==F, select=)

write.table(feor, "feor.csv", sep = ";", row.names = F)


################################################

#install miniconda: https://docs.conda.io/en/latest/miniconda.html
#Miniconda3 Windows 64-bit

#ez volt miniconda idejĂ©n
#set virtual env by writing this into anaconda power shell:


#py_config() - ezt nem merem futtatni, mert elallitodnak a dolgok

#######################################################


# problĂ©ma volt nltk installal (windows alatt nem lehet telepiteni) ->
#reticulate ujratelepitese: install.packages("reticulate")
#library("reticulate")
#Sys.which("python") -> "C:\\PROGRA~2\\Python\\python.exe"
#use_python("C:/Users/Asus/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
#use_virtualenv("myenv")
#py_install("nltk")
#ide pakolta: C:\Users\Asus\anaconda3\envs\r-reticulate\python.exe
#es ide: C:\Users\Asus\anaconda3\envs\r-reticulate\Lib\site-packages\nltk
#ezt uzente meg:
#To activate this environment, use
#
#     $ conda activate r-reticulate
#
# To deactivate an active environment, use
#
#     $ conda deactivate

install.packages("reticulate")
library("reticulate")
Sys.which("python")
#use_python("C:/Users/Asus/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
use_python("C:/Users/Asus/anaconda3/envs/r-reticulate/python.exe") #Nem ezt kĂ©ne?
use_virtualenv("myenv")


#NLTK install & load
py_install("nltk")
nltk<- import ("nltk")

#tokenize install & load
#nltk$download('punkt')
tokenezes<-nltk$tokenize$word_tokenize

#stopword list download & load
#nltk$download('stopwords') here: C:\Users\Asus\AppData\Roaming\nltk_data...
stop_words <- nltk$corpus$stopwords #all languages
stopwordshun <- stop_words$words('hungarian')

#Atirni %%chin%-re?
"%notin%" <- Negate("%in%") #function to negation 


#import earlier data cleansing
mcfogl<-read.csv("fogl.csv", header=T, sep=";")

#transform data.frame to data.table and create an index
mcfogl<-data.table(mcfogl)
system.time(setkey(mcfogl, fogl4))

mcfogl$fogl5<-NA #blank column for mcfogl


#omit stopwords in the df mcfogl
system.time(
for (i in 1:nrow(mcfogl)) { #in every row of the df
  words_tokens<-tokenezes(mcfogl$fogl4[i]) #tokenize the string in the givien cell
  for (w in words_tokens){ 
    if (w %notin% stopwordshun) mcfogl$fogl5[i]<-trimws(stri_flatten(c(mcfogl$fogl5[i],w),collapse= " ", na_empty=T))
  }
  mcfogl$hossz[i]<-nchar(mcfogl$fogl5[i])-nchar(mcfogl$fogl4[i]) #change in string length
})

# user  system elapsed 
#1981.81    4.06 2125.57  ->35 min!!!



#whether there are NAs? 9 p.
subset(mcfogl,is.na(mcfogl$fogl5)==T, select=c(sorsz,fogl4,fogl5))
#NA for "-", a, bármi, egyéb etc.

#drop Nas
mcfogl<-subset(mcfogl,is.na(mcfogl$fogl5)==F, select=)


#which strings became shorter
tmp<-subset(mcfogl,mcfogl$hossz!=0, select=)
tmp<-tmp[order(tmp$hossz),]

length(unique(unlist(strsplit(mcfogl$fogl4, " ")))) #43967 unique words before stopword check
length(unique(unlist(strsplit(mcfogl$fogl5, " ")))) #43873 unique words after stopword omit

#save mcfogl (order by length)
write.table(mcfogl, "fogl2.csv", sep = ";", row.names = F)

################
#feor


#import earlier data cleansing
feor<-read.csv("feor.csv", header=T, sep=";")

feor$munkk5<-NA #blank column for feor

#omit stopwords in the df feor
system.time(
for (i in 1:nrow(feor)) { #in every row of the df
  words_tokens2<-tokenezes(feor$munkk4[i]) #tokenize the string in the givien cell
  for (w in words_tokens2){ 
    if (w %notin% stopwordshun) feor$munkk5[i]<-trimws(stri_flatten(c(feor$munkk5[i],w),collapse= " ", na_empty=T))
  }
  feor$hossz[i]<-nchar(feor$munkk5[i])-nchar(feor$munkk4[i]) #change in string length
})

#that was fast!!!
#user  system elapsed 
#6.06    0.28    6.60 

#whether there are NAs? 
subset(feor,is.na(feor$munkk5)==T, select=) #-> no
subset(feor,feor$munkk5=="NA", select=) #->no



#which strings became shorter
tmp2<-subset(feor,feor$hossz!=0, select=)
tmp2<-tmp2[order(tmp2$hossz),]

length(unique(unlist(strsplit(feor$munkk4, " ")))) #6864 unique words before stopword check
length(unique(unlist(strsplit(feor$munkk5, " ")))) #6849 unique words after stopword omit

#save feor (order by length)
write.table(feor, "feor2.csv", sep = ";", row.names = F)

###############################################################
#import after stopword

mcfogl<-read.csv("fogl2.csv", header=T, sep=";")
feor<-read.csv("feor2.csv", header=T, sep=";")

#create a dictionary with freq of all of the words
# create a vector of all words in your df
all_words<-unlist(str_split(feor$munkk5, pattern = " ")) #feor
#all_words2<-unlist(str_split(mcfogl$fogl5, pattern = " ")) #mcfogl
#words<-c(all_words, all_words2) #combine the two char vectors
#class(all_words2)
#write.table(all_words2, "all_words2.csv", sep = ";", row.names = F)


# create a frequency table = dictionary
#dict<-as.data.frame(table(words), stringsAsFactors = F) # altogether ... word in the dict
#dict<-dict[order(-dict$Freq),] #order by freq
#dict<-dict[order(nchar(dict$words)),] #order by length: to find quicker the given entry
#dict<-dict[order(dict$all_words),]  #alphabetic order

# create a frequency table = dictionary, only based on feor: 6850 entries
dict<-as.data.frame(table(all_words), stringsAsFactors = F) 
dict<-dict[order(nchar(dict$all_words)),] #order by length: to find quicker the given entry

#is there NA? no
str_subset(feor$munkk5, "NA")


#find special rows in dict
str_subset(feor$munkk5, " of ")
uj<-subset(feor,str_detect(feor$munkk5, ""), select=) #select partial match


#save dictionary (order by length)
write.table(dict, "dict.csv", sep = ";", row.names = F)




#new column for mcfogl (fogl6) 
#"candidates" for a given world: words within max 1 Levensthein distance
#wordssum<-data.frame() #word, candidate, freq table
mcfogl$fogl6<-NA #blank column for mcfogl
mcfogl$fogl7<-NA


#transform data.frames to data.tables and create an index
mcfogl<-data.table(mcfogl)
dict<-data.table(dict)
setkey(mcfogl, fogl5)
setkey(dict, all_words)

nrow(mcfogl)

system.time(
  for (i in 1:nrow(mcfogl)) { #in every row of the df
    if (substr(i, nchar(as.integer(i))-2,nchar(as.integer(i)))=='000'){
      print(i)
    }
    words_tokens<-tokenezes(mcfogl$fogl5[i]) #tokenize the string in the given cell
    words<-data.frame(w=words_tokens,cand=character(length(words_tokens)),freq=numeric(length(words_tokens))) #temp df for words of one expression
    n<-1    # the position of the word in the expression
    for (w in words_tokens){#in every word of the expression
      if (nchar(w)<3 ||                 #if shorter than 3 chars
          nchar(w)-1>25 ||              #if longer than 25 chars
          w %chin% dict$all_words==T) { #IF exactly matches the word with a dictionary entry
        words[n,2]<-w }               #paste original word, and step to next w
      
      else {                      #IF DO NOT exactly matches the word with a dictionary entry
        range<-c(nchar(w)-1,nchar(w), nchar(w)+1) #range of length of the given entry: +/-1 long (this vector with 3 elements)
         #if () { #if the "shortest" range word is longer than longest entry in dict (25 char)-> go to next word
        #words[n,2]<-w  #paste original word
        #n<-n+1
         # next} 
        #else {
          dictrange<-data.table(subset(dict, nchar(dict$all_words) %in% range==T, select=)) #subsetting the dictionary
          words[n,2]<-w             #paste original word  
          ##ITT NEM BIZTOS, HOGY TOVÁBB MEGY A MAGASABB FREQ-JU SZÓHOZ...
          for (d in 1:nrow(dictrange)){ #in every entry of the subsetted dictionary
            if (stringdist(w,as.character(dictrange[d,1]), method="lv")==1  # whether distance=1 
                && words[n,3]< dictrange[d,2] ) {      # and whether the new candidates' freq is higher (ergo if equals, the first one remains)
              words[n,2]<-dictrange[d,1]             #paste candidate word
              words[n,3]<-dictrange[d,2]}           #paste freq  
          }}#}                  
      n<-n+1                                  #jump to next word
      if (n>length(words_tokens)) {           
        mcfogl$fogl6[i]<- trimws(stri_flatten(c(words[1:nrow(words),2]),collapse= " ",na_empty=T)) #concat candidate words, and paste into a new column
        break}}                                 #until the last word
    #wordssum<-rbind(wordssum,words) 
  }) 

#i=45194
#Timing stopped at: 6361 3.23 3.921e+04

#11 row
#113.78    4.04  121.54
#0.05 0 0.05

#100 row
#753.54   26.59 2290.21=38 min
# 92.61    2.78   97.75 
#115.94    3.36  131.33 
# 52.27    2.41   63.28 
# 49.79    1.86   57.08 
#44.45    1.85   46.34



#i=68.191...

#final:
#60415.38  2291.19 66227.76 =18,4 óra

#str(dict[d,1])

#Character columns cannot be sorted in descending order!!!
#wordssum2<-wordssum[order(-wordssum$freq),]

#check: 	whether remained NA?
subset(mcfogl,is.na(mcfogl$fogl6)==T, select=c(fogl5,fogl6)) #NO



length(unique(unlist(strsplit(mcfogl$fogl5, " ")))) # 43.873 unique words before dictionary check
length(unique(unlist(strsplit(mcfogl$fogl6, " ")))) # 36.440 unique words after dictionary check

write.table(feor, "feor3.csv", sep = ";", row.names = F)
write.table(mcfogl, "fogl3.csv", sep = ";", row.names = F)

###############################################
feor<-read.csv("feor3.csv", header=T, sep=";")
mcfogl<-read.csv("fogl3.csv", header=T, sep=";")

#save once again in utf-8 encoding, because python likes that better, than normal saving
write.table(feor, "feor3.csv", sep = ";", row.names = F, fileEncoding = "UTF-8")
write.table(mcfogl, "fogl3.csv", sep = ";", row.names = F, fileEncoding = "UTF-8")

################################################
