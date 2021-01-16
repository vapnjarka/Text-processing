rm(list=ls())

setwd("c:/Users/Asus/Rlabel/_coding2")


install.packages("data.table")
install.packages("stringr")
install.packages("readr")
install.packages("assertthat")
install.packages("tidyverse")
install.packages("backports")
install.packages("plyr")


library("data.table")
library("stringr")
library("readr")
library("tidyverse")
library(stringi)
library(plyr)


#mcfogl
#################################

#import work file after python lemmatizing process
mcfoglp<-read.csv("foglp.csv", encoding="UTF-8", header=T)


#1, convert strings into list
mcfoglp$lemmas3<-as.list(lapply(strsplit(gsub("['|']|[][]|\\s", '', mcfoglp$lemmas), ',', fixed = TRUE), as.character))
mcfoglp$pos2<-as.list(lapply(strsplit(gsub("['|']|[][]|\\s", '', mcfoglp$pos), ',', fixed = TRUE), as.character))

#how to refer to a list element
mcfoglp$pos2[[5]][1]
length(mcfoglp$pos2[[5]])


#freq of pos: 
#([('PRON', 755), ('PUNCT', 1163), ('NUM', 480), ('ADJ', 83524), 
#('NOUN', 149749), ('PROPN', 86), ('ADV', 1599), ('X', 112), ('ADP', 97),
#('VERB', 3156), ('CCONJ', 516), ('DET', 350), ('INTJ', 12), ('SCONJ', 28), 
#('AUX', 12), ('PART', 5)])


#find given pos
aux<-subset(mcfoglp, mcfoglp$pos2 %in% str_subset(mcfoglp$pos2, 'AUX'), select=c("lemmas3", "pos2"))
num<-subset(mcfoglp, mcfoglp$pos2 %in% str_subset(mcfoglp$pos2, 'NUM'), select=c("fogl", "lemmas3", "pos2"))
#interesting: written numbers also became num eg. hatvan; or the char "i" as well
x<-subset(mcfoglp, mcfoglp$pos2 %in% str_subset(mcfoglp$pos2, 'X'), select=c("fogl", "lemmas3", "pos2"))
punct<-subset(mcfoglp, mcfoglp$pos2 %in% str_subset(mcfoglp$pos2, 'PUNCT'), select=c("fogl", "lemmas3", "pos2"))
#strange: "manager" also punct...

#finally we do not drop pos, we do not like to loose information

#FEOR
#################################

#import work file after python lemmatizing process
feorp<-read.csv("feorp.csv", encoding="UTF-8", header=T)


########################################################################################
#import hungarian webcorpus (tab separated, ISO Latin-2 encoded Unix text file )


###readr csomag
#huncorp<-read_tsv("web2.2-freq-sorted.txt", col_names = F, col_types = NULL, locale(encoding ="latin2"), quote = "\"")
huncorp<-read_delim("web2.2-freq-sorted.txt", delim='\t', escape_double=FALSE, 
  escape_backslash=FALSE, quote="\"", col_names = F, col_types = NULL, 
  locale(encoding ="latin2"), na = c("", "NA"))

problems(huncorp)

tail(huncorp, n=5)
subset(huncorp,is.na(huncorp$X1)==T, select=)
#1       X2    X3    X4    X5
#<chr> <dbl> <dbl> <dbl> <dbl>
#1 NA    46285 10657  3255  1212 - üres cella
#2 NA     3300  2705  1987  1153 - szószerint ez volt a szövegben, hogy NA 
#3 NA       NA    NA    NA    NA - a fájl legvége

subset(huncorp,is.na(huncorp$X2)==T, select=)


#filter huncorp
#a, freq=>1000
huncorp2<-subset(huncorp,huncorp$X2>999, select=X1:X5)


#b, only alphabetical chars
huncorp3<-subset(huncorp2,grepl("[[:alpha:]]", huncorp2$X1), select=)

#somehow it does not really works -> need to do it separately too
huncorp4<-subset(huncorp3,!grepl("[[:punct:]]", huncorp3$X1), select=)
huncorp5<-subset(huncorp4,!grepl("[[:digit:]]", huncorp4$X1), select=)
str_subset(huncorpx$X1, "û")


#filter for asterix
#str_subset(huncorp3$X1, "\\*")


#c, lower case
huncorp5$X1<-tolower(huncorp5$X1)

#d, minimum 3 char length
huncorp6<-subset(huncorp5,nchar(huncorp5$X1)>2, select=)

#e, find non-Hungarian words, and extract them from huncorp
nonhun<-subset(huncorp6,huncorp6$X3<1000, select=)
huncorp7<-subset(huncorp6,huncorp6$X3>999, select=X1:X2)

#f, check uniqueness
huncorp7[duplicated(huncorp7$X1),]

#eg. how many rows of "the" , "magyar"
huncorp7[huncorp7$X1 == 'the', ]
huncorp7[huncorp7$X1 == 'magyar', ]

#Remove duplicates
huncorp8<-huncorp7[!duplicated(huncorp7$X1), ]
tail(huncorp8, n=10)

#export table
write.table(huncorp8, "huncorp.csv", sep = ";", row.names = F, fileEncoding = "UTF-8")

#import
huncorp8<-read.csv("huncorp.csv", sep = ";",encoding="UTF-8", header=T)
####################################################################

#import table after python lemmatizing 
huncorpp<-read.csv("huncorpp.csv", encoding="UTF-8", header=T)

#remove [''] from lemmas
huncorpp$lemmas2<-gsub("['|']|[][]|\\s", '', huncorpp$lemmas)

#check and remove duplicates - no dupl
huncorpp[duplicated(huncorpp$lemmas2),]
huncorpp<-huncorpp[!duplicated(huncorpp$lemmas2), ] #(original: 73069 entry, now: 33932)

#check length of words, remove 2 char long words (original: 33932 entry, now: 33666)
subset(huncorpp,nchar(huncorpp$lemmas2)<3, select=)
huncorpp<-subset(huncorpp,nchar(huncorpp$lemmas2)>2, select=)



#add lemmatized words (and their freq) from mcfogl and feor to huncorpp
############################################################################


#1, extract word list from mcfogl$lemmas2
mcfoglp_list<-data.frame("lemmas2"=unlist(strsplit(mcfoglp$lemmas2, " ")))
length(unique(unlist(strsplit(mcfoglp$lemmas2, " ")))) #32.547 various words



#2, extract word list from feorp$lemmas2
feorp_list<-data.frame("lemmas2"=unlist(strsplit(feorp$lemmas2, " ")))
length(unique(unlist(strsplit(feorp$lemmas2, " ")))) #6705 various words

#add the word lists to the dictionary 
huncorpp<-rbind.fill(huncorpp, mcfoglp_list)
huncorpp<-rbind.fill(huncorpp, feorp_list)

#check doubles: 227.590
length(huncorpp$lemmas2[duplicated(huncorpp$lemmas2)])
huncorpp$lemmas2[duplicated(huncorpp$lemmas2)]

#Remove duplicates
huncorpp<-huncorpp[!duplicated(huncorpp$lemmas2), ]
tail(huncorpp, n=10)

#remove rows, which are realizing not valid decomposotions
to_drop_from_dict<-c("kez", "fel", "ism", "elõ", "ség", "ten", "meg", "zen", "ész", "szaki", "sza", "szobr")
huncorpp <- huncorpp[!huncorpp$lemmas2 %in% to_drop_from_dict, ]


#see 2 char long entries
subset(huncorpp,nchar(huncorpp$lemmas2)<3, select=)


#save dictionary
write.table(huncorpp, "huncorpp_final.csv", sep = ";", row.names = F)

##############################################################

#decompose composite/compound words in mcfoglp

#order huncorpp dictionary by length of words
#huncorpp<-huncorpp[order(nchar(huncorpp$lemmas2)),]

#order huncorpp dictionary by length of words and alphabet
#huncorpp<-huncorpp[order(nchar(huncorpp$lemmas2),huncorpp$lemmas2),]

#transform data.frame to data.table and create an index
huncorppdt<-data.table(huncorpp)
system.time(setkey(huncorppdt, lemmas2))

#iterating through all rows and all list elements


mcfoglp$comp<-NA #blank column  



system.time(     
  for (r in 1:nrow(mcfoglp)) {                    #through the rows of the table
    for (e in 1:length(mcfoglp$lemmas3[[r]])) {   #through the elements of the row/list
      if (nchar(mcfoglp$lemmas3[[r]][e])<6) {     #if word<6 go to next word
        next}
      else {  
        for (f in 3:nchar(mcfoglp$lemmas3[[r]][e])-3) {  #through valid decompositions, (from 3, but in reallity it begins from 0)
  
          if (substr(mcfoglp$lemmas3[[r]][e],1,f) %chin%  # is the first part 
              huncorppdt$lemmas2==TRUE)                   # in the dictionary?
            
          { if (substr(mcfoglp$lemmas3[[r]][e],f+1,nchar(mcfoglp$lemmas3[[r]][e])) %chin% #is the second part
                huncorppdt$lemmas2==TRUE)                 # in the dictionary?
          { fstp<-substr(mcfoglp$lemmas3[[r]][e],1,f)     # save first part
          secp<-substr(mcfoglp$lemmas3[[r]][e],f+1,nchar(mcfoglp$lemmas3[[r]][e])) #save second part
          mcfoglp$comp[r]<-stri_flatten(c(mcfoglp$comp[r],fstp, secp),collapse= " ",na_empty=T) #concat first and second part to the list, and paste into a new column
          }}    
          else                      
          {next}
        }}}
})

#r=1843
# 40.88    0.03   41.39 

#r=94.421 
#3346.19    9.65 3366.41 = 56 min
#3725.10   44.87 5620.47 = 93 min


#"ügyi", "gyógy" ez hiányzik a szótárból...
huncorpp[huncorpp$lemmas2 == 'ügyi', ]  #hianyzik
str_subset(huncorpp$lemmas2, "gyógy")
str_subset(huncorpp$lemmas2, "mész")
str_subset(huncorpp$lemmas2, "gyártó") #ez benne van
huncorpp[huncorpp$lemmas2 == 'autó', ] #ez benne van
huncorpp[huncorpp$lemmas2 == 'mosógép', ] #ez benne van
huncorpp[huncorpp$lemmas2 == 'szõnyeg', ] #ez benne van
huncorpp[huncorpp$lemmas2 == 'szövõ', ] #ez benne van
huncorpp[huncorpp$lemmas2 == 'motor', ] #ez benne van
huncorpp[huncorpp$lemmas2 == 'fûrész', ] #hianyzik

#eredetiben sincsenek?
huncorp8[huncorp8$X1 == 'ügyi', ] #nem
huncorp8[huncorp8$X1 == 'gyógy', ] #nem
huncorp8[huncorp8$X1 == 'mész', ] #IGEN! (szótövesítés valszeg menni, vagy megy-et csinált belõle...)
huncorp8[huncorp8$X1 == 'szövõ', ] #nem
huncorp8[huncorp8$X1 == 'fûrész', ] #nem

#legleg erdetiben?
huncorp[huncorp$X1 == 'fûrész', ] #nem


#check wether if there is a word which has been divedid in more various ways
#i.e.  bõr ruhakészítõ, bõrruha készítõ




######################################################
#compose final wordlist (lemmas2+comp)

#combine two cols but ignore NAs
#(it puts underscore)
mcfoglp%>% unite("wordl", c(lemmas2, comp), na.rm = TRUE, remove = FALSE) %>%
  {.} -> mcfoglp # This saves the changed data frame under the old name

#omit underscores
mcfoglp$wordl<-gsub("_", '', mcfoglp$wordl)

#final wordlist in mcfogl
mcfoglp2<-subset(mcfoglp, select=c("sorsz","fogl","foglkod", "fogl6", "wordl"))

#add unaccented version too   
mcfoglp2$wordl_noacc<-iconv(mcfoglp2$wordl, from='UTF-8', to='ASCII//TRANSLIT')

#save mcfoglp
write.table(mcfoglp2, "mcfogl_final.csv", sep = ";", row.names = F)

######################################

#decompose composite/compound words in feorp

#convert strings into list
feorp$lemmas3<-as.list(lapply(strsplit(feorp$lemmas2, ' ', fixed = TRUE), as.character))


#iterating through all rows and all list elements
feorp$comp<-NA #blank column  

system.time(     
  for (r in 1:nrow(feorp)) {                    #through the rows of the table
    for (e in 1:length(feorp$lemmas3[[r]])) {   #through the elements of the row/list
      if (nchar(feorp$lemmas3[[r]][e])<6) {     #if word<6 go to next word
        next}
      else {  
        for (f in 3:nchar(feorp$lemmas3[[r]][e])-3) {  #through valid decompositions, (from 3, but in reallity it begins from 0)
          
          if (substr(feorp$lemmas3[[r]][e],1,f) %chin%  # is the first part 
              huncorppdt$lemmas2==TRUE)                   # in the dictionary?
            
          { if (substr(feorp$lemmas3[[r]][e],f+1,nchar(feorp$lemmas3[[r]][e])) %chin% #is the second part
                huncorppdt$lemmas2==TRUE)                 # in the dictionary?
          { fstp<-substr(feorp$lemmas3[[r]][e],1,f)     # save first part
          secp<-substr(feorp$lemmas3[[r]][e],f+1,nchar(feorp$lemmas3[[r]][e])) #save second part
          feorp$comp[r]<-stri_flatten(c(feorp$comp[r],fstp, secp),collapse= " ",na_empty=T) #concat first and second part to the list, and paste into a new column
          }}    
          else                      
          {next}
        }}}
})

#for 10.000 row...
#314.73    4.85  323.29 


#for 7284 rows
# 244.91    0.22  246.15 


#compose final wordlist (lemmas2+comp)

#combine  cols but ignore NAs
#(it puts put underscore)
feorp%>% unite("wordl", c(lemmas2, comp), na.rm = TRUE, remove = FALSE) %>%
  {.} -> feorp # This saves the changed data frame under the old name

#omit underscores
feorp$wordl<-gsub("_", '', feorp$wordl)

#final wordlist in feor
feorp2<-subset(feorp, select=c("munkk","feor.08","munkk5", "wordl"))


#check for doubles
length(feorp2$wordl[duplicated(feorp2$wordl)]) #9 cases, but 1 of them is not a real double, but now we drop them
a<-subset(feorp2, feorp2$wordl %in% feorp2$wordl[duplicated(feorp2$wordl)], select=)
a<-a[order(a$munkk),]


#Remove duplicates
feorp2<-feorp2[!duplicated(feorp2$wordl), ]


#save feor
write.table(feorp2, "feor_final.csv", sep = ";", row.names = F)

##############################################

#import files
mcfoglp2<-read.csv("fogl_final.csv", header=T, sep = ";")
feorp2<-read.csv("feor_final.csv", header=T, sep = ";")

#attach fogl4(from mcfogl2) to mcfoglp2, because that will be the key to the original file
mcfogl2<-read.csv("fogl2.csv", header=T, sep=";")
mcfoglp2$fogl4 <- mcfogl2$fogl4[match(mcfoglp2$sorsz, mcfogl2$sorsz)]


#how many equal rows between mcfoglp2  original fogl and feorp2 original munkk
common <- intersect(mcfoglp2$fogl, feorp2$munkk)  #out of 94.421, only 1423
result1<-subset(mcfoglp2, mcfoglp2$fogl %in% common, select=) # give you common rows in data frame 1  

#add code and original munkk 
result1$code <- feorp2$feor.08[match(result1$fogl, feorp2$munkk)]
result1$munkk <- feorp2$munkk[match(result1$fogl, feorp2$munkk)]

#is there ambivalence between the given code by g-code and the code from now?
# yes, 23 cases (ebbõl néhány másik fõcsoportba is kerül)
diff<-subset(result1, result1$foglkod!=result1$code, select=c("sorsz","fogl","foglkod","code", "munkk"))


#add extra, empty cols (for future merge)
extra1<-data.frame(matrix(nrow=1423, ncol=2))
colnames(extra1) <- c("wordl2", "inter1")

result1<-cbind(subset(result1, select=c(1:6)),extra1,subset(result1, select=c(7:9)))
result1$wordl2<-as.list(result1$wordl2); result1$inter1<-as.character(result1$inter1); 

#######################################

# 92.998 ommitted rows from mcfogl
tmp1<-subset(mcfoglp2, !mcfoglp2$fogl %in% common, select=) 

#how many equal rows between tmp1 fogl6 and feorp munkk5
common2 <- intersect(tmp1$fogl6, feorp2$munkk5)  #out of 92.998 only 836, 1932 (because of dubles?)
result2 <-subset(tmp1, tmp1$fogl6 %in% common2, select=) # give you common rows in data frame 1  

#add code and original fogl - xx db
result2$code <- feorp2$feor.08[match(result2$fogl6, feorp2$munkk5)]
result2$munkk <- feorp2$munkk[match(result2$fogl6, feorp2$munkk5)]

#is there ambivalence between the given code by g-code and the code from now?
# yes, 201 cases (ebbõl néhány másik fõcsoportba is kerül)
diff2<-subset(result2, result2$foglkod!=result2$code, select=c("sorsz","fogl","foglkod","code", "munkk"))

#add extra, empty cols (for future merge)
extra2<-data.frame(matrix(nrow=1932, ncol=2))
colnames(extra2) <- c("wordl2", "inter1")

result2<-cbind(subset(result2, select=c(1:6)),extra2,subset(result2, select=c(7:9)))
result2$wordl2<-as.list(result2$wordl2); result2$inter1<-as.character(result2$inter1); 


##################################xx

# 91.066 ommitted rows from tmp1 
tmp2<-subset(tmp1, !tmp1$fogl6 %in% common2, select=) 

#one row from tmp2 is contrasted to 7.285 rows in feorp2 wordl

#1 create a list from wordl
tmp2$wordl2<-as.list(lapply(strsplit(tmp2$wordl, ' ', fixed = TRUE), as.character))

#2 create a list from wordl in feorp2
feorp2$wordl2<-as.list(lapply(strsplit(feorp2$wordl, ' ', fixed = TRUE), as.character))

#how to intersect the list
tmp2$inters[1] <- Map(intersect, tmp2$wordl2[4], feorp2$wordl2[334])
#find a row, where "autó" is included ->340
#proba<-subset(feorp, feorp$lemmas2 %in% str_subset(feorp$lemmas2, "autó"), select=) #

#32 is in reality 33
tmp2$wordl2[32]

#create 100 cols
uj<-as.data.frame(matrix(NA, ncol=100, dimnames=list(NULL, paste0('inter',1:100))))




tmp2<-subset(tmp2, select=(1:7)) #
#tmp2$inters<-NA #blank column 
#bind 100 cols
tmp2<-cbind(tmp2, uj)


c<-1
system.time(  
for (f in 1:nrow(tmp2)) { #all rows of tmp2 fogl
  if (str_sub(as.character(f),nchar(as.character(f))-3,nchar(as.character(f)))=='0000') {print(f)} #showing loop counter
  if (length(tmp2$wordl2[[f]])>2) {   #longer than 2 words
  
    for (i in 1:nrow(feorp2)) {        #all rows of legitime munkk
      tmp<-Map(intersect, tmp2$wordl2[f], feorp2$wordl2[i]) #save intersected words (common in lookup fogl and legitime fogl=munkk)
      
      if (length(tmp2$wordl2[[f]])-1==length(feorp2$wordl2[[i]]) && #original fogl and feor munkk have word length difference only 1
        lengths(tmp)==length(tmp2$wordl2[[f]])-1) #matching max-1 word
      {#print(c(i,feorp2$wordl2[i]))
    
      tmp2[f,paste0("inter", c)] <-toString(feorp2$wordl2[i])   #row , col inter+counter
      c<-c+1                                             #col counter +1
      } 
}
c<-1  #get col counter again to 1
}})


#save tmp2: Saving on object in RData format
save(tmp2, file = "tmp2.RData")
# To load the data again
load("tmp2.RData")




#where there is only one match (only inter1): 3040 row...
result3<-subset(tmp2, (is.na(tmp2$inter1)==F & is.na(tmp2$inter2)==T), select=c(sorsz,fogl,foglkod,fogl6,wordl,wordl_noacc,wordl2, inter1))

#attach fogl4 lately
result3$fogl4 <- mcfogl2$fogl4[match(result3$sorsz, mcfoglp$sorsz)]


#add code and original fogl - xx db
result3$code <- feorp2$feor.08[match(result3$inter1, feorp2$wordl2)]
result3$munkk <- feorp2$munkk[match(result3$code, feorp2$feor.08)]

#select where codes do not match
diff3<-subset(result3, result3$foglkod!=result3$code, select=c(foglkod,wordl2,inter1,code))


#mistake: altató orvos asszisztens, without asszisztens it became to altató orvos


#f=33
#97.76   18.33  116.36 
#89.62   17.08  107.32 
#71.76   10.03   81.90 - ksh
# 3.42    0.02    3.46  - ksh, after some reduction

#f=100
#240.35   51.84  293.86 
# 12.21    0.07   12.32 - ksh, after some reduction

#f=94421
#stopped at 91084
#=Error in tmp2$wordl2[[f]] : subscript out of bounds
#Timing stopped at: 2.075e+04 3.96 2.083e+04
############################################################################
#only two long occupations, where one word can differ

# 88.043 ommitted rows from tmp2
tmp3<-subset(tmp2, !tmp2$wordl2 %in% result3$wordl2, select=) 

tmp3<-subset(tmp3, select=(1:7)) #
tmp3<-cbind(tmp3, uj)

#create non-significant word list, which will be omitted
omit<-c("alkalmazott", "vállalkozó", "betanított", "menedzser", "manager", "specialista", "tervezõ",
        "alsó", "belsõ", "vezetõ", "alkalmi", "egyéni", "hivatásos", "intézményi", "igazgató", 
        "helyettes", "segéd", "technikus", "nõ", "média", "szállodai", "termelési")


c<-1
system.time(  
  for (f in 1:nrow(tmp3)) { #all rows of tmp3 fogl
    if (str_sub(as.character(f),nchar(as.character(f))-3,nchar(as.character(f)))=='0000') {print(f)} #showing loop counter
    if (length(tmp3$wordl2[[f]])==2) {   #only 2 words
      
      for (i in 1:nrow(feorp2)) {        #all rows of legitime munkk
        tmp<-Map(intersect, tmp3$wordl2[f], feorp2$wordl2[i]) #save intersected words (common in lookup fogl and legitime fogl=munkk)
        
        if (length(tmp3$wordl2[[f]])==length(feorp2$wordl2[[i]]) && #original fogl and feor munkk have  both 2 words
            lengths(tmp)==length(tmp3$wordl2[[f]])-1 &&  #matching max-1 word
            (length((intersect(omit,feorp2$wordl2[[i]])))==0)==TRUE) #word do not included  in the followings
        {tmp3[f,paste0("inter", c)] <-toString(feorp2$wordl2[i])   #row , col inter+counter
        c<-c+1                                             #col counter +1
        } 
      }
      c<-1  #get col counter again to 1
    }})

#f=88043
#3326.59    5.26 3340.67 = 56 min
#3742.69   10.56 3804.07


#save tmp3: Saving on object in RData format
save(tmp3, file = "tmp3.RData")
# To load the data again
load("tmp3.RData")

#where there is only one match (only inter1): 1358 row...
result3<-subset(tmp3, (is.na(tmp3$inter1)==F & is.na(tmp3$inter2)==T), select=c(sorsz,fogl,foglkod,fogl6,wordl,wordl_noacc,wordl2, inter1, foglkod))


#attach fogl4 lately
result4$fogl4 <- mcfogl2$fogl4[match(result4$sorsz, mcfoglp$sorsz)]

#add code and original fogl - xx db
result4$code <- feorp2$feor.08[match(result4$inter1, feorp2$wordl2)]
result4$munkk <- feorp2$munkk[match(result4$code, feorp2$feor.08)]

#where fõcsoport do not agree
diffx<-subset(result4,str_sub(result4$foglkod,1,1)!=str_sub(result4$code,1,1))




############################################################################
#two word difference

# 86.685 ommitted rows from tmp2
tmp4<-subset(tmp3, !tmp3$wordl2 %in% result4$wordl2, select=) 

tmp4<-subset(tmp4, select=(1:7)) #
tmp4<-cbind(tmp4, uj)



c<-1
system.time(  
  for (f in 1:nrow(tmp4)) {             #all rows of tmp4 fogl
    if (str_sub(as.character(f),nchar(as.character(f))-3,nchar(as.character(f)))=='0000') {print(f)} #showing loop counter
    if (length(tmp4$wordl2[[f]])>3) {   #longer than 3 words
      
      for (i in 1:nrow(feorp2)) {        #all rows of legitime munkk
        tmp<-Map(intersect, tmp4$wordl2[f], feorp2$wordl2[i]) #save intersected words (common in lookup fogl and legitime fogl=munkk)
        
        if (length(tmp4$wordl2[[f]])-1==length(feorp2$wordl2[[i]]) && #original fogl and feor munkk have word length difference only 1
            lengths(tmp)==length(tmp4$wordl2[[f]])-2) #matching max-2 word
        {#print(c(i,feorp2$wordl2[i]))
          
          tmp4[f,paste0("inter", c)] <-toString(feorp2$wordl2[i])   #row , col inter+counter
          c<-c+1                                             #col counter +1
        } 
      }
      c<-1  #get col counter again to 1
}})

#f=88043
#16554.98     1.17 16576.13 

#save tmp4: Saving on object in RData format
save(tmp4, file = "tmp4.RData")
# To load the data again
load("tmp4.RData")

#where there is only one match (only inter1): 2895 row...
result5<-subset(tmp4, (is.na(tmp4$inter1)==F & is.na(tmp4$inter2)==T), select=c(sorsz,fogl,foglkod,fogl6,wordl,wordl_noacc,wordl2, inter1))

#attach fogl4 lately
result5$fogl4 <- mcfogl2$fogl4[match(result5$sorsz, mcfoglp$sorsz)]


#add code and original fogl - xx db
result5$code <- feorp2$feor.08[match(result5$inter1, feorp2$wordl2)]
result5$munkk <- feorp2$munkk[match(result5$code, feorp2$feor.08)]

#select where codes do not match: 1805
diff5<-subset(result5, result5$foglkod!=result5$code, select=c(foglkod,wordl2,inter1,code))


#mistake: everywhere where is alkalmazott, became alkalmazott fotográfus


#########################################################################
#merge results vertically
#for now we omit result4, because it is not so reliable
results<-rbind(result1, result2, result3, result4)


#join to mcfogl original
mcfogl_full<-read.csv("fogl_full.csv", header=T, sep=";")
final<-left_join(mcfogl_full, results, by = "fogl4")
final <-final[order(final$fogl4),]

#where is result by me
#82134                       (79318):-(((
myres<-subset(final, is.na(final$foglkod.y)==F, select=)

#how has gcode handeled the rows where i found something
count(myres$foglf)
#x  freq
#1  1 72089
#2  2  9267
#3  4   777
#4 NA     1


#foglf is wrongly coded, it is need to be recoded



#where is no result
noresult<-subset(final, is.na(final$foglkod.y)==T, select=)

#eg. no result for építész - only építészmérnök
#újságíró - only gazdasági, oópolitikai, stb újságíró
#közgazdász - onéy elemzõ közgazdász, stb




#how many result from g-code automatic coding foglf=1
#144.721
gcodeautm<-subset(final, final$foglf==1, select=)

#whether foglf=1 means only one code?
#in 304 cases no
gcode1<-subset(final, final$foglf==1 & (is.na(final$foglg1)==F | is.na(final$foglg2)==F), select=)



#where theres is result by me, where gcode did not find:0 
myres2<-subset(myres, myres$foglf!=1, select=)

myres3<-subset(myres2, select=c(1,2,3,4,5,6,7,20,21))

##where fõcsoport do not agree
diffxx<-subset(myres3,str_sub(myres3$foglkod,1,1)!=str_sub(myres3$code,1,1))

               