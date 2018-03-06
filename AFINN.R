

######### 3 class data sets ######

cname3 <-
  c(
    "Amazon.txt",
    "Comments_BBC.txt",
    "Comments_Digg.txt",
    "Comments_NYT.txt",
    "Comments_TED.txt",
    "Comments_YTB.txt",
    "Myspace.txt",
    "Reviews_II.txt",
    "RW.txt",
    "Tweets_DBT.txt",
    "Tweets_RND_I.txt",
    "Tweets_RND_II.txt",
    "Tweets_RND_III.txt",
    "Tweets_RND_IV.txt",
    "Tweets_SAN.txt",
    "Tweets_Semeval.txt"
  )




#######  2 class data sets ######
cname2 <- c("Reviews_I.txt", "Tweets_STF.txt")


# path <-
#   "/Volumes/Study/TUT/Phd/golden_standard/Sentences/Tweets_Semeval.txt"
# data <-  readLines(path)
###################### Afinn dictonary loading ############
dic_words <-
  readLines("/Volumes/Study/TUT/Phd/golden_standard/code/AFINN/AFINN.txt") 


####################  making new dictonary hash code ########

vecto1 <- c(1:length(dic_words))
vecto2 <- c(1:length(dic_words))

######################### score below 0 is negative and above 0 is positive. 0 means neutral.

for (i in 1:length(dic_words)) {
  hash <- unlist(strsplit(dic_words[i], "\t"))
  
  if (hash[2] > 0)
  {
    vecto1[i] <- hash[1]
    vecto2[i] <- 1
  }
  else if (hash[2] < 0) {
    vecto1[i] <- hash[1]
    vecto2[i] <- -1
    
  }
  else {
    vecto1[i] <- hash[1]
    vecto2[i] <- 0
    
  }
  
}

df <- data.frame(vecto1, vecto2)

####################  check polarity  ########


for (j in 1:length(cname3)) {
  filename <- cname3[j]
  
  ################## load the sentences to analyze sentiment as a text file #######
  path <-
    paste0("/Volumes/Study/TUT/Phd/golden_standard/Sentences/",
           filename)
  data <-  readLines(path)
  
  pred_AFINN_label <- length(data)
  
  for (i in 1:length(data)) {
    s <- data[i]
    s <- gsub('[[:punct:] ]+',' ',s)
    bag_of_words <-  strsplit(s, " ")
    Opinion_words <- unlist(bag_of_words)
    temp <- c()
    temp <- match(Opinion_words, df$vecto1)
    v <- which(!is.na(temp))
    temp <- sum(df[temp[v], 2])
    
    if (temp > 0)
      senti <- 1
    
    else if (temp < 0)
      senti <- -1
    
    else
      senti <- 0
    
    pred_AFINN_label[i] <- senti
  }
  
  # pathTowrite <-
  #   paste0("/Volumes/Study/TUT/Phd/golden_standard/code/AFINN/label2/",
  #          filename)
  # 
  # write(pred_AFINN_label, file = pathTowrite, sep = "\n")
  # 
}
