

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

######## load dictionary #####

emolex <-
  readLines("/Volumes/Study/TUT/Phd/golden_standard/code/Emolex/emolex.txt")

vector1 <- c(1:length(emolex))
vector2 <- c(1:length(emolex))



for (i in 1:length(emolex)) {
  hash <-  unlist(strsplit(emolex[i], " "))
  
  pos <- hash[c(2, 8, 11)]
  key <- unlist(strsplit(pos, ":"))
  pos_val <- length(which(key == "1"))
  
  neg <-  hash[c(3, 4, 6, 7, 9)]
  key <- unlist(strsplit(neg, ":"))
  neg_val <- length(which(key == "1"))
  
  
  neu <-  hash[c(5, 10)]
  key <- unlist(strsplit(neu, ":"))
  neu_val <- length(which(key == "1"))
  
  
  
  if ((pos_val > neg_val) & (pos_val > neu_val)) {
    vector1[i] <- hash[1]
    vector2[i] <- 1
  }
  else if ((neg_val > pos_val) & (neg_val > neu_val)) {
    vector1[i] <- hash[1]
    vector2[i] <- -1
    
  }
  else {
    vector1[i] <- hash[1]
    vector2[i] <- 0
  }
  
  
  
}

#df <- data.frame(vector1,vector2)


####################  check polarity  ########


for (j in 1:length(cname3)) {
  filename <- cname3[j]
  
  path <-
    paste0("/Volumes/Study/TUT/Phd/golden_standard/Sentences/",
           filename)
  data <-  readLines(path)
  
  pred_Emolex_label <- length(data)
  for (i in 1:length(data)) {
    s <- data[i]
    s <- gsub('[[:punct:] ]+',' ',s)
    bag_of_words <-  strsplit(s, " ")
    Opinion_words <- unlist(bag_of_words)
    temp <- c()
    temp <- match(Opinion_words, vector1)
    v <- which(!is.na(temp))
    if (length(v) == 0)
    {
      temp  <- 0
      senti <- 0
    }
    else
    {
      temp <- sum(as.numeric(vector2[temp[v]]))
      
      if (temp > 0)
        senti <- 1
      
      else if (temp < 0)
        senti <- -1
      
      else
        senti <- 0
      
    }
    
    
    pred_Emolex_label[i] <- senti
  }
  
  pathTowrite <-
    paste0("/Volumes/Study/TUT/Phd/golden_standard/code/Emolex/label/",
           filename)
  
  write(pred_Emolex_label, file = pathTowrite, sep = "\n")
  
  
}
