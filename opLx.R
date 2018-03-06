

#####  library calling ########
# library("openNLP")
# library("NLP")
# library("openNLPmodels.en")


######## load the data #####
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

######## load dictionary #####

positive_words <-
  scan(
    "/Volumes/Study/TUT/Phd/golden_standard/code/Opinion_Lexicon/opdic/positive-words.txt",
    what = "character"
  )
negative_words <-
  scan(
    "/Volumes/Study/TUT/Phd/golden_standard/code/Opinion_Lexicon/opdic/negative-words.txt",
    what = "character"
  )



for (j in 1:length(cname3)) {
  filename <- cname3[j]
  
  path <-
    paste0("/Volumes/Study/TUT/Phd/golden_standard/Sentences/",
           filename)
  data <-  readLines(path)
  
  
  pred_OPLX_label <- length(data)
  for (i in 1:length(data)) {
    s <- data[i]
    s <- gsub('[[:punct:] ]+',' ',s)
    bag_of_words <-  strsplit(s, " ")
    Opinion_words <- unlist(bag_of_words)
    
    
    
    #############  Predicting the orientations of opinion sentences (positive / negative / neutral)  #########
    
    
    Num_Pos_word <- sum(!is.na(match(Opinion_words, positive_words)))
    Num_Neg_word <- sum(!is.na(match(Opinion_words, negative_words)))
    
    if (Num_Pos_word > Num_Neg_word)
    {
      senti <- 1
    }
    else if (Num_Pos_word < Num_Neg_word)
    {
      senti <- -1
    }
    else
      senti <-  0
    pred_OPLX_label[i] <- senti
    
  }
  
  ############  write the label in working directory ####
  
  pathTowrite <-
    paste0(
      "/Volumes/Study/TUT/Phd/golden_standard/code/Opinion_Lexicon/label/",
      filename
    )
  
  # write(pred_OPLX_label, file = pathTowrite,
  #      ncolumns = if(is.character(x)) 1 else 5,
  #      append = FALSE, sep = " ")
  write(pred_OPLX_label, file = pathTowrite, sep = "\n")
  
  ######### test ######
  
 # head(pred_OPLX_label)
  
}
