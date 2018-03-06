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

attentiveness = 0
fatigue = 0
fear = 0
guilt = 0
hostility = 0
joviality = 0
sadness = 0
assurance = 0
serene = 0
shyness = 0
surprise = 0

#########  baseline 


Attentiveness =0.0008997
Joviality = 0.0182421
Assurance = 0.0036012
Serene = 0.0022914
Surprise = 0.0084612

Fatigue = 0.0240757
Fear = 0.0063791
Guilt = 0.0021756
Hostility = 0.0018225
Sadness = 0.0086279
Shyness = 0.0007608

#row_names <- c("attentiveness", "joviality", "assurance", "serene", "surprise","fatigue", "fear" ,"guilt" , "hostility" , "sadness" , "shyness") 
#sentiment_df <- data.frame(beta_valu,row.names = row_names)

##################



pos <- 0
neg <- 0
neu <- 0

for(k in 1:length(cname3)) {
  
  filename <- cname3[k]
  #path <-"/Volumes/Study/TUT/Phd/golden_standard/Sentences/Tweets_Semeval.txt"
  path <- paste0("/Volumes/Study/TUT/Phd/golden_standard/Sentences/",filename)
  data <-  readLines(path)


pred_panas_label <-  length(data)

for (i in 1:length(data)) {
  s <- data[i]
  s <- gsub('[[:punct:] ]+', ' ', s) ## remove punctuations from words
  bag_of_words <-  strsplit(s, " ")
  Opinion_words <- unlist(bag_of_words)
  
  attentiveness = 0
  fatigue = 0
  fear = 0
  guilt = 0
  hostility = 0
  joviality = 0
  sadness = 0
  assurance = 0
  serene = 0
  shyness = 0
  surprise = 0
  # 
  for (j in 1:length(Opinion_words)) {
    if (is.element(Opinion_words[j],
                   c('alert', 'attentive', 'concentrating', 'determined'))) {
      attentiveness <- attentiveness + 1
    }
    else if (is.element(Opinion_words[j],
                        c('sleepy', 'tired', 'sluggish', 'drowsy', 'drowsi'))) {
      fatigue <- fatigue + 1
    }
    else if (is.element(
      Opinion_words[j],
      c(
        'afraid',
        'scare',
        'scari',
        'scary',
        'jittery',
        'shaki',
        'shaky',
        'nervous'
      )
    )) {
      fear <- fear + 1
    }
    else if (is.element(
      Opinion_words[j],
      c(
        'guilt',
        'ashame',
        'shame',
        'blameworthy',
        'dissatisfi',
        'disgust'
      )
    )) {
      guilt <- guilt + 1
    }
    
    else if (is.element(
      Opinion_words[j],
      c(
        'angry',
        'anger',
        'angri',
        'hostile',
        'irritable',
        'scornful',
        'disgust',
        'loathing'
      )
    )) {
      hostility <-  hostility + 1
    }
    
    else if (is.element(
      Opinion_words[j],
      c(
        'happy',
        'joyful',
        'delighted',
        'cheerful',
        'excited',
        'enthusiastic',
        'lively',
        'energetic'
      )
    )) {
      joviality <- joviality + 1
    }
    else if (is.element(Opinion_words[j], c('sad', 'downhearted', 'alone', 'lonely','blue'))) {
      sadness <- sadness + 1
    }
    
    else if (is.element(Opinion_words[j], c('proud', 'strong', 'dare', 'daring'))) {
      assurance <- assurance + 1
    }
    
    else if (is.element(Opinion_words[j], c('calm', 'relax'))) {
      serene <-  serene + 1
    }
    
    else if (is.element(Opinion_words[j], c('shy', 'bashful', 'sheepish', 'timid'))) {
      shyness <-  shyness + 1
    }
    
    else if (is.element(Opinion_words[j],
                        c('amazing', 'surpris', 'astonish', 'amazed', 'amaze'))) {
      surprise <-  surprise + 1
    }
    
    
  }  # end of sentence
  
  #beta_valu  <- c(attentiveness, joviality, assurance, serene, surprise,fatigue, fear , guilt , hostility , sadness , shyness)    
  
  
  #sentiment_df <-  cbind(sentiment_df,beta_valu)
  
   pos  <- sum(c(attentiveness, joviality, assurance, serene, surprise))
   neg  <- sum(c(fatigue, fear , guilt , hostility , sadness , shyness))
  
   
   if (pos > neg)
     senti <- 1
   
   else if (neg > pos)
     senti <-  -1
   
   else
     senti <-  0


  
  
   pred_panas_label[i] <- senti
  
}  

 pathTowrite <-
   paste0("/Volumes/Study/TUT/Phd/golden_standard/code/panas_t/label/",
          filename)
 
 write(pred_panas_label, file = pathTowrite, sep = "\n")


}


