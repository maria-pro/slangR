#install.packages("rvest")
#install.packages("tidyverse")
install.packages("quanteda")
install.packages("tm")
options(scipen=999)
options(stringsAsFactors = FALSE)

#-----
#datascrapping
#library(rvest)
library(tidyverse)
#library(quanteda)
#library(tm)
library(tidytext)
#url<-"https://www.noslang.com/dictionary/"


#tidy_tweets_short<-tidy_tweets%>%select(text)
#tidy_tweets_short$id<-seq.int(nrow(tidy_tweets_short))

#id_v<-as_vector(tidy_tweets$id)
#corpus_tweet_txt<-corpus(tidy_tweets, docid_field=id_v)


#cleaning with custom dictionary

slang_dic<-read_csv("/Users/e5028514/Desktop/tutorials/data/slang_dict_test.csv", col_names=c("slang", "word"))

slang_dic<-data.frame(slang_dic, stringsAsFactors=FALSE)


#tweets<-read.csv("depression.csv", stringsAsFactors=FALSE)
tweets<-read.csv("/Users/e5028514/Desktop/tutorials/data/tweet_test.csv", stringsAsFactors=FALSE)

df1<-tweets[1:10,]%>%select(text,created_at)
dput(df1)

df_test<-tibble(x=1:5, text=c("7734 me after", 
                               "hello 0day",
                               "its 0vr",
                               "hi 12b",
                               "1337 10b"))

tweets_tidy<-df_test%>%
 # select(author_id, conversation_id, text)%>%
  unnest_tokens(token="tweets", output=word, input=text)

tweets_tidy$replace<-slang_dic$word[match(tweets_tidy$word, slang_dic$slang)]



tweets_tidy<-tweets_tidy%>%mutate(
  final=case_when(is.na(tweets_tidy$replace) ~ tweets_tidy$word,
                  TRUE~tweets_tidy$replace)
  )


df1 <- data.frame(
  number = round(runif(n = 5, 1, 9),0),
  text = c("perfect day 1daful",
               "bread $$",
               "you look $xy",
               "my darling *4u",
               "thank you"),
  stringsAsFactors=FALSE)

df2<-data.frame(
  slang=c("$xy",
          "1daful",
          "*4u",
          "$$"
          ),
  word=c("sexy",
         "wonderful",
         "kiss for you",
         "money"),
  stringsAsFactors=FALSE)  

str_replace_all(df1$text, fixed(setNames(df2$word,df2$slang)))




slang_dic<-data.frame(number = round(runif(n = 10, 1, 9),0),
                       slang=rep("10tacle", nrow(tweet_test)),
                      word=rep("test", nrow(tweet_test)),
                      stringsAsFactors=FALSE)

tweet_test<-tweet_test%>% mutate(
  rpl2<-str_replace_all(tweet_test$rpl, fixed(setNames(slang_dic$word,slang_dic$slang)))
)



tweet_test%>% mutate(
  rpl3=str_replace_all(tweet_test$rpl, setNames(fixed(slang_dic$slang), fixed(slang_dic$word)))
)


i


df1 <- data.frame(
  datalist = c("wiki/anarchist_schools_of_thought can differ fundamentally supporting anything from extreme wiki/individualism to complete wiki/collectivism",
               "strains of anarchism have often been divided into the categories of wiki/social_anarchism and wiki/individualist_anarchism or similar dual classifications",
               "the word is composed from the word wiki/anarchy and the suffix wiki/-ism themselves derived respectively from the greek i.e",
               "anarchy from anarchos meaning one without rulers from the wiki/privative prefix wiki/privative_alpha an- i.e",
               "authority sovereignty realm magistracy and the suffix or -ismos -isma from the verbal wiki/infinitive suffix -izein",
               "the first known use of this word was in 1539"),
  words = c("anarchist_schools_of_thought  individualism  collectivism", "social_anarchism  individualist_anarchism",
            "anarchy  -ism", "privative  privative_alpha", "infinitive", ""),
  
  stringsAsFactors=FALSE)

df2 <- data.frame(
  vocabword = c("anarchist_schools_of_thought", "individualism","collectivism" , "1965-66_nhl_season_by_team","social_anarchism","individualist_anarchism",                
                "anarchy","-ism","privative","privative_alpha", "1310_the_ticket",  "infinitive"),
  token = c("Anarchist_schools_of_thought" ,"Individualism", "Collectivism",  "1965-66_NHL_season_by_team", "Social_anarchism", "Individualist_anarchism" ,"Anarchy",
            "-ism", "Privative" ,"Alpha_privative", "KTCK_(AM)" ,"Infinitive"), 
  stringsAsFactors = F)                  

str_replace_all(df1$datalist, setNames(df2$vocabword, df2$token))
#correct
 

corpus_tweet_txt<-corpus(tidy_tweets$text)


corp_tagged <- corpus(c("##INTRO This is the introduction.
                         ##DOC1 This is the first document.  Second sentence in Doc 1.
                         ##DOC3 Third document starts here.  End of third document.",
                        "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))



corp_sect <- corpus_segment(corp_tagged, pattern = "##*")

cbind(docvars(corp_sect), text = as.character(corp_sect))





#cleaning
remove_reg <- "&amp;|&lt;|&gt;|\\d+\\w*\\d*|#\\w+|[^\x01-\x7F]|[[:punct:]]|https\\S*"
# &amp = @
# &lt;= <
# &gt; >


#removing retweets characters
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) 

#custom dictionary
text<-data.frame(tidy_tweets$text)
corpus_text<-corpus(text)
ndoc(corpus_text)

slang_dic<-corpus(slang_dic)
slang_tokens <- tokens(slang_dic)

dic_test<-tidy_tweets%>%select(conversation_id, text)

DF1$Text1 <- sapply(strsplit(DF1$Text, '\\s+'), function(x) 
  paste0(Filter(english.words, x), collapse = ' '))

lexicon2 <- lexicon %>% 
  select(c("WORD","POLARITY")) %>% 
  rename('word'="WORD",'value'="POLARITY")


tidy_tweets %>%
  mutate(linenumber = row_number()) %>% #line number for later sentence grouping 
  unnest_tokens(text, format="tweets"', to_lower=TRUE) %>% #tokenization - sentence to words
  inner_join(lexicon2) %>% # inner join with our lexicon to get the polarity score
  group_by(linenumber) %>% #group by for sentence polarity
  summarise(sentiment = sum(value)) %>% # final sentence polarity from words
  left_join(
    sent %>%
      mutate(linenumber = row_number()) #get the actual text next to the sentiment value
  ) %>% write.csv("sentiment_output.csv",row.names = FALSE)
