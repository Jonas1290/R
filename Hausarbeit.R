library(lsa)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(stringr)
library(readr)
library(janeaustenr)
library(gridExtra)
library(grid)
library(lattice)

#Seitenzahl
SP7S1<-c(149,236,334)
SBertelsmann<-c(174,166,144)
SG<-c(SP7S1,SBertelsmann)

#Statistik Seitenzahl
#Gesamt
median(SG)
mean(SG)
sd(SG)

#ProSiebenSat1
median(SP7S1)
mean(SP7S1)
sd(SP7S1)

#Bertelsmann
median(SBertelsmann)
mean(SBertelsmann)
sd(SBertelsmann)


#Einlessen und Vorbereitung der Datein
P72008 <- readLines("01_Pro_Sieben_2008.txt", encoding = "UTF-8")
P72008 <- P72008 %>% removePunctuation() %>% removeNumbers()
P72008_df <- tibble(lines = 1:20133, text = P72008)
P72008_words <- P72008_df %>% unnest_tokens(word, text)

P72011 <- readLines("02_Pro_Sieben_2011.txt", encoding = "UTF-8")
P72011 <- P72011 %>% removePunctuation() %>% removeNumbers()
P72011_df <- tibble(lines = 1:22559, text = P72011)
P72011_words <- P72011_df %>% unnest_tokens(word, text)

P72015 <- readLines("03_Pro_Sieben_2015.txt", encoding = "UTF-8")
P72015 <- P72015 %>% removePunctuation() %>% removeNumbers()
P72015_df <- tibble(lines = 1:38606, text = P72015)
P72015_words <- P72015_df %>% unnest_tokens(word, text)

BM2008 <- readLines("04_Bertelsmann_2008.txt", encoding = "UTF-8")
BM2008 <- BM2008  %>% removePunctuation() %>% removeNumbers()
BM2008_df <- tibble(lines = 1:23363, text = BM2008)
BM2008_words <- BM2008_df  %>% unnest_tokens(word, text)

BM2011 <- readLines("05_Bertelsmann_2011.txt", encoding = "UTF-8")
BM2011 <- BM2011  %>% removePunctuation() %>% removeNumbers()
BM2011_df <- tibble(lines = 1:20501, text = BM2011)
BM2011_words <- BM2011_df  %>% unnest_tokens(word, text)

BM2015 <- readLines("06_Bertelsmann_2015.txt", encoding = "UTF-8")
BM2015 <- BM2015  %>% removePunctuation() %>% removeNumbers()
BM2015_df <- tibble(lines = 1:21134, text = BM2015)
BM2015_words <- BM2015_df  %>% unnest_tokens(word, text)

neg_df <- read_tsv("SentiWS_v2.0_Negative.txt", col_names = FALSE)
names(neg_df) <- c("Wort_POS", "Wert", "Inflektionen")
neg_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> neg_df
pos_df <- read_tsv("SentiWS_v2.0_Positive.txt", col_names = FALSE)
names(pos_df) <- c("Wort_POS", "Wert", "Inflektionen")
pos_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> pos_df
bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos") -> sentiment_df
sentiment_df %>% select(neg_pos, Wort, Wert, Inflektionen, -Wort_POS) -> sentiment_df

sentiment_df %>%
  rename(stam=Wort)->sentiment_df

# EinfÃ¼gen des Titels in den Word Data Fram
BM2008_words$Titel<-rep("Bertelsmann 2008",NROW(BM2008_words))
BM2011_words$Titel<-rep("Bertelsmann 2011",NROW(BM2011_words))
BM2015_words$Titel<-rep("Bertelsmann 2015",NROW(BM2015_words))
P72008_words$Titel<-rep("ProSiebenSat1 2008",NROW(P72008_words))
P72011_words$Titel<-rep("ProSiebenSat1 2011",NROW(P72011_words))
P72015_words$Titel<-rep("ProSiebenSat1 2015",NROW(P72015_words))

#Erstellen neur DataFrams pro Unternehmen pro Jahr
BMGesamt_Words<-dplyr::bind_rows(BM2008_words,BM2015_words,BM2011_words)
P7Gesamt_Words<-dplyr::bind_rows(P72008_words,P72011_words,P72015_words)
Words2008<-dplyr::bind_rows(P72008_words,BM2008_words)
Words2011<-dplyr::bind_rows(P72011_words,BM2011_words)
Words2015<-dplyr::bind_rows(P72015_words,BM2015_words)
WordsGesamt<-dplyr::bind_rows(BMGesamt_Words,P7Gesamt_Words)

#Stopwords und Stamm
data(stopwords_de)
data(stopwords_en)
stopwordsLsa <- stopwords_de
stopwordsTm <- stopwords("german")

#StÃ¤ndige Erweiterung der StopWords
Stopword_Eigene<-c("wesentlichen",	"prosiebensat",	"media",	"hÃ¶he",	"se",	"co",	"rahmen","geschäftsbericht",	"dr",	"geschÃ¤ftsbericht",	"summe",	"zeitwert",
                   "geschÃ¤fts",	"je",	"entspricht",	"juni",	"folgt",	"zeigt",	"folgenden",	"position",	"dargestellt",	"november",	
                   "maÃŸnahmen",	"norge",	"kohlberg",	"salzmann",	"entsprechend",	"anzahl",	"group",	"unternehmen",	"jahr",	"vj",	
                   "erfasst",	"seite",	"zudem",	"insgesamt",	"neben",	"abs",	"sbs",	"mÃ¤rz",	"ebenfalls",	"neuen",	"derzeit",	
                   "thomas",	"ersten",	"peter",	"christmann",	"neue",	"jeweils",	"anstieg",	"johannes",	"qq",	"eng",	"ias",	
                   "bertelsmann",	"www",	"sonstige",	"arvato",	"vorstands",	"anhang",	"hinaus",	"erfolgt",	"januar",	"prosieben",	
                   "juli",	"teil",	"april",	"november",	"ver",	"roberts",	"chancen",	"loan",	"bereits",	"juni",	"folgt",	"zeigt",	
                   "folgenden",	"position",	"dargestellt",	"november",	"sowie",	"de",	"dezember",	"rtl",	"davon",	"rahmen",	"basis",	
                   "house",	"sat",	"zusammengefasster",	"mai",	"deutlich",	"neue",	"raum",	"kravis",	"umfasst",	"folgenden",	
                   "assozierten",	"neuen",	"derzeit",	"thomas",	"ersten",	"peter",	"christmann",	"neue",	"prozent",	"ag",	
                   "geschÃ¤ftsjahr",	"insbesondere",	"aufgrund",	"wesentlicehn",	"mrd",	"random",	"informationen",	"dabei",	
                   "zusammenhang",	"betrÃ¤gt",	"maÃŸnahmen",	"norge",	"kohlberg",	"salzmann",	"entsprechend",	"anzahl",	
                   "november",	"ver",	"roberts",	"chancen",	"loan",	"bereits",	"mio",	"gmbh",	"vorjahr",	"rund",	"jahren",	
                   "abb",	"tsd",	"angaben",	"Ã¤nderungen",	"enthalten",	"juni",	"jeweils",	"anstieg",	"johannes",	
                   "qq",	"eng",	"ias",	"neue",	"raum",	"kravis",	"umfasst",	"folgenden",	"assozierten",	"euro",	"ias",	
                   "gruner",	"ifrs",	"direct",	"kgaa",	"jahre",	"neue",	"stand",	"bertelsmannkonzern",	"mohn",	"sa",	"financial",	
                   "bmg",	"ltd",	"kg",	"ermittelt",	"bereich",	"deren",	"annahmen",	"beim",	"bertelsmannkonzerns",	"penguin",	
                   "bestehen",	"segmentberichterstattung",	"aufstellung",	"drei",	"jeweils",	"inc",	"anteil",	"neuen",	"bereits",	
                   "dar",	"entsprechend",	"folgenden",	"umfassen",	"weiterhin",	"zugrunde",	"finance",	"lag",	"berÃ¼cksichtigt",	
                   "folgt",	"tabelle",	"Ã¼brigen",	"ansatz",	"llc",	"sowohl",	"wesentliche",	"anzahl",	"jeweiligen",	"umfasst",	
                   "anwendung",	"effekte",	"finden",	"folgende",	"vorgenommen",	"zwei",	"anstieg",	"fÃ¼nf",	"neuer",	"printers",	
                   "verhÃ¤ltnis",	"denen",	"mbh",	"Ã¤hnliche",	"sinne",	"textziffer",	"vier",	"worden",	"hinsichtlich",	"titel","corporate","konzernabschluss","konzernlagebericht","eur","höhe","Gütersloh")

#Gesamt
wordsNoUse <- tibble(word = unique(c(stopwordsLsa, stopwordsTm,stopwords_de,stopwords_en,Stopword_Eigene)))
WordsGesamt_Stop <- WordsGesamt %>% anti_join(wordsNoUse)
WordsGesamt_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> WordsGesamt_Stop_Stam

#2008
Words2008_Stop <- Words2008 %>% anti_join(wordsNoUse)
Words2008_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> Words2008_Stop_Stam

#2011
Words2011_Stop <- Words2011 %>% anti_join(wordsNoUse)
Words2011_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> Words2011_Stop_Stam

#2015
Words2015_Stop <- Words2015 %>% anti_join(wordsNoUse)
Words2015_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> Words2015_Stop_Stam

#ProSieben
P7Gesamt_Words_Stop <- P7Gesamt_Words %>% anti_join(wordsNoUse)
P7Gesamt_Words_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> P7Gesamt_Words_Stop_Stam


P72008_words_Stop <- P72008_words %>% anti_join(wordsNoUse)
P72008_words_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> P72008_words_Stop_Stam


P72011_words_Stop <- P72011_words %>% anti_join(wordsNoUse)
P72011_words_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> P72011_words_Stop_Stam

P72015_words_Stop <- P72015_words %>% anti_join(wordsNoUse)
P72015_words_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> P72015_words_Stop_Stam


#Bertelsmann
BMGesamt_Words_Stop <- BMGesamt_Words %>% anti_join(wordsNoUse)
BMGesamt_Words_Stop %>% mutate(stam=wordStem(.$word,language = "german"))-> BMGesamt_Words_Stop_Stam


BM2008_words_Stop <- BM2008_words %>% anti_join(wordsNoUse)
BM2008_words_Stop  %>% mutate(stam=wordStem(.$word,language = "german"))-> BM2008_words_Stop_Stam



BM2011_words_Stop <- BM2011_words %>% anti_join(wordsNoUse)
BM2011_words_Stop  %>% mutate(stam=wordStem(.$word,language = "german"))-> BM2011_words_Stop_Stam


BM2015_words_Stop <- BM2015_words %>% anti_join(wordsNoUse)
BM2015_words_Stop  %>% mutate(stam=wordStem(.$word,language = "german"))-> BM2015_words_Stop_Stam

#Sentiment Analysis 
#Gewichte Analyse
#Gesamt

WordsGesamt_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  WordsGesamt_Stop_Stam

WordsGesamt_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> WordsGesamt_Stop_Stam_summe

#2008
Words2008_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  Words2008_Stop_Stam

Words2008_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> Words2008_Stop_Stam_sentiment_summe

#2011
Words2011_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  Words2011_Stop_Stam

Words2011_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> Words2011_Stop_Stam_sentiment_summe

#2015
Words2015_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  Words2015_Stop_Stam

Words2015_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> Words2015_Stop_Stam_sentiment_summe

#ProSieben
P7Gesamt_Words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  P7Gesamt_Words_Stop_Stam

P7Gesamt_Words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> P7Gesamt_Words_Stop_Stam_sentiment_summe

P72008_words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  P72008_words_Stop_Stam

P72008_words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> P72008_words_Stop_Stam_sentiment_summe

P72011_words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  P72011_words_Stop_Stam

P72011_words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> P72011_words_Stop_Stam_sentiment_summe

P72015_words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  P72015_words_Stop_Stam

P72015_words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> P72015_words_Stop_Stam_sentiment_summe

#Bertelsmann
BMGesamt_Words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  BMGesamt_Words_Stop_Stam

BMGesamt_Words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> BMGesamt_Words_Stop_Stam_sentiment_summe

BM2008_words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  BM2008_words_Stop_Stam

BM2008_words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> BM2008_words_Stop_Stam_sentiment_summe

BM2011_words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  BM2011_words_Stop_Stam

BM2011_words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> BM2011_words_Stop_Stam_sentiment_summe

BM2015_words_Stop_Stam %>% 
  left_join(sentiment_df,by="stam") ->
  BM2015_words_Stop_Stam

BM2015_words_Stop_Stam %>% 
  filter(!is.na(Wert)) %>% 
  summarise(Sentimentwert = sum(Wert, na.rm = TRUE)) -> BM2015_words_Stop_Stam_sentiment_summe

#Statistik Anzahl Wörter Gesamt
#ProSiebnSat1
P7W<-c(NROW(P72008_words),NROW(P72011_words),NROW(P72015_words))
median(P7W)
mean(P7W)
sd(P7W)

#Bertelsmann
BMW<-c(NROW(BM2008_words),NROW(BM2011_words),NROW(BM2015_words))
median(BMW)
mean(BMW)
sd(BMW)

#Gesamt
WG<-c(P7W,BMW)
median(WG)
mean(WG)
sd(WG)

#Wörter Top 10 mit und ohne Stamm
##Gesamt
###Gesamt/Gesamt
WordsGesamt_Stop  %>% count(word, sort = TRUE)->WordsGesamt_Stop_count

WordsGesamt_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill="dark orange")+
  labs(title ="Ohne Trunkierung")+
  xlab("Wörter")+
  coord_flip()+ylim(0,1600)-> WordsGesamt_Stop_count_ggplot_10

WordsGesamt_Stop_Stam  %>% count(stam, sort = TRUE)->WordsGesamt_Stop_Stam_count

WordsGesamt_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange"))+
  labs(title ="Mit Trunkierung")+ xlab("Wortstamm")+ylim(0,1300)+coord_flip()-> WordsGesamt_Stop_Stam_count_ggplot_10_clean

WordsGesamt_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange"," dark green","dark orange","dark red","dark red","dark green","dark orange","dark green","dark green","dark red"))+
  labs(title ="Mit Trunkierung")+ xlab("Wortstamm")+coord_flip()+ylim(0,1600)-> WordsGesamt_Stop_Stam_count_ggplot_10

grid.arrange(WordsGesamt_Stop_count_ggplot_10,WordsGesamt_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung über alle Geschäftsberichte in allen Jahren",gp=gpar(fontsize=22,front=3)))

###Gesamt/2008
Words2008_Stop  %>% count(word, sort = TRUE)->Words2008_Stop_count

Words2008_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill="dark orange")+
  labs(title ="2008 ohne Trunkierung")+xlab("Wörter")+
  coord_flip()-> Words2008_Stop_count_ggplot_10

Words2008_Stop_Stam  %>% count(stam, sort = TRUE)->Words2008_Stop_Stam_count

Words2008_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange","dark green","dark red","dark red","dark orange","dark orange","dark green","dark red","dark red","dark green"))+
  labs(title ="2008 mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()-> Words2008_Stop_Stam_count_ggplot_10

Words2008_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange"))+
  labs(title ="2008 mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,650)-> Words2008_Stop_Stam_count_ggplot_10_clean


grid.arrange(Words2008_Stop_count_ggplot_10,Words2008_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung über alle Geschäftsberichte für das Jahr 2008",gp=gpar(fontsize=20,front=3)))

###Gesamt/2011
###Gesamt/2011
Words2011_Stop  %>% count(word, sort = TRUE)->Words2011_Stop_count

Words2011_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill="dark orange")+
  labs(title ="2011 ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,450)-> Words2011_Stop_count_ggplot_10

Words2011_Stop_Stam  %>% count(stam, sort = TRUE)->Words2011_Stop_Stam_count

Words2011_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange","dark green","dark orange","dark red","dark red","dark red","dark green","dark orange","dark green","dark green"))+
  labs(title ="2011 mit Trunkierung")+ xlab("Wörter")+ylim(0,450)+
  coord_flip()-> Words2011_Stop_Stam_count_ggplot_10

Words2011_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange"))+
  labs(title ="2011 mit Trunkierung")+ xlab("Wörter")+
  coord_flip()+ylim(0,650)-> Words2011_Stop_Stam_count_ggplot_10_clean

grid.arrange(Words2011_Stop_count_ggplot_10,Words2011_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung über alle Geschäftsberichte für das Jahr 2011",gp=gpar(fontsize=20,front=3)))

###Gesamt/2015
Words2015_Stop  %>% count(word, sort = TRUE)->Words2015_Stop_count

Words2015_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill="dark orange")+
  labs(title ="2015 ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,650)-> Words2015_Stop_count_ggplot_10

Words2015_Stop_Stam  %>% count(stam, sort = TRUE)->Words2015_Stop_Stam_count

Words2015_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange","dark orange","dark green","dark green","dark green","dark red","dark green","dark red","dark red","dark red"))+
  labs(title ="2015 mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,650)-> Words2015_Stop_Stam_count_ggplot_10

Words2015_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark orange"))+
  labs(title ="2015 mit Trunkierung")+ xlab("Wortstamm")+ylim(0,700)+
  coord_flip()+ylim(0,650)-> Words2015_Stop_Stam_count_ggplot_10_clean

grid.arrange(Words2015_Stop_count_ggplot_10,Words2015_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung über alle Geschäftsberichte für das Jahr 2015",gp=gpar(fontsize=20,front=3)))

###ProSieben/Gesamt
P7Gesamt_Words_Stop  %>% count(word, sort = TRUE)->P7Gesamt_Words_Stop_count

P7Gesamt_Words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="Gesamtübersicht ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,750)-> P7Gesamt_Words_Stop_countt_ggplot_10

P7Gesamt_Words_Stop_Stam  %>% count(stam, sort = TRUE)->P7Gesamt_Words_Stop_Stam_count

P7Gesamt_Words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark green","dark red","dark green","#e1153e","dark green","dark green","dark green","dark red","dark green","dark red"))+
  labs(title ="Gesamtübersicht mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,750)-> P7Gesamt_Words_Stop_Stam_count_ggplot_10

P7Gesamt_Words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e"))+
  labs(title ="Gesamtübersicht mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,750)-> P7Gesamt_Words_Stop_Stam_count_ggplot_10_clean

P7Gesamt_Words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e"))+
  labs(title ="Gesamtübersicht mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,750)-> P7Gesamt_Words_Stop_Stam_count_ggplot_10_clean2

grid.arrange(P7Gesamt_Words_Stop_countt_ggplot_10,P7Gesamt_Words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der ProSiebenSat1 SE über alle Geschäftsberichte",gp=gpar(fontsize=20,front=3)))

##ProSieben/2008
P72008_words_Stop %>% count(word, sort = TRUE)->P72008_words_Stop_count

P72008_words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="2008 ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,150)-> P72008_words_Stop_countt_ggplot_10

P72008_words_Stop_Stam  %>% count(stam, sort = TRUE)->P72008_words_Stop_Stam_count

P72008_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="2008 mit Trunkierung")+ xlab("Wörter")+
  geom_col(fill=c("dark green","dark green","dark red","dark red","dark green","dark green","#e1153e","dark red","dark green","dark green"))+
  coord_flip()+ylim(0,150)-> P72008_words_Stop_Stam_count_ggplot_10

P72008_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="2008 mit Trunkierung")+ xlab("Wortstamm")+
  geom_col(fill=c("#e1153e"))+
  coord_flip()+ylim(0,450)-> P72008_words_Stop_Stam_count_ggplot_10_clean

P72008_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="2008 mit Trunkierung")+ xlab("Wortstamm")+
  geom_col(fill=c("#e1153e"))+
  coord_flip()+ylim(0,300)-> P72008_words_Stop_Stam_count_ggplot_10_clean2

grid.arrange(P72008_words_Stop_countt_ggplot_10,P72008_words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der ProSiebenSat1 SE für das Jahr 2008",gp=gpar(fontsize=20,front=3)))

##ProSieben/2011
P72011_words_Stop %>% count(word, sort = TRUE)->P72011_words_Stop_count

P72011_words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="2011 ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,250)-> P72011_words_Stop_countt_ggplot_10

P72011_words_Stop_Stam  %>% count(stam, sort = TRUE)->P72011_words_Stop_Stam_count

P72011_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("dark green","dark green","#e1153e","dark green","dark red","dark green","dark green","dark green","dark green","dark green"))+
  labs(title ="2011 mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,250)-> P72011_words_Stop_Stam_count_ggplot_10

P72011_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e"))+xlab("Wörter")+
  labs(title ="2011 mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,450)-> P72011_words_Stop_Stam_count_ggplot_10_clean

P72011_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e"))+xlab("Wörter")+
  labs(title ="2011 mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,250)-> P72011_words_Stop_Stam_count_ggplot_10_clean2


grid.arrange(P72011_words_Stop_countt_ggplot_10,P72011_words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der ProSiebenSat1 SE für das Jahr 2011",gp=gpar(fontsize=20,front=3)))

##ProSieben/2015
P72015_words_Stop %>% count(word, sort = TRUE)->P72015_words_Stop_count

P72015_words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#e1153e"))+
  labs(title ="2015  ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,450)-> P72015_words_Stop_countt_ggplot_10

P72015_words_Stop_Stam  %>% count(stam, sort = TRUE)->P72015_words_Stop_Stam_count

P72015_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e","#e1153e","dark green","dark green","dark green","dark red","dark red","dark green","dark red","dark red"))+
  labs(title ="2015  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,450)-> P72015_words_Stop_Stam_count_ggplot_10

P72015_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e"))+
  labs(title ="2015  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,450)-> P72015_words_Stop_Stam_count_ggplot_10_clean

P72015_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#e1153e"))+
  labs(title ="2015  mit Trunkierung")+ xlab("Wortstamm")+
  
  grid.arrange(P72015_words_Stop_countt_ggplot_10,P72015_words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der ProSiebenSat1 SE über das Jahr 2015",gp=gpar(fontsize=20,front=3)))

###Bertelsmann/Gesamt
BMGesamt_Words_Stop  %>% count(word, sort = TRUE)->BMGesamt_Words_Stop_count

BMGesamt_Words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="Gesamtübersicht  ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,550)-> BMGesamt_Words_Stop_count_ggplot_10

BMGesamt_Words_Stop_Stam  %>% count(stam, sort = TRUE)->BMGesamt_Words_Stop_Stam_count

BMGesamt_Words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#101c4b","dark green","dark red","dark green","dark red","dark green","#101c4b","dark green","dark red","dark red"))+
  labs(title ="Gesamtübersicht  mit Trunkierung")+xlab("Wortstamm")+
  coord_flip()+ylim(0,550)-> BMGesamt_Words_Stop_Stam_count_ggplot_10

BMGesamt_Words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="Gesamtübersicht  mit Trunkierung")+xlab("Wortstamm")+
  coord_flip()+ylim(0,550)-> BMGesamt_Words_Stop_Stam_count_ggplot_10_clean

BMGesamt_Words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="Gesamtübersicht  mit Trunkierung")+xlab("Wortstamm")+
  coord_flip()+ylim(0,750)-> BMGesamt_Words_Stop_Stam_count_ggplot_10_clean2

grid.arrange(BMGesamt_Words_Stop_count_ggplot_10,BMGesamt_Words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der Bertelsmann SE & Co. KGaA über alle Geschäftsberichte",gp=gpar(fontsize=20,front=3)))

###Bertelsmann/2008
BM2008_words_Stop  %>% count(word, sort = TRUE)->BM2008_Words_Stop_count

BM2008_Words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2008  ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,260)-> BM2008_Words_Stop_count__gplot_10

BM2008_words_Stop_Stam  %>% count(stam, sort = TRUE)->BM2008_words_Stop_Stam_count

BM2008_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+geom_col(fill=c("#101c4b","#101c4b","dark green","dark red","dark green","dark green","dark red","dark red","dark green","dark green"))+
  labs(title ="2008  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,260)-> BM2008_words_Stop_Stam_count_ggplot_10

BM2008_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2008  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,260)-> BM2008_words_Stop_Stam_count_ggplot_10_clean

BM2008_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2008  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,300)-> BM2008_words_Stop_Stam_count_ggplot_10_clean2

grid.arrange(BM2008_Words_Stop_count__gplot_10,BM2008_words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der Bertelsmann SE & Co. KGaA für das Jahr 2008",gp=gpar(fontsize=20,front=3)))

###Bertelsmann/2011
BM2011_words_Stop  %>% count(word, sort = TRUE)->BM2011_words_Stop_count

BM2011_words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2011  ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,200)-> BM2011_words_Stop_count_ggplot_10

BM2011_words_Stop_Stam  %>% count(stam, sort = TRUE)->BM2011_words_Stop_Stam_count

BM2011_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#101c4b","dark green","dark green","dark red","dark green","dark red","dark green","dark red","dark red","dark green"))+
  labs(title ="2011  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,200)-> BM2011_words_Stop_Stam_count_ggplot_10

BM2011_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2011  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,260)-> BM2011_words_Stop_Stam_count_ggplot_10_clean

BM2011_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2011  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,250)-> BM2011_words_Stop_Stam_count_ggplot_10_clean2

grid.arrange(BM2011_words_Stop_count_ggplot_10,BM2011_words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der Bertelsmann SE & Co. KGaA für das Jahr 2011",gp=gpar(fontsize=20,front=3)))

###Bertelsmann/2015
BM2015_words_Stop %>% count(word, sort = TRUE)->BM2015_words_Stop_count

BM2015_words_Stop_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(word,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2015  ohne Trunkierung")+xlab("Wörter")+
  coord_flip()+ylim(0,250)-> BM2015_words_Stop_count_ggplot_10

BM2015_words_Stop_Stam  %>% count(stam, sort = TRUE)->BM2015_words_Stop_Stam_count

BM2015_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=c("#101c4b","dark green","dark red","dark green","dark red","dark green","dark green","dark green","dark red","dark red"))+
  labs(title ="2015  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,250)-> BM2015_words_Stop_Stam_count_ggplot_10

BM2015_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2015  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,260)-> BM2015_words_Stop_Stam_count_ggplot_10_clean

BM2015_words_Stop_Stam_count %>%
  top_n(10) %>% 
  ggplot()+
  aes(x=reorder(stam,n),y=n)+
  geom_col(fill=("#101c4b"))+
  labs(title ="2015  mit Trunkierung")+ xlab("Wortstamm")+
  coord_flip()+ylim(0,400)-> BM2015_words_Stop_Stam_count_ggplot_10_clean2

grid.arrange(BM2015_words_Stop_count_ggplot_10,BM2015_words_Stop_Stam_count_ggplot_10,ncol=2,top=textGrob("Häufigkeitsverteilung der Bertelsmann SE & Co. KGaA für das Jahr 2015",gp=gpar(fontsize=20,front=3)))

##StanWörter Vergleich Jahre
grid.arrange(WordsGesamt_Stop_Stam_count_ggplot_10_clean,Words2008_Stop_Stam_count_ggplot_10_clean,Words2011_Stop_Stam_count_ggplot_10_clean,Words2015_Stop_Stam_count_ggplot_10_clean,ncol=2,top=textGrob("Häufigkeitsverteilung über den gesamten Zeitraum nach Jahren",gp=gpar(fontsize=20,front=3)))
grid.arrange(P7Gesamt_Words_Stop_Stam_count_ggplot_10_clean,P72008_words_Stop_Stam_count_ggplot_10_clean,P72011_words_Stop_Stam_count_ggplot_10_clean,P72015_words_Stop_Stam_count_ggplot_10_clean,ncol=2,top=textGrob("Häufigkeitsverteilung über den gesamten Zeitraum nach Jahren für die ProSiebenSat1 SE",gp=gpar(fontsize=20,front=3)))
grid.arrange(BMGesamt_Words_Stop_Stam_count_ggplot_10_clean,BM2008_words_Stop_Stam_count_ggplot_10_clean,BM2011_words_Stop_Stam_count_ggplot_10_clean,BM2015_words_Stop_Stam_count_ggplot_10_clean,ncol=2,top=textGrob("Häufigkeitsverteilung über den gesamten Zeitraum nach Jahren für die Bertelsmann SE & Co. KGaA",gp=gpar(fontsize=20,front=3)))

##ProSiebenvsBertelsmann
grid.arrange(P7Gesamt_Words_Stop_Stam_count_ggplot_10_clean2,BMGesamt_Words_Stop_Stam_count_ggplot_10_clean2,ncol=2,top=textGrob("Bertelsmann Vs ProSiebenSat1 Gesamt",gp=gpar(fontsize=20,front=3)))
grid.arrange(P72008_words_Stop_Stam_count_ggplot_10_clean2,BM2008_words_Stop_Stam_count_ggplot_10_clean2,ncol=2,top=textGrob("ProSiebenSat1 Vs Bertelsmann für das Jahr 2008",gp=gpar(fontsize=20,front=3)))
grid.arrange(P72011_words_Stop_Stam_count_ggplot_10_clean2,BM2011_words_Stop_Stam_count_ggplot_10_clean2,ncol=2,top=textGrob("ProSiebenSat1 Vs Bertelsmann für das Jahr 2011",gp=gpar(fontsize=20,front=3)))
grid.arrange(P72015_words_Stop_Stam_count_ggplot_10_clean2,BM2015_words_Stop_Stam_count_ggplot_10_clean2,ncol=2,top=textGrob("ProSiebenSat1 Vs Bertelsmann für das Jahr 2015",gp=gpar(fontsize=20,front=3)))

#Aufbereitung der Sentiment
##Verlauf Ã¼ber die Jahre/Gesamt
Jahre<-c(2008,2011,2015)
Jahre <- as.data.frame(Jahre)
Sentiment_Gesamt<-dplyr::bind_rows(Words2008_Stop_Stam_sentiment_summe,Words2011_Stop_Stam_sentiment_summe,Words2015_Stop_Stam_sentiment_summe)
Sentiment_Gesamt_Jahre<-dplyr::bind_cols(Jahre,Sentiment_Gesamt)
Sentiment_Gesamt_Jahre1<-Sentiment_Gesamt_Jahre
Sentiment_Gesamt_Jahre1$Jahre<-factor(Sentiment_Gesamt_Jahre1$Jahre)
ggplot(Sentiment_Gesamt_Jahre1,aes(y=Sentimentwert,x=Jahre,group=1))+geom_line()+geom_point()

##Verlauf Ã¼ber die Jahre/ProSieben
Sentiment_ProSieben<-dplyr::bind_rows(P72008_words_Stop_Stam_sentiment_summe,P72011_words_Stop_Stam_sentiment_summe,P72015_words_Stop_Stam_sentiment_summe)
Sentiment_ProSieben_Jahre<-dplyr::bind_cols(Jahre,Sentiment_ProSieben)
Sentiment_ProSieben_Jahre1<-Sentiment_ProSieben_Jahre
Sentiment_ProSieben_Jahre1$Jahre<-factor(Sentiment_ProSieben_Jahre1$Jahre)
ggplot(Sentiment_ProSieben_Jahre1,aes(y=Sentimentwert,x=Jahre,group=1))+geom_line(col="red")+geom_point(col="red")

##Verlauf Ã¼ber die Jahre/Bertelsmann
Sentiment_Bertelsmann<-dplyr::bind_rows(BM2008_words_Stop_Stam_sentiment_summe,BM2011_words_Stop_Stam_sentiment_summe,BM2015_words_Stop_Stam_sentiment_summe)
Sentiment_Bertelsmann_Jahre<-dplyr::bind_cols(Jahre,Sentiment_Bertelsmann)
Sentiment_Bertelsmann_Jahre1<-Sentiment_Bertelsmann_Jahre
Sentiment_Bertelsmann_Jahre1$Jahre<-factor(Sentiment_Bertelsmann_Jahre1$Jahre)
ggplot(Sentiment_Bertelsmann_Jahre1,aes(y=Sentimentwert,x=Jahre,group=1))+geom_line(col="blue")+geom_point(col="blue")


## Top 10 Pos/Negativ
###Gesamt
WordsGesamt_Stop_Stam %>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->WordsGesamt_Stop_Stam_Sentiment_pos

WordsGesamt_Stop_Stam_Sentiment_pos%>%
  top_n(10)->WordsGesamt_Stop_Stam_Sentiment_pos_10

WordsGesamt_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("Orange"))+
  labs(title="Top 10 posetive Wörter")+xlab("Wortstamm")+
  coord_flip()+ylim(0,1)->WordsGesamt_Stop_Stam_Sentiment_pos_10_ggplot

WordsGesamt_Stop_Stam %>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->WordsGesamt_Stop_Stam_Sentiment_neg

WordsGesamt_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->WordsGesamt_Stop_Stam_Sentiment_neg_10


WordsGesamt_Stop_Stam_Sentiment_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("Orange"))+
  labs(title="Top 10 negative Wörter")+xlab("Wortstam")+xlab("Wortstam")+
  coord_flip()+ylim(-1,0)->WordsGesamt_Stop_Stam_Sentiment_neg_10_ggplot

grid.arrange(WordsGesamt_Stop_Stam_Sentiment_neg_10_ggplot,WordsGesamt_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2,top=textGrob("Top 10 negative/posetive Wörter über alle Geschäftsberichte in allen Jahren",gp=gpar(fontsize=20,front=3)))

WordsGesamt_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(WordsGesamt_Stop_Stam_Sentiment_pos_10,WordsGesamt_Stop_Stam_Sentiment_neg_10)
WordsGesamt_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("Orange"))+
  labs(title="Top 10 negative/positive Wörter in allen Berichten")+xlab("Wortstam")+
  coord_flip()+xlab("Wortstam")+
  ylim(-1,1)->WordsGesamt_Stop_Stam_Sentiment_pos_neg_10_ggplot

###2008
Words2008_Stop_Stam %>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->Words2008_Stop_Stam_Sentiment_pos

Words2008_Stop_Stam_Sentiment_pos%>%
  top_n(10)->Words2008_Stop_Stam_Sentiment_pos_10

Words2008_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 posetive Wörter")+xlab("Wortstam")+xlab("Wortstam")+
  coord_flip()+ylim(0,1)->Words2008_Stop_Stam_Sentiment_pos_10_ggplot

Words2008_Stop_Stam %>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->Words2008_Stop_Stam_Sentiment_neg

Words2008_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->Words2008_Stop_Stam_Sentiment_neg_10


Words2008_Stop_Stam_Sentiment_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 negative Wörter")+xlab("Wortstam")+
  coord_flip()+ylim(-1,0)->Words2008_Stop_Stam_Sentiment_neg_10_ggplot

grid.arrange(Words2008_Stop_Stam_Sentiment_neg_10_ggplot,Words2008_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2,top=textGrob("Top 10 negative/posetive Wörter über alle Geschäftsberichte im Jahr 2008",gp=gpar(fontsize=20,front=3)))

Words2008_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(Words2008_Stop_Stam_Sentiment_pos_10,Words2008_Stop_Stam_Sentiment_neg_10)

Words2008_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 negative/posetiv Wörter 2008")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->Words2008_Stop_Stam_Sentiment_pos_neg_10_ggplot

###2011
Words2011_Stop_Stam %>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->Words2011_Stop_Stam_Sentiment_pos

Words2011_Stop_Stam_Sentiment_pos%>%
  top_n(10)->Words2011_Stop_Stam_Sentiment_pos_10

Words2011_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 posetive Wörter")+xlab("Wortstam")+
  coord_flip()+ylim(0,1)->Words2011_Stop_Stam_Sentiment_pos_10_ggplot

Words2011_Stop_Stam %>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->Words2011_Stop_Stam_Sentiment_neg

Words2011_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->Words2011_Stop_Stam_Sentiment_neg_10


Words2011_Stop_Stam_Sentiment_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 negative Wörter")+xlab("Wortstam")+
  coord_flip()+ylim(-1,0)->Words2011_Stop_Stam_Sentiment_neg_10_ggplot

grid.arrange(Words2011_Stop_Stam_Sentiment_neg_10_ggplot,Words2011_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2,top=textGrob("Top 10 negative/posetive Wörter über alle Geschäftsberichte im Jahr 2011",gp=gpar(fontsize=20,front=3)))

Words2011_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(Words2011_Stop_Stam_Sentiment_pos_10,Words2011_Stop_Stam_Sentiment_neg_10)
Words2011_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 negative/posetiv Wörter 2011")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->Words2011_Stop_Stam_Sentiment_pos_neg_10_ggplot

###2015
Words2015_Stop_Stam %>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->Words2015_Stop_Stam_Sentiment_pos

Words2015_Stop_Stam_Sentiment_pos%>%
  top_n(10)->Words2015_Stop_Stam_Sentiment_pos_10

Words2015_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 posetive Wörter")+xlab("Wortstam")+
  coord_flip()+ylim(0,1)->Words2015_Stop_Stam_Sentiment_pos_10_ggplot

Words2015_Stop_Stam %>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->Words2015_Stop_Stam_Sentiment_neg

Words2015_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->Words2015_Stop_Stam_Sentiment_neg_10


Words2015_Stop_Stam_Sentiment_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 negative Wörter")+xlab("Wortstam")+
  coord_flip()+ylim(-1,0)->Words2015_Stop_Stam_Sentiment_neg_10_ggplot

grid.arrange(Words2015_Stop_Stam_Sentiment_neg_10_ggplot,Words2015_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2,top=textGrob("Top 10 negative/posetive Wörter über alle Geschäftsberichte im Jahr 2015",gp=gpar(fontsize=20,front=3)))

Words2015_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(Words2015_Stop_Stam_Sentiment_pos_10,Words2015_Stop_Stam_Sentiment_neg_10)

Words2015_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=c("orange"))+
  labs(title="Top 10 negative/posetiv Wörter 2015")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->Words2015_Stop_Stam_Sentiment_pos_neg_10_ggplot

grid.arrange(WordsGesamt_Stop_Stam_Sentiment_pos_neg_10_ggplot,Words2008_Stop_Stam_Sentiment_pos_neg_10_ggplot,Words2011_Stop_Stam_Sentiment_pos_neg_10_ggplot,Words2015_Stop_Stam_Sentiment_pos_neg_10_ggplot,ncol=2)

#ProSieben
##Gesamt##
P7Gesamt_Words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->P7Gesamt_Words_Stop_Stam_Sentiment_pos

P7Gesamt_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->P7Gesamt_Words_Stop_Stam_Sentiment_pos_10

P7Gesamt_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 posetive Wörter")+xlab("Wortstam")+
  coord_flip()->P7Gesamt_Words_Stop_Stam_Sentiment_pos_10_ggplot

P7Gesamt_Words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->P7Gesamt_Words_Stop_Stam_Sentiment_neg

P7Gesamt_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->P7Gesamt_Words_Stop_Stam_Sentiment_neg_10


P7Gesamt_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative Wörter")+
  coord_flip()->P7Gesamt_Words_Stop_Stam_Sentiment_neg_10_ggplot

grid.arrange(P7Gesamt_Words_Stop_Stam_Sentiment_neg_10_ggplot,P7Gesamt_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

P7Gesamt_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(P7Gesamt_Words_Stop_Stam_Sentiment_pos_10,P7Gesamt_Words_Stop_Stam_Sentiment_neg_10)
P7Gesamt_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative/posetiv Wörter")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->P7Gesamt_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

#2008
P72008_words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->P72008_Words_Stop_Stam_Sentiment_pos

P72008_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->P72008_Words_Stop_Stam_Sentiment_pos_10

P72008_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 posetive Wörter 2008")+
  xlab("Wortstam")+coord_flip()->P72008_Words_Stop_Stam_Sentiment_pos_10_ggplot

P72008_words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->P72008_Words_Stop_Stam_Sentiment_neg

P72008_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->P72008_Words_Stop_Stam_Sentiment_neg_10

P72008_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative Wörter 2008")+xlab("Wortstam")+xlab("Wortstam")+xlab("Wortstam")+
  coord_flip()->P72008_Words_Stop_Stam_Sentiment_neg_10_ggplot
grid.arrange(P72008_Words_Stop_Stam_Sentiment_neg_10_ggplot,P72008_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

P72008_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(P72008_Words_Stop_Stam_Sentiment_pos_10,P72008_Words_Stop_Stam_Sentiment_neg_10)
P72008_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative/posetiv Wörter 2008")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->P72008_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

#2011
P72011_words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->P72011_Words_Stop_Stam_Sentiment_pos

P72011_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->P72011_Words_Stop_Stam_Sentiment_pos_10

P72011_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 posetive Wörter 2011")+
  xlab("Wortstam")+coord_flip()->P72011_Words_Stop_Stam_Sentiment_pos_10_ggplot

P72011_words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->P72011_Words_Stop_Stam_Sentiment_neg

P72011_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->P72011_Words_Stop_Stam_Sentiment_neg_10


P72011_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative Wörter 2011")+
  xlab("Wortstam")+ coord_flip()->P72011_Words_Stop_Stam_Sentiment_neg_10_ggplot
grid.arrange(P72011_Words_Stop_Stam_Sentiment_neg_10_ggplot,P72011_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

P72011_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(P72011_Words_Stop_Stam_Sentiment_pos_10,P72011_Words_Stop_Stam_Sentiment_neg_10)
P72011_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative/posetiv Wörter 2011")+
  xlab("Wortstam")+ coord_flip()+
  xlab("Wörter")+
  ylim(-1,1)->P72011_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

#2015
P72015_words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->P72015_Words_Stop_Stam_Sentiment_pos

P72015_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->P72015_Words_Stop_Stam_Sentiment_pos_10

P72015_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 posetive Wörter 2015")+
  xlab("Wortstam")+coord_flip()->P72015_Words_Stop_Stam_Sentiment_pos_10_ggplot

P72015_words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->P72015_Words_Stop_Stam_Sentiment_neg

P72015_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->P72015_Words_Stop_Stam_Sentiment_neg_10


P72015_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative Wörter 2015")+
  xlab("Wortstam")+coord_flip()->P72015_Words_Stop_Stam_Sentiment_neg_10_ggplot
grid.arrange(P72015_Words_Stop_Stam_Sentiment_neg_10_ggplot,P72015_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

P72015_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(P72015_Words_Stop_Stam_Sentiment_pos_10,P72015_Words_Stop_Stam_Sentiment_neg_10)
P72015_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col(fill=("#e1153e"))+  labs(title="Top 10 negative/posetiv Wörter 2015")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->P72015_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

grid.arrange(P72008_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,P72011_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,P72015_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,ncol=2)

#Bertelsmann
##Gesamt##
BMGesamt_Words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->BMGesamt_Words_Stop_Stam_Sentiment_pos

BMGesamt_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->BMGesamt_Words_Stop_Stam_Sentiment_pos_10

BMGesamt_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 posetive Wörter")+xlab("Wortstam")+
  coord_flip()->BMGesamt_Words_Stop_Stam_Sentiment_pos_10_ggplot

BMGesamt_Words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->BMGesamt_Words_Stop_Stam_Sentiment_neg

BMGesamt_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->BMGesamt_Words_Stop_Stam_Sentiment_neg_10


BMGesamt_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative Wörter")+xlab("Wortstam")+
  coord_flip()->BMGesamt_Words_Stop_Stam_Sentiment_neg_10_ggplot

grid.arrange(BMGesamt_Words_Stop_Stam_Sentiment_neg_10_ggplot,BMGesamt_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

BMGesamt_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(BMGesamt_Words_Stop_Stam_Sentiment_pos_10,BMGesamt_Words_Stop_Stam_Sentiment_neg_10)
BMGesamt_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative/posetiv Wörter")+
  coord_flip()+
  xlab("Wörter")+
  ylim(-1,1)->BMGesamt_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

#2008
BM2008_words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->BM2008_Words_Stop_Stam_Sentiment_pos

BM2008_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->BM2008_Words_Stop_Stam_Sentiment_pos_10

BM2008_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 posetive Wörter 2008")+xlab("Wortstam")+
  coord_flip()->BM2008_Words_Stop_Stam_Sentiment_pos_10_ggplot

BM2008_words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->BM2008_Words_Stop_Stam_Sentiment_neg

BM2008_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->BM2008_Words_Stop_Stam_Sentiment_neg_10

BM2008_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative Wörter 2008")+
  coord_flip()+xlab("Wortstam")->BM2008_Words_Stop_Stam_Sentiment_neg_10_ggplot
grid.arrange(BM2008_Words_Stop_Stam_Sentiment_neg_10_ggplot,BM2008_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

BM2008_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(BM2008_Words_Stop_Stam_Sentiment_pos_10,BM2008_Words_Stop_Stam_Sentiment_neg_10)
BM2008_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative/posetiv Wörter 2008")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->BM2008_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot


#2011
BM2011_words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->BM2011_Words_Stop_Stam_Sentiment_pos

BM2011_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->BM2011_Words_Stop_Stam_Sentiment_pos_10

BM2011_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 posetive Wörter 2011")+xlab("Wortstam")+
  coord_flip()->BM2011_Words_Stop_Stam_Sentiment_pos_10_ggplot

BM2011_words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->BM2011_Words_Stop_Stam_Sentiment_neg

BM2011_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->BM2011_Words_Stop_Stam_Sentiment_neg_10


BM2011_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative Wörter 2011")+xlab("Wortstam")+
  coord_flip()->BM2011_Words_Stop_Stam_Sentiment_neg_10_ggplot
grid.arrange(BM2011_Words_Stop_Stam_Sentiment_neg_10_ggplot,BM2011_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

BM2011_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(BM2011_Words_Stop_Stam_Sentiment_pos_10,BM2011_Words_Stop_Stam_Sentiment_neg_10)
BM2011_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative/posetiv Wörter 2011")+
  coord_flip()+
  xlab("Wortstam")+
  ylim(-1,1)->BM2011_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

#2015
BM2015_words_Stop_Stam%>%
  filter(neg_pos == "pos") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(-Wert) %>% 
  dplyr::select(stam, Wert)->BM2015_Words_Stop_Stam_Sentiment_pos

BM2015_Words_Stop_Stam_Sentiment_pos%>%
  top_n(10)->BM2015_Words_Stop_Stam_Sentiment_pos_10

BM2015_Words_Stop_Stam_Sentiment_pos_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 posetive Wörter 2015")+xlab("Wortstam")+
  coord_flip()->BM2015_Words_Stop_Stam_Sentiment_pos_10_ggplot

BM2015_words_Stop_Stam%>%
  filter(neg_pos == "neg") %>% 
  distinct(stam, .keep_all = TRUE) %>% 
  arrange(Wert) %>% 
  dplyr::select(stam, Wert)->BM2015_Words_Stop_Stam_Sentiment_neg

BM2015_Words_Stop_Stam_Sentiment_neg%>%
  top_n(-10)->BM2015_Words_Stop_Stam_Sentiment_neg_10


BM2015_Words_Stop_Stam_Sentiment_neg_10 %>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative Wörter 2015")+xlab("Wortstam")+
  coord_flip()->BM2015_Words_Stop_Stam_Sentiment_neg_10_ggplot
grid.arrange(BM2015_Words_Stop_Stam_Sentiment_neg_10_ggplot,BM2015_Words_Stop_Stam_Sentiment_pos_10_ggplot,ncol=2)

BM2015_Words_Stop_Stam_Sentiment_pos_neg_10<-dplyr::bind_rows(BM2015_Words_Stop_Stam_Sentiment_pos_10,BM2015_Words_Stop_Stam_Sentiment_neg_10)
BM2015_Words_Stop_Stam_Sentiment_pos_neg_10%>%
  ggplot()+
  aes(x=reorder(stam,Wert),y=Wert)+
  geom_col()+
  labs(title="Top 10 negative/posetiv Wörter 2015")+xlab("Wortstam")+
  coord_flip()+
  xlab("")+
  ylim(-1,1)->BM2015_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot

grid.arrange(BMGesamt_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,BM2008_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,BM2011_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,BM2015_Words_Stop_Stam_Sentiment_pos_neg_10_ggplot,ncol=2)
#Bertelsmann VS ProSieben
