#psuedocode
# Inti
require(plyr)
require(tm)
require(class)

# Set options
options(stringsAsFactors = F)


# Clean text
cleanCorpus<- function(corpus){
  corpus.tm<- tm_map(corpus, removePunctuation)
  corpus.tm<- tm_map(corpus.tm, stripWhitespace)
  corpus.tm<-tm_map(corpus.tm, content_transformer(tolower))
  corpus.tm<- tm_map(corpus.tm, removeWords, stopwords('english'))
  corpus.tm<- tm_map(corpus.tm, PlainTextDocument)
  return(corpus.tm)
  
}

# build Text Ducument Matrix

toCorpus<-sprintf("%s__%s", diag.icd$Free_Text, diag.icd$Icd_10) 

generateTDM<- function(corpus){
  corpus.code<-word(corpus, 2,2, sep="\\__")
  corpus.str<-word(corpus, 1,1, sep="\\__")
 s.corp<- Corpus(VectorSource(corpus.str))
 s.corp.cln<-  cleanCorpus(s.corp)
 s.tdm<- TermDocumentMatrix(s.corp.cln)
 s.tdm<- removeSparseTerms(s.tdm, 0.7)
 return(
   list(code= corpus.code, doc.mat= s.tdm)
 )
 
}

tdm<- lapply(toCorpus, generateTDM)




# Attach name:  ICD nme 

bindIcdToTDM<- function (tdm){
    s.mat<- t(data.matrix(tdm[["doc.mat"]]))
  s.df<- as.data.frame(s.mat, stringsAsFactors=F)
  s.df<- cbind(s.df, rep(tdm[["code"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)]<- "targetICD"
  return(s.df)
}

ICDtdm<- lapply(tdm, bindIcdToTDM)

# stack
tdm.stack<- do.call(rbind.fill, ICDtdm)
tdm.stack[is.na(tdm.stack)]<- 0
head(tdm.stack)
# hold-out( test, train) #measure how accurate
train.idx<- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack)*0.7))
test.idx<- (1:nrow(tdm.stack))[-train.idx]

# model  --KNNeighbor
tdm.icd<- tdm.stack[, "targetICD"]
tdm.stack.nl<- tdm.stack[, !colnames(tdm.stack) %in% "targetICD"]


knn.pred<- knn(tdm.stack.nl[train.idx, ]
               , tdm.stack.nl[test.idx,]
               , tdm.icd[train.idx]
               , k= 21L)


#accruracy

confusion.mat<- table("Predictions"=knn.pred, Actual.icd=tdm.icd[test.idx])

accuracy<- sum(diag(confusion.mat))/ length(test.idx)*100
accuracy
