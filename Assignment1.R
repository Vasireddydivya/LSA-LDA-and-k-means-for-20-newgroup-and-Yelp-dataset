install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
require(tm)
require(SnowballC)
install.packages('NbClust')
library(NbClust)
install.packages('wordcloud')
library(wordcloud)
install.packages('lsa')
library(lsa)
install.packages('topicmodels')
library(topicmodels)

folder=c()
foldername='C:/Users/vasir/Documents/20_groupsets/20news-bydate-train/'
folders<-c('sci.electronics','talk.religion.misc','sci.space')
folders<-c('alt.atheism','talk.politics.misc','talk.religion.misc')
for (i in folders){
  folder<-c(folder,paste(foldername,i,sep=''))
}
folder<-as.list(folder)
sci.electr.train <- Corpus(DirSource(folder[[1]], encoding = "UTF-8"), readerControl=list(reader=readPlain,language="en"))
talk.religion.misc<-Corpus(DirSource(folder[[2]], encoding = "UTF-8"), readerControl=list(reader=readPlain,language="en"))
sci.space<-Corpus(DirSource(folder[[3]], encoding = "UTF-8"), readerControl=list(reader=readPlain,language="en"))


#processing the data
train.data=c(sci.electr.train,talk.religion.misc,sci.space)
train.data.p<-tm_map(train.data,removeWords,"Subject")
train.data.p<-tm_map(train.data.p,removeWords,"Organization")
train.data.p<-tm_map(train.data.p,removeWords,"writes")
train.data.p<-tm_map(train.data.p,removeWords,"From")
train.data.p<-tm_map(train.data.p,removeWords,"lines")
train.data.p<-tm_map(train.data.p,removeWords,"NNTP-Posting-Host")
train.data.p<-tm_map(train.data.p,removeWords,"article")
train.data.p<-tm_map (train.data.p, content_transformer(tolower))
train.data.p<-tm_map (train.data.p, removePunctuation)
train.data.p<-tm_map (train.data.p, stripWhitespace)
train.data.p<-tm_map (train.data.p, removeNumbers)
myStopwords<-stopwords('english')
train.data.p<-tm_map (train.data.p, removeWords,myStopwords)
train.data.p<-tm_map (train.data.p, stemDocument)


tdm <- TermDocumentMatrix (train.data.p,control = list(wordLengths=c(4,Inf)))
inspect(tdm[1:20, 1:100])
dtm<-DocumentTermMatrix(train.data.p,control = list(wordLengths=c(4,Inf)))
inspect(dtm[1:20, 1:100])
mat=as.matrix(tdm)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)

freq<-sort(colSums(as.matrix(dtm)),decreasing = TRUE)
wordcloud(names(freq),freq,max.words=100,color=brewer.pal(8,"Dark2"))


tdm_tfidf<-weightTfIdf(tdm)
dtm_tfidf<-weightTfIdf(dtm)
inspect(dtm_tfidf[1:10, 100:200])
m <- as.matrix(dtm_tfidf)
rownames(m) <- 1:nrow(m)

#Euclidean distance
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)

#Remove sparse items
dtms <- removeSparseTerms(dtm, 0.96)

num_cluster<-3
cl <- kmeans(dtms, num_cluster)
table(c1$cluster)
plot(prcomp(dtms)$x,col=c1$cluster)
num_cluster<-5
cl_1 <- kmeans(dtms, num_cluster)
plot(prcomp(dtms)$x,col=c1_1$cluster)
num_cluster<-10
cl_2 <- kmeans(dtms, num_cluster)
plot(prcomp(dtms)$x,col=c1_2$cluster)

#For k=3 the sum of squares value is less. So k=3 is good for clustering.

#nbclust
nbc<-NbClust(as.matrix(dtms),distance='euclidean',min.nc = 3,max.nc = 5,method = 'kmeans',index='silhouette')

#LSA
reduce<-function(mat,dim){
  sv<-svd(mat)
  u<-as.matrix(sv$u[,1:dim])
  v<-as.matrix(sv$v[,1:dim])
  d<-as.matrix(diag(sv$d)[1:dim,1:dim])
  return(as.matrix(u%*%d%*%t(v),type="green"))
}
reduce_dtm<-reduce(dtm_tfidf,5)
nrom<-norm_eucl(reduce_dtm)

#k-means
nrom_k3<-kmeans(nrom,3)
clu_3<-nrom_k3$cluster
pr<-prcomp(nrom)$x
plot( pr,col=clu_3)

nrom_k5<-kmeans(nrom,5)
clu_5<-nrom_k5$cluster
plot( pr,col=clu_5)


nrom_k10<-kmeans(nrom,10)
clu_10<-nrom_k10$cluster
plot( pr,col=clu_10)

#term document matrix
tdm_tfidf<-weightTfIdf(tdm)
reduce_tdm<-reduce(tdm_tfidf,50)

reduce<-function(mat,dim){
  sv<-svd(mat)
  u<-as.matrix(sv$u[,1:dim])
  v<-as.matrix(sv$v[,1:dim])
  d<-as.matrix(diag(sv$d)[1:dim,1:dim])
  return(as.matrix(u%*%d%*%t(v),type="green"))
}

#Most representative words
sv<-svd(dtm)
sv1<-sort.list(abs(sv[,50]),decreasing=TRUE)
write.csv(dtm_tfidf$dimnames$Terms[head(sv1,50)],file='50 Representative words')

sv1<-sort.list(abs(sv[,100]),decreasing=TRUE)
write.csv(dtm_tfidf$dimnames$Terms[head(sv1,100)],file='100 Representative words')

reduce_dtm<-reduce(dtm_tfidf,5)
nrom<-norm_eucl(reduce_dtm)

reduce_tdm_150<-reduce(tdm_tfidf,150)
nrom<-norm_eucl(reduce_tdm_150)
nrom<-na.omit(nrom)
nrom_tdm_k3<-kmeans(nrom,3)
clu_tdm_3<-nrom_tdm_k3$cluster
pr<-prcomp(nrom)$x
plot(pr,col=clu_tdm_3)

nrom_tdm_k5<-kmeans(nrom,5)
clu_tdm_5<-nrom_tdm_k5$cluster
plot(pr,col=clu_tdm_5)

reduce_tdm_100<-reduce(tdm_tfidf,100)
nrom<-norm_eucl(reduce_tdm_100)
nrom<-na.omit(nrom)
nrom_tdm100_k3<-kmeans(nrom,3)
clu_tdm100_3<-nrom_tdm100_k3$cluster
pr<-prcomp(nrom)$x
plot(pr,col=clu_tdm100_3)

#LDA
burnin <- 4000
iter <- 1000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k <- 3

ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut_50 <-LDA(dtm,50, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
tot_prob<-as.data.frame(ldaOut@gamma)
pro<-as.matrix(tot_prob)
rownames(pro)<-1:nrow(pro)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
norm_pro<-norm_eucl(pro)
cl_3<-kmeans(norm_pro,k)
plot(prcomp(norm_pro)$x,col=cl_3$cluster,main='LDA for k=3')


#yelp data set

library(jsonlite)
load('review.RData')
yelp<-stream_in(file(business_file))
yelp_flat<-flatten(yelp)
yelp_tbl<-as.data.frame(yelp_flat)
business_cat<-which(grepl("Italian",yelp_tbl$categories) | grepl("Mexican",yelp_tbl$categories))
business_indx<-yelp_tbl$business_id[business_cat]
#businss_ca<-yelp_tbl$categories[business_cat]
business_df<-data.frame(business_indx)
colnames(business_df)<-"business_id"
business_df$Categories<-yelp$categories[business_cat]

business_df<-data.frame(lapply(business_df,as.character),stringsAsFactors=FALSE)
rev<-review[,c('business_id','text')]
merge_table<-merge(rev,business_df,by='business_id')
merge_table<-merge_table[sample(nrow(merge_table),nrow(merge_table)),]
merge_sample<-merge_table[sample(1:nrow(merge_table),5000,replace=FALSE),]

corp<-Corpus(DataframeSource(merge_sample))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, stemDocument)
corp<-tm_map(corp, PlainTextDocument)


#Document Term Matrix
dtm_yelp <- DocumentTermMatrix(corp,control = list(wordLengths=c(4,Inf)))
tdm_yelp <- TermDocumentMatrix(corp,control = list(wordLengths=c(4,Inf)))

#find frequency of words
freq_yelp<-sort(colSums(as.matrix(dtm_yelp)),decreasing=TRUE)
wordcloud(names(freq_yelp),freq_yelp,max.words=50)

#tfidf
tdm_tfidf_yelp<-weightTfIdf(tdm_yelp)
dtm_tfidf_yelp<-weightTfIdf(dtm_yelp)

#remove sparse items
dtms_yelp <- removeSparseTerms(dtm_yelp, 0.92)


nbc<-NbClust(as.matrix(dtm_yelp),distance='euclidean',min.nc = 2,max.nc = 10,method = 'kmeans',index='silhouette')

num_cluster<-2
km_2 <- kmeans(dtms_yelp, num_cluster)
plot(prcomp(dtms_yelp)$x,col=km_2$cluster)

num_cluster<-5
km_5 <- kmeans(dtms_yelp, num_cluster)
plot(prcomp(dtms_yelp)$x,col=km_5$cluster)

num_cluster<-10
km_10 <- kmeans(dtms_yelp, num_cluster)
plot(prcomp(dtms_yelp)$x,col=km_10$cluster)






text_process<-merge_sample %>% mutate(text=stripWhitespace(text)) %>% mutate(text=removeNumbers(text)) %>% mutate(text=tolower(text)) %>% mutate(text=removePunctuation(text)) %>% mutate(text=iconv(text,to='ASCII'))

tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))




assign_yelp_label <- function(text){
  if(grepl("Mexican",unlist(text)) == TRUE) return(1)
  else return(2)
}

#Document Term Matrix
dtm_yelp <- DocumentTermMatrix(corp,control = list(wordLengths=c(4,Inf)))
tdm_yelp <- TermDocumentMatrix(corp,control = list(wordLengths=c(4,Inf)))

#find frequency of words
freq_yelp<-sort(colSums(as.matrix(dtm_yelp)),decreasing=TRUE)
wordcloud(names(freq_yelp),freq_yelp,max.words=50)

#tfidf
tdm_tfidf_yelp<-weightTfIdf(tdm_yelp)
dtm_tfidf_yelp<-weightTfIdf(dtm_yelp)



#confusion matrix for 20 news groups

analyze_cluster<-function(dtm,cl,clu_num){
  clus<-(dtm[cl$cluster==clu_num,])
  freq<-sort(colSums(as.matrix(clus)),decreasing = TRUE)
  wordcloud(names(freq),freq,max.words=100,color=brewer.pal(8,"Dark2"))
  fre<-data.frame(sort(colSums(as.matrix(clus)),decreasing = TRUE))
  head(fre,n=10)
}

analyze_cluster(dtm,cl,1)
analyze_cluster(dtm,cl,2)
analyze_cluster(dtm,cl,3)

conf_mat<-data.frame(cl$cluster)
colnames(conf_mat)<-"cluster_num"


assign_cluster_label <- function(rownum){
  if(rownum <= 480 ){ # 590 + 591 Documents ==> Computer Topic.
    return(1)
  }else if((rownum > 480) & (rownum <= 945)){ #1181 + 600 + 597 ==> Sports Topic.
    return(2)} else return(3)
}

conf_mat$actual <- sapply(1:nrow(conf_mat), assign_cluster_label)

#caret package
confusionMatrix(conf_mat$cluster_num,conf_mat$actual)

#Confusion Matrix for Yelp dataset

  