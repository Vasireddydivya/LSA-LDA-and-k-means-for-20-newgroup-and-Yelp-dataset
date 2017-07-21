# LSA-LDA-and-k-means-for-20-newgroup-and-Yelp-dataset
I have implemented the LSA,LDA, k-means for 20-newgroup dataset and Yelp dataset in R
From 20- newsgroup data set I selected 3 groups to perform clustering.
After selecting the 3 groups, created a corpus with all these groups.
As part of data prepocessing, removed the Subject, title, lines, article, punctuations, white spaces, numbers, stopwords etc. Then stemming the corpus helps to remove few words. example: stemming converts the 'singing' to sing.
Then created a Term-Document matrix with word size of 4. For viewing the frequently occuring words in the data set I've created a word cloud.
For finding important words in a document in the entire corpus we can use tf-idf(term frequency, inverse term frequency). Then, saved the frequently occuring words in an excel sheet
We can remove the sparse terms in the data set. This will remove the words which occur less frequently in the corpus.
Now, corpus is ready for modeling. For finding the best number of clusters we can use nbclust. Applied k-means,LSA and LDA on the term document matrix.
For finding the mis classification error, manually assigned labels to the data set and calculated the accuracy, precision and recall.
