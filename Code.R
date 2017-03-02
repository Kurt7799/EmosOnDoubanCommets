
Ruquie Rwordseg & rJava


library(Rwordseg)
library(plyr)
setwd("E://Rweibo")

#input comments
train<- read.csv("DrStrange.csv",quote = "",sep = ",", 
	header = T, stringsAsFactors = F)
head(train) #DataFrame :Id/Comments/Label /chr 

#input postive & negative emos dict by HowNet
pos <- read.csv("pos.txt", header = T, sep = ",", stringsAsFactors = F)  # term
#take weights pos as 1, neg as-1
weight <- rep(1, length(pos[,1]))
pos <- cbind(pos, weight)

neg <- read.csv("neg.txt", header = T, sep = ",", stringsAsFactors = F)
weight <- rep(-1, length(neg[,1]))  
neg <- cbind(neg, weight)

posneg <- rbind(pos, neg)
# put posneg into dict 
dict <- posneg[, "term"] 
insertWords(dict) 

#数据清洗、分词
sentence <- as.vector(train$Comments)
sentence <- gsub("[[:digit:]]*", "", sentence)
sentence <- gsub("[a-zA-Z]", "", sentence)
sentence <- gsub("\\.", "", sentence)

system.time(x <- segmentCN(strwords = sentence)) 
# it would takes about 15s to run which depends on ur pc

x <- segmentCN(strwords = sentence)

temp <- lapply(x, length)  # Rwordseg divied comments into *temp* words
temp <- unlist(temp) 
id <- rep(train[, 1], temp) 
label <- rep(train[, 3], temp)  
term <- unlist(x)
testterm <- as.data.frame(cbind(id, term, label), stringsAsFactors = F)

#计算正反向情感得分
testterm <- join(testterm, posneg)  # 按照term加到一起

dictresult <- aggregate(weight ~ id, data = testterm, sum) 
#按照ID把评分聚合到一起
names(dictresult)[2]<-"emos"
write.table(dictresult,"E:/Result/DrStrange_result.csv",sep=" ")
