## Data Preparation
## 3/30/2012

path = "C:\\Users\\Ryan Wang\\Desktop\\amazon\\data"
setwd(path)
library(plyr)

##########################
## SELLER-LEVEL DATASET ##
##########################
## To study/categorize seller strategies
tmp <- listings[with(listings,order(seller,isbn,dateacc)),]
tmp$price2.prev <- c(NA,tmp$price2[-dim(tmp)[1]])
tmp$isbn.prev <- c(NA,as.character(tmp$isbn[-dim(tmp)[1]]))
tmp$seller.prev <- c(NA,as.character(tmp$seller[-dim(tmp)[1]]))
tmp$price2.prev[ c(1,with(tmp,which(seller!=seller.prev | isbn!=isbn.prev))) ] <- NA
tmp$isbn.prev=tmp$seller.prev=NULL
tmp$price2.change <- tmp$price2 - tmp$price2.prev
tmp$rank.prev <- c(NA,tmp$rank[-dim(tmp)[1]])
tmp$rank.prev[is.na(tmp$price2.change)] <- NA

## Distance from next lowest
tmp <- tmp[with(tmp,order(isbn,dateacc,rank)),]
tmp$price.next.lowest <- c(NA,tmp$price2[-dim(tmp)[1]])
tmp$isbn.prev <- c(NA,tmp$isbn[-dim(tmp)[1]])
tmp$dateacc.prev <- c(NA,tmp$dateacc[-dim(tmp)[1]])
tmp$price.next.lowest[ with(tmp,which(isbn!=isbn.prev|dateacc!=dateacc.prev)) ] <- NA
tmp$isbn.prev=tmp$dateacc.prev=NULL

## Distance from next highest
tmp$price.next.highest <- c(tmp$price2[-1],NA)
tmp$isbn.next <- c(tmp$isbn[-1],NA)
tmp$dateacc.next <- c(tmp$dateacc[-1],NA)
tmp$price.next.highest[ with(tmp,which(isbn!=isbn.next|dateacc!=dateacc.next)) ] <- NA
tmp$isbn.next=tmp$dateacc.next=NULL

## Number of price increases / decreases
tmp2 <- subset(tmp,price2.change!=0 | is.na(price2.change))
tmp2 <- tmp2[with(tmp2,order(seller,isbn,dateacc)),]
tmp2$price.inc <- as.integer(tmp2$price2.change > 0) # price increase
tmp2$price.dec <- 1 - tmp2$price.inc # price decrease
tmp2$pct.change <- tmp2$price2.change / tmp2$price2.prev # percent change

## Days between price changes
tmp2$dateacc.prev <- c(NA,tmp2$dateacc[-dim(tmp2)[1]])
tmp2$days.bet.change <- as.integer(tmp2$dateacc) - as.integer(tmp2$dateacc.prev) # days between price changes
tmp2$days.bet.change[which(is.na(tmp2$price2.change))] <- NA
tmp2$dateacc.prev <- NULL

## Indicator for first price change, since we don't know when the first price change occurred
tmp2$isbn.prev <- c(NA,tmp2$isbn[-dim(tmp2)[1]])
tmp2$seller.prev <- c(NA,tmp2$seller[-dim(tmp2)[1]])
tmp2$first.change <- 0
first <- which(tmp2$isbn!=tmp2$isbn.prev|tmp2$seller!=tmp2$seller.prev) + 1
tmp2$first.change[ first[-length(first)] ] <- 1
tmp2$first.change[1] <- 1
tmp2$first.change[which(is.na(tmp2$price2.change))] <- NA
rm(first)
tmp2$isbn.prev=tmp2$seller.prev=NULL

tmp2$days.bet.change2 <- tmp2$days.bet.change ## days between change, ignoring first change
tmp2$days.bet.change2[which(tmp2$first.change==1)] <- NA

## Ranks 'bumped' since last price change
tmp2$rank.last.change <- c(NA,tmp2$rank[-dim(tmp2)[1]])
tmp2$rank.last.change[is.na(tmp2$price2.change)] <- NA
tmp2$ranks.bumped <- tmp2$rank.prev - tmp2$rank.last.change

tmp2 <- tmp2[-which(is.na(tmp2$price2.change)),] ## clear out is.na
price.changes <- tmp2
# rm(tmp,tmp2)

## Distance from lowest price
lowest.price <- subset(listings,rank==1)[,c('dateacc2','isbn','price2')]
colnames(lowest.price)[3] <- 'price.lowest'

## Distance from Amazon
amzn.price <- subset(listings,seller=="Amazon.com")[,c('dateacc2','isbn','price2')]
colnames(amzn.price)[3] <- 'price.amzn'

special.prices <- merge(amzn.price,lowest.price,by=c('dateacc2','isbn'),all.y=T)
special.prices <- special.prices[order(special.prices$isbn,special.prices$dateacc2),]
write.csv(special.prices,'special_prices.csv',row.names=F)

price.changes <- merge(price.changes,special.prices,by=c('dateacc2','isbn'))
rm(lowest.price,amzn.price,special.prices)

## Convert to distance in percent
price.changes$pct.diff.next.lowest <- with(price.changes,(price2-price.next.lowest)/price2)
price.changes$pct.diff.next.highest <- with(price.changes,(price2-price.next.highest)/price2)
price.changes$pct.diff.lowest <- with(price.changes,(price2-price.lowest)/price2)
price.changes$pct.diff.amzn <- with(price.changes,(price2-price.amzn)/price2)
price.changes <- price.changes[with(price.changes,order(seller,isbn,dateacc)),]

## Aggregate up to isbn/seller
isbn.seller <- ddply(price.changes,.(seller,isbn),function(x) c( sum(x$price.inc), # number of price increases
                                                                 sum(x$price.dec)) # number of price decreases
                     ,.progress='text')
colnames(isbn.seller)[3:4] <- c('num.price.inc','num.price.dec')
isbn.seller$num.price.change <- isbn.seller$num.price.inc + isbn.seller$num.price.dec

## Percent of time spent on first page (requires listings data frame)
listings <- listings[order(listings$seller,listings$isbn,listings$dateacc),]
listings$dateacc.next <- c(listings$dateacc[-1],NA)
listings$isbn.next <- c(listings$isbn[-1],NA)
listings$seller.next <- c(listings$seller[-1],NA)
tona <- with(listings,which(isbn!=isbn.next|seller!=seller.next))
listings$dateacc.next[tona] <- NA
listings$days.bet <- as.integer(listings$dateacc.next-listings$dateacc)
listings$days.first.page <- listings$first.page * listings$days.bet
rm(tona)
listings$isbn.next=listings$seller.next=listings$dateacc.next=NULL
first.page <- ddply(listings[,c('isbn','seller','days.bet','days.first.page')],.(seller,isbn),function(x) c(sum(x$days.bet,na.rm=T),
                                                                                                            sum(x$days.first.page,na.rm=T)),
                    .progress='text')
colnames(first.page)[3:4] <- c('days.observed','days.first.page')

isbn.seller <- merge(isbn.seller,first.page,by=c('seller','isbn'),all.x=T,all.y=T) ## merge
isbn.seller[is.na(isbn.seller)] <- 0
isbn.seller$pct.first.page <- with(isbn.seller, days.first.page/days.observed)
isbn.seller$changes.per.day <- with(isbn.seller, num.price.change/days.observed)
isbn.seller$inc.per.day <- with(isbn.seller, num.price.inc/days.observed)
isbn.seller$dec.per.day <- with(isbn.seller, num.price.dec/days.observed)
isbn.seller$no.change <- as.integer(isbn.seller$num.price.change == 0)

## Seller variables (aggregated)
seller1 <- ddply(isbn.seller,.(seller), function(x) c( colSums(x[,c(3:5,12)],na.rm=T),
                                                       colMeans(x[,6:11],na.rm=T) ),
                 .progress='text')
seller1$prop.changes.inc <- seller1$num.price.inc / seller1$num.price.change
seller1[is.na(seller1)] <- 0
colnames(seller1)[2:12] <- c( 'num.price.inc', # 2. total number of price increases
                              'num.price.dec', # 3. total number of price decreases
                              'num.price.change', # 4. total number of price changes
                              'listings.no.change', # 5. total number of listings with no price change
                              'avg.days.observed', # 6. average number of days observed (over listings)
                              'avg.days.first.page', # 7. average number of days on first page (over listings)
                              'avg.pct.first.page', # 8. average percentage of days on first page (over listings)
                              'avg.changes.per.day', # 9. average number of changes per day (over listings)
                              'avg.inc.per.day', # 10. average number of increases per day (over listings)
                              'avg.dec.per.day', # 11. average number of decreases per day (over listings)
                              'prop.changes.inc') # 12. proportion of changes that were increases (over all changes)                                                      

## Seller variables (related to changes)
seller2 <- ddply(price.changes,.(seller),function(x) c( mean(x$days.bet.change), # avg. days between change
                                                        sd(x$days.bet.change), # sd. days between change
                                                        mean(x$ranks.bumped), # avg. number of ranks bumped
                                                        sd(x$ranks.bumped), # sd. number of ranks bumped
                                                        mean(abs(x$ranks.bumped)), # avg. abs. number of ranks bumped
                                                        sd((abs(x$ranks.bumped))), # sd. abs. number of ranks bumped
                                                        mean(abs(x$pct.change[which(x$price.inc==1)])), # avg. size of price increase
                                                        mean(abs(x$pct.change[which(x$price.dec==1)])), # avg. size of price decrease
                                                        mean(abs(x$pct.change)), # avg. percent change
                                                        mean(abs(x$price2.change))) # avg. price change
                 ,.progress='text')
                                                        
colnames(seller2)[2:11] <- c('avg.days.bet.change', # 2. average days between change (over all changes)
                             'sd.days.bet.change', # 3. sd of days beween change (over all changes)
                             'avg.ranks.bumped', # 4. average number of ranks bumped since last change (over all changes)
                             'sd.ranks.bumped', # 5. sd of number of ranks bumped since last change (over all changes)
                             'avg.abs.ranks.bumped', # 6. average magnitude of ranks bumped since last change (over all changes)
                             'sd.abs.ranks.bumped', # 7. sd of magnitude of ranks bumped since last change (over all changes)
                             'avg.pct.inc', # 8. average magnitude of price increase (over all changes)
                             'avg.pct.dec', # 9. average magnitude of price decrease (over all changes)
                             'avg.pct.change', # 10. average magnitude of price change, in % (over all changes)
                             'avg.price.change') # 11. average magnitude of price change, in $ (over all changes)

## Seller target variables
seller3 <- ddply(price.changes,.(seller),function(x) c( mean(x$rank), # average targeted rank
                                                        sd(x$rank), # std. dev. of targeted rank
                                                        mean(x$pct.diff.next.lowest,na.rm=T), # average targeted distance from next lowest
                                                        mean(x$pct.diff.next.highest,na.rm=T), # average targeted distance from next highest
                                                        mean(x$pct.diff.lowest,na.rm=T), # average targeted distance from lowest
                                                        mean(x$pct.diff.amzn,na.rm=T)) # average targeted distance from amazon
                 ,.progress='text')
colnames(seller3)[2:7] <- c( 'avg.tgt.rank', # average rank immediately following change (over all changes)
                             'avg.sd.tgt.rank', # average sd of rank immediately following change (over all changes)
                             'avg.tgt.pct.diff.next.lowest', # average percent difference from next lowest immediately following change (over all changes)
                             'avg.tgt.pct.diff.next.highest', # average percent difference from next high immediately following change (over all changes)
                             'avg.tgt.pct.diff.lowest', # average percent difference from lowest immediately following change (over all changes)
                             'avg.tgt.pct.diff.amzn') # average percent difference from Amazon immediately following change (over all changes)
seller3$avg.tgt.pct.diff.next.lowest[is.na(seller3[,3])] <- 0
seller3$avg.tgt.pct.diff.next.highest[is.na(seller3[,4])] <- 0
seller3$avg.tgt.pct.diff.lowest[is.na(seller3[,5])] <- 0

## Total number of listings
tmp <- paste(listings$isbn,listings$seller,sep=';.;')
tmp <- unique(tmp)
tmp <- data.frame( matrix( unlist(strsplit(tmp,';.;',fixed=T)), ncol=2, byrow=T ) )
tmp$listing <- 1
colnames(tmp) <- c('isbn','seller','listing')
num.listings <- ddply(tmp,.(seller),function(x) sum(x$listing),.progress='text')
colnames(num.listings)[2] <- "num.listings"
rm(tmp)

## Merge seller variables together
seller1 <- merge(num.listings,seller1,by=c('seller'))
seller <- cbind(seller2,seller3[,2:6])
seller <- merge(seller1,seller,by=c('seller'),all.x=T)
rm(seller1,seller2,seller3)

## Merge in synchronization
synch <- ddply(price.changes,.(dateacc,seller),dim,.progress='text') # same-day price synchronization
synch <- synch[,1:3]
colnames(synch)[3] <- 'num.same.day.change' # number of price changes in the same day
price.changes <- merge(price.changes,synch,by=c('dateacc','seller'))
synch2 <- ddply(synch,.(seller),function(x) mean(x$num.same.day.change))
colnames(synch2)[2] <- 'avg.num.same.day.change' # 2. average number of price changes in the same day (over days with change)
seller <- merge(seller,synch2,by=c('seller'),all.x=T)
rm(synch2)
seller$avg.pct.inventory.changed <- seller$avg.num.same.day.change/seller$num.listings
seller$pct.listings.no.change <- seller$listings.no.change / seller$num.listings
write.csv(synch,'price_change_synchronization.csv')
rm(synch)

## Merge in rating
listings$rating <- gsub("%","",listings$rating)
listings$rating <- as.numeric(listings$rating)
seller.ratings <- ddply(listings[,c('seller','rating','numrating')],.(seller),function(x) c( mean(x$rating,na.rm=T),
                                                                                             mean(x$numrating,na.rm=T) ),
                        .progress='text')
colnames(seller.ratings)[2:3] <- c("avg.rating","avg.num.rating")

seller <- merge(seller,seller.ratings,by=c('seller'))
rm(seller.ratings)

# seller[is.na(seller)] <- 0 # zero out NAs
seller <- seller[order(seller$num.listings,decreasing=T),]

## usable data frames (in decreasing size)
## 1. listings - isbn/dateacc/seller; no extra variables
## 2. price.changes - isbn/dateacc/seller; many extra variables pertaining to changes
## 2. isbn.seller - isbn/seller; number of price increase/decrease/changes
## 3. seller - seller; aggregated versions of variables from 2.

#######################
## TEXT DESCRIPTIONS ##
#######################
path <- "C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\descriptions"
setwd(path)

gram2 <- read.csv('2gram.csv')
gram3 <- read.csv('3gram.csv')
grams <- rbind(gram2[which(gram2$content==1),],gram3[which(gram3$content==1),])
rm(gram2,gram3)

## Merge with seller/isbn level descriptions
## Bring in and clean up descriptions
descriptions <- read.csv("C:\\Users\\Ryan Wang\\Documents\\Dropbox\\amazon\\new_listing_descriptions.csv",stringsAsFactors=F)
descriptions$used <- NULL

descriptions$shipsfrom <- gsub(", United States","",descriptions$shipsfrom,fixed=T)
descriptions$shipsfrom <- gsub("United Kingdom","UK",descriptions$shipsfrom,fixed=T)
descriptions$shipsfrom[which(descriptions$shipsfrom=="United States")] <- NA

## Clean out some junk: (1) overflow from show less/read more (2) punctuation (3) standalone numbers (to remove repeated listings w/ a product indicator)
cleanText = function(doc) {
  doc = gsub("&#171;Show less.+&#187;Read more"," ",doc,perl=T) # remove overflow
  doc = gsub("[[:punct:]]"," ",doc) # remove punctuation
  doc = gsub("(?<=\\s)\\d+(?=\\s)"," ",doc,perl=T) # remove standalone numbers
  # doc = gsub("[0-9]+%"," ",doc) # remove x%
  doc
}
descriptions$comments <- lapply(descriptions$comments,function(x) cleanText(x))
descriptions$comments <- tolower(descriptions$comments)
descriptions$comments <- gsub("[\\s]+"," ",descriptions$comments,perl=T)

usage <- sapply(grams$gram,function(x) as.numeric(grepl(x,descriptions$comments,fixed=T)))
colnames(usage) <- grams$gram

descriptions <- cbind(descriptions,usage)
descriptions <- descriptions[with(descriptions,order(seller,comments,isbn)),]
descriptions <- descriptions[-which(descriptions$seller==""),]
descriptions$phrase.count <- rowSums(descriptions[,as.character(grams$gram)]) ## number of phrases used
descriptions$informative <- as.integer(descriptions$phrase.count > 0) ## indicator for any informative phrases used
descriptions$listing <- 1

descriptions$comments.prev <- c(NA,descriptions$comments[-dim(descriptions)[1]]) ## identify 'generic' descriptions
descriptions$repeat.comment <- with(descriptions,as.integer(comments == comments.prev))
descriptions$repeat.comment[is.na(descriptions$repeat.comment)] <- 0
descriptions$comments.prev <- NULL
rm(usage,grams)

## collapse to seller-level
descriptions2 <- ddply(descriptions,.(seller),function(x) colSums(x[,c(as.character(grams$gram),'repeat.comment','informative','listing')]),.progress='text')
descriptions2 <- descriptions2[,c('seller','repeat.comment','informative','listing')]

descriptions2$pct.generic <- with(descriptions2,repeat.comment/listing)
descriptions2$pct.informative <- with(descriptions2,informative/listing)

descriptions2 <- descriptions2[order(descriptions2$pct.informative,decreasing=T),]

## merge w/ seller
seller <- merge(seller,descriptions2,by=c('seller'))
seller <- seller[order(seller$num.price.change,decreasing=T),]

View(seller[1:100,c('seller',
                    'num.listings',                    
                    'avg.days.bet.change',
                    'avg.rating',
                    'avg.num.rating',
                    'pct.generic',
                    'pct.informative')])

# rm(descriptions,grams,descriptions2,usage)
write.csv(seller,'seller_characteristics.csv',row.names=F)

## Investigate text descriptions
## 1. Biggest sellers use 'generic' descriptions
seller <- seller[order(seller$avg.num.rating,decreasing=T),]

## 2. Fringe sellers experience gains from using informative phrases
fringe <- subset(listings,numrating < 415)
fringe.age <- ddply(fringe,.(seller,isbn),function(x)c(as.character(x$dateacc[1]), ## Calculate the age of each listing
                                                       as.character(x$dateacc[dim(x)[1]]),
                                                       length(unique(x$price2)),
                                                       mean(x$rank))
                    ,.progress='text')
colnames(fringe.age)[3:6] <- c('first','last','num.unique.prices','mean.rank')
fringe.age$first <- as.Date(fringe.age$first,format="%Y-%m-%d")
fringe.age$last <- as.Date(fringe.age$last,format="%Y-%m-%d")
fringe.age$still <- as.integer( fringe.age$last >= as.Date("2012-03-03",format="%Y-%m-%d") )
fringe.age$age <- as.integer(with(fringe.age,last-first+1))
descriptions$num.words <- sapply(gregexpr("\\W+", descriptions$comments), length) + 1

fringe.age <- merge(fringe.age,descriptions[,c('seller','isbn','informative','phrase.count','num.words')],all.x=T)
write.csv(fringe.age,'fringe_age.csv',row.names=F)

lm.tmp <- lm(age~1+log(mean.rank)+still+num.unique.prices+num.words+phrase.count,data=fringe.age) ## This regression is useless
summary(lm.tmp)

## 3. Digression: What about seller ratings?
hist(seller$avg.rating[1:100])
sum(seller$avg.rating > 90, na.rm=T) / sum(!is.na(seller$avg.rating))
sum(seller$pct.informative[1:100] > 0)

###############################################################
###############################################################
###############################################################
###############################################################

## Investigation of firm strategies
## 4/6/2012

seller <- read.csv("C:\\Users\\Ryan Wang\\Desktop\\amazon\\data\\seller_characteristics.csv")

seller$pct.listings.no.change <- seller$listings.no.change / seller$num.listings
seller$log.avg.days.bet.change <- log(seller$avg.days.bet.change+.01)
seller$log.sd.days.bet.change <- log(seller$sd.days.bet.change+.01)
seller$log.avg.tgt.rank <- log(seller$avg.tgt.rank+.01)
seller$log.avg.abs.ranks.bumped <- log(seller$avg.abs.ranks.bumped+.01)
seller$log.avg.rating <- log(seller$avg.rating+.01)
seller$log.avg.num.rating <- log(seller$avg.num.rating+.01)
seller$log.avg.days.observed <- log(seller$avg.days.observed)
seller$log.avg.pct.inventory.changed <- log(seller$avg.pct.inventory.changed)
seller$log.avg.pct.change <- log(seller$avg.pct.change)
seller$log.avg.pct.first.page <- log(seller$avg.pct.first.page)

# seller2 <- subset(seller,avg.num.rating > 0 
#                   & num.price.change > 75 # this makes a difference                 
#                   & !is.na(log.avg.rating) 
#                   & !is.na(sd.days.bet.change)
#                   & avg.abs.ranks.bumped > 0
#                   & num.listings > 0
#                   & avg.days.observed > 0)

## Identify FBA sellers
listings <- read.csv('listings_clean.txt',header=T)
sellers.fba <- unique(subset(listings,fba==1)$seller) ## Which sellers have ever used FBA
tmp <- subset(listings,seller %in% sellers.fba)
tmp$dummy <- 1
tmp2 <- ddply(tmp[,c('seller','isbn','dummy','fba')],.(seller,isbn),function(x) c(sum(x$fba),sum(x$dummy)),.progress='text')
tmp2$fba <- as.integer(tmp2$V1 > 0)
tmp2$dummy <- 1
fba <- ddply(tmp2,.(seller),function(x) c(sum(x$dummy),sum(x$fba)))
fba$pct.fba <- fba[,3]/fba[,2]
fba$V1=fba$V2=NULL
seller <- merge(seller,fba,by=c('seller'),all.x=T)
seller$pct.fba[which(is.na(seller$pct.fba))] <- 0
rm(tmp,tmp2,sellers.fba,fba)

seller2 <- subset(seller,num.price.change > 90) ## quantile(seller$num.price.change,probs=c(.99))
seller2$fba <- as.integer(seller2$pct.fba > .1)

sum(seller$num.price.change[1:dim(seller2)[1]])/sum(seller$num.price.change) ## Remaining N sellers account for X% of the price changes observed in the sample

## Start w/ some distributions
attach(seller2)
hist(avg.days.observed)
hist(avg.pct.first.page)
hist(avg.changes.per.day,breaks=20)
hist(prop.changes.inc)
hist(avg.days.bet.change)
hist(avg.ranks.bumped)
hist(avg.abs.ranks.bumped)
hist(avg.pct.inc)
hist(avg.pct.dec)
hist(avg.pct.change)
hist(avg.tgt.rank)
hist(avg.tgt.pct.diff.lowest)
hist(avg.pct.inventory.changed)
hist(log.avg.rating)
hist(log.avg.num.rating)
detach(seller2)

## Categorization
classify <- c( # 'log.avg.days.observed', ## logged variables
               'log.avg.tgt.rank',
               'log.avg.pct.inventory.changed',
               'log.avg.days.bet.change',
               # 'log.sd.days.bet.change',
               'log.avg.abs.ranks.bumped',                              
               'log.avg.pct.change')
               # 'pct.informative',
               # 'pct.generic',
               # 'avg.pct.first.page',
               # 'log.avg.num.rating')
               # 'log.avg.rating')

classify.nl <- c( # 'avg.days.observed', ## unlogged variables
                  'avg.tgt.rank',                   
                  'avg.days.bet.change',
                  # 'sd.days.bet.change',
                  'avg.abs.ranks.bumped',                                  
                  'avg.pct.change',
                  'avg.pct.inventory.changed',
                  'pct.informative',
                  'pct.generic',
                  'fba',
                  'pct.fba')
                  # 'avg.pct.first.page',
                  # 'avg.num.rating')
                  # 'avg.rating')
                              
seller2.normalized <- scale(seller2[,classify],center=T,scale=T) # normalize for k-means
View(cor(seller2.normalized)) # pairwise correlations
clusters.3 <- kmeans(seller2.normalized,3,iter.max=10000,nstart=100)
clusters.4 <- kmeans(seller2.normalized,4,iter.max=10000,nstart=100)
clusters.3$size

tmp <- clusters.3$cluster ## re-index clusters to correspond to size i.e. cluster 1 is the largest
ordering <- order(clusters.3$size,decreasing=T)
tmp[which(clusters.3$cluster==which(ordering==1))] <- 1
tmp[which(clusters.3$cluster==which(ordering==2))] <- 2
tmp[which(clusters.3$cluster==which(ordering==3))] <- 3
clusters.3$cluster <- tmp
rm(tmp)

seller2$cluster.3 <- clusters.3$cluster

# means and standard errors by cluster
print(colMeans(seller2[which(clusters.3$cluster==1),classify.nl]),digits=2)
print(apply(seller2[which(clusters.3$cluster==1),classify.nl],2,sd),digits=2)
print(colMeans(seller2[which(clusters.3$cluster==2),classify.nl]),digits=2)
print(apply(seller2[which(clusters.3$cluster==2),classify.nl],2,sd),digits=2)
print(colMeans(seller2[which(clusters.3$cluster==3),classify.nl]),digits=2)
print(apply(seller2[which(clusters.3$cluster==3),classify.nl],2,sd),digits=2)

##
## Make plot for usage
pdf('seller-strategy-classification.pdf')
par(mar=c(4,4,1,1)+0.1)
par(mfrow=c(2,2))
## 1: Change Freq ~ Target Rank
with(seller2,plot(log.avg.days.bet.change~log.avg.tgt.rank,col=seller2$cluster.3+1,xaxt='n',yaxt='n',xlab="Target Rank",ylab="Days Since Change",pch=seller2$cluster.3-1))
xlabs <- c(1,5,25,50)
ylabs <- c(5,10,25,100)
with(seller2,axis(side=1,at=log(xlabs+.01),labels=xlabs))
with(seller2,axis(side=2,at=log(ylabs+.01),labels=ylabs))
## 2: Inventory ~ Ranks Bumped
with(seller2,plot(log.avg.pct.inventory.changed~log.avg.abs.ranks.bumped,xlab="Ranks Bumped",ylab="Synchronization (% Inv. Changed)",xaxt='n',yaxt='n',col=seller2$cluster.3+1,pch=seller2$cluster.3-1))
xlabs <- c(.1,1,2.5,5)
ylabs <- c(.01,.1,.5)
with(seller2,axis(side=1,at=log(xlabs+.01),labels=xlabs))
with(seller2,axis(side=2,at=log(ylabs),labels=ylabs*100))
## 3: Size ~ Target Rank
with(seller2,plot(log.avg.pct.change~log.avg.tgt.rank,xlab="Target Rank",ylab="Size of Change (%)",xaxt='n',yaxt='n',col=seller2$cluster.3+1,pch=seller2$cluster.3-1))
xlabs <- c(1,5,25,50)
ylabs <- c(.01,.1,.25,.5)
with(seller2,axis(side=1,at=log(xlabs+.01),labels=xlabs))
with(seller2,axis(side=2,at=log(ylabs),labels=ylabs*100))

frame()
legend('center',legend=c('Cluster 1','Cluster 2','Cluster 3'),col=c(2,3,4),pch=c(0,1,2))
mtext("Note: Variables averaged over",line=-12,cex=.75)
mtext("multiple listings. Color and",line=-13,cex=.75)
mtext("shape indicates cluster. Plots",line=-14,cex=.75)
mtext("shown on log scale.",line=-15,cex=.75)
rm(xlabs,ylabs)
dev.off()

## SET! DON'T CHANGE THE ABOVE!! TESTING SHOULD BE DONE ELSEWHERE OR W/ VERSION CONTROL

seller2$cluster.3 <- clusters.3$cluster
View(seller2[,c('seller','num.listings','avg.tgt.rank','avg.pct.first.page','avg.days.bet.change','pct.informative','cluster.4')])

write.csv(seller2,"sellers_clustered.csv",row.names=F)
rm(classify,classify.nl)

############################################
############################################
############################################

## Try plotting some series
listings <- read.csv('listings_new_3-30.txt',header=F) # bring in listings data
colnames(listings) <- c('dateacc','isbn','seller','rating','fba','numrating','shipping','price','condition','rank')
listings$condition <- NULL
listings$price2 <- listings$price+listings$shipping
listings <- listings[with(listings,order(seller,isbn,dateacc)),]
listings$dateacc <- as.Date(as.character(listings$dateacc),format="%Y-%m-%d")

tmp <- subset(listings,seller==as.character(seller2$seller[12]))
isbns <- unique(tmp$isbn)

with(subset(tmp,isbn==isbns[i]),plot(price2~dateacc,type='l',main=paste(isbns[i])))
with(subset(tmp,isbn==isbns[i]),text(price2~dateacc,labels=as.character(rank),col=rank,cex=0.75))
i = i + 1

## Consider representative sellers (by cluster)
seller2$cluster <- clusters.4$cluster
seller2 <- seller2[order(seller2$cluster,seller2$avg.num.rating,seller2$avg.tgt.rank),]
View(subset(seller2[,c('seller',classify.nl,'avg.num.rating','cluster')],cluster==3))

c2 <- '--textbooksrus--'

tmp <- subset(listings,seller==c2)
isbns <- unique(as.character(tmp$isbn))

with(subset(tmp,isbn==isbns[i]),plot(price2~dateacc,type='l',main=paste(isbns[i])))
with(subset(tmp,isbn==isbns[i]),text(price2~dateacc,labels=as.character(rank),col=rank,cex=0.75))
i = i + 1
View(subset(listings,seller==c2))
plot(price)

## Try PCA reduction first
tmp <- prcomp(seller2[,classify],center=T,scale.=T)
plot(tmp)
tmp <- tmp$x
clusters.4 <- kmeans(tmp[,1:5],4,iter.max=1000)
with(seller2,plot(log.avg.days.bet.change~log.avg.tgt.rank,col=clusters.4$cluster+1))
with(seller2,plot(log.avg.tgt.rank~pct.listings.no.change,col=clusters.4$cluster+1))
                  
#########################                  
## REGRESSION APPROACH ##
#########################
tmp <- lm(log(days.bet.change)~1+first.change+fba+first.page+log(rank.prev)+ranks.bumped+log(numrating+.01),data=subset(price.changes,numrating>100))
summary(tmp)

# random effects - justify?                                    
tmp2 <- lmer(log(days.bet.change)~1+first.change+first.page+fba+(1|isbn)+(1|seller)+log(rank.prev)+log(numrating+.01),price.changes)                  
summary(tmp2)                
                  
# initial hypothesis: days between change is increasing in seller number of ratings
#                     i.e. big sellers change prices less frequently
#                     AND higher ranked listings change prices less frequently
                  
seller.effects <- ranef(tmp2)[[1]]                  
n.seller <- dim(seller.effects)[1]                  
isbn.effects <- ranef(tmp2)[[2]]
n.isbn <- dim(isbn.effects)[1]                  
seller.effects <- sort(seller.effects[1:n.seller,])
isbn.effects <- sort(isbn.effects[1:n.isbn,])                  
plot(seller.effects~qnorm(0:(n.seller-1)/n.seller,mean=mean(seller.effects),sd=sd(seller.effects)))
plot(isbn.effects~qnorm(0:(n.isbn-1)/n.isbn,mean=mean(isbn.effects),sd=sd(isbn.effects)))
abline(0,1)

## STUB::
## Days between change is decreasing in the number of listings
summary(lm(log.avg.days.bet.change~1+avg.tgt.rank+avg.pct.inventory.changed+log(num.listings),seller))

###############################
## CLUSTERING BY SELLER/ISBN ##
###############################
## 5/10/2012

listings <- read.csv('listings_clean.csv',header=T)

## prepare dataset
tmp$days.bet[which(is.na(tmp$days.bet))] <- 1
seller.isbn1 <- ddply(tmp,.(seller,isbn),function(x) c(wtd.sd(x$price2,x$days.bet), # CV of price
                                                      weighted.mean(x$price2,x$days.bet), 
                                                      weighted.mean(x$rank,x$days.bet), # avg. rank                                                      
                                                      sum(x$days.bet,na.rm=T), # days observed
                                                      length(unique(x$price2)), # num. of unique prices
                                                      max(x$price2), # range of prices
                                                      min(x$price2))
                     ,.progress='text')
colnames(seller.isbn1) <- c('seller',
                           'isbn',
                           'price.stdev',
                           'price.mean',
                           'rank.mean',
                           'days.observed',
                           'num.unique.price',
                           'price.max',
                           'price.min')
seller.isbn1$coeff.var <- with(seller.isbn1,price.stdev/price.mean)
seller.isbn1$price.range <- with(seller.isbn1,log(price.max)-log(price.min))

seller.isbn2 <- ddply(tmp2,.(seller,isbn),function(x) c( mean(x$rank), # average targeted rank
                                                         mean(x$days.bet.change), # average days between change
                                                         mean(abs(x$pct.change)), # average percentage change
                                                         sum(x$price.inc), # number of price increases
                                                         sum(x$price.dec), # number of price decreases
                                                         mean(x$ranks.bumped) ) # number of ranks bumped
                      ,.progress='text')
colnames(seller.isbn2) <- c('seller',
                            'isbn',
                            'avg.tgt.rank',
                            'avg.days.bet.change',
                            'avg.pct.change',
                            'num.increase',
                            'num.decrease',
                            'avg.ranks.bumped')
seller.isbn2$num.change <- with(seller.isbn2,num.increase+num.decrease)

seller.isbn <- merge(seller.isbn1,seller.isbn2,all.x=T,by=c('seller','isbn'))
seller.isbn[which(is.na(seller.isbn$avg.tgt.rank)),12:18] <- 0
write.csv(seller.isbn,'seller_isbn.csv',row.names=F)
rm(tmp,tmp2,listings,seller.isbn1,seller.isbn2)

sellers.clustered <- read.csv('sellers_clustered.csv')

bob <- subset(seller.isbn,seller %in% sellers.clustered$seller & num.change > 0 & days.observed > 50)
bob$pct.decrease <- bob$num.decrease/bob$num.change
bob$pct.decrease[which(bob$num.change==0)] <- 0

bob$log.avg.rank <- log(bob$rank.mean)
bob <- subset(bob,log.avg.rank != Inf)
bob$log.num.change <- log(bob$num.change+0.1)
bob$log.num.unique.price <- log(bob$num.unique.price)
bob$log.avg.days.bet.change <- log(bob$avg.days.bet.change+0.1)
bob$log.avg.pct.change <- log(bob$avg.pct.change+0.1)
bob$log.coeff.var <- log(bob$coeff.var+0.1)
bob <- subset(bob,coeff.var != Inf)
bob$log.pct.decrease <- log(bob$pct.decrease+0.1)

## :: Classification :: seller-isbn level
## 5/10/2012

classify <- c('log.avg.rank',
              'log.num.change',
              'log.num.unique.price',
              'log.avg.days.bet.change',
              'log.avg.pct.change',
              'log.coeff.var',
              'log.pct.decrease')            

classify.nl <- c('rank.mean',
                 'num.change',
                 'avg.pct.change',
                 'coeff.var',
                 # 'price.range',
                 'avg.days.bet.change')                                                   

normalized <- scale(bob[,classify],center=T,scale=T)
View(cor(normalized))
system.time( clusters.3 <- kmeans(normalized,3,iter.max=10000,nstart=100) )

##
## Make plots
##

plot(log.avg.days.bet.change~log.coeff.var,data=bob,col=clusters.3$cluster)
plot(log.coeff.var~log.avg.rank,data=bob,col=clusters.3$cluster)

print(colMeans(bob[which(clusters.3$cluster==1),classify.nl]),digits=2)
print(colMeans(bob[which(clusters.3$cluster==2),classify.nl]),digits=2)
print(colMeans(bob[which(clusters.3$cluster==3),classify.nl]),digits=2)
                 