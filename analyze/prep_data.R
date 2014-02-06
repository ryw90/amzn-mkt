## Prepare data
## 4/23/2012

path = "C:\\Users\\Ryan Wang\\Desktop\\amazon\\data"
setwd(path)
rm(path)
library(plyr)

#########################
## CLEAN LISTINGS DATA ##
#########################
## Bring in listings data
listings <- read.csv('listings_new_3-30.txt',header=F) # bring in listings data
colnames(listings) <- c('dateacc','isbn','seller','rating','fba','numrating','shipping','price','condition','rank')
listings$condition <- NULL

## Fill in cases where shipping is NA
listings$shipping[which(is.na(listings$shipping))] <- 0
listings$price2 <- listings$price+listings$shipping
listings$shipping = listings$price = NULL

## Convert numrating to numeric
listings$numrating<-replace(listings$numrating,which(is.na(listings$numrating)),"0")
listings$numrating<-as.numeric(gsub(",","",as.character(listings$numrating)))

## Convert date to date format
listings$dateacc <- as.Date(listings$dateacc,format="%Y-%m-%d")

## Convert rating to numeric
listings$rating <- gsub("%","",listings$rating,fixed=T)
listings$rating <- as.numeric(listings$rating)

## Drop outliers i.e. twice the median price
# median_price <- ddply(listings,.(dateacc,isbn),function(x) median(x$price),.progress="text") # 80 mins
median_price <- read.table('median_price.txt',header=T,sep=",")
listings <- merge(listings,median_price,by=c('dateacc','isbn'))
listings <- subset(listings, (price2 - 3.99) < 2 * price_median)
listings$price_median <- NULL
rm(median_price)

## Drop outliers i.e. twice the list price
profiles2 <- read.csv('profiles2.csv',header=T)
profiles2 <- profiles2[,c('isbn','list.price')]
listings <- merge(listings,profiles2,by=c('isbn'),all.x=T)
listings <- subset(listings,(price2 - 3.99) < 1.5 * list.price | is.na(list.price))
listings$list.price <- NULL
rm(profiles2)

## Eliminate multiple listings (same seller) on the same day
listings <- listings[order(listings$seller,listings$isbn,listings$dateacc),]
listings <- listings[-which(listings$seller==""),] # not reliable
listings$dateacc.prev <- c(NA,listings$dateacc[-dim(listings)[1]])
listings$seller <- listings$seller
listings$isbn <- listings$isbn
listings$isbn.prev <- c(NA,as.character(listings$isbn[-dim(listings)[1]]))
listings$seller.prev <- c(NA,as.character(listings$seller[-dim(listings)[1]]))
listings$duplicates <- as.integer(listings$seller==listings$seller.prev & listings$dateacc==listings$dateacc.prev & listings$isbn==listings$isbn.prev)
listings <- subset(listings,duplicates==0)
listings$rank.prev <- c(NA,listings$rank[-dim(listings)[1]]) ## Generate previous rank (for days between change)
listings$rank.prev[c(1,with(listings,which(seller!=seller.prev | isbn!=isbn.prev)))] <- NA
listings$duplicates=listings$isbn.prev=listings$seller.prev=listings$dateacc.prev=NULL

## Eliminate multiple listings (same rank) on the same day
listings <- listings[order(listings$isbn,listings$dateacc,listings$rank,listings$price),]
listings$dateacc2 <- as.character(listings$dateacc)
listings$dateacc2.prev <- c(NA,listings$dateacc2[-dim(listings)[1]])
listings$rank.l1 <- c(NA,listings$rank[-dim(listings)[1]])
remove <- which(listings$rank.l1==listings$rank & listings$dateacc2.prev==listings$dateacc2)
listings <- listings[-remove,]
rm(remove)
listings$rank.l1=listings$dateacc2.prev=listings$dateacc2=NULL

## Zero out Amazon rating / numrating (have a bug so that sometimes Amazon's rating same as previous seller's)
amzn <- which(listings$seller=="Amazon.com")
listings$rating[amzn] <- 100
listings$numrating[amzn] <- NA
listings$fba[amzn] <- 1
rm(amzn)

## Drop outliers: Lowest-price < $.25 for more than half of the time
tmp <- subset(listings,rank==1)
tmp$below.min <- as.integer(tmp$price2 < 3.99 + .25)
tmp2 <- ddply(tmp,.(isbn),function(x) mean(x$below.min),.progress='text')
tokeep <- tmp2$isbn[tmp2$V1 < .5]
listings <- subset(listings,isbn %in% tokeep)
rm(tokeep,tmp,tmp2)

## Drop outliers: No Amazon listing
amzn.price <- subset(listings,seller == "Amazon.com")
amzn.isbns <- unique(amzn.price$isbn)
listings <- subset(listings,isbn %in% amzn.isbns)
rm(amzn.price,amzn.isbns)

## Generate variable: Days between observations
listings <- listings[with(listings,order(seller,isbn,dateacc)),]
listings$dateacc.prev <- c(NA,listings$dateacc[-dim(listings)[1]])
listings$dateacc.prev[which(is.na(listings$rank.prev))] <- NA
listings$days.bet <- as.integer(listings$dateacc - listings$dateacc.prev)
listings$dateacc.prev <- NULL

## Restrict to dates in the paper
listings <- subset(listings, dateacc >= as.Date("2011-08-30",format="%Y-%m-%d"))

## Write to file
write.csv(listings,'listings_clean.csv',row.names=F)

##########################
## SELLER-LEVEL DATASET ##
##########################
## Generate variable for price changes
tmp <- listings[with(listings,order(seller,isbn,dateacc)),]
tmp$price2.prev <- c(NA,tmp$price2[-dim(tmp)[1]])
tmp$isbn.prev <- c(NA,as.character(tmp$isbn[-dim(tmp)[1]]))
tmp$seller.prev <- c(NA,as.character(tmp$seller[-dim(tmp)[1]]))
tmp$price2.prev[ c(1,with(tmp,which(seller!=seller.prev | isbn!=isbn.prev))) ] <- NA
tmp$price2.change <- tmp$price2 - tmp$price2.prev
tmp$isbn.prev=tmp$seller.prev=NULL

# ## Distance from next lowest price
# tmp <- tmp[with(tmp,order(isbn,dateacc,rank)),]
# tmp$price.next.lowest <- c(NA,tmp$price2[-dim(tmp)[1]])
# tmp$isbn.prev <- c(NA,tmp$isbn[-dim(tmp)[1]])
# tmp$dateacc.prev <- c(NA,tmp$dateacc[-dim(tmp)[1]])
# tmp$price.next.lowest[ with(tmp,which(isbn!=isbn.prev|dateacc!=dateacc.prev)) ] <- NA
# tmp$isbn.prev=tmp$dateacc.prev=NULL
# 
# ## Distance from next highest price
# tmp$price.next.highest <- c(tmp$price2[-1],NA)
# tmp$isbn.next <- c(tmp$isbn[-1],NA)
# tmp$dateacc.next <- c(tmp$dateacc[-1],NA)
# tmp$price.next.highest[ with(tmp,which(isbn!=isbn.next|dateacc!=dateacc.next)) ] <- NA
# tmp$isbn.next=tmp$dateacc.next=NULL

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

# ## Indicator for first price change, since we don't know when the first price change occurred
# tmp2$isbn.prev <- c(NA,tmp2$isbn[-dim(tmp2)[1]])
# tmp2$seller.prev <- c(NA,tmp2$seller[-dim(tmp2)[1]])
# tmp2$first.change <- 0
# first <- which(tmp2$isbn!=tmp2$isbn.prev|tmp2$seller!=tmp2$seller.prev) + 1
# tmp2$first.change[ first[-length(first)] ] <- 1
# tmp2$first.change[1] <- 1
# tmp2$first.change[which(is.na(tmp2$price2.change))] <- NA
# rm(first)
# tmp2$isbn.prev=tmp2$seller.prev=NULL
# 
# tmp2$days.bet.change2 <- tmp2$days.bet.change ## days between change, ignoring first change
# tmp2$days.bet.change2[which(tmp2$first.change==1)] <- NA

## Ranks 'bumped' since last price change
tmp2$rank.last.change <- c(NA,tmp2$rank[-dim(tmp2)[1]])
tmp2$rank.last.change[is.na(tmp2$price2.change)] <- NA
tmp2$ranks.bumped <- tmp2$rank.prev - tmp2$rank.last.change

tmp2 <- tmp2[-which(is.na(tmp2$price2.change)),] ## clear out is.na
price.changes <- tmp2
rm(tmp,tmp2)

# ## Distance from lowest price
# lowest.price <- subset(listings,rank==1)[,c('dateacc2','isbn','price2')]
# colnames(lowest.price)[3] <- 'price.lowest'
# 
# ## Distance from Amazon
# amzn.price <- subset(listings,seller=="Amazon.com")[,c('dateacc2','isbn','price2')]
# colnames(amzn.price)[3] <- 'price.amzn'
# 
# special.prices <- merge(amzn.price,lowest.price,by=c('dateacc2','isbn'),all.y=T)
# special.prices <- special.prices[order(special.prices$isbn,special.prices$dateacc2),]
# write.csv(special.prices,'special_prices.csv',row.names=F)

# price.changes <- merge(price.changes,special.prices,by=c('dateacc2','isbn'))
# rm(lowest.price,amzn.price,special.prices)

# ## Convert to distance in percent
# price.changes$pct.diff.next.lowest <- with(price.changes,(price2-price.next.lowest)/price2)
# price.changes$pct.diff.next.highest <- with(price.changes,(price2-price.next.highest)/price2)
# price.changes$pct.diff.lowest <- with(price.changes,(price2-price.lowest)/price2)
# price.changes$pct.diff.amzn <- with(price.changes,(price2-price.amzn)/price2)
# price.changes <- price.changes[with(price.changes,order(seller,isbn,dateacc)),]

## Collapse to isbn/seller
isbn.seller <- ddply(price.changes,.(seller,isbn),function(x) c( sum(x$price.inc), # number of price increases
                                                                 sum(x$price.dec)) # number of price decreases
                     ,.progress='text')
colnames(isbn.seller)[3:4] <- c('num.price.inc','num.price.dec')
isbn.seller$num.price.change <- isbn.seller$num.price.inc + isbn.seller$num.price.dec

## Bind number of days observed
days.obs <- ddply(listings[,c('isbn','seller','days.bet')],.(isbn,seller),function(x) sum(x$days.bet,na.rm=T),.progress='text')
colnames(days.obs)[3] <- 'days.observed'
days.obs$days.observed <- days.obs$days.observed + 1
write.csv(days.obs,"days_obs.csv",row.names=F)
isbn.seller <- merge(isbn.seller,days.obs,by=c('isbn','seller'),all.x=T)
rm(days.obs)

# ## Percent of time spent on first page (requires listings data frame)
# listings <- listings[order(listings$seller,listings$isbn,listings$dateacc),]
# listings$dateacc.next <- c(listings$dateacc[-1],NA)
# listings$isbn.next <- c(listings$isbn[-1],NA)
# listings$seller.next <- c(listings$seller[-1],NA)
# tona <- with(listings,which(isbn!=isbn.next|seller!=seller.next))
# listings$dateacc.next[tona] <- NA
# listings$days.bet <- as.integer(listings$dateacc.next-listings$dateacc)
# listings$days.first.page <- listings$first.page * listings$days.bet
# rm(tona)
# listings$isbn.next=listings$seller.next=listings$dateacc.next=NULL
# first.page <- ddply(listings[,c('isbn','seller','days.bet','days.first.page')],.(seller,isbn),function(x) c(sum(x$days.bet,na.rm=T),
#                                                                                                             sum(x$days.first.page,na.rm=T)),
#                     .progress='text')
# colnames(first.page)[3:4] <- c('days.observed','days.first.page')
# isbn.seller <- merge(isbn.seller,first.page,by=c('seller','isbn'),all.x=T,all.y=T) ## merge

## Generate variables to cluster with
isbn.seller[is.na(isbn.seller)] <- 0
# isbn.seller$pct.first.page <- with(isbn.seller, days.first.page/days.observed)
isbn.seller$changes.per.day <- with(isbn.seller, num.price.change/days.observed)
isbn.seller$inc.per.day <- with(isbn.seller, num.price.inc/days.observed)
isbn.seller$dec.per.day <- with(isbn.seller, num.price.dec/days.observed)
isbn.seller$no.change <- as.integer(isbn.seller$num.price.change == 0)

## Seller variables (aggregated)
seller1 <- ddply(isbn.seller,.(seller), function(x) c( colSums(x[,c(3:5,10)],na.rm=T),
                                                       colMeans(x[,6:9],na.rm=T) ),
                 .progress='text')
seller1$prop.changes.inc <- seller1$num.price.inc / seller1$num.price.change
seller1[is.na(seller1)] <- 0
colnames(seller1)[2:10] <- c( 'num.price.inc', # 2. total number of price increases
                              'num.price.dec', # 3. total number of price decreases
                              'num.price.change', # 4. total number of price changes
                              'listings.no.change', # 5. total number of listings with no price change
                              'avg.days.observed', # 6. average number of days observed (over listings)                              
                              'avg.changes.per.day', # 7. average number of changes per day (over listings)
                              'avg.inc.per.day', # 8. average number of increases per day (over listings)
                              'avg.dec.per.day', # 9. average number of decreases per day (over listings)
                              'prop.changes.inc') # 10. proportion of changes that were increases (over all changes)

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
                                                        sd(x$rank)) # std. dev. of targeted rank
                                                        # mean(x$pct.diff.next.lowest,na.rm=T), # average targeted distance from next lowest
                                                        # mean(x$pct.diff.next.highest,na.rm=T), # average targeted distance from next highest
                                                        # mean(x$pct.diff.lowest,na.rm=T), # average targeted distance from lowest
                                                        # mean(x$pct.diff.amzn,na.rm=T)) # average targeted distance from amazon
                 ,.progress='text')
colnames(seller3)[2:3] <- c( 'avg.tgt.rank', # average rank immediately following change (over all changes)
                             'avg.sd.tgt.rank') # average sd of rank immediately following change (over all changes)
#                              'avg.tgt.pct.diff.next.lowest', # average percent difference from next lowest immediately following change (over all changes)
#                              'avg.tgt.pct.diff.next.highest', # average percent difference from next high immediately following change (over all changes)
#                              'avg.tgt.pct.diff.lowest', # average percent difference from lowest immediately following change (over all changes)
#                              'avg.tgt.pct.diff.amzn') # average percent difference from Amazon immediately following change (over all changes)
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
rm(seller1,seller2,seller3,num.listings)

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
write.csv(seller.ratings,'seller_ratings.csv',row.names=F)

seller <- merge(seller,seller.ratings,by=c('seller'))
rm(seller.ratings)

# seller[is.na(seller)] <- 0 # zero out NAs
seller <- seller[order(seller$num.listings,decreasing=T),]
write.csv(seller,'seller_characteristics.csv',row.names=F)

