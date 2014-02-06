## OLS Model for price variability
## 4/17/12

################################
## DATA CLEANING: TO BE MOVED ##
################################

## Read in listings data
listings <- read.csv('C:\\Users\\Ryan Wang\\Desktop\\amazon\\data\\listings_clean.csv')
# listings$first.page = listings$days.bet = listings$price.change = listings$price2.prev = listings$indic.change = listings$rank.prev = NULL

## Bring in seller data
seller <- read.csv('C:\\Users\\Ryan Wang\\Desktop\\amazon\\data\\seller_characteristics.csv')

## Bring in book source data
isbns <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\isbns master.txt',header=T)
colnames(isbns)[1] <- 'isbn'

## Bring in book profiles data
profiles <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\profiles.csv',header=F)
colnames(profiles) <- c('dateacc','isbn','num.review','book.rating','sales.rank','price.amzn')
profiles$price.amzn = NULL

tmp <- subset(profiles, (dateacc=="2012-04-11" | dateacc == "2012-04-12") & is.na(sales.rank) ) ## Drop isbns w/ no sales rank as of 4/12/12
profiles <- subset(profiles, !(isbn %in% unique(tmp$isbn)))
listings <- subset(listings, !(isbn %in% unique(tmp$isbn)))
rm(tmp)

## Bring in more book profiles data (including list price)
profiles2 <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\profiles2.csv',header=T)

## Bring in NYT list data
nyt.list <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\nytimes bestsellers\\nyt_bestsellers.csv',header=T)
nyt.list <- nyt.list[,c('display_name','primary_isbn10')]
nyt.list <- nyt.list[!duplicated(nyt.list),]
colnames(nyt.list)[2] <-'isbn'

## Remove isbns with unknown source
profiles <- subset(profiles, profiles$isbn %in% isbns$isbn)
listings <- subset(listings, listings$isbn %in% isbns$isbn)

listings <- listings[with(listings,order(isbn,dateacc,rank)),]

######################
## DATA PREPARATION ##
######################

## Lowest price
lowest.price <- subset(listings,rank==1)
colnames(lowest.price)[3] <- 'seller.lowest'
colnames(lowest.price)[8] <- 'price.lowest'

## Lowest price, of listings where seller has at least MINRAT ratings
# quantile(seller$avg.num.rating,probs=c(.9))
MINRAT <- 415 ## 415 is the 90th percentile
tmp <- subset(listings,numrating >= MINRAT & rank < 10 | seller == "Amazon.com")
lowest.minrat<- ddply(tmp,.(isbn,dateacc),function(x) min(x$price2),.progress='text') ## Robustness 1: Lowest non-fringe price
colnames(lowest.minrat)[3] <- 'price.lowest.minrat'
rm(tmp)

## Average of three lowest prices
tmp <- subset(listings,rank <= 3)
lowest.avg3 <- ddply(tmp,.(isbn,dateacc),function(x) mean(x$price2),.progress='text') ## Robustness 2: Average of 3 lowest prices
colnames(lowest.avg3)[3] <- 'price.lowest.avg3'
rm(tmp)

## Amazon's price
amzn.price <- subset(listings,seller=="Amazon.com")
colnames(amzn.price)[8] <- 'price.amzn'

## Combine all the above
tmp <- merge(lowest.price,amzn.price[,c('isbn','dateacc','price.amzn')]) ## Remove all ISBNs for which Amazon has never listed a price
tmp <- merge(tmp,lowest.minrat,by=c('isbn','dateacc'))
tmp <- merge(tmp,lowest.avg3,by=c('isbn','dateacc'))
tmp <- merge(tmp,profiles,by=c('isbn','dateacc'),all.x=T)
lowest <- tmp

write.csv(lowest.minrat,"lowest_minrat.csv",row.names=F)
write.csv(lowest.avg3,"lowest_avg3.csv",row.names=F)
rm(tmp,lowest.price,lowest.minrat,lowest.avg3,amzn.price,profiles,isbns)

lowest <- lowest[order(lowest$isbn,lowest$dateacc),]

## Collapse to ISBN-level dataset (For now: just price.lowest)
lowest$dateacc.prev <- c(NA,lowest$dateacc[-dim(lowest)[1]])
lowest$days.bet <- as.integer(lowest$dateacc) - as.integer(lowest$dateacc.prev) # days between observations
lowest$isbn.prev <- c(NA,as.character(lowest$isbn[-dim(lowest)[1]]))
lowest$days.bet[which(lowest$isbn!=lowest$isbn.prev)] <- 1
lowest$days.bet[1] <- 1
lowest$dateacc.prev=lowest$isbn.prev=NULL

wtd.sd <- function(x.vector,x.weight,na.rm=T) { ## Calculate 'weighted' SD
  N <- sum(x.weight,na.rm=T)
  x.bar <- sum(x.weight*x.vector,na.rm=T)/N
  sqrt( 1/(N-1)*sum(x.weight*(x.vector-x.bar)^2,na.rm=T) )
}

lowest.var <- ddply(lowest,.(isbn), function(x) c( min(x$price.lowest), ## max-min/min
                                                   max(x$price.lowest),
                                                   weighted.mean(x$price.lowest,x$days.bet), ## mean, controlling for days observed
                                                   wtd.sd(x$price.lowest,x$days.bet), ## sd, controlling for days observed
                                                   mean(x$num.review,na.rm=T), ## Some ISBN-level controls
                                                   mean(x$book.rating,na.rm=T),
                                                   weighted.mean(x$sales.rank,x$days.bet,na.rm=T),
                                                   wtd.sd(x$sales.rank,x$days.bet),
                                                   weighted.mean(x$price.amzn,x$days.bet,na.rm=T),
                                                   wtd.sd(x$price.amzn,x$days.bet),
                                                   min(x$price.lowest.minrat), ## alternative measures
                                                   max(x$price.lowest.minrat),
                                                   weighted.mean(x$price.lowest.minrat,x$days.bet),
                                                   wtd.sd(x$price.lowest.minrat,x$days.bet),
                                                   weighted.mean(x$price.lowest.avg3,x$days.bet), ## average of 3 lowest
                                                   wtd.sd(x$price.lowest.avg3,x$days.bet))
                    ,.progress='text')
colnames(lowest.var) <- c('isbn',
                          'price.lowest.min',
                          'price.lowest.max',
                          'mean.lowest',
                          'sd.lowest',
                          'avg.num.reviews',
                          'avg.ratings',
                          'avg.sales.rank',
                          'sd.sales.rank',
                          'avg.price.amzn',
                          'sd.price.amzn',
                          'price.lowest.minrat.min',
                          'price.lowest.minrat.max',
                          'mean.minrat',
                          'sd.minrat',
                          'mean.avg3',
                          'sd.avg3')
lowest.var$pct.diff.max.min <- with(lowest.var,(price.lowest.max-price.lowest.min)/price.lowest.min) ## percent difference, max to min
lowest.var$coeff.var <- with(lowest.var,sd.lowest/mean.lowest) ## coefficient of variation
lowest.var$coeff.var.amzn <- with(lowest.var,sd.price.amzn/avg.price.amzn)
lowest.var$pct.diff.minrat <- with(lowest.var,(price.lowest.minrat.max-price.lowest.minrat.min)/price.lowest.minrat.min)
lowest.var$coeff.var.minrat <- with(lowest.var,sd.minrat/mean.minrat)
lowest.var$coeff.var.avg3 <- with(lowest.var,sd.avg3/mean.avg3)
lowest.var <- lowest.var[order(lowest.var$pct.diff.max.min,decreasing=T),]

lowest.var <- subset(lowest.var,price.lowest.min > 0.1) ## Ignore listings with prices too low

## Generate ISBN-level covariates
listings$dummy <- 1
listings$fba[which(listings$seller == "Amazon.com")] <- 1
listings$dateacc <- as.Date(listings$dateacc,format="%Y-%m-%d")
tmp <- subset(listings,isbn %in% unique(lowest.var$isbn))
isbn.date <- ddply(tmp,.(isbn,dateacc),function(x) c( sum(x$dummy), ## Number of sellers
                                                           sum(x$numrating >= MINRAT | x$seller=="Amazon.com"), ## Number of non-fringe sellers
                                                           sum(x$fba)) ## Number of FBA sellers (more price competition)
                                                           ## STUB Number of sellers by cluster?
                   ,.progress='text')
colnames(isbn.date)[3:5] <- c('num.sellers','num.sellers.minrat','num.fba')
rm(tmp)
isbns <- ddply(isbn.date,.(isbn), function(x) colMeans(x[,3:5]), .progress='text') ## Collapse to ISBN-level
write.csv(isbn.date,"isbn_date.csv",row.names=F)
rm(isbn.date)

## Generate covariate - number of sellers by cluster
seller2 <- read.csv("sellers_clustered.csv",header=T) 
tmp <- subset(listings,seller %in% unique(seller2$seller))
tmp <- tmp[,c('dateacc','isbn','seller')]
tmp <- merge(tmp,seller2[,c('seller','cluster.3')],all.x=T)
tmp$c1 <- as.integer(tmp$cluster.3==1)
tmp$c2 <- as.integer(tmp$cluster.3==2)
tmp$c3 <- as.integer(tmp$cluster.3==3)
num.cluster <- ddply(tmp,.(isbn,dateacc),function(x) colSums(x[,c('c1','c2','c3')]),.progress='text')
write.csv(num.cluster,"num_cluster.csv",row.names=F)
rm(tmp)

num.cluster.isbn <- ddply(num.cluster,.(isbn),function(x) colMeans(x[,3:5])) ## collapse to ISBN level

## Generate covariate - number of excluded sellers
tmp <- subset(listings,!(seller %in% unique(seller2$seller)))
tmp <- tmp[,1:3]
tmp$dummy <- 1
tmp2 <- ddply(tmp,.(isbn,dateacc),function(x) sum(x$dummy),.progress='text')
num.excluded <- ddply(tmp2,.(isbn),function(x) mean(x[,3]))
colnames(num.excluded)[2] <- 'num.sellers.excluded'
write.csv(num.excluded,'num_excluded.csv',row.names=F)
rm(tmp,tmp2)

## Merge isbns with lowest.var
tmp <- merge(isbns,lowest.var)
tmp <- merge(tmp,num.cluster.isbn,all.x=T)
tmp <- merge(tmp,num.excluded,all.x=T)
rm(num.cluster.isbn,num.excluded)

## Merge in extraneous ISBN-level data
isbns.master <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\isbns master.txt',header=T)
colnames(isbns.master)[1] <- 'isbn'
tmp <- merge(tmp,isbns.master,by=c('isbn'),all.x=T)

tmp <- merge(tmp,nyt.list,by=c('isbn'),all.x=T)
tmp$display_name <- as.character(tmp$display_name)
tmp$display_name[which(is.na(tmp$display_name))] <- as.character(tmp$source[which(is.na(tmp$display_name))])
tmp$source <- NULL
tmp <- merge(tmp,profiles2[,c('isbn','list.price','format','category.1')],by=c('isbn'),all.x=T)
fluctuation <- tmp
rm(isbns,lowest.var,nyt.list,isbns.master,tmp,profiles2)

## Clean up certain variables
fluctuation$sd.lowest[which(is.na(fluctuation$sd.lowest))] <- 0 ## Lowest price never changed
fluctuation$sd.sales.rank[which(is.na(fluctuation$sd.sales.rank))] <- 0 ## Sales rank never changed
fluctuation$no.amzn.change <- as.integer(is.na(fluctuation$sd.price.amzn))
fluctuation$sd.price.amzn[which(is.na(fluctuation$sd.price.amzn))] <- 0 ## Amazon price never changed
fluctuation$coeff.var[which(is.na(fluctuation$coeff.var))] <- 0 ## Amazon price never changed
fluctuation$coeff.var.amzn[which(is.na(fluctuation$coeff.var.amzn))] <- 0 ## Amazon price never changed

fluctuation$no.list.price <- as.integer(is.na(fluctuation$list.price))
fluctuation$list.price[which(is.na(fluctuation$list.price))] <- 0

fluctuation <- fluctuation[-which(duplicated(fluctuation)),] ## remove duplicates
fluctuation <- fluctuation[-which(duplicated(fluctuation$isbn)),] ## remove duplicate isbns -- I think these just have different list names

## Generate covariate: How often are sellers displaced in the top 15
tmp <- subset(listings,isbn %in% fluctuation$isbn)
tmp <- subset(tmp,rank <= 15)
tmp <- tmp[order(tmp$isbn,tmp$rank,tmp$dateacc),]

tmp$seller.prev <- c(NA,as.character(tmp$seller[-dim(tmp)[1]]))
tmp$displaced.prev.seller <- as.integer(tmp$seller != tmp$seller.prev) ## 1 if this seller displaced the previous seller in this rank
tmp$isbn.prev <- c(NA,as.character(tmp$isbn[-dim(tmp)[1]]))
tmp$rank.prev <- c(NA,tmp$rank[-dim(tmp)[1]])
tmp$displaced.prev.seller[with(tmp,which(isbn!=isbn.prev | rank!=rank.prev))] <- NA ## Zero out certain entries
tmp$seller.prev=tmp$isbn.prev=tmp$rank.prev=NULL

tmp <- subset(tmp,!is.na(displaced.prev.seller))
tmp <- tmp[order(tmp$isbn,tmp$dateacc,tmp$rank),]

displaced <- ddply(tmp,.(isbn,dateacc),function(x) colSums(x[,c('dummy','displaced.prev.seller')]),.progress='text') ## collapse
lowest <- merge(lowest,displaced,all.x=T)
displaced.isbn <- ddply(lowest,.(isbn),function(x) weighted.mean(x$displaced.prev.seller,x$days.bet,na.rm=T))
colnames(displaced.isbn)[2] <- 'avg.displaced'
write.csv(displaced,'displaced.csv',row.names=F)
rm(tmp)

## Merge displaced with fluctuation
fluctuation <- merge(fluctuation,displaced.isbn)
rm(displaced.isbn,displaced)

fluctuation <- fluctuation[order(-fluctuation$pct.diff.max.min),]
write.csv(fluctuation,'fluctuation.csv',row.names=F)

#####################
## SUMMARY / PLOTS ##
#####################
fluctuation$num.sellers.rounded <- round(fluctuation$num.sellers)
fluctuation$num.sellers.minrat.rounded <- round(fluctuation$num.sellers.minrat)
fluctuation$num.fba.rounded <- round(fluctuation$num.fba)
fluctuation$num.sellers.excluded.rounded <- round(fluctuation$num.sellers.excluded)
fluctuation$c1.rounded <- round(fluctuation$c1)
fluctuation$c2.rounded <- round(fluctuation$c2)
fluctuation$c3.rounded <- round(fluctuation$c3)

## Plot variability vs. number of sellers
p1 <- ddply(fluctuation,.(num.sellers.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)) )
colnames(p1)[2:3] <- c('pct.diff.max.min','coeff.var')

p2 <- ddply(fluctuation,.(num.sellers.minrat.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)))
colnames(p2)[2:3] <- c('pct.diff.max.min','coeff.var')

p3 <- ddply(fluctuation,.(num.fba.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)))
colnames(p3)[2:3] <- c('pct.diff.max.min','coeff.var')

p.c1 <- ddply(fluctuation,.(c1.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)))
p.c2 <- ddply(fluctuation,.(c2.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)))
p.c3 <- ddply(fluctuation,.(c3.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)))
p.excl <- ddply(fluctuation,.(num.sellers.excluded.rounded),function(x) c(mean(x$pct.diff.max.min),mean(x$coeff.var)))
colnames(p.c1)[2:3] = colnames(p.c2)[2:3] = colnames(p.c3)[2:3] = colnames(p.excl)[2:3] = c('pct.diff.max.min','coeff.var')

pdf('variability-by-num-seller.pdf',width=8,height=5)
par(mar=c(5,4,3,1)+0.1)
plot(pct.diff.max.min~num.sellers.rounded,data=subset(p1,num.sellers.rounded < 55),type='l',ylim=c(-.2,1.5),xlab="Number of Sellers",ylab="% Difference (Max to Min)")
# lines(pct.diff.max.min~num.sellers.minrat.rounded,data=p2,col='red',lty=2)
mtext('All Sellers',side=3,line=1,cex=1.25)
# legend('bottomright',legend=c('All Sellers','Excluding Fringe'),col=c('black','red'),lty=c(1,2))
# mtext('Note: Sellers with less than 415 ratings (90th percentile) constitute fringe sellers.',side=1,line=4,cex=.75)
dev.off()

pdf('variability-by-num-clustered.pdf',width=8,height=5)
par(mar=c(6,4,3,1)+0.1)
plot(pct.diff.max.min~c1.rounded,p.c1,type='l',xlab="Number of Sellers",ylab="% Difference (Max to Min)",xaxt='n')
lines(pct.diff.max.min~c2.rounded,p.c2,type='l',col=2,lty=2)
lines(pct.diff.max.min~c3.rounded,p.c3,type='l',col=4,lty=4)
lines(pct.diff.max.min~num.sellers.excluded.rounded,p.excl,col=5,lty=5)
axis(1,at=0:13,tick=T,labels=T)
legend('bottomright',legend=c('Low Rank, Frequent Change','High Rank, Infrequent Change','High Rank, Frequent Change','Excluded from Clustering'),lty=c(1:2,4:5),col=c(1:2,4:5))
mtext('Sellers By Type',side=3,line=1,cex=1.25)
mtext('Note: Limits of x-axis restricted by number of low rank, frequent change sellers.',side=1,line=4,cex=.75)
dev.off()

rm(p1,p2,p3,p.c1,p.c2,p.c3,p.excl)

###################
## PRELIMINARIES ##
###################

## Difference between highest and lowest price
lowest.minrat <- read.csv("lowest_minrat.csv",header=T)
lm.var <- ddply(lowest.minrat,.(isbn),function(x) c(min(x$price.lowest.minrat,na.rm=T),max(x$price.lowest.minrat,na.rm=T)))
colnames(lm.var)[2:3] <- c('min.price.lowest.minrat','max.price.lowest.minrat')
lm.var$pct.diff <- (lm.var[,3]-lm.var[,2])/lm.var[,2]
lm.var <- lm.var[order(-lm.var$pct.diff),]
lm.var <- subset(lm.var,isbn %in% unique(fluctuation$isbn) & min.price.lowest.minrat > .1)

lm.var <- subset(lm.var,max.price.lowest.minrat < 300) ## Some weird stuff going on in certain ISBNs...

median(lm.var$pct.diff)                                 

## Rule out possibility that price changes explained away by Amazon changes / demand changes
lowest.price <- subset(listings,rank==1)
colnames(lowest.price)[8] <- 'price.lowest'
amzn.price <- subset(listings,seller=="Amazon.com")
colnames(amzn.price)[8] <- 'price.amzn'

explain <- merge(lowest.price[,c(1:2,8)],amzn.price[,c(1:2,8)])
rm(lowest.price,amzn.price)

profiles <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\profiles.csv',header=F)
colnames(profiles) <- c('dateacc','isbn','reviews','rating','rank','currprice')
explain <- merge(explain,profiles[,c(1:2,5)],all.x=T)
rm(profiles)

explain <- explain[order(explain$isbn,explain$dateacc),]
N <- dim(explain)[1]
explain$isbn.prev <- c(NA,as.character(explain$isbn[-N]))
explain$price.lowest.prev <- c(NA,explain$price.lowest[-N])
explain$price.amzn.prev <- c(NA,explain$price.amzn[-N])
explain$rank.prev <- c(NA,explain$rank[-N])
rm(N)

remove <- which(explain$isbn != explain$isbn.prev)
explain <- explain[-c(1,remove),]
rm(remove)

explain$log.diff.lowest <- with(explain,log(price.lowest) - log(price.lowest.prev))
explain$log.diff.amzn <- with(explain,log(price.amzn) - log(price.amzn.prev))
explain$log.diff.rank <- with(explain,log(rank) - log(rank.prev))

tmp <- lm(log.diff.lowest ~ 1 + log.diff.amzn + log.diff.rank, data = explain)

######################
## REGRESSION MODEL ##
######################
save <- fluctuation
fluctuation <- subset(save,avg.sales.rank < 1000000 & num.sellers >= 2)

## Regression 1: Lowest price variability (max to min) as a function of number of sellers
collapsed.lm1 <- lm( 100 * pct.diff.max.min ~ 1 + factor(display_name) + log(list.price+1) + no.list.price + avg.ratings + log(avg.sales.rank) + log(sd.sales.rank+1) + coeff.var.amzn + num.sellers.excluded + c1 + c2 + c3, data = fluctuation)
summary(collapsed.lm1)

## Regression 2: Coefficient of variation as a function of number of sellers
collapsed.lm2 <- lm( 100 * coeff.var ~ 1 + factor(display_name) + log(list.price+1) + no.list.price + avg.ratings + log(avg.sales.rank) + log(sd.sales.rank+1) + coeff.var.amzn + num.sellers.excluded + c1 + c2 + c3, data = fluctuation)
summary(collapsed.lm2)

## Try with different response
collapsed.lm3 <- lm( 100 * coeff.var.minrat ~ 1 + factor(display_name) + log(list.price+1) + no.list.price + avg.ratings + log(avg.sales.rank) + log(sd.sales.rank+1) + coeff.var.amzn + num.sellers.excluded + c1 + c2 + c3, data = fluctuation)
summary(collapsed.lm3)

collapsed.lm4 <- lm( 100 * coeff.var.avg3 ~ 1 + factor(display_name) + log(list.price+1) + no.list.price + avg.ratings + log(avg.sales.rank) + log(sd.sales.rank+1) + coeff.var.amzn + num.sellers.excluded + c3 + c2 + c1, data = fluctuation)
summary(collapsed.lm4)

## Try separating by source
tmp <- subset(fluctuation,display_name!="Bookrags")
collapsed.lm5 <- lm( 100 * coeff.var.avg3 ~ 1 + log(list.price+1) + no.list.price + avg.ratings + log(avg.sales.rank) + log(sd.sales.rank+1) + coeff.var.amzn + num.sellers.excluded + c3 + c2 + c1, data = tmp)
summary(collapsed.lm5)
