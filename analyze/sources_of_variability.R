## Models for predictability of pricing strategy
## 4/16/12

###################
## BRING IN DATA ##
###################
path <- "C:\\Users\\Ryan Wang\\Desktop\\amazon\\data"
setwd(path)
rm(path)

listings <- read.csv('listings_clean.csv',header=T)
seller <- read.csv('seller_characteristics.csv')
seller2 <- read.csv('sellers_clustered.csv')

## Open question: consider sellers that change price at least once? Or only those clustered?
listings <- subset(listings,seller %in% seller2$seller)
listings$days.bet[which(is.na(listings$days.bet))] <- 1

#########################################
## MODEL: COEFF OF VAR PER SELLER/ISBN ##
#########################################
wtd.sd <- function(x.vector,x.weight,na.rm=T) { ## Calculate 'weighted' SD
  N <- sum(x.weight,na.rm=T)
  x.bar <- sum(x.weight*x.vector,na.rm=T)/N
  sqrt( 1/(N-1)*sum(x.weight*(x.vector-x.bar)^2,na.rm=T) )
}

## Generate response: CV_ij where i indicates seller and j indicates ISBN
bob <- ddply(listings,.(isbn,seller),function(x) c(wtd.sd(x$price2,x$days.bet),weighted.mean(x$price2,x$days.bet),weighted.mean(x$rank,x$days.bet)),.progress='text')
colnames(bob)[3:5] <- c('stdev.price','mean.price','mean.rank')
bob <- bob[-which(is.na(bob$stdev.price)),]
bob <- bob[-which(bob$stdev.price == Inf),]
bob <- bob[-which(is.na(bob$mean.price)),]
write.csv(bob,'book_seller_variability.csv',row.names=F)

bob$cv <- bob$stdev.price / bob$mean.price

lm.var <- lm(100*cv~1+factor(display_name)+c1+c2+c3+num.sellers.excluded+log(avg.sales.rank+0.1)
             +log(sd.sales.rank+0.1)+log(list.price+0.1)+as.integer(list.price==0)+coeff.var.amzn+log(num.listings)+avg.days.bet.change
             +avg.tgt.rank+avg.pct.inventory.changed,
             subset(bob,cluster.3 == 1))
summary(lm.var)

## Generate response: indicator for price change
listings <- listings[order(listings$seller,listings$isbn,listings$dateacc),]
listings$price2.prev <- c(NA,listings$price2[-dim(listings)[1]])
listings$price.change <- listings$price2 - listings$price2.prev

listings$seller.prev <- c(NA,as.character(listings$seller[-dim(listings)[1]]))
listings$isbn.prev <- c(NA,as.character(listings$isbn[-dim(listings)[1]]))
listings$price.change[with(listings,which(seller != seller.prev | isbn != isbn.prev))] <- NA
listings$seller.prev=listings$isbn.prev = NULL

listings$indic.change <- as.integer(listings$price.change != 0)

## Generate covariate: number of ranks bumped since last change
## TODO: this is hard...
listings$rank.prev <- c(NA,listings$rank[-dim(listings)[1]])
listings$rank.prev[is.na(listings$price.change)] <- NA

## TODO: fill in rank.last.change

notna <- which(listings$indic.change == 1 | is.na(listings$indic.change)) # indices of observed changes
fillin <- sapply(1:dim(listings)[1],function(i) max(which(i>=notna)))
tmp <- listings$rank[notna] # intermediate step
tmp$rank.last.change <- tmp[fillin] # fill in price

# change <- subset(listings,indic.change==1)
# change$rank.last.change <- c(NA,change$rank[-dim(change)[1]])
# change$isbn.prev <- c(NA,as.character(change$isbn[-dim(change)[1]]))
# change$seller.prev <- c(NA,as.character(change$seller[-dim(change)[1]]))
# change$rank.last.change[with(change,which(isbn != isbn.prev | seller != seller.prev))] <- NA
# change$isbn.prev=change$seller.prev=NULL
# 
# listings <- listings[with(listings,order(-indic.change,seller,isbn,dateacc)),] ## bind variables from change to listings
# tmp <- cbind(listings[1:dim(change)[1],],change$rank.last.change)
# colnames(tmp)[17] <- 'rank.last.change'
# rm(change)
# tmp2 <- listings[(dim(tmp)[1]+1):dim(listings)[1],]
# tmp2$rank.last.change <- NA
# listings <- rbind(tmp,tmp2)
# rm(tmp,tmp2)

## Bring in seller data
seller <- read.csv('C:\\Users\\Ryan Wang\\Desktop\\amazon\\data\\seller_characteristics.csv')

tmp2 <- merge(tmp,seller,all.x=T,by=c('seller'))

#######################
## REGRESSION MODELS ##
#######################
probit.cat <- glm(indic.change ~ 1 + days.bet + rating + fba + numrating + no.rating + ranks.bumped, family=binomial(link="probit"), data=listings)
summary(probit.cat)

lm.cat <- glm(indic.change ~ 1 + days.bet + rating + fba + log(numrating+.01) + no.rating + ranks.bumped, family=binomial(link='probit'), data = listings[1:10000,])
summary(lm.cat)

tmp <- subset(listings,numrating < 35)
tmp[1:10,]

lm.small <- lm(indic.change ~ 1 + days.bet + rank + no.rating + numrating + pct.informative + pct.generic, data=tmp2)
summary(lm.small)


#########################################
#########################################
#########################################

## Seasonal changes
path <- "C:\\Users\\Ryan Wang\\Desktop\\amazon\\data"
setwd(path)
rm(path)
listings <- read.csv('listings_clean.csv',header=T)
amzn <- subset(listings, seller=="Amazon.com")
lowest <- subset(listings,rank==1)
rm(listings)

names(amzn)[8] <- 'price.amzn'
names(lowest)[8] <- 'price.lowest'

amzn <- amzn[,c(1,2,8)]
lowest <- lowest[,c(1,2,8)]
seasonal <- merge(amzn,lowest)
rm(amzn,lowest)

seasonal$dateacc <- as.Date(seasonal$dateacc,format="%Y-%m-%d")
seasonal$dateacc2 <- as.integer(seasonal$dateacc)
seasonal$dateacc2 <- seasonal$dateacc2 - min(seasonal$dateacc2) + 2
seasonal$week.elapsed <- floor( seasonal$dateacc2 / 7 + 1 )

bob <- ddply(seasonal,.(isbn,week.elapsed),function(x) c(mean(x$price.amzn),mean(x$price.lowest)),.progress='text')
names(bob)[3:4] <- c('avg.price.amzn','avg.price.lowest')

fillin <- expand.grid( as.vector(unique(bob$isbn)), as.vector(seq(1:max(bob$week.elapsed))) ) # fill in for missing weeks
names(fillin) <- c('isbn','week.elapsed')
bob <- merge(bob,fillin,all.y=T)
rm(fillin)

bob$isbn.prev <- c(NA,as.character(bob$isbn[-dim(bob)[1]])) # fill in prices for missings
# NOTE: couldn't figure out how to automate this...
#       in some cases there are multiple missing weeks in a row. so repeat the code below until there are none
# tofillin <- which(is.na(bob$avg.price.amzn) & bob$isbn.prev == bob$isbn)
# bob$avg.price.amzn[tofillin] <- bob$avg.price.amzn[tofillin - 1]
# bob$avg.price.lowest[tofillin] <- bob$avg.price.lowest[tofillin - 1]
# rm(tofillin)
bob$isbn.prev <- NULL

appearance <- ddply(bob,.(isbn),function(x)sum(!is.na(x$avg.price.amzn)))
remove <- appearance$isbn[which(appearance[,2] < 10)] # remove isbns that appear too few times
bob <- subset(bob,!(isbn %in% remove))
rm(appearance,remove)

isbns <- read.csv('C:\\Users\\Ryan Wang\\Documents\\Dropbox\\ethesis\\data\\isbns master.txt',header=T) # bring in book source data
colnames(isbns)[1] <- 'isbn'
bob <- merge(bob,isbns) # isbn/week.elapsed level
rm(isbns)

bob <- subset(bob,!is.na(bob$avg.price.amzn)) # remove cases where ISBN has not appeared yet
initial <- ddply(bob,.(isbn),function(x) c(x$avg.price.amzn[1],x$avg.price.lowest[1])) # calculate initial price)
names(initial)[2:3] <- c('price.amzn.initial','price.lowest.initial')
bob <- merge(bob,initial,by=c('isbn'),all.x=T)

bob$amzn.initial.change <- log(bob$avg.price.amzn) - log(bob$price.amzn.initial)
bob$lowest.initial.change <- log(bob$avg.price.lowest) - log(bob$price.lowest.initial)

# plot(amzn.initial.change~week.elapsed,data=subset(bob,isbn=="0073511366"),type='l')
# plot(lowest.initial.change~week.elapsed,data=subset(bob,isbn=="0073511366"),type='l')

seasonal.change <- ddply(bob,.(source,week.elapsed),function(x) c(mean(x$amzn.initial.change),
                                                                  mean(x$lowest.initial.change)))
names(seasonal.change)[3:4] <- c('amzn.initial.change','lowest.initial.change')

# make some plots for the paper!
pdf('seasonal.pdf',height=8,width=8)
par(mfrow=c(2,2),mar=c(5,4,2,2)+0.1)
plot(100*amzn.initial.change~week.elapsed,
     data=subset(seasonal.change,source=="UChicago Syllabus"),
     type='l',ylim=c(-7,7),
     xlab='Weeks Elapsed',ylab="Avg. % Change from Initial",xaxt='n')
lines(100*lowest.initial.change~week.elapsed,data=subset(seasonal.change,source=="UChicago Syllabus"),col=2,lty=2)
axis(1,at=c(4,8,12,16,20,24),labels=c('Sep','Oct','Nov','Dec','Jan','Feb'))
mtext('UChicago Syllabus',side=3,line=.5)

plot(100*amzn.initial.change~week.elapsed,
     data=subset(seasonal.change,source=="Bookrags"),
     type='l',ylim=c(-7,7),
     xlab='Weeks Elapsed',ylab="Avg. % Change from Initial",xaxt='n')
lines(100*lowest.initial.change~week.elapsed,data=subset(seasonal.change,source=="Bookrags"),col=2,lty=2)
axis(1,at=c(4,8,12,16,20,24),labels=c('Sep','Oct','Nov','Dec','Jan','Feb'))
mtext('Bookrags',side=3,line=.5)

plot(100*amzn.initial.change~week.elapsed,
     data=subset(seasonal.change,source=="NYT Bestseller"),     
     type='l',ylim=c(-7,7),
     xlab='Weeks Elapsed',ylab="Avg. % Change from Initial",xaxt='n')
lines(100*lowest.initial.change~week.elapsed,data=subset(seasonal.change,source=="NYT Bestseller"),col=2,lty=2)
axis(1,at=c(4,8,12,16,20,24),labels=c('Sep','Oct','Nov','Dec','Jan','Feb'))
mtext('NYT Bestseller',side=3,line=.5)

frame()
legend('center',legend=c("Amazon's Price","Lowest Price"),lty=1:2,col=1:2)
#mtext("Note: x-axis denotes weeks elapsed",side=1,line=-4,cex=0.75)
dev.off()
