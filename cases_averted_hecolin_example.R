##############################################################################
## Some illustrative extremly simple examples of the potential              ##
## impact of Hecolin in historical epidemics.                               ##
##                                                                          ##
## Please contact Andrew Azman (azman@jhu.edu) with any questions or issues ##
##############################################################################

## a few settings
library(dplyr)
palette(brewer.pal(8,"Dark2"))

## load data from Kitgum
## NOTE THAT THESE data were extracted from:
## Teshale EH, Howard CM, Grytdal SP, et al. Hepatitis E epidemic, Uganda.
## Emerg Infect Dis 2010;16(1):126–9.
raw.dat <- read.csv("Data/UgandaHepE.csv",header=F)
clean.dat <- data.frame(week=c(43:53,1:53,1:4),
                        year=c(rep(2007,length(43:53)),rep(2008,length(1:53)),rep(2009,length(1:4))),
                        cases=ceiling(raw.dat[,2]))

### now for maban
raw.mab1 <- read.csv("Data/mabanstart.csv",header=F) #5080 cases as of Jan 21
raw.mab2 <- read.csv("Data/mabanend.csv",header=F)

clean.mab <- data.frame(week=c(21:52,1:52,1),
                        cases=ceiling(c(raw.mab1[,2],600,600,raw.mab2[,2])))

## function for making plot
make.quick.plot <- function(epi.week=TRUE){
    par(mfrow=c(2,1),mar=c(1,1,1,1),oma=c(2,2,0,2),mgp=c(1,.7,0))
    plot(1:68,clean.dat$cases,xaxt="n",type="l",lwd=2,col="grey",xlim=c(0,85))
    if(epi.week){
        axis(1,at=1:68,labels=paste0(clean.dat$week),las=2,cex.axis=.6)
    } else {
        axis(1,at=seq(1,85,by=2),labels=seq(0,84,by=2),las=2,cex.axis=.6)
    }

    ves <- c(0,(1-(1-.895*.955)),.895) # assuming Modified Intent to Treat 0-30 month results from Zhang
    cov <- .75
    grid()
    first.dose <- 4*4 # 3 months in
    second.dose <- first.dose + 4
    last.dose <- first.dose + 4*6
    abline(v=c(first.dose,second.dose,last.dose),col=AddAlpha("grey",.5),lty=2)
    text(first.dose-1,590,"dose 1",srt=90,col=AddAlpha("grey",.9),cex=.8)
    text(second.dose-1,590,"dose 2",srt=90,col=AddAlpha("grey",.9),cex=.8)
    text(last.dose-1,590,"dose 3",srt=90,col=AddAlpha("grey",.9),cex=.8)

    new.curve <- clean.dat$cases
    new.curve[(second.dose+1):last.dose] <- new.curve[(second.dose+1):last.dose]*(1-ves[2])*cov
    new.curve[(last.dose+1):68] <- new.curve[(last.dose+1):68]*(1-ves[3])*cov
    lines(second.dose:68,new.curve[second.dose:68],col=AddAlpha(2,.75),type="l",lwd=1.5)
    print(sum(clean.dat$cases) - sum(new.curve))

    new.curve <- clean.dat$cases
    new.curve[(last.dose+1):68] <- new.curve[(last.dose+1):68]*(1-ves[3])*cov
    lines(last.dose:68,new.curve[last.dose:68],col=AddAlpha(3,.75),type="l",lwd=1.5)
    print(sum(clean.dat$cases) - sum(new.curve))

    legend("topright",c("observed epidemic","partial protection after dose 2","no protection until dose 3"),
           col=c("grey",AddAlpha(2,.75),AddAlpha(3,.75)),lwd=c(2,1.5,1.5),bty="n")
    text(5,600,"Kitgum District, \n Uganda",cex=.7)

    plot(1:85,clean.mab$cases,xaxt="n",type="l",lwd=2,col="grey",xlim=c(0,85))
    if(epi.week){
        axis(1,at=1:85,labels=paste0(clean.mab$week),las=2,cex.axis=.6)
    } else {
        axis(1,at=seq(1,85,by=2),labels=seq(0,84,by=2),las=2,cex.axis=.6)
    }

    grid()

    first.dose <- 4*4 # 4 months in
    second.dose <- first.dose + 4
    last.dose <- first.dose + 4*6
    text(first.dose+1,590,"dose 1",srt=90,col=AddAlpha("grey",.9),cex=.8)
    text(second.dose+1,590,"dose 2",srt=90,col=AddAlpha("grey",.9),cex=.8)
    text(last.dose+1,590,"dose 3",srt=90,col=AddAlpha("grey",.9),cex=.8)
    abline(v=c(first.dose,second.dose,last.dose),col=AddAlpha("grey",.8),lty=2)
    new.curve <- clean.mab$cases
    new.curve[(second.dose+1):last.dose] <- new.curve[(second.dose+1):last.dose]*(1-ves[2])*cov
    new.curve[(last.dose+1):85] <- new.curve[(last.dose+1):85]*(1-ves[3])*cov
    lines(second.dose:85,new.curve[second.dose:85],col=AddAlpha(2,.75),type="l",lwd=1.5)
    print(sum(clean.mab$cases) - sum(new.curve))

    new.curve <- clean.mab$cases
    new.curve[(last.dose+1):85] <- new.curve[(last.dose+1):85]*(1-ves[3])*cov
    print(sum(clean.mab$cases) - sum(new.curve))

    lines(last.dose:85,new.curve[last.dose:85],col=AddAlpha(3,.75),type="l",lwd=1.5)
    mtext("Week Number",side=1,outer=T,line=.8)
    mtext("Suspected HEV Cases",side=2,outer=T,line=.8)
    text(5,600,"Maban County, \n South Sudan",cex=.7)
}
