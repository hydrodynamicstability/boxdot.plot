# This is David Shera's version of the function.

libsrc(lighten)

boxquant = function(x,y,main="",
xlab=deparse(substitute(x)),ylab=deparse(substitute(y)),
yrange=range(y,na.rm=TRUE),
xrange=range(x,na.rm=TRUE),
col=gray(0),pch=1,
boxcol = darken(col),
ptpch=1,
hlines=0,
meanpch = NA,
## nonames=FALSE,
boxwex = .5,
test=0,
ptlab = NA,
ptlabcex = .7,
cex=1,
## xgrplab = NULL,
...
) {
    ## x = categorical group variable
    
    ## Automatically splits boxplots by x
    ## uses col for color
    ## if pch exists, uses it for plot character
    
    ## sel = !is.na(y) & !is.na(x)

    ## this code figures out which colors to use based on data$col

    if (length(col)==1) {
        colvec = col
    } else {
        
        if (test) cat("boxquant: before col > 1\n")
        if (test) pv(length(col))
        if (test) pv(length(x))
        
        tcol = table(x,col)
        if (test>0) print(tcol)
        tpos = tcol>0
        if (test>0) pv(tpos)
        trow = apply(tpos,1,any)
        if (test>0) pv(trow)
        tcol = tcol[trow,]
        if (test>0) print(tcol)
        tdim = dim(tcol)
        tcolcol = colnames(tcol)

        for (ir in 1:(tdim[1])) {
            trow = tcol[ir,]
            if (ir == 1) {
                colvec = tcolcol[trow>0]
            } else {
                colvec = c(colvec, tcolcol[trow>0])
            }
        }
    }

    if (test>0) pv(colvec)

    colveclt = lighten(colvec)
    colvecdk = darken(colvec)

    if (test>0) pv(colveclt)



    bqdata = data.frame(x=x,y=y,ptlab=ptlab)
    if (test>0) cat("just after bqdata= \n")
    if (test>0) pv(dim(bqdata))

    collt = lighten(col)
    bqdata$col = collt
    bqdata$ptcol = col
    bqdata$pch = ptpch
    if (test>0) pv(colnames(bqdata))
    if (test>0) pv(dim(bqdata))

    if (test>0) pv(yrange)
    ff = as.formula("y ~ x")
    if (test>0) pv(ff)

    if (test>0) pv(dim(data))

    pv(ptpch)
    
    boxplot(ff,data=bqdata,
            main = main,
            ylab = ylab,
            xlab = xlab,
            boxcol = colveclt,
            whiskcol = colveclt,
            whisklty = 1,
            outcol = colveclt,
            outpch = bqdata$ptpch,
            staplecol = colveclt,
            medcol = colveclt,
            boxwex = boxwex,
            staplwex = .3,
            ylim = yrange,
            ...
            )




    
    ## overlayed quantile plots
    xlist = sort(unique(x))
    if (test>0) pv(xlist)
    abline(h=hlines,col=gray(.8))
    center = 0
    abline(v=center+.5,col=gray(.9))

    
    for (g in xlist) {
        if (test>0) pv(g)
        seldd = bqdata$x==g & !is.na(bqdata$y)
        dd = bqdata[seldd,]
        colvecdd = colvec[seldd]
        if (test>0) pv(dim(dd))
        if (test>0) pv(colnames(dd))
        center = center + 1
        abline(v=center+.5,col=gray(.9))

        if (!is.na(meanpch)) {
            grpmean = mean(dd[,"y"],na.rm=TRUE)
            points(center,grpmean,pch=meanpch,col=dd[1,"col"],cex=2)
        }
       
        ngrp = dim(dd)[1]
        if (test>0) pv(ngrp)
        ## print(ngrp)
        ## print(dim(dd))
        sel = !is.na(dd[,"y"])
        if (test>0) tb(sel)
        yy = dd[sel,"y"]
        if (test>0) pv(length(yy))
        r = rank(yy,ties="random")
        rx = (r-.5)/ngrp  - .5  + center
        ## print(cbind(x,dd$ageresid))
        if (test>0) pv(length(rx))
        points(rx,yy,pch=dd$pch,col=dd$ptcol,cex=cex)

        if (length(ptlab)>1 | !is.na(ptlab)) {
            text(rx,yy,labels=dd$ptlab,cex=ptlabcex,col=dd$ptcol,adj=c(-.1,.5))
        }
    }
}

