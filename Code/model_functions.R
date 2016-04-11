###################################################################
# Simple model
###################################################################

# Calculate the equilibrium payoff to producers and scroungers at q
E.simple<-function(q, G=20){
	P<-lambda/(gamma+alpha*q*G)
	S<-(1-q)*G*alpha*lambda/(gamma*(gamma+alpha*q*G))	
	return(cbind(P,S))
}

# Calculate the rate of change in q as a function of q
dq.simple<-function(q){
	P<-lambda/(gamma+alpha*q*G)
	S<-(1-q)*G*alpha*lambda/(gamma*(gamma+alpha*q*G))	
	return((S-P)*q*(1-q))
}

###################################################################
# Stage-structured model
###################################################################

# Equilibrium payoff to small producers
P1<-function(params, q){
	lambda<-params[['lambda']]
	alpha<-params[['alpha']]
	theta<-params[['theta']]
	gamma<-params[['gamma']]
	G<-params[['G']]
	as.numeric(
		lambda/(G*theta*alpha[1, 1]*q[1]-G*theta*alpha[2, 1]*q[2]+G*alpha[2, 1]*q[2]+gamma[1])
		)
	}

# Equilibrium payoff to large producers
P2<-function(params, q){
	lambda<-params[['lambda']]
	alpha<-params[['alpha']]
	theta<-params[['theta']]
	gamma<-params[['gamma']]
	G<-params[['G']]
	as.numeric(
		lambda/(G*theta*alpha[1, 2]*q[1]-G*theta*alpha[2, 2]*q[2]+G*alpha[2, 2]*q[2]+gamma[2])
		)
	}

	
# Equilibrium payoff to large scroungers
S2<-function(params, q){
	lambda<-params[['lambda']]
	alpha<-params[['alpha']]
	theta<-params[['theta']]
	gamma<-params[['gamma']]
	G<-params[['G']]
	
	return(-G*lambda*(alpha[2, 1]*(theta*q[1]-theta*q[2]+q[2]-1)*(theta*alpha[1, 2]*q[1]-theta*alpha[2, 2]*q[2]+alpha[2, 2]*q[2])*G+theta*alpha[2, 1]*gamma[2]*q[1]-theta*alpha[2, 2]*gamma[1]*q[2]-theta*alpha[2, 1]*gamma[2]+theta*alpha[2, 2]*gamma[1]+alpha[2, 2]*gamma[1]*q[2]-alpha[2, 2]*gamma[1])/((theta*alpha[1, 2]*q[1]-theta*alpha[2, 2]*q[2]+alpha[2, 2]*q[2])*(theta*alpha[1, 2]*gamma[1]*q[1]-theta*alpha[2, 1]*gamma[2]*q[2]+alpha[2, 1]*gamma[2]*q[2])*G^2+gamma[2]*(2*theta*alpha[1, 2]*gamma[1]*q[1]-theta*alpha[2, 1]*gamma[2]*q[2]-theta*alpha[2, 2]*gamma[1]*q[2]+alpha[2, 1]*gamma[2]*q[2]+alpha[2, 2]*gamma[1]*q[2])*G+gamma[1]*gamma[2]^2))
	
	}

# Equilibrium payoff to small scroungers
S1<-function(params, q){
	lambda<-params[['lambda']]
	alpha<-params[['alpha']]
	theta<-params[['theta']]
	gamma<-params[['gamma']]
	G<-params[['G']]
	
	return(-G*lambda*(alpha[1, 2]*(theta*q[1]-theta*q[2]+q[2]-1)*(theta*alpha[1, 1]*q[1]-theta*alpha[2, 1]*q[2]+alpha[2, 1]*q[2])*G+theta*alpha[1, 1]*gamma[2]*q[1]-theta*alpha[1, 2]*gamma[1]*q[2]-theta*alpha[1, 1]*gamma[2]+theta*alpha[1, 2]*gamma[1]+alpha[1, 2]*gamma[1]*q[2]-alpha[1, 2]*gamma[1])/(((theta*alpha[1, 1]*q[1]-theta*alpha[2, 1]*q[2]+alpha[2, 1]*q[2])*G+gamma[1])*((theta*alpha[1, 2]*gamma[1]*q[1]-theta*alpha[2, 1]*gamma[2]*q[2]+alpha[2, 1]*gamma[2]*q[2])*G+gamma[1]*gamma[2])))
	
	}

# Rate of change in q
dq<-function(t,y,parameters){
	y[1]<-min(max(y[1], 0), 1)
	y[2]<-min(max(y[2], 0), 1)
	dy<-numeric(2)
	dy[1]<-(S1(parameters, y)-P1(parameters, y))*(1-y[1])*y[1]
	dy[2]<-(S2(parameters, y)-P2(parameters, y))*(1-y[2])*y[2]
	return(list(dy))
}

###################################################################
# Plotting functions
###################################################################
#My own filled contour function that does not use layout

filledContour<-function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2, las=1)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}


plot.scale.bar<-function(zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, log.y=FALSE,...){
    
   par(mar=c(2,2,1,5))
   plot(c(0, 1), range(levels), "n", xaxs = "i", yaxs = "i", xaxt="n", yaxt="n", xlab="", ylab="")
    
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border=NA)
        
    if (missing(key.axes)) {
        if (axes) 
            if(log.y==TRUE) axis(4, at=pretty(zlim, n=6), labels=10^pretty(zlim, n=6), las=1, cex.axis=1.5) else axis(4, las=1, cex.axis=1.3)
    }
 }
    
