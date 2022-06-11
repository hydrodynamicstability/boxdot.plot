

##########################################################################
###
### Draft R function for creating a "boxdot" plot.
### This is a boxplot with overlaid data points.  The data points are arranged systematically,
### as in an empirical cumulative distribution function on its side.
### Warning:  this is not a bulletproofed function.  It will need to be improved.
### 
### This is a variation on Parzen's quantile boxplot:
### E. Parzen, 1979: Nonparametric statistical data modeling. Journal of the American Statistical Association, 74: 105-121. 
### 
### See also 
### D. M. Shera, 1991: Some uses of quantile plots to enhance data presentation. Computing Science and Statistics, 23: 50-53. 
### [Proceedings of the 23d Symposium on the Interface, Seattle, WA, 21-24 April 1991, ed. by E. M. Keramidas.] 
###
### Programmer:
### Christopher Tong
### Initial version 31 Jan 2015.
### Acknowledgments:  David M. Shera and Dan Holder.
###
##########################################################################


boxdot <- function(x,y, boxhalfwidth=0.4, pch=1, ylim=range(y,na.rm=TRUE), ...)
{
    # First make the boxplot; do not include outliers.
     ff <- as.formula("y ~ x")
     loc.bp <- boxplot(ff, outline=FALSE, border="grey", ylim=ylim, boxwex=0.5, ...)

    # Now include overlaid data points for each box
     unique.x <- sort(unique(x))
     for (i in 1:length(unique.x))
     {
        # Select data for i'th box and sort them
        loop.y <- y[x == unique.x[i]]
        loop.sort.y <- sort(loop.y) # will remove NA's
        
        # Calculate x-positions of the data points
        loop.x <- seq(i - boxhalfwidth, i + boxhalfwidth,by = 2*boxhalfwidth/(length(loop.sort.y)-1))
        # I assume the edges of the boxes are at integers +/- boxhalfwidth
       
        # Plot the data symbols
        points(loop.x,loop.sort.y,pch=pch)
     }
    loc.bp
    # Return output of the boxplot function
}

# Example usage:
# boxdot(OrchardSprays$treatment,OrchardSprays$decrease,boxhalfwidth=0.3,pch=2,main="My Plot")

# Copyright (C) 2022 by Christopher Tong

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
