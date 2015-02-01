# boxdot.plot
Boxdot plot function

The boxdot plot is a variation on Tukey's box-and-whisker plot, and Parzen's quantile boxplot.  
Its purpose is to overlay the data points on a boxplot in a systematic manner, as opposed to jittering.
The data points for each box are arranged in sorted order, so that they resemble an empirical distribution function on its side.

I was first introduced to this type of plot by Dan Holder, who calls it a boxdot plot.  
I am grateful to David Shera for the background information on this type of plot, and sharing R code that inspired this function.

References:

E. Parzen, 1979: Nonparametric statistical data modeling. Journal of the American Statistical Association, 74: 105-121. 

D. M. Shera, 1991: Some uses of quantile plots to enhance data presentation. Computing Science and Statistics, 23: 50-53.

J. W. Tukey, 1977:  Exploratory Data Analysis.  Addison-Wesley.

