# This is David Shera's function.

lighten = function(cin,dist=.5,target=c(1,1,1)) {
    crgb = col2rgb(cin)

    cred = crgb["red",]
    red = ((target[1]*255-cred)*.5 + cred)/255
    cgreen = crgb["green",]
    green = ((target[2]*255-cgreen)*.5 + cgreen)/255
    cblue = crgb["blue",]
    blue = ((target[3]*255-cblue)*.5 + cblue)/255

    ret = rgb(red,green,blue)
    return(ret)
}

darken = function(cin,dist=.5,target=c(0,0,0)) {
    ret = lighten(cin,dist=dist,target=target)
    return(ret)
}
    
