mcmcplotsPalette <- function(n){
    if(n==1)
        return(rainbow_hcl(1, start=240, l=50, c=75))
    return(rainbow_hcl(n, start=0, end=240, c=75))
}
