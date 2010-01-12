mcmcplotsPalette <- function(n, seq=FALSE){
    if (seq)
        return(sequential_hcl(n))
    if(n==1)
        return(rainbow_hcl(1, start=240, l=50, c=100))
    return(rainbow_hcl(n, start=0, end=240, c=100))
}
