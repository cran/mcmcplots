corplot <- function(mat, col=gray(11:0/11), ...){
    nm <- dimnames(mat)[[1]]
    p <- nrow(mat)
    mat <- abs(mat[, p:1])
    image(mat, axes=FALSE, col=col)
    axis(2, at=0:(p-1)/(p-1), label=nm[p:1], las=2, tick=FALSE, ...)
    axis(3, at=0:(p-1)/(p-1), label=nm, las=2, tick=FALSE, ...)
    grid(p, p, col="black", lty=1)
}
