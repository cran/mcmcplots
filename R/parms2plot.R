parms2plot <- function(parnames, parms, regex, random){
    re.group <- "[]\\[(),[:digit:][:space:]]"
    pargnames <- unique(gsub(re.group, "", parnames)) # unique parameter "groups"
    if (is.null(parms) && is.null(regex)){
        re <- paste("^", pargnames, re.group, "*$", sep="")
    } else {
        if (!is.null(parms)){
            parms <- gsub("(\\[|\\]|\\.|\\+|\\*|\\(|\\))", "\\\\\\1", parms) # add backslashes before
            parms <- paste("^", parms, re.group, "*$", sep="")
        }
        re <- c(parms, regex)
    }
    parlist <- lapply(re, function(r, p) p[grep(r, p)], p=parnames)
    if (!is.null(random)){
        random <- rep(random, length=length(re))
        random[is.na(random)] <- length(parnames)
        for (i in seq(along=parlist)){
            x <- parlist[[i]]
            r <- random[i]
            if (length(x)>r)
                parlist[[i]] <- x[sort(sample(seq(along=x), r))]
        }
    }
    parnames <- unlist(parlist)
    return(parnames)
}
