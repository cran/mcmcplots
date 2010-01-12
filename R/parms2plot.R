parms2plot <- function(parnames, parms, regex, random){
    re.leaf <- "[]\\[_(),[:digit:][:space:]]"
    stems <- unique(gsub(re.leaf, "", parnames)) # unique parameter "groups"
    if (is.null(parms) && is.null(regex)){
        ## Create a regular expression that selects all in 'parnames'.
        re <- paste("^", stems, re.leaf, "*$", sep="")
    } else {
        if (!is.null(parms)){
            ## 'parms' will eventually be used as a regular expression,
            ## so all special characters currently in 'parms' need to
            ## have backslashes added to them
            parms <- gsub("(\\[|\\]|\\.|\\+|\\*|\\(|\\))", "\\\\\\1", parms)
            parms <- paste("^", parms, re.leaf, "*$", sep="")
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
