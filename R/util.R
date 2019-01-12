#' Quickie Visual Comparison
#'
#' Quick X-Y plot to compare contents of two vectors. If vectors have row
#' names, use that to key them for comparison.
#'
#' I constantly find myself comparing two vectors to see if they are equal.
#' A quick X-Y plot is a great way to do this visually and is better than
#' correlation at spotting outliers or other structure.
#'
#' Usually pass two column-like things (matrices or data frames), usually
#' keyed by row names. But can pass two list-like things (e.g. numerics) or
#' even a two-column data frame/matrix. (Columns to the right of the first
#' two are ignored).
#'
#' @param a First array
#' @param b Second array
#' @param xlab Default x-axis label
#' @param ylab Default y-axis label
#' @param ... Additional arguments passed to \code{plot}
#'
#' @export
#' @examples
#' x <- rnorm(50)
#' y <- 0.3*x^2
#' cmp(x, y)
#'
#' x <- data.frame(x=rnorm(26), y=rnorm(26), row.names=toupper(letters))
#' cmp(x)
#'
#' x <- data.frame(x=rnorm(26), row.names=toupper(letters))
#' y <- data.frame(y=rnorm(26), row.names=toupper(letters))
#' cmp(x, y)
cmp <- function(a, b, xlab="A", ylab="B", ...) {
    mrg <- NULL
    if (missing(b)) {
        if (is.null(dim(a))) {
            cat("ERROR: In 'cmp', only one input. Was expecting 2 columns\n")
            return()
        }
        else {
            if (ncol(a) < 2) {
                cat("ERROR: In 'cmp', only one column\n")
                return()
            }
            mrg <- a[, 1:2]
        }
    }
    else {
        if ( is.null(dim(a)) & is.null(dim(b)) ) {
            if (length(a) != length(b)) {
                cat("ERROR: In 'cmp' inputs are of different length\n")
                return()
            }
            mrg <- cbind(a, b)
        }
        else {
            mrg <- merge(a[,1,drop=F], b[,1,drop=F], by="row.names")
            rownames(mrg) <- mrg[, 1]
            mrg <- mrg[, -1]
            colnames(mrg) <- c(colnames(a), colnames(b))
        }
    }
    oldpar <- par(pty="s")
    plot(mrg[,1], mrg[,2], xlab=xlab, ylab=ylab, ...)
    x <- 0.67 * max(mrg[, 1]) - 0.2 * min(mrg[, 1])
    y <- 0.67 * max(mrg[, 2]) - 0.2 * min(mrg[, 2])
    text(x, y, paste("corr =",
            round(cor(mrg[, 1], mrg[, 2], use="pairwise"), 3)))

    abline(0, 1, col=2)
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    par(oldpar)

    invisible()
}

#' List Objects with Detail
#'
#' List objects in w/ more detail than ls(). Similar to a unix-like
#' \code{ls -l}.
#'
#' List objects in w/ more detail than ls(). Modified from a
#' \href{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}{Stackoverflow question}
#'
#' @param pattern Optional \code{\link{regular expression}}. Only
#' names matching \code{pattern} are returned.
#' @param pos Specify environment as a position in the search list.
#' @param order.by Order by \sQuote{type} or \sQuote{size}
#' @param decreasing List in decreasing/increasing order of /code{order.by}
#'
#' @export
#' @examples
#' require(graphics)
#' ll()
ll <- function(pattern, pos=1, order.by="size",
    decreasing=TRUE) {
    napply <- function(names, fn) {
        sapply(names, function(x) fn(get(x, pos = pos)))
    }
    nms <- c("type", "size", "Rows", "Columns")
    order.by=match.arg(order.by, nms)

    names <- ls(pos=pos, pattern=pattern)
    if (length(names) == 0) return()
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]

    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- nms
    if (!missing(order.by))
        out <- out[rev(order(out[[order.by]], decreasing=decreasing)), ]

    return( out )
}

#' Report Resource Usage
#'
#' Report on memory usage (not Mac OS X yet) and processing time.
#'
#' Evaluate \code{expr} and print processing time, session time (both in
#' seconds) and memory usage (certain platforms only).
#' Gives CPU & memory resources used in evaluating 'expr'
#' (modified from Venebles & Ripley).
#'
#' @param expr An object to be evaluated (e.g. function call).
#'
#' @export
#' @examples
#' A <- matrix(rnorm(1e6), 1e3)
#' resources( svd(A) )
resources <- function(expr) {
    loc <- sys.parent(1)
    if (loc == 1) loc <- F
    on.exit( { cat("Session time (s)\n") ; print(proc.time())} )

    expr <- substitute(expr)
    stime <- proc.time()
    sys <- Sys.info()["sysname"]

    w <- eval(expr, envir=loc)
    etime <- proc.time()
    if (sys == "Windows") {
        mem <- memory.size()
        mem <- paste(mem, "Mb allocated")
    }

    time <- etime - stime
    time[3] <- max(time[3], time[1] + time[2])

    cat("Process time (s)\n")
    print(time)
    if (sys == "Windows") { cat(mem, "\n\n") }

    invisible(w)
}

#' Matrix from dated files
#'
#' Create a single matrix from data in separate (dated) files.
#'
#' This is a general-purpose function to parse dated files to a data matrix.
#' There's nothing here about the file-naming convention; that's the
#' responsibility of the caller. (Columns get labelled exactly like the
#' input files). Missing data padded w/ 'NA'.
#' Load data from columns in the dated files. Can either specify files
#' directly or specify a directory 'dir' and file-pattern 'filePat' (regexpr).
#' Column description 'col', which can be either an integer column index
#' or a character which will be passed to grep to match a column header.
#' Arguments like 'skip', 'header', 'sep' & 'stringsAsFactors'
#' are passed to 'read.table'.
#'
#' This is a general-purpose function to parse dated files to a data matrix.
#' There's nothing here about the file-naming convention; that's the
#' responsibility of the caller. (Columns get labelled exactly like the
#' input files).
#'
#' @param fls Files (character) to process
#' @param dir Directory (character)
#' @param filePat File pattern (regexpr)
#' @param col Column description (integer or character)
#' @param ... Additional arguments passed to \code{read.table}
#'
#' @return A matrix
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- matrixFromDatedFiles('data', 'dat*', sep=",", header=T)
#' }
matrixFromDatedFiles <- function(fls, dir, filePat="*", col=1, ...) {
    if (missing(fls)) {
        fls <- list.files(path=dir, pattern=filePat)
        fls <- paste(dir, fls, sep="/")
        fls <- fls[file.exists(fls)]
    }
    if (length(fls) <= 0) {
        cat("No files found...\n")
        return()
    }
    dtStrs <- regmatches(fls, regexpr("[0-9]+", fls))
    if ( length(dtStrs) != length(fls) ) {
        cat("Not all files appear to be dated\n")
        cat(dir, filePat, "\n")
        return()
    }

    dat <- vector("list", length(fls))
    names(dat) <- dtStrs
    gUniv <- character()
    for(f in 1:length(fls)) {
        fl <- fls[f]
        cat("\tReading", fl, "\n")
        dat[[f]] <- read.table(fl, ...)
        gUniv <- c(gUniv, rownames(dat[[f]]))
    }
    gUniv <- sort(unique(gUniv))
    out <- matrix(nrow=length(gUniv), ncol=length(fls))
    rownames(out) <- gUniv
    colnames(out) <- dtStrs
    for(f in 1:length(fls)) {
        cidx <- col
        if( is.character(col) ) {
            colStr <- col
            cidx <- grep(col, colnames(dat[[f]]))
            if (length(cidx) == 0) {
                cat("No column matching", colStr, "in file", fls[f], "\n")
                next
            }
        }
        else {
            if ( col > ncol(dat[[f]]) ) {
                cat("No such column", col, "in file", fls[f], "\n")
                next
            }
        }
        out[, f] <- dat[[f]][gUniv, cidx[1]]
    }

    return(out)
}

#' SEDOL check digits
#'
#' Append a check-digit to input SEDOLs
#'
#' Implemented the algorithm for computing the SEDOL check digit, as
#' described on Wikipedia.
#'
#' @references Wiki page: \url{https://en.wikipedia.org/wiki/SEDOL}
#' @param x Input containing SEDOLs (character)
#'
#' @return Character.
#' @export
#'
#' @examples
#' sedols <- c("208942", "298356", "BFSSDS", "270502")
#' appendSEDOLCheckDigit(sedols)
appendSEDOLCheckDigit <- function(x) {
    xnona <- na.omit(x)
    ok <- grep("^[[:digit:][:upper:]]{6}$", xnona)
    if(length(ok) < length(xnona)) {
        stop("SEDOLs input contains invalid lines.")
    }

    if (length(x) > 1) {
        return(sapply(x, appendSEDOLCheckDigit))
    }
    if (is.na(x)) return(NA)

    x <- as.character(x)
    ascii <- as.integer(charToRaw(x))
    scores <- ifelse(ascii < 65, ascii - 48, ascii - 55)
    weights <- c(1, 3, 1, 7, 3, 9)
    chkdig <- (10 - sum(scores * weights) %% 10) %% 10

    return(paste(x, as.character(chkdig), sep=""))
}

#' Exponential weights
#'
#' Compute exponential weights for a specified half-life.
#'
#' Generate array of exponential weights of length \code{length},
#' and half-life \code{halfLife}. Maximum weight is \code{max} (default 1.0).
#'
#' @param length Length of output array (default=10)
#' @param halfLife Half-life, in periods (default=6)
#' @param max Maximum weight (default=1)
#'
#' @return Numeric
#' @export
#'
#' @examples
#' expWts()
#' expWts(50, 30)
expWts <- function(length=10, halfLife=6, max=1) {
    lam <- log(2)/halfLife
    t <- seq(0, length=length)
    n <- max * exp(-1*lam*t)

    return(n)
}

#' Winsorize
#'
#' "Windorize" the (numeric) input by truncating outliers
#'
#' Handle outliers in the columns of a numeric matrix by setting them
#' equal to \code{mult} times the distribution median.
#' Simple handling of data outliers. Less involved than iterative
#' re-computation of sd & truncation.
#'
#' Inspired by blog post here: \url{https://www.r-bloggers.com/winsorization/}.
#'
#' @param x a numeric matrix-like object
#' @param mult multiplier
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' norm1 <- rnorm(100)
#' cauch1 <- rcauchy(100)
#' dat <- cbind(norm1, cauch1)
#'
#' datw <- winsorize(dat)
winsorize <- function(x, mult=3) {
    x <- as.matrix(x)

    f <- function(v, mult=mult) {
        nas <- is.na(v)
        v <- v[!is.na(v)]
        med <- median(v)
        y <- v-med
        sc <- mad(y, center=0)*mult
        y[y > sc] <- sc
        y[y < -sc] <- -sc
        z <- numeric(length=length(nas))
        names(z) <- names(nas)
        z[nas] <- NA
        z[!nas] <- y+med
        z
    }

    apply(x, 2L, f, mult)
}

