#' Sample a \code{zoo}
#'
#' Sample an input \code{zoo} onto a monthly frequency.
#'
#' Sub-sample a \code{zoo} from (usually) daily to a monthly frequency. Register
#' the output on either first or last day of the month. This simply extracts
#' the records from the input that are closest to either the beginning or ending
#' day of each month covered. It's a "coarsening" of a \code{zoo} with a
#' frequency higher than monthly (e.g. daily to monthly).
#'
#' @param x An input \code{zoo} object
#' @param register Flag to indicate where to register the output:
#' Either 'first' or 'last'
#'
#' @return A \code{zoo}
#' @export
#' @examples
#' library(zoo)
#'
#' dts1 <- seq(as.Date("2012-08-01"), as.Date("2013-09-30"), "days")
#' dts2 <- seq(strptime("20120801", "\%Y\%m\%d"),
#' strptime("20130930", "\%Y\%m\%d"), "day")
#'
#' nr <- length(dts1)
#' z1 <- zoo(matrix(rnorm(nr*4), nr, 4), dts1)
#' z2 <- zoo(matrix(rnorm(nr*4), nr, 4), dts2)
#'
#' sampleZooMonthly(z1)
#' sampleZooMonthly(z2, "last")
sampleZooMonthly <- function (x, register = c("first", "last")) {
    register <- match.arg(register)
    if (!is(x, "zoo")) {
        stop("ERROR: Input must be a 'zoo'.")
    }
    if (!is.DateLike(index(x))) {
        stop("ERROR: Coarsening only really makes sense for",
            "'zoo's with date-like indexes.")
    }
    if (max(diff(index(x))) > 5) {
        stop("ERROR: Looks like index of input is coarser than daily.")
    }
    dts <- index(x)
    ym <- year(dts)*100+month(dts)
    regdts <- switch(register,
        first = tapply(as.numeric(dts), ym, min),
        last = tapply(as.numeric(dts), ym, max))
    if (is(dts, "Date")) {
        regdts <- as.Date(regdts, origin='1970-01-01')
    }
    if (is(dts, "POSIXct")) {
        regdts <- as.POSIXct(regdts, origin='1970-01-01')
    }
    if (is(dts, "POSIXlt")) {
        regdts <- as.POSIXlt(regdts, origin='1970-01-01')
    }

    # Round out possible diffs for second-accuracy case
    if (is(dts, "POSIXct")) {
        idx <- match(as.POSIXlt(round(regdts, "days")),
            as.POSIXlt(round(dts, "days")))
        return(x[idx,, drop=FALSE])
    }

    return(x[regdts, , drop = FALSE])
}

#' Splice \code{zoo}s
#'
#' Splice zoos together.
#'
#' Splices \code{y} into \code{x}, with \code{x} taking precedence.
#' If \code{y} only introduces a new series, then it works just like
#' a 'merge'.
#'
#' \code{spliceZoos} uses the input's column names to find common series,
#' then merges series "end-to-end" into a single series that may come from
#' both inputs. This is useful, for example, if series come from different
#' data sources but are different instances of the same underlying series.
#' If there are additional columns in the inputs that are not in-common, merges
#' those as normal. If there are not columns in-common, the function acts simply
#' as a regular merge.
#'
#' @param x First input \code{zoo} (takes precendence)
#' @param y First input \code{zoo}
#'
#' @return A \code{zoo} with the spliced output
#' @export
#' @examples
#' library(zoo)
#'
#' dts1 <- seq(as.Date("2012-08-01"), as.Date("2013-09-30"), "days")
#'
#' nr <- length(dts1)
#' z1 <- zoo(matrix(rnorm(nr*4), nr, 4), dts1)
#' colnames(z1) <- paste("S", 1:4, sep="")
#'
#' dts4 <- seq(as.Date("2013-10-01"), as.Date("2013-11-01"), "days")
#' nr <- length(dts4)
#' z4 <- zoo(matrix(rnorm(nr), nrow=nr), dts4)
#' colnames(z4) <- 'S4'
#'
#' allser <- spliceZoos(z1, z4)
spliceZoos <- function(x, y) {
    # Check inputs
    if (!is(x, "zoo") | !is(y, "zoo")) {
        stop("Expected inputs to be 'zoo's")
    }
    if (is.null(dim(x)) | is.null(dim(y))) {
        stop("Expected zoo's with columns")
    }
    aCols <- colnames(x)
    bCols <- colnames(y)

    # If 'y' only introduces new series, simply merge
    if (length(intersect(aCols, bCols)) == 0) {
        return( merge(x, y, all=T) )
    }

    # Identify & isolate brand new cols as 'bNew'
    bNew <- NULL
    new <- FALSE
    if (any(!is.element(bCols, aCols))) {
        bNewCls <- setdiff(bCols, aCols)
        bOtherCls <- setdiff(bCols, bNewCls)
        bNew <- y[, bNewCls, drop=F]
        y <- y[, bOtherCls, drop=F]
        new <- TRUE
    }

    # Hold out cols in 'x', not in 'y' to be re-introduced after splicing
    aOnly <- NULL
    reintro <- FALSE
    if (any(!is.element(aCols, bCols))) {
        aOnlyCls <- setdiff(aCols, bCols)
        aOtherCls <- setdiff(aCols, aOnlyCls)
        aOnly <- x[, aOnlyCls, drop=F]
        x <- x[, aOtherCls, drop=F]
        reintro <- TRUE
    }

    # Handle different numbers of observations by padding out w/ NAs
    nms <- colnames(x)
    y <- y[, nms, drop=F]
    mrg <- merge(x, y, all=T)
    x <- mrg[, 1:ncol(x), drop=F]
    colnames(x) <- nms

    y <- mrg[, (ncol(x)+1):ncol(mrg), drop=F]
    colnames(y) <- nms

    # Identify the pattern of NAs & do the splice
    aNAs <- is.na(coredata(x))
    bNAs <- is.na(coredata(y))
    idx <- !bNAs & aNAs
    coredata(x)[idx] <- coredata(y)[idx]

    # Re-add any "only in X" cols
    if (reintro) {
        x <- merge(x, aOnly, all=T)
    }

    # Add any brand new cols
    if (new) {
        x <- merge(x[, aCols, drop=F], bNew, all=T)
    }

    return(x)
}

#' Time-series score a \code{zoo}
#'
#' Time-series score \code{x} using an expanding window & weighting stats
#' estimates using exponentially decaying weights with half-life
#' \code{hl}.
#'
#' \code{scoreZoos} time-series scores the input zoo using an expanding
#' window and exponentially weighting the estimated mean and standard deviation
#' using the supplied half-life. Units of the half-life are however the input
#' series is indexed. If no \code{hl} argument is supplied, uses
#' equally-weighted estimates.
#'
#' @param x Input \code{zoo}
#' @param hl Half life (in whatever frequeny indexes \code{x})
#' @param na.rm Remove missing values? (Boolean)
#'
#' @return A \code{zoo} with the scored output
#' @export
scoreZoos <- function(x, hl, na.rm=TRUE) {
    expSc <- function(x, hl, na.rm) {
        wts <- rev(expWts(length(x), hl))
        mn <- wtd.mean(x, wts, na.rm=na.rm)
        stdev <- sqrt(wtd.var(x, wts, na.rm=na.rm))
        ((x-mn)/stdev)[length(x)]
    }

    if (missing(hl)) {
        mns <- rollapplyr(x, seq_along(x), mean, na.rm=na.rm)
        sds <- rollapplyr(x, seq_along(x), sd, na.rm=na.rm)
        return( (x-mns)/sds )
    }

    rollapplyr(x, seq_along(x), expSc, hl, na.rm=na.rm)
}

#' Plot a \code{zoo}
#'
#' Plot an input \code{zoo}, embedding my personal preferences
#'
#' @param x Input \code{zoo}
#' @param ... Additional parameters passed to \code{plot}
#'
#' @export
plotz <- function(x, ...) {
    if (!is(x, "zoo")) {
        stop("ERROR: Expected a 'zoo'")
    }

    ucol <- 1
    nc <- ncol(x)
    if (!is.null(nc)) {
        ucol <- 1:nc
    }
    if (hasArg(col)) { ucol <- col }

    plot.zoo(x, plot.type="single", xlab="", col=ucol, ...)
}

