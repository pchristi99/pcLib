#' Date arithmetic
#'
#' Bunch of convenience functions to do simple arithmetic on dates.
#'
#' Increment or decrement the vector 'date' forward or backward by
#' 'n' day(s), 'n' business day(s), or 'n' months. Business days
#' are considered to be Monday through Friday. A vector of holiday
#' dates can be passed in.
#'
#' @param date A 'datelike' object
#' @param n An integer number of days/months to add/subtract (default 1)
#' @param hol A 'datelike' object containing holiday dates
#'
#' @seealso is.DateLike
#' @export
#'
#' @rdname dateArith
#' @examples
#' \dontrun{
#' x <- seq(from=as.Date("2001-01-01"), to=as.Date("2001-01-31"), by="day")
#' ax <- addDays(x, 3)
#' difftime(x, ax)
#'
#' x <- seq(from=as.Date("2001-01-01"), to=as.Date("2001-01-31"), by="day")
#'
#' sx <- subtractDays(x, 3)
#' difftime(x, sx)
#'
#' x <- seq(from=as.Date("2001-01-01"), to=as.Date("2002-06-01"), by="months")
#' h <- as.Date("2001-07-02")
#' ax <- addBizDays(x, hol=h)
#' }
addDays <- function(date, n=1) {
    day(date) <- day(date) + n

    return(date)
}

#' @rdname dateArith
#' @export
subtractDays <- function(date, n=1) {
    day(date) <- day(date) - n

    return( date )
}

#' @rdname dateArith
#' @export
addBizDays <- function(date, n=1, hol=NULL) {
    k <- length(date)
    nxt <- addDays(date, n)

    max <- 5
    for (i in 1:max) {
        notBiz <- !isBizday(nxt, hol=hol)
        if (sum(notBiz) > 0)
            nxt[notBiz] <- addDays(nxt[notBiz], n)
        if (sum(isBizday(nxt, hol=hol)) >= k) { break }
    }

    return(nxt)
}

#' @rdname dateArith
#' @export
subtractBizDays <- function(date, n=1, hol=NULL) {
    return( addBizDays(date, -1*n, hol=hol) )
}

#' @rdname dateArith
#' @export
addMonths <- function(date, n=1) {
    month(date) <- month(date) + n

    return( date )
}

#' @rdname dateArith
#' @export
subtractMonths <- function(date, n=1) {
    month(date) <- month(date) - n

    return( date )
}

#' Date Manipulation
#'
#' Move date input to convenient dates at the beginning or end
#' of the the month.
#'
#' Business days
#' are considered to be Monday through Friday. A vector of holiday
#' dates can be passed in.
#'
#' @param x A 'datelike' object
#' @param hol A 'datelike' object containing holiday dates
#'
#' @seealso is.DateLike
#' @export
#'
#' @name Date Manipulation
#' @rdname dateManip
firstDayInMonth <- function(x) {
    day(x) <- 1
    return(x)
}

#' @rdname dateManip
#' @export
lastDayInMonth <- function(x) {
    last.day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    y <- year(x)
    leap.year <- (y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0))
    leap.day <- as.integer(leap.year) * as.integer(month(x) == 2)
    day(x) <- last.day[month(x)] + leap.day

    return(x)
}

#' @rdname dateManip
#' @export
firstBizDayInMonth <- function(x, hol=NULL) {
    if (!is.null(hol)) hol <- as.Date(hol)
    fst <- firstDayInMonth(x)
    bol <- !isBizday(fst, hol)
    if(any(bol)) {
        fst[bol] <- addBizDays(fst[bol], hol=hol)
    }
    fst
}

#' @rdname dateManip
#' @export
lastBizDayInMonth <- function(x, hol=NULL) {
    if (!is.null(hol)) hol <- as.Date(hol)
    lst <- lastDayInMonth(x)
    bol <- !isBizday(lst, hol)
    if(any(bol)) {
        lst[bol] <- subtractBizDays(lst[bol], hol=hol)
    }
    lst
}

#' @rdname dateManip
#' @export
previousMonthEnd <- function(x) {
    day(x) <- 15

    return(lastDayInMonth(subtractMonths(x)))
}

#' Check for business days
#'
#' Which of input 'x' is a "business" day? Standard def'n of weekdays is
#' 1:5 = Monday - Friday. Returns logical.
#' @param x A 'datelike' object
#' @param hol A 'datelike' object containing holiday dates
#' @param wday Weekdays to use (integer; default: Monday - Friday)
#'
#' @return Boolean
#' @export
isBizday <- function(x, hol=NULL, wday=1:5) {
    idays <- as.POSIXlt(x)$wday
    ans <- idays %in% wday
    ans <- as.logical(ans - x %in% hol)

    return(ans)
}

#' Check that input is a date
#'
#' Test if input is a date-like object
#'
#' Test if input is one of my favorite date-like objects. In other words,
#' a \code{Date}, or a \code{POSIX*}.
#'
#' @param x Input to test
#'
#' @return Boolean
#' @export
#' @examples
#' dtstr <- c("1969-08-03", "2011-06-24")
#'
#' is.DateLike("AAA")
#' is.DateLike(251)
#' is.DateLike(as.Date(dtstr))
#' is.DateLike(strptime(dtstr, "\%Y-\%m-\%d"))
is.DateLike <- function(x) {
    is(x, "Date") | is(x, "POSIXt")
}



