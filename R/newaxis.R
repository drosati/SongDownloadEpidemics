eaxis <-
function (side, at = if (log && getRversion() >= "2.14.0") axTicks(side, 
    log = log, nintLog = nintLog) else axTicks(side, log = log), 
    labels = NULL, log = NULL, f.smalltcl = 3/5, at.small = NULL, 
    small.mult = NULL, small.args = list(), draw.between.ticks = TRUE, 
    between.max = 4, outer.at = TRUE, drop.1 = TRUE, las = 1, 
    nintLog = max(10, par("lab")[2L - is.x]), max.at = Inf,
          lab.type="plotmath",...) 
{
    is.x <- side%%2 == 1
    if (is.null(log)) {
        XY <- function(ch) paste0(if (is.x) 
            "x"
        else "y", ch)
        log <- par(XY("log"))
    }
    if (is.finite(max.at <- round(max.at))) {
        if (max.at < 1) 
            stop("'max.at' must be >= 1")
        at <- quantile(at, (0:max.at)/max.at, names = FALSE, 
            type = 3)
        if (!log && is.null(at.small) && {
            d <- diff(at)
            any(abs(diff(d)) > 0.001 * mean(d))
        }) 
            at.small <- FALSE
    }
    use.expr <- log || format.info(as.numeric(at), digits = 7)[3] > 
        0
    if (is.null(labels)) 
        labels <- if (use.expr) 
            pretty10exp(at, drop.1 = drop.1, lab.type=lab.type)
        else TRUE
    else if (length(labels) == 1 && is.na(labels)) 
        labels <- TRUE
    axis(side, at = at, labels = labels, las = las, ...)
    if (log) {
        l1 <- (lat <- log10(at))%%1
        l.int <- l1 < 1e-05 | l1 > 1 - 1e-05
        if (draw.between.ticks && all(l.int)) {
            if (any(diff(lat <- sort(round(lat, 5))) > 1)) {
                nl <- length(lat0 <- lat)
                lat <- lat[1]:lat[nl]
                if (length(lat) > between.max * nl) {
                  lat <- unique(round(seqXtend(lat0, between.max * 
                    nl, "interpolate")))
                  if (is.null(at.small) && median(diff(lat)) > 
                    1.5) 
                    at.small <- FALSE
                }
                at <- 10^lat
                axis(side, at = at, labels = FALSE, las = las, 
                  ...)
            }
        }
    }
    if (is.null(at.small)) {
        at.small <- if (log) {
            if (!all(l.int)) 
                at <- at[l.int]
            if (is.null(small.mult)) 
                small.mult <- 9
            if (length(at)) 
                outer(2:small.mult, c(if (outer.at) at[1]/10, 
                  at))
        }
        else {
            d <- diff(at <- sort(at))
            if (any(abs(diff(d)) > 0.001 * (dd <- mean(d)))) 
                stop("'at' is not equidistant")
            if (is.null(small.mult)) {
                d. <- dd/10^floor(log10(dd))
                small.mult <- {
                  if (d.%%5 == 0) 
                    5
                  else if (d.%%4 == 0) 
                    4
                  else if (d.%%2 == 0) 
                    2
                  else if (d.%%3 == 0) 
                    3
                  else if (d.%%0.5 == 0) 
                    5
                  else 2
                }
            }
            outer(1:(small.mult - 1)/small.mult * dd, c(if (outer.at) at[1] - 
                dd, at), "+")
        }
        if (outer.at) {
            p.u <- sort(par("usr")[if (is.x) 
                1:2
            else 3:4])
            if (log) 
                p.u <- 10^p.u
            at.small <- at.small[p.u[1] <= at.small & at.small <= 
                p.u[2]]
        }
    }
    if (is.numeric(at.small) && any(is.finite(at.small))) 
        do.call(axis, c(list(side, at = at.small, labels = FALSE, 
            tcl = f.smalltcl * par("tcl")), small.args))
}

pretty10exp <-
function (x, drop.1 = FALSE, digits.fuzz = 7,
          lab.type=c("plotmath","latex"))
{
    lab.type <- match.arg(lab.type)
    eT <- floor(log10(abs(x)) + 10^-digits.fuzz)
    mT <- signif(x/10^eT, digits.fuzz)
    ss <- vector("list", length(x))
    if (lab.type=="plotmath") {
        for (i in seq(along = x)) ss[[i]] <- if (x[i] == 0) 
            quote(0)
        else if (drop.1 && mT[i] == 1) 
            substitute(10^E, list(E = eT[i]))
        else if (drop.1 && mT[i] == -1) 
            substitute(-10^E, list(E = eT[i]))
        else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))
        do.call("expression", ss)
    } else {
        ## FIXME: haven't thought about the rules too carefully.
        ## * allow format specifiers as argument (would also have
        ##   to be passed through eaxis)?
        ## * format entire vector together, for consistency?
        ##   (probably)
        for (i in seq(along = x))
            ss[[i]] <- if (x[i] == 0) 
                ""
            else if (drop.1 && mT[i] == 1)
                sprintf("$10^{%s}$",format(eT[i]))
            else if (drop.1 && mT[i] == -1)
                sprintf("$-10^{%s}$",format(eT[i]))
        else sprintf("$%s \\times 10^{%s}$", format(mT[i]),format(eT[i]))
        ss
    }
}

## examples
if (FALSE) {
    ## library(sfsmisc)
    source("newaxis.R")
    x <- 10^seq(-10, -1, length = 201)
    plot(x, pt(x, df=3), type = "l", xaxt = "n", log = "x")
    eaxis(1, at.small=FALSE, lab.type="latex")
}
