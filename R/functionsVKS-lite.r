angle <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: angle
# Author	: VKS
# Date		: 11/20/20
# Args		: x = number representing distance between points A & B
#		: y = number representing distance between points B & C
#		: n = number representing distance between points A & C
# Output	: angle ABC
# -----------------------------------------------------------------
#
# ENFORCE TRIANGLE INEQUALITY
	n <- min(n, 0.99999 * (x + y))
	x <- min(x, 0.99999 * (n + y))
	y <- min(y, 0.99999 * (x + n))
#
# CREATE RESULT
	z <- 180 * (1 - acos((n^2 - x^2 - y^2)/(2 * x * y))/pi)
#
# RETURN RESULT
	z
}
array.bind <- function(x, y) {
# -----------------------------------------------------------------
# Name		: array.bind
# Author	: VKS
# Date		: 2/26/21
# Args		: x = an array
#		: y = an array
# Output	: binds together <x> and <y> along the dimension they differ on
# Notes		: <x> and <y> must have exact same dimnames along every other axis
# -----------------------------------------------------------------
#
# UNLIST
	x <- array.unlist(x)
	y <- array.unlist(y)
#
# COMBINE
	x <- rbind(x, y)
	x <- mat.sort(x, dim(x)[2]:2 - 1, rep(F, dim(x)[2] - 1))
#
# ARRAY
	z <- lapply(x[, -dim(x)[2]], unique)
	z <- array(x[, dim(x)[2]], sapply(z, length), z)
#
# RETURN RESULT
	z
}
array.ex.list <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: array.ex.list
# Author	: VKS
# Date		: 12/21/20
# Args		: x = a list of mat objects or named vectors
#		: y = T/F depending on whether you want union of rows
#		: n = T/F depending on whether you want union of columns
# Output	: array
# -----------------------------------------------------------------
#
# DECIDE MAT OR NAMED VECTOR
	w <- !is.null(dim(x[[1]]))
#
# ALIGN ROWS
	if (y) y <- union else y <- intersect
	if (w) fcn <- function(x) dimnames(x)[[1]] else fcn <- names
	y <- Reduce(y, lapply(x, fcn))
	x <- lapply(x, function(x) map.rname(x, y))
#
# ALIGN COLUMNS
	if (w) {
		if (n) n <- union else n <- intersect
		n <- Reduce(n, lapply(x, function(x) dimnames(x)[[2]]))
		x <- lapply(x, function(x) t(map.rname(t(x), n)))
	}
#
# CREATE RESULT
	z <- simplify2array(x)
#
# RETURN RESULT
	z
}
array.unlist <- function(x, y) {
# -----------------------------------------------------------------
# Name		: array.unlist
# Author	: VKS
# Date		: 9/20/16,1/3/18,1/18,1/26,2/27,3/1,12/20,1/25/19,6/13
# Args		: x = any numerical array
#		: y = a vector of names for the columns of the output corresponding to the dimensions of <x>
# Output	: unlists the contents of an array
# -----------------------------------------------------------------
#
# CHECK INPUT NAMES
	n <- length(dim(x))
	if (missing(y)) y <- col.ex.int(0:n + 1)
	if (length(y) != n + 1) stop("Problem")
#
# CREATE RESULT
	z <- expand.grid(dimnames(x), stringsAsFactors = F)
	names(z) <- y[1:n]
	z[, y[n + 1]] <- as.vector(x)
#
# RETURN RESULT
	z
}
ascending <- function(x) {
# -----------------------------------------------------------------
# Name		: ascending
# Author	: VKS
# Date		: 5/8/17
# Args		: x = a vector
# Output	: T/F depending on whether <x> is ascending
# -----------------------------------------------------------------
	if (any(is.na(x))) stop("Problem")
	z <- x[order(x)]
	z <- all(z == x)
	z
}
avail <- function(x) {
# -----------------------------------------------------------------
# Name		: avail
# Author	: VKS
# Date		: 11/4/16,1/18/19
# Args		: x = a matrix/data-frame
# Output	: For each row, returns leftmost entry with data
# -----------------------------------------------------------------
	fcn <- function(x, y) ifelse(is.na(x), y, x)
	z <- Reduce(fcn, mat.ex.matrix(x))
	z
}
avg.model <- function(x) {
# -----------------------------------------------------------------
# Name		: avg.model
# Author	: VKS
# Date		: 2/27/17,10/10,1/24/18,10/5,1/24/19
# Args		: x = vector of results
# Output	: constant-only (zero-variable) regression model
# -----------------------------------------------------------------
#
# LOSE NA's
	x <- x[!is.na(x)]
#
# CREATE RESULT
	z <- vec.named(mean(x), "Estimate")
	z["Std. Error"] <- sd(x)/sqrt(length(x))
	z["t value"] <- z["Estimate"]/nonneg(z["Std. Error"])
#
# RETURN RESULT
	z
}
avg.winsorized <- function(x, y = 100) {
# -----------------------------------------------------------------
# Name		: avg.winsorized
# Author	: VKS
# Date		: 11/28/17
# Args		: x = a numeric vector
#		: y = number of quantiles
# Output	: mean is computed over the quantiles 2 through <y> - 1
# -----------------------------------------------------------------
	x <- x[!is.na(x)]
	w <- qtl(x, y)
	w <- is.element(w, 3:y - 1)
	z <- x[w]
	z <- mean(z)
	z
}
avg.wtd <- function(x, y) {
# -----------------------------------------------------------------
# Name		: avg.wtd
# Author	: VKS
# Date		: 9/19/16,12/18/17
# Args		: x = a numeric vector
#		: y = a numeric vector of weights
# Output	: returns the weighted mean of <x> given weights <n>
# -----------------------------------------------------------------
	fcn <- function(x, y) sum(x * y)/nonneg(sum(y))
	z <- fcn.num.nonNA(fcn, x, y, F)
	z
}
base.ex.int <- function(x, y = 26) {
# -----------------------------------------------------------------
# Name		: base.ex.int
# Author	: VKS
# Date		: 12/13/17,12/14,12/20/18
# Args		: x = a non-negative integer
#		: y = a positive integer
# Output	: Expresses <x> in base <y>
# -----------------------------------------------------------------
	if (x == 0) z <- 0 else z <- NULL
	while (x > 0) {
		z <- c(x %% y, z)
		x <- (x - x %% y)/y
	}
	z
}
base.to.int <- function(x, y = 26) {
# -----------------------------------------------------------------
# Name		: base.to.int
# Author	: VKS
# Date		: 12/13/17,12/20/18
# Args		: x = a vector of positive integers
#		: y = a positive integer
# Output	: Evaluates the base <y> number <x>
# -----------------------------------------------------------------
	m <- length(x)
	z <- x * y^(m:1 - 1)
	z <- sum(z)
	z
}
bbk <- function(x, y, floW = 1, retW = 5, nBin = 5, doW = NULL, sum.flows = F, lag = 0, delay = 2, idx = NULL, prd.size = 1, sprds = F) {
# -----------------------------------------------------------------
# Name		: bbk
# Author	: VKS
# Date		: 2/22/17,2/24,5/8,10/19,10/20,12/8,1/18/18,2/15,12/21,
#		: 10/26/22
# Args		: x = predictor indexed by yyyymmdd or yyyymm
#		: y = total return index indexed by the same date format as <x>
#		: floW = number of <prd.size>'s over which the predictor should be compounded/summed
#		: retW = return window in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: nBin = number of bins to divide the variable into
#		: doW = day of the week you will trade on (5 = Fri)
#		: sum.flows = T/F depending on whether <x> should be summed or compounded
#		: lag = predictor lag in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: delay = delay in knowing data in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: idx = the index within which you are trading
#		: prd.size = size of each period in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: sprds = T/F depending on whether spread changes, rather than returns, are needed
# Output	: standard model output
# -----------------------------------------------------------------
#
# GET DATA
	x <- bbk.data(x, y, floW, sum.flows, lag, delay, doW, retW, idx, prd.size, sprds)
#
# BIN RETURNS
	z <- lapply(bbk.bin.xRet(x$x, x$fwdRet, nBin, T, T), mat.reverse)
#
# SUMMARY
	z <- c(z, bbk.summ(z$rets, z$bins, retW, ifelse(is.null(doW), 1, 5)))
#
# RETURN RESULT
	z
}
bbk.bin.rets.prd.summ <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: bbk.bin.rets.prd.summ
# Author	: VKS
# Date		: 9/7/16,10/3,10/19,1/2/18,12/19,1/3/19,1/25
# Args		: fcn = function you use to summarize results
#		: x = a matrix/df with rows indexed by time and columns indexed by bins
#		: y = a vector corresponding to the rows of <x> that maps each row to a sub-period of interest (e.g. calendar year)
#		: n = number of rows of <x> needed to cover an entire calendar year
# Output	: Summarizes bin excess returns by sub-periods of interest (as defined by <y>)
# -----------------------------------------------------------------
#
# SUBSET OUT NA VALUES OF <y>
	w <- !is.na(y)
	y <- y[w]
	x <- x[w, ]
	x <- mat.ex.matrix(x)
#
# MODIFY INPUT FUNCTION
	fcn.loc <- function(x) fcn(x, n, T)
#
# APPLY THE FUNCTION
	z <- split(x, y)
	z <- sapply(z, fcn.loc, simplify = "array")
#
# RETURN RESULT
	z
}
bbk.bin.rets.summ <- function(x, y, n = F) {
# -----------------------------------------------------------------
# Name		: bbk.bin.rets.summ
# Author	: VKS
# Date		: 9/7/16,5/19/17,5/26,12/19/18,1/24/19
# Args		: x = a matrix/df with rows indexed by time and columns indexed by bins
#		: y = number of rows of <x> needed to cover an entire calendar year
#		: n = T/F depending on if you want to count number of periods
# Output	: Summarizes bin excess returns arithmetically
# -----------------------------------------------------------------
	z <- c("AnnMn", "AnnSd", "Sharpe", "HitRate", "Beta", "Alpha", "DrawDn", "DDnBeg", "DDnN")
	if (n) z <- c(z, "nPrds")
	z <- matrix(NA, length(z), dim(x)[2], F, list(z, dimnames(x)[[2]]))
#
# BASICS
	if (n) z["nPrds", ] <- sum(!is.na(x[, 1]))
	z["AnnMn", ] <- apply(x, 2, mean, na.rm = T) * y
	z["AnnSd", ] <- apply(x, 2, sd, na.rm = T) * sqrt(y)
	z["Sharpe",] <- 100 * z["AnnMn", ]/z["AnnSd", ]
	z["HitRate",] <- apply(sign(x), 2, mean, na.rm = T) * 50
#
# JENSEN'S ALPHA
	w <- dimnames(x)[[2]] == "uRet"
	if (any(w)) {
		z[c("Alpha", "Beta"), "uRet"] <- 0:1
		h <- !is.na(x[, "uRet"])
		m <- sum(h)
		if (m > 1) {
			vec <- c(rep(1, m), x[h, "uRet"])
			vec <- matrix(vec, m, 2, F, list(1:m, c("Alpha", "Beta")))
			vec <- run.cs.reg(t(x[h, !w]), vec)
			vec[, "Alpha"] <- vec[, "Alpha"] * y
			z[dimnames(vec)[[2]], dimnames(vec)[[1]]] <- t(vec)
		}
	}
#
# DRAWDOWN
	if (dim(x)[1] > 1) {
		x <- x[order(dimnames(x)[[1]]), ] # ORDER MATTERS FOR DRAWDOWN
		w <- fcn.mat.vec(bbk.drawdown, x,, T)
		z["DDnN",] <- colSums(w)
		z["DrawDn",] <- colSums(w * zav(x))
		y <- fcn.mat.num(which.max, w,, T)
		y <- dimnames(x)[[1]][y]
		if (any(substring(y, 5, 5) == "Q")) y <- yyyymm.ex.qtr(y)
		z["DDnBeg",] <- as.numeric(y)
	}
#
# RETURN RESULT
	z
}
bbk.bin.xRet <- function(x, y, n = 5, w = F, h = F) {
# -----------------------------------------------------------------
# Name		: bbk.bin.xRet
# Author	: VKS
# Date		: 9/8/16,9/19,2/15/17,5/8,12/19/18
# Args		: x = a matrix/df of predictors, the rows of which are indexed by time
#		: y = a matrix/df of the same dimensions as <x> containing associated forward returns
#		: n = number of desired bins
#		: w = T/F depending on whether universe return is desired
#		: h = T/F depending on whether full detail or bin returns are needed
# Output	: Returns equal weight bin returns through time
# -----------------------------------------------------------------
#
# SAVE DETAILS
	if (h) rslt <- list(raw.fwd.rets = y, raw = x)
#
# WHEN FORWARD RETURNS ARE NA THEN SET THE PREDICTOR TO NA ALSO
	x <- bbk.holidays(x, y)
#
# COMPUTE BINS
	x <- qtl.eq(x, n)
	if (h) rslt[["bins"]] <- x
#
# COMPUTE EXCESS RETURNS
	uRetVec <- rowMeans(y, na.rm = T)
	y <- mat.ex.matrix(y) - uRetVec
#
# CREATE RESULT
	z <- array.unlist(x, c("date", "security", "bin"))
	z$ret <- unlist(y)
	z <- pivot(mean, z$ret, z$date, z$bin)
	z <- map.rname(z, dimnames(x)[[1]])
	dimnames(z)[[2]] <- paste0("Q", dimnames(z)[[2]])
	z <- mat.ex.matrix(z)
	z$TxB <- z[, 1] - z[, dim(z)[2]]
#
# ADD UNIVERSE RETURN AS NEEDED
	if (w) z$uRet <- uRetVec
#
# RETURN RESULT
	if (h) {
		rslt[["rets"]] <- z
		z <- rslt
	}
	z
}
bbk.data <- function(x, y, floW, sum.flows, lag, delay, doW, retW, idx, prd.size, sprds) {
# -----------------------------------------------------------------
# Name		: bbk.data
# Author	: VKS
# Date		: 12/8/17,1/2/18,1/18,1/2/19,4/12,10/23/22,10/25
# Args		: x = predictor indexed by yyyymmdd or yyyymm
#		: y = total return index indexed by the same date format as <x>
#		: floW = number of <prd.size>'s over which the predictor should be compounded/summed
#		: sum.flows = T/F depending on whether <x> should be summed or compounded
#		: lag = predictor lag in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: delay = delay in knowing data in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: doW = day of the week you will trade on (5 = Fri)
#		: retW = return window in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: idx = the index within which you are trading
#		: prd.size = size of each period in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: sprds = T/F depending on whether spread changes, rather than returns, are needed
# Output	: fetches data required to compute standard model output
# -----------------------------------------------------------------
#
# THROW OUT NA's
	x <- x[!is.na(avail(x)), ]
#
# BASIC CHECKS
	if (!ascending(dimnames(x)[[1]])) stop("Flows are crap")
	if (any(yyyymm.lag(dimnames(x)[[1]][dim(x)[1]], dim(x)[1]:1 - 1, F) != dimnames(x)[[1]])) stop("Missing flow dates")
	if (!ascending(dimnames(y)[[1]])) stop("Returns are crap")
	if (any(yyyymm.lag(dimnames(y)[[1]][dim(y)[1]], dim(y)[1]:1 - 1) != dimnames(y)[[1]])) stop("Missing return dates")
#
# COMPOUND FLOW
	x <- compound.flows(x, floW, sum.flows)
#
# HANDLE LAG AND DELAY
	x <- mat.lag(x, lag + delay)
#
# SUBSET TO THAT PARTICULAR DAY OF THE WEEK
	if (!is.null(doW)) x <- mat.daily.to.weekly(x, doW)
#
# GET FORWARD RETURNS INDEXED BY THE BEGINNING OF THE PERIOD
	y <- bbk.fwdRet(x, y, retW, !sprds)
#
# INDEX CHANGES
	if (!is.null(idx)) y <- Ctry.msci.index.changes(y, idx)
#
# RETURN RESULT
	z <- list(x = x, fwdRet = y)
	z
}
bbk.drawdown <- function(x) {
# -----------------------------------------------------------------
# Name		: bbk.drawdown
# Author	: VKS
# Date		: 5/19/17,12/19/18,1/18/19,1/24,1/30,10/22/22
# Args		: x = a numeric vector
# Output	: returns a logical vector identifying the contiguous periods
#		:	corresponding to max drawdown
# -----------------------------------------------------------------
#
# PRELIMINARIES
	n <- length(x)
	x <- c(0, cumsum(zav(x)))
#
# FIND DRAWDOWN
	z <- list()
	for (j in 1:n) {
		w <- diff(x, j)
		z[[as.character(j)]] <- c(j, order(w)[1] + j, w[order(w)[1]])
	}
	z <- t(simplify2array(z))
	z <- z[order(z[, 3]), ][1, ]
#
# EXPRESS AS LOGICAL
	z <- is.element(1:n, z[2] - z[1]:1)
#
# RETURN RESULT
	z
}
bbk.fanChart <- function(x) {
# -----------------------------------------------------------------
# Name		: bbk.fanChart
# Author	: VKS
# Date		: 10/8/18,12/19,1/31/19
# Args		: x = "rets" part of the output of function bbk
# Output	: quintile fan charts
# -----------------------------------------------------------------
	x <- mat.reverse(x[!is.na(x[, 1]), paste0("Q", 1:5)])
	for (j in 2:dim(x)[1]) x[j, ] <- apply(x[j - 1:0, ], 2, compound)
	z <- mat.reverse(x)/100
	z
}
bbk.fwdRet <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: bbk.fwdRet
# Author	: VKS
# Date		: 9/14/16,9/19,2/2/17,6/30,7/19,1/2/18,10/22/22
# Args		: x = a matrix/data frame of predictors
#		: y = a matrix/data frame of total return indices
#		: n = the number of days in the return window
#		: w = T/F depending on whether returns or spread changes are needed
# Output	: returns a matrix/data frame of the same dimensions as <x>
# -----------------------------------------------------------------
#
# BASIC CHECKS
	if (dim(x)[2] != dim(y)[2]) stop("Problem 1")
	if (any(dimnames(x)[[2]] != dimnames(y)[[2]])) stop("Problem 2")
#
# TOTAL RETURN INDEXED BY TRADE DATE
	y <- ret.ex.idx(y, n, T, w)
#
# MAP TO ROW SPACE OF <x>
	z <- map.rname(y, dimnames(x)[[1]])
#
# REMOVE CASES WHERE THE FORWARD RETURN IS PRECISELY ZERO (MARKET WAS CLOSED THAT DAY)
	z <- excise.zeroes(z)
#
# RETURN RESULT
	z
}
bbk.histogram <- function(x) {
# -----------------------------------------------------------------
# Name		: bbk.histogram
# Author	: VKS
# Date		: 10/8/18
# Args		: x = "rets" part of the output of function bbk
# Output	: return distribution
# -----------------------------------------------------------------
	z <- vec.count(0.01 * round(x$TxB/0.5) * 0.5)
	z <- matrix(z, length(z), 3, F, list(names(z), c("Obs", "Plus", "Minus")))
	z[, "Plus"] <- ifelse(as.numeric(dimnames(z)[[1]]) < 0, NA, z[, "Plus"]/sum(z[, "Plus"]))
	z[, "Minus"] <- ifelse(as.numeric(dimnames(z)[[1]]) < 0, z[, "Minus"]/sum(z[, "Minus"]), NA)
#
# RETURN RESULT
	z
}
bbk.holidays <- function(x, y) {
# -----------------------------------------------------------------
# Name		: bbk.holidays
# Author	: VKS
# Date		: 10/3/16,12/20/17,1/24/19
# Args		: x = a matrix/df of predictors, the rows of which are indexed by time
#		: y = a matrix/df of the same dimensions as <x> containing associated forward returns
# Output	: Sets <x> to NA whenever <y> is NA
# -----------------------------------------------------------------
	fcn <- function(x, y) ifelse(is.na(y), NA, x)
	z <- fcn.mat.vec(fcn, x, y, T)
	z
}
bbk.matrix <- function(x, y, floW, lag, item = "AnnMn", idx = NULL, retW = 5, delay = 2, nBin = 5, doW = 5, sum.flows = F, prd.size = 1, sprds = F) {
# -----------------------------------------------------------------
# Name		: bbk.matrix
# Author	: AM
# Date		: 10/21/22
# Args		: x = predictor indexed by yyyymmdd or yyyymm
#		: y = total return index indexed by the same date format as <x>
#		: floW = number of <prd.size>'s over which the predictor should be compounded/summed
#		: lag = predictor lag in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: item = data to output in the matrix from bbk summary (e.g. AnnMn, Sharpe)
#		: idx = the index within which you are trading
#		: retW = return window in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: delay = delay in knowing data in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: nBin = number of bins to divide the variable into
#		: doW = day of the week you will trade on (5 = Fri)
#		: sum.flows = T/F depending on whether <x> should be summed or compounded
#		: prd.size = size of each period in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: sprds = T/F depending on whether spread changes, rather than returns, are needed
# Output	: standard model output summary value of <item> for "TxB" for various argument combinations
# -----------------------------------------------------------------
#
# CREATE RESULT
	z <- x <- as.list(environment())
	z <- z[!is.element(names(z), c("x", "y", "item"))]
	z <- z[sapply(z, function(x) length(x) > 1)]
	x <- x[!is.element(names(x), c(names(z), "item"))]
	z <- expand.grid(z)
	z[, item] <- rep(NA, dim(z)[1])
#
# POPULATE RESULT
	for (j in 1:dim(z)[1]) {
		y <- lapply(z[, -dim(z)[2]], function(x) x[j])
		cat("\t", paste(paste(names(y), "=", unlist(y)), collapse = ", "), "..\n")
		z[j, item] <- do.call(bbk, c(y, x))[["summ"]][item, "TxB"]
	}
	z <- reshape.wide(z)
#
# RETURN RESULT
	z
}
bbk.summ <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: bbk.summ
# Author	: VKS
# Date		: 10/20/17,11/27,5/9/18,12/21,1/9/19
# Args		: x = bin returns
#		: y = bin memberships
#		: n = return window in days or months depending on whether <x> is YYYYMMDD or YYYYMM
#		: w = quantum size (<n> is made up of non-overlapping windows of size <w>)
# Output	: summarizes by year and overall
# -----------------------------------------------------------------
#
# CHECK QUANTUM SIZE
	if (n %% w != 0) stop("Quantum size is wrong!")
#
# ANNUALIZATION FACTOR
	prdsPerYr <- yyyy.periods.count(dimnames(x)[[1]])
#
# BASIC SUMMARY
	fcn <- function(x) bbk.bin.rets.summ(x, prdsPerYr/n)
	z <- mat.ex.matrix(summ.multi(fcn, x, n/w))
#
# TURNOVER
	fcn <- function(x) bbk.turnover(x) * prdsPerYr/n
	y <- summ.multi(fcn, mat.ex.matrix(y), n/w)
#
# COMBINE
	z <- map.rname(z, c(dimnames(z)[[1]], "AnnTo"))
	z["AnnTo", ] <- map.rname(y, dimnames(z)[[2]])
	z <- list(summ = z)
#
# SUMMARY BY YEAR
	if (n == w) {
		z.ann <- yyyy.ex.period(dimnames(x)[[1]], n)
		z.ann <- bbk.bin.rets.prd.summ(bbk.bin.rets.summ, x, z.ann, prdsPerYr/n)
		z.ann <- rbind(z.ann["AnnMn",,], z.ann["nPrds", "uRet",])
		z.ann <- t(z.ann)
		dimnames(z.ann)[[2]][dim(z.ann)[2]] <- "nPrds"
		z[["annSumm"]] <- z.ann
	}
#
# RETURN RESULT
	z
}
bbk.turnover <- function(x) {
# -----------------------------------------------------------------
# Name		: bbk.turnover
# Author	: VKS
# Date		: 5/19/17,12/18,12/21/18
# Args		: x = a matrix/df of positive integers
# Output	: returns average name turnover per bin
# -----------------------------------------------------------------
	z <- vec.unique(x)
	x <- zav(x)
#
# COMPUTE TURNOVER
	new <- x[-1, ]
	old <- x[-dim(x)[1], ]
	z <- vec.named(rep(NA, length(z)), z)
	for (i in names(z)) z[i] <- mean(nameTo(old == i, new == i), na.rm = T)
#
# FINAL RESULT
	names(z) <- paste0("Q", names(z))
	z["TxB"] <- z["Q1"] + z["Q5"]
	z["uRet"] <- 0
#
# RETURN RESULT
	z
}
bear <- function(x) {
# -----------------------------------------------------------------
# Name		: bear
# Author	: VKS
# Date		: 11/20/20
# Args		: x = vector of returns of the form log(1 + r/100)
# Output	: T/F depending on whether period fell in a bear market
# -----------------------------------------------------------------
	n <- length(x)
	z <- bbk.drawdown(x)
	if (100 * exp(sum(x[z])) < 80) {
		v <- (1:n)[z][1]
		if (v > 1) {
			v <- seq(1, v - 1)
			z[v] <- bear(x[v])
		}
		#
		v <- (1:n)[z][sum(z)]
		if (v < n) {
			v <- seq(v + 1, n)
			z[v] <- bear(x[v])
		}
	} else {
		z <- rep(F, n)
	}
#
# RETURN RESULT
	z
}
best.linear.strategy.blend <- function(x, y) {
# -----------------------------------------------------------------
# Name		: best.linear.strategy.blend
# Author	: VKS
# Date		: 10/5/16
# Args		: x = a return stream from a strategy
#		: y = an isomekic return stream from a strategy
# Output	: Returns optimal weights to put on <x> and <y>
# -----------------------------------------------------------------
#
# SUBSET
	w <- !is.na(x) & !is.na(y)
	x <- x[w]
	y <- y[w]
#
# STATS
	mx <- mean(x)
	my <- mean(y)
	sx <- sd(x)
	sy <- sd(y)
	gm <- correl(x, y)
#
# COVARIANCE MATRIX
	V <- c(sx^2, rep(sx * sy * gm, 2), sy^2)
	V <- matrix(V, 2, 2)
#
# RESULT
	V <- solve(V)
	z <- V %*% c(mx, my)
	z <- renorm(z[, 1])
#
# RETURN RESULT
	z
}
binomial.trial <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: binomial.trial
# Author	: VKS
# Date		: 9/28/17
# Args		: x = probability of success in a 1/0 Bernoulli trial
#		: y = number of coin flips
#		: n = number of heads
#		: w = T/F variable depending on which tail you want
# Output	: returns the likelihood of getting <n> or more/fewer heads
#		: depending on whether <w> is T/F
# -----------------------------------------------------------------
	if (w) pbinom(y - n, y, 1 - x) else pbinom(n, y, x)
}
bond.curve.expand <- function(x) {
# -----------------------------------------------------------------
# Name		: bond.curve.expand
# Author	: VKS
# Date		: 3/11/20
# Args		: x = named vector of interest rates
# Output	: full yield curve
# -----------------------------------------------------------------
	approx(as.numeric(names(x)), as.numeric(x), 1:as.numeric(names(x)[length(x)]), method = "constant", f = 1, rule = 2)$y
}
bond.price <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: bond.price
# Author	: VKS
# Date		: 3/11/20,3/13
# Args		: x = numeric vector representing annual coupon rates
#		: y = integer vector representing years to maturity
#		: n = named vector of interest rates
# Output	: bond prices
# -----------------------------------------------------------------
#
# DETERMINE IF <n> REPRESENTS YIELD CURVE OR TERM STRUCTURE
	w <- length(x) == length(n)
#
# EXPAND OUT YIELD CURVE
	if (!w) n <- bond.curve.expand(n)
#
# COMPUTE PRICES
	z <- rep(0, length(x))
	if (w) discount <- rep(1, length(x)) else discount <- 1
	for (j in seq_along(n)) {
		if (w) {
			discount <- discount/(1 + n/100)
		} else {
			discount <- discount/(1 + n[j]/100)
		}
		z <- z + ifelse(y >= j, x * discount, 0)
		z <- z + ifelse(y == j, 100 * discount, 0)
	}
#
# RETURN RESULT
	z
}
brinson <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: brinson
# Author	: VKS
# Date		: 3/19/20
# Args		: x = numeric vector of benchmark weights
#		: y = numeric vector of active weights
#		: n = numeric vector of returns
#		: w = numeric vector of group memberships
# Output	: performs a Brinson attribution
# -----------------------------------------------------------------
	z <- list()
#
# WEIGHTS BY GROUP
	z[["BmkWgt"]] <- pivot.1d(sum, w, x)
	z[["ActWgt"]] <- pivot.1d(sum, w, y)
#
# NUMERATOR OF RETURNS BY GROUP
	z[["BmkRet"]] <- pivot.1d(sum, w, x * n)
	z[["PorRet"]] <- pivot.1d(sum, w, (x + y) * n)
#
# DATA FRAME
	w <- unique(w)
	w <- sapply(z, function(x) map.rname(x, w))
#
# RETURNS BY GROUP
	w[, "BmkRet"] <- w[, "BmkRet"]/nonneg(w[, "BmkWgt"])
	w[, "PorRet"] <- w[, "PorRet"]/nonneg(rowSums(w[, c("BmkWgt", "ActWgt")]))
	w[, "PorRet"] <- ifelse(is.na(w[, "PorRet"]), w[, "BmkRet"], w[, "PorRet"])
#
# COMPUTE ACTIVES
	w[, "PorRet"] <- w[, "PorRet"] - w[, "BmkRet"]
#
# CREATE RESULT
	z <- list()
	z[["Selec"]] <- sum(w[, "PorRet"] * w[, "BmkWgt"])
	z[["Alloc"]] <- sum(w[, "BmkRet"] * w[, "ActWgt"])
	z[["Intcn"]] <- sum(w[, "PorRet"] * w[, "ActWgt"])
	z <- unlist(z)/100
#
# RETURN RESULT
	z
}
britten.jones <- function(x, y) {
# -----------------------------------------------------------------
# Name		: britten.jones
# Author	: VKS
# Date		: 10/12/17,10/17,12/18/18
# Args		: x = design matrix of a regression with 1st column assumed to be dependent
#		: y = constitutent lagged returns that go into the first period
# Output	: transforms the design matrix as set out in Britten-Jones, M., Neuberger
#		:  , A., & Nolte, I. (2011). Improved inference in regression with overlapping
#		:  observations. Journal of Business Finance & Accounting, 38(5-6), 657-683.
# -----------------------------------------------------------------
	m <- length(y)
	n <- dim(x)[1]
	orig.nms <- dimnames(x)[[2]]
#
# RETURNS: DEPENDENT VARIABLE
	for (i in 1:n) y <- c(y, x[i, 1] - sum(y[i - 1 + 1:m]))
#
# PREDICTORS: INDEPENDENT VARIABLES
	x <- as.matrix(x[, -1])
	z <- matrix(0, n + m, dim(x)[2], F, list(seq(1, m + n), dimnames(x)[[2]]))
	for (i in 0:m) z[1:n + i, ]  <- z[1:n + i, ] + x
#
# CREATE RESULT
	if (det(crossprod(z)) > 0) {
		z <- z %*% solve(crossprod(z)) %*% crossprod(x)
		z <- data.frame(y, z)
		names(z) <- orig.nms
	} else z <- NULL
#
# RETURN RESULT
	z
}
britten.jones.data <- function(x, y, n, w = NULL) {
# -----------------------------------------------------------------
# Name		: britten.jones.data
# Author	: VKS
# Date		: 10/17/17,1/2/18,2/15,2/5/20,10/25/22
# Args		: x = a data frame of predictors
#		: y = total return index of the same size as <x>
#		: n = number of periods of forward returns used
#		: w = the index within which you are trading
# Output	: returns data needed for a Britten-Jones analysis
# -----------------------------------------------------------------
#
# CHECK INPUTS
	if (any(dim(x) != dim(y))) stop("x/y are mismatched!")
#
# SINGLE-PERIOD RETURNS
	prd.ret <- 100 * mat.lag(y, -1)/nonneg(y) - 100
	prd.ret <- list(prd1 = prd.ret) # FIRST PERIOD FORWARD RETURN
	if (n > 1) for (i in 2:n) prd.ret[[paste0("prd", i)]] <- mat.lag(prd.ret[["prd1"]], 1 - i) # SUBSEQUENT PERIOD FORWARD RETURN
#
# FORWARD RETURN (INDEXED BY BOP)
	y <- ret.ex.idx(y, n, T, T)
#
# REMOVE CASES WHERE THE FORWARD RETURN IS PRECISELY ZERO
# FOR THE WHOLE PERIOD AND ALL SUB-PERIODS (MARKET WAS CLOSED THAT PERIOD)
	vec <- as.numeric(unlist(y))
	w1 <- !is.na(vec) & abs(vec) < 1e-6
	#
	if (any(w1)) {
		for (i in names(prd.ret)) {
			w2 <- as.numeric(unlist(prd.ret[[i]]))
			w2 <- is.na(w2) | abs(w2) < 1e-6
			w1 <- w1 & w2
		}
	}
	#
	if (any(w1)) {
		vec <- ifelse(w1, NA, vec)
		y <- matrix(vec, dim(y)[1], dim(y)[2], F, dimnames(y))
	}
#
# INDEX CHANGES
	if (!is.null(w)) y <- Ctry.msci.index.changes(y, w)
#
# STANDARD MODEL
	x <- bbk.bin.xRet(x, y, 5, F, T)
#
# LOG RETURNS
	y <- ret.to.log(y)
	prd.ret <- lapply(prd.ret, ret.to.log)
#
# ENSURE THE FULL & SINGLE-PERIOD RETURNS HAVE PRECISELY THE SAME OBSERVATIONS
	w1 <- !is.na(unlist(y))
	for (i in names(prd.ret)) {
		vec <- as.numeric(unlist(prd.ret[[i]]))
		vec <- ifelse(w1, vec, NA)
		prd.ret[[i]] <- matrix(vec, dim(y)[1], dim(y)[2], F, dimnames(y))
	}
#
# ACTIVE RETURNS
	fcn <- function(x) x - rowMeans(x, na.rm = T)
	y <- fcn(y)
	prd.ret <- lapply(prd.ret, fcn)
#
# STACKED REGRESSION ON BIN MEMBERSHIP
	z <- NULL
	for (i in dimnames(x$bins)[[2]]) {
		if (sum(!is.na(x$bins[, i]) & !duplicated(x$bins[, i])) > 1) {
#
# FOR SMALL GAPS, DUMP NA's IN BIN 3
			df <- as.numeric(x$bins[, i])
			w1 <- !is.na(df)
			n.beg <- find.data(w1, T)
			n.end <- find.data(w1, F)
			if (n > 1 & n.end - n.beg + 1 > sum(w1)) {
				vec <- find.gaps(w1)
				if (any(vec < n - 1)) {
					vec <- vec[vec < n - 1]
					for (j in names(vec)) df[as.numeric(j) + 1:as.numeric(vec[j]) - 1] <- 3
				}
			}
#
# SIMPLE DESIGN MATRIX
			df <- mat.ex.vec(df)
			w1 <- rowSums(df) == 1
#
# INTRODUCE A COEFFICIENT TO REPRESENT TXB RETURN
			if (all(is.element(c("Q1", "Q5"), names(df)))) {
				df$TxB <- (df$Q1 - df$Q5)/2
			} else if (any(names(df) == "Q1")) {
				df$TxB <- df$Q1/2
			} else if (any(names(df) == "Q5")) {
				df$TxB <- -df$Q5/2
			}
			df <- df[, !is.element(names(df), c("Q1", "Q5"))]
			df$ActRet <- y[, i]
			df <- mat.last.to.first(df)
#
# APPLY BRITTEN-JONES AND THEN STACK
			w1 <- !is.na(prd.ret[["prd1"]][, i]) & w1
			n.beg <- find.data(w1, T)
			n.end <- find.data(w1, F)
			if (n == 1 | n.end - n.beg + 1 == sum(w1)) {
				z <- britten.jones.data.stack(df[n.beg:n.end, ], n, prd.ret, n.beg, i)
			} else {
				vec <- find.gaps(w1)
#
# HANDLE SMALL RETURN GAPS
				if (any(vec < n - 1)) stop("Small return gap detected: i = ", i, ", retHz =", n, "...\n")
#
# WHEN YOU HAVE BIG GAPS THEN TREAT AS SEPARATE ENTITIES
				if (any(vec >= n - 1)) {
					vec <- vec[vec >= n - 1]
					n.beg <- c(n.beg, as.numeric(names(vec)) + as.numeric(vec))
					n.end <- c(as.numeric(names(vec)) - 1, n.end)
					for (j in seq_along(n.beg)) z <- britten.jones.data.stack(df[n.beg[j]:n.end[j], ], n, prd.ret, n.beg[j], i)
				}
			}
		}
	}
#
# RETURN RESULT
	z
}
britten.jones.data.stack <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: britten.jones.data.stack
# Author	: VKS
# Date		: 10/17/17
# Args		: x =
#		: y =
#		: n =
#		: w =
#		: h =
# Output	: applies the Britten-Jones transformation to a subset and then stacks
# -----------------------------------------------------------------
#
# REMOVE ANY DEGENERATE COLUMNS THAT MIGHT HAVE CREPT IN
	u <- colSums(x[, -1] == 0) == dim(x)[1]
	if (any(u)) {
		u <- !is.element(dimnames(x)[[2]], dimnames(x)[[2]][-1][u])
		x <- x[, u]
	}
#
# PERFORM BRITTEN-JONES ADJUSTMENT
	if (y > 1) {
		vec <- NULL
		for (j in names(n)[-y]) vec <- c(vec, n[[j]][w, h])
		n <- dim(x)[1]
		x <- britten.jones(x, vec)
		if (is.null(x)) cat("Discarding", n, "observations for", h, "due to Britten-Jones singularity ...\n")
	}
#
# MAP TO FULL COLUMN RANGE PRIOR TO STACKING
	if (!is.null(x)) x <- mat.ex.matrix(zav(t(map.rname(t(x), c("ActRet", paste0("Q", 2:4), "TxB")))))
#
# STACK
	if (!is.null(x)) {
		if (is.null(z)) {
			dimnames(x)[[1]] <- 1:dim(x)[1]
			z <- x
		} else {
			dimnames(x)[[1]] <- 1:dim(x)[1] + dim(z)[1]
			z <- rbind(z, x)
		}
	}
#
# RETURN RESULT
	z
}
char.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: char.ex.int
# Author	: VKS
# Date		: 11/28/16
# Args		: x = a string of integers
# Output	: the characters whose ascii values correspond to <x>
# -----------------------------------------------------------------
	z <- rawToChar(as.raw(x))
	z <- strsplit(z, "")[[1]]
	z
}
char.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: char.lag
# Author	: VKS
# Date		: 11/2/18
# Args		: x = a vector of characters
#		: y = a number
# Output	: lags <x> by <y>
# -----------------------------------------------------------------
	obj.lag(x, y, char.to.int, char.ex.int)
}
char.seq <- function(x, y, n = 1) {
# -----------------------------------------------------------------
# Name		: char.seq
# Author	: VKS
# Date		: 1/3/18
# Args		: x = a SINGLE character
#		: y = a SINGLE character
#		: n = quantum size
# Output	: returns a sequence of ASCII characters between (and including) x and y
# -----------------------------------------------------------------
	obj.seq(x, y, char.to.int, char.ex.int, n)
}
char.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: char.to.int
# Author	: VKS
# Date		: 11/28/16
# Args		: x = a string of single characters
# Output	: ascii values
# -----------------------------------------------------------------
	z <- paste(x, collapse = "")
	z <- strtoi(charToRaw(z), 16L)
	z
}
char.to.num <- function(x) {
# -----------------------------------------------------------------
# Name		: char.to.num
# Author	: VKS
# Date		: 11/22/16
# Args		: x = a vector of strings
# Output	: coerces to numeric much more brutally than does as.numeric
# -----------------------------------------------------------------
	z <- txt.replace(x, "\"", "")
	z <- txt.replace(z, ",", "")
	z <- as.numeric(z)
	z
}
classification.threshold <- function(x, y) {
# -----------------------------------------------------------------
# Name		: classification.threshold
# Author	: VKS
# Date		: 12/2/20
# Args		: x = a 1/0 vector
#		: y = a vector of predictors of the same length as <x>
# Output	: threshold value that causes fewest classification errors
# -----------------------------------------------------------------
	n <- length(x)
#
# SORT
	x <- x[order(y)]
	y <- y[order(y)]
#
# CREATE RESULT
	z <- c(n + 1, y[1] - 1)
	for (j in 2:n) {
		v <- mean(y[j - 1:0])
		w <- y > v
		h <- min(sum(w) + sum(x[!w]) - sum(x[w]), sum(!w) + sum(x[w]) - sum(x[!w]))
		if (h < z[1]) z <- c(h, v)
	}
#
# RETURN RESULT
	z
}
col.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: col.ex.int
# Author	: VKS
# Date		: 1/30/17,12/14,12/20/18,1/18/19,1/25,1/22/20
# Args		: x = a vector of positive integers
# Output	: Returns the relevant excel column (1 = "A", 2 = "B", etc.)
# -----------------------------------------------------------------
	z <- rep("", length(x))
	w <- x > 0
	while (any(w)) {
		h <- x[w] %% 26
		h <- ifelse(h == 0, 26, h)
		x[w] <- (x[w] - h)/26
		z[w] <- paste0(char.ex.int(h + 64), z[w])
		w <- x > 0
	}
#
# RETURN RESULT
	z
}
col.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: col.lag
# Author	: VKS
# Date		: 1/30/17,1/3/18,1/22/20
# Args		: x = string representation of an excel column
#		: y = an integer representing the desired column lag
# Output	: Lags <x> by <y> columns
# -----------------------------------------------------------------
	obj.lag(x, y, col.to.int, col.ex.int)
}
col.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: col.to.int
# Author	: VKS
# Date		: 1/30/17,12/13,12/14,12/20/18,1/25/19
# Args		: x = a vector of string representations of excel columns
# Output	: Returns the relevant associated integer (1 = "A", 2 = "B", etc.)
# -----------------------------------------------------------------
#
# BREAK INTO INDIVIDUAL CHARACTERS
	z <- lapply(vec.to.list(x), txt.to.char)
#
# CONVERT TO INTEGER
	z <- lapply(z, function(x) char.to.int(x) - 64)
#
# RECOVER VECTOR
	z <- as.numeric(sapply(z, base.to.int))
#
# RETURN RESULT
	z
}
combinations <- function(x, y) {
# -----------------------------------------------------------------
# Name		: combinations
# Author	: VKS
# Date		: 6/2/17,3/9/18
# Args		: x = a vector
#		: y = an integer between 1 and <length(x)>
# Output	: returns all possible combinations of <y> values of <x>
# -----------------------------------------------------------------
#
# FIRST COMBINATION
	w <- rep(F, length(x))
	if (y > 0) w[1:y] <- T
#
# ALL THE OTHERS
	if (all(w)) {
		z <- paste(x, collapse = " ")
	} else if (all(!w)) {
		z <- ""
	} else {
		z <- NULL
		while (any(w)) {
			z <- c(z, paste(x[w], collapse = " "))
			w <- combinations.next(w)
		}
	}
#
# RETURN RESULT
	z
}
combinations.ex.int <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: combinations.ex.int
# Author	: VKS
# Date		: 10/16/19
# Args		: x = a positive integer
#		: y = a positive integer
#		: n = a positive integer
# Output	: inverse of combinations.to.int; returns a logical vector
#		:	of length <n>, <y> of which elements are T
# -----------------------------------------------------------------
	z <- x <= choose(n - 1, y - 1)
	if (n > 1 & z) {
		z <- c(z, combinations.ex.int(x, y - 1, n - 1))
	} else if (n > 1 & !z) {
		z <- c(z, combinations.ex.int(x - choose(n - 1, y - 1), y, n - 1))
	}
#
# RETURN RESULT
	z
}
combinations.next <- function(x) {
# -----------------------------------------------------------------
# Name		: combinations.next
# Author	: VKS
# Date		: 6/13/17,1/30/19
# Args		: x = a logical vector
# Output	: returns the next combination in dictionary order
# -----------------------------------------------------------------
#
# FIND THE LENGTH OF <x>
	m <- length(x)
#
# FIND THE LAST <FALSE>
	n <- find.data(!x, F)
#
# FIND THE LAST <TRUE> BEFORE THE LAST <FALSE>
	if (any(x[1:n])) {
		n <- find.data(x[1:n], F)
		nT <- sum(x) - sum(x[1:n])
		x[n:m] <- F
		x[n + 1 + 0:nT] <- T
		z <- x
	} else {
		z <- rep(F, m)
	}
#
# RETURN RESULT
	z
}
combinations.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: combinations.to.int
# Author	: VKS
# Date		: 10/16/19
# Args		: x = a logical vector
# Output	: maps each particular way to choose <sum(x)> things
#		:	amongst <length(x)> things to the number line
# -----------------------------------------------------------------
	n <- length(x)
	m <- sum(x)
	if (m == 0 | n == 1) {
		z <- 1
	} else if (x[1]) {
		z <- combinations.to.int(x[-1])
	} else {
		z <- choose(n - 1, m - 1) + combinations.to.int(x[-1])
	}
	#
	# RETURN RESULT
	z
}
compound <- function(x) {
# -----------------------------------------------------------------
# Name		: compound
# Author	: VKS
# Date		: 9/21/16,1/3/18,3/14
# Args		: x = a vector of percentage returns
# Output	: Outputs the compounded return
# -----------------------------------------------------------------
	z <- !is.na(x)
	if (any(z)) z <- 100 * product(1 + x[z]/100) - 100 else z <- NA
	z
}
compound.flows <- function(x, y, n = F) {
# -----------------------------------------------------------------
# Name		: compound.flows
# Author	: VKS
# Date		: 10/22/22
# Args		: x = a matrix/data-frame of percentage flows
#		: y = number of trailing rows to compound/sum
#		: n = if T, flows get summed. Otherwise they get compounded.
# Output	: compounded flows over <n> trailing periods indexed by last day in the flow window
# -----------------------------------------------------------------
#
# FIND NA's
	h <- nonneg(mat.to.obs(x))
#
# PRELIMINARIES
	z <- zav(x)
	if (!n) z <- log(1 + z/100)
#
# ROLLING WINDOWS
	z <- mat.rollsum(z, y)
	if (!n) z <- 100 * exp(z) - 100
#
# ENSURE FINAL DATE IN EACH ROLLING WINDOW IS NON-NA
	z <- z * h
#
# RETURN RESULT
	z
}
compound.sf <- function(x, y) {
# -----------------------------------------------------------------
# Name		: compound.sf
# Author	: VKS
# Date		: 11/1/16,11/2,12/19/17
# Args		: x = a matrix/data-frame of percentage flows
#		: y = if T, flows get summed. Otherwise they get compounded.
# Output	: compounds flows
# -----------------------------------------------------------------
	if (y) fcn <- sum else fcn <- compound
	w <- rowSums(mat.to.obs(x)) > dim(x)[2]/2
	x <- zav(x)
	z <- rep(NA, dim(x)[1])
	if (any(w)) z[w] <- fcn.mat.num(fcn, x[w, ], , F)
	z
}
correl <- function(x, y, n = T) {
# -----------------------------------------------------------------
# Name		: correl
# Author	: VKS
# Date		: 9/13/16,9/23,11/3,12/18/17
# Args		: x = a numeric vector/matrix/data frame
#		: y = either missing or a numeric isomekic vector
#		: n = T/F depending on whether rank correlations are desired
# Output	: the estimated correlation between <x> and <y> or the columns of <x>
# -----------------------------------------------------------------
	if (missing(y)) fcn.mat.col(cor, x, , n) else fcn.mat.col(cor, x, y, n)
}
correl.PrcMo <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: correl.PrcMo
# Author	: VKS
# Date		: 6/8/17,1/2/18,10/25/22
# Args		: x = one-day flow percentage
#		: y = total return index
#		: n = flow window
#		: w = the number of days needed for the flow data to be known
# Output	: returns correlation of <n> day flows with price momentum (175d lag 10)
# -----------------------------------------------------------------
#
# COMPOUND FLOWS
	x <- compound.flows(x, n, F)
	dimnames(x)[[1]] <- yyyymmdd.lag(dimnames(x)[[1]], -w)
#
# PRICE MOMENTUM
	z <- map.rname(y, yyyymmdd.lag(dimnames(y)[[1]], 175))
	z <- nonneg(z)
	y <- as.matrix(y)/z
	dimnames(y)[[1]] <- yyyymmdd.lag(dimnames(y)[[1]], -10)
#
# RANK
	x <- qtl.eq(x, 5)
	y <- qtl.eq(y, 5)
#
# CORREL
	x <- x[is.element(dimnames(x)[[1]], dimnames(y)[[1]]), ]
	y <- y[dimnames(x)[[1]], ]
	z <- correl(unlist(x), unlist(y), F)
#
# RETURN RESULT
	z
}
covar <- function(x) {
# -----------------------------------------------------------------
# Name		: covar
# Author	: VKS
# Date		: 9/13/16,12/18/17,2/11/19,2/21
# Args		: x = a matrix
# Output	: efficient estimated covariance between the columns of <x>
# -----------------------------------------------------------------
	cov(x, use = "pairwise.complete.obs")
}
cpt.RgnSec <- function(x, y) {
# -----------------------------------------------------------------
# Name		: cpt.RgnSec
# Author	: VKS
# Date		: 11/29/16,1/23/18
# Args		: x = a vector of Sectors
#		: y = a vector of country codes
# Output	: makes Region-Sector groupings
# -----------------------------------------------------------------
#
# CONVERT ALL COUNTRIES TO COUNTRY GROUPS
	y <- Ctry.to.CtryGrp(y)
#
# CONVERT SOME SECTORS TO SECTOR GROUPS
	z <- GSec.to.GSgrp(x)
	z <- ifelse(is.element(z, "Cyc"), x, z)
#
# DEFINE MAP
	vec <- c(seq(15, 25, 5), "Def", "Fin")
	vec <- txt.expand(vec, c("Pac", "Oth"), , T)
	vec <- vec.named(c(seq(1, 9, 2), 1 + seq(1, 9, 2)), vec)
	vec["45-Pac"] <- vec["45-Oth"] <- 11
#
# IMPLEMENT MAP
	z <- paste(z, y, sep = "-")
	z <- map.rname(vec, z)
	z <- as.numeric(z)
#
# RETURN RESULT
	z
}
cpt.RgnSecJP <- function(x, y) {
# -----------------------------------------------------------------
# Name		: cpt.RgnSecJP
# Author	: VKS
# Date		: 4/7/20
# Args		: x = a vector of Sectors
#		: y = a vector of country codes
# Output	: makes Region-Sector groupings
# -----------------------------------------------------------------
#
# CONVERT ALL COUNTRIES TO COUNTRY GROUPS
	y <- ifelse(is.element(y, c("US", "CA")), "NoAm", Ctry.to.CtryGrp(y))
#
# CONVERT SOME SECTORS TO SECTOR GROUPS
	z <- GSec.to.GSgrp(x)
	z <- ifelse(is.element(z, "Cyc"), x, z)
#
# DEFINE MAP
	vec <- c(seq(15, 25, 5), "Def", "Fin")
	vec <- txt.expand(vec, c("Pac", "NoAm", "Oth"), , T)
	#
	x <- NULL
	for (j in 1:3) x <- c(x, seq(j, by = 3, length.out = 5))
	vec <- vec.named(x, vec)
	vec[paste("45", c("Pac", "NoAm", "Oth"), sep = "-")] <- length(vec) + 1
#
# IMPLEMENT MAP
	z <- paste(z, y, sep = "-")
	z <- map.rname(vec, z)
	z <- as.numeric(z)
#
# RETURN RESULT
	z
}
cptRollingAverageWeights <- function(x = 4, y = 100, n = 0) {
# -----------------------------------------------------------------
# Name		: cptRollingAverageWeights
# Author	: VKS
# Date		: 9/6/16
# Args		: x = number of trailing weeks to use
#		: y = weight on the earliest as a percentage of weight on latest week
#		: n = number of additional weeks to lag data
# Output	: Returns weights on individual weeks with the most recent week being to the RIGHT
# -----------------------------------------------------------------
#
# COMPUTE WEIGHTS
	z <- x - 1
	z <- (y/100)^(1/z)
	z <- (z^(x:1 - 1))
	z <- z/sum(z)
#
# INCORPORATE LAGS
	z <- c(z, rep(0, n))
#
# RETURN RESULT
	z
}
Ctry.info <- function(x, y) {
# -----------------------------------------------------------------
# Name		: Ctry.info
# Author	: VKS
# Date		: 7/5/17,1/23/18,4/25
# Args		: x = a vector of country codes
#		: y = a column in the classif-ctry file
# Output	: handles the addition and removal of countries from an index
# Example	: Ctry.info("PK", "CtryNm")
# -----------------------------------------------------------------
	z <- mat.read(parameters("classif-ctry"), ",")
	z <- map.rname(z, x)[, y]
	z
}
Ctry.msci <- function(x) {
# -----------------------------------------------------------------
# Name		: Ctry.msci
# Author	: VKS
# Date		: 6/27/17,12/4,1/16/18,12/19,5/20/22
# Args		: x = an index name such as ACWI/EAFE/EM
# Output	: Countries added or removed from the index in ascending order
# -----------------------------------------------------------------
#
# READ IN CLASSIFICATION CHANGES
	z <- parameters("MsciCtryClassification")
	z <- mat.read(z, "\t", NULL)
	z <- z[order(z$yyyymm), ]
#
# DEFINE SUBSTITUTIONS
	if (x == "ACWI") {
		rein <- c("Developed", "Emerging")
	} else if (x == "EAFE") {
		rein <- "Developed"
	} else if (x == "EM") {
		rein <- "Emerging"
	} else if (x == "Frontier") {
		rein <- "Frontier"
	} else stop("Bad Index")
	raus <- setdiff(c("Developed", "Emerging", "Frontier", "Standalone"), rein)
#
# IMPLEMENT SUBSTITUTIONS
	vec <- as.character(unlist(mat.subset(z, c("From", "To"))))
	vec <- ifelse(is.element(vec, rein), "in", vec)
	vec <- ifelse(is.element(vec, raus), "out", vec)
	z[, c("From", "To")] <- vec
#
# TOSS DEGENERATES
	z <- z[z$From != z$To, ]
#
# CREATE RESULT
	z <- mat.subset(z, c("CCode", "To", "yyyymm"))
	dimnames(z)[[2]] <- c("CCODE", "ACTION", "YYYYMM")
	z$ACTION <- toupper(z$ACTION)
#
# RETURN RESULT
	z
}
Ctry.msci.index.changes <- function(x, y) {
# -----------------------------------------------------------------
# Name		: Ctry.msci.index.changes
# Author	: VKS
# Date		: 6/27/17,6/30,7/5,7/6,12/5
# Args		: x = a matrix/df of total returns indexed by the beginning
#		:	of the period (trade date in yyyymmdd format)
#		: y = an MSCI index such as ACWI/EAFE/EM
# Output	: handles the addition and removal of countries from an index
# -----------------------------------------------------------------
#
# LIST ALL THE COUNTRIES YOU EXPECT
	super.set <- Ctry.msci.members.rng(y, dimnames(x)[[1]][1], dimnames(x)[[1]][dim(x)[1]])
#
# READ IN INDEX CHANGES
	z <- Ctry.msci(y)
	if (nchar(dimnames(x)[[1]][1]) == 8) z$YYYYMM <- yyyymmdd.ex.yyyymm(z$YYYYMM) # LAST WEEKDAY
#
# HANDLE CURRENCIES
	if (nchar(dimnames(x)[[2]][1]) == 3) {
		z$CCODE <- Ctry.info(z$CCODE, "Curr")
		super.set <- Ctry.info(super.set, "Curr")
		z <- z[!is.element(z$CCODE, c("USD", "EUR")), ]
	}
#
# MISSING COUNTRIES/CURRENCIES
	w <- !is.element(z$CCODE, dimnames(x)[[2]])
	if (any(w)) {
		w2 <- is.element(super.set, z$CCODE[w])
		z <- z[!w, ]
		if (any(w2)) err.raise(super.set[w2], F, "Warning: No data for the following")
	}
#
# IMPLEMENT INDEX CHANGES
	u.Ctry <- z$CCODE[!duplicated(z$CCODE)]
	z <- z[order(z$YYYYMM), ] # DATES ASCEND
	for (i in u.Ctry) {
		vec <- z$CCODE == i
		if (z[vec, "ACTION"][1] == "OUT") vec <- c("19720809", z[vec, "YYYYMM"]) else vec <- z[vec, "YYYYMM"]
		if (length(vec) %% 2 == 0) vec <- c(vec, "30720809")
		#
		w <- dimnames(x)[[1]] < vec[1]
		vec <- vec[-1]
		#
		while (length(vec) > 0) {
			w <- w | (dimnames(x)[[1]] >= vec[1] & dimnames(x)[[1]] < vec[2])
			vec <- vec[-1]
			vec <- vec[-1]
		}
		#
		x[w, i] <- NA # NULL OUT RETURNS
	}
	z <- x
#
# RETURN RESULT
	z
}
Ctry.msci.members <- function(x, y) {
# -----------------------------------------------------------------
# Name		: Ctry.msci.members
# Author	: VKS
# Date		: 12/4/17,12/5,2/15/18,4/25,1/30/19,5/20/22
# Args		: x = an index name such as ACWI/EAFE/EM
#		: y = one of the following:
#		:	(a) a YYYYMM date
#		:	(b) a YYYYMMDD date
#		:	(c) "" for a static series
# Output	: lists countries in an index at <y>
# -----------------------------------------------------------------
#
# INDEX MEMBERS IN 2016
	z <- mat.read(parameters("MsciCtry2016"), ",")
	z <- dimnames(z)[[1]][is.element(z[, x], 1)]
#
# CUT POINT
	point.in.2016 <- "201603"
	if (nchar(y) == 8) point.in.2016 <- "20160331"
#
# READ IN INDEX CHANGES IF NEEDED
	if (y != "") {
		x <- Ctry.msci(x)
		if (nchar(y) == 8) x$YYYYMM <- yyyymmdd.ex.yyyymm(x$YYYYMM) # LAST WEEKDAY
	}
#
# HANDLE INDEX ADDITIONS/DELETIONS
	if (y != "" & y > point.in.2016) {
		w <- x$YYYYMM >= point.in.2016
		w <- w & x$YYYYMM <= y
		if (any(w)) {
			for (i in 1:sum(w)) {
				if (x[w, "ACTION"][i] == "IN") z <- union(z, x[w, "CCODE"][i])
				if (x[w, "ACTION"][i] == "OUT") z <- setdiff(z, x[w, "CCODE"][i])
			}
		}
	}
	if (y != "" & y < point.in.2016) {
		w <- x$YYYYMM <= point.in.2016
		w <- w & x$YYYYMM > y
		if (any(w)) {
			x <- mat.reverse(x)
			w <- rev(w)
			x[, "ACTION"] <- ifelse(x[, "ACTION"] == "IN", "OUT", "IN")
			for (i in 1:sum(w)) {
				if (x[w, "ACTION"][i] == "IN") z <- union(z, x[w, "CCODE"][i])
				if (x[w, "ACTION"][i] == "OUT") z <- setdiff(z, x[w, "CCODE"][i])
			}
		}
	}
#
# RETURN RESULT
	z
}
Ctry.msci.members.rng <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: Ctry.msci.members.rng
# Author	: VKS
# Date		: 12/5/17
# Args		: x = an index name such as ACWI/EAFE/EM
#		: y = a YYYYMM or YYYYMMDD date
#		: n = after <y> and of the same date type
# Output	: lists countries that were ever in an index between <y> and <n>
# -----------------------------------------------------------------
	if (nchar(y) != nchar(n) | y >= n) stop("Problem")
#
# FIND INDEX MEMBERS AT THE BEGINNING
	z <- Ctry.msci.members(x, y)
#
# READ IN INDEX CHANGES IF NEEDED
	x <- Ctry.msci(x)
	if (nchar(y) == 8) x$YYYYMM <- yyyymmdd.ex.yyyymm(x$YYYYMM) # LAST WEEKDAY
#
# INDEX ADDITIONS
	w <- x$YYYYMM >= y
	w <- w & x$YYYYMM <= n
	w <- w & x$ACTION == "IN"
	if (any(w)) z <- union(z, x[w, "CCODE"])
#
# RETURN RESULT
	z
}
Ctry.to.CtryGrp <- function(x) {
# -----------------------------------------------------------------
# Name		: Ctry.to.CtryGrp
# Author	: VKS
# Date		: 11/29/16
# Args		: x = a vector of country codes
# Output	: makes Country groups
# -----------------------------------------------------------------
	z <- c("JP", "AU", "NZ", "HK", "SG", "CN", "KR", "TW", "PH", "ID", "TH", "MY", "KY", "BM")
	z <- ifelse(is.element(x, z), "Pac", "Oth")
	z
}
day.ex.date <- function(x) {
# -----------------------------------------------------------------
# Name		: day.ex.date
# Author	: VKS
# Date		: 9/8/16
# Args		: x = a vector of R dates
# Output	: calendar dates
# -----------------------------------------------------------------
	format(x, "%Y%m%d")
}
day.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: day.ex.int
# Author	: VKS
# Date		: 1/3/18
# Args		: x = an integer or vector of integers
# Output	: the <x>th day after Monday, January 1, 2018
# -----------------------------------------------------------------
	format(as.Date(x, origin = "2018-01-01"), "%Y%m%d")
}
day.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: day.lag
# Author	: VKS
# Date		: 11/11/16,1/3/18
# Args		: x = a vector of calendar dates
#		: y = an integer or vector of integers (if <x> and <y> are vectors then <y> isomekic)
# Output	: lags <x> by <y> days.
# -----------------------------------------------------------------
	obj.lag(x, y, day.to.int, day.ex.int)
}
day.seq <- function(x, y, n = 1) {
# -----------------------------------------------------------------
# Name		: day.seq
# Author	: VKS
# Date		: 1/3/18
# Args		: x = a single calendar date
#		: y = a single calendar date
#		: n = quantum size in calendar date
# Output	: returns a sequence of calendar dates between (and including) x and y
# -----------------------------------------------------------------
	obj.seq(x, y, day.to.int, day.ex.int, n)
}
day.to.date <- function(x) {
# -----------------------------------------------------------------
# Name		: day.to.date
# Author	: VKS
# Date		: 12/6/19,12/31
# Args		: x = a vector of calendar dates
# Output	: converts to an R date
# -----------------------------------------------------------------
	as.Date(x, "%Y%m%d")
}
day.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: day.to.int
# Author	: VKS
# Date		: 1/3/18
# Args		: x = a vector of calendar dates
# Output	: number of days after Monday, January 1, 2018
# -----------------------------------------------------------------
	as.numeric(day.to.date(x) - as.Date("2018-01-01"))
}
day.to.week <- function(x, y) {
# -----------------------------------------------------------------
# Name		: day.to.week
# Author	: VKS
# Date		: 4/12/19
# Args		: x = a vector of calendar dates
#		: y = an integer representing the day the week ends on
#		:	0 is Sun, 1 is Mon, ..., 6 is Sat
# Output	: maps days to weeks
# -----------------------------------------------------------------
	x <- day.to.int(x)
	z <- (x + 1) %% 7
	z <- ifelse(z <= y, y - z, 7 + y - z)
	z <- day.ex.int(x + z)
	z
}
day.to.weekday <- function(x) {
# -----------------------------------------------------------------
# Name		: day.to.weekday
# Author	: VKS
# Date		: 9/14/16,1/3/18
# Args		: x = a vector of calendar dates
# Output	: Converts to 0 = Sun, 1 = Mon, ..., 6 = Sat
# -----------------------------------------------------------------
	z <- day.to.int(x)
	z <- z + 1
	z <- as.character(z %% 7)
	z
}
decimal.format <- function(x, y) {
# -----------------------------------------------------------------
# Name		: decimal.format
# Author	: VKS
# Date		: 11/29/20
# Args		: x = numeric vector
#		: y = positive integer
# Output	: rounds <x> to <y> decimals and renders as nice character vector
# -----------------------------------------------------------------
#
# RETURN RESULT
	x <- round(x, y)
	y <- 10^(-y - 1)
#
# CREATE RESULT
	z <- as.character(x + y * ifelse(x < 0, -1, 1))
	z <- txt.left(z, nchar(z) - 1)
#
# RETURN RESULT
	z
}
dir.all.files <- function(x, y) {
# -----------------------------------------------------------------
# Name		: dir.all.files
# Author	: VKS
# Date		: 11/9/16,2/27/18
# Args		: x = a path such as "C:\\temp"
#		: y = a string such as "*.txt"
# Output	: Returns all files in the folder including sub-directories
# -----------------------------------------------------------------
	z <- dir(x, y, recursive = T)
	if (length(z) > 0) {
		z <- paste(x, z, sep = "\\")
		z <- txt.replace(z, "/", "\\")
	}
	z
}
dir.clear <- function(x, y) {
# -----------------------------------------------------------------
# Name		: dir.clear
# Author	: VKS
# Date		: 12/18/18
# Args		: x = a path such as "C:\\temp"
#		: y = a string such as "*.txt"
# Output	: rids <x> of files of type <y>
# -----------------------------------------------------------------
	cat("Ridding folder", x, "of", y, "files ...\n")
	z <- dir(x, y)
	if (length(x) > 0) file.kill(paste(x, z, sep = "\\"))
	invisible()
}
dir.ensure <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.ensure
# Author	: VKS
# Date		: 1/18/17,1/31/18,1/4/19
# Args		: x = a vector of full file paths
# Output	: Creates necessary folders so files can be copied to <x>
# -----------------------------------------------------------------
#
# SUBSET TO DIRECTORIES THAT DON'T EXIST
	x <- dirname(x)
	x <- x[!duplicated(x)]
	x <- x[!dir.exists(x)]
#
# BULK UP WITH INTERMEDIATE DIRECTORIES
	z <- x
	while (length(z) > 0) {
		z <- dirname(z)
		z <- z[!dir.exists(z)]
		x <- union(z, x)
	}
#
# MAKE DIRECTORIES
	if (length(x) > 0) dir.make(x)
#
# RETURN NOTHING
	invisible()
}
dir.kill <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.kill
# Author	: VKS
# Date		: 1/31/18,1/7/19
# Args		: x = a vector of full folder paths
# Output	: removes <x>
# -----------------------------------------------------------------
	w <- dir.exists(x)
	if (any(w)) unlink(x[w], recursive = T)
	invisible()
}
dir.make <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.make
# Author	: VKS
# Date		: 8/4/17
# Args		: x = a vector of full folder paths
# Output	: creates folders <x>
# -----------------------------------------------------------------
	for (z in x) dir.create(z)
	invisible()
}
dir.parameters <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.parameters
# Author	: VKS
# Date		: 12/15/17,12/22
# Args		: x = desired sub-folder
# Output	: returns full path to relevant parameters sub-folder
# -----------------------------------------------------------------
	paste(fcn.dir(), "New Model Concept\\General", x, sep = "\\")
}
dir.parent <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.parent
# Author	: VKS
# Date		: 12/4/17
# Args		: x = a string of full paths
# Output	: returns paths to the parent directory
# Notes		: as an intended side effect, "/" are replaced by "\\"
# -----------------------------------------------------------------
	z <- dirname(x)
	z <- ifelse(z == ".", "", z)
	z <- txt.replace(z, "/", "\\")
	z
}
dir.publications <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.publications
# Author	: VKS
# Date		: 12/12/18
# Args		: x = desired sub-folder
# Output	: desired output directory for relevant publication
# -----------------------------------------------------------------
	dir.parameters(paste("Publications", x, sep = "\\"))
}
dir.size <- function(x) {
# -----------------------------------------------------------------
# Name		: dir.size
# Author	: VKS
# Date		: 2/26/18,2/27
# Args		: x = a SINGLE path to a directory
# Output	: size of directory <x> in KB
# Notes		: even though warnings get generated when access to a file
#		:	is denied, file size still gets returned
# -----------------------------------------------------------------
	z <- dir.all.files(x, "*.*")
	if (length(z) == 0) {
		z <- 0
	} else {
		z <- file.size(z)
		z <- sum(z, na.rm = T)/2^10
	}
	z
}
dtw <- function(x, y) {
# -----------------------------------------------------------------
# Name		: dtw
# Author	: VKS
# Date		: 8/11/20
# Args		: x = a numeric vector
#		: y = a numeric vector
# Output	: Dynamic time-warped distance between <x> and <y>
# Notes		: Similar algorithm to txt.levenshtein
# -----------------------------------------------------------------
#
# INSTANTIATE RESULT
	n <- length(x)
	m <- length(y)
	z <- matrix(NA, n + 1, m + 1, F, list(c(0, x), c(0, y)))
	z[1, ] <- z[, 1] <- Inf
	z[1, 1] <- 0
#
# POPULATE RESULT
	for (i in 1:m + 1) {
		for (j in 1:n + 1) {
			z[j, i] <- min(z[j - 1, i], min(z[j, i - 1], z[j - 1, i - 1])) + abs(x[j - 1] - y[i - 1])
		}
	}
#
# RECOVER PATH
	w <- list(x = n, y = m)
	i <- m + 1
	j <- n + 1
	while (max(i, j) > 2) {
		if (z[j - 1, i - 1] < min(z[j - 1, i], z[j, i - 1])) {
			i <- i - 1
			j <- j - 1
		} else if (z[j - 1, i] < z[j, i - 1]) {
			j <- j - 1
		} else {
			i <- i - 1
		}
		w[["x"]] <- c(j - 1, w[["x"]])
		w[["y"]] <- c(i - 1, w[["y"]])
	}
	z <- mat.ex.matrix(w)
#
# RETURN RESULT
	z
}
dup.code <- function(x, y) {
# -----------------------------------------------------------------
# Name		: dup.code
# Author	: VKS
# Date		: 11/20/20
# Args		: x = string vector
#		: y = string vector of same length as <x>
# Output	: T/F depending on whether code is duplicated
# -----------------------------------------------------------------
	z <- list(A = x, B = y)
#
# EXPRESSIONS
	z <- lapply(z, function(z) tryCatch(parse(text = z), error = function(e){NULL}))
	halt <- all(!sapply(z, is.null))
#
# VARIABLE COUNT
	if (halt) {
		v <- lapply(z, all.vars)
		halt <- length(unique(sapply(v, length))) == 1
	}
#
# NUMBER OF NAMES
	if (halt) {
		z <- lapply(z, all.names)
		halt <- lapply(z, vec.count)
		halt <- length(unique(sapply(halt, length))) == 1
	}
#
# NAME COUNT
	if (halt) halt <- length(unique(sapply(z, length))) == 1
#
# SAME NON-VARIABLE NAME COUNT
	if (halt) {
		for (s in names(z)) {
			v[[s]] <- vec.count(z[[s]][is.element(z[[s]], v[[s]])])
			z[[s]] <- z[[s]][!is.element(z[[s]], names(v[[s]]))]
		}
		halt <- length(unique(sapply(z, length))) == 1
	}
#
# SAME NON-VARIABLE NAMES
	if (halt) halt <- all(z[[1]] == z[[2]])
#
# SAME VARIABLE COUNTS
	if (halt) {
		v <- lapply(v, sort)
		halt <- all(v[["A"]] == v[["B"]])
	}
	z <- halt
#
# RETURN RESULT
	z
}
email.exists <- function(x, y) {
# -----------------------------------------------------------------
# Name		: email.exists
# Author	: VKS
# Date		: 2/10/20,2/11,12/17
# Args		: x = report name
#		: y = date for which you want to send the report
# Output	: T/F depending on whether email already went out
# -----------------------------------------------------------------
	record.exists(x, y, "emails.txt")
}
email.kill <- function(x) {
# -----------------------------------------------------------------
# Name		: email.kill
# Author	: VKS
# Date		: 2/11/20,12/17
# Args		: x = report name
# Output	: deletes entry <x> in the email record. Returns nothing.
# -----------------------------------------------------------------
	record.kill(x, "emails.txt")
}
email.list <- function() {
# -----------------------------------------------------------------
# Name		: email.list
# Author	: VKS
# Date		: 2/11/20,12/17
# Args		: none
# Output	: named vector of emails and sent dates
# -----------------------------------------------------------------
	record.read("emails.txt")
}
email.record <- function(x, y) {
# -----------------------------------------------------------------
# Name		: email.record
# Author	: VKS
# Date		: 2/10/20,12/17
# Args		: x = report name
#		: y = date for which you sent the report
# Output	: updates the email record. Returns nothing.
# -----------------------------------------------------------------
	record.write(x, y, "emails.txt")
}
err.raise <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: err.raise
# Author	: VKS
# Date		: 12/19/17,1/3/18,8/27
# Args		: x = a vector
#		: y = T/F depending on whether output goes on many lines
#		: n = main line of error message
# Output	: error message
# -----------------------------------------------------------------
	cat(err.raise.txt(x, y, n), "\n")
	invisible()
}
err.raise.txt <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: err.raise.txt
# Author	: VKS
# Date		: 12/19/17,1/3/18,8/27
# Args		: x = a vector
#		: y = T/F depending on whether output goes on many lines
#		: n = main line of error message
# Output	: error message
# -----------------------------------------------------------------
	n <- paste0(n, ":")
	if (y) {
		z <- paste(c(n, paste0("\t", x)), collapse = "\n")
	} else {
		z <- paste0(n, "\n\t", paste(x, collapse = " "))
	}
	z <- paste0(z, "\n")
#
# RETURN RESULT
	z
}
event.read <- function(x) {
# -----------------------------------------------------------------
# Name		: event.read
# Author	: VKS
# Date		: 12/31/19,12/25/20
# Args		: x = path to a text file of dates in dd/mm/yyyy format
# Output	: data frame with events sorted and numbered
# -----------------------------------------------------------------
#
# READ FILE
	z <- readLines(x)
	z <- yyyymmdd.ex.txt(z, "/", "DMY")
#
# CREATE RESULT
	z <- z[order(z)]
	x <- seq_along(z)
	z <- data.frame(z, x, row.names = x, stringsAsFactors = F)
	dimnames(z)[[2]] <- c("Date", "EventNo")
#
# RETURN RESULT
	z
}
excise.zeroes <- function(x) {
# -----------------------------------------------------------------
# Name		: excise.zeroes
# Author	: VKS
# Date		: 9/19/16,12/15/17,1/24/19
# Args		: x = a vector/matrix/dataframe
# Output	: Coverts zeroes to NA
# -----------------------------------------------------------------
	fcn <- function(x) ifelse(!is.na(x) & abs(x) < 1e-6, NA, x)
	z <- fcn.mat.vec(fcn, x,, T)
	z
}
extract.AnnMn.sf <- function(x, y) {
# -----------------------------------------------------------------
# Name		: extract.AnnMn.sf
# Author	: VKS
# Date		: 3/9/17,3/24,4/7,6/20,12/19/18,1/17/20
# Args		: x = a 3D object.
#		: 	The first dimension has AnnMn/AnnSd/Sharp/HitRate
#		: 	The second dimension has bins Q1/Q2/Qna/Q3/Q4/Q5
#		: 	The third dimension is some kind of parameter
#		: y = a string which must be one of AnnMn/AnnSd/Sharp/HitRate
# Output	: Subsets to "AnnMn" and re-labels columns
# -----------------------------------------------------------------
	z <- x
	w <- dimnames(z)[[2]] != "uRet"
	z <- mat.ex.matrix(t(z[y, w, ]))
	z <- mat.last.to.first(z)
	z
}
extract.AnnMn.sf.wrapper <- function(x, y = "AnnMn") {
# -----------------------------------------------------------------
# Name		: extract.AnnMn.sf.wrapper
# Author	: VKS
# Date		: 11/21/17,12/19/18,1/25/19
# Args		: x = a list object, each element of which is a 3D object
#		: 	The first dimension has AnnMn/AnnSd/Sharp/HitRate
#		: 	The second dimension has bins Q1/Q2/Qna/Q3/Q4/Q5
#		: 	The third dimension is some kind of parameter
#		: y = a string which must be one of AnnMn/AnnSd/Sharp/HitRate
# Output	: Subsets to "AnnMn" and re-labels columns
# -----------------------------------------------------------------
	fcn <- function(x) extract.AnnMn.sf(x, y)
	if (dim(x[[1]])[3] == 1) z <- t(sapply(x, fcn)) else z <- mat.ex.matrix(lapply(x, fcn))
	z
}
factordump.rds <- function(x, y, n, w, h, u) {
# -----------------------------------------------------------------
# Name		: factordump.rds
# Author	: VKS
# Date		: 5/15/19,5/23
# Args		: x = variable name (e.g. "HerdingLSV")
#		: y = local folder (e.g. "C:\\temp\\mystuff")
#		: n = starting QTR
#		: w = ending QTR
#		: h = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
#		:	c) fldr - stock-flows folder
#		: u = output variable name
# Output	: Dumps variable <x> to folder <y> in standard text format
# -----------------------------------------------------------------
#
# CREATE FILES
	for (j in qtr.seq(n, w)) {
		z <- list()
		for (k in yyyymm.lag(yyyymm.ex.qtr(j), 2:0)) {
			cat(k, "")
			df <- sql.query.underlying(sql.HSIdmap(k), h$conn, F)
			is.dup <- duplicated(df$SecurityId)
			if (any(is.dup)) {
				df <- df[!is.dup, ]
				cat("Removing", sum(is.dup), "duplicated SecurityId at", k, "...\n")
			}
			df <- vec.named(df[, "HSecurityId"], df[, "SecurityId"])
			vbl <- fetch(x, yyyymm.lag(k, -1), 1, paste(h$fldr, "derived", sep = "\\"), h$classif)
			is.data <- !is.na(vbl) & is.element(dimnames(h$classif)[[1]], names(df))
			vbl <- vbl[is.data]
			df <- as.character(df[dimnames(h$classif)[[1]][is.data]])
			df <- data.frame(rep(yyyymm.to.day(k), length(vbl)), df, vbl)
			dimnames(df)[[2]] <- c("ReportDate", "HSecurityId", x)
			z[[k]] <- df
		}
		z <- Reduce(rbind, z)
		factordump.write(z, paste0(y, "\\", u, j, ".txt"))
		cat("\n")
	}
#
# RETURN NOTHING
	invisible()
}
factordump.write <- function(x, y) {
# -----------------------------------------------------------------
# Name		: factordump.write
# Author	: VKS
# Date		: 5/15/19
# Args		: x = a matrix/data-frame
#		: y = output path
# Output	: Dumps variable <x> to path <y> in standard text format
# -----------------------------------------------------------------
	x[, "ReportDate"] <- yyyymmdd.to.txt(x[, "ReportDate"])
	dir.ensure(y)
	write.table(x, y, sep = "\t", , row.names = F, col.names = T, quote = F)
	invisible()
}
farben <- function(x, y) {
# -----------------------------------------------------------------
# Name		: farben
# Author	: VKS
# Date		: 1/15/20,11/20,11/24
# Args		: x = number of colours needed
#		: y = T/F depending on whether fill or border is wanted
# Output	: vector of R colours
# -----------------------------------------------------------------
#
# READ IN COLOUR PALETTE
	h <- mat.read(parameters("classif-colours"))
#
# FILL OR BORDER
	if (!y) {
		v <- dimnames(h)[[1]]
		h <- map.rname(h, h$border)
		dimnames(h)[[1]] <- v
	}
	h <- h[, c("R", "G", "B")]
#
# COLOUR COMBINATIONS
	h <- mat.ex.matrix(t(h))
	if (x == 9) {
		z <- c("H", "B", "A", "D", "C", "T", "S", "R", "Q")
	} else if (x == "V") {
		z <- c("H", "D", "T", "S", "Q")
	} else if (x == 5) {
		z <- c("Q", "N", "H", "F", "M")
	} else if (x == 4) {
		z <- c("Q", "N", "B", "K")
	} else if (x == 3) {
		z <- c("Q", "N", "H")
	} else if (x == 2) {
		z <- c("Q", "H")
	} else if (x == 1) {
		z <- "Q"
	} else {
		stop("farben: Can't handle this!")
	}
#
# CONVERT TO HEXADECIMAL
	if (length(z) == 1) z <- list(One = h[, z]) else z <- h[, z]
	z <- lapply(z, function(x) paste(txt.right(paste0("0", as.hexmode(x)), 2), collapse = ""))
	z <- paste0("#", toupper(as.character(unlist(z))))
#
# RETURN RESULT
	z
}
fcn.all.canonical <- function() {
# -----------------------------------------------------------------
# Name		: fcn.all.canonical
# Author	: VKS
# Date		: 12/12/17,1/29/18,1/7/19,1/25
# Args		: none
# Output	: Checks all functions are in standard form
# -----------------------------------------------------------------
	x <- fcn.list()
	w <- sapply(vec.to.list(x), fcn.canonical)
	if (all(w)) cat("All functions are canonical ...\n")
	if (any(!w)) err.raise(x[!w], F, "The following functions are non-canonical")
	invisible()
}
fcn.all.roxygenize <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.all.roxygenize
# Author	: VKS
# Date		: 1/22/18,1/23,1/25,10/5,3/30/21
# Args		: x = path to output file
# Output	: roxygenizes all functions
# -----------------------------------------------------------------
#
# FUNCTION FAMILIES
	n <- fcn.list()
	n <- txt.parse(n, ".")
	n <- n[n[, 2] != "", 1]
	n <- vec.count(n)
	n <- names(n)[n > 1]
#
# ONE FUNCTION FOR EACH SPECIAL LIBRARY
	y <- vec.named("mat.read", "utils")
	y["stats"] <- "ret.outliers"
	y["RODBC"] <- "mk.1mPerfTrend"
	y["RDCOMClient"] <- "email"
	y["RCurl"] <- "ftp.dir"
	z <- NULL
	for (w in names(y)) z <- c(z, "", fcn.roxygenize(y[w], w, n))
#
# ALL REMAINING FUNCTIONS
	y <- setdiff(fcn.list(), y)
	for (w in y) z <- c(z, "", fcn.roxygenize(w, , n))
	cat(z, file = x, sep = "\n")
#
# RETURN NOTHING
	invisible()
}
fcn.all.sub <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.all.sub
# Author	: VKS
# Date		: 1/16/18
# Args		: x = a vector of function names
# Output	: a string vector of names of all sub-functions
# -----------------------------------------------------------------
	fcn.indirect(fcn.direct.sub, x)
}
fcn.all.super <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.all.super
# Author	: VKS
# Date		: 1/16/18
# Args		: x = a vector of function names
# Output	: names of all functions that depend on <x>
# -----------------------------------------------------------------
	fcn.indirect(fcn.direct.super, x)
}
fcn.args.actual <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.args.actual
# Author	: VKS
# Date		: 12/11/17
# Args		: x = a SINGLE function name
# Output	: list of actual arguments
# -----------------------------------------------------------------
	names(formals(x))
}
fcn.canonical <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.canonical
# Author	: VKS
# Date		: 12/7/17,12/21,12/22,1/10/18,1/12,1/29,2/5/20
# Args		: x = a SINGLE function name
# Output	: T/F depending on whether <x> is in standard form
# -----------------------------------------------------------------
	y <- fcn.to.comments(x)
	z <- fcn.comments.parse(y)
#
# NAME
	if (z$canonical) {
		if (z$name != x) {
			cat(x, "has a problem with NAME!\n")
			z$canonical <- F
		}
	}
#
# DATE
	if (z$canonical) {
		if (!ascending(fcn.dates.parse(z$date))) {
			cat(x, "has a problem with DATE!\n")
			z$canonical <- F
		}
	}
#
# CHECK NUMBER OF ARGUMENTS IS IN ORDER
	if (z$canonical) {
		actual.args <- fcn.args.actual(x)
		if (length(z$args) != length(actual.args)) {
			cat(x, "has a problem with NUMBER of COMMENTED ARGUMENTS!\n")
			z$canonical <- F
		}
	}
#
# CHECK ARGUMENTS ARE IN ORDER
	if (z$canonical) {
		if (any(z$args != actual.args)) {
			cat(x, "has a problem with COMMENTED ARGUMENTS NOT MATCHING ACTUAL!\n")
			z$canonical <- F
		}
	}
#
# CHECK ARGUMENTS ARE CANONICAL
	canon <- c("fcn", "x", "y", "n", "w", "h", "u")
	if (z$canonical) {
		if (length(z$args) < length(canon)) {
			n <- length(z$args)
			if (any(z$args != canon[1:n]) & any(z$args != canon[1:n + 1])) {
				cat(x, "has NON-CANONICAL ARGUMENTS!\n")
				z$canonical <- F
			}
		}
	}
#
# CHECK FUNCTION IS INDENTED PROPERLY
	if (z$canonical) {
		z <- fcn.indent.proper(x)
	} else z <- F
#
# RETURN RESULT
	z
}
fcn.clean <- function() {
# -----------------------------------------------------------------
# Name		: fcn.clean
# Author	: VKS
# Date		: 12/22/17,1/30/18,1/31,2/1,12/25/20
# Args		: none
# Output	: removes trailing spaces and tabs & indents properly
# -----------------------------------------------------------------
#
# READ IN FUNCTION
	z <- readLines(fcn.path())
#
# FIND COMMENTS, COMMENT DELIMITERS AND FIRST LINE OF EACH FUNCTION
	w.com <- fcn.indent.ignore(z, 0)
	w.del <- txt.has(z, paste("#", txt.space(65, "-")), T)
	w.beg <- txt.has(z, " <- function(", T) & c(w.del[-1], F)
#
# TRIM SPACES AND TABS FROM ACTUAL CODE
	if (any(!w.com)) z[!w.com] <- txt.trim(z[!w.com], c(" ", "\t"))
#
# INDENT PROPERLY
	i <- 1
	n <- length(z)
	while (i <= n) {
		if (w.beg[i]) {
			i <- i + 1
			phase <- 1
		} else if (phase == 1 & w.del[i]) {
			phase <- 2
			w <- 1
		} else if (phase == 2 & fcn.indent.else(toupper(z[i]), 1)) {
			w <- w - 1
			z[i] <- paste0(txt.space(w, "\t"), z[i])
			w <- w + 1
		} else if (phase == 2 & fcn.indent.decrease(toupper(z[i]), 1)) {
			w <- w - 1
			z[i] <- paste0(txt.space(w, "\t"), z[i])
		} else if (phase == 2 & fcn.indent.increase(toupper(z[i]), 0)) {
			z[i] <- paste0(txt.space(w, "\t"), z[i])
			w <- w + 1
		} else if (phase == 2 & !w.com[i]) {
			z[i] <- paste0(txt.space(w, "\t"), z[i])
		}
		i <- i + 1
	}
#
# WRITE OUT CLEAN CODE
	cat(z, file = fcn.path(), sep = "\n")
#
# RETURN NOTHING
	invisible()
}
fcn.comments.parse <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.comments.parse
# Author	: VKS
# Date		: 1/10/18,1/22,1/31
# Args		: x = comments section of a function
# Output	: extracts information from the comments
# -----------------------------------------------------------------
	z <- list(canonical = !is.null(x))
#
# NAME
	if (z$canonical) {
		if (txt.left(x[1], 10) != "# Name\t\t: ") {
			cat("Problem with NAME!\n")
			z$canonical <- F
		} else {
			z$name <- txt.right(x[1], nchar(x[1]) - 10)
			x <- x[-1]
		}
	}
#
# AUTHOR
	if (z$canonical) {
		if (txt.left(x[1], 11) != "# Author\t: ") {
			cat("Problem with AUTHOR!\n")
			z$canonical <- F
		} else {
			z$author <- txt.right(x[1], nchar(x[1]) - 11)
			x <- x[-1]
		}
	}
#
# DATE
	if (z$canonical) {
		if (txt.left(x[1], 10) != "# Date\t\t: ") {
			cat("Problem with DATE!\n")
			z$canonical <- F
		} else {
			z$date <- txt.right(x[1], nchar(x[1]) - 10)
			x <- x[-1]
			while (length(x) > 0 & txt.left(x[1], 5) == "#\t\t: ") {
				z$date <- paste0(z$date, txt.right(x[1], nchar(x[1]) - 5))
				x <- x[-1]
			}
		}
	}
#
# ARGUMENTS
	if (z$canonical) {
		if (txt.left(x[1], 10) != "# Args\t\t: ") {
			cat("Problem with ARGS!\n")
			z$canonical <- F
		} else {
			z$detl.args <- x[1]
			x <- x[-1]
			while (length(x) > 0 & any(txt.left(x[1], 5) == c("#\t\t: ", "#\t\t:\t"))) {
				z$detl.args <- c(z$detl.args, x[1])
				x <- x[-1]
			}
			z$detl.args <- fcn.extract.args(z$detl.args)
			if (length(z$detl.args) == 1 & z$detl.args[1] != "none") {
				z$args <- txt.parse(z$detl.args, " =")[1]
			} else if (length(z$detl.args) > 1) z$args <- txt.parse(z$detl.args, " =")[, 1]
		}
	}
#
# OUTPUT
	if (z$canonical) {
		if (txt.left(x[1], 11) != "# Output\t: ") {
			cat("Problem with OUTPUT!\n")
			z$canonical <- F
		} else {
			z$out <- x[1]
			x <- x[-1]
			while (length(x) > 0 & any(txt.left(x[1], 5) == c("#\t\t: ", "#\t\t:\t"))) {
				z$out <- c(z$out, x[1])
				x <- x[-1]
			}
			z$out <- fcn.extract.out(z$out)
		}
	}
#
# NOTES
	if (z$canonical & length(x) > 0) {
		if (txt.left(x[1], 11) == "# Notes\t\t: ") {
			x <- x[-1]
			while (length(x) > 0 & any(txt.left(x[1], 5) == c("#\t\t: ", "#\t\t:\t"))) x <- x[-1]
		}
	}
#
# EXAMPLE
	if (z$canonical & length(x) > 0) {
		if (txt.left(x[1], 12) == "# Example\t: ") {
			z$example <- txt.right(x[1], nchar(x[1]) - 12)
			x <- x[-1]
		}
	}
#
# IMPORT
	if (z$canonical & length(x) > 0) {
		if (txt.left(x[1], 11) == "# Import\t: ") {
			z$import <- txt.right(x[1], nchar(x[1]) - 11)
			x <- x[-1]
		}
	}
#
# OTHER
	if (z$canonical & length(x) > 0) {
		cat("Other bizarre problem!\n")
		z$canonical <- F
	}
#
# RETURN RESULT
	z
}
fcn.date <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.date
# Author	: VKS
# Date		: 12/7/17,1/10/18
# Args		: x = a SINGLE function name
# Output	: date of last modification
# -----------------------------------------------------------------
	max(fcn.dates.parse(fcn.comments.parse(fcn.to.comments(x))$date))
}
fcn.dates.parse <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.dates.parse
# Author	: VKS
# Date		: 1/10/18,1/11,12/19/18,12/25/20
# Args		: x = date item from fcn.comments.parse
# Output	: dates a function was modified
# -----------------------------------------------------------------
	z <- txt.parse(x, ",")
	if (length(z) == 1) z <- yyyymmdd.ex.txt(z)
	if (length(z) > 1) {
		z <- txt.parse(z, "/")[, 1:3]
		z[, 3] <- fix.gaps(as.numeric(z[, 3]))
		z[, 3] <- yyyy.ex.yy(z[, 3])
		z <- matrix(as.numeric(unlist(z)), dim(z)[1], dim(z)[2], F, dimnames(z))
		z <- as.character(colSums(t(z) * 100^c(1, 0, 2)))
	}
	z
}
fcn.dir <- function() {
# -----------------------------------------------------------------
# Name		: fcn.dir
# Author	: VKS
# Date		: 12/22/17,1/24/18,2/1,8/16/19,8/19,2/4/20,12/25
# Args		: none
# Output	: folder of function source file
# -----------------------------------------------------------------
	z <- "C:\\temp\\Automation"
	if (Sys.info()[["nodename"]] == "OpsServerDev") z <- "C:\\Users\\vik\\Documents"
	z <- readLines(paste0(z, "\\root.txt"))
	z
}
fcn.direct.sub <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.direct.sub
# Author	: VKS
# Date		: 12/6/17,1/7/19,1/25
# Args		: x = a SINGLE function name
# Output	: a string vector of names of all direct sub-functions
# -----------------------------------------------------------------
#
# FUNCTION CODE
	x <- fcn.to.txt(x)
#
# FIND ALL SUB-FUNCTIONS
	z <- fcn.list()
	fcn <- function(z) {txt.has(x, paste0(z, "("), T)}
	w <- sapply(vec.to.list(z), fcn)
	if (any(w)) z <- z[w] else z <- NULL
#
# RETURN RESULT
	z
}
fcn.direct.super <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.direct.super
# Author	: VKS
# Date		: 12/6/17,1/7/19,1/25
# Args		: x = a SINGLE function name
# Output	: names of all functions that directly depend on <x>
# -----------------------------------------------------------------
	fcn.has(paste0(x, "("))
}
fcn.expressions.count <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.expressions.count
# Author	: VKS
# Date		: 1/26/18
# Args		: x = a SINGLE function name
# Output	: number of expressions
# -----------------------------------------------------------------
	z <- fcn.lines.code(x, F)
	z <- parse(text = z)
	z <- length(z)
	z
}
fcn.extract.args <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.extract.args
# Author	: VKS
# Date		: 12/11/17,1/22/18,1/30,1/31
# Args		: x = string vector representing argument section of comments
# Output	: vector of arguments with explanations
# -----------------------------------------------------------------
	n <- length(x)
#
# STUFF ON THE RIGHT OF THE COLON
	x <- txt.right(x, nchar(x) - ifelse(1:n == 1, 10, 5))
#
# COLLAPSE MULTI-LINE ARGUMENTS
	if (n > 1) {
		w <- txt.has(x, "=", T)
		while (any(w[-n] & !w[-1])) {
			i <- 2:n - 1
			i <- i[w[-n] & !w[-1]][1]
			j <- i:n + 1
			j <- j[c(w, T)[j]][1] - 1
			x[i] <- paste(txt.trim(x[i:j], "\t"), collapse = " ")
			while (j > i) {
				x <- x[-j]
				w <- w[-j]
				j <- j - 1
				n <- n - 1
			}
		}
	}
	z <- x
#
# RETURN RESULT
	z
}
fcn.extract.out <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.extract.out
# Author	: VKS
# Date		: 1/22/18
# Args		: x = string vector representing output section of comments
# Output	: extracts output
# -----------------------------------------------------------------
	n <- length(x)
	z <- txt.right(x, nchar(x) - ifelse(1:n == 1, 11, 5))
	z <- paste(z, collapse = " ")
	z
}
fcn.has <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.has
# Author	: VKS
# Date		: 1/23/19,1/25
# Args		: x = substring to be searched for
# Output	: Checks all functions are in standard form
# -----------------------------------------------------------------
	fcn <- function(y) txt.has(fcn.to.txt(y, F), x, T)
	z <- fcn.list()
	z <- z[sapply(vec.to.list(z), fcn)]
	z
}
fcn.indent.decrease <- function(x, y) {
# -----------------------------------------------------------------
# Name		: fcn.indent.decrease
# Author	: VKS
# Date		: 1/29/18
# Args		: x = a line of code in a function
#		: y = number of tabs
# Output	: T/F depending on whether indent should be decreased
# -----------------------------------------------------------------
	txt.left(x, y) == paste0(txt.space(y - 1, "\t"), "}")
}
fcn.indent.else <- function(x, y) {
# -----------------------------------------------------------------
# Name		: fcn.indent.else
# Author	: VKS
# Date		: 1/29/18
# Args		: x = a line of code in a function
#		: y = number of tabs
# Output	: T/F depending on whether line has an else statement
# -----------------------------------------------------------------
	h <- "} ELSE "
	z <- any(txt.left(x, nchar(h) + y - 1) == paste0(txt.space(y - 1, "\t"), h))
	z <- z & txt.right(x, 1) == "{"
	z
}
fcn.indent.ignore <- function(x, y) {
# -----------------------------------------------------------------
# Name		: fcn.indent.ignore
# Author	: VKS
# Date		: 1/29/18
# Args		: x = a line of code in a function
#		: y = number of tabs
# Output	: T/F depending on whether line should be ignored
# -----------------------------------------------------------------
	txt.left(txt.trim.left(x, "\t"), 1) == "#"
}
fcn.indent.increase <- function(x, y) {
# -----------------------------------------------------------------
# Name		: fcn.indent.increase
# Author	: VKS
# Date		: 1/29/18,1/7/19
# Args		: x = a line of code in a function
#		: y = number of tabs
# Output	: T/F depending on whether indent should be increased
# -----------------------------------------------------------------
	h <- c("FOR (", "WHILE (", "IF (")
	z <- any(txt.left(x, nchar(h) + y) == paste0(txt.space(y, "\t"), h))
	z <- z | txt.has(x, " <- FUNCTION(", T)
	z <- z & txt.right(x, 1) == "{"
	z
}
fcn.indent.proper <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.indent.proper
# Author	: VKS
# Date		: 1/29/18,1/30,4/25/19
# Args		: x = a SINGLE function name
# Output	: T/F depending on whether the function is indented properly
# -----------------------------------------------------------------
	y <- toupper(fcn.lines.code(x, T))
	n <- c(LETTERS, 1:9)
	w <- 1
	i <- 1
	z <- T
	while (i < 1 + length(y) & z) {
		if (fcn.indent.decrease(y[i], w) & !fcn.indent.else(y[i], w)) {
			w <- w - 1 # DECREASE INDENT
		} else if (fcn.indent.increase(y[i], w)) {
			w <- w + 1 # INCREASE INDENT
		} else if (!fcn.indent.ignore(y[i], w) & !fcn.indent.else(y[i], w)) {
			z <- nchar(y[i]) > nchar(txt.space(w, "\t"))
			if (z) z <- is.element(substring(y[i], w + 1, w + 1), n)
			if (!z) cat(x, ":", y[i], "\n")
		}
		i <- i + 1
	}
	z
}
fcn.indirect <- function(fcn, x) {
# -----------------------------------------------------------------
# Name		: fcn.indirect
# Author	: VKS
# Date		: 1/16/18
# Args		: fcn = a function to apply
#		: x = vector of function names
# Output	: applies <fcn> recursively
# -----------------------------------------------------------------
	z <- NULL
	while (length(x) > 0) {
		y <- NULL
		for (j in x) y <- union(y, fcn(j))
		y <- setdiff(y, x)
		z <- union(z, y)
		x <- y
	}
	z
}
fcn.lines.code <- function(x, y) {
# -----------------------------------------------------------------
# Name		: fcn.lines.code
# Author	: VKS
# Date		: 1/26/18,12/25/20
# Args		: x = a SINGLE function name
#		: y = T/F depending on whether internal comments count
# Output	: lines of actual code
# -----------------------------------------------------------------
	z <- length(fcn.to.comments(x))
	x <- fcn.to.txt(x, T)
	x <- txt.parse(x, "\n")
	z <- x[seq(z + 4, length(x) - 1)]
	if (!y) z <- z[txt.left(txt.trim.left(z, "\t"), 1) != "#"]
	z
}
fcn.lines.count <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: fcn.lines.count
# Author	: VKS
# Date		: 12/19/17,1/26/18
# Args		: x = a SINGLE function name
#		: y = T/F depending on whether internal comments count
# Output	: number of lines of code
# -----------------------------------------------------------------
	length(fcn.lines.code(x, y))
}
fcn.list <- function(x = "*") {
# -----------------------------------------------------------------
# Name		: fcn.list
# Author	: VKS
# Date		: 1/9/17,12/8,1/24/18,1/26,1/29
# Args		: x = pattern you want to see in returned objects
# Output	: Returns the names of objects that are or are not functions
# -----------------------------------------------------------------
	w <- globalenv()
	while (!is.element("fcn.list", ls(envir = w))) w <- parent.env(w)
	z <- ls(envir = w, all.names = T, pattern = x)
	w <- is.element(z, as.character(lsf.str(envir = w, all.names = T)))
	z <- z[w]
	z
}
fcn.lite <- function() {
# -----------------------------------------------------------------
# Name		: fcn.lite
# Author	: VKS
# Date		: 12/1/22
# Args		: none
# Output	: functions in alphabetical order ex RODBC/RDCOMClient
# -----------------------------------------------------------------
#
# SUBSET TO NEEDED FUNCTIONS
	x <- fcn.list()
	x <- setdiff(x, fcn.all.super("COMCreate"))
	x <- setdiff(x, fcn.all.super("odbcDriverConnect"))
#
# CREATE OUTPUT
	x <- vec.to.list(x, T)
	fcn <- function(x) paste(x, "<-", fcn.to.txt(x, T, F))
	x <- sapply(x, fcn)
#
# WRITE OUTPUT
	y <- fcn.path()
	y <- paste0(txt.left(y, nchar(y) - nchar(".r")), "-lite.r")
	cat(x, file = y, sep = "\n")
#
# RETURN NOTHING
	invisible()
}
fcn.mat.col <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: fcn.mat.col
# Author	: VKS
# Date		: 12/19/17
# Args		: fcn = function mapping two vectors to a single value
#		: x = a vector/matrix/dataframe
#		: y = either missing or a numeric isomekic vector
#		: n = T/F depending on whether inputs should be ranked
# Output	: applies <fcn> to the columns of <x> pairwise
# -----------------------------------------------------------------
	if (missing(y)) {
		z <- matrix(NA, dim(x)[2], dim(x)[2], F, list(dimnames(x)[[2]], dimnames(x)[[2]]))
		for (i in 1:dim(x)[2]) for(j in 1:dim(x)[2]) z[i, j] <- fcn.num.nonNA(fcn, x[, i], x[, j], n)
	} else if (is.null(dim(x))) {
		z <- fcn.num.nonNA(fcn, x, y, n)
	} else {
		z <- rep(NA, dim(x)[2])
		for (i in 1:dim(x)[2]) z[i] <- fcn.num.nonNA(fcn, x[, i], y, n)
	}
	z
}
fcn.mat.num <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: fcn.mat.num
# Author	: VKS
# Date		: 12/15/17,2/15/18,3/5,1/24/19
# Args		: fcn = function mapping vector(s) to a single value
#		: x = a vector/matrix/dataframe
#		: y = a number/vector or matrix/dataframe with the same dimensions as <x>
#		: n = T/F depending on whether you want <fcn> applied to columns or rows
# Output	: applies <fcn> to <x> if a vector or the columns/rows of <x> otherwise
# -----------------------------------------------------------------
	if (is.null(dim(x)) & missing(y)) {
		z <- fcn(x)
	} else if (is.null(dim(x)) & !missing(y)) {
		z <- fcn(x, y)
	} else if (missing(y)) {
		z <- apply(x, as.numeric(n) + 1, fcn)
	} else if (is.null(dim(y))) {
		z <- apply(x, as.numeric(n) + 1, fcn, y)
	} else {
		w <- dim(x)[2 - as.numeric(n)]
		fcn.loc <- function(x) fcn(x[1:w], x[1:w + w])
		if (n) x <- rbind(x, y) else x <- cbind(x, y)
		z <- apply(x, as.numeric(n) + 1, fcn.loc)
	}
#
# RETURN RESULT
	z
}
fcn.mat.vec <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: fcn.mat.vec
# Author	: VKS
# Date		: 12/15/17,1/25/19
# Args		: fcn = function mapping vector(s) to an isomekic vector
#		: x = a vector/matrix/dataframe
#		: y = a number/vector or matrix/dataframe with the same dimensions as <x>
#		: n = T/F depending on whether you want <fcn> applied to columns or rows
# Output	: applies <fcn> to <x> if a vector or the columns/rows of <x> otherwise
# -----------------------------------------------------------------
	if (is.null(dim(x)) & missing(y)) {
		z <- fcn(x)
	} else if (is.null(dim(x)) & !missing(y)) {
		z <- fcn(x, y)
	} else if (n & missing(y)) {
		z <- sapply(mat.ex.matrix(x), fcn)
	} else if (!n & missing(y)) {
		z <- t(sapply(mat.ex.matrix(t(x)), fcn))
	} else if (n & is.null(dim(y))) {
		z <- sapply(mat.ex.matrix(x), fcn, y)
	} else if (!n & is.null(dim(y))) {
		z <- t(sapply(mat.ex.matrix(t(x)), fcn, y))
	} else if (n) {
		w <- dim(x)[1]
		fcn.loc <- function(x) fcn(x[1:w], x[1:w + w])
		y <- rbind(x, y)
		z <- sapply(mat.ex.matrix(y), fcn.loc)
	} else {
		w <- dim(x)[2]
		fcn.loc <- function(x) fcn(x[1:w], x[1:w + w])
		y <- cbind(x, y)
		z <- t(sapply(mat.ex.matrix(t(y)), fcn.loc))
	}
#
# FIX COLUMN NAMES
	if (!is.null(dim(x))) dimnames(z) <- dimnames(x)
#
# RETURN RESULT
	z
}
fcn.nonNA <- function(fcn, x) {
# -----------------------------------------------------------------
# Name		: fcn.nonNA
# Author	: VKS
# Date		: 12/15/17,12/22
# Args		: fcn = a function that maps a vector to a vector
#		: x = a vector
# Output	: applies <fcn> to the non-NA values of <x>
# -----------------------------------------------------------------
	w <- !is.na(x)
	z <- rep(NA, length(x))
	if (any(w)) z[w] <- fcn(x[w])
	z
}
fcn.num.nonNA <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: fcn.num.nonNA
# Author	: VKS
# Date		: 12/18/17
# Args		: fcn = a function that maps a vector to a number
#		: x = a vector
#		: y = either missing or an isomekic vector
#		: n = T/F depending on whether inputs should be ranked
# Output	: applies <fcn> to the non-NA values of <x> and <y>
# -----------------------------------------------------------------
	if (missing(y)) w <- !is.na(x) else w <- !is.na(x) & !is.na(y)
	if (all(!w)) {
		z <- NA
	} else if (missing(y) & !n) {
		z <- fcn(x[w])
	} else if (missing(y) & n) {
		z <- fcn(rank(x[w]))
	} else if (!n) {
		z <- fcn(x[w], y[w])
	} else if (n) {
		z <- fcn(rank(x[w]), rank(y[w]))
	}
	z
}
fcn.order <- function() {
# -----------------------------------------------------------------
# Name		: fcn.order
# Author	: VKS
# Date		: 12/26/17,1/23/19,1/25,12/21/20
# Args		: none
# Output	: functions in alphabetical order
# -----------------------------------------------------------------
	x <- vec.to.list(fcn.list(), T)
	fcn <- function(x) paste(x, "<-", fcn.to.txt(x, T, F))
	x <- sapply(x, fcn)
	cat(x, file = fcn.path(), sep = "\n")
	invisible()
}
fcn.path <- function() {
# -----------------------------------------------------------------
# Name		: fcn.path
# Author	: VKS
# Date		: 12/22/17
# Args		: none
# Output	: path to function source file
# -----------------------------------------------------------------
	paste(fcn.dir(), "functionsVKS.r", sep = "\\")
}
fcn.roxygenize <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: fcn.roxygenize
# Author	: VKS
# Date		: 1/22/18,1/23,1/24,1/25,2/16
# Args		: x = function name
#		: y = library to import
#		: n = vector of function families
# Output	: roxygenized function format
# Notes		: R CMD Check tries all the examples. Never give example
#		:	for fcn.order. This will overwrite source code!
# -----------------------------------------------------------------
#
# COMMENTS
	w <- fcn.to.comments(x)
	w <- txt.replace(w, "\\", "\\\\")
	w <- txt.replace(w, "%", "\\%")
	w <- txt.replace(w, "@", "@@")
	w <- fcn.comments.parse(w)
#
# BEGIN OUTPUT
	z <- c(w$name, "", w$out)
	if (any(names(w) == "args")) z <- c(z, paste("@param", w$detl.args))
	z <- c(z, paste("@keywords", w$name), "@export")
#
# FAMILY
	if (!missing(n)) {
		if (any(x == n) | any(txt.left(x, nchar(n) + 1) == paste0(n, "."))) {
			z <- c(z, paste("@family", txt.parse(x, ".")[1]))
		}
	}
#
# IMPORTS
	if (!missing(y)) {
		z <- c(z, paste("@import", y))
	} else if (any(names(w) == "import")) z <- c(z, w$import)
#
# EXAMPLES
	if (any(names(w) == "example")) z <- c(z, "@examples", w$example)
	z <- c(paste("#'", z), "")
#
# CODE SECTION OF OUTPUT
	x <- fcn.to.txt(x, F, T)
	x[1] <- paste(w$name, "<-", x[1])
	z <- c(z, x)
#
# RETURN RESULT
	z
}
fcn.sho <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.sho
# Author	: VKS
# Date		: 12/6/17
# Args		: x = a SINGLE function name
# Output	: cats <x> to the screen
# -----------------------------------------------------------------
	x <- fcn.to.txt(x, T)
	cat(x, "\n")
	invisible()
}
fcn.simple <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.simple
# Author	: VKS
# Date		: 1/26/18
# Args		: x = a SINGLE function name
# Output	: T/F depending on whether <x> has multi-line expressions
# -----------------------------------------------------------------
	fcn.lines.count(x, F) == fcn.expressions.count(x)
}
fcn.to.comments <- function(x) {
# -----------------------------------------------------------------
# Name		: fcn.to.comments
# Author	: VKS
# Date		: 12/7/17,12/22
# Args		: x = a SINGLE function name
# Output	: returns the comment section
# -----------------------------------------------------------------
	y <- fcn.to.txt(x, T, T)
#
# TRAILING CHARACTERS
	z <- all(!is.element(txt.right(y, 1), c(" ", "\t")))
	if (!z) cat(x, "has lines with trailing whitespace!\n")
#
# LEADING CHARACTERS ON THE FIRST LINE
	if (z & txt.left(y[1], 9) != "function(") {
		cat(x, "has a first line with non-canonical leading characters!\n")
		z <- F
	}
#
# OTHER LEADING CHARACTERS
	if (z & any(!is.element(txt.left(y[-1], 1), c("#", "\t", "}")))) {
		cat(x, "has lines with non-canonical leading characters!\n")
		z <- F
	}
#
# PRECISELY TWO COMMENT DELIMITERS
	comment.delimiter <- paste("#", txt.space(65, "-"))
	w <- y == comment.delimiter
	if (z & sum(w) != 2) {
		cat(x, "does not have precisely two comment delimiters!\n")
		z <- F
	}
#
# BEGINNING COMMENT DELIMITER
	w <- seq(1, length(y))[w]
	if (z & w[1] != 2) {
		cat(x, "does not have a proper beginning comment delimiter!\n")
		z <- F
	}
#
# ENDING COMMENT DELIMITER
	if (z & w[2] - w[1] < 5) {
		cat(x, "has an ending too close to the beginning comment delimiter!\n")
		z <- F
	}
#
# LAST LINE
	if (z & length(y) - w[2] > 2) {
		z <- is.element(y[length(y) - 1], c("\tz", "\tinvisible()"))
		if (!z) cat(x, "returns a non-canonical variable!\n")
	}
#
# RETURN RESULT
	if (z) z <- y[seq(w[1] + 1, w[2] - 1)] else z <- NULL
	z
}
fcn.to.txt <- function(x, y = F, n = F) {
# -----------------------------------------------------------------
# Name		: fcn.to.txt
# Author	: VKS
# Date		: 12/6/17,12/21
# Args		: x = a SINGLE function name
#		: y = T/F vbl controlling whether comments are returned
#		: n = T/F vbl controlling whether output is a string vector
# Output	: represents <x> as a string or string vector
# -----------------------------------------------------------------
	x <- get(x)
	if (y) z <- deparse(x, control = "useSource") else z <- deparse(x)
	if (!n) z <- paste(z, collapse = "\n")
	z
}
fcn.vec.grp <- function(fcn, x, y) {
# -----------------------------------------------------------------
# Name		: fcn.vec.grp
# Author	: VKS
# Date		: 12/19/18
# Args		: fcn = function to be applied within groups
#		: x = a vector/matrix/dataframe
#		: y = a vector of groups (e.g. GSec)
# Output	: applies <fcn> to <x> within groups <y>
# -----------------------------------------------------------------
#
# SPLIT BY GROUP
	x <- split(x, y)
#
# APPLY FUNCTION
	z <- lapply(x, fcn)
#
# RECONSTITUTE
	z <- unsplit(z, y)
#
# RETURN RESULT
	z
}
fcn.vec.num <- function(fcn, x, y) {
# -----------------------------------------------------------------
# Name		: fcn.vec.num
# Author	: VKS
# Date		: 12/15/17
# Args		: fcn = function mapping elements to elements
#		: x = an element or vector
#		: y = an element or isomekic vector
# Output	: applies <fcn> to <x>
# -----------------------------------------------------------------
	n <- length(x)
	if (n == 1 & missing(y)) {
		z <- fcn(x)
	} else if (n == 1 & !missing(y)) {
		z <- fcn(x, y)
	} else if (n > 1 & missing(y)) {
		z <- rep(NA, n)
		for (i in 1:n) z[i] <- fcn(x[i])
	} else if (n > 1 & length(y) == 1) {
		z <- rep(NA, n)
		for (i in 1:n) z[i] <- fcn(x[i], y)
	} else {
		z <- rep(NA, n)
		for (i in 1:n) z[i] <- fcn(x[i], y[i])
	}
	z
}
fetch <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: fetch
# Author	: VKS
# Date		: 10/26/16,1/23/18,5/15/19
# Args		: x = either a single variable or a vector of variable names
#		: y = the YYYYMM or YYYYMMDD for which you want data
#		: n = number of daily/monthly trailing periods
#		: w = R-object folder
#		: h = classif file
# Output	: fetches <x> for the trailing <n> periods ending at <y>
# -----------------------------------------------------------------
	daily <- nchar(y) == 8
#
# PARAMETERS
	if (daily) {
		yyyy <- yyyymmdd.to.yyyymm(y)
		mm <- txt.right(y, 2)
	} else {
		yyyy <- yyyymm.to.yyyy(y)
		mm <- as.numeric(txt.right(y, 2))
	}
#
# GET DATA
	if (n > 1 & length(x) > 1) {
		stop("Can't handle this!\n")
	} else if (n > 1) {
		z <- paste0(w, "\\", x, ".", yyyy, ".r")
		lCol <- paste(x, mm, sep = ".")
		z <- readRDS(z)
		m <- 1:dim(z)[2]
		m <- m[dimnames(z)[[2]] == lCol]
		dimnames(z)[[2]] <- paste(dimnames(z)[[2]], yyyy, sep = ".")
		while (m < n) {
			if (daily) yyyy <- yyyymm.lag(yyyy, 1) else yyyy <- yyyy - 1
			df <- paste0(w, "\\", x, ".", yyyy, ".r")
			df <- readRDS(df)
			dimnames(df)[[2]] <- paste(dimnames(df)[[2]], yyyy, sep = ".")
			z <- data.frame(df, z)
			m <- m + dim(df)[2]
		}
		z <- z[, seq(m - n + 1, m)]
	} else if (length(x) > 1) {
		z <- matrix(NA, dim(h)[1], length(x), F, list(dimnames(h)[[1]], x))
		z <- mat.ex.matrix(z)
		for (i in dimnames(z)[[2]]) {
			df <- paste0(w, "\\", i, ".", yyyy, ".r")
			lCol <- paste(i, mm, sep = ".")
			if (file.exists(df)) {
				z[, i] <- readRDS(df)[, lCol]
			} else {
				cat("Warning:", df, "does not exist. Proceeding regardless ...\n")
			}
		}
	} else {
		z <- paste0(w, "\\", x, ".", yyyy, ".r")
		lCol <- paste(x, mm, sep = ".")
		if (file.exists(z)) {
			z <- readRDS(z)[, lCol]
		} else {
			cat("Warning:", z, "does not exist. Proceeding regardless ...\n")
			z <- rep(NA, dim(h)[1])
		}
	}
#
# RETURN RESULT
	z
}
file.bkp <- function(x, y) {
# -----------------------------------------------------------------
# Name		: file.bkp
# Author	: VKS
# Date		: 11/14/16,1/18/17
# Args		: x = a string of full paths
#		: y = an isomekic string of full paths
# Output	: Copies <x> to <y>
# -----------------------------------------------------------------
#
# EXISTENCE CHECK
	w <- file.exists(x)
#
# NON-EXISTENCE ALERT
	if (any(!w)) err.raise(x[!w], T, "Warning: The following files to be copied do not exist")
#
# PERFORM COPY
	if (any(w)) {
		x <- x[w]
		y <- y[w]
		file.kill(y)
		dir.ensure(y)
		file.copy(x, y)
	}
#
# RETURN NOTHING
	invisible()
}
file.break <- function(x) {
# -----------------------------------------------------------------
# Name		: file.break
# Author	: VKS
# Date		: 2/5/18
# Args		: x = path to a file
# Output	: breaks up the file into 1GB chunks and rewrites to same
#		:	directory with a "-001", "-002", etc extension
# -----------------------------------------------------------------
	y <- c(txt.left(x, nchar(x) - 4), txt.right(x, 4))
#
# DETERMINE NUMBER OF DIGITS IN OUTPUT-FILE EXTENSION
	m <- ceiling(log(2 * file.size(x)/2^30, base = 10))
#
# FIGURE OUT ROUGHLY HOW MANY LINES TO READ AT ONCE
	w <- 1000000
	n <- scan(file = x, what = "", skip = 0, sep = "\n", quiet = T, nlines = w)
	n <- as.numeric(object.size(n))/2^30
	n <- round(w/n)
#
# CREATE OUTPUT
	i <- 1
	z <- scan(file = x, what = "", skip = (i - 1) * n, sep = "\n", quiet = T, nlines = n)
	while (length(z) == n) {
		cat(z, file = paste0(y[1], "-", txt.right(10^m + i, m), y[2]), sep = "\n")
		i <- i + 1
		z <- scan(file = x, what = "", skip = (i - 1) * n, sep = "\n", quiet = T, nlines = n)
	}
	cat(z, file = paste0(y[1], "-", txt.right(10^m + i, m), y[2]), sep = "\n")
#
# RETURN NOTHING
	invisible()
}
file.date <- function(x) {
# -----------------------------------------------------------------
# Name		: file.date
# Author	: VKS
# Date		: 1/13/17
# Args		: x = a vector of full file paths
# Output	: Returns the last modified date in yyyymmdd format
# -----------------------------------------------------------------
	z <- file.mtime(x)
	z <- day.ex.date(z)
	z
}
file.kill <- function(x) {
# -----------------------------------------------------------------
# Name		: file.kill
# Author	: VKS
# Date		: 11/14/16
# Args		: x = a string of full paths
# Output	: Deletes designated files
# -----------------------------------------------------------------
	unlink(x)
	invisible()
}
file.mtime.to.time <- function(x) {
# -----------------------------------------------------------------
# Name		: file.mtime.to.time
# Author	: VKS
# Date		: 1/13/17
# Args		: x = a vector of dates
# Output	: Converts to HHMMSS times
# -----------------------------------------------------------------
	format(x, "%H%M%S")
}
file.time <- function(x) {
# -----------------------------------------------------------------
# Name		: file.time
# Author	: VKS
# Date		: 1/13/17
# Args		: x = a vector of full file paths
# Output	: Returns the last modified date in yyyymmdd format
# -----------------------------------------------------------------
	z <- file.mtime(x)
	z <- file.mtime.to.time(z)
	z
}
file.to.last <- function(x) {
# -----------------------------------------------------------------
# Name		: file.to.last
# Author	: VKS
# Date		: 10/30/17,12/15,12/19,4/25/18
# Args		: x = csv file containing the predictors
# Output	: the last YYYYMMDD or the last day of the YYYYMM for which we have data
# -----------------------------------------------------------------
	z <- mat.read(x, ",")
	z <- mat.to.last.Idx(z)
	if (nchar(z) == 6) z <- yyyymm.to.day(z)
	z
}
find.data <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: find.data
# Author	: VKS
# Date		: 10/12/17,12/19,1/30/19
# Args		: x = a logical vector
#		: y = T/F depending on whether the position of the first/last true value of x is desired
# Output	: returns the position of the first/last true value of x
# -----------------------------------------------------------------
	z <- seq_along(x)
	if (!y) {
		x <- rev(x)
		z <- rev(z)
	}
	z <- z[x & !duplicated(x)]
	z
}
find.gaps <- function(x) {
# -----------------------------------------------------------------
# Name		: find.gaps
# Author	: VKS
# Date		: 10/12/17
# Args		: x = a logical vector
# Output	: returns the position of the first and last true value of x
#		:	together with the first positions of all gaps
# -----------------------------------------------------------------
	m <- find.data(x, T)
	n <- find.data(x, F)
	z <- list(pos = NULL, size = NULL)
	while (n - m + 1 > sum(x[m:n])) {
		m <- m + find.data((!x)[m:n], T) - 1
		gap.size <- find.data(x[m:n], T) - 1
		z[["pos"]] <- c(z[["pos"]], m)
		z[["size"]] <- c(z[["size"]], gap.size)
		m <- m + gap.size
	}
	z <- vec.named(z[["size"]], z[["pos"]])
	z
}
find.whitespace.trail <- function(x) {
# -----------------------------------------------------------------
# Name		: find.whitespace.trail
# Author	: VKS
# Date		: 10/25/18
# Args		: x = the name of a function
# Output	: cats 2 lines above and below lines with trailing white space
# -----------------------------------------------------------------
#
# READ IN FUNCTION
	z <- deparse(get(x), control = "useSource")
#
# FIND TRAILING WHITESPACE
	n <- seq(1, length(z))[is.element(txt.right(z, 1), c(" ", "\t"))]
#
# FIND LINES BEFORE AND AFTER
	n <- c(n, n + 1, n + 2, n - 1, n - 2)
	n <- n[!duplicated(n)]
	n <- n[order(n)]
	n <- vec.min(n, length(z))
	n <- vec.max(n, 1)
#
# SUBSET AND RETURN THOSE LINES
	z <- z[n]
	vec.cat(z)
#
# RETURN NOTHING
	invisible()
}
fix.gaps <- function(x) {
# -----------------------------------------------------------------
# Name		: fix.gaps
# Author	: VKS
# Date		: 12/14/17
# Args		: x = a vector
# Output	: replaces NA's by previous value
# -----------------------------------------------------------------
	if (is.na(x[1])) stop("Problem")
	z <- x
	n <- length(z)
	w <- is.na(z[-1])
	while (any(w)) {
		z[-1] <- ifelse(w, z[-n], z[-1])
		w <- is.na(z[-1])
	}
	z
}
flowdate.diff <- function(x, y) {
# -----------------------------------------------------------------
# Name		: flowdate.diff
# Author	: VKS
# Date		: 6/13/19
# Args		: x = a vector of flow dates in YYYYMMDD format
#		: y = an isomekic vector of flow dates in YYYYMMDD format
# Output	: returns <x - y> in terms of flowdates
# -----------------------------------------------------------------
	obj.diff(flowdate.to.int, x, y)
}
flowdate.ex.AllocMo <- function(x, y = 23) {
# -----------------------------------------------------------------
# Name		: flowdate.ex.AllocMo
# Author	: VKS
# Date		: 2/21/20
# Args		: x = a single yyyymm
#		: y = calendar day in the next month when allocations are known (usually the 23rd)
# Output	: Returns the flowdates corresponding to <x> (inverse of yyyymmdd.to.AllocMo)
# -----------------------------------------------------------------
#
# FLOW DATES THAT FALL IN THE NEXT MONTH
	x <- yyyymm.lag(x, -1)
	z <- flowdate.ex.yyyymm(x, F)
	z <- z[as.numeric(txt.right(z, 2)) >= y]
#
# FLOW DATES THAT FALL IN THE MONTH AFTER THAT
	x <- yyyymm.lag(x, -1)
	z <- c(z, flowdate.ex.yyyymm(x, F))
	z <- z[as.numeric(txt.right(z, 2)) < y | yyyymmdd.to.yyyymm(z) < x]
#
# RETURN RESULT
	z
}
flowdate.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: flowdate.ex.int
# Author	: VKS
# Date		: 4/11/19
# Args		: x = an integer or vector of integers
# Output	: the <x>th daily flow-publication date after Friday, December 29, 2017
# -----------------------------------------------------------------
#
# BUILD MAP
	z <- c(0, x)
	z <- y <- seq(min(z), max(z))
	w <- !flowdate.exists(yyyymmdd.ex.int(z))
	while (any(w)) {
		if (any(w & z <= 0)) {
			for (h in sort(z[w & z <= 0], decreasing = T)) {
				z <- ifelse(z <= h, z - 1, z)
			}
		}
		if (any(w & z > 0)) {
			for (h in z[w & z > 0]) {
				z <- ifelse(z >= h, z + 1, z)
			}
		}
		w <- !flowdate.exists(yyyymmdd.ex.int(z))
	}
#
# CREATE RESULT
	if (length(z) > 1) z <- approx(y, z, x)$y
	z <- yyyymmdd.ex.int(z)
#
# RETURN RESULT
	z
}
flowdate.ex.yyyymm <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: flowdate.ex.yyyymm
# Author	: VKS
# Date		: 12/7/18
# Args		: x = a vector/single YYYYMM depending on if y is T/F
#		: y = T/F variable depending on whether the last or all
#		:	daily flow-publication dates in <x> are desired
# Output	: last/all trading days daily flow-publication dates in <x>
# -----------------------------------------------------------------
	z <- yyyymmdd.ex.yyyymm(x, y)
	if (!y)	z <- z[flowdate.exists(z)]
#
# RETURN RESULT
	z
}
flowdate.exists <- function(x) {
# -----------------------------------------------------------------
# Name		: flowdate.exists
# Author	: VKS
# Date		: 12/7/18
# Args		: x = a vector of calendar dates
# Output	: returns T if <x> is a daily flow-publication date
# -----------------------------------------------------------------
	yyyymmdd.exists(x) & !is.element(txt.right(x, 4), c("0101", "1225"))
}
flowdate.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: flowdate.lag
# Author	: VKS
# Date		: 12/7/18,4/11/19
# Args		: x = a vector of daily flow-publication dates
#		: y = an integer
# Output	: lags <x> by <y> daily flow-publication dates
# -----------------------------------------------------------------
	obj.lag(x, y, flowdate.to.int, flowdate.ex.int)
}
flowdate.seq <- function(x, y, n = 1) {
# -----------------------------------------------------------------
# Name		: flowdate.seq
# Author	: VKS
# Date		: 12/7/18,4/11/19
# Args		: x = a single daily flow-publication date
#		: y = a single daily flow-publication date
#		: n = a positive integer
# Output	: a sequence of dly flow-pub dates starting at <x> and, if possible, ending at <y>
# -----------------------------------------------------------------
	if (any(!flowdate.exists(c(x, y)))) stop("Inputs are not daily flow-publication dates")
	z <- obj.seq(x, y, flowdate.to.int, flowdate.ex.int, n)
	z
}
flowdate.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: flowdate.to.int
# Author	: VKS
# Date		: 4/11/19
# Args		: x = a vector of flow dates in YYYYMMDD format
# Output	: number of daily flow-publication dates after Friday, December 29, 2017
# -----------------------------------------------------------------
#
# WEEKDAY HOLIDAYS
	z <- unique(c("2018", yyyymm.to.yyyy(yyyymmdd.to.yyyymm(x))))
	z <- as.numeric(z)[order(z)]
	z <- seq(z[1], z[length(z)])
	z <- txt.expand(z, c("0101", "1225"), "")
	z <- z[yyyymmdd.exists(z)]
	z <- vec.named(seq_along(z), z)
	z <- z - z["20180101"]
#
# CREATE RESULT
	x <- yyyymmdd.to.int(x)
	y <- floor(approx(yyyymmdd.to.int(names(z)), z, x, rule = 1:2)$y)
	z <- x - ifelse(is.na(y), z[1] - 1, y)
#
# RETURN RESULT
	z
}
ftp.all.dir <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: ftp.all.dir
# Author	: VKS
# Date		: 2/20/18,2/22,2/27,11/28
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = ftp site (defaults to standard)
#		: n = user id (defaults to standard)
#		: w = password (defaults to standard)
# Output	: remote-site directory listing of all sub-folders
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(y)) y <- ftp.credential("ftp")
	if (missing(n)) n <- ftp.credential("user")
	if (missing(w)) w <- ftp.credential("pwd")
#
# CREATE RESULT
	z <- ftp.all.files.underlying(x, y, n, w, F)
	z <- txt.right(z, nchar(z) - nchar(x) - 1)
#
# RETURN RESULT
	z
}
ftp.all.files <- function(x, y, n, w, h = "ftp") {
# -----------------------------------------------------------------
# Name		: ftp.all.files
# Author	: VKS
# Date		: 8/11/17,11/2,11/10,1/23/18,2/12,2/22,2/27,3/8,11/28,
#		: 3/29/21
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = ftp site (defaults to standard)
#		: n = user id (defaults to standard)
#		: w = password (defaults to standard)
#		: h = protocol (either "ftp" or "sftp")
# Output	: remote-site directory listing of files (incl. sub-folders)
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(y)) y <- ftp.credential("ftp", h)
	if (missing(n)) n <- ftp.credential("user", h)
	if (missing(w)) w <- ftp.credential("pwd", h)
#
# CREATE RESULT
	z <- ftp.all.files.underlying(x, y, n, w, T, h)
	if (x == "/") x <- ""
	z <- txt.right(z, nchar(z) - nchar(x) - 1)
#
# RETURN RESULT
	z
}
ftp.all.files.underlying <- function(x, y, n, w, h, u = "ftp") {
# -----------------------------------------------------------------
# Name		: ftp.all.files.underlying
# Author	: VKS
# Date		: 8/11/17,11/2,11/10,1/23/18,2/12,2/22,2/27,3/8,4/2/19,
#		: 4/17,3/29/21
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = ftp site
#		: n = user id
#		: w = password
#		: h = T/F depending on whether you want files or folders
#		: u = protocol (either "ftp" or "sftp")
# Output	: remote-site directory listing of files or folders
# -----------------------------------------------------------------
	z <- NULL
	while (length(x) > 0) {
		cat(x[1], "...\n")
		m <- ftp.dir(x[1], y, n, w, F, u)
		if (!is.null(m)) {
			j <- names(m)
			if (x[1] != "/" & x[1] != "") j <- paste(x[1], j, sep = "/") else j <- paste0("/", j)
			if (any(m == h)) z <- c(z, j[m == h])
			if (any(!m)) x <- c(x, j[!m])
		}
		x <- x[-1]
	}
	z
}
ftp.break <- function(x) {
# -----------------------------------------------------------------
# Name		: ftp.break
# Author	: VKS
# Date		: 3/30/21
# Args		: x = string vector
# Output	: integer vector locating " MMM " in <x>
# -----------------------------------------------------------------
#
# PARAMETERS
	month.abbrv <- vec.named(1:12, month.abb)
#
# MOST LIKELY BREAK POINT
	n <- min(nchar(x)) - 4
	z <- rep(NA, n)
	for (i in 1:n) z[i] <- sum(!is.element(substring(x, i, i + 4), paste0(" ", names(month.abbrv), " ")))
	z <- (1:n)[order(z)][1]
	w <- is.element(substring(x, z, z + 4), paste0(" ", names(month.abbrv), " "))
	z <- ifelse(w, z, NA)
	if (any(!w)) z[!w] <- ftp.break(x[!w])
#
# RETURN RESULT
	z
}
ftp.credential <- function(x, y = "ftp", n = F) {
# -----------------------------------------------------------------
# Name		: ftp.credential
# Author	: VKS
# Date		: 11/9/18,12/25/20,3/29/21,2/10/22
# Args		: x = one of ftp/user/pwd
#		: y = one of ftp/sftp
#		: n = T/F flag for ftp.use.epsv argument of getCurlHandle
# Output	: relevant ftp credential
# -----------------------------------------------------------------
	z <- ifelse(n & y == "ftp", "-credential-T", "-credential")
	z <- as.character(map.rname(vec.read(parameters(paste0(y, z))), x))
	z
}
ftp.del <- function(x, y, n, w, h, u = "ftp") {
# -----------------------------------------------------------------
# Name		: ftp.del
# Author	: VKS
# Date		: 9/18/20,10/9,10/14,2/23/21,3/26,3/30
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = a SINGLE remote file (e.g. "foo.txt")
#		: n = ftp site (defaults to standard)
#		: w = user id (defaults to standard)
#		: h = password (defaults to standard)
#		: u = protocol (either "ftp" or "sftp")
# Output	: deletes file <y> on remote site <x>
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- ftp.credential("ftp", u)
	if (missing(w)) w <- ftp.credential("user", u)
	if (missing(h)) h <- ftp.credential("pwd", u)
#
# EXECUTE COMMAND
	z <- paste0(u, "://", n, x, "/", y)
	u <- ifelse(u == "ftp", "DELE", "RM")
	z <- tryCatch(curlPerform(url = z, quote = paste0(u, " ", x, "/", y), userpwd = paste0(w, ":", h)), error = function(e){NULL})
#
# RETURN NOTHING
	invisible()
}
ftp.dir <- function(x, y, n, w, h = F, u = "ftp", v = F) {
# -----------------------------------------------------------------
# Name		: ftp.dir
# Author	: VKS
# Date		: 8/11/17,11/10,3/2/18,8/28,11/9,4/2/19,4/17,4/25,9/11/20,
#		: 10/9,12/11,1/7/21,1/11,2/23,3/25,3/29,3/30,2/7/22,2/10,
#		: 7/1
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = ftp site (defaults to standard)
#		: n = user id (defaults to standard)
#		: w = password (defaults to standard)
#		: h = T/F depending on whether you want time stamps
#		: u = protocol (either "ftp" or "sftp")
#		: v = T/F flag for ftp.use.epsv argument of getURL
# Output	: logical or YYYYMMDD vector indexed by remote file names
# Import	: @importFrom RCurl getURL
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(y)) y <- ftp.credential("ftp", u, v)
	if (missing(n)) n <- ftp.credential("user", u, v)
	if (missing(w)) w <- ftp.credential("pwd", u, v)
#
# FILE LIST
	z <- getURL(paste0(u, "://", y, x, "/"), userpwd = paste0(n, ":", w), ftp.use.epsv = v)
	z <- txt.parse(z, ifelse(u == "ftp", "\r\n", "\n"))
#
# PARSE OUTPUT
	if (v & u == "ftp") z <- ftp.dir.parse.new(z) else z <- ftp.dir.parse.77(z)
#
# EXCISE ZERO-SIZE FILES
	z <- z[!z[, "is.file"] | z[, "size"] > 0, ]
#
# PARSE OUTPUT
	if (dim(z)[1] > 0) {
		h <- ifelse(h, "yyyymmdd", "is.file")
		z <- vec.named(z[, h], z[, "file"])
	} else {
		z <- NULL
	}
#
# RETURN RESULT
	z
}
ftp.dir.parse.77 <- function(x) {
# -----------------------------------------------------------------
# Name		: ftp.dir.parse.77
# Author	: VKS
# Date		: 2/8/22
# Args		: x = string vector (raw output of ftp)
# Output	: data frame with ftp information
# -----------------------------------------------------------------
	n <- ftp.break(x)
#
# BASICS
	z <- substring(x, n + 1, nchar(x))
	z <- data.frame(substring(z, 1, 3), as.numeric(substring(z, 5, 6)), substring(z, 8, 12), substring(z, 14, nchar(z)), stringsAsFactors = F)
	names(z) <- c("mm", "dd", "yyyy", "file")
#
# ADD SIZE INFORMATION
	y <- substring(x, 1, n - 1)
	z[, "is.file"] <- txt.left(y, 1) == "-"
	if (dim(z)[1] == 1) {
		z[, "size"] <- as.numeric(txt.parse(txt.itrim(y), txt.space(1))[5])/2^10
	} else {
		z[, "size"] <- as.numeric(txt.parse(txt.itrim(y), txt.space(1))[, 5])/2^10
	}
#
# YYYYMMDD REPRESENTATION
	month.abbrv <- vec.named(1:12, month.abb)
	z$mm <- map.rname(month.abbrv, z$mm)
	z$yyyy <- ifelse(txt.has(z$yyyy, ":", T), yyyymm.to.yyyy(yyyymmdd.to.yyyymm(today())), z$yyyy)
	z$yyyy <- as.numeric(z$yyyy)
	z[, "yyyymmdd"] <- as.character(10000 * z$yyyy + 100 * z$mm + z$dd)
#
# FINAL RESULT
	z <- z[, c("size", "is.file", "yyyymmdd", "file")]
#
# RETURN RESULT
	z
}
ftp.dir.parse.new <- function(x) {
# -----------------------------------------------------------------
# Name		: ftp.dir.parse.new
# Author	: VKS
# Date		: 2/8/22,3/25
# Args		: x = string vector (raw output of ftp)
# Output	: data frame with ftp information
# -----------------------------------------------------------------
#
# BASICS
	z <- data.frame(substring(x, 1, 8), substring(x, 18, 39), substring(x, 40, nchar(x)), stringsAsFactors = F)
	names(z) <- c("yyyymmdd", "size", "file")
#
# IS FILE
	z[, "is.file"] <- !txt.has(x, " <DIR> ", T)
	z[, "size"] <- ifelse(z[, "is.file"], z[, "size"], 0)
	z[, "size"] <- as.numeric(z[, "size"])/2^10
#
# YYYYMMDD REPRESENTATION
	z[, "yyyymmdd"] <- paste0("20", substring(z[, "yyyymmdd"], 7, 8), substring(z[, "yyyymmdd"], 4, 5), substring(z[, "yyyymmdd"], 1, 2))
#
# FINAL RESULT
	z <- z[, c("size", "is.file", "yyyymmdd", "file")]
#
# RETURN RESULT
	z
}
ftp.download <- function(x, y, n, w, h, u = "ftp") {
# -----------------------------------------------------------------
# Name		: ftp.download
# Author	: VKS
# Date		: 9/18/20,3/25/21,3/29
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = local folder (e.g. "C:\\temp\\mystuff")
#		: n = ftp site
#		: w = user id
#		: h = password
#		: u = protocol (either "ftp" or "sftp")
# Output	: replicates <x> in folder <y>
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- ftp.credential("ftp", u)
	if (missing(w)) w <- ftp.credential("user", u)
	if (missing(h)) h <- ftp.credential("pwd", u)
#
# LIST ALL FILES ON THE REMOTE SITE
	z <- ftp.all.files(x, n, w, h, u)
#
# RECREATE DIRECTORY STRUCTURE LOCALLY
	y <- paste(y, dir.parent(z), sep = "\\")
	y <- ifelse(txt.right(y, 1) == "\\", txt.left(y, nchar(y) - 1), y)
	dir.ensure(paste(unique(y), "foo.txt", sep = "\\"))
#
# PERFORM DOWNLOAD
	z <- paste(x, z, sep = "/")
	for (j in seq_along(z)) {
		cat(txt.right(z[j], nchar(z[j]) - nchar(x)), "...\n")
		ftp.get(z[j], y[j], n, w, h, u)
	}
#
# RETURN NOTHING
	invisible()
}
ftp.exists <- function(x, y) {
# -----------------------------------------------------------------
# Name		: ftp.exists
# Author	: VKS
# Date		: 12/17/20
# Args		: x = report name
#		: y = date for which you want to send the report
# Output	: T/F depending on whether upload already happened
# -----------------------------------------------------------------
	record.exists(x, y, "upload.txt")
}
ftp.file <- function(x) {
# -----------------------------------------------------------------
# Name		: ftp.file
# Author	: VKS
# Date		: 10/9/20
# Args		: x = a string of full paths
# Output	: strips out parent directory, returning just the file name
# -----------------------------------------------------------------
	txt.right(x, nchar(x) - nchar(ftp.parent(x)) - 1)
}
ftp.get <- function(x, y, n, w, h, u = "ftp", v = F) {
# -----------------------------------------------------------------
# Name		: ftp.get
# Author	: VKS
# Date		: 8/27/18,11/9,5/9/19,9/16/20,10/9,10/12,12/11,
#		: 2/23/21,3/25,3/29,5/26/22
# Args		: x = remote file on an ftp site (e.g. "/ftpdata/mystuff/foo.txt")
#		: y = local folder (e.g. "C:\\temp")
#		: n = ftp site (defaults to standard)
#		: w = user id (defaults to standard)
#		: h = password (defaults to standard)
#		: u = protocol (either "ftp" or "sftp")
#		: v = T/F flag for ftp.use.epsv argument of getCurlHandle
# Output	: file <x> from remote site
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- ftp.credential("ftp", u, v)
	if (missing(w)) w <- ftp.credential("user", u, v)
	if (missing(h)) h <- ftp.credential("pwd", u, v)
#
# GET DATA
	z <- getCurlHandle(ftp.use.epsv = v, userpwd = paste0(w, ":", h))
	z <- getBinaryURL(paste0(u, "://", n, x), curl = z)
	writeBin(z, con = paste0(y, "\\", ftp.file(x)))
#
# RETURN NOTHING
	invisible()
}
ftp.info <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: ftp.info
# Author	: VKS
# Date		: 8/30/18,9/5
# Args		: x = M/W/D depending on whether flows are monthly/weekly/daily
#		: y = T/F depending on whether you want to check Fund or Share-Class level data
#		: n = one of sql.table/date.field/ftp.path
#		: w = filter (e.g. Aggregate/Active/Passive/ETF/Mutual)
# Output	: parameter <n> associated with <x> flows at the <y> level with the <w> filter
# -----------------------------------------------------------------
#
# READ IN PARAMETERS FILE
	z <- mat.read(parameters("classif-ftp"), "\t", NULL)
#
# SUBSET PARAMETERS FILE
	z <- z[z[, "Type"] == x & z[, "FundLvl"] == y & z[, "filter"] == w, n]
#
# RETURN RESULT
	z
}
ftp.kill <- function(x) {
# -----------------------------------------------------------------
# Name		: ftp.kill
# Author	: VKS
# Date		: 12/17/20,12/18
# Args		: x = report name
# Output	: deletes entry <x> in the ftp record. Returns nothing.
# -----------------------------------------------------------------
	record.kill(x, "upload.txt")
}
ftp.list <- function() {
# -----------------------------------------------------------------
# Name		: ftp.list
# Author	: VKS
# Date		: 2/11/20,12/17,12/18
# Args		: none
# Output	: named vector of emails and sent dates
# -----------------------------------------------------------------
	record.read("upload.txt")
}
ftp.parent <- function(x) {
# -----------------------------------------------------------------
# Name		: ftp.parent
# Author	: VKS
# Date		: 9/18/20
# Args		: x = a string of full paths
# Output	: returns paths to the parent directory
# -----------------------------------------------------------------
	z <- dirname(x)
	z <- ifelse(z == ".", "", z)
	z
}
ftp.put <- function(x, y, n, w, h, u = "ftp", v = F) {
# -----------------------------------------------------------------
# Name		: ftp.put
# Author	: VKS
# Date		: 9/18/20,10/9,10/14,2/23/21,3/25,3/30,2/7/22,2/10,3/10
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = a vector of local file path(s) (e.g. "C:\\temp\\foo.txt")
#		: n = ftp site (defaults to standard)
#		: w = user id (defaults to standard)
#		: h = password (defaults to standard)
#		: u = protocol (either "ftp" or "sftp")
#		: v = T/F flag for ftp.use.epsv argument of getCurlHandle
# Output	: puts file <y> to remote site <x>, creating folders as needed
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- ftp.credential("ftp", u, v)
	if (missing(w)) w <- ftp.credential("user", u, v)
	if (missing(h)) h <- ftp.credential("pwd", u, v)
#
# EXECUTE
	ctr <- 5
	z <- NULL
	while (is.null(z) & ctr > 0) {
		if (ctr < 5) cat("Trying to upload to", x, "again ..\n")
		z <- getCurlHandle(ftp.use.epsv = v, userpwd = paste0(w, ":", h))
		z <- tryCatch(ftpUpload(y, paste0(u, "://", n, x, "/", ftp.file(y)), curl = z, ftp.create.missing.dirs = T), error = function(e){NULL})
		ctr <- ctr - 1
	}
#
# RETURN NOTHING
	invisible()
}
ftp.record <- function(x, y) {
# -----------------------------------------------------------------
# Name		: ftp.record
# Author	: VKS
# Date		: 12/17/20,12/18
# Args		: x = report name
#		: y = date for which you sent the report
# Output	: updates the email record. Returns nothing.
# -----------------------------------------------------------------
	record.write(x, y, "upload.txt")
}
ftp.rmdir <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: ftp.rmdir
# Author	: VKS
# Date		: 9/18/20,10/9,10/14,2/23/21,3/26
# Args		: x = remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = folder to be deleted (e.g. "hoo")
#		: n = ftp site (defaults to standard)
#		: w = user id (defaults to standard)
#		: h = password (defaults to standard)
# Output	: removes directory <y> under <x>
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- ftp.credential("ftp")
	if (missing(w)) w <- ftp.credential("user")
	if (missing(h)) h <- ftp.credential("pwd")
#
# EXECUTE COMMAND
	z <- tryCatch(curlPerform(url = paste0("ftp://", n, x, "/", y, "/"), quote = paste0("RMD ", x, "/", y, "/"), userpwd = paste0(w, ":", h)), error = function(e){NULL})
#
# RETURN NOTHING
	invisible()
}
ftp.sql.factor <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: ftp.sql.factor
# Author	: VKS
# Date		: 8/30/18,9/4,9/5,9/6,9/10,9/14,9/17,10/1,10/2,10/3,10/8,
#		: 10/9,10/11,10/24,11/1,4/1/19,4/4,5/9,5/15,8/13,8/21,9/16,
#		: 11/10/20,12/2,12/3,12/9,4/2/21,3/28/22
# Args		: x = vector of M/W/D depending on whether flows are monthly/weekly/daily
#		: y = flow date in YYYYMMDD format
#		: n = fund filter (e.g. Aggregate/Active/Passive/ETF/Mutual)
#		: w = stock filter (e.g. All/China/Japan)
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
# Output	: SQL code to validate <x> flows at the <y> level
# -----------------------------------------------------------------
#
# DEFAULT BREAKDOWN FILTER
	if (missing(h)) {
		if (any(x == c("StockM", "StockD", "FwtdEx0", "FwtdIn0", "SwtdEx0", "SwtdIn0", "FundCtM", "HoldSum", "FundCt"))) {
			h <- "GeoId"
		} else {
			h <- "All"
		}
	}
#
# CREATE RESULT
	if (all(is.element(x, paste0("Flo", c("Trend", "Diff", "Diff2"))))) {
		z <- sql.1dFloTrend(y, c(x, qa.filter.map(n)), 26, w, T)
	} else if (all(is.element(x, paste0("ActWt", c("Trend", "Diff", "Diff2"))))) {
		z <- sql.1dActWtTrend(y, c(x, qa.filter.map(n)), w, T)
	} else if (all(x == "FloMo")) {
		z <- sql.1dFloMo(y, c(x, qa.filter.map(n)), w, T, h)
	} else if (all(x == "StockD")) {
		z <- sql.1dFloMo(y, c("FloDollar", qa.filter.map(n)), w, T, h)
	} else if (all(x == "AssetsStartDollarD")) {
		z <- sql.1dFloMo(y, c("AssetsStartDollar", qa.filter.map(n)), w, T, h)
	} else if (all(x == "FundCtD")) {
		z <- sql.1dFundCt(y, c("FundCt", qa.filter.map(n)), w, T, "GeoId")
	} else if (all(x == "FundCtM")) {
		z <- sql.1mFundCt(yyyymmdd.to.yyyymm(y), c("FundCt", qa.filter.map(n)), w, T, h)
	} else if (all(x == "HoldAum")) {
		z <- sql.1mHoldAum(yyyymmdd.to.yyyymm(y), c("HoldAum", qa.filter.map(n)), w, T, h)
	} else if (all(x == "HoldSum")) {
		z <- sql.1mFundCt(yyyymmdd.to.yyyymm(y), c("HoldSum", qa.filter.map(n)), w, T, h)
	} else if (all(x == "HoldSumTopV")) {
		z <- sql.1mFundCt(yyyymmdd.to.yyyymm(y), c("HoldSum", qa.filter.map(n)), w, T, h, 5)
	} else if (all(x == "HoldSumTopX")) {
		z <- sql.1mFundCt(yyyymmdd.to.yyyymm(y), c("HoldSum", qa.filter.map(n)), w, T, h, 10)
	} else if (all(x == "Dispersion")) {
		z <- sql.Dispersion(yyyymmdd.to.yyyymm(y), c(x, qa.filter.map(n)), w, T)
	} else if (all(is.element(x, c("FundCt", "Herfindahl")))) {
		z <- sql.Herfindahl(yyyymmdd.to.yyyymm(y), c(x, qa.filter.map(n)), w, T, h)
	} else if (all(x == "StockM")) {
		z <- sql.1mFloMo(yyyymmdd.to.yyyymm(y), c("FloDollar", qa.filter.map(n)), w, T, h)
	} else if (all(x == "FloMoM")) {
		z <- sql.1mFloMo(yyyymmdd.to.yyyymm(y), c("FloMo", qa.filter.map(n)), w, T, h)
	} else if (all(x == "IOND")) {
		z <- sql.1dFloMo(y, c("Inflow", "Outflow", qa.filter.map(n)), w, T, h)
	} else if (all(x == "IONM")) {
		z <- sql.1mFloMo(yyyymmdd.to.yyyymm(y), c("Inflow", "Outflow", qa.filter.map(n)), w, T, h)
	} else if (all(is.element(x, paste0("Alloc", c("Trend", "Diff", "Mo"))))) {
		z <- sql.1mAllocMo(yyyymmdd.to.yyyymm(y), c(x, qa.filter.map(n)), w, T)
	} else if (all(x == "AllocD")) {
		z <- sql.1mAllocD(yyyymmdd.to.yyyymm(y), c("AllocDA", "AllocDInc", "AllocDDec", "AllocDAdd", "AllocDRem", qa.filter.map(n)), w, T, F)
	} else if (all(x == "AllocSkew")) {
		z <- sql.1mAllocSkew(yyyymmdd.to.yyyymm(y), c(x, qa.filter.map(n)), w, T)
	} else if (all(is.element(x, c("FwtdEx0", "FwtdIn0", "SwtdEx0", "SwtdIn0")))) {
		z <- sql.TopDownAllocs(yyyymmdd.to.yyyymm(y), c(x, qa.filter.map(n)), w, T, h)
	} else {
		stop("Bad factor")
	}
#
# RETURN RESULT
	z
}
ftp.sql.other <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: ftp.sql.other
# Author	: VKS
# Date		: 8/30/18,9/4,9/5,9/6,9/10,9/14,9/17,10/1,10/2,10/3,10/5,10/8,
#		: 10/11,10/24,6/10/19,11/23/20
# Args		: x = M/W/D/C/I/S depending on flows or allocations
#		: y = flow date in YYYYMMDD format
#		: n = filter (e.g. Aggregate/Active/Passive/ETF/Mutual)
# Output	: SQL code to validate <x> flows at the <y> level
# -----------------------------------------------------------------
#
# PARAMETERS
	sql.table <- ftp.info(x, T, "sql.table", n)
	h <- ftp.info(x, T, "date.field", n)
	cols <- qa.columns(x)[-1][-1]
#
# GENERATE SCRIPT
	if (any(x == c("M", "W", "D"))) {
		w <- list(A = sql.ui(), B = paste(h, "= @dy"))
		w <- sql.and(w)
		z <- c("FundId", sql.yyyymmdd(h, "ReportDate"))
		z <- c(z, paste0(cols, " = sum(", cols, ")"))
		z <- sql.tbl(z, paste(sql.table, "t1 inner join FundHistory t2 on t1.HFundId = t2.HFundId"), w, paste(h, "FundId", sep = ", "))
	} else if (any(x == c("C", "I", "S"))) {
		w <- list(A = sql.ui(), B = paste(h, "= @dy"), C = "FundType in ('B', 'E')")
		if (x == "C") w[["D"]] <- c("(", sql.and(sql.cross.border(F), "or"), ")")
		w <- sql.and(w)
		z <- c("t2.FundId", paste0("ReportDate = convert(char(8), ", h, ", 112)"))
		z <- c(z, cols)
		z <- sql.tbl(z, c(paste(sql.table, "t1"), "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId"), w)
	} else {
		stop("Bad Argument")
	}
#
# FINISH UP
	z <- c(sql.declare("@dy", "datetime", y), sql.unbracket(z))
	z <- paste(z, collapse = "\n")
#
# RETURN RESULT
	z
}
ftp.upload <- function(x, y, n, w, h, u = "ftp", v = F) {
# -----------------------------------------------------------------
# Name		: ftp.upload
# Author	: VKS
# Date		: 9/18/20,10/9,10/14,3/29/21,4/11/22
# Args		: x = empty remote folder on an ftp site (e.g. "/ftpdata/mystuff")
#		: y = local folder containing the data (e.g. "C:\\temp\\mystuff")
#		: n = ftp site (defaults to standard)
#		: w = user id (defaults to standard)
#		: h = password (defaults to standard)
#		: u = protocol (either "ftp" or "sftp")
#		: v = T/F flag for ftp.use.epsv argument of getCurlHandle
# Output	: Copies up files from the local machine
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- ftp.credential("ftp", u, v)
	if (missing(w)) w <- ftp.credential("user", u, v)
	if (missing(h)) h <- ftp.credential("pwd", u, v)
#
# LOCAL FILE LIST
	z <- dir.all.files(y, "*.*")
	s <- ftp.parent(z)
	s <- txt.right(s, nchar(s) - nchar(y))
	s <- paste0(x, s)
#
# PERFORM UPLOAD
	for (j in seq_along(z)) {
		cat(ftp.file(z[j]), "")
		ftp.put(s[j], z[j], n, w, h, u, v)
		cat(substring(Sys.time(), 12, 16), "\n")
	}
#
# RETURN NOTHING
	invisible()
}
fwd.probs <- function(x, y, floW, sum.flows, lag, delay, doW, retW, idx, prd.size) {
# -----------------------------------------------------------------
# Name		: fwd.probs
# Author	: VKS
# Date		: 12/8/17
# Args		: x = predictor indexed by yyyymmdd or yyyymm
#		: y = total return index indexed by yyyymmdd or yyyymm
#		: floW = flow window in days
#		: sum.flows = T/F depending on whether the predictor is to be summed or compounded
#		: lag = number of periods to lag the predictor
#		: delay = delay in knowing data
#		: doW = day of the week you will trade on (5 = Fri, NULL for monthlies)
#		: retW = size of forward return horizon
#		: idx = the index within which you trade
#		: prd.size = size of each period in terms of days if the rows of <x> are yyyymmdd or months otherwise
# Output	: probability that forward return is positive given predictor is positive
# -----------------------------------------------------------------
#
# BASIC DATA
	x <- bbk.data(x, y, floW, sum.flows, lag, delay, doW, retW, idx, prd.size, F)
	y <- x$fwdRet
	x <- x$x
#
# SUMMARIZE
	z <- c("All", "Pos", "Exc", "Last")
	z <- matrix(NA, dim(x)[2], length(z), F, list(dimnames(x)[[2]], z))
	z[, "Last"] <- unlist(x[dim(x)[1], ])
	for (j in dimnames(x)[[2]]) {
		w1 <- x[, j]
		w2 <- y[, j]
		z[j, "All"] <- sum(!is.na(w2) & w2 > 0)/sum(!is.na(w2))
		z[j, "Pos"] <- sum(!is.na(w1) & !is.na(w2) & w2 > 0 & w1 > 0)/sum(!is.na(w1) & !is.na(w2) & w1 > 0)
	}
	z[, "Exc"] <- z[, "Pos"] - z[, "All"]
#
# RETURN RESULT
	z
}
fwd.probs.wrapper <- function(x, y, floW, sum.flows, lags, delay, doW, hz, idx, prd.size) {
# -----------------------------------------------------------------
# Name		: fwd.probs.wrapper
# Author	: VKS
# Date		: 12/8/17,1/9/19
# Args		: x = predictor indexed by yyyymmdd or yyyymm
#		: y = total return index indexed by yyyymmdd or yyyymm
#		: floW = flow window in days
#		: sum.flows = T/F depending on whether the predictor is to be summed or compounded
#		: lags = number of periods to lag the predictor
#		: delay = delay in knowing data
#		: doW = day of the week you will trade on (5 = Fri, NULL for monthlies)
#		: hz = a vector of forward return windows
#		: idx = the index within which you trade
#		: prd.size = size of each period in terms of days if the rows of <x> are yyyymmdd or months otherwise
# Output	: probability that forward return is positive given predictor is positive
# -----------------------------------------------------------------
	z <- list()
	for (retW in hz) {
		z[[as.character(retW)]] <- list()
		for (lag in lags) z[[as.character(retW)]][[as.character(lag)]] <- fwd.probs(x, y, floW, sum.flows, lag, delay, doW, retW, idx, prd.size)
		z[[as.character(retW)]] <- simplify2array(z[[as.character(retW)]])
	}
	z <- simplify2array(z)
	z
}
glome.ex.R3 <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: glome.ex.R3
# Author	: VKS
# Date		: 11/28/20
# Args		: x = a number or numeric vector between 0 and 1
#		: y = a number or numeric vector between 0 and 1
#		: n = a number or numeric vector between 0 and 1
# Output	: maps unit cube to the glome (sphere in 4D)
# -----------------------------------------------------------------
	w <- length(x)
#
# CREATE RESULT
	z <- sqrt(1 - x^2) * sin(2 * pi * y)
	z <- c(z, sqrt(1 - x^2) * cos(2 * pi * y))
	z <- c(z, x * sin(2 * pi * n))
	z <- c(z, x * cos(2 * pi * n))
#
# FORMAT RESULT
	if(w > 1) z <- matrix(z, w, 4, F, list(1:w, char.seq("A", "D")))
#
# RETURN RESULT
	z
}
gram.schmidt <- function(x, y) {
# -----------------------------------------------------------------
# Name		: gram.schmidt
# Author	: VKS
# Date		: 4/24/19
# Args		: x = a numeric vector/matrix/data frame
#		: y = a numeric isomekic vector
# Output	: Gram-Schmidt orthogonalization of <x> to <y>
# -----------------------------------------------------------------
	x - tcrossprod(y, crossprod(x, y)/sum(y^2))
}
greek.ex.english <- function() {
# -----------------------------------------------------------------
# Name		: greek.ex.english
# Author	: VKS
# Date		: 12/19/17
# Args		: none
# Output	: returns a named vector
# Notes		: Ephesians 3:18
#		: to platos kai mekos kai hypsos kai bathos
#		: the breadth and length and height and depth
# -----------------------------------------------------------------
	vec.named(c("platos", "mekos", "hypsos", "bathos"), c("breadth", "length", "height", "depth"))
}
GSec.to.GSgrp <- function(x) {
# -----------------------------------------------------------------
# Name		: GSec.to.GSgrp
# Author	: VKS
# Date		: 11/29/16
# Args		: x = a vector of sectors
# Output	: makes Sector groups
# -----------------------------------------------------------------
	z <- rep("", length(x))
	z <- ifelse(is.element(x, c(15, 20, 25, 45)), "Cyc", z)
	z <- ifelse(is.element(x, c(10, 30, 35, 50, 55)), "Def", z)
	z <- ifelse(is.element(x, 40), "Fin", z)
	z
}
html.and <- function(x) {
# -----------------------------------------------------------------
# Name		: html.and
# Author	: VKS
# Date		: 1/27/20
# Args		: x = a string vector
# Output	: <x> stated in a grammatical phrase
# -----------------------------------------------------------------
#
# PRELIMINARIES
	n <- length(x)
#
# GRAMMATICAL MODIFIERS
	w <- rep("", n)
	if (n > 1) w[n - 1] <- " and"
	if (n > 2) w[3:n - 2] <- ","
#
# CREATE RESULT
	z <- paste(paste0(x, w), collapse = " ")
#
# RETURN RESULT
	z
}
html.ex.utf8 <- function(x) {
# -----------------------------------------------------------------
# Name		: html.ex.utf8
# Author	: VKS
# Date		: 4/6/21
# Args		: x = string vector
# Output	: code to represent <x> in html
# -----------------------------------------------------------------
#
# SPLIT
	z <- txt.to.char(x)
#
# EXEMPT
	w <- !is.element(z, char.seq("A", "Z"))
	w <- w & !is.element(z, tolower(char.seq("A", "Z")))
	w <- w & !is.element(z, c("\n", "\t", "\\", " "))
	w <- w & !is.element(z, c(">", "<", "=", "/", "%", "%", "$", ":", ".", ",", ";", "?", "!"))
	w <- w & !is.element(z, 0:9)
#
# CONVERT
	for (j in seq_along(z[w])) z[w][j] <- paste0("&#x", as.hexmode(utf8ToInt(z[w][j])), ";")
#
# RECOMBINE
	z <- paste(z, collapse = "")
#
# RETURN RESULT
	z
}
html.flow.breakdown <- function(x, y, n = 0) {
# -----------------------------------------------------------------
# Name		: html.flow.breakdown
# Author	: VKS
# Date		: 1/24/20,1/27,2/14,5/22
# Args		: x = a named numeric vector
#		: y = a string
#		: n = a number representing miscellaneous flows
# Output	: html breaking down flows into constituents
# -----------------------------------------------------------------
	if (y != "") y <- paste0(" ", y)
#
# SORT
	x <- x[order(abs(x), decreasing = T)]
	x <- x[order(x > 0, decreasing = sum(x) + n > 0)]
#
# PRESERVE SIGN
	u <- as.numeric(sign(sum(x) + n))
	x <- x * u
#
# COUNT CONTRIBUTORS AND DETRACTORS
	h <- sum(x > 0)
	m <- length(x) - h
	x <- paste0(names(x), " ($", int.format(round(abs(x))), " million)")
#
# CREATE RESULT
	if (h == 0) {
		z <- paste("This week's", ifelse(u > 0, "inflows", "outflows"), "were driven by sundry small contributions which overwhelmed", ifelse(u > 0, "outflows from", "inflows into"), html.and(x))
	} else if (m == 0) {
		if (u > 0) {
			z <- paste0("inflows ", ifelse(abs(n) > 0, "primarily ", ""), "went into")
		} else {
			z <- paste0("outflows ", ifelse(abs(n) > 0, "primarily ", ""), "came from")
		}
		z <- paste("This week's", z, html.and(x))
	} else {
		z <- paste("This week's", ifelse(u > 0, "inflows", "outflows"), ifelse(abs(n) > 0, "were primarily", "were"), "driven by", html.and(x[1:h]))
		z <- paste0(z, y, ", but offset by")
		z <- paste(z, ifelse(u > 0, "outflows from", "inflows into"), html.and(x[h + 1:m]))
	}
	z <- paste0(z, y)
#
# RETURN RESULT
	z
}
html.flow.english <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: html.flow.english
# Author	: VKS
# Date		: 12/5/19,12/6,12/10,12/13,12/16,4/3/20,4/9,4/10,10/1
# Args		: x = a named vector of integers (numbers need to be rounded)
#		: y = a named text vector
#		: n = line number(s) at which to insert a statement
#		: w = statement(s) to be inserted
# Output	: writes a flow report in English
# -----------------------------------------------------------------
#
# INTRODUCTION
	z <- format(day.to.date(y["date"]), "%B %d %Y")
	z <- paste("For the week ended", z, "fund flow data from EPFR for", y["AssetClass"], "($")
	z <- paste0(z, int.format(x["AUM"]), " million in total assets) reported net")
	z <- paste(z, ifelse(x["last"] > 0, "INFLOWS", "OUTFLOWS"), "of $")
	z <- paste0(z, int.format(abs(x["last"])), " million vs an")
	z <- paste(z, ifelse(x["prior"] > 0, "inflow", "outflow"), "of $")
	z <- paste0(z, int.format(abs(x["prior"])), " million the prior week")
#
# STRAIGHT
	if (x["straight"] > 0) {
		u <- paste("This is the", txt.ex.int(x["straight"], T), ifelse(x["straight"] > 4, "straight", "consecutive"))
		u <- paste(u, "week of", ifelse(x["last"] > 0, "inflows", "outflows"))
	} else if (x["straight"] == -1) {
		u <- paste("This is the first week of", ifelse(x["last"] > 0, "inflows,", "outflows,"))
		u <- paste(u, "the prior one seeing", ifelse(x["last"] > 0, "outflows", "inflows"))
	} else {
		u <- paste("This is the first week of", ifelse(x["last"] > 0, "inflows,", "outflows,"))
		u <- paste(u, "the prior", txt.ex.int(-x["straight"]), "seeing", ifelse(x["last"] > 0, "outflows", "inflows"))
	}
	z <- c(z, u)
#
# YTD
	u <- paste(txt.left(y["date"], 4), "YTD has seen")
	if (x["YtdCountOutWks"] > x["YtdCountInWks"]) {
		u <- paste(u, txt.ex.int(x["YtdCountOutWks"]), "weeks of outflows and")
		if (x["YtdCountInWks"] > 0) {
			u <- paste(u, txt.ex.int(x["YtdCountInWks"]), "of inflows")
		} else u <- paste(u, "none of inflows")
	} else {
		u <- paste(u, txt.ex.int(x["YtdCountInWks"]), "weeks of inflows and")
		if (x["YtdCountOutWks"] > 0) {
			u <- paste(u, txt.ex.int(x["YtdCountOutWks"]), "of outflows")
		} else u <- paste(u, "none of outflows")
	}
	if (x["YtdCountInWks"] > 0 & x["YtdCountOutWks"] > 0) {
		u <- paste0(u, " (largest inflow $", int.format(x["YtdBigIn"]), " million; largest outflow $", int.format(x["YtdBigOut"]), " million)")
	} else if (x["YtdCountInWks"] > 0) {
		u <- paste0(u, " (largest inflow $", int.format(x["YtdBigIn"]), " million)")
	} else {
		u <- paste0(u, " (largest outflow $", int.format(x["YtdBigOut"]), " million)")
	}
	z <- c(z, u)
#
# PRIOR YEAR
	u <- paste("For", txt.left(y["PriorYrWeek"], 4), "there were")
	if (x["PriorYrCountOutWks"] > x["PriorYrCountInWks"]) {
		u <- paste(u, txt.ex.int(x["PriorYrCountOutWks"]), "weeks of outflows and")
		if (x["PriorYrCountInWks"] > 0) {
			u <- paste(u, txt.ex.int(x["PriorYrCountInWks"]), "of inflows")
		} else u <- paste(u, "none of inflows")
	} else {
		u <- paste(u, txt.ex.int(x["PriorYrCountInWks"]), "weeks of inflows and")
		if (x["PriorYrCountOutWks"] > 0) {
			u <- paste(u, txt.ex.int(x["PriorYrCountOutWks"]), "of outflows")
		} else u <- paste(u, "none of outflows")
	}
	if (x["PriorYrCountInWks"] > 0 & x["PriorYrCountOutWks"] > 0) {
		u <- paste0(u, " (largest inflow $", int.format(x["PriorYrBigIn"]), " million; largest outflow $", int.format(x["PriorYrBigOut"]), " million)")
	} else if (x["PriorYrCountInWks"] > 0) {
		u <- paste0(u, " (largest inflow $", int.format(x["PriorYrBigIn"]), " million)")
	} else {
		u <- paste0(u, " (largest outflow $", int.format(x["PriorYrBigOut"]), " million)")
	}
	z <- c(z, u)
#
# FOUR-WEEK MOVING AVERAGE
	if (x["FourWeekAvg"] > 0) {
		u <- paste0("four-week moving average: $", int.format(x["FourWeekAvg"]), " million inflow (four-week cumulative: $", int.format(x["FourWeekSum"]), " million inflow)")
	} else {
		u <- paste0("four-week moving average: $", int.format(-x["FourWeekAvg"]), " million outflow (four-week cumulative: $", int.format(-x["FourWeekSum"]), " million outflow)")
	}
	z <- c(z, u)
#
# LATEST CUMULATIVE
	u <- paste(txt.left(y["date"], 4), "flow data (through", format(day.to.date(y["date"]), "%B %d"))
	if (x["YtdCumSum"] > 0) {
		u <- paste0(u, "): $", int.format(x["YtdCumSum"]), " million cumulative INFLOW, or a weekly average inflow of $", int.format(x["YtdCumAvg"]), " million")
	} else {
		u <- paste0(u, "): $", int.format(-x["YtdCumSum"]), " million cumulative OUTFLOW, or a weekly average outflow of $", int.format(-x["YtdCumAvg"]), " million")
	}
	z <- c(z, u)
#
# PRIOR CUMULATIVE
	u <- paste(txt.left(y["PriorYrWeek"], 4), "flow data (through", format(day.to.date(y["PriorYrWeek"]), "%B %d"))
	if (x["PriorYrCumSum"] > 0) {
		u <- paste0(u, "): $", int.format(x["PriorYrCumSum"]), " million cumulative INFLOW, or a weekly average inflow of $", int.format(x["PriorYrCumAvg"]), " million")
	} else {
		u <- paste0(u, "): $", int.format(-x["PriorYrCumSum"]), " million cumulative OUTFLOW, or a weekly average outflow of $", int.format(-x["PriorYrCumAvg"]), " million")
	}
	z <- c(z, u)
#
# COLLATE
	if (!missing(n) & !missing(w)) {
		while (length(n) > 0) {
			z <- c(z[1:n[1]], w[1], z[seq(n[1] + 1, length(z))])
			n <- n[-1]
			w <- w[-1]
		}
	}
	z <- paste(c(paste0("<br>", z[1]), html.list(z[-1]), "</p>"), collapse = "\n")
#
# RETURN RESULT
	z
}
html.flow.underlying <- function(x) {
# -----------------------------------------------------------------
# Name		: html.flow.underlying
# Author	: VKS
# Date		: 12/16/19,4/10/20,1/4/21
# Args		: x = a numeric vector indexed by YYYYMMDD
# Output	: list object containing the following items:
#		:	a) text - dates and text information about flows
#		:	b) numbers - numeric summary of the flows
# -----------------------------------------------------------------
#
# PRELIMINARIES
	x <- x[order(names(x), decreasing = T)]
#
# BASICS
	z <- vec.named(x[1:2], c("last", "prior"))
	n <- vec.named(names(x)[1], "date")
	z["FourWeekAvg"] <- mean(x[1:4])
	z["FourWeekSum"] <- sum(x[1:4])
#
# STRAIGHT
	y <- x > 0
	z["straight"] <- straight(y)
	if (z["straight"] == 1) z["straight"] <- -straight(y[-1])
#
# YTD
	y <- x[txt.left(names(x), 4) == txt.left(names(x)[1], 4)]
	z["YtdCountInWks"] <- sum(y > 0)
	z["YtdCountOutWks"] <- sum(y < 0)
	z["YtdBigIn"] <- max(y)
	z["YtdBigOut"] <- -min(y)
#
# PRIOR YEAR
	y <- x[txt.left(names(x), 4) != txt.left(names(x)[1], 4)]
	y <- y[txt.left(names(y), 4) == txt.left(names(y)[1], 4)]
	z["PriorYrCountInWks"] <- sum(y > 0)
	z["PriorYrCountOutWks"] <- sum(y < 0)
	z["PriorYrBigIn"] <- max(y)
	z["PriorYrBigOut"] <- -min(y)
#
# LATEST CUMULATIVE
	y <- x[txt.left(names(x), 4) == txt.left(names(x)[1], 4)]
	z["YtdCumAvg"] <- mean(y)
	z["YtdCumSum"] <- sum(y)
#
# PRIOR CUMULATIVE
	y <- x[txt.left(names(x), 4) != txt.left(names(x)[1], 4)]
	y <- y[txt.left(names(y), 4) == txt.left(names(y)[1], 4)]
	y <- y[order(names(y))]
	y <- y[1:min(sum(txt.left(names(x), 4) == txt.left(names(x)[1], 4)), length(y))]
	y <- y[order(names(y), decreasing = T)]
	n["PriorYrWeek"] <- names(y)[1]
	z["PriorYrCumAvg"] <- mean(y)
	z["PriorYrCumSum"] <- sum(y)
#
# FINAL OUTPUT
	z <- list(numbers = z, text = n)
#
# RETURN RESULT
	z
}
html.image <- function(x, y) {
# -----------------------------------------------------------------
# Name		: html.image
# Author	: VKS
# Date		: 4/20/19,10/12/20
# Args		: x = path to the image
#		: y = percentage magnification
# Output	: html to attach an image
# -----------------------------------------------------------------
	paste0("<br><img src='cid:", ftp.file(x), "' width= ", y, "% height= ", y, "%>")
}
html.list <- function(x) {
# -----------------------------------------------------------------
# Name		: html.list
# Author	: VKS
# Date		: 4/3/20
# Args		: x = a string vector
# Output	: <x> expressed as an html list
# -----------------------------------------------------------------
	c("<ul>", paste0("<li>", x, "</li>"), "</ul>")
}
html.positioning <- function(x, y) {
# -----------------------------------------------------------------
# Name		: html.positioning
# Author	: VKS
# Date		: 4/10/20
# Args		: x = matrix of indicator values
#		: y = security names (corresponding to columns of <x>)
# Output	: writes a positioning report
# -----------------------------------------------------------------
#
# COLUMN LABELS
	if (missing(y)) {
		y <- dimnames(x)[[2]]
	} else {
		y <- paste0(y, " (", dimnames(x)[[2]], ")")
	}
#
# ORDER DATA SO TIME FLOWS BACKWARDS AND HIGH ALPHA IS TO THE LEFT
	x <- x[order(dimnames(x)[[1]], decreasing = T), ]
	#
	y <- y[order(x[1,], decreasing = T)]
	x <- x[, order(x[1,], decreasing = T)]
#
# QUINTILE
	n <- qtl.eq(x)
#
# WEEK-OVER-WEEK CHANGES
	w1.new <- is.element(n[1, ], 1) & !is.na(n[2, ]) & n[2, ] > 1
	w5.new <- is.element(n[1, ], 5) & !is.na(n[2, ]) & n[2, ] < 5
	w1.old <- is.element(n[2, ], 1) & !is.element(n[1, ], 1)
	w5.old <- is.element(n[2, ], 5) & !is.element(n[1, ], 5)
	#
	z <- paste("<p>The week ended", format(day.to.date(dimnames(n)[[1]][1]), "%B %d %Y"), "saw")
	if (sum(w1.new) == 0 & sum(w5.new) == 0) {
		z <- c(z, "no new entrants into either the top or bottom quintile.")
	} else if (sum(w1.new) > 0) {
		z <- c(z, html.and(y[w1.new]))
		if (sum(w1.old) == 0) {
			z <- c(z, "rise to the top quintile.")
		} else {
			z <- c(z, "rise to the top quintile, displacing")
			z <- c(z, paste0(html.and(y[w1.old]), "."))
		}
		if (sum(w5.new) == 0) {
			z <- c(z, "There were no new entrants into the bottom quintile.")
		} else {
			z <- c(z, "Over the same week,")
			z <- c(z, html.and(y[w5.new]))
			if (sum(w5.old) == 0) {
				z <- c(z, "fell to the bottom quintile.")
			} else {
				z <- c(z, "fell to the bottom quintile, displacing")
				z <- c(z, paste0(html.and(y[w5.old]), "."))
			}
		}
	} else {
		z <- c(z, html.and(y[w5.new]))
		if (sum(w5.old) == 0) {
			z <- c(z, "fall to the bottom quintile.")
		} else {
			z <- c(z, "fall to the bottom quintile, displacing")
			z <- c(z, paste0(html.and(y[w5.old]), "."))
		}
		z <- c(z, "There were no new entrants into the top quintile.")
	}
	z <- c(z, "</p>")
#
# LONGEVITY
	h <- sapply(mat.ex.matrix(n == matrix(n[1, ], dim(n)[1], dim(n)[2], T)), straight)
	w <- is.element(n[1, ], c(1, 5)) & h > 1
	if (any(w)) {
		h <- (ifelse(is.element(n[1, ], 5), -1, 1) * h)[w]
		names(h) <- y[w]
		z <- c(z, html.tenure(h, c("week of top-quintile rating for", "for"), c("week of bottom-bucket status for", "for")))
	}
#
# CREATE RESULT
	z <- list(html = z, indicator = x[1, ], quintiles = n[1, ])
#
# RETURN RESULT
	z
}
html.problem <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: html.problem
# Author	: VKS
# Date		: 5/22/20
# Args		: x = report names
#		: y = string vector
#		: n = logical vector (T = error, F = not, NA = no check)
# Output	: problem report
# -----------------------------------------------------------------
	paste(c("Dear All,", html.problem.underlying(x, y, n), html.signature()), collapse = "\n")
}
html.problem.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: html.problem.underlying
# Author	: VKS
# Date		: 12/17/20
# Args		: x = report names
#		: y = string vector
#		: n = logical vector (T = error, F = not, NA = no check)
# Output	: problem report
# -----------------------------------------------------------------
#
# ERROR/NOT ERROR
	w <- !is.na(n) & n
	z <- NULL
	if (sum(w) == 0) {
		z <- c(z, "<p>", paste(y[1], txt.ex.int(sum(!is.na(n))), y[2], "</p>"))
	} else {
		z <- c(z, "<p>", y[3], html.list(x[w]), "</p>")
	}
#
# COULD NOT CHECK
	w <- is.na(n)
	if (any(w)) {
		z <- c(z, "<p>", y[4])
		z <- c(z, html.list(x[w]), "</p>")
	}
#
# RETURN RESULT
	z
}
html.signature <- function() {
# -----------------------------------------------------------------
# Name		: html.signature
# Author	: VKS
# Date		: 1/28/20,2/4,4/24,12/25,4/26/22
# Args		: none
# Output	: signature at the end of an email
# -----------------------------------------------------------------
	z <- paste0("<p>", sample(readLines(parameters("letterClosings")), 1), "</p><p>")
	z <- paste0(z, quant.info(machine.info("Quant"), "Name"), "<br>Quantitative Team, EPFR</p>")
	z <- paste0(z, "<p><i>", sample(readLines(parameters("letterSayings")), 1), "</i></p>")
	z
}
html.tbl <- function(x, y) {
# -----------------------------------------------------------------
# Name		: html.tbl
# Author	: VKS
# Date		: 1/16/20
# Args		: x = matrix/data-frame
#		: y = T/F depending on whether integer format is to be applied
# Output	: renders <x> in html
# -----------------------------------------------------------------
#
# APPLY INTEGER FORMAT
	if (y) {
		x <- round(x)
		x <- mat.ex.matrix(lapply(x, int.format), dimnames(x)[[1]])
	}
#
# HEADER
	z <- "<TABLE border=\"0\""
	z <- c(z, paste0("<TR><TH><TH>", paste(dimnames(x)[[2]], collapse = "<TH>")))
#
# DATA
	y <- dimnames(x)[[1]]
	x <- mat.ex.matrix(x)
	x$sep <- "</TD><TD align=\"right\">"
	z <- c(z, paste0("<TR><TH>", y, "<TD align=\"right\">", do.call(paste, x)))
#
# FINISH UP
	z <- paste(c(z, "</TABLE>"), collapse = "\n")
#
# RETURN RESULT
	z
}
html.tenure <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: html.tenure
# Author	: VKS
# Date		: 4/10/20,4/17
# Args		: x = a named vector of integers
#		: y = vector of length two for positive descriptions
#		: n = vector of length two for negative descriptions
# Output	: describes how long securities/factors have belonged to a group
# -----------------------------------------------------------------
#
# SORT INPUT VECTOR
	x <- x[order(abs(x), decreasing = T)]
	x <- x[order(sign(x), decreasing = T)]
#
# PIECES
	z <- NULL
	pos <- neg <- T
	for (j in unique(x)) {
		if (j > 0) {
			phrase <- ifelse(pos, y[1], y[2])
			pos <- F
		} else {
			phrase <- ifelse(neg, n[1], n[2])
			neg <- F
		}
		z <- c(z, paste("the", txt.ex.int(abs(j), T), phrase, html.and(names(x)[x == j])))
	}
	x <- unique(x)
#
# CREATE RESULT
	if (all(x > 0)) {
		z <- paste0("This is ", html.and(z[x > 0]), ".")
	} else if (all(x < 0)) {
		z <- paste0("This is ", html.and(z[x < 0]), ".")
	} else {
		z <- paste0("This is not only ", html.and(z[x > 0]), " but also ", html.and(z[x < 0]), ".")
	}
#
# RETURN RESULT
	z
}
int.format <- function(x) {
# -----------------------------------------------------------------
# Name		: int.format
# Author	: VKS
# Date		: 8/27/18,1/23/19,1/16/20
# Args		: x = a vector of integers
# Output	: adds commas "1,234,567"
# -----------------------------------------------------------------
#
# CONVERT TO CHARACTER
	z <- as.character(x)
#
# HANDLE NEGATIVES
	y <- ifelse(txt.left(z, 1) == "-", "-", "")
	z <- ifelse(txt.left(z, 1) == "-", txt.right(z, nchar(z) - 1), z)
#
# INSERT COMMAS
	n <- 3
	w <- nchar(z)
	while (any(w > n)) {
		z <- ifelse(w > n, paste(txt.left(z, w - n), txt.right(z, n), sep = ","), z)
		w <- w + ifelse(w > n, 1, 0)
		n <- n + 4
	}
#
# REPLACE SIGN
	z <- paste0(y, z)
#
# RETURN RESULT
	z
}
int.to.prime <- function(x) {
# -----------------------------------------------------------------
# Name		: int.to.prime
# Author	: VKS
# Date		: 3/22/18
# Args		: x = an integer
# Output	: prime factors of <x>
# -----------------------------------------------------------------
	n <- floor(sqrt(x))
	while (n > 1 & x %% n > 0) n <- n - 1
	if (n == 1) z <- x else z <- z <- c(int.to.prime(n), int.to.prime(x/n))
	z <- z[order(z)]
	z
}
isin.exists <- function(x) {
# -----------------------------------------------------------------
# Name		: isin.exists
# Author	: VKS
# Date		: 3/24/20
# Args		: x = string vector
# Output	: T/F depending on whether each element is an isin
# -----------------------------------------------------------------
#
# PRELIMINARIES
	charset <- vec.named(0:35, c(0:9, char.seq("A", "Z")))
	x <- toupper(txt.trim(x))
	z <- !is.na(x) & nchar(x) == 12
	for (j in 1:11) z <- z & is.element(substring(x, j, j), names(charset))
	z <- z & is.element(substring(x, 12, 12), 0:9)
#
# SUBSTITUTE INTEGERS FOR CHARACTERS
	y <- x[z]
	y <- y[!duplicated(y)]
	y <- matrix(NA, length(y), 11, F, list(y, char.seq("A", "K")))
	for (j in 1:dim(y)[2]) y[, j] <- as.numeric(map.rname(charset, substring(dimnames(y)[[1]], j, j)))
	y <- mat.ex.matrix(y)
	y <- vec.named(do.call(paste0, y), dimnames(y)[[1]])
#
# DOUBLE EVERY OTHER INTEGER
	y <- split(y, names(y))
	y <- lapply(y, function(x) as.numeric(txt.to.char(x)))
	y <- lapply(y, function(x) x * rep(2:1, ceiling(length(x)/2))[seq_along(x)])
	y <- sapply(y, function(x) sum(as.numeric(txt.to.char(paste(x, collapse = "")))))
#
# EXPECTED CHECK DIGIT
	y <- 10 * ceiling(y/10) - y
	y <- txt.right(names(y), 1) == y
#
# MAP TO ORIGINAL ROW SPACE
	z[z] <- as.logical(y[x[z]])
#
# RETURN RESULT
	z
}
knapsack.count <- function(x, y) {
# -----------------------------------------------------------------
# Name		: knapsack.count
# Author	: VKS
# Date		: 3/26/19,3/27
# Args		: x = a non-negative integer
#		: y = a positive integer
# Output	: number of ways to subdivide <x> things amongst <y> people
# -----------------------------------------------------------------
	z <- matrix(1, x + 1, y, F, list(0:x, 1:y))
	if (x > 0 & y > 1) for (i in 1:x) for (j in 2:y) z[i + 1, j] <- z[i, j] + z[i + 1, j - 1]
	z <- z[x + 1, y]
	#
	# RETURN RESULT
	z
}
knapsack.ex.int <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: knapsack.ex.int
# Author	: VKS
# Date		: 3/27/19
# Args		: x = a positive integer
#		: y = a positive integer
#		: n = a positive integer
# Output	: inverse of knapsack.to.int; returns a vector of length <n>,
#		:	the elements of which sum to <y>
# -----------------------------------------------------------------
	z <- NULL
	while (x != 1) {
		x <- x - 1
		i <- 0
		while (x > 0) {
			i <- i + 1
			h <- knapsack.count(i, n - 1)
			x <- x - h
		}
		z <- c(y - i, z)
		x <- x + h
		y <- y - z[1]
		n <- n - 1
	}
	z <- c(rep(0, n - 1), y, z)
#
# RETURN RESULT
	z
}
knapsack.next <- function(x) {
# -----------------------------------------------------------------
# Name		: knapsack.next
# Author	: VKS
# Date		: 3/20/19
# Args		: x = a vector of non-negative integers
# Output	: next way to subdivide <sum(x)> things amongst <length(x)> people
# -----------------------------------------------------------------
	m <- length(x)
	#
	# FIRST NON-ZERO
	w <- x > 0
	w <- w & !duplicated(w)
	#
	# CREATE RESULT
	if (w[1]) {
		n <- x[1]
		x[1] <- 0
		w <- x > 0
		w <- w & !duplicated(w)
		x[(1:m)[w] - 1:0] <- x[(1:m)[w] - 1:0] + c(1 + n, -1)
	} else {
		x[(1:m)[w] - 1:0] <- x[(1:m)[w] - 1:0] + c(1, -1)
	}
	z <- x
	#
	# RETURN RESULT
	z
}
knapsack.prev <- function(x) {
# -----------------------------------------------------------------
# Name		: knapsack.prev
# Author	: VKS
# Date		: 3/25/19
# Args		: x = a vector of non-negative integers
# Output	: inverse of knapsack.next
# -----------------------------------------------------------------
	m <- length(x)
	#
	# FIRST NON-ZERO
	w <- x > 0
	w <- w & !duplicated(w)
	w <- (1:m)[w]
	#
	# CREATE RESULT
	if (x[w] == 1 | w == 1) {
		x[w + 0:1] <- x[w + 0:1] + c(-1, 1)
	} else {
		x[c(1, w + 0:1)] <- x[c(1, w + 0:1)] + c(x[w] - 1, -x[w], 1)
	}
	z <- x
	#
	# RETURN RESULT
	z
}
knapsack.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: knapsack.to.int
# Author	: VKS
# Date		: 3/27/19
# Args		: x = a vector of non-negative integers
# Output	: maps each particular way to subdivide <sum(x)> things
#		:	amongst <length(x)> people to the number line
# -----------------------------------------------------------------
	n <- sum(x)
	z <- 1
	m <- length(x) - 1
	while (m > 0) {
		i <- sum(x[1:m])
		while (i > 0) {
			z <- z + knapsack.count(i - 1, m)
			i <- i - 1
		}
		m <- m - 1
	}
	#
	# RETURN RESULT
	z
}
latin.ex.arabic <- function(x) {
# -----------------------------------------------------------------
# Name		: latin.ex.arabic
# Author	: VKS
# Date		: 8/21/17
# Args		: x = a numeric vector
# Output	: returns <x> expressed as lower-case latin numerals
# -----------------------------------------------------------------
	y <- latin.to.arabic.underlying()
#
# HANDLE INPUTS
	x <- as.numeric(x)
	w <- is.na(x) | x < 0 | round(x) != x
	z <- rep("", length(x))
#
# CREATE RESULT
	if (all(!w)) {
		for (i in names(y)) {
			w <- x >= y[i]
			while (any(w)) {
				z[w] <- paste0(z[w], i)
				x[w] <- x[w] - y[i]
				w <- x >= y[i]
			}
		}
	} else z[!w] <- latin.ex.arabic(x[!w])
#
# RETURN RESULT
	z
}
latin.to.arabic <- function(x) {
# -----------------------------------------------------------------
# Name		: latin.to.arabic
# Author	: VKS
# Date		: 8/21/17
# Args		: x = a character vector of latin numerals
# Output	: returns <x> expressed as an integer
# -----------------------------------------------------------------
	y <- latin.to.arabic.underlying()
#
# HANDLE INPUTS
	x <- as.character(x)
	x <- txt.trim(x)
	x <- ifelse(is.na(x), "NA", x)
	x <- tolower(x)
	w <- x
	for (i in names(y)) w <- txt.replace(w, i, "")
	w <- w == ""
#
# CREATE RESULT
	if (all(w)) {
		z <- rep(0, length(x))
		for (i in names(y)) {
			n <- nchar(i)
			w <- txt.left(x, n) == i
			while (any(w)) {
				z[w] <- z[w] + as.numeric(y[i])
				x[w] <- txt.right(x[w], nchar(x[w]) - n)
				w <- txt.left(x, n) == i
			}
		}
	} else {
		z <- rep(NA, length(x))
		z[w] <- latin.to.arabic(x[w])
	}
#
# RETURN RESULT
	z
}
latin.to.arabic.underlying <- function() {
# -----------------------------------------------------------------
# Name		: latin.to.arabic.underlying
# Author	: VKS
# Date		: 8/21/17
# Args		: none
# Output	: basic map of latin to arabic numerals
# -----------------------------------------------------------------
	z <- c(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
	names(z) <- c("m", "cm", "d", "cd", "c", "xc", "l", "xl", "x", "ix", "v", "iv", "i")
	z
}
load.dy.vbl <- function(beg, end, mk.fcn, optional.args, vbl.name, out.fldr, env) {
# -----------------------------------------------------------------
# Name		: load.dy.vbl
# Author	: VKS
# Date		: 10/26/16,11/29,1/23/18,1/23/19
# Args		: beg = a single YYYYMMDD
#		: end = a single YYYYMMDD
#		: mk.fcn = a function
#		: optional.args = passed down to <mk.fcn>
#		: vbl.name = name under which the variable is to be stored
#		: out.fldr = R-object folder
#		: env = stock-flows environment
# Output	: Loads a daily variable
# -----------------------------------------------------------------
	load.dy.vbl.underlying(beg, end, mk.fcn, optional.args, vbl.name, out.fldr, env, yyyymmdd.to.yyyymm, load.dy.vbl.1obj)
	invisible()
}
load.dy.vbl.1obj <- function(beg, end, mk.fcn, optional.args, vbl.name, mo, env) {
# -----------------------------------------------------------------
# Name		: load.dy.vbl.1obj
# Author	: VKS
# Date		: 11/29/16,4/15/19
# Args		: beg = a single YYYYMMDD
#		: end = a single YYYYMMDD
#		: mk.fcn = a function
#		: optional.args = passed down to <mk.fcn>
#		: vbl.name = name under which the variable is to be stored
#		: mo = the YYYYMM for which the object is to be made
#		: env = stock-flows environment
# Output	: Loads a daily variable
# -----------------------------------------------------------------
#
# CREATE RESULT
	z <- flowdate.ex.yyyymm(mo, F)
	z <- paste(vbl.name, txt.right(z, 2), sep = ".")
	z <- matrix(NA, dim(env$classif)[1], length(z), F, list(dimnames(env$classif)[[1]], z))
#
# DETERMINE WHICH DAYS TO COMPUTE FOR
	dd <- txt.right(dimnames(z)[[2]], 2)
	dd <- dd[as.numeric(paste0(mo, dd)) >= as.numeric(beg)]
	dd <- dd[as.numeric(paste0(mo, dd)) <= as.numeric(end)]
#
# POPULATE RESULT
	for (i in dd) {
		cat(i, "")
		z[, paste(vbl.name, i, sep = ".")] <- mk.fcn(paste0(mo, i), optional.args, env)
	}
	z <- mat.ex.matrix(z)
#
# RETURN RESULT
	z
}
load.dy.vbl.underlying <- function(beg, end, mk.fcn, optional.args, vbl.name, out.fldr, env, fcn.conv, fcn.load) {
# -----------------------------------------------------------------
# Name		: load.dy.vbl.underlying
# Author	: VKS
# Date		: 10/26/16,11/29,1/23/18,1/23/19
# Args		: beg = a single YYYYMMDD
#		: end = a single YYYYMMDD
#		: mk.fcn = a function
#		: optional.args = passed down to <mk.fcn>
#		: vbl.name = name under which the variable is to be stored
#		: out.fldr = R-object folder
#		: env = stock-flows environment
#		: fcn.conv = conversion from period of columns to period of objects
#		: fcn.load = function to load one object
# Output	: Loads a variable
# -----------------------------------------------------------------
	for (mo in yyyymm.seq(fcn.conv(beg), fcn.conv(end))) {
		cat(mo, ":")
		z <- fcn.load(beg, end, mk.fcn, optional.args, vbl.name, mo, env)
		saveRDS(z, file = paste(out.fldr, paste(vbl.name, mo, "r", sep = "."), sep = "\\"), ascii = T)
		cat("\n")
	}
	invisible()
}
load.mo.vbl <- function(beg, end, mk.fcn, optional.args, vbl.name, out.fldr, env) {
# -----------------------------------------------------------------
# Name		: load.mo.vbl
# Author	: VKS
# Date		: 10/21/16,11/29,1/23/18,1/23/19
# Args		: beg = a single YYYYMM
#		: end = a single YYYYMM
#		: mk.fcn = a function
#		: optional.args = passed down to <mk.fcn>
#		: vbl.name = name under which the variable is to be stored
#		: out.fldr = R-object folder
#		: env = stock-flows environment
# Output	: Loads a monthly variable
# -----------------------------------------------------------------
	load.dy.vbl.underlying(beg, end, mk.fcn, optional.args, vbl.name, out.fldr, env, yyyymm.to.yyyy, load.mo.vbl.1obj)
	invisible()
}
load.mo.vbl.1obj <- function(beg, end, mk.fcn, optional.args, vbl.name, yyyy, env) {
# -----------------------------------------------------------------
# Name		: load.mo.vbl.1obj
# Author	: VKS
# Date		: 11/29/16,1/23/18
# Args		: beg = a single YYYYMM
#		: end = a single YYYYMM
#		: mk.fcn = a function
#		: optional.args = passed down to <mk.fcn>
#		: vbl.name = name under which the variable is to be stored
#		: yyyy = the period for which the object is to be made
#		: env = stock-flows environment
# Output	: Loads a monthly variable
# -----------------------------------------------------------------
#
# CREATE RESULT
	z <- paste(vbl.name, 1:12, sep = ".")
	z <- matrix(NA, dim(env$classif)[1], length(z), F, list(dimnames(env$classif)[[1]], z))
#
# DETERMINE WHICH MONTHS TO COMPUTE FOR
	mm <- 1:12
	mm <- mm[100 * yyyy + mm >= beg]
	mm <- mm[100 * yyyy + mm <= end]
#
# POPULATE RESULT
	for (i in mm) {
		cat(i, "")
		z[, paste(vbl.name, i, sep = ".")] <- mk.fcn(as.character(100 * yyyy + i), optional.args, env)
	}
	z <- mat.ex.matrix(z)
#
# RETURN RESULT
	z
}
machine.info <- function(x) {
# -----------------------------------------------------------------
# Name		: machine.info
# Author	: VKS
# Date		: 2/4/20
# Args		: x = a column in the classif-Machines file
# Output	: folder of function source file
# -----------------------------------------------------------------
	mat.read(parameters("classif-Machines"), "\t")[Sys.info()[["nodename"]], x]
}
map.classif <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: map.classif
# Author	: VKS
# Date		: 2/10/17,3/28,3/7/18,1/4/19,1/25/19
# Args		: x = a named vector
#		: y = <classif>
#		: n = something like "isin" or "HSId"
# Output	: Maps data to the row space of <y>
# -----------------------------------------------------------------
#
# COLUMNS OF INTEREST
	z <- vec.to.list(intersect(c(n, paste0(n, 1:5)), dimnames(y)[[2]]))
#
# DEFINE MAPPING FUNCTION
	fcn <- function(i) as.numeric(map.rname(x, y[, i]))
#
# CREATE RESULT
	z <- avail(sapply(z, fcn))
#
# RETURN RESULT
	z
}
map.rname <- function(x, y) {
# -----------------------------------------------------------------
# Name		: map.rname
# Author	: VKS
# Date		: 9/8/16,10/27,12/21/17,1/23/18,3/5
# Args		: x = a vector/matrix/data-frame
#		: y = a vector (usually string)
# Output	: returns a matrix/df, the row names of which match up with <y>
# -----------------------------------------------------------------
	if (is.null(dim(x))) {
		z <- vec.named(, y)
		w <- is.element(y, names(x))
		if (any(w)) z[w] <- x[names(z)[w]]
	} else {
		w <- !is.element(y, dimnames(x)[[1]])
		if (any(w)) {
			y.loc <- matrix(NA, sum(w), dim(x)[2], F, list(y[w], dimnames(x)[[2]]))
			x <- rbind(x, y.loc)
		}
		if (dim(x)[2] == 1) {
			z <- matrix(x[as.character(y), 1], length(y), 1, F, list(y, dimnames(x)[[2]]))
		} else z <- x[as.character(y), ]
	}
	z
}
mat.compound <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.compound
# Author	: VKS
# Date		: 11/16/17,12/15
# Args		: x = a matrix/df of percentage returns
# Output	: Compounds across the rows
# -----------------------------------------------------------------
	fcn.mat.num(compound, x, , F)
}
mat.correl <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.correl
# Author	: VKS
# Date		: 9/23/16,12/15/17
# Args		: x = a vector/matrix/data-frame
#		: y = an isomekic vector or isomekic isoplatic matrix/data-frame
# Output	: Returns the correlation of <x> & <y> if <x> is a vector or those between the rows of <x> and <y> otherwise
# -----------------------------------------------------------------
	fcn.mat.num(correl, x, y, F)
}
mat.count <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.count
# Author	: VKS
# Date		: 8/10/17,10/6,1/22/18
# Args		: x = a matrix/df
# Output	: counts observations of the columns of <x>
# -----------------------------------------------------------------
	fcn <- function(x) sum(!is.na(x))
	z <- fcn.mat.num(fcn, x, , T)
	z <- c(z, round(100 * z/dim(x)[1], 1))
	z <- matrix(z, dim(x)[2], 2, F, list(dimnames(x)[[2]], c("obs", "pct")))
	z
}
mat.daily.to.monthly <- function(x, y = F) {
# -----------------------------------------------------------------
# Name		: mat.daily.to.monthly
# Author	: VKS
# Date		: 5/19/17,10/27,2/15/18,4/12/19
# Args		: x = a matrix/df of daily data
#		: y = T/F depending on whether data points must be from month ends
# Output	: returns latest data in each month indexed by <yyyymm> ascending
# -----------------------------------------------------------------
#
# LATEST DATA IN A MONTH
	z <- x[order(dimnames(x)[[1]], decreasing = T), ]
	z <- z[!duplicated(yyyymmdd.to.yyyymm(dimnames(z)[[1]])), ]
#
# REQUIRE MONTH ENDS
	if (y)	{
		w <- yyyymmdd.to.yyyymm(dimnames(z)[[1]])
		w <- yyyymmdd.ex.yyyymm(w)
		w <- w == dimnames(z)[[1]]
		z <- z[w, ]
	}
#
# CREATE RESULT
	dimnames(z)[[1]] <- yyyymmdd.to.yyyymm(dimnames(z)[[1]])
	z <- mat.reverse(z)
#
# RETURN RESULT
	z
}
mat.daily.to.weekly <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.daily.to.weekly
# Author	: VKS
# Date		: 4/12/19
# Args		: x = a matrix/df of daily data
#		: y = an integer representing the day the week ends on
#		:	0 is Sun, 1 is Mon, ..., 6 is Sat
# Output	: returns latest data in each week in ascending order
# -----------------------------------------------------------------
#
# LATEST DATA IN A WEEK
	z <- x[order(dimnames(x)[[1]], decreasing = T), ]
	z <- z[!duplicated(day.to.week(dimnames(z)[[1]], y)), ]
#
# CREATE RESULT
	dimnames(z)[[1]] <- day.to.week(dimnames(z)[[1]], y)
	z <- mat.reverse(z)
#
# RETURN RESULT
	z
}
mat.diff <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.diff
# Author	: VKS
# Date		: 12/22/22
# Args		: x = a matrix/df
#		: y = a non-negative integer
# Output	: difference between <x> and itself lagged <y>
# -----------------------------------------------------------------
	fcn.mat.vec(function(x) vec.diff(x, y), x,, T)
}
mat.ex.array <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.ex.array
# Author	: VKS
# Date		: 9/21/16,1/9/19,1/25,1/30,6/13,10/21/22
# Args		: x = an array
# Output	: a data frame with the first dimension forming the column space
# Notes		: order of other dimensions reversed to display row names properly
# -----------------------------------------------------------------
	apply(x, 1, function(x) mat.index(array.unlist(x), length(dim(x)):1))
}
mat.ex.matrix <- function(x, y = NULL) {
# -----------------------------------------------------------------
# Name		: mat.ex.matrix
# Author	: VKS
# Date		: 11/3/17,2/15/18
# Args		: x = a matrix
#		: y = desired row names (defaults to NULL)
# Output	: converts into a data frame
# -----------------------------------------------------------------
	as.data.frame(x, row.names = y, stringsAsFactors = F)
}
mat.ex.vec <- function(x, y, n = T) {
# -----------------------------------------------------------------
# Name		: mat.ex.vec
# Author	: VKS
# Date		: 10/11/17,12/20,1/16/18,4/4,1/24/19,12/11/20
# Args		: x = a numeric or character vector
#		: y = an isomekic vector of associated values
#		: n = T/F depending on whether "Q" is to be appended to column headers
# Output	: transforms into a 1/0 matrix of bin memberships if <y>
#		: is missing or the values of <y> otherwise
# -----------------------------------------------------------------
#
# PRELIMINARIES
	if (!is.null(names(x))) w <- names(x) else w <- seq_along(x)
	if (n) x <- paste0("Q", x)
#
# CREATE RESULT
	z <- data.frame(w, x, y, stringsAsFactors = F)
	z <- reshape.wide(z)
#
# RETURN RESULT
	z
}
mat.fake <- function() {
# -----------------------------------------------------------------
# Name		: mat.fake
# Author	: VKS
# Date		: 12/15/17,4/24/20
# Args		: none
# Output	: Returns a data frame for testing purposes
# -----------------------------------------------------------------
	mat.ex.matrix(matrix(sample(35), 7, 5, F, list(1:7, char.ex.int(64 + 1:5))))
}
mat.index <- function(x, y = 1, n = T) {
# -----------------------------------------------------------------
# Name		: mat.index
# Author	: VKS
# Date		: 12/15/17,1/8/19,1/18/19
# Args		: x = a matrix/df
#		: y = columns
#		: n = T/F depending on whether you remove columns <y>
# Output	: indexes <x> by, and, if <n>, removes, columns <y>
# -----------------------------------------------------------------
#
# NAMES OR POSITIONS?
	if (all(is.element(y, 1:dim(x)[2]))) {
		w <- is.element(1:dim(x)[2], y)
	} else {
		w <- is.element(dimnames(x)[[2]], y)
	}
#
# ROW NAMES
	if (sum(w) > 1) z <- do.call(paste, mat.ex.matrix(x)[, y]) else z <- x[, w]
#
# BASIC CHECKS
	if (any(is.na(z))) stop("NA's in row indices ...")
	if (any(duplicated(z))) stop("Duplicated row indices ...")
#
# PERFORM INDEXING
	if (!n) {
		dimnames(x)[[1]] <- z
		z <- x
	} else if (sum(!w) > 1) {
		dimnames(x)[[1]] <- z
		z <- x[, !w]
	} else {
		z <- vec.named(x[, !w], z)
	}
#
# RETURN RESULT
	z
}
mat.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.lag
# Author	: VKS
# Date		: 9/8/16,6/30/17,7/19,4/12/19,12/9/20,10/25/22
# Args		: x = a matrix/df indexed by time running FORWARDS
#		: y = number of periods over which to lag
# Output	: Returns data lagged <y> periods with the same row space as <x>
# -----------------------------------------------------------------
	if (is.null(dim(x))) vec.lag(x, y) else fcn.mat.vec(vec.lag, x, y, T)
}
mat.last.to.first <- function(x, y = 1) {
# -----------------------------------------------------------------
# Name		: mat.last.to.first
# Author	: VKS
# Date		: 12/6/16,4/29/19
# Args		: x = a matrix/df
#		: y = a non-negative integer
# Output	: Re-orders so the last <y> columns come first
# -----------------------------------------------------------------
	x[, order((1:dim(x)[2] + y - 1) %% dim(x)[2])]
}
mat.rank <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.rank
# Author	: VKS
# Date		: 9/23/16,12/15/17
# Args		: x = a vector/matrix/data-frame
# Output	: ranks <x> if <x> is a vector or the rows of <x> otherwise
# -----------------------------------------------------------------
	fcn <- function(x) fcn.nonNA(rank, -x)
	z <- fcn.mat.vec(fcn, x, , F)
	z
}
mat.read <- function(x = "C:\\temp\\write.csv", y = ",", n = 1, w = T, h = "") {
# -----------------------------------------------------------------
# Name		: mat.read
# Author	: VKS
# Date		: 9/8/16,9/15,9/29,10/19,11/14,11/22,2/1/18,4/25,3/6/20
# Args		: x = a path to a text file
#		: y = the separator
#		: n = the column containing the row names (or NULL if none)
#		: w = T/F variable depending on whether <x> has a header
#		: h = the set of quoting characters. Defaults to no quoting altogether
# Output	: reads the file into data frame
# -----------------------------------------------------------------
#
# IF MISSING SEPARATORS TRY STANDARD ONES
	if (missing(y)) y <- c("\t", ",")
	if (is.null(n)) adj <- 0:1 else adj <- rep(0, 2)
#
# RAISE ERROR IF THE FILE DOESN'T EXIST
	if (!file.exists(x)) stop("File ", x, " doesn't exist!\n")
#
# READ FILE
	u <- length(y)
	z <- read.table(x, w, y[u], row.names = n, quote = h, as.is = T, na.strings = txt.na(), comment.char = "", check.names = F)
	while (min(dim(z) - adj) == 0 & u > 1) {
		u <- u - 1
		z <- read.table(x, w, y[u], row.names = n, quote = h, as.is = T, na.strings = txt.na(), comment.char = "", check.names = F)
	}
#
# RETURN RESULT
	z
}
mat.reverse <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.reverse
# Author	: VKS
# Date		: 2/15/18
# Args		: x = a matrix/data-frame
# Output	: reverses row order
# -----------------------------------------------------------------
	x[dim(x)[1]:1, ]
}
mat.rollsum <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.rollsum
# Author	: VKS
# Date		: 12/22/22
# Args		: x = a matrix/df
#		: y = a non-negative integer
# Output	: rolling sum of <n> rows
# -----------------------------------------------------------------
	fcn.mat.vec(function(x) vec.diff(vec.cum(x), y)[-1], x,, T)
}
mat.same <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.same
# Author	: VKS
# Date		: 1/24/18
# Args		: x = a matrix/df
#		: y = an isomekic isoplatic matrix/df
# Output	: T/F depending on whether <x> and <y> are identical
# -----------------------------------------------------------------
	all(fcn.mat.num(vec.same, x, y, T))
}
mat.sort <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mat.sort
# Author	: VKS
# Date		: 4/2/20
# Args		: x = a matrix/df
#		: y = string vector of column names of <x>
#		: n = logical vector of the same length as <y>
# Output	: sorts <x> by <y> in decreasing order if <n> is T
# -----------------------------------------------------------------
#
# SORT BY THE LEAST IMPORTANT ELEMENTS FIRST
	w <- length(y)
	while (w > 0) {
		x <- x[order(x[, y[w]], decreasing = n[w]), ]
		w <- w - 1
	}
	z <- x
#
# RETURN RESULT
	z
}
mat.subset <- function(x, y) {
# -----------------------------------------------------------------
# Name		: mat.subset
# Author	: VKS
# Date		: 1/16/18,12/18
# Args		: x = a matrix/df
#		: y = a vector
# Output	: <x> subset to <y>
# -----------------------------------------------------------------
	w <- is.element(y, dimnames(x)[[2]])
#
# SUBSET
	if (any(!w)) {
		err.raise(y[!w], F, "Warning: The following columns are missing")
		z <- t(map.rname(t(x), y))
	} else if (length(y) == 1) {
		z <- vec.named(x[, y], dimnames(x)[[1]])
	} else {
		z <- x[, y]
	}
#
# RETURN RESULT
	z
}
mat.to.last.Idx <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.to.last.Idx
# Author	: VKS
# Date		: 12/15/17
# Args		: x = a matrix/df
# Output	: the last row index for which we have data
# -----------------------------------------------------------------
	z <- dimnames(x)[[1]][dim(x)[1]]
	cat("Original data had", dim(x)[1], "rows ending at", z, "...\n")
	z
}
mat.to.obs <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.to.obs
# Author	: VKS
# Date		: 9/26/16,12/15/17,1/18/19,1/24/19
# Args		: x = a vector/matrix/dataframe
# Output	: Returns 0 if <x> is NA or 1 otherwise.
# -----------------------------------------------------------------
	fcn <- function(x) as.numeric(!is.na(x))
	z <- fcn.mat.vec(fcn, x,, T)
	z
}
mat.to.xlModel <- function(x, y = 2, n = 5, w = F) {
# -----------------------------------------------------------------
# Name		: mat.to.xlModel
# Author	: VKS
# Date		: 7/19/17,11/27,2/15/18
# Args		: x = a data frame indexed by data dates or trade open dates
#		: y = number of days needed for flow data to be known
#		: n = return horizon in weekdays
#		: w = T/F depending on whether the index is data or trade-open date
# Output	: prepends the trade open and close dates and re-indexes by data date (as needed)
# -----------------------------------------------------------------
#
# DATE MATRIX
	z <- c("Open", "Close")
	z <- matrix(NA, dim(x)[1], length(z), F, list(dimnames(x)[[1]], z))
	if (w) z[, "Open"] <- yyyymm.lag(dimnames(z)[[1]], -y)
	if (!w) {
		z[, "Open"] <- dimnames(z)[[1]]
		dimnames(z)[[1]] <- yyyymm.lag(z[, "Open"], y)
	}
	z[, "Close"] <- yyyymm.lag(z[, "Open"], -n)
#
# ISSUE WARNING WHEN YOU ARE NOT TRADING FRIDAY TO FRIDAY
	if (all(nchar(dimnames(x)[[1]]) == 8)) {
		if (any(day.to.weekday(z[, "Open"]) != "5") | any(day.to.weekday(z[, "Close"]) != "5")) {
			cat("WARNING: YOU ARE NOT TRADING FRIDAY TO FRIDAY!\n")
		}
	}
#
# PREPEND
	z <- cbind(z, x)
#
# ENSURE DATES DESCEND
	z <- z[order(dimnames(z)[[1]], decreasing = T), ]
#
# RETURN RESULT
	z
}
mat.weekly.to.daily <- function(x) {
# -----------------------------------------------------------------
# Name		: mat.weekly.to.daily
# Author	: VKS
# Date		: 10/23/22
# Args		: x = a matrix/df of daily data
# Output	: daily file having latest weekly data known by each flow date
# -----------------------------------------------------------------
#
# LABEL BY THE LAST FLOW DATE FALLING WITHIN EACH WEEK
	h <- dimnames(x)[[1]]
	w <- flowdate.exists(h)
	while (any(!w)) {
		h[!w] <- yyyymmdd.lag(h[!w], 1)
		w <- flowdate.exists(h)
	}
	dimnames(x)[[1]] <- h
#
# FALL BACK FOR OTHER FLOW DATES
	z <- flowdate.seq(min(h), max(h))
	h <- approx(h, h, z, method = "constant")[["y"]]
	x <- map.rname(x, h)
	dimnames(x)[[1]] <- z
	z <- x
#
# RETURN RESULT
	z
}
mat.write <- function(x, y, n = ",", w = T) {
# -----------------------------------------------------------------
# Name		: mat.write
# Author	: VKS
# Date		: 9/9/16,9/27,11/10,8/22/19,2/4/20,11/11
# Args		: x = any matrix/df
#		: y = file intended to receive the output
#		: n = the separator
#		: w = T/F depending on whether to write row names
# Output	: Writes <x> as a <n>-separated file to <y>
# -----------------------------------------------------------------
#
# DEFAULT ARGUMENTS
	if (missing(y)) y <- paste(machine.info("temp"), "write.csv", sep = "\\")
#
# WRITE OUTPUT
	if (w) {
		write.table(x, y, sep = n, quote = F, col.names = NA)
	} else {
		write.table(x, y, sep = n, quote = F, col.names = T, row.names = F)
	}
#
# RETURN NOTHING
	invisible()
}
mat.zScore <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mat.zScore
# Author	: VKS
# Date		: 11/3/16,12/20/18
# Args		: x = a vector/matrix/data-frame
#		: y = a 1/0 membership vector
#		: n = a vector of groups (e.g. GSec)
# Output	: zScores <x> within groups <n> using weights <y>
# -----------------------------------------------------------------
#
# DECIDE IF YOU'RE DEALING WITH A VECTOR
	h <- is.null(dim(x))
	if (h) {
		m <- length(x)
		z <- rep(NA, m)
	} else {
		m <- dim(x)[1]
		z <- matrix(NA, m, dim(x)[2], F, dimnames(x))
	}
#
# MISSING ARGUMENTS
	if (missing(y)) y <- rep(1, m)
	if (missing(n)) n <- rep(1, m)
	y <- is.element(y, 1)
#
# CREATE RESULT
	w <- !is.na(n)
	x <- data.frame(x, y, stringsAsFactors = F)
	x <- fcn.vec.grp(zScore.underlying, x[w, ], n[w])
	if (any(w) & h) {
		z[w] <- x
	} else {
		z[w, ] <- unlist(x)
	}
#
# RETURN RESULT
	z
}
maturity.bucket <- function(x) {
# -----------------------------------------------------------------
# Name		: maturity.bucket
# Author	: VKS
# Date		: 6/15/22
# Args		: x = named numeric vector
# Output	: where clauses for SQL case statement
# -----------------------------------------------------------------
	x <- x[order(x)]
	x <- vec.named(paste("v >=", x, "and v <", c(x[-1], "?")), names(x))
	x[length(x)] <- txt.left(x[length(x)], nchar(x[length(x)]) - nchar(" and v < ?"))
	z <- txt.replace(x, "v", "datediff(day, @date, BondMaturity)")
	z
}
mk.1dFloMo <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.1dFloMo
# Author	: VKS
# Date		: 10/26/16,10/27,11/8,11/28,3/2/17,3/7,5/22,5/31,6/1,6/16,8/14,
#		: 9/14,10/2,1/23/18,4/11,5/15,5/16,6/18,7/16,8/20,10/9,10/11,
#		: 10/22,11/2,4/15/19,12/3/20,3/14/21
# Args		: x = a single YYYYMMDD
#		: y = a string vector of variables to build with the last elements
#		:	specifying the type of funds to use
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
#		:	c) DB - any of StockFlows/China/Japan/CSI300/Energy
# Output	: Returns a flow variable with the same row space as <n>
# -----------------------------------------------------------------
#
# LAG <x> BY 2 DAYS TO ACCOUNT FOR THE FLOW DATA DELAY
	x <- flowdate.lag(x, 2)
#
# SQL QUERY
	if (any(y[1] == c("FwtdIn0", "FwtdEx0", "SwtdIn0", "SwtdEx0"))) {
		z <- sql.1dFloMoAggr(x, y, n$DB) # ONLY DOES ALL!
	} else if (any(y[1] == c("ION$", "ION%"))) {
		z <- sql.1dION(x, y, 26, n$DB)
	} else stop("Bad Argument")
#
# GET DATA FROM THE SERVER
	z <- sql.map.classif(z, n$conn, n$classif)
#
# RETURN RESULT
	z
}
mk.1dFloMo.Ctry.rslt <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.1dFloMo.Ctry.rslt
# Author	: VKS
# Date		: 11/10/21
# Args		: x = item (one of Flow/AssetsStart/AssetsEnd)
#		: y = flow momentum output
#		: n = named vector of country codes indexed by CountryId
# Output	: formats flow momentum output
# -----------------------------------------------------------------
	x <- split(x, x)
	for (j in names(x)) {
		x[[j]] <- reshape.wide(y[, c(dimnames(y)[[2]][1:2], j)])
		x[[j]] <- map.rname(t(x[[j]]), names(n))
		x[[j]] <- aggregate(x = x[[j]], by = list(grp = n), FUN = sum)
		x[[j]] <- matrix(unlist(x[[j]][, -1]), dim(x[[j]])[1], dim(x[[j]])[2] - 1, F, list(x[[j]][, 1], dimnames(x[[j]])[[2]][-1]))
	}
	if (length(names(x)) == 1) z <- x[[1]] else z <- simplify2array(x)
#
# RETURN RESULT
	z
}
mk.1dFloMo.Sec.rslt <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: mk.1dFloMo.Sec.rslt
# Author	: VKS
# Date		: 11/24/21
# Args		: x = item (one of Flow/AssetsStart/AssetsEnd)
#		: y = flow momentum output
#		: n = named vector of sector codes indexed by SectorId
#		: w = T/F depending on whether daily/weekly
#		: h = IndustryId/SectorId
# Output	: formats flow momentum output
# -----------------------------------------------------------------
	x <- split(x, x)
	for (j in names(x)) {
		x[[j]] <- reshape.wide(y[, c(ifelse(w, "DayEnding", "WeekEnding"), h, j)])
		x[[j]] <- map.rname(t(x[[j]]), names(n))
		dimnames(x[[j]])[[1]] <- as.character(n)
	}
	if (length(names(x)) == 1) z <- x[[1]] else z <- simplify2array(x)
#
# RETURN RESULT
	z
}
mk.1mAllocMo <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.1mAllocMo
# Author	: VKS
# Date		: 10/28/16,11/8,11/14,3/2/17,3/10,6/16,6/20,1/23/18,6/18,
#		: 7/16,8/20,10/9,10/24,11/2,3/5/19,3/15,4/1,4/4,9/27,
#		: 5/7/20,6/5,9/4,11/10/20,12/3,2/17/21,3/14,5/11,8/26/21
# Args		: x = a single YYYYMM
#		: y = a string vector of variables to build with the last elements
#		:	specifying the type of funds to use
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
#		:	c) DB - any of StockFlows/China/Japan/CSI300/Energy
# Output	: Returns a flow variable with the same row space as <n>
# -----------------------------------------------------------------
#
# LAG <x> BY 1 MONTH TO ACCOUNT FOR THE 30 DAY DELAY IN KNOWING ALLOCATIONS
	x <- yyyymm.lag(x, 1)
#
# SQL QUERY
	if (y[1] == "AllocSkew") {
		z <- sql.1mAllocSkew(x, y, n$DB, F)
	} else if (y[1] == "ActWtIncrPct") {
		z <- sql.1mActWtIncrPct(x, y, n$DB, F)
	} else if (y[1] == "SRIAdvisorPct") {
		z <- sql.1mSRIAdvisorPct(x, y, n$DB, F)
	} else if (y[1] == "FloDollar") {
		z <- sql.1mFloMo(x, y, n$DB, F, "All")
	} else if (y[1] == "Bullish") {
		z <- sql.Bullish(x, y, n$DB, F)
	} else if (y[1] == "Dispersion") {
		z <- sql.Dispersion(x, y, n$DB, F)
	} else if (any(y[1] == c("Herfindahl", "HerfindahlEq", "FundCt"))) {
		z <- sql.Herfindahl(x, y, n$DB, F, "All")
	} else if (y[1] == "HoldSum") {
		z <- sql.1mFundCt(x, y, n$DB, F, "All")
	} else if (any(y[1] == c("AllocDInc", "AllocDDec", "AllocDAdd", "AllocDRem"))) {
		z <- sql.1mAllocD(x, y, n$DB, F, F)
	} else if (any(y[1] == c("FwtdEx0", "FwtdIn0", "SwtdEx0", "SwtdIn0"))) {
		z <- sql.TopDownAllocs(x, y, n$DB, F, "All")
	} else if (any(y[1] == paste0("Alloc", c("Mo", "Trend", "Diff")))) {
		z <- sql.1mAllocMo(x, y, n$DB, F)
	} else {
		z <- sql.1mFloMo(x, y, n$DB, F, "All")
	}
#
# GET DATA FROM THE SERVER
	z <- sql.map.classif(z, n$conn, n$classif)
#
# RETURN RESULT
	z
}
mk.1mPerfTrend <- function(x, y, n, w = F) {
# -----------------------------------------------------------------
# Name		: mk.1mPerfTrend
# Author	: VKS
# Date		: 8/4/17,1/9/18,1/11,1/12,1/23,1/24,4/11,10/8,5/11/21
# Args		: x = a single YYYYMM
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
#		:	c) uiconn - a connection to EPFRUI, the output of odbcDriverConnect
#		:	d) DB - any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Returns a variable with the same row space as <n>
# Notes		: Needs a separate connection to the EPFRUI database called <uiconn>
# Import	: @importFrom RODBC sqlQuery
# -----------------------------------------------------------------
#
# PRELIMINARY CHECKS
	vbls <- paste0("Perf", txt.expand(c("", "ActWt"), c("Trend", "Diff", "Diff2"), ""))
	y <- sql.arguments(y)
	if (length(y$factor) != 1) stop("Too many factors!")
	if (!is.element(y$factor, vbls)) stop("Factors must be one of", paste(vbls, collapse = "\\"))
#
# LAG <x> BY 1 MONTH TO ACCOUNT FOR THE 30 DAY DELAY IN KNOWING ALLOCATIONS
	if (!w) x <- yyyymm.lag(x, 1)
#
# EPFRUI DATA
	ui <- c("MonthlyData t1", "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
	h <- c("FundId", "GeographicFocus = max(GeographicFocus)")
	h <- c(h, "FundRet = sum(PortfolioChange)/sum(AssetsStart)")
	ui <- sql.tbl(h, ui, "MonthEnding = @newDt", "FundId", "sum(AssetsStart) > 0")
	ui <- c(sql.declare("@newDt", "datetime", yyyymm.to.day(x)), "", sql.unbracket(ui))
	ui <- sql.query.underlying(paste(ui, collapse = "\n"), n$uiconn, F)
#
# STOCK FLOWS DATA
	if (is.element(y$factor, vbls[1:3])) {
		sf <- ifelse(w, "n1.HSecurityId", "n1.SecurityId")
		sf <- c(sf, "his.FundId", "WtCol = n1.HoldingValue/AssetsEnd - o1.HoldingValue/AssetsStart")
		u <- sql.1mAllocMo.underlying.pre(y$filter, yyyymm.to.day(x), yyyymm.to.day(yyyymm.lag(x)))
		h <- sql.1mAllocMo.underlying.from(y$filter)
		if (!w) h <- c(h, "inner join", "SecurityHistory id on id.HSecurityId = n1.HSecurityId")
		if (n$DB == "All") {
			sf <- sql.unbracket(sql.tbl(sf, h))
		} else {
			sf <- sql.unbracket(sql.tbl(sf, h, sql.in("n1.HSecurityId", sql.RDSuniv(n$DB))))
		}
		sf <- c(paste(u, collapse = "\n"), paste(sf, collapse = "\n"))
	} else {
		sf <- sql.FundHistory(y$filter, T, c("FundId", "GeographicFocusId"))
		sf <- c(sql.label(sf, "his"), "\ton his.HFundId = t.HFundId")
		sf <- c(sql.label(sql.MonthlyAssetsEnd("@newDt"), "t"), "inner join", sf)
		sf <- c(sf, "inner join", sql.label(sql.MonthlyAlloc("@newDt"), "n1"), "\ton n1.FundId = his.FundId")
		if (!w) sf <- c(sf, "inner join", "SecurityHistory id on id.HSecurityId = n1.HSecurityId")
		h <- ifelse(w, "n1.HSecurityId", "SecurityId")
		h <- c(h, "his.FundId", "GeographicFocusId", "WtCol = HoldingValue/AssetsEnd")
		if (n$DB == "All") {
			sf <- sql.tbl(h, sf)
		} else {
			sf <- sql.tbl(h, sf, sql.in("n1.HSecurityId", sql.RDSuniv(n$DB)))
		}
		sf <- c(sql.declare("@newDt", "datetime", yyyymm.to.day(x)), "", sql.unbracket(sf))
		sf <- paste(sf, collapse = "\n")
	}
	sf <- sql.query.underlying(sf, n$conn, F)
#
# COMMON ROW SPACE
	ui <- ui[is.element(ui[, "FundId"], sf[, "FundId"]), ]
	sf <- sf[is.element(sf[, "FundId"], ui[, "FundId"]), ]
#
# ACTIVE RETURNS VERSUS EQUAL-WEIGHT GEOID AVERAGE
	ui[, "FundRet"] <- ui[, "FundRet"] - map.rname(pivot.1d(mean, ui[, "GeographicFocus"], ui[, "FundRet"]), ui[, "GeographicFocus"])
	ui <- mat.index(ui, "FundId")
	ui <- as.numeric(map.rname(ui, sf[, "FundId"])[, "FundRet"])
#
# IF REQUIRED, COMPUTE ACTIVE WEIGHTS (EQUAL-WEIGHT EXCLUDE-ZERO APPROACH)
	if (any(is.element(y, vbls[4:6]))) {
		vec <- paste(sf[, 1], sf[, "GeographicFocusId"])
		vec <- pivot.1d(mean, vec, sf[, "WtCol"])
		vec <- as.numeric(map.rname(vec, paste(sf[, 1], sf[, "GeographicFocusId"])))
		sf[, "WtCol"] <- sf[, "WtCol"] - vec
	}
#
# CREATE RESULT
	z <- ui
	if (is.element(y$factor, c("PerfDiff2", "PerfActWtDiff2"))) z <- sign(z)
	if (is.element(y$factor, c("PerfDiff", "PerfActWtDiff"))) {
		z <- z * sign(sf[, "WtCol"])
	} else {
		z <- z * sf[, "WtCol"]
	}
	num <- pivot.1d(sum, sf[, 1], z)
	den <- pivot.1d(sum, sf[, 1], abs(z))
	if (w) {
		den <- den[den > 0]
		num <- num[names(den)]
		z <- list(HSecurityId = names(den))
		z[[y$factor]] <- num/den
		z[["ReportDate"]] <- rep(yyyymmdd.to.txt(yyyymm.to.day(x)), length(z[[1]]))
		z <- mat.ex.matrix(z)[, c("ReportDate", "HSecurityId", y$factor)]
	} else {
		z <- map.rname(den, dimnames(n$classif)[[1]])
		z <- nonneg(z)
		z <- map.rname(num, dimnames(n$classif)[[1]])/z
		z <- as.numeric(z)
	}
#
# RETURN RESULT
	z
}
mk.ActWt <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.ActWt
# Author	: VKS
# Date		: 1/27/17,1/23/18
# Args		: x = a single YYYYMM
#		: y = a string vector of names of the portfolio and benchmark
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Active weight
# -----------------------------------------------------------------
#
# FETCH LATEST PORTFOLIO
	z <- fetch(y[1], x, 1, paste(n$fldr, "data", sep = "\\"), n$classif)
#
# FETCH BENCHMARK FROM THE PREVIOUS MONTH (PORTFOLIO HAS A ONE MONTH DELAY INCORPORATED)
	w <- fetch(y[2], yyyymm.lag(x), 1, paste(n$fldr, "data", sep = "\\"), n$classif)
#
# COMPUTE ACTIVE WEIGHT
	z <- z - w
#
# RETURN RESULT
	z
}
mk.Alpha <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.Alpha
# Author	: VKS
# Date		: 11/4/16,1/23/18,3/13,12/20
# Args		: x = a single YYYYMM
#		: y = a string vector, the first two elements of which are
#		:	universe and group to zScore on and within. This is then followed by
#		:	a list of variables which are, in turn, followed by weights to put on variables
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: makes Alpha
# -----------------------------------------------------------------
#
# PARSE ARGS
	m <- length(y)
	if (m %% 2 != 0) stop("Bad Arguments")
	univ <- y[1]
	grp.nm <- y[2]
	vbls <- y[seq(3, m/2 + 1)]
	wts <- renorm(as.numeric(y[seq(m/2 + 2, m)]))/100
#
# FETCH VBLS
	z <- fetch(vbls, x, 1, paste(n$fldr, "derived", sep = "\\"), n$classif)
	grp <- n$classif[, grp.nm]
	mem <- fetch(univ, x, 1, paste0(n$fldr, "\\data"), n$classif)
#
# zSCORE
	z <- mat.zScore(z, mem, grp)
#
# CREATE RESULT
	z <- zav(z)
	z <- as.matrix(z)
	z <- z %*% wts
	z <- as.numeric(z)
#
# RETURN RESULT
	z
}
mk.Alpha.daily <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.Alpha.daily
# Author	: VKS
# Date		: 3/15/18,12/20
# Args		: x = a single YYYYMMDD
#		: y = a string vector, the first two elements of which are
#		:	universe and group to zScore on and within. This is then followed by
#		:	a list of variables which are, in turn, followed by weights to put on variables
#		:	and a logical vector indicating whether the variables are daily.
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: makes Alpha
# -----------------------------------------------------------------
#
# PARSE ARGS
	m <- length(y)
	if ((m - 2) %% 3 != 0) stop("Bad Arguments")
	univ <- y[1]
	grp.nm <- y[2]
	wts <- renorm(as.numeric(y[seq((m + 7)/3, (2 * m + 2)/3)]))/100
	vbls <- vec.named(as.logical(y[seq((2 * m + 5)/3, m)]), y[seq(3, (m + 4)/3)])
	vbls[univ] <- F
#
# FETCH DAILY VBLS
	z <- matrix(NA, dim(n$classif)[1], length(vbls), F, list(dimnames(n$classif)[[1]], names(vbls)))
	for (i in names(vbls)) {
		if (vbls[i]) x.loc <- x else x.loc <- yyyymm.lag(yyyymmdd.to.yyyymm(x))
		if (i == univ) sub.fldr <- "data" else sub.fldr <- "derived"
		z[, i] <- fetch(i, x.loc, 1, paste(n$fldr, sub.fldr, sep = "\\"), n$classif)
	}
	z <- mat.ex.matrix(z)
	z$grp <- n$classif[, grp.nm]
#
# zSCORE
	vbls <- setdiff(names(vbls), univ)
	z <- mat.zScore(z[, vbls], z[, univ], z$grp)
#
# CREATE RESULT
	z <- zav(z)
	z <- as.matrix(z)
	z <- z %*% wts
	z <- as.numeric(z)
#
# RETURN RESULT
	z
}
mk.avail <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.avail
# Author	: VKS
# Date		: 11/10/16,1/23/18
# Args		: x = a single YYYYMM or YYYYMMDD
#		: y = a string vector, the elements of which are:
#		:	1) folder to fetch data from
#		:	2+) variables to fetch
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Returns leftmost non-NA variable
# -----------------------------------------------------------------
	avail(fetch(y[-1], x, 1, paste(n$fldr, y[1], sep = "\\"), n$classif))
}
mk.beta <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.beta
# Author	: VKS
# Date		: 12/27/16,1/23/18,4/4,4/25
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are:
#		:	1) benchmark (e.g. "Eafe")
#		:	2) number of trailing months of returns (e.g. 12)
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Computes monthly beta versus relevant benchmark
# -----------------------------------------------------------------
#
# ARGUMENTS
	m <- as.numeric(y[2])
	univ <- y[1]
#
# MONTHLY INDEX RETURNS
	w <- paste(dir.parameters("csv"), "IndexReturns-Monthly.csv", sep = "\\")
	w <- mat.read(w, ",")
#
# BETA
	z <- fetch("Ret", x, m, paste(n$fldr, "data", sep = "\\"), n$classif)
	vec <- map.rname(w, yyyymm.lag(x, m:1 - 1))[, univ]
	vec <- matrix(c(rep(1, m), vec), m, 2, F, list(1:m, c("Intercept", univ)))
	z <- run.cs.reg(z, vec)
	z <- as.numeric(z[, univ])
#
# RETURN RESULT
	z
}
mk.EigenCentrality <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.EigenCentrality
# Author	: VKS
# Date		: 10/28/19,10/29
# Args		: x = a single YYYYMM
#		: y = a string vector of variables to build with the last elements
#		:	specifying the type of funds to use
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
#		:	c) DB - any of StockFlows/China/Japan/CSI300/Energy
# Output	: Returns EigenCentrality with the same row space as <n>
# -----------------------------------------------------------------
#
# LAG <x> BY 1 MONTH TO ACCOUNT FOR THE 30 DAY DELAY IN KNOWING ALLOCATIONS
	x <- yyyymm.lag(x, 1)
#
# SQL QUERY
	x <- sql.declare("@floDt", "datetime", yyyymm.to.day(x))
	z <- sql.and(list(A = "ReportDate = @floDt", B = sql.in("t1.HSecurityId", sql.RDSuniv(n[["DB"]]))))
	h <- c("Holdings t1", "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	z <- c(x, sql.unbracket(sql.tbl("HFundId, SecurityId", h, z, "HFundId, SecurityId")))
	z <- paste(z, collapse = "\n")
#
# GET DATA FROM THE SERVER
	x <- sql.query.underlying(z, n$conn, F)
#
# READ IN DATA
	x <- x[is.element(x[, "SecurityId"], dimnames(n$classif)[[1]]), ] # SUBSET
	x <- split(x[, "HFundId"], x[, "SecurityId"]) # SPLIT BY SECURITY
	w <- Reduce(union, x) # UNIQUE FUNDS
	x <- sapply(x, function(x) is.element(w, x)) # MATRIX
	dimnames(x)[[1]] <- w # FUND NAMES
	x <- crossprod(x) # NAME OVERLAPS
	w <- diag(x) > 9
	x <- x[w, w] # THROW OUT SPARSELY-HELD SECURITIES
	w <- order(diag(x))
	x <- x[w, w] # SORT ON HOW WIDELY A SECURITY IS HELD
#
# ADJACENCY MATRIX
	w <- floor(dim(x)[2]/50) # NUMBER OF BUCKETS
	w <- qtl.fast(diag(x), w) # SIZE BUCKETS
	diag(x) <- NA
	z <- matrix(F, dim(x)[1], dim(x)[2], F, dimnames(x))
	for (j in 1:max(w)) {
		for (k in 1:max(w)) {
			y <- x[w == j, w == k]
			y <- as.numeric(unlist(y))
			y[!is.na(y)] <- is.element(qtl.fast(y[!is.na(y)], 20), 1)
			y[is.na(y)] <- F
			z[w == j, w == k] <- as.logical(y)
		}
	}
#
# LARGEST EIGEN VECTOR
	x <- rep(1, dim(z)[1])
	x <- x/sqrt(sum(x^2))
	y <- z %*% x
	y <- y/sqrt(sum(y^2))
	while (sqrt(sum((y - x)^2)) > 1e-6) {
		x <- y
		y <- z %*% x
		y <- y/sqrt(sum(y^2))
	}
	z <- dim(z)[1] * y
	z <- as.numeric(map.rname(z, dimnames(n[["classif"]])[[1]]))
#
# RETURN RESULT
	z
}
mk.FloAlphaLt.Ctry <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.FloAlphaLt.Ctry
# Author	: VKS
# Date		: 12/5/16,1/23/18
# Args		: x = a single YYYYMM
#		: y = an object name (preceded by #) or the path to a ".csv" file
#		: n = list object containing the following items:
#		:	a) classif - classif file
# Output	: Monthly Country Flow Alpha
# Notes		: the file read in has DAILY prices
# -----------------------------------------------------------------
	z <- read.prcRet(y)
	z <- unlist(z[yyyymmdd.ex.yyyymm(x), ])
	z <- map.rname(z, n$classif$CCode)
	z <- as.numeric(z)
	z
}
mk.Fragility <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.Fragility
# Author	: VKS
# Date		: 2/11/19,2/28,4/16,4/25
# Args		: x = a single YYYYMM
#		: y = vector containing the following items:
#		:	a) folder - where the underlying data live
#		:	b) trail - number of return periods to use
#		:	c) factors - number of eigenvectors to use
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Generates the fragility measure set forth in
#		:	Greenwood & Thesmar (2011) "Stock Price Fragility"
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	trail <- as.numeric(y[2])
	eigen <- as.numeric(y[3])
	y <- y[1]
#
# LAG <x> BY 1 MONTH TO ACCOUNT FOR THE 26-DAY DELAY IN KNOWING ALLOCATIONS
	x <- yyyymm.lag(x)
#
# READ IN FLOWS
	h <- readRDS(paste(y, "FlowPct.r", sep = "\\"))
	h <- t(h[, yyyymm.lag(x, trail:1 - 1)])
#
# READ IN HOLDING VALUES
	x <- readRDS(paste0(y, "\\HoldingValue-", x, ".r"))
#
# COVARIANCE MATRIX
	h <- h[, mat.count(h)[, 1] == trail & is.element(dimnames(h)[[2]], dimnames(x)[[2]])]
	h <- principal.components.covar(h, eigen)
#
# SUBSET TO FUNDS WITH COVARIANCES AND SECURITIES IN CLASSIF
	x <- x[is.element(dimnames(x)[[1]], dimnames(n$classif)[[1]]), is.element(dimnames(x)[[2]], dimnames(h)[[1]])]
	h <- h[is.element(dimnames(h)[[1]], dimnames(x)[[2]]), ]
	h <- h[, dimnames(h)[[1]]]
#
# COMPUTE FRAGILITY
	h <- tcrossprod(h, x)
	z <- colSums(t(x) * h)
	x <- rowSums(x)^2
	z <- z/nonneg(x)
	z <- as.numeric(map.rname(z, dimnames(n$classif)[[1]]))
#
# RETURN RESULT
	z
}
mk.FundsMem <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.FundsMem
# Author	: VKS
# Date		: 1/26/17,3/30,1/23/18
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are:
#		:	1) column to match in classif (e.g. "FundType")
#		:	2) column value (e.g. "E" or "B")
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Returns a 1/0 vector with the same row space as <n> that is 1 whenever
#		:	it has the right fund type as well as one-month forward return.
# -----------------------------------------------------------------
	w <- is.element(n[, y[1]], y[2])
	z <- fetch("Ret", yyyymm.lag(x, -1), 1, paste(n$fldr, "data", sep = "\\"), n$classif)
	z <- w & !is.na(z)
	z <- as.numeric(z)
	z
}
mk.HerdingLSV <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.HerdingLSV
# Author	: VKS
# Date		: 2/7/19,2/8
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are:
#		:	1) file to read from
#		:	2) variable to compute (LSV/DIR)
#		: n = list object containing the following items:
#		:	a) fldr - stock-flows folder
# Output	: Generates the herding measure set forth in LSV's 1991 paper
#		:	"Do institutional investors destabilize stock prices?"
# -----------------------------------------------------------------
#
# READ IN DATA
	x <- paste0(n$fldr, "\\sqlDump\\", y[1], ".", x, ".r")
	x <- readRDS(x)[, c("B", "S", "expPctBuy")]
#
# EXPECTED PROBABILITY THAT A FUND BUYS A STOCK
	u <- x[, "expPctBuy"]
	u <- u[!is.na(u)][1]
#
# NUMBER OF MANAGERS TRADING A STOCK
	n <- rowSums(x[, c("B", "S")])
	h <- vec.unique(nonneg(n))
#
# CREATE RESULT
	z <- rep(NA, length(n))
	for (i in h) {
		w <- is.element(n, i)
		if (y[2] == "LSV") {
			z[w] <- abs(x[w, "B"]/n[w] - u) - sum(abs(0:i/i - u) * dbinom(0:i, i, u))
		} else if (y[2] == "DIR") {
			w2 <- w & x[, "B"] >= x[, "S"]
			if (any(w2)) z[w2] <- pbinom(x[w2, "B"] - 1, i, u) # LIKELIHOOD OF FEWER
			if (any(w & !w2)) z[w & !w2] <- -pbinom(x[w & !w2, "B"], i, u, F) # LIKELIHOOD OF MORE (SIGN FLIPPED)
		} else {
			stop("Bad <y> argument!")
		}
	}
#
# RETURN RESULT
	z
}
mk.isin <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.isin
# Author	: VKS
# Date		: 12/19/16,2/10/17,3/2,3/29,1/23/18
# Args		: x = a single YYYYMM or YYYYMMDD
#		: y = a string vector, the elements of which are:
#		:	1) an object name (preceded by #) or the path to a ".csv" file
#		:	2) defaults to "isin"
#		: n = list object containing the following items:
#		:	a) classif - classif file
# Output	: Looks up date from external file and maps on isin
# -----------------------------------------------------------------
	if (length(y) == 1) y <- c(y, "isin")
	z <- read.prcRet(y[1])
	z <- vec.named(z[, x], dimnames(z)[[1]])
	z <- map.classif(z, n[["classif"]], y[2])
	z
}
mk.JensensAlpha.fund <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.JensensAlpha.fund
# Author	: VKS
# Date		: 11/15/17,11/20,12/21,1/23/18,1/18/19
# Args		: x = a single YYYYMM
#		: y = number of months of trailing returns to use
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
#		:	c) CATRETS - category returns
# Output	: Returns variable with the same row space as <n>
# -----------------------------------------------------------------
	y <- as.numeric(y)
#
# READ IN DATA (USE 1mPrcMo, NOT Ret, TO REFLECT 1-MONTH DELAY)
	fndR <- fetch("1mPrcMo", x, y, paste(n$fldr, "derived", sep = "\\"), n$classif)
	fndR <- as.matrix(fndR)
	dimnames(fndR)[[2]] <- yyyymm.lag(x, y:1 - 1)
#
# CATEGORY RETURNS
	catR <- n$CATRETS[, dimnames(fndR)[[2]]]
	w <- as.logical(apply(mat.to.obs(cbind(fndR, catR)), 1, min))
#
# RUN REGRESSIONS IF NEEDED
	z <- rep(NA, dim(fndR)[1])
	if (any(w)) {
		fndM <- rowMeans(fndR[w, ])
		catM <- rowMeans(catR[w, ])
		beta <- rowSums((catR[w, ] - catM) * (catR[w, ] - catM))
		beta <- rowSums((fndR[w, ] - fndM) * (catR[w, ] - catM))/nonneg(beta)
		z[w] <- fndM - beta * catM
	}
#
# RETURN RESULT
	z
}
mk.Mem <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.Mem
# Author	: VKS
# Date		: 10/21/16,10/5/17,1/9/18,1/10,1/11,1/12,1/23,
#		: 4/24/20,3/14/21
# Args		: x = a single YYYYMM
#		: y = a vector of FundId
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
# Output	: Returns a 1/0 membership vector
# Notes		: unlike others, this function does not introduce a lag
# -----------------------------------------------------------------
#
# SQL QUERY
	y <- sql.in("FundId", paste0("(", paste(y, collapse = ", "), ")"))
	y <- sql.and(list(A = y, B = "ReportDate = @mo"))
	z <- c("Holdings t1", "inner join", "SecurityHistory t2 on t1.HSecurityId = t2.HSecurityId")
	z <- sql.unbracket(sql.tbl("SecurityId, Mem = sign(max(HoldingValue))", z, y, "SecurityId"))
	z <- paste(c(sql.declare("@mo", "datetime", yyyymm.to.day(x)), z), collapse = "\n")
#
# GET DATA FROM THE SERVER
	z <- sql.map.classif(z, n$conn, n$classif)
	z <- zav(z)
#
# RETURN RESULT
	z
}
mk.SatoMem <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.SatoMem
# Author	: VKS
# Date		: 11/14/18,1/25/19,12/25/20
# Args		: x = an argument which is never used
#		: y = path to a file containing isin's
#		: n = list object containing the following items:
#		:	a) classif - classif file
# Output	: Returns a 1/0 membership vector
# -----------------------------------------------------------------
	n <- n[["classif"]]
#
# READ IN FILE
	y <- readLines(y)
#
# COLUMNS OF INTEREST
	z <- vec.to.list(intersect(c("isin", paste0("isin", 1:5)), dimnames(n)[[2]]))
#
# DEFINE MAPPING FUNCTION
	fcn <- function(i) is.element(n[, i], y)
#
# CREATE RESULT
	z <- sapply(z, fcn)
	z <- as.numeric(apply(z, 1, max))
#
# RETURN RESULT
	z
}
mk.sqlDump <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.sqlDump
# Author	: VKS
# Date		: 11/25/16,3/2/17,1/23/18,4/15/19
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are:
#		:	1) file to read from
#		:	2) variable to read
#		:	3) lag (defaults to zero)
#		: n = list object containing the following items:
#		:	a) fldr - stock-flows folder
# Output	: Returns variable with the same row space as <n>
# -----------------------------------------------------------------
	if (length(y) > 2) x <- yyyymm.lag(x, as.numeric(y[3], F))
	z <- paste0(n$fldr, "\\sqlDump\\", y[1], ".", x, ".r")
	z <- readRDS(z)
	z <- z[, y[2]]
	z
}
mk.SRIMem <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.SRIMem
# Author	: VKS
# Date		: 2/7/19,3/14/21
# Args		: x = a single YYYYMM
#		: y = a positive integer
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
#		:	c) DB - any of StockFlows/China/Japan/CSI300/Energy
# Output	: 1/0 depending on whether <y> or more SRI funds own the stock
# -----------------------------------------------------------------
	x <- yyyymm.lag(x)
	x <- sql.SRI(x, n$DB)
	z <- sql.map.classif(x, n$conn, n$classif)
	z <- as.numeric(!is.na(z) & z >= y)
	z
}
mk.vbl.chg <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.chg
# Author	: VKS
# Date		: 1/27/17,1/23/18
# Args		: x = a single YYYYMM
#		: y = variable name
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Makes the MoM change in the variable
# -----------------------------------------------------------------
	z <- fetch(y, x, 2, paste(n$fldr, "data", sep = "\\"), n$classif)
	z <- z[, 2] - z[, 1]
	z
}
mk.vbl.diff <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.diff
# Author	: VKS
# Date		: 11/27/17,1/23/18
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are the variables
#		:	being subtracted and subtracted from respectively.
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Computes the difference of the two variables
# -----------------------------------------------------------------
	z <- fetch(y, x, 1, paste(n$fldr, "data", sep = "\\"), n$classif)
	z <- z[, 1] - z[, 2]
	z
}
mk.vbl.lag <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.lag
# Author	: VKS
# Date		: 2/15/17,1/23/18
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are:
#		:	1) the variable to be lagged
#		:	2) the lag in months
#		:	3) the sub-folder in which the variable lives
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Lags the variable
# -----------------------------------------------------------------
	x <- yyyymm.lag(x, as.numeric(y[2]))
	z <- fetch(y[1], x, 1, paste(n$fldr, y[3], sep = "\\"), n$classif)
	z
}
mk.vbl.max <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.max
# Author	: VKS
# Date		: 9/6/17,1/23/18
# Args		: x = a single YYYYMM
#		: y = a string vector of names of two variables
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Computes the maximum of the two variables
# -----------------------------------------------------------------
	z <- fetch(y, x, 1, paste(n$fldr, "data", sep = "\\"), n$classif)
	z <- vec.max(z[, 1], z[, 2])
	z
}
mk.vbl.ratio <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.ratio
# Author	: VKS
# Date		: 2/14/17,11/27,1/23/18,3/23/21
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are the
#		:	numerator and denominator respectively.
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Computes the ratio of the two variables
# -----------------------------------------------------------------
	z <- fetch(y, x, 1, paste(n$fldr, "data", sep = "\\"), n$classif)
	z <- zav(z[, 1])/nonneg(z[, 2])
	z
}
mk.vbl.scale <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.scale
# Author	: VKS
# Date		: 2/15/19
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are:
#		:	1) the variable to be scaled
#		:	2) the secondary variable
#		:	3) the universe within which to scale
#		:	4) the grouping within which to scale
#		:	5) scaling factor on top decile
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Linearly scales the first variable based on percentiles of the second.
#		:	Top decile goes to scaling factor. Bot decile is fixed.
# -----------------------------------------------------------------
#
# FETCH DATA
	w <- is.element(fetch(y[3], x, 1, paste(n$fldr, "data", sep = "\\"), n$classif), 1)
	h <- n$classif[, y[4]]
	x <- fetch(y[1:2], x, 1, paste(n$fldr, "derived", sep = "\\"), n$classif)
	y <- as.numeric(y[5])
#
# PERCENTILE
	x[w, 2] <- 1 - fcn.vec.grp(ptile, x[w, 2], h[w])/100
	x[w, 2] <- ifelse(is.na(x[w, 2]), 0.5, x[w, 2])
#
# CREATE RESULT
	z <- rep(NA, dim(x)[1])
	z[w] <- (x[w, 2] * 5 * (1 - y)/4 + (9 * y - 1)/8) * x[w, 1]
#
# RETURN RESULT
	z
}
mk.vbl.sum <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.sum
# Author	: VKS
# Date		: 9/27/19
# Args		: x = a single YYYYMM
#		: y = a string vector, the elements of which are the variables
#		:	to be added.
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: Computes the sum of the two variables
# -----------------------------------------------------------------
	z <- fetch(y, x, 1, paste(n$fldr, "data", sep = "\\"), n$classif)
	z <- z[, 1] + z[, 2]
	z
}
mk.vbl.trail.fetch <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.trail.fetch
# Author	: VKS
# Date		: 12/2/16,1/23/18,4/4,4/5,4/6
# Args		: x = a single YYYYMM or YYYYMMDD
#		: y = a string vector, the elements of which are:
#		:	1) variable to fetch (e.g. "AllocMo"/"AllocDiff"/"AllocTrend"/"Ret")
#		:	2) number of trailing periods to use (e.g. "11")
#		:	3) number of periods to lag (defaults to "0")
#		:	4) sub-folder to fetch basic variable from (defaults to "derived")
#		:	5) T/F depending on whether the compounded variable is daily (defaults to F, matters only if <x> is monthly)
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: compounded variable over some trailing window
# -----------------------------------------------------------------
#
# DEFAULTS
	if (length(y) == 2) y <- c(y, 0, "derived", F)
	if (length(y) == 3) y <- c(y, "derived", F)
	if (length(y) == 4) y <- c(y, F)
#
# EXPAND WINDOW TO ACCOUNT FOR LAG
	m <- as.numeric(y[2])
	trail <- m + as.numeric(y[3])
#
# DATE TO FETCH DATA FROM
	if (nchar(x) == 6 & as.logical(y[5])) x <- yyyymmdd.ex.yyyymm(x)
#
# CREATE RESULT
	z <- fetch(y[1], x, trail, paste(n$fldr, y[4], sep = "\\"), n$classif)
	z <- z[, 1:m]
#
# RETURN RESULT
	z
}
mk.vbl.trail.sum <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.trail.sum
# Author	: VKS
# Date		: 12/2/16,1/23/18,4/4,4/5,4/6
# Args		: x = a single YYYYMM or YYYYMMDD
#		: y = a string vector, the elements of which are:
#		:	1) variable to fetch (e.g. "1mAllocMo"/"1dAllocDiff"/"1dAllocTrend"/"Ret")
#		:	2) T to sum or F to compound (e.g. "T")
#		:	3) number of trailing periods to use (e.g. "11")
#		:	4) number of periods to lag (defaults to "0")
#		:	5) sub-folder to fetch basic variable from (defaults to "derived")
#		:	6) T/F depending on whether the compounded variable is daily (defaults to F, matters only if <x> is monthly)
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: compounded variable over some trailing window
# -----------------------------------------------------------------
	z <- mk.vbl.trail.fetch(x, y[-2], n)
	z <- compound.sf(z, as.logical(y[2]))
	z <- as.numeric(z)
	z
}
mk.vbl.vol <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.vbl.vol
# Author	: VKS
# Date		: 4/5/18,4/6
# Args		: x = a single YYYYMM or YYYYMMDD
#		: y = a string vector, the elements of which are:
#		:	1) variable to fetch (e.g. "AllocMo"/"AllocDiff"/"AllocTrend"/"Ret")
#		:	2) number of trailing periods to use (e.g. "11")
#		:	3) number of periods to lag (defaults to "0")
#		:	4) sub-folder to fetch basic variable from (defaults to "derived")
#		:	5) T/F depending on whether the compounded variable is daily (defaults to F, matters only if <x> is monthly)
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) fldr - stock-flows folder
# Output	: volatility of variable over some trailing window
# -----------------------------------------------------------------
	z <- mk.vbl.trail.fetch(x, y, n)
	z <- apply(z, 1, sd)
	z <- as.numeric(z)
	z
}
mk.Wt <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: mk.Wt
# Author	: VKS
# Date		: 1/27/17,1/9/18,1/12,1/23,4/5,3/14/21
# Args		: x = a single YYYYMM
#		: y = FundId of the fund of interest
#		: n = list object containing the following items:
#		:	a) classif - classif file
#		:	b) conn - a connection, the output of odbcDriverConnect
# Output	: Generates the SQL query to get monthly index weight for individual stocks
# -----------------------------------------------------------------
#
# SQL QUERY
	y <- sql.and(list(A = sql.in("t1.HFundId", sql.tbl("HFundId", "FundHistory", paste("FundId =", y))), B = "ReportDate = @mo"))
	z <- c("Holdings t1", "inner join", sql.label(sql.MonthlyAssetsEnd("@mo"), "t3"), "\ton t1.HFundId = t3.HFundId")
	z <- c(z, "inner join", "SecurityHistory t2 on t1.HSecurityId = t2.HSecurityId")
	z <- sql.unbracket(sql.tbl("SecurityId, Wt = 100 * HoldingValue/AssetsEnd", z, y))
	z <- paste(c(sql.declare("@mo", "datetime", yyyymm.to.day(x)), z), collapse = "\n")
#
# GET DATA FROM THE SERVER
	z <- sql.map.classif(z, n$conn, n$classif)
	z <- zav(z)
#
# RETURN RESULT
	z
}
multi.asset <- function(x) {
# -----------------------------------------------------------------
# Name		: multi.asset
# Author	: VKS
# Date		: 3/23/17,5/15,12/15,12/21,4/25/18,1/17/20
# Args		: x = a vector of paths to files
# Output	: Reads in data relevant to the multi-asset strategy
# -----------------------------------------------------------------
	x <- vec.to.list(x)
	x <- lapply(x, mat.read)
	z <- Reduce(function(x, y) mat.index(merge(x, y, by = 0)), x)
	z
}
nameTo <- function(x, y) {
# -----------------------------------------------------------------
# Name		: nameTo
# Author	: VKS
# Date		: 12/18/17
# Args		: x = a logical vector/matrix/dataframe without NA's
#		: y = a logical value, isomekic vector or isomekic isoplatic matrix/df without NA's
# Output	: pct name turnover between <x> and <y> if <x> is a vector or their rows otherwise
# -----------------------------------------------------------------
	fcn <- function(x, y) nameTo.underlying(sum(x), sum(y), sum(x & y))
	z <- fcn.mat.num(fcn, x, y, F)
	z
}
nameTo.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: nameTo.underlying
# Author	: VKS
# Date		: 10/21/16,12/18/17
# Args		: x = a vector of counts over the current period
#		: y = a vector of counts over the previous period
#		: n = a vector of numbers of names common between current and previous periods
# Output	: percent name turnover
# -----------------------------------------------------------------
	100 - 100 * n/max(x, y)
}
nonneg <- function(x) {
# -----------------------------------------------------------------
# Name		: nonneg
# Author	: VKS
# Date		: 10/31/16,12/15/17,1/24/19
# Args		: x = a vector/matrix/dataframe
# Output	: returns <x> if non-negative or NA otherwise
# -----------------------------------------------------------------
	fcn <- function(x) ifelse(!is.na(x) & x > 0, x, NA)
	z <- fcn.mat.vec(fcn, x,, T)
	z
}
nyse.holidays <- function(x = "yyyymmdd") {
# -----------------------------------------------------------------
# Name		: nyse.holidays
# Author	: VKS
# Date		: 11/22/16,3/15/17
# Args		: x = either "yyyymmdd" or "reason"
# Output	: returns full day NYSE holidays from the year 2000 and after
# -----------------------------------------------------------------
	z <- parameters("NyseHolidays")
	z <- scan(z, what = list(yyyymmdd = "", reason = ""), sep = "\t", quote = "", quiet = T)
	z <- z[[x]]
	z
}
obj.diff <- function(fcn, x, y) {
# -----------------------------------------------------------------
# Name		: obj.diff
# Author	: VKS
# Date		: 1/3/18
# Args		: fcn = a function mapping objects to the number line
#		: x = a vector
#		: y = an isomekic isotypic vector
# Output	: returns <x - y>
# -----------------------------------------------------------------
	fcn(x) - fcn(y)
}
obj.lag <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: obj.lag
# Author	: VKS
# Date		: 1/3/17
# Args		: x = a vector of objects
#		: y = an integer or vector of integers (if <x> and <y> are vectors then <y> isomekic)
#		: n = a function mapping these objects to the number line
#		: w = the bijective inverse of <n>
# Output	: lags <x> by <y>
# -----------------------------------------------------------------
	w(n(x) - y)
}
obj.seq <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: obj.seq
# Author	: VKS
# Date		: 1/3/17,1/12/18
# Args		: x = a SINGLE object
#		: y = a SINGLE object of the same type as <x>
#		: n = a function mapping these objects to the number line
#		: w = the bijective inverse of <n>
#		: h = a positive integer representing quantum size
# Output	: returns a sequence of objects between (and including) <x> and <y>
# -----------------------------------------------------------------
	x <- n(x)
	y <- n(y)
	if (x > y) z <- -h else z <- h
	z <- seq(x, y, z)
	z <- w(z)
	z
}
optimal <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: optimal
# Author	: VKS
# Date		: 5/20/19
# Args		: x = a matrix/df of indicators
#		: y = an isomekic isoplatic matrix/df containing associated forward returns
#		: n = an isoplatic matrix/df of daily returns on which to train the risk model
#		: w = a numeric vector, the elements of which are:
#		:	1) number of trailing days to train the risk model on
#		:	2) number of principal components (when 0 raw return matrix is used)
#		:	3) number of bins (when 0, indicator is ptiled)
#		:	4) forward return window in days or months depending on the row space of <x>
# Output	: Performance statistics of the optimal zero-cost unit-variance portfolio
# -----------------------------------------------------------------
#
# PRELIMINARIES
	period.count <- yyyy.periods.count(dimnames(x)[[1]])
#
# RANK THE VARIABLE
	if (w[3] > 0) {
		x <- qtl.eq(x, w[3])
		x <- (1 + w[3] - 2 * x)/(w[3] - 1)
		x <- ifelse(!is.na(x) & abs(x) < 1, 0, x)
	} else x <- ptile(x)
#
# LOOP OVER EACH PERIOD
	for (j in dimnames(x)[[1]]) {
	#
	# RISK MODEL
		if (period.count == 260) z <- j else z <- yyyymmdd.ex.yyyymm(j) # TRAINING-PERIOD END DATE
		z <- map.rname(n, flowdate.lag(z, w[1]:1 - 1)) # FETCH RISK-MODEL RETURNS
		z <- z[, mat.count(z)[, 1] == w[1] & !is.na(x[j, ])] # TOSS NA's
		if (w[2] != 0) {
			z <- principal.components.covar(z, w[2])
		} else {
			z <- covar(z)/(1 - 1/w[1] + 1/w[1]^2) # UNBIASED COVARIANCE ESTIMATOR
		}
	#
	# UNIT-VARIANCE ZERO-COST OPTIMAL PORTFOLIO
		opt <- solve(z) %*% map.rname(x[j, ], dimnames(z)[[2]]) # OPTIMAL PORTFOLIO
		unity <- solve(z) %*% rep(1, dim(z)[1]) # MUST BE PERPENDICULAR TO THIS VECTOR
		opt <- opt - unity * as.numeric(crossprod(opt, z) %*% unity)/as.numeric(crossprod(unity, z) %*% unity) # ZERO-COST OPTIMAL PORTFOLIO
		opt <- opt[, 1]/sqrt(260 * (crossprod(opt, z) %*% opt)[1, 1]) # UNIT-RISK ZERO-COST OPTIMAL PORTFOLIO
		x[j, ] <- zav(map.rname(opt, dimnames(x)[[2]]))
	}
#
# SUMMARIZE
	x <- rowSums(x * zav(y))
	y <- period.count/w[4]
	z <- vec.named(, c("AnnMn", "AnnSd", "Sharpe", "HitRate"))
	z["AnnMn"] <- mean(x) * y
	z["AnnSd"] <- sd(x) * sqrt(y)
	z["Sharpe"] <- 100 * z["AnnMn"]/z["AnnSd"]
	z["HitRate"] <- mean(sign(x)) * 50
	z <- z/100
#
# RETURN RESULT
	z
}
parameters <- function(x) {
# -----------------------------------------------------------------
# Name		: parameters
# Author	: VKS
# Date		: 3/15/17,12/15
# Args		: x = parameter type
# Output	: returns full path to relevant parameters file
# -----------------------------------------------------------------
	paste0(dir.parameters("parameters"), "\\", x, ".txt")
}
permutations <- function(x) {
# -----------------------------------------------------------------
# Name		: permutations
# Author	: VKS
# Date		: 1/29/18
# Args		: x = a string vector without NA's
# Output	: all possible permutations of <x>
# -----------------------------------------------------------------
#
# FIRST PERMUTATION
	h <- length(x)
	w <- 1:h
	z <- NULL
#
# ALL THE OTHERS
	while (!is.null(w)) {
		z <- c(z, paste(x[w], collapse = " "))
		w <- permutations.next(w)
	}
#
# RETURN RESULT
	z
}
permutations.next <- function(x) {
# -----------------------------------------------------------------
# Name		: permutations.next
# Author	: VKS
# Date		: 1/29/18
# Args		: x = a vector of integers 1:length(<x>) in some order
# Output	: returns the next permutation in dictionary order
# -----------------------------------------------------------------
#
# PRELIMINARIES
	z <- x
	n <- length(z)
#
# FIND SMALLEST RIGHT TAIL THAT ISN'T PERFECTLY REVERSE SORTED
	j <- n - 1
	while (z[j] > z[j + 1] & j > 1) j <- j - 1
#
# NEXT PERMUTATION
	if (z[j] > z[j + 1]) {
		z <- NULL
	} else {
		#
		# SWAP <z[j]> WITH SMALLEST ELEMENT IN THE TAIL THAT'S BIGGER
		k <- n
		while (z[j] > z[k]) k <- k - 1
		z <- vec.swap(z, j, k)
		#
		# SORT THE ELEMENTS IN THE TAIL IN ASCENDING ORDER
		r <- n
		s <- j + 1
		while (r > s) {
			z <- vec.swap(z, r, s)
			r <- r - 1
			s <- s + 1
		}
	}
#
# RETURN RESULT
	z
}
phone.list <- function(x = 4) {
# -----------------------------------------------------------------
# Name		: phone.list
# Author	: VKS
# Date		: 3/27/17,12/14,12/20
# Args		: x = number of desired columns
# Output	: Cat's phone list to the screen
# -----------------------------------------------------------------
#
# READ IN DATA
	y <- parameters("PhoneList")
	y <- mat.read(y, "\t", NULL, F)
	y <- paste(y[, 1], y[, 2], sep = "\t")
#
# PUT IN COLUMNS
	vec <- seq(0, length(y) - 1)
	z <- split(y, vec %% x)
	z[["sep"]] <- "\t\t"
	z <- do.call(paste, z)
	z <- paste(z, collapse = "\n")
	cat(z, "\n")
#
# RETURN NOTHING
	invisible()
}
pivot <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: pivot
# Author	: VKS
# Date		: 9/15/16,9/19,10/26
# Args		: fcn = summary function to be applied
#		: x = a numeric vector
#		: y = a grouping vector
#		: n = a grouping vector
# Output	: returns a table, the rows and columns of which are unique members of rowIdx and colIdx
#		: The cells of the table are the <fcn> of <x> whenever <y> and <n> take on their respective values
# -----------------------------------------------------------------
	z <- aggregate(x = x, by = list(row = y, col = n), FUN = fcn)
	z <- reshape.wide(z)
	z
}
pivot.1d <- function(fcn, x, y) {
# -----------------------------------------------------------------
# Name		: pivot.1d
# Author	: VKS
# Date		: 9/21/16,10/26,12/15/17,12/21
# Args		: fcn = summary function to be applied
#		: x = a grouping vector
#		: y = a numeric vector/matrix/data-frame
# Output	: returns a table, having the same column space of <x>, the rows of which are unique members of <grp>
#		: The cells of the table are the summ.fcn of <x> whenever <grp> takes on its respective value
# -----------------------------------------------------------------
	z <- aggregate(x = y, by = list(grp = x), FUN = fcn)
	z <- mat.index(z)
	z
}
plurality.map <- function(x, y) {
# -----------------------------------------------------------------
# Name		: plurality.map
# Author	: VKS
# Date		: 10/6/17,12/21,1/8/19
# Args		: x = a vector
#		: y = an isomekic vector
# Output	: returns a map from <x> to <y>
# -----------------------------------------------------------------
#
# TOSS NA's
	w <- !is.na(x) & !is.na(y)
	x <- x[w]
	y <- y[w]
#
# COUNT INCIDENCES
	z <- vec.count(paste(x, y))
	z <- data.frame(txt.parse(names(z), " "), z)
	names(z) <- c("x", "map", "obs")
#
# LOSE DUPLICATE MAPS
	z <- z[order(-z$obs), ]
	z <- z[!duplicated(z$x), ]
	z <- mat.index(z, "x")
#
# COMPUTE CONFIDENCE
	z$pct <- 100 * z$obs/map.rname(vec.count(x), dimnames(z)[[1]])
	z <- z[order(-z$pct), ]
#
# RETURN RESULT
	z
}
portfolio.beta.wrapper <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: portfolio.beta.wrapper
# Author	: VKS
# Date		: 10/29/18,11/26,12/20,1/25/19,10/22/22
# Args		: x = a file of total return indices indexed so that time runs forward
#		: y = the name of the benchmark w.r.t. which beta is to be computed (e.g. "ACWorld")
#		: n = the window in days over which beta is to be computed
# Output	: <n> day beta of columns of <x> with respect to benchmark <y>
# -----------------------------------------------------------------
#
# BENCHMARK TOTAL RETURN INDEX
	y <- map.rname(mat.read(paste(dir.parameters("csv"), "IndexReturns-Daily.csv", sep = "\\")), dimnames(x)[[1]])[, y]
	x[, "Benchmark"] <- y
#
# CONVERT TO DAILY RETURN
	z <- mat.ex.matrix(ret.ex.idx(x, 1, F, T))[-1, ]
#
# COMPUTE ROLLING <n>-PERIOD SUM
	z <- list(x = z, xy = z * z[, "Benchmark"])
	z <- lapply(z, function(x) mat.rollsum(x, n))
#
# COMPUTE ROLLING <n>-PERIOD BETA
	z <- z[["xy"]]/n - z[["x"]] * z[["x"]][, "Benchmark"]/n^2
	z <- z[, dimnames(z)[[2]] != "Benchmark"]/nonneg(z[, "Benchmark"])
#
# RETURN RESULT
	z
}
portfolio.residual <- function(x, y) {
# -----------------------------------------------------------------
# Name		: portfolio.residual
# Author	: VKS
# Date		: 10/29/18,10/22/22
# Args		: x = a matrix/df
#		: y = a matrix/df of the same dimensions as <x>
# Output	: residual of <x> after factoring out <y> for each row
# -----------------------------------------------------------------
	x <- x - rowMeans(x)
	y <- y - rowMeans(y)
	z <- x - y * rowSums(x * y)/nonneg(rowSums(y^2))
	z
}
position.floPct <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: position.floPct
# Author	: VKS
# Date		: 1/16/18,1/23,2/21,3/8,4/16
# Args		: x = strategy path
#		: y = subset
#		: n = last publication date
# Output	: Latest four-week flow percentage
# -----------------------------------------------------------------
#
# READ DATA
	x <- strat.path(x, "daily")
	x <- multi.asset(x)
#
# CREATE RESULT
	if (all(n != dimnames(x)[[1]])) {
		cat("Date", n, "not recognized! No output will be published ...\n")
		z <- NULL
	} else {
		if (dimnames(x)[[1]][dim(x)[1]] != n) {
			cat("Warning: Latest data not being used! Proceeding regardless ...\n")
			x <- x[dimnames(x)[[1]] <= n, ]
		}
		if (missing(y)) y <- dimnames(x)[[2]] else x <- mat.subset(x, y)
		#
		z <- x[dim(x)[1] - 19:0, ]
		z <- vec.named(mat.compound(t(z)), y)
		z <- z[order(-z)]
		#
		x <- x[dim(x)[1] - 19:0 - 5, ]
		x <- vec.named(mat.compound(t(x)), y)
		x <- map.rname(x, names(z))
		x <- rank(z) - rank(x)
		#
		y <- vec.named(qtl.eq(z), names(z))
		y <- mat.ex.vec(y, z)
		#
		z <- 0.01 * data.frame(z, 100 * x, y)
		dimnames(z)[[2]][1:2] <- c("Current", "RankChg")
	}
#
# RETURN RESULT
	z
}
principal.components <- function(x, y = 2) {
# -----------------------------------------------------------------
# Name		: principal.components
# Author	: VKS
# Date		: 11/17/17,11/18,12/18/18,5/17/19,5/20
# Args		: x = a matrix/df
#		: y = number of principal components desired
# Output	: first <y> principal components
# -----------------------------------------------------------------
	principal.components.underlying(x, y)$factor
}
principal.components.covar <- function(x, y) {
# -----------------------------------------------------------------
# Name		: principal.components.covar
# Author	: VKS
# Date		: 4/24/19,4/30,5/14,5/20
# Args		: x = a matrix/df
#		: y = number of principal components considered important
# Output	: covariance using first <y> components as factors
# Notes		: If <y> is zero or negative, number of components is auto-detected
# -----------------------------------------------------------------
#
# FACTORS
	z <- principal.components.underlying(x, y)
	if (is.null(dim(z$factor))) {
		z <- tcrossprod(as.matrix(z$factor), as.matrix(z$exposure))
	} else {
		z <- tcrossprod(z$factor, z$exposure)
	}
#
# RESIDUALS
	x <- x - z
#
# FACTOR RISK
	z <- crossprod(z)/(dim(x)[1] - 1)
#
# ADD IDIOSYNCRATIC RISK
	diag(z) <- diag(z) + colSums(x^2)/(dim(x)[1] - 1)
#
# RETURN RESULT
	z
}
principal.components.underlying <- function(x, y) {
# -----------------------------------------------------------------
# Name		: principal.components.underlying
# Author	: VKS
# Date		: 5/20/19
# Args		: x = a matrix/df
#		: y = number of principal components desired
# Output	: first <y> principal components
# Notes		: If <y> is zero or negative, number of components is auto-detected
# -----------------------------------------------------------------
#
# MEAN ADJUST TO CENTRE THE CLOUD ABOUT THE ORIGIN
	x <- scale(x, scale = F)
#
# SINGULAR-VALUE DECOMPOSITION
	z <- svd(x)
	dimnames(z$u)[[1]] <- dimnames(x)[[1]]
	dimnames(z$v)[[1]] <- dimnames(x)[[2]]
#
# AUTO-DETECT NUMBER OF PRINCIPAL COMPONENTS
	if (y < 1) y <- scree(z$d)
#
# CREATE RESULT
	if (y == 1) {
		z <- list(factor = z$u[, 1] * z$d[1], exposure = z$v[, 1])
	} else {
		z <- list(factor = z$u[, 1:y] %*% diag(z$d[1:y]), exposure = z$v[, 1:y])
	}
#
# RETURN RESULT
	z
}
proc.count <- function(x = 10) {
# -----------------------------------------------------------------
# Name		: proc.count
# Author	: VKS
# Date		: 7/9/21
# Args		: x = number of records to return (0 = everything)
# Output	: returns top <x> processes by number running
# -----------------------------------------------------------------
#
# GET LIST OF RUNNING PROCESSES
	z <- shell("tasklist /FO LIST", intern = T)
	z <- z[seq(2, length(z), by = 6)]
	z <- txt.right(z, nchar(z) - nchar("Image Name:"))
	z <- txt.trim(z)
#
# COUNT PROCESSES
	z <- vec.count(z)
#
# SORT ON COUNT DESCENDING
	z <- z[order(z, decreasing = T)]
#
# RETURN TOP <x>
	if (x > 0) z <- z[1:x]
#
# RETURN RESULT
	z
}
proc.kill <- function(x, y) {
# -----------------------------------------------------------------
# Name		: proc.kill
# Author	: VKS
# Date		: 2/23/11
# Args		: x = process name (e.g. "ftp.exe")
#		: y = number of instances
# Output	: kills of all processes <x> if more than <y> of them are running
# -----------------------------------------------------------------
#
# COUNT
	z <- shell("tasklist", intern = T)
	z <- txt.parse(z, " ")[, 1]
	z <- sum(z == x)
#
# KILL
	if (z > y) z <- shell(paste("TASKKILL /IM", x, "/F"), intern = T)
#
# RETURN NOTHING
	invisible()
}
product <- function(x) {
# -----------------------------------------------------------------
# Name		: product
# Author	: VKS
# Date		: 1/3/18
# Args		: x = a numeric vector
# Output	: product of <x>
# -----------------------------------------------------------------
	exp(sum(log(x)))
}
production.write <- function(x, y) {
# -----------------------------------------------------------------
# Name		: production.write
# Author	: VKS
# Date		: 5/15/17,12/5,12/15,4/25/18
# Args		: x = latest output
#		: y = path to output
# Output	: Writes production output if warranted
# -----------------------------------------------------------------
#
# CHECK THERE'S SOMETHING TO WRITE
	proceed <- !is.null(x)
#
# CHECK NUMBER OF COLUMNS IS UNCHANGED
	if (proceed) {
		w <- mat.read(y, ",")
		proceed <- dim(w)[2] == dim(x)[[2]]
	}
#
# CHECK COLUMN NAMES ARE UNCHANGED
	if (proceed) proceed <- all(dimnames(w)[[2]] == dimnames(x)[[2]])
#
# CHECK THAT NEW FILE IS BIGGER
	if (proceed) proceed <- dim(x)[1] > dim(w)[1]
#
# CHECK THAT NEW FILE HAS ALL THE OLD DATES
	if (proceed) proceed <- all(is.element(dimnames(w)[[1]], dimnames(x)[[1]]))
#
# CHECK THAT NEW FILE HAS ALL THE OLD OBSERVATIONS
	if (proceed) proceed <- all(colSums(mat.to.obs(x[dimnames(w)[[1]], ])) == colSums(mat.to.obs(w)))
#
# CHECK THAT OLD FILE AND NEW FILE MATCH EXACTLY
	if (proceed) proceed <- all(unlist(zav(x[dimnames(w)[[1]], ]) == zav(w)))
#
# ONCE ALL THE ABOVE CONDITIONS HAVE BEEN MEET, OVER WRITE OLD FILE
	if (proceed) {
		mat.write(x, y)
		cat("Writing to", y, "...\n")
	}
#
# RETURN NOTHING
	invisible()
}
pstudent2 <- function(x) {
# -----------------------------------------------------------------
# Name		: pstudent2
# Author	: VKS
# Date		: 9/7/16
# Args		: x = any real number
# Output	: Returns cumulative t-distribution with df = 2
# -----------------------------------------------------------------
	return(pt(x, 2))
}
ptile <- function(x) {
# -----------------------------------------------------------------
# Name		: ptile
# Author	: VKS
# Date		: 9/26/16,12/15/17
# Args		: x = a vector/matrix/data-frame
# Output	: Converts <x>, if a vector, or the rows of <x> otherwise, to a ptile
# -----------------------------------------------------------------
	fcn <- function(x) 100 * (rank(x) - 1)/(length(x) - 1)
	fcn2 <- function(x) fcn.nonNA(fcn, x)
	z <- fcn.mat.vec(fcn2, x, , F)
	z
}
publish.daily.last <- function(x) {
# -----------------------------------------------------------------
# Name		: publish.daily.last
# Author	: VKS
# Date		: 12/19/17,3/20/18,3/22,12/7/18
# Args		: x = a YYYYMMDD date
# Output	: last daily flow-publication date
# -----------------------------------------------------------------
	if (missing(x)) x <- today()
	z <- flowdate.lag(x, 2)
	z
}
publish.monthly.last <- function(x, y = 23, n = 0) {
# -----------------------------------------------------------------
# Name		: publish.monthly.last
# Author	: VKS
# Date		: 12/19/17,3/22/18,9/25/19
# Args		: x = a YYYYMMDD date
#		: y = calendar day in the next month when allocations are known (usually the 23rd)
#		: n = additional monthly lag (defaults to zero)
# Output	: date of last monthly publication
# -----------------------------------------------------------------
	if (missing(x)) x <- today()
	z <- yyyymmdd.lag(x, 1)
	z <- yyyymmdd.to.AllocMo(z, y)
	z <- yyyymm.lag(z, n)
	z <- yyyymm.to.day(z)
	z
}
publish.weekly.last <- function(x) {
# -----------------------------------------------------------------
# Name		: publish.weekly.last
# Author	: VKS
# Date		: 11/22/16,12/19/17,3/22/18
# Args		: x = a YYYYMMDD date
# Output	: date of last weekly publication
# -----------------------------------------------------------------
	if (missing(x)) x <- today()
	z <- as.numeric(day.to.weekday(x))
	if (any(z == 5:6)) z <- z - 3 else z <- z + 4
	z <- day.lag(x, z)
	z
}
qa.columns <- function(x) {
# -----------------------------------------------------------------
# Name		: qa.columns
# Author	: VKS
# Date		: 10/3/18,10/24,5/9/19,9/12,9/16
# Args		: x = M/W/D depending on whether flows are monthly/weekly/daily
# Output	: columns expected in ftp file
# -----------------------------------------------------------------
	if (any(x == c("M", "W", "D"))) {
		z <- c("ReportDate", "FundId", "Flow", "AssetsStart", "AssetsEnd", "ForexChange", "PortfolioChange")
	} else if (x == "S") {
		z <- mat.read(parameters("classif-GSec"))$AllocTable[1:10]
		z <- c("ReportDate", "FundId", z)
	} else if (x == "I") {
		z <- mat.read(parameters("classif-GIgrp"))$AllocTable
		z <- c("ReportDate", "FundId", z)
	} else if (x == "C") {
		z <- mat.read(parameters("classif-ctry"), ",")
		z <- z$AllocTable[is.element(z$OnFTP, 1)]
		z <- c("ReportDate", "FundId", z)
	} else if (any(x == c("StockM", "StockD"))) {
		z <- c("ReportDate", "HSecurityId", "GeoId", "CalculatedStockFlow")
	} else if (any(x == c("FundCtM", "FundCtD"))) {
		z <- c("ReportDate", "HSecurityId", "GeoId", "FundCt")
	} else if (any(x == c("IOND", "IONM"))) {
		z <- c("ReportDate", "HSecurityId", "Inflow", "Outflow")
	} else if (any(x == c("FwtdEx0", "FwtdIn0", "SwtdEx0", "SwtdIn0"))) {
		z <- c("ReportDate", "HSecurityId", "GeoId", "AverageAllocation")
	} else if (x == "AllocD") {
		z <- c("ReportDate", "SecurityId", "AllocDA", "AllocDInc", "AllocDDec", "AllocDAdd", "AllocDRem")
	} else if (x == "HoldSum") {
		z <- c("ReportDate", "HSecurityId", "GeoId", x)
	} else {
		z <- c("ReportDate", "HSecurityId", x)
	}
#
# RETURN RESULT
	z
}
qa.filter.map <- function(x) {
# -----------------------------------------------------------------
# Name		: qa.filter.map
# Author	: VKS
# Date		: 10/2/18,10/23/19,10/28,11/17/20,12/25
# Args		: x = filter (e.g. Aggregate/Active/Passive/ETF/Mutual)
# Output	: maps to appropriate code on the R side
# -----------------------------------------------------------------
	z <- as.character(map.rname(vec.read(parameters("classif-filterNames")), x))
	z <- ifelse(is.na(z), x, z)
	z
}
qa.index <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: qa.index
# Author	: VKS
# Date		: 10/3/18,9/16/19
# Args		: x = data frame
#		: y = T/F depending on whether <x> pertains to a macro strategy
#		: n = T/F depending on whether <x> pertains to a factor
# Output	: unique index for <x>
# -----------------------------------------------------------------
	if (y) {
		z <- x[, "FundId"]
	} else if (n) {
		z <- c("HSecurityId", "SecurityId")
		w <- is.element(z, dimnames(x)[[2]])
		z <- x[, z[w & !duplicated(w)]]
	} else {
		z <- paste(x[, "HSecurityId"], x[, "GeoId"])
	}
#
# RETURN RESULT
	z
}
qa.mat.read <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: qa.mat.read
# Author	: VKS
# Date		: 10/8/18,10/12,11/26,8/16/19
# Args		: x = remote file on an ftp site (e.g. "/ftpdata/mystuff/foo.txt")
#		: y = local folder (e.g. "C:\\temp")
#		: n = ftp site (defaults to standard)
#		: w = user id (defaults to standard)
#		: h = password (defaults to standard)
# Output	: contents of <x> as a data frame
# Notes		: If the first character of <x> is not "/" then it just reads in <x>
# -----------------------------------------------------------------
	local <- txt.left(x, 1) != "/"
#
# DOWNLOAD THE DATA
	if (!local) {
		if (missing(n)) n <- ftp.credential("ftp")
		if (missing(w)) w <- ftp.credential("user")
		if (missing(h)) h <- ftp.credential("pwd")
		ftp.get(x, y, n, w, h)
		x <- txt.right(x, nchar(x) - nchar(dirname(x)) - 1)
		x <- paste(y, x, sep = "\\")
	}
#
# READ IN THE DATA AND KILL THE LOCAL VERSION
	z <- NULL
	if (file.exists(x)) {
		z <- mat.read(x, "\t", NULL)
		if (!local) {
			Sys.sleep(1) # ELSE FILES OF THE FORM Tmp7630.tmp ARE CREATED
			file.kill(x)
		}
		dimnames(z)[[2]][1] <- "ReportDate"
		z[, "ReportDate"] <- yyyymmdd.ex.txt(z[, "ReportDate"])
	}
#
# RETURN RESULT
	z
}
qa.secMenu <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: qa.secMenu
# Author	: VKS
# Date		: 10/7/18,10/8,11/26,11/4/19
# Args		: x = a YYYYMM month
#		: y = SecMenuM/SecMenuD
#		: n = a connection, the output of odbcDriverConnect
#		: w = stock filter (e.g. All/China/Japan)
# Output	: compares HSecurityId/ReportDate pairs in Security Menu versus Flow Dollar
# -----------------------------------------------------------------
#
# PRELIMINARIES
	fldr <- "C:\\temp\\crap"
	z <- vec.named(, c("isFTP", "isSQL", "DUP", "FTP", "SQL", "FTPxSQL", "SQLxFTP"))
#
# READ IN DATA
	secMenuFile <- txt.replace(ftp.info(y, T, "ftp.path", "Aggregate"), "YYYYMM", x)
	secMenuFile <- qa.mat.read(secMenuFile, fldr)
	z["isFTP"] <- as.numeric(!is.null(secMenuFile))
	#
	if (z["isFTP"] == 1) {
		floDolrFile <- ftp.sql.factor(txt.replace(y, "SecMenu", "Stock"), yyyymm.to.day(x), "Aggregate", w)
		floDolrFile <- sql.query.underlying(floDolrFile, n, F)
		z["isSQL"] <- as.numeric(!is.null(floDolrFile))
	}
#
# REPORTDATE/HSECURITY ID PAIRS
	if (z["isFTP"] == 1 & z["isSQL"] == 1) {
		x <- paste(floDolrFile[, "ReportDate"], floDolrFile[, "HSecurityId"])
		x <- x[!duplicated(x)]
		#
		y <- paste(secMenuFile[, "ReportDate"], secMenuFile[, "HSecurityId"])
		z["DUP"] <- sum(duplicated(y))
		y <- y[!duplicated(y)]
	}
#
# POPULATE RESULT
	if (z["isFTP"] == 1 & z["isSQL"] == 1) {
		z["FTP"] <- sum(length(y))
		z["SQL"] <- sum(length(x))
		z["FTPxSQL"] <- sum(!is.element(y, x))
		z["SQLxFTP"] <- sum(!is.element(x, y))
	}
#
# RETURN RESULT
	z
}
qtl <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: qtl
# Author	: VKS
# Date		: 10/27/16,1/10/17,12/19/18
# Args		: x = a vector
#		: y = number of desired bins
#		: n = a weight vector
#		: w = a vector of groups (e.g. GSec)
# Output	: performs an equal-weight binning on <x> so that the members of <mem>
#		: are divided into <n> equal bins within each group <w>
# -----------------------------------------------------------------
#
# MISSING ARGUMENTS
	if (missing(n)) n <- rep(1, length(x))
	if (missing(w)) w <- rep(1, length(x))
#
# CREATE RESULT
	h <- !is.na(x) & !is.na(w)
	x <- data.frame(x, n, stringsAsFactors = F)
	fcn <- function(x) qtl.single.grp(x, y)
	z <- rep(NA, length(h))
	if (any(h)) z[h] <- fcn.vec.grp(fcn, x[h, ], w[h])
#
# RETURN RESULT
	z
}
qtl.eq <- function(x, y = 5) {
# -----------------------------------------------------------------
# Name		: qtl.eq
# Author	: VKS
# Date		: 9/7/16,9/14,9/19,12/15/17
# Args		: x = a vector/matrix/data-frame
#		: y = number of desired bins
# Output	: performs an equal-weight binning on <x> if <x> is a vector or
#		: the rows of <x> otherwise
# -----------------------------------------------------------------
	fcn.mat.vec(qtl, x, y, F)
}
qtl.fast <- function(x, y = 5) {
# -----------------------------------------------------------------
# Name		: qtl.fast
# Author	: VKS
# Date		: 3/27/19
# Args		: x = a vector
#		: y = number of desired bins
# Output	: performs a FAST equal-weight binning on <x>. Can't handle NAs.
# -----------------------------------------------------------------
	x <- order(-x)
	z <- ceiling((length(x)/y) * (0:y) + 0.5) - 1
	z <- z[-1] - z[-(y + 1)]
	z <- rep(1:y, z)[order(x)]
	z
}
qtl.single.grp <- function(x, y) {
# -----------------------------------------------------------------
# Name		: qtl.single.grp
# Author	: VKS
# Date		: 1/10/17,12/18/18
# Args		: x = a two-column numeric data frame. No NA's in first two columns
#		: y = number of desired bins
# Output	: an equal-weight binning so that the first column of <x>
#		: is divided into <y> equal bins. Weights determined by the 2nd column
# -----------------------------------------------------------------
	n <- x[, 2]
	x <- x[, 1]
#
# COMPUTE BINS ON NAMES WITH NON-ZERO NON-NA WEIGHT
	z <- rep(NA, length(x))
	w <- !is.element(n, 0) & !is.na(n)
	w <- w & !is.na(x)
	if (any(w)) z[w] <- qtl.underlying(x[w], n[w], y)
#
# EXTRAPOLATE TO NAMES WITH ZERO WEIGHT
	w2 <- is.element(n, 0) | is.na(n)
	w2 <- w2 & !is.na(x)
	if (any(w) & any(w2)) z[w2] <- qtl.zero.weight(x[w], z[w], x[w2], y)
#
# RETURN RESULT
	z
}
qtl.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: qtl.underlying
# Author	: VKS
# Date		: 1/10/17,12/19/18
# Args		: x = a vector with no NA's
#		: y = an isomekic vector lacking NA's or zeroes
#		: n = a positive integer
# Output	: divided <x> into <n> equal bins of roughly equal weight (as defined by <y>)
# -----------------------------------------------------------------
#
# NEGATIVE WEIGHTS
	if (any(y < 0)) stop("Can't handle negative weights!")
	if (n < 2) stop("Can't do this either!")
#
# NORMALIZE WEIGHTS SO THEY SUM TO UNITY
	y <- y/sum(y)
#
# SORT
	ord <- order(-x)
	x <- x[ord]
	y <- y[ord]
#
# IDENTIFY CUT POINTS
	if (all(y == y[1])) {
		h <- ceiling((length(x)/n) * (0:n) + 0.5) - 1
	} else {
		h <- 0
		for (i in 2:n - 1) h <- c(h, qtl.weighted(y, i/n))
		h <- c(h, length(x))
		h <- floor(h)
	}
	h <- h[-1] - h[-(n + 1)]
#
# BIN ASSIGNMENTS
	z <- rep(1:n, h)
#
# RESTORE ORIGINAL ORDER
	z <- z[order(ord)]
#
# RETURN RESULT
	z
}
qtl.weighted <- function(x, y) {
# -----------------------------------------------------------------
# Name		: qtl.weighted
# Author	: VKS
# Date		: 1/10/17
# Args		: x = an isomekic vector, lacking NA's or zeroes, that sums to unity
#		: y = a number between zero and one
# Output	: returns a number <z> so that the sum of x[1:z] is as close as possible to <y>.
# -----------------------------------------------------------------
	beg <- 0
	end <- 1 + length(x)
	while (end > beg + 1) {
		z <- floor((beg + end)/2)
		if (sum(x[1:z]) - x[z]/2 >= y) end <- z else beg <- z
	}
	z <- (beg + end)/2
	z
}
qtl.zero.weight <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: qtl.zero.weight
# Author	: VKS
# Date		: 1/10/17
# Args		: x = a vector of variables
#		: y = a corresponding vector of bin assignments
#		: n = a vector of variables that are to be assigned to bins
#		: w = number of bins to divide <x> into
# Output	: assigns the members of <x> to bins
# -----------------------------------------------------------------
	z <- approx(x, y, n, "constant", yleft = 1, yright = w)$y
	z <- ifelse(is.na(z), max(y), z)
	z
}
qtr.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: qtr.ex.int
# Author	: VKS
# Date		: 12/4/17
# Args		: x = a vector of integers
# Output	: returns a vector of <yyyymm> months
# -----------------------------------------------------------------
	z <- (x - 1) %/% 4
	x <- x - 4 * z
	z <- paste(z, x, sep = "Q")
	z <- txt.prepend(z, 6, 0)
	z
}
qtr.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: qtr.lag
# Author	: VKS
# Date		: 5/9/18
# Args		: x = a vector of quarters
#		: y = a number
# Output	: lags <x> by <y> quarters
# -----------------------------------------------------------------
	obj.lag(x, y, qtr.to.int, qtr.ex.int)
}
qtr.seq <- function(x, y, n = 1) {
# -----------------------------------------------------------------
# Name		: qtr.seq
# Author	: VKS
# Date		: 11/24/17,12/4,1/3/18
# Args		: x = a QTR
#		: y = a QTR
#		: n = quantum size in QTR
# Output	: returns a sequence of QTR between (and including) x and y
# -----------------------------------------------------------------
	obj.seq(x, y, qtr.to.int, qtr.ex.int, n)
}
qtr.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: qtr.to.int
# Author	: VKS
# Date		: 12/4/17
# Args		: x = a vector of <qtr>
# Output	: returns a vector of integers
# -----------------------------------------------------------------
	z <- as.numeric(substring(x, 1, 4))
	z <- 4 * z + as.numeric(substring(x, 6, 6))
	z
}
quant.info <- function(x, y) {
# -----------------------------------------------------------------
# Name		: quant.info
# Author	: VKS
# Date		: 2/4/20
# Args		: x = unique identifier of the quant
#		: y = a column in the classif-Quants file
# Output	: folder of function source file
# -----------------------------------------------------------------
	mat.read(parameters("classif-Quants"), "\t")[x, y]
}
read.EPFR <- function(x) {
# -----------------------------------------------------------------
# Name		: read.EPFR
# Author	: VKS
# Date		: 12/4/17
# Args		: x = a path to a file written by the dev team
# Output	: reads in the file
# -----------------------------------------------------------------
	z <- read.table(x, T, "\t", row.names = NULL, quote = "", as.is = T, na.strings = txt.na(), comment.char = "")
	names(z)[1] <- "ReportDate"
	z$ReportDate <- yyyymmdd.ex.txt(z$ReportDate)
	z
}
read.prcRet <- function(x) {
# -----------------------------------------------------------------
# Name		: read.prcRet
# Author	: VKS
# Date		: 11/30/16,12/19,4/25/18
# Args		: x = an object name (preceded by #) or the path to a ".csv" file
# Output	: returns the contents of the file
# -----------------------------------------------------------------
	if (txt.left(x, 1) == "#") {
		z <- substring(x, 2, nchar(x))
		z <- get(z)
	} else z <- mat.read(x, ",")
	z
}
recipient.exists <- function(x) {
# -----------------------------------------------------------------
# Name		: recipient.exists
# Author	: VKS
# Date		: 2/16/21
# Args		: x = report name
# Output	: T/F depending on whether recipient list exists
# -----------------------------------------------------------------
	any(is.element(mat.read(parameters("classif-recipient"), "\t", NULL)[, 1], x))
}
recipient.read <- function(x) {
# -----------------------------------------------------------------
# Name		: recipient.read
# Author	: VKS
# Date		: 2/16/21
# Args		: x = report name
# Output	: vector of recipient tranches
# -----------------------------------------------------------------
#
# READ IN DATA
	z <- mat.read(parameters("classif-recipient"), "\t", NULL)
#
# SUBSET
	z <- z[is.element(z[, "email"], x), ]
#
# SPLIT
	z <- split(z$recipient, z$tranche)
#
# ALLES LIST
	w <- sapply(z, function(x) any(x == "ALLES"))
	for (j in names(z)[w]) {
		z[[j]] <- setdiff(z[[j]], "ALLES")
		z[[j]] <- c(z[[j]], recipient.read("ALLES"))
	}
#
# FINAL RESULT
	z <- sapply(z, function(x) paste(x, collapse = "; "))
#
# RETURN RESULT
	z
}
record.exists <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: record.exists
# Author	: VKS
# Date		: 2/17/20
# Args		: x = report name
#		: y = date for which you want to send the report
#		: n = file name
# Output	: T/F depending on whether action already taken
# -----------------------------------------------------------------
	z <- record.read(n)
	if (!is.null(z) & any(names(z) == x)) z <- z[x] >= y else z <- F
	z
}
record.kill <- function(x, y) {
# -----------------------------------------------------------------
# Name		: record.kill
# Author	: VKS
# Date		: 2/11/20,12/17,12/25
# Args		: x = report name
#		: y = file name
# Output	: deletes entry <x> in the record <y>. Returns nothing.
# -----------------------------------------------------------------
#
# KILL CORRECT ENTRY
	n <- paste(dir.parameters("parameters"), y, sep = "\\")
	if (file.exists(n)) {
		z <- vec.read(n)
		if (any(names(z) == x)) {
			z <- z[!is.element(names(z), x)]
			vec.write(z, n)
		}
	}
#
# RETURN NOTHING
	invisible()
}
record.read <- function(x) {
# -----------------------------------------------------------------
# Name		: record.read
# Author	: VKS
# Date		: 12/17/20,12/25
# Args		: x = file name
# Output	: named vector of records and sent dates
# -----------------------------------------------------------------
	z <- paste(dir.parameters("parameters"), x, sep = "\\")
	if (file.exists(z)) z <- vec.read(z) else z <- NULL
	z
}
record.track <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: record.track
# Author	: VKS
# Date		: 12/17/20,12/18,6/24/22
# Args		: x = a SINGLE YYYYMMDD date
#		: y = file name
#		: n = T/F depending on whether this is for standard or Asia process
# Output	: writes report for date <x> and type <y>
# -----------------------------------------------------------------
#
# PRELIMINARIES
	z <- paste0(y, ifelse(n, "", "Asia"))
	z <- mat.read(parameters(paste0("classif-", z)), "\t")
	z <- z[is.element(z[, "day"], c(weekday.to.name(day.to.weekday(x)), "All")), ]
	z$yyyymmdd <- map.rname(record.read(paste0(y, ".txt")), dimnames(z)[[1]])
	z$today <- z$target <- rep(NA, dim(z)[1])
#
# DAILY DATA
	w <- z[, "entry"] == "date" & z[, "freq"] == "D"
	z[w, "target"] <- x
	z[w, "today"] <- T
#
# DAILY FLOWS
	w <- z[, "entry"] == "flow" & z[, "freq"] == "D"
	z[w, "target"] <- publish.daily.last(flowdate.lag(x, -as.numeric(!n)))
	z[w, "today"] <- T
#
# WEEKLY FLOWS
	w <- z[, "entry"] == "flow" & z[, "freq"] == "W"
	z[w, "target"] <- publish.weekly.last(flowdate.lag(x, -as.numeric(!n)))
	z[w, "today"] <- publish.weekly.last(flowdate.lag(x, -as.numeric(!n))) > publish.weekly.last(flowdate.lag(x, 1 - as.numeric(!n)))
#
# MONTHLY FLOWS
	w <- z[, "entry"] == "flow" & z[, "freq"] == "M"
	z[w, "target"] <- publish.monthly.last(x, 16)
	z[w, "today"] <- publish.monthly.last(x, 16) > publish.monthly.last(flowdate.lag(x, 1), 16)
#
# STOCK FLOWS
	w <- z[, "entry"] == "hold" & z[, "freq"] == "M"
	z[w, "target"] <- publish.monthly.last(x, 26)
	z[w, "today"] <- publish.monthly.last(x, 26) > publish.monthly.last(flowdate.lag(x, 1), 26)
#
# FX ALLOCATIONS
	w <- z[, "entry"] == "FXalloc" & z[, "freq"] == "M"
	z[w, "target"] <- publish.monthly.last(x, 9, 1)
	z[w, "today"] <- publish.monthly.last(x, 9, 1) > publish.monthly.last(flowdate.lag(x, 1), 9, 1)
#
# RETURN RESULT
	z
}
record.write <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: record.write
# Author	: VKS
# Date		: 12/17/20,12/25
# Args		: x = report name
#		: y = date for which you sent the report
#		: n = file name
# Output	: updates the record. Returns nothing.
# -----------------------------------------------------------------
#
# CREATE RECORD
	n <- paste(dir.parameters("parameters"), n, sep = "\\")
	if (file.exists(n)) {
		z <- vec.read(n)
		if (any(names(z) == x)) {
			z[x] <- max(z[x], y)
		} else {
			z[x] <- y
		}
		vec.write(z, n)
	}
#
# RETURN NOTHING
	invisible()
}
refresh.predictors.append <- function(x, y, n = F, w = F) {
# -----------------------------------------------------------------
# Name		: refresh.predictors.append
# Author	: VKS
# Date		: 5/16/17,10/30,12/4,1/2/18,1/27/22
# Args		: x = old data
#		: y = new data (must be a data-frame, cannot be a matrix)
#		: n = T/F depending on whether you want changes in data to be ignored
#		: w = T/F depending on whether the data already have row names
# Output	: Appends new to old data after performing checks
# -----------------------------------------------------------------
#
# INDEX THE ROWS BY THE FIRST COLUMN
	if (!w) y <- mat.index(y)
#
# CHECK COLUMNS
	if (dim(y)[2] != dim(x)[2]) stop("Problem 3")
	if (any(!is.element(dimnames(y)[[2]], dimnames(x)[[2]]))) stop("Problem 4")
	z <- y[, dimnames(x)[[2]]]
#
# CHECK THE DATE THAT OVERLAPS
	w <- is.element(dimnames(z)[[1]], dimnames(x)[[1]])
	if (sum(w) != 1) stop("Problem 5")
	m <- data.frame(unlist(z[w, ]), unlist(x[dimnames(z)[[1]][w], ]), stringsAsFactors = F)
	m <- correl(m[, 1], m[, 2])
	m <- zav(m)
	if (!n & m < 0.90) stop("Problem: Correlation between new and old data is", round(100 * m), "!")
#
# APPEND DATA
	z <- rbind(x, z[!w, ])
	z <- z[order(dimnames(z)[[1]]), ]
	last.date <- dimnames(z)[[1]][dim(z)[1]]
	cat("Final data have", dim(z)[1], "rows ending at", last.date, "...\n")
#
# RETURN RESULT
	z
}
refresh.predictors.script <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: refresh.predictors.script
# Author	: VKS
# Date		: 10/30/17,12/6/19
# Args		: x = query needed to get full history
#		: y = last part of the query that goes after the date restriction
#		: n = column corresponding to date in relevant sql table
#		: w = last date for which you already have data
# Output	: generates the SQL script to refresh predictors
# -----------------------------------------------------------------
	if (nchar(y) > 0) {
		z <- paste0(txt.left(x, nchar(x) - nchar(y)), "where\n\t", n, " >= '", w, "'\n", y)
	} else {
		z <- x
	}
#
# RETURN RESULT
	z
}
renorm <- function(x) {
# -----------------------------------------------------------------
# Name		: renorm
# Author	: VKS
# Date		: 9/23/16,10/3,12/15/17
# Args		: x = a numeric vector
# Output	: renormalizes, so the absolute weights sum to 100, <x>, if a vector, or the rows of <x> otherwise
# -----------------------------------------------------------------
	fcn <- function(x) 100 * x/excise.zeroes(sum(abs(x)))
	fcn2 <- function(x) fcn.nonNA(fcn, x)
	z <- fcn.mat.vec(fcn2, x, , F)
	z
}
reshape.wide <- function(x) {
# -----------------------------------------------------------------
# Name		: reshape.wide
# Author	: VKS
# Date		: 12/1/16,12/11/20,10/21/22
# Args		: x = a matrix/data-frame with last columns corresponding
#		:	to the entries of the resulting array
# Output	: converts <x> to an array
# Notes		: inverse of array.unlist
# -----------------------------------------------------------------
	z <- lapply(x[-dim(x)[2]], unique)
	x <- map.rname(mat.index(x, 2:dim(x)[2] - 1), do.call(paste, expand.grid(z)))
	z <- array(x, sapply(z, length), z)
	z
}
ret.ex.idx <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: ret.ex.idx
# Author	: VKS
# Date		: 6/30/17,1/31/19,10/22/22
# Args		: x = a file of total return indices indexed so that time runs forward
#		: y = number of rows over which the return is computed
#		: n = if T the result is labelled by the beginning of the period, else by the end.
#		: w = T/F depending on whether returns or spread changes are needed
# Output	: computes return
# -----------------------------------------------------------------
	if (w) x <- log(x)
	z <- mat.diff(x, y)
	if (w) z <- z <- 100 * exp(z) - 100
	if (n) z <- mat.lag(z, -y)
	z
}
ret.idx.gaps.fix <- function(x) {
# -----------------------------------------------------------------
# Name		: ret.idx.gaps.fix
# Author	: VKS
# Date		: 9/15/16,12/14/17,2/15/18
# Args		: x = a file of total return indices indexed by <yyyymmdd> dates so that time runs forward
# Output	: replaces NA's by latest available total return index (i.e. zero return over that period)
# -----------------------------------------------------------------
	fcn.mat.vec(fix.gaps, yyyymmdd.bulk(x), , T)
}
ret.outliers <- function(x, y = 1.5) {
# -----------------------------------------------------------------
# Name		: ret.outliers
# Author	: VKS
# Date		: 11/29/16,12/1
# Args		: x = a vector of returns
#		: y = outlier threshold
# Output	: Sets big ones to NA (a way to control for splits)
# -----------------------------------------------------------------
	mdn <- median(x, na.rm = T)
	y <- c(1/y, y) * (100 + mdn) - 100
	z <- !is.na(x) & x > y[1] & x < y[2]
	z <- ifelse(z, x, NA)
	z
}
ret.to.idx <- function(x) {
# -----------------------------------------------------------------
# Name		: ret.to.idx
# Author	: VKS
# Date		: 10/26/17,12/20/18
# Args		: x = a file of total returns indexed so that time runs forward
# Output	: computes a total-return index
# -----------------------------------------------------------------
	if (is.null(dim(x))) {
		z <- x
		w <- !is.na(z)
		n <- find.data(w, T)
		m <- find.data(w, F)
		if (n > 1) n <- n - 1
		z[n] <- 100
		while (n < m) {
			n <- n + 1
			z[n] <- (1 + zav(z[n])/100) * z[n - 1]
		}
	} else {
		z <- fcn.mat.vec(ret.to.idx, x,, T)
	}
#
# RETURN RESULT
	z
}
ret.to.log <- function(x) {
# -----------------------------------------------------------------
# Name		: ret.to.log
# Author	: VKS
# Date		: 2/15/18
# Args		: x = a vector of returns
# Output	: converts to logarithmic return
# -----------------------------------------------------------------
	log(1 + x/100)
}
rgb.diff <- function(x, y) {
# -----------------------------------------------------------------
# Name		: rgb.diff
# Author	: VKS
# Date		: 3/29/18,4/4
# Args		: x = a vector of length three containing numbers between 0 and 256
#		: y = a vector of length three containing numbers between 0 and 256
# Output	: distance between RGB colours <x> and <y>
# -----------------------------------------------------------------
	z <- (x[1] + y[1])/2
	z <- c(z/256, 2, 1 - z/256) + 2
	z <- sqrt(sum(z * (x - y)^2))
	z
}
rquaternion <- function(x) {
# -----------------------------------------------------------------
# Name		: rquaternion
# Author	: VKS
# Date		: 11/28/20
# Args		: x = number of quaternions desired
# Output	: n x 4 matrix of randomly generated number of unit size
# Notes		: Shoemake, K. "Uniform Random Rotations." Graphics Gems III
#		: (K. David, ed.). New York: Academic Press, 1992.
# -----------------------------------------------------------------
	z <- mat.ex.matrix(matrix(runif(3 * x), x, 3, F, list(1:x, c("x", "y", "n"))))
	z <- do.call(glome.ex.R3, z)
	z
}
rrw <- function(prdBeg, prdEnd, vbls, univ, grp.nm, ret.nm, fldr, orth.factor = NULL, classif) {
# -----------------------------------------------------------------
# Name		: rrw
# Author	: VKS
# Date		: 11/3/16,3/27/17,1/16/18,1/23,1/31,3/22/19
# Args		: prdBeg = a first-return date in yyyymm format representing the first month of the backtest
#		: prdEnd = a first-return date in yyyymm format representing the last month of the backtest
#		: vbls = vector of variables against which return is to be regressed
#		: univ = universe (e.g. "R1Mem", or c("EafeMem", 1, "CountryCode", "JP"))
#		: grp.nm = neutrality group (e.g. "GSec")
#		: ret.nm = return variable (e.g. "Ret")
#		: fldr = stock-flows folder
#		: orth.factor = factor to orthogonalize all variables to (e.g. "PrcMo")
#		: classif = classif file
# Output	: regression results
# -----------------------------------------------------------------
#
# BASIC DATA
	dts <- yyyymm.seq(prdBeg, prdEnd)
	df <- NULL
	for (i in dts) {
		if (txt.right(i, 2) == "01") cat("\n", i, "") else cat(txt.right(i, 2), "")
		x <- rrw.underlying(i, vbls, univ, grp.nm, ret.nm, fldr, orth.factor, classif)
		x <- mat.subset(x, c("ret", vbls))
		dimnames(x)[[1]] <- paste(i, dimnames(x)[[1]])
		if (is.null(df)) df <- x else df <- rbind(df, x)
	}
	cat("\n")
#
# CREATE RESULT
	z <- list(value = map.rname(rrw.factors(df), vbls), corr = correl(df), data = df)
#
# RETURN RESULT
	z
}
rrw.factors <- function(x) {
# -----------------------------------------------------------------
# Name		: rrw.factors
# Author	: VKS
# Date		: 3/27/17,3/14/18,3/6/20
# Args		: x = a data frame, the first column of which has returns
# Output	: Returns the t-values of factors that best predict return
# -----------------------------------------------------------------
#
# HANDLE COLUMN NAMES LIKE "ION%" & "ION$"
	y <- dimnames(x)[[2]]
	names(y) <- fcn.vec.num(col.ex.int, 1:dim(x)[2])
	dimnames(x)[[2]] <- names(y)
#
# CREATE RESULT
	z <- summary(lm(txt.regr(dimnames(x)[[2]]), x))$coeff[-1, "t value"]
	while (any(z < 0) & any(z > 0)) {
		x <- x[, !is.element(dimnames(x)[[2]], names(z)[order(z)][1])]
		z <- summary(lm(txt.regr(dimnames(x)[[2]]), x))$coeff[, "t value"][-1]
	}
#
# ORIGINAL FACTOR NAMES
	names(z) <- map.rname(y, names(z))
#
# RETURN RESULT
	z
}
rrw.underlying <- function(prd, vbls, univ, grp.nm, ret.nm, fldr, orth.factor, classif) {
# -----------------------------------------------------------------
# Name		: rrw.underlying
# Author	: VKS
# Date		: 3/27/17,1/23/18,12/20,2/4/19,3/29,11/3/20
# Args		: prd = a first-return date in yyyymm format representing the return period of interest
#		: vbls = vector of variables against which return is to be regressed
#		: univ = universe (e.g. "R1Mem", or c("EafeMem", 1, "CountryCode", "JP"))
#		: grp.nm = neutrality group (e.g. "GSec")
#		: ret.nm = return variable (e.g. "Ret")
#		: fldr = parent directory containing derived/data
#		: orth.factor = factor to orthogonalize all variables to (e.g. "PrcMo")
#		: classif = classif file
# Output	: Runs regressions
# -----------------------------------------------------------------
#
# BASIC DATA
	z <- fetch(c(vbls, orth.factor), yyyymm.lag(prd, 1), 1, paste0(fldr, "\\derived"), classif)
	grp <- classif[, grp.nm]
	mem <- sf.subset(univ, prd, fldr, classif)
#
# zSCORE
	z <- mat.ex.matrix(mat.zScore(z, mem, grp))
#
# SUBSET
	z$grp <- grp
	z$mem <- mem
	z$ret <- fetch(ret.nm, prd, 1, paste0(fldr, "\\data"), classif)
	z <- mat.last.to.first(z)
	z <- z[is.element(z$mem, 1) & !is.na(z$grp) & !is.na(z$ret), ]
#
# ORTHOGONALIZE
	if (!is.null(orth.factor)) {
		z[, orth.factor] <- zav(z[, orth.factor])
		for (j in vbls) {
			w <- !is.na(z[, j])
			z[w, j] <- as.numeric(summary(lm(txt.regr(c(j, orth.factor)), z[w, ]))$residuals)
			z[, j] <- mat.zScore(z[, j], z$mem, z$grp)
		}
	}
#
# SUBSET
	w <- apply(mat.to.obs(z[, c(vbls, "ret")]), 1, max) > 0
	z <- mat.ex.matrix(zav(z[w, ]))
#
# MEAN ADJUST THE RETURNS SO NO SINGLE PERIOD IS MORE IMPORTANT THAN ANY OTHER
	z$ret <- z$ret - mean(z$ret)
#
# RETURN RESULT
	z
}
run.cs.reg <- function(x, y) {
# -----------------------------------------------------------------
# Name		: run.cs.reg
# Author	: VKS
# Date		: 12/28/16,12/18/18
# Args		: x = a matrix of n columns (usually stocks go down and returns go across)
#		: y = a matrix of n rows (whatever vectors you're regressing on)
# Output	: regresses each row of <x> on design matrix <y>
# -----------------------------------------------------------------
	y <- as.matrix(y)
	z <- tcrossprod(as.matrix(x), tcrossprod(solve(crossprod(y)), y))
	z
}
scree <- function(x) {
# -----------------------------------------------------------------
# Name		: scree
# Author	: VKS
# Date		: 4/25/19
# Args		: x = a decreasing numerical vector
# Output	: number of eigenvectors to use (by looking at the "kink")
# -----------------------------------------------------------------
#
# PRELIMINARIES
	n <- length(x)
	y <- x[1]/n
#
# FIRST DIFFERENCES
	x <- x[-n] - x[-1]
#
# ANGLES
	x <- 1.5 * pi - atan(x[1 - n]/y) - atan(y/x[-1])
#
# COMPUTE NUMBER OF EIGENVECTORS TO USE
	z <- (3:n - 1)[order(x)][1]
#
# RETURN RESULT
	z
}
seconds.sho <- function(x) {
# -----------------------------------------------------------------
# Name		: seconds.sho
# Author	: VKS
# Date		: 2/16/18
# Args		: x = a number
# Output	: time elapsed since <x> in hh:mm:ss format
# Notes		: intended to measure time under a 100 hours
# -----------------------------------------------------------------
#
# MEASURE TIME ELAPSED
	z <- proc.time()[["elapsed"]] - x
#
# CONVERT TO BASE 60
	z <- round(z)
	z <- base.ex.int(z, 60)
#
# COLLAPSE HIGHER-ORDER REPRESENTATIONS
	n <- length(z)
	if (n > 3) {
		z <- c(base.to.int(z[3:n - 2], 60), z[n - 1:0])
		n <- 3
	}
#
# BULK UP SMALL <z>
	while (n < 3) {
		z <- c(0, z)
		n <- n + 1
	}
#
# CREATE RESULT
	z <- paste(txt.right(100 + z, 2), collapse = ":")
#
# RETURN RESULT
	z
}
separating.hyperplane <- function(x, y) {
# -----------------------------------------------------------------
# Name		: separating.hyperplane
# Author	: VKS
# Date		: 11/28/20,12/2
# Args		: x = a unit vector of length dim(x)[2] - 1
#		: y = a matrix, the first column being a 1/0 vector
# Output	: number of errors and distance from origin for best separating hyperlane
# -----------------------------------------------------------------
	classification.threshold(x[, 1], x[, -1] %*% y)
}
sf <- function(fcn, prdBeg, prdEnd, univ, grp.nm, ret.nm, trails, fldr, nBins = 5, geom.comp = F, retHz = 1, classif) {
# -----------------------------------------------------------------
# Name		: sf
# Author	: VKS
# Date		: 11/2/16,11/3,12/7,12/15,2/13/17,3/24,4/7,5/22,11/16,
#		: 1/23/18,1/9/19,6/1/20,10/14
# Args		: fcn = function that fetches data for the appropriate period and parameter
#		: prdBeg = first-return date in YYYYMM
#		: prdEnd = first-return date in YYYYMM after <prdBeg>
#		: univ = membership (e.g. "EafeMem" or c("GemMem", 1))
#		: grp.nm = group within which binning is to be performed
#		: ret.nm = return variable
#		: trails = variable parameter
#		: fldr = data folder
#		: nBins = number of bins
#		: geom.comp = T/F depending on whether you want bin excess returns summarized geometrically or arithmetically
#		: retHz = forward return horizon in months
#		: classif = classif file
# Output	: runs a stock-flows simulation
# -----------------------------------------------------------------
	n.trail <- length(trails)
#
# SUMMARY FUNCTION
	summ.fcn <- ifelse(geom.comp, "bbk.bin.rets.geom.summ", "bbk.bin.rets.summ")
	summ.fcn <- get(summ.fcn)
	fcn.loc <- function(x) {summ.fcn(x, 12/retHz)}
#
# CREATE AND POPULATE RESULT
	z <- list()
	for (j in 1:n.trail) {
		cat(trails[j], "")
		if (j %% 10 == 0) cat("\n")
		#
		x <- sf.single.bsim(fcn, prdBeg, prdEnd, univ, grp.nm, ret.nm, fldr, trails[j], T, nBins, retHz, classif)$returns
		x <- t(map.rname(t(x), c(dimnames(x)[[2]], "TxB")))
		x[, "TxB"] <- x[, "Q1"] - x[, paste0("Q", nBins)]
		x <- mat.ex.matrix(x)
		z[[as.character(trails[j])]] <- summ.multi(fcn.loc, x, retHz)
	}
	z <- simplify2array(z)
	cat("\n")
#
# RETURN RESULT
	z
}
sf.bin.nms <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sf.bin.nms
# Author	: VKS
# Date		: 4/17/17
# Args		: x = number of bins
#		: y = T/F depending on whether you want universe returns returned
# Output	: returns bin names
# -----------------------------------------------------------------
	z <- c(1:x, "na")
	z <- z[order(c(1:x, x/2 + .25))]
	z <- paste0("Q", z)
	if (y) z <- c(z, "uRet")
	z
}
sf.detail <- function(fcn, prdBeg, prdEnd, univ, grp.nm, ret.nm, trail, fldr, nBins = 5, classif, weighting.factor = NULL) {
# -----------------------------------------------------------------
# Name		: sf.detail
# Author	: VKS
# Date		: 5/10/18,5/15,3/18/19,4/3/20,6/2,10/14
# Args		: fcn = function that fetches data for the appropriate period and parameter
#		: prdBeg = first-return date in YYYYMM
#		: prdEnd = first-return date in YYYYMM after <prdBeg>
#		: univ = membership (e.g. "EafeMem" or c("GemMem", 1))
#		: grp.nm = group within which binning is to be performed
#		: ret.nm = return variable
#		: trail = variable parameter
#		: fldr = data folder
#		: nBins = number of bins or numeric vector with last element T/F for dependent/independent
#		: classif = classif file
#		: weighting.factor = factor you want to use for Cap-weighted back-tests (defaults to NULL)
# Output	: runs a stock-flows simulation
# -----------------------------------------------------------------
#
# BASIC DATA
	x <- sf.single.bsim(fcn, prdBeg, prdEnd, univ, grp.nm, ret.nm, fldr, trail, T, nBins, 1, classif, weighting.factor)
	x <- lapply(x, mat.ex.matrix)
	if (length(nBins) == 1) x$returns$TxB <- x$returns$Q1 - x$returns[, paste0("Q", nBins)]
#
# SUMMARY
	z <- bbk.bin.rets.summ(x$returns, 12)
	z.ann <- t(bbk.bin.rets.prd.summ(bbk.bin.rets.summ, x$returns, txt.left(dimnames(x$returns)[[1]], 4), 12)["AnnMn", , ])
	z <- list(summ = z, annSumm = z.ann, counts = x$counts)
#
# RETURN RESULT
	z
}
sf.single.bsim <- function(fcn, prdBeg, prdEnd, univ, grp.nm, ret.nm, fldr, trail, uRet = F, nBins = 5, retHz = 1, classif, weighting.factor = NULL) {
# -----------------------------------------------------------------
# Name		: sf.single.bsim
# Author	: VKS
# Date		: 12/7/16,12/19,2/13/17,4/17,11/16,1/23/18,4/3/20,4/23,
#		: 6/1,10/14,12/21
# Args		: fcn = function that fetches data for the appropriate period and parameter
#		: prdBeg = first-return date in YYYYMM
#		: prdEnd = first-return date in YYYYMM after <prdBeg>
#		: univ = membership (e.g. "EafeMem" or c("GemMem", 1))
#		: grp.nm = group within which binning is to be performed
#		: ret.nm = return variable
#		: fldr = data folder
#		: trail = variable parameter
#		: uRet = T/F depending on whether the equal-weight universe return is desired
#		: nBins = number of bins or numeric vector with last element T/F for dependent/independent
#		: retHz = forward return horizon in months
#		: classif = classif file
#		: weighting.factor = factor you want to use for Cap-weighted back-tests (defaults to NULL)
# Output	: runs a single quintile simulation
# -----------------------------------------------------------------
#
# FETCH DATA
	grp <- classif[, grp.nm]
	z <- list()
	for (i in yyyymm.seq(prdBeg, prdEnd)) {
		z[[i]] <- sf.underlying.data(fcn, univ, ret.nm, i, trail, grp, nBins, fldr, retHz, classif, weighting.factor)
	}
#
# BIN COUNTS
	fcn <- function(x) {
		z <- ifelse(is.na(x[, "ret"]), 0, x[, "mem"])
		x <- x[, "bin"]
		pivot.1d(sum, x[z > 0], z[z > 0])
	}
	h <- array.ex.list(lapply(z, fcn), T)
	if (length(nBins) == 1) h <- map.rname(h, sf.bin.nms(nBins, uRet))
	h <- t(h)
#
# BIN RETURNS
	z <- lapply(z, function(x) sf.underlying.summ(x, uRet))
	z <- array.ex.list(z, T)
	if (length(nBins) == 1) z <- map.rname(z, sf.bin.nms(nBins, uRet))
	z <- t(z)
#
# RETURN RESULT
	z <- list(returns = z, counts = h)
	z
}
sf.subset <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sf.subset
# Author	: VKS
# Date		: 2/22/17,2/23,1/23/18
# Args		: x = membership (e.g. "EafeMem" or c("GemMem", 1))
#		: y = a YYYYMM or YYYYMMDD
#		: n = folder in which to find the data
#		: w = classif file
# Output	: Returns a 1/0 mem vector
# -----------------------------------------------------------------
#
# PARSE <x> ARGUMENT
	m <- length(x)
	if (m == 1) x <- c(x, 1)
#
# DETERMINE THE PERIOD TO RECOVER THE UNIVERSE FROM
	z <- y
	if (nchar(y) == 8) z <- yyyymmdd.to.yyyymm(z)
	z <- yyyymm.lag(z, 1)
#
# FETCH THE UNIVERSE
	z <- fetch(x[1], z, 1, paste(n, "data", sep = "\\"), w)
	z <- is.element(z, x[2])
#
# SECONDARY RESTRICTIONS
	if (m > 2) z <- z & is.element(w[, x[3]], x[4])
#
# CONVERT TO NUMERIC
	z <- as.numeric(z)
#
# RETURN RESULT
	z
}
sf.underlying.data <- function(fcn, univ, ret.nm, ret.prd, trail, grp, nBins, fldr, retHz, classif, weighting.factor) {
# -----------------------------------------------------------------
# Name		: sf.underlying.data
# Author	: VKS
# Date		: 11/7/16,12/12,2/13/17,3/15,6/19,11/16,1/23/18,3/14,
#		: 1/8/19,3/29,4/3/20,6/1,6/5,10/13,10/14
# Args		: fcn = function that fetches data for the appropriate period and parameter
#		: univ = membership (e.g. "EafeMem" or c("GemMem", 1))
#		: ret.nm = return variable
#		: ret.prd = the period for which you want returns
#		: trail = variable parameter
#		: grp = group within which binning is to be performed
#		: nBins = number of bins or numeric vector with last element T/F for dependent/independent
#		: fldr = data folder
#		: retHz = forward return horizon in months
#		: classif = classif file
#		: weighting.factor = factor you want to use for Cap-weighted back-tests
# Output	: Gets data needed to back-test a single period
# -----------------------------------------------------------------
#
# MEM VECTOR (ALWAYS MONTHLY)
	mem <- sf.subset(univ, ret.prd, fldr, classif)
#
# VARIABLE
	vbl <- fcn(ret.prd, trail, fldr, classif)
#
# RETURN
	if (retHz == 1) {
		ret <- fetch(ret.nm, ret.prd, 1, paste(fldr, "data", sep = "\\"), classif)
	} else {
		ret <- fetch(ret.nm, yyyymm.lag(ret.prd, 1 - retHz), retHz, paste(fldr, "data", sep = "\\"), classif)
		ret <- mat.compound(ret)
	}
#
# WEIGHTING FACTOR
	bin <- ifelse(is.na(ret), 0, mem)
	if (!is.null(weighting.factor)) {
		weighting.factor <- fetch(weighting.factor, yyyymm.lag(ret.prd, 1), 1, paste(fldr, "derived", sep = "\\"), classif)
		bin <- weighting.factor <- vec.max(zav(weighting.factor) * bin, bin)
	}
#
# BINNING
	bin <- sf.underlying.data.bin(vbl, nBins, bin, grp)
#
# CREATE RESULT
	z <- data.frame(bin, ret, mem, grp, row.names = dimnames(classif)[[1]], stringsAsFactors = F)
	if (!is.null(weighting.factor)) z$wgt <- weighting.factor
#
# RETURN RESULT
	z
}
sf.underlying.data.bin <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sf.underlying.data.bin
# Author	: VKS
# Date		: 10/14/20
# Args		: x = either vector or list of vectors
#		: y = number of bins or numeric vector with last element T/F for dependent/independent
#		: n = numeric vector of weighting factors
#		: w = vector of binning groups
# Output	: character vector of bin memberships
# Notes		: last element of <y> is 1/0 for dependent binning or not
# -----------------------------------------------------------------
	if (!is.list(x)) {
		z <- qtl(x, y, n, w)
		z <- ifelse(is.na(z), "Qna", paste0("Q", z))
	} else {
		h <- length(names(x))
		if (length(y) == h) u <- T else u <- is.element(y[h + 1], 1)
		if (!u) {
			for (j in 1:h) {
				x[[j]] <- qtl(x[[j]], y[j], n, w)
				x[[j]] <- ifelse(is.na(x[[j]]), "na", x[[j]])
				x[[j]] <- paste0(names(x)[j], x[[j]])
			}
			z <- Reduce(paste, x)
		} else {
			j <- 1
			x[[j]] <- qtl(x[[j]], y[j], n, w)
			x[[j]] <- ifelse(is.na(x[[j]]), "na", x[[j]])
			x[[j]] <- paste0(names(x)[j], x[[j]])
			z <- x[[j]]
			#
			for (j in 2:h) {
				x[[j]] <- qtl(x[[j]], y[j], n, paste(z, w))
				x[[j]] <- ifelse(is.na(x[[j]]), "na", x[[j]])
				x[[j]] <- paste0(names(x)[j], x[[j]])
				z <- paste(z, x[[j]])
			}
		}
	}
#
# RETURN RESULT
	z
}
sf.underlying.summ <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sf.underlying.summ
# Author	: VKS
# Date		: 3/15/17,4/3/20,10/14
# Args		: x = a matrix/df with the following columns:
#		:	a) bin - bin memberships
#		:	b) ret - forward returns
#		:	c) mem - 1/0 universe memberships
#		:	d) wgt - universe weights (optional)
#		: y = T/F variable controlling whether universe return is returned
# Output	: Returns a named vector of bin returns
# -----------------------------------------------------------------
#
# DETERMINE SUBSET
	if (all(dimnames(x)[[2]] != "wgt")) x$wgt <- x$mem
	u <- is.element(x$mem, 1) & !is.na(x$ret) & !is.na(x$wgt) & x$wgt > 0
#
# CREATE RESULT
	if (any(u)) {
		univ.ret <- sum(x$ret[u] * x$wgt[u])/sum(x$wgt[u])
		x$ret <- x$ret - univ.ret
		z <- pivot.1d(sum, x$bin[u], x$ret[u] * x$wgt[u])
		z <- z/map.rname(pivot.1d(sum, x$bin[u], x$wgt[u]), names(z))
		if (y) z["uRet"] <- univ.ret
	} else {
		z <- NULL
	}
#
# RETURN RESULT
	z
}
sf.vec.to.array <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sf.vec.to.array
# Author	: VKS
# Date		: 10/14/20
# Args		: x = named vector of characteristics
#		: y = variable names
#		: n = number of bins
# Output	: expresses <x> as an array
# -----------------------------------------------------------------
#
# ARRAY NAMES
	z <- split(n, y)
	for (j in names(z)) {
		z[[j]] <- sf.bin.nms(z[[j]], F)
		z[[j]] <- paste0(j, substring(z[[j]], 2, nchar(z[[j]])))
	}
#
# CREATE RESULT
	x <- map.rname(x, do.call(paste, expand.grid(z)))
	z <- array(x, lapply(z, length), z)
#
# RETURN RESULT
	z
}
shell.wrapper <- function(x, y) {
# -----------------------------------------------------------------
# Name		: shell.wrapper
# Author	: VKS
# Date		: 9/11/20,11/11
# Args		: x = string to issue as command
#		: y = timeout in seconds
# Output	: result of command <x>
# Notes		: times out after <y> seconds
# -----------------------------------------------------------------
#
# CREATE RESULT
	setTimeLimit(elapsed = y, transient = T)
	z <- tryCatch(shell(x, intern = T), error = function(e){NULL})
#
# RETURN RESULT
	z
}
sim.direction <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sim.direction
# Author	: VKS
# Date		: 3/18/20,3/23
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = vector of group limits (names correspond to columns in <x>)
# Output	: percentage needed to get worst group under control
# -----------------------------------------------------------------
#
# HOW MUCH WE NEED TO BUY TO GET WORST GROUP UNDER CONTROL
	z <- round(max(sim.direction.buy(x, y)), 4)
#
# HOW MUCH WE NEED TO SELL TO GET WORST GROUP UNDER CONTROL
	y <- round(max(sim.direction.sell(x, y)), 4)
#
# CREATE RESULT
	z <- ifelse(z > y, z, -y)
#
# RETURN RESULT
	z
}
sim.direction.buy <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sim.direction.buy
# Author	: VKS
# Date		: 3/23/20,4/24
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = vector of group limits (names correspond to columns in <x>)
# Output	: percentage buy needed to get worst group under control
# -----------------------------------------------------------------
	if (length(y) > 1) {
		z <- -apply(x[, paste0(names(y), "Wt")], 2, min)
	} else {
		z <- -min(x[, paste0(names(y), "Wt")])
	}
	z <- vec.max(z - y, 0)
	z
}
sim.direction.sell <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sim.direction.sell
# Author	: VKS
# Date		: 3/23/20,4/24
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = vector of group limits (names correspond to columns in <x>)
# Output	: percentage sell needed to get worst group under control
# -----------------------------------------------------------------
	if (length(y) > 1) {
		z <- apply(x[, paste0(names(y), "Wt")], 2, max)
	} else {
		z <- min(x[, paste0(names(y), "Wt")])
	}
	z <- vec.max(z - y, 0)
	z
}
sim.fetch <- function(x, y, n, w, h = NULL) {
# -----------------------------------------------------------------
# Name		: sim.fetch
# Author	: VKS
# Date		: 3/13/20,3/18,3/19
# Args		: x = YYYYMM representing period of interest
#		: y = string representing variable name
#		: n = string representing universe name
#		: w = stock-flow environment
#		: h = risk factors
# Output	: data needed to run simulation
# -----------------------------------------------------------------
#
# GROUPINGS
	z <- w$classif[, c("GSec", "CountryCode")]
	dimnames(z)[[2]] <- c("Sec", "Ctry")
#
# BASIC DATA AT END OF PRIOR MONTH
	z$Alp <- fetch(y, yyyymm.lag(x), 1, paste(w$fldr, "derived", sep = "\\"), w$classif)
	z$Bmk <- fetch(paste0(n, "Wt"), yyyymm.lag(x), 1, paste(w$fldr, "data", sep = "\\"), w$classif)
	u <- fetch(paste0(n, "Mem"), yyyymm.lag(x), 1, paste(w$fldr, "data", sep = "\\"), w$classif)
#
# RETURN OVER CURRENT MONTH
	z$Ret <- zav(fetch("Ret", x, 1, paste(w$fldr, "data", sep = "\\"), w$classif))
#
# RISK FACTORS
	if (!is.null(h)) z <- data.frame(z, fetch(h, yyyymm.lag(x), 1, paste(w$fldr, "derived", sep = "\\"), w$classif), stringsAsFactors = F)
#
# QUINTILE VARIABLE(S)
	h <- c("Alp", h)
	for (j in h) {
		z[, j] <- qtl(z[, j], 5, u, w$classif$RgnSec)
		z[, j] <- ifelse(is.na(z[, j]), 3, z[, j])
	}
#
# SUBSET TO INDEX MEMBERS
	z <- z[is.element(u, 1), ]
#
# RENORMALIZE BENCHMARK
	z$Bmk <- renorm(z$Bmk)
#
# RETURN RESULT
	z
}
sim.limits <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sim.limits
# Author	: VKS
# Date		: 3/18/20
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = vector of group limits (names correspond to columns in <x>)
# Output	: returns group active-weight limits applying to each stock
# -----------------------------------------------------------------
	for (j in names(y)) x[, paste0(j, "Wt")] <- zav(map.rname(pivot.1d(sum, x[, j], x$Act), x[, j]))
	z <- x
	z
}
sim.optimal <- function(x, y, n, w, h, u) {
# -----------------------------------------------------------------
# Name		: sim.optimal
# Author	: VKS
# Date		: 3/13/20,3/18,3/19,4/2
# Args		: x = a data frame with columns Alp/Bmk/Sec/Ctry
#		: y = initial portfolio weight
#		: n = percentage single-stock active-weight name limit
#		: w = vector of group limits (names correspond to columns in <x>)
#		: h = quintile to sell (stocks in bin <h> and higher are flushed)
#		: u = integer between 0 and 100
# Output	: returns named vector of optimal portfolio weights
# -----------------------------------------------------------------
	x$Act <- y - x$Bmk
#
# ENFORCE NAME LIMITS
	x$Act <- vec.max(vec.min(x$Act, n), -n)
#
# FLUSH BOTTOM QUINTILES
	x$Act <- ifelse(is.element(x$Alp, h:5), -vec.min(x$Bmk, n), x$Act)
#
# ENFORCE GROUP LIMITS
	x <- sim.limits(x, w)
	h <- sim.direction(x, w)
	while (h != 0) {
		if (h > 0) y <- sim.direction.buy(x, w) else y <- sim.direction.sell(x, w)
		y <- y[y > 0]
		y <- names(y)[order(y, decreasing = T)]
		#
		x$Stk <- sim.trade.stk(x, h > 0, n, F)
		x$Grp <- sim.trade.grp(x, h > 0, w)
		x$Trd <- vec.min(x$Stk, x$Grp) > 0
		#
		x <- mat.sort(x, c("Trd", y, "Alp", "Stk"), c(T, rep(h < 0, length(y) + 1), T))
		x$Act[1] <- x$Act[1] + sign(h) * min(x$Stk[1], x$Grp[1])
		#
		x <- sim.limits(x, w)
		h <- sim.direction(x, w)
	}
#
# ENSURE PORTFOLIO IS FULLY INVESTED
	x <- sim.limits(x, w)
	h <- -round(sum(x$Act), 4)
	while (h != 0) {
		x$Stk <- sim.trade.stk(x, h > 0, n, T)
		x$Grp <- sim.trade.grp(x, h > 0, w)
		x$Trd <- vec.min(x$Stk, x$Grp) > 0
		#
		if (u > 0) {
			vec <- list(A = apply(x[, c("Grp", "Stk")], 1, min), B = sign(h) * x[, "Ret"])
			vec <- sapply(vec, function(x) x/sqrt(sum(x^2)), simplify = "array")
			x <- x[order((vec %*% c(100 - u, u))[, 1], decreasing = T), ]
			x <- mat.sort(x, c("Trd", "Alp"), c(T, h < 0))
		} else {
			x <- mat.sort(x, c("Trd", "Alp", "Grp", "Stk"), c(T, h < 0, T, T))
		}
		x$Act[1] <- x$Act[1] + sign(h) * min(x$Stk[1], x$Grp[1])
		#
		x <- sim.limits(x, w)
		h <- -round(sum(x$Act), 4)
	}
#
# CREATE RESULT
	z <- x[, c("Bmk", "Act", "Ret", names(w))]
#
# RETURN RESULT
	z
}
sim.overall <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sim.overall
# Author	: VKS
# Date		: 3/18/20,3/19
# Args		: x = list object, elements are the output of <sim.optimal>
#		: y = named vector of turnover
#		: n = vector of group limits (names correspond to columns in elements of <x>)
# Output	: summarizes simulation
# -----------------------------------------------------------------
	x <- mat.ex.matrix(t(sapply(x, function(x) sim.summ(x, n))))
	x$to <- y
#
# CREATE RESULT
	z <- c("to", "Names", "Act", txt.expand(names(n), c("Selec", "Alloc", "Intcn"), ""))
	z <- colMeans(x[, z])
	z[names(z) != "Names"] <- z[names(z) != "Names"] * 12
	z["Sharpe"] <- z["Act"]/nonneg(sd(x$Act) * sqrt(12))
	z <- c(z, apply(x[, paste0(c("Name", names(n)), "Max")], 2, max))
#
# RE-ORDER
	n <- vec.named(seq_along(z), names(z))
	n["Sharpe"] <- n["Act"] + 0.5
	z <- z[order(n)]
#
# RETURN RESULT
	z
}
sim.seed <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sim.seed
# Author	: VKS
# Date		: 3/25/20
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = percentage single-stock active-weight name limit
#		: n = vector of group limits (names correspond to columns in <x>)
# Output	: initial portfolio satisfying limits prioritizing earlier records of <x>
# -----------------------------------------------------------------
#
# TOP QUINTILE NAMES
	x <- x[order(x$Alp), ]
#
# INVEST
	x$Act <- -x$Bmk
#
# ENFORCE NAME LIMITS
	x$Act <- vec.max(vec.min(x$Act, y), -y)
#
# ENSURE PORTFOLIO IS FULLY INVESTED, PRESERVING RECORD ORDER
	x <- sim.limits(x, n)
	x$Stk <- sim.trade.stk(x, T, y, T)
	x$Grp <- sim.trade.grp(x, T, n)
	x <- x[order(vec.min(x$Stk, x$Grp) > 0, decreasing = T), ]
	while (sum(x$Act) < 0.0001 & min(x$Stk[1], x$Grp[1]) > 0.0001) {
		x$Act[1] <- x$Act[1] + min(x$Stk[1], x$Grp[1])
		x$Stk <- sim.trade.stk(x, T, y, T)
		x$Grp <- sim.trade.grp(x, T, n)
		x <- x[order(vec.min(x$Stk, x$Grp) > 0, decreasing = T), ]
	}
#
# CREATE RESULT
	z <- rowSums(x[, c("Bmk", "Act")])
#
# RETURN RESULT
	z
}
sim.summ <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sim.summ
# Author	: VKS
# Date		: 3/13/20,3/19
# Args		: x = data frame, the output of <sim.optimal>
#		: y = vector of group limits (names correspond to columns of <x>)
# Output	: summarizes simulation
# -----------------------------------------------------------------
#
# RETURNS
	z <- colSums(x[, c("Bmk", "Act")] * x$Ret)/100
#
# NAME COUNT
	z["Names"] <- sum(rowSums(x[, c("Bmk", "Act")]) > 0)
#
# INDIVIDUAL NAMES
	z["NameMax"] <- max(abs(x[, "Act"]))
#
# GROUPS
	for (j in names(y)) {
		n <- brinson(x$Bmk, x$Act, x$Ret, x[, j])
		n["Max"] <- max(abs(pivot.1d(sum, x[, j], x$Act)))
		names(n) <- paste0(j, names(n))
		z <- c(z, n)
	}
#
# RETURN RESULT
	z
}
sim.trade.grp <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sim.trade.grp
# Author	: VKS
# Date		: 3/18/20
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = T/F depending on whether you buy/sell
#		: n = vector of group limits (names correspond to columns in <x>)
# Output	: max you can trade without breaching group limits
# -----------------------------------------------------------------
	z <- matrix(n, dim(x)[1], length(n), T, list(dimnames(x)[[1]], paste0(names(n), "Wt")))
	if (y) {
		z <- z - x[, dimnames(z)[[2]]]
	} else {
		z <- z + x[, dimnames(z)[[2]]]
	}
	z <- vec.max(apply(z, 1, min), 0)
#
# RETURN RESULT
	z
}
sim.trade.stk <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sim.trade.stk
# Author	: VKS
# Date		: 3/18/20
# Args		: x = a data frame, the output of <sim.fetch>
#		: y = T/F depending on whether you buy/sell
#		: n = percentage single-stock active-weight name limit
#		: w = T/F depending on whether you're fully investing the portfolio
# Output	: max you can trade without breaching name limits
# -----------------------------------------------------------------
	if (y) {
		z <- n - x$Act
	} else {
		z <- x$Act + x$Bmk
		z <- vec.min(z, n + x$Act)
	}
	z <- vec.max(z, 0)
	if (w) z <- vec.min(z, max(ifelse(y, -1, 1) * sum(x$Act), 0))
#
# RETURN RESULT
	z
}
smear.Q1 <- function(x) {
# -----------------------------------------------------------------
# Name		: smear.Q1
# Author	: VKS
# Date		: 9/27/16
# Args		: x = any real number
# Output	: Returns weights associated with ranks 1:x so that
#		:	a) every position in the top quintile has an equal positive weight
#		:	b) every position in the bottom 3 quintiles has an equal negative weight
#		:	c) second quintile positions get a linear interpolation
#		:	d) the weights sum to zero
#		:	e) the positive weights sum to 100
# -----------------------------------------------------------------
#
# COMPUTE QUINTILES
	bin <- qtl.eq(x:1)
#
# INCREMENTS
	incr <- rep(NA, x)
	w <- bin == 2
	incr[w] <- sum(w):1
	incr[bin == 1] <- 1 + sum(w)
	incr[bin > 2] <- 0
	tot.incr <- sum(incr)
#
# FIND RIGHT NUMBER OF POSITIVE POSITIONS
	m <- sum(bin < 3)
	pos.incr <- sum(incr[1:m])
	wt.incr <- 100/(pos.incr - m * tot.incr/x)
	neg.act <- tot.incr * wt.incr/x
	z <- incr * wt.incr - neg.act
	while (abs(sum(vec.max(z, 0)) - 100) > 1e-5) {
		m <- m - 1
		pos.incr <- sum(incr[1:m])
		wt.incr <- 100/(pos.incr - m * tot.incr/x)
		neg.act <- tot.incr * wt.incr/x
		z <- incr * wt.incr - neg.act
	}
#
# RETURN RESULT
	z
}
sql.1dActWtTrend <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend
# Author	: VKS
# Date		: 10/28/16,11/14,11/25,11/28,12/21,1/10/18,1/11,1/23,
#		: 1/24,3/19,10/9,11/2,3/14/21
# Args		: x = vector of flow dates in YYYYMMDD (known two days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: the SQL query to get 1dActWtTrend
# -----------------------------------------------------------------
	y <- sql.arguments(y)
	z <- sql.1dActWtTrend.underlying(x, y$filter, sql.RDSuniv(n))
	z <- c(z, sql.1dActWtTrend.topline(y$factor, w, T))
	z
}
sql.1dActWtTrend.Alloc <- function(x, y, n, w = NULL) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.Alloc
# Author	: VKS
# Date		: 3/30/22
# Args		: x = YYYYMM month
#		: y = temp table name (e.g. "#CTRY")
#		: n = identifier column (SectorId/CountryId)
#		: w = vector of acceptable identifiers
# Output	: SQL query for allocation table for FloTrend
# -----------------------------------------------------------------
	z <- paste0("MonthEnding = '", yyyymm.to.day(x), "'")
	z <- sql.tbl(c("HFundId", "AUM = sum(AssetsEnd)"), "MonthlyData", z, "HFundId")
	z <- c(sql.label(z, "t1"), "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
	v <- sql.label(sql.1dFloTrend.Alloc.fetch(x, n, w, F, F), "t3 on t3.FundId = t2.FundId")
	z <- c(z, "inner join", v)
	z <- sql.tbl(c("t2.FundId", n, "Allocation", "AUM"), z)
	if (!missing(y)) z <- c(sql.drop(y), "", sql.into(z, y))
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.Final <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.Final
# Author	: VKS
# Date		: 3/30/22
# Args		: x = temp table name (#CTRY/#SEC)
#		: y = factor (one of ActWtTrend/ActWtDiff/ActWtDiff2)
#		: n = identifier column (SectorId/CountryId)
# Output	: SQL query for daily ActWtTrend
# -----------------------------------------------------------------
#
# FUND-WEIGHTED AVERAGE WITHIN EACH GEOID
	r <- c("DayEnding", n, "GeographicFocus", "WtdAvg = sum(Allocation * AUM)/sum(AUM)")
	z <- c("#FLO t1", "inner join", sql.label(x, "t2 on t2.FundId = t1.FundId"))
	z <- sql.tbl(r, z,, paste(r[-length(r)], collapse = ", "), "sum(AUM) > 0")
#
# FINAL FROM STATEMENT
	z <- c(sql.label(z, "t1"), "inner join", "#FLO t2")
	z <- c(z, "\ton t2.GeographicFocus = t1.GeographicFocus and t2.DayEnding = t1.DayEnding")
	z <- c(z, "inner join", sql.label(x, "t3"))
	z <- c(z, paste0("\ton t3.FundId = t2.FundId and t3.", n, " = t1.", n))
#
# FINAL SELECT STATEMENT
	if (y == "ActWtTrend") {
		y <- paste(y, sql.Trend("Flow * (Allocation - WtdAvg)", ""))
	} else if (y == "ActWtDiff") {
		y <- paste(y, sql.Diff("Flow", "Allocation - WtdAvg", ""))
	} else if (y == "ActWtDiff2") {
		y <- paste(y, sql.Diff("Allocation - WtdAvg", "Flow", ""))
	} else stop("Bad Argument")
	y <- c(sql.yyyymmdd("t2.DayEnding", "DayEnding"), paste0("t3.", n), y)
	z <- sql.tbl(y, z,, paste0("t2.DayEnding, t3.", n))
	z <- paste(sql.unbracket(z), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.Flow <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.Flow
# Author	: VKS
# Date		: 3/30/22
# Args		: x = flowdate
#		: y = vector of filters
# Output	: SQL query for flows to compute ActWtTrend for Ctry/Sec
# -----------------------------------------------------------------
	x <- list(A = paste0("'", x, "'"))
	z <- c("DayEnding", "FundId", "GeographicFocus = max(GeographicFocus)", "Flow = sum(Flow)")
	x <- sql.Flow(z, x, c("CB", y, "UI"), "GeographicFocus", T, paste(z[1:2], collapse = ", "))
	z <- sql.into(x, "#FLO")
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.select <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.select
# Author	: VKS
# Date		: 10/1/18,6/16/21
# Args		: x = desired factor
# Output	: select statement to compute <x>
# -----------------------------------------------------------------
#
# HANDLE SUFFIX
	y <- ""
	if (is.element(txt.right(x, 3), c("Num", "Den"))) {
		y <- txt.right(x, 3)
		x <- txt.left(x, nchar(x) - nchar(y))
	}
#
# CREATE RESULT
	if (x == "ActWtTrend") {
		z <- paste0(x, y, " ", sql.Trend("Flow * (hld.HoldingValue/aum.PortVal - FundWtdExcl0)", y))
	} else if (x == "ActWtDiff") {
		z <- paste0(x, y, " ", sql.Diff("Flow", "hld.HoldingValue/aum.PortVal - FundWtdExcl0", y))
	} else if (x == "ActWtDiff2") {
		z <- paste0(x, y, " ", sql.Diff("hld.HoldingValue/aum.PortVal - FundWtdExcl0", "Flow", y))
	} else stop("Bad Argument")
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.topline <- function(x, y, n = F) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.topline
# Author	: VKS
# Date		: 3/19/18,10/1,10/2,10/9,1/7/19,1/25,11/9/20,
#		: 3/7/21,3/14
# Args		: x = a string vector of factors to be computed
#		: y = T/F depending on whether you are checking ftp
#		: n = T/F depending on whether ReportDate must be a column
# Output	: SQL query to get the select statement for 1dActWtTrend
# -----------------------------------------------------------------
#
# SELECT STATEMENT
	z <- h <- ifelse(y, "hld.HSecurityId", "SecurityId")
	if (y | n) {
		z <- c(sql.yyyymmdd("flo.ReportDate", "ReportDate", y), z)
		h <- paste0(h, ", flo.ReportDate")
	}
	z <- c(z, sapply(vec.to.list(x), sql.1dActWtTrend.select))
#
# BODY OF FINAL QUERY
	x <- sql.1dActWtTrend.topline.from()
	if (!y) x <- c(x, "inner join", "SecurityHistory id on id.HSecurityId = hld.HSecurityId")
	z <- paste(sql.unbracket(sql.tbl(z, x,, h)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.topline.from <- function() {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.topline.from
# Author	: VKS
# Date		: 10/3/18,10/19,3/7/21
# Args		: none
# Output	: SQL query to get the select statement for 1dActWtTrend
# -----------------------------------------------------------------
	w <- "ReportDate, HSecurityId, GeographicFocusId, FundWtdExcl0 = sum(HoldingValue)/sum(PortVal)"
	z <- c("#FLO t1", "inner join", "#HLD t2 on t2.FundId = t1.FundId", "inner join", "#AUM t3 on t3.FundId = t1.FundId")
	w <- sql.label(sql.tbl(w, z, , "ReportDate, HSecurityId, GeographicFocusId"), "mnW")
	z <- c("#FLO flo", "inner join", "#HLD hld on hld.FundId = flo.FundId", "inner join", "#AUM aum on aum.FundId = hld.FundId", "inner join")
	z <- c(z, w, "\ton mnW.ReportDate = flo.ReportDate and mnW.HSecurityId = hld.HSecurityId and mnW.GeographicFocusId = flo.GeographicFocusId")
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.underlying
# Author	: VKS
# Date		: 3/19/18,4/11,4/13,6/18,8/20,10/1,10/18,3/7/21,11/24
# Args		: x = vector of flow dates in YYYYMMDD (known two days later)
#		: y = the type of fund used in the computation
#		: n = "" or the SQL query to subset to securities desired
# Output	: the SQL query to get the data for 1dActWtTrend
# -----------------------------------------------------------------
	mo.end <- yyyymmdd.to.AllocMo.unique(x, 26, T)
	x <- paste0("'", x, "'")
	if (length(x) == 1) x <- paste("=", x) else x <- paste0("in (", paste(x, collapse = ", "), ")")
	x <- paste("ReportDate", x)
#
# BASIC TABLES
	z <- c("DailyData t1", "inner join", sql.label(sql.FundHistory(y, T, c("FundId", "GeographicFocusId")), "t2"), "on t2.HFundId = t1.HFundId")
	z <- sql.tbl("ReportDate, FundId, GeographicFocusId = max(GeographicFocusId), Flow = sum(Flow), AssetsStart = sum(AssetsStart)", z, x, "ReportDate, FundId")
	z <- sql.1dActWtTrend.underlying.basic(z, mo.end)
#
# BULK UP HOLDINGS
	z <- c(z, sql.Holdings.bulk.wrapper("#HLD", y, mo.end, "#BMKHLD", "#BMKAUM"))
#
# SUBSET HOLDINGS
	if (n[1] != "") z <- c(z, "", "delete from #HLD where", paste0("\t", sql.in("HSecurityId", n, F)))
#
# CREATE RESULT
	z <- paste(z, collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dActWtTrend.underlying.basic <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1dActWtTrend.underlying.basic
# Author	: VKS
# Date		: 11/10/21,11/24
# Args		: x = SQL query
#		: y = month end in YYYYMMDD format
# Output	: Query to insert <x> into flow table
# -----------------------------------------------------------------
	x <- c("insert into", "\t#FLO (ReportDate, FundId, GeographicFocusId, Flow, AssetsStart)", sql.unbracket(x))
	x <- c(sql.index("#FLO", "ReportDate, FundId"), x)
	x <- c("create table #FLO (ReportDate datetime not null, FundId int not null, GeographicFocusId int, Flow float, AssetsStart float)", x)
	x <- c(sql.drop(c("#AUM", "#HLD", "#FLO")), "", x)
	#
	x <- c(x, "", "create table #AUM (FundId int not null, PortVal float not null)")
	z <- c(x, sql.index("#AUM", "FundId"))
	#
	w <- c("MonthlyData t1", "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
	w <- sql.unbracket(sql.tbl("FundId, PortVal = sum(AssetsEnd)", w, paste0("ReportDate = '", y, "'"), "FundId", "sum(AssetsEnd) > 0"))
	z <- c(z, "insert into", "\t#AUM (FundId, PortVal)", w)
	#
	z <- c(z, "", "create table #HLD (FundId int not null, HFundId int not null, HSecurityId int not null, HoldingValue float)")
	z <- c(z, sql.index("#HLD", "FundId, HSecurityId"))
	z <- c(z, "insert into", "\t#HLD (FundId, HFundId, HSecurityId, HoldingValue)", sql.unbracket(sql.MonthlyAlloc(paste0("'", y, "'"))))
#
# RETURN RESULT
	z
}
sql.1dFloMo <- function(x, y, n, w, h, u = "All") {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo
# Author	: VKS
# Date		: 10/26/16,11/10,11/15,1/18/17,3/1,5/22,6/16,8/14,1/4/18,1/5,
#		: 1/9,1/11,1/12,1/23,5/16,6/18,10/1,10/9,10/11,10/12,10/24,
#		: 11/2,4/1/19,12/3/20,3/8/21,3/14,9/27/21,9/28,3/2/22
# Args		: x = vector of flow dates in YYYYMMDD (known two days later)
#		: y = a string vector of factors to be computed,
#		:	the last elements of which are the type of fund used
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = one or more breakdown filters (e.g. All/GeoId/DomicileId)
#		: u = share-class filter (one of All/Inst/Retail)
# Output	: Generates the SQL query to get the data for 1dFloMo for individual stocks
# -----------------------------------------------------------------
#
# BASIC TABLES
	v <- sql.1dFloMo.underlying(x)
#
# BULK UP HOLDINGS
	v <- c(v, sql.Holdings.bulk.wrapper("#HLD", y, yyyymm.to.day(yyyymmdd.to.AllocMo(x, 26)), "#BMKHLD", "#BMKAUM"))
#
# SELECT STATEMENT
	z <- sql.1dFloMo.select.wrapper(y, w, h, T)
#
# GROUP BY CLAUSE
	grp <- sql.1dFloMo.grp(w, h)
#
# FINAL RESULT
	x <- sql.DailyFlo(paste0("'", x, "'"), F,, u, h = T)
	#
	y <- c(sql.label(sql.1dFloMo.filter(y, h), "t0"), "inner join", "#HLD t1 on t1.FundId = t0.FundId")
	y <- c(y, "inner join", sql.label(x, "t2 on t2.HFundId = t0.HFundId"))
	y <- c(y, "inner join", "#AUM t3 on t3.FundId = t1.FundId")
	if (!w) y <- c(y, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	#
	if (n == "All") {
		z <- sql.tbl(z, y, , grp, "sum(HoldingValue/t3.AssetsEnd) > 0")
	} else {
		z <- sql.tbl(z, y, sql.in("t1.HSecurityId", sql.RDSuniv(n)), grp, "sum(HoldingValue/t3.AssetsEnd) > 0")
	}
	z <- c(paste(v, collapse = "\n"), paste(sql.unbracket(z), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.1dFloMo.CountryId.List <- function(x, y = "") {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.CountryId.List
# Author	: VKS
# Date		: 7/15/20,9/28,11/19,1/14/21,1/28,1/29,4/6,7/30,
#		: 3/22/22,5/20,5/23
# Args		: x = one of Ctry/FX/Sector/EMDM/Aux
#		: y = one of the following: (a) a YYYYMM date (b) a YYYYMMDD date
# Output	: map of security to CountryId
# -----------------------------------------------------------------
#
# PRELIMINARIES
	classif.type <- x
	sep <- ","
	if (x == "Ctry") {
		z <- Ctry.msci.members.rng("ACWI", "200704", "300012")
		classif.type <- "Ctry"
	} else if (x == "Aux") {
		z <- c("BG", "EE", "GH", "KE", "KZ", "LT", "UA", "NG", "RO", "RS", "SI", "LK")
		classif.type <- "Ctry"
	} else if (x == "OtherFrontier") {
		z <- c("BH", "HR", "LB", "MU", "OM", "TN", "TT", "BD", "CI", "IS")
		classif.type <- "Ctry"
	} else if (x == "APac") {
		z <- c("AU", "CN", "ID", "IN", "JP", "MY", "PH", "SG", "TW", "NZ", "HK", "PK", "BD", "LK", "VN", "PG", "KH", "MM", "MN", "KR", "TH")
		classif.type <- "Ctry"
	} else if (x == "LatAm") {
		z <- mat.read(parameters("classif-Ctry"))
		z <- dimnames(z)[[1]][is.element(z$EpfrRgn, "Latin America")]
		classif.type <- "Ctry"
	} else if (x == "CountryFlow") {
		z <- mat.read(parameters("classif-Ctry"))
		z <- dimnames(z)[[1]][!is.na(z$CountryId)]
		classif.type <- "Ctry"
	} else if (x == "EMDM") {
		z <- Ctry.msci.members("ACWI", y)
		classif.type <- "Ctry"
	} else if (x == "FX") {
		z <- Ctry.msci.members.rng("ACWI", "200704", "300012")
		z <- c(z, "CY", "EE", "LV", "LT", "SK", "SI")
		classif.type <- "Ctry"
	} else if (x == "Sector") {
		z <- dimnames(mat.read(parameters("classif-GSec"), "\t"))[[1]]
		classif.type <- "GSec"
		sep <- "\t"
	} else if (x == "Industry") {
		z <- dimnames(mat.read(parameters("classif-GIgrp"), "\t"))[[1]]
		classif.type <- "GIgrp"
		sep <- "\t"
	} else if (nchar(x) == 2) {
		z <- x
		classif.type <- "Ctry"
	}
#
# READ IN ALLOCATION TABLE COLUMN NAMES
	h <- parameters(paste("classif", classif.type, sep = "-"))
	h <- mat.read(h, sep)
	h <- map.rname(h, z)
#
# CREATE RESULT
	if (any(x == c("Ctry", "CountryFlow", "LatAm", "APac", "Aux", "OtherFrontier"))) {
		z <- vec.named(z, h$CountryId)
	} else if (x == "EMDM") {
		w.dm <- is.element(z, c("US", "CA", Ctry.msci.members("EAFE", y)))
		w.em <- is.element(z, Ctry.msci.members("EM", y))
		z <- c(vec.named(rep("DM", sum(w.dm)), h$CountryId[w.dm]), vec.named(rep("EM", sum(w.em)), h$CountryId[w.em]))
	} else if (x == "FX") {
		z <- vec.named(h$Curr, h$CountryId)
	} else if (x == "Sector") {
		z <- vec.named(z, h$SectorId)
		z["30"] <- "FinsExREst" # MADE-UP SECTOR
	} else if (x == "Industry") {
		z <- vec.named(z, h$IndustryId)
	} else if (nchar(x) == 2) {
		z <- vec.named(z, h$CountryId)
	}
#
# RETURN RESULT
	z
}
sql.1dFloMo.FI <- function() {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.FI
# Author	: VKS
# Date		: 10/27/17,1/5/18,1/12,1/4/19,11/23/20
# Args		: none
# Output	: Generates the SQL query to get daily 1dFloMo for fixed income
# -----------------------------------------------------------------
#
# CONDITIONS & LABELS
	x <- c("FundType = 'M'", "StyleSector = 130", "StyleSector = 134 and GeographicFocus = 77", "StyleSector = 137 and GeographicFocus = 77", "StyleSector = 141 and GeographicFocus = 77", "StyleSector = 185 and GeographicFocus = 77", "StyleSector = 125 and Category = '9'", "Category = '8'", "GeographicFocus = 31", "GeographicFocus = 30")
	names(x) <- c("CASH", "FLOATS", "USTRIN", "USTRLT", "USTRST", "USMUNI", "HYIELD", "WESEUR", "GLOBEM", "GLOFIX")
#
# FROM STATEMENT
	z <- sql.case("grp", x, c(names(x), "OTHER"), F)
	z <- c(sql.label(sql.FundHistory("FundType in ('B', 'M')", F, z), "t2"))
	z <- c("DailyData t1", "inner join", z, "\ton t2.HFundId = t1.HFundId")
#
# SELECT STATEMENT
	y <- paste0("case when grp = '", names(x), "' then Flow else NULL end")
	x <- paste(names(x), sql.Mo(y, txt.replace(y, "Flow", "AssetsStart"), NULL, T))
	z <- sql.tbl(c(sql.yyyymmdd("DayEnding"), x), z,, "DayEnding")
	z <- paste(sql.unbracket(z), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dFloMo.filter <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.filter
# Author	: VKS
# Date		: 10/24/18,11/2,12/3/20
# Args		: x = a string vector of factors to be computed, the
#		:	last elements of which are the type of fund used
#		: y = breakdown filter (e.g. All/GeoId/DomicileId)
# Output	: implements filters for 1dFloMo
# -----------------------------------------------------------------
	sql.FundHistory(sql.arguments(x)$filter, T, c("FundId", sql.breakdown(y)))
}
sql.1dFloMo.grp <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.grp
# Author	: VKS
# Date		: 10/24/18,12/3/20,3/8/21
# Args		: x = T/F depending on whether you are checking ftp
#		: y = one or more breakdown filters (e.g. All/GeoId/DomicileId)
# Output	: group by clause for 1dFloMo
# -----------------------------------------------------------------
	z <- c("ReportDate", ifelse(x, "HSecurityId", "SecurityId"), sql.breakdown(y))
	z <- paste(z, collapse = ", ")
	z
}
sql.1dFloMo.Rgn <- function() {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.Rgn
# Author	: VKS
# Date		: 10/27/17,12/5,1/5/18,1/11,1/12,1/16,1/7/19
# Args		: none
# Output	: Generates the SQL query to get daily 1dFloMo for regions
# -----------------------------------------------------------------
	rgn <- c(4, 24, 43, 46, 55, 76, 77)
	names(rgn) <- c("AsiaXJP", "EurXGB", "Japan", "LatAm", "PacXJP", "UK", "USA")
#
# SELECT
	x <- paste0("sum(case when grp = ", rgn, " then AssetsStart else NULL end)")
	x <- sql.nonneg(x)
	z <- paste0(names(rgn), " = 100 * sum(case when grp = ", rgn, " then Flow else NULL end)/", x)
	z <- c(sql.yyyymmdd("DayEnding"), z)
#
# FROM
	y <- c("HFundId, grp = case when GeographicFocus in (6, 80, 35, 66) then 55 else GeographicFocus end")
	w <- sql.and(list(A = "FundType = 'E'", B = "Idx = 'N'", C = sql.in("GeographicFocus", "(4, 24, 43, 46, 55, 76, 77, 6, 80, 35, 66)")))
	y <- c(sql.label(sql.tbl(y, "FundHistory", w), "t1"), "inner join", "DailyData t2", "\ton t2.HFundId = t1.HFundId")
#
# CREATE RESULT
	z <- paste(sql.unbracket(sql.tbl(z, y, , "DayEnding")), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dFloMo.Sec.flush <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.Sec.flush
# Author	: VKS
# Date		: 11/24/21
# Args		: x = classif object
#		: y = name of SQL temp table
# Output	: SQL to flush single-sector funds
# -----------------------------------------------------------------
	z <- paste0("StyleSector in (", paste(x$StyleSector, collapse = ", "), ")")
	z <- sql.in("FundId", sql.tbl("FundId", "#FLO", z))
	z <- sql.unbracket(sql.tbl("FundId", y, z))
	z[1] <- "delete from"
	z <- z[-2][-2]
	z
}
sql.1dFloMo.Sec.topline <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.Sec.topline
# Author	: VKS
# Date		: 11/24/21
# Args		: x = SectorId/IndustryId
#		: y = item (one of Flow/AssetsStart/AssetsEnd)
#		: n = name of SQL temp table (#SEC/#INDY)
#		: w = T/F depending on whether daily/weekly
# Output	: top line SQL statement for daily/weekly CBE flow momentum
# -----------------------------------------------------------------
	r <- sql.yyyymmdd(ifelse(w, "DayEnding", "WeekEnding"))
	r <- c(r, x, paste0(y, " = 0.0001 * sum(", y, " * Universe * Allocation)"))
	z <- c("#FLO t1", "inner join", "#CTRY t2 on t2.FundId = t1.FundId")
	z <- c(z, "inner join", paste(n, "t3 on t3.FundId = t1.FundId"))
	z <- sql.tbl(r, z,, paste0(ifelse(w, "DayEnding", "WeekEnding"), ", ", x))
	z <- paste(sql.unbracket(z), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dFloMo.select <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.select
# Author	: VKS
# Date		: 10/11/18,10/12,10/23,12/9/20,4/11/22
# Args		: x = desired factor
# Output	: select statement to compute <x>
# -----------------------------------------------------------------
	if (is.element(x, paste0("FloMo", c("", "CB", "PMA")))) {
		z <- paste(x, sql.Mo("Flow", "AssetsStart", "HoldingValue/t3.AssetsEnd", T))
	} else if (x == "FloDollar") {
		z <- paste(x, "= sum(Flow * HoldingValue/t3.AssetsEnd)")
	} else if (x == "AssetsStartDollar") {
		z <- paste(x, "= sum(AssetsStart * HoldingValue/t3.AssetsEnd)")
	} else if (x == "AssetsEndDollar") {
		z <- paste(x, "= sum(t2.AssetsEnd * HoldingValue/t3.AssetsEnd)")
	} else if (x == "Inflow") {
		z <- paste(x, "= sum(case when Flow > 0 then Flow else 0 end * HoldingValue/t3.AssetsEnd)")
	} else if (x == "Outflow") {
		z <- paste(x, "= sum(case when Flow < 0 then Flow else 0 end * HoldingValue/t3.AssetsEnd)")
	} else if (x == "FloDollarGross") {
		z <- paste(x, "= sum(abs(Flow) * HoldingValue/t3.AssetsEnd)")
	} else stop("Bad Argument")
#
# RETURN RESULT
	z
}
sql.1dFloMo.select.wrapper <- function(x, y, n, w = F) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.select.wrapper
# Author	: VKS
# Date		: 10/24/18,11/2,11/9/20,12/3,3/14/21,9/28
# Args		: x = a string vector of factors to be computed, the last elements of
#		:	are the type of fund used
#		: y = T/F depending on whether you are checking ftp
#		: n = one or more breakdown filters (e.g. All/GeoId/DomicileId)
#		: w = T/F depending on whether ReportDate must be a column
# Output	: Generates the SQL query to get the data for 1mFloMo for individual stocks
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	x <- sql.arguments(x)$factor
#
# CREATE RESULT
	if (length(n) > 1) {
		z <- n
	} else if (n == "GeoId") {
		z <- "GeoId = GeographicFocusId"
	} else {
		z <- sql.breakdown(n)
	}
	if (y | w) z <- c(sql.yyyymmdd("ReportDate",, y), z)
	z <- c(z, ifelse(y, "HSecurityId", "SecurityId"))
	for (i in x) {
		if (y & i == "FloDollar") {
			z <- c(z, paste("CalculatedStockFlow", txt.right(sql.1dFloMo.select(i), nchar(sql.1dFloMo.select(i)) - nchar(i) - 1)))
		} else {
			z <- c(z, sql.1dFloMo.select(i))
		}
	}
#
# RETURN RESULT
	z
}
sql.1dFloMo.underlying <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMo.underlying
# Author	: VKS
# Date		: 10/1/18,3/8/21
# Args		: x = the date for which you want flows (known one day later)
# Output	: Underlying part of SQL query to get 1dFloMo for individual stocks
# -----------------------------------------------------------------
#
# ALLOCATION MONTH
	x <- yyyymmdd.to.AllocMo.unique(x, 26, T)
#
# CREATE RESULT
	z <- c(sql.into(sql.MonthlyAlloc(paste0("'", x, "'")), "#HLD"))
	z <- c(z, "", sql.into(sql.MonthlyAssetsEnd(paste0("'", x, "'"), F, T), "#AUM"))
	z <- c(sql.drop(c("#HLD", "#AUM")), "", z, "")
#
# RETURN RESULT
	z
}
sql.1dFloMoAggr <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1dFloMoAggr
# Author	: VKS
# Date		: 3/7/17,3/10,5/22,12/18,1/4/18,1/5,1/11,1/12,1/25,10/8,
#		: 11/16/20
# Args		: x = the YYYYMMDD for which you want flows (known two days later)
#		: y = one or more of FwtdIn0/FwtdEx0/SwtdIn0/SwtdEx0
#		: n = any of StockFlows/China/Japan/CSI300/Energy
# Output	: Generates the SQL query to get the data for aggregate 1dFloMo
# -----------------------------------------------------------------
#
# AVERAGE ALLOCATIONS (GEOID LEVEL)
	z <- yyyymm.to.day(yyyymmdd.to.AllocMo(x, 26))
	z <- sql.into(sql.TopDownAllocs.underlying(z, y, n, T), "#ALLOC")
	y <- sql.arguments(y)
#
# DAILY FLOWS
	h <- "GeographicFocusId, Flow = sum(Flow), AssetsStart = sum(AssetsStart)"
	w <- sql.label(sql.FundHistory(y$filter, T, c("FundId", "GeographicFocusId")), "t1")
	w <- c(w, "inner join", "DailyData t2 on t2.HFundId = t1.HFundId")
	h <- sql.tbl(h, w, paste0("ReportDate = '", x, "'"), "GeographicFocusId", "sum(AssetsStart) > 0")
	z <- c(z, "", sql.into(h, "#FLOWS"))
#
# FINAL RESULT
	y <- y$factor
	if (length(y) > 1) {
		y <- paste0(y, " = 100 * sum(Flow * ", y, ")/", sql.nonneg(paste0("sum(AssetsStart * ", y, ")")))
	} else {
		y <- paste0(y, " = 100 * sum(Flow * AverageAllocation)/", sql.nonneg(paste0("sum(AssetsStart * AverageAllocation)")))
	}
	y <- c("SecurityId", y)
	w <- c("#ALLOC t1", "inner join", "#FLOWS t2 on GeographicFocusId = GeoId")
	w <- c(w, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	w <- paste(sql.unbracket(sql.tbl(y, w, , "SecurityId")), collapse = "\n")
	z <- paste(c(sql.drop(c("#FLOWS", "#ALLOC")), "", z), collapse = "\n")
	z <- c(z, w)
#
# RETURN RESULT
	z
}
sql.1dFloTrend <- function(x, y, n, w, h, u = "All") {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend
# Author	: VKS
# Date		: 10/28/16,11/9,11/14,11/16,11/25,1/18/17,3/7,5/22,5/31,6/1,
#		: 6/16,6/20,8/14,1/5/18,1/12,1/23,4/12,10/9,11/2,1/7/19,
#		: 1/25,11/9/20,3/7/21,3/14,3/2/22
# Args		: x = vector of flow dates in YYYYMMDD (known two days later)
#		: y = a string vector of factors to be computed,
#		:       the last element of which is the type of fund used.
#		: n = the delay in knowing allocations
#		: w = any of StockFlows/China/Japan/CSI300/Energy
#		: h = T/F depending on whether you are checking ftp
#		: u = share-class filter (one of All/Inst/Retail)
# Output	: Generates the SQL query to get the data for 1dFloTrend
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# SELECT STATEMENT
	z <- sql.yyyymmdd("ReportDate",, h)
	h <- ifelse(h, "idn.HSecurityId", "idn.SecurityId")
	z <- c(z, h)
	z <- c(z, sapply(vec.to.list(y$factor), sql.1dFloTrend.select))
#
# PRELIMINARY QUERY
	x <- sql.1dFloTrend.underlying(y$filter, w, x, n, u)
#
# FINAL QUERY
	h <- paste0(h, ", ReportDate")
	z <- c(paste(x$PRE, collapse = "\n"), paste(sql.unbracket(sql.tbl(z, x$FINAL, , h)), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.1dFloTrend.Alloc <- function(x, y, n, w = NULL) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.Alloc
# Author	: VKS
# Date		: 3/23/22
# Args		: x = YYYYMM month
#		: y = temp table name (e.g. "#CTRY")
#		: n = identifier column (SectorId/CountryId)
#		: w = vector of acceptable identifiers
# Output	: SQL query for allocation table for FloTrend
# -----------------------------------------------------------------
	z <- sql.drop(y)
	z <- c(z, paste0("create table ", y, " (FundId int not null, ", n, " int not null, Allocation float)"))
	z <- c(z, "", "insert into", paste0("\t", y, " (FundId, ", n, ", Allocation)"), sql.1dFloTrend.Alloc.fetch(x, n, w, F, T))
	z <- c(z, "", "insert into", paste0("\t", y, " (FundId, ", n, ", Allocation)"), sql.1dFloTrend.Alloc.fetch(yyyymm.lag(x), n, w, T, T))
#
# RETURN RESULT
	z
}
sql.1dFloTrend.Alloc.fetch <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.Alloc.fetch
# Author	: VKS
# Date		: 3/23/22,3/29
# Args		: x = YYYYMM month
#		: y = identifier column (SectorId/CountryId)
#		: n = vector of acceptable identifiers
#		: w = T/F depending on whether sign needs to be reversed
#		: h = T/F depending on whether to unbracket
# Output	: SQL query for allocation table for FloTrend
# -----------------------------------------------------------------
	z <- paste0("ReportDate = '", yyyymm.to.day(x), "'")
	if (!is.null(n)) z <- sql.and(list(A = z, B = paste0(y, " in (", paste(n, collapse = ", "), ")")))
	w <- ifelse(w, "Allocation = -Allocation", "Allocation")
	z <- sql.Allocation(c("FundId", y, w), txt.left(y, nchar(y) - 2),,, z)
	if (h) z <- sql.unbracket(z)
#
# RETURN RESULT
	z
}
sql.1dFloTrend.Alloc.final <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.Alloc.final
# Author	: VKS
# Date		: 3/23/22,3/28
# Args		: x = from statement
#		: y = factor (one of FloTrend/FloDiff/FloDiff2)
#		: n = identifier column (SectorId/CountryId)
#		: w = T/F depending on whether daily/weekly
# Output	: SQL query for daily/weekly FloTrend
# -----------------------------------------------------------------
#
# FACTORS
	if (y == "FloTrend") {
		y <- paste(y, sql.Trend("Flow * Allocation", ""))
	} else if (y == "FloDiff") {
		y <- paste(y, sql.Diff("Flow", "Allocation", ""))
	} else if (y == "FloDiff2") {
		y <- paste(y, sql.Diff("Allocation", "Flow", ""))
	} else stop("Bad Argument")
#
# CREATE RESULT
	w <- ifelse(w, "DayEnding", "WeekEnding")
	y <- c(sql.yyyymmdd(w), n, y)
	z <- sql.tbl(y, x,, paste0(w, ", ", n))
	z <- paste(sql.unbracket(z), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dFloTrend.Alloc.from <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.Alloc.from
# Author	: VKS
# Date		: 3/23/22
# Args		: x = flowdate/YYYYMMDD depending on whether daily/weekly
#		: y = temp table name (e.g. "#CTRY")
#		: n = identifier column (SectorId/CountryId)
#		: w = T/F depending on whether daily/weekly
#		: h = vector of filters
# Output	: SQL query for daily/weekly FloTrend
# -----------------------------------------------------------------
	x <- list(A = paste0("'", x, "'"))
	z <- c(ifelse(w, "DayEnding", "WeekEnding"), "FundId", "Flow")
	z <- sql.label(sql.Flow(z, x, c("CB", h, "UI"),, w), "t1")
	#
	r <- c("FundId", n, "Allocation = sum(Allocation)")
	r <- sql.tbl(r, y,, paste(r[1:2], collapse = ", "))
	z <- c(z, "inner join", sql.label(r, "t2"), "\ton t2.FundId = t1.FundId")
#
# RETURN RESULT
	z
}
sql.1dFloTrend.Alloc.purge <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.Alloc.purge
# Author	: VKS
# Date		: 3/23/22
# Args		: x = temp table name (e.g. "#CTRY")
#		: y = identifier column (SectorId/CountryId)
# Output	: Ensures two sets of entries
# -----------------------------------------------------------------
	h <- c("FundId", y)
	z <- sql.tbl(h, x,, paste(h, collapse = ", "), "not count(Allocation) = 2")
	h <- lapply(split(h, h), function(h) paste0(x, ".", h, " = t.", h))
	z <- sql.tbl(c("FundId", y), sql.label(z, "t"), sql.and(h))
	z <- c(paste("delete from", x, "where exists"), z)
#
# RETURN RESULT
	z
}
sql.1dFloTrend.select <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.select
# Author	: VKS
# Date		: 9/21/18,3/11/21,6/14
# Args		: x = desired factor
# Output	: select statement to compute <x>
# -----------------------------------------------------------------
#
# HANDLE SUFFIX
	y <- ""
	if (is.element(txt.right(x, 3), c("Num", "Den"))) {
		y <- txt.right(x, 3)
		x <- txt.left(x, nchar(x) - nchar(y))
	}
#
# CREATE RESULT
	if (is.element(x, paste0("FloTrend", c("", "CB", "PMA")))) {
		z <- paste0(x, y, " ", sql.Trend("Flow * (n1.HoldingValue - o1.HoldingValue)", y))
	} else if (is.element(x, paste0("FloDiff", c("", "CB", "PMA")))) {
		z <- paste0(x, y, " ", sql.Diff("Flow", "n1.HoldingValue - o1.HoldingValue", y))
	} else if (is.element(x, paste0("FloDiff2", c("", "CB", "PMA")))) {
		z <- paste0(x, y, " ", sql.Diff("n1.HoldingValue - o1.HoldingValue", "Flow", y))
	} else stop("Bad Argument")
#
# RETURN RESULT
	z
}
sql.1dFloTrend.underlying <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1dFloTrend.underlying
# Author	: VKS
# Date		: 10/28/16,11/9,1/18/17,3/7,5/31,6/16,8/14,1/4/18,1/5,
#		: 1/9,1/11,1/12,4/12,9/12,10/1,11/2,3/7/21,3/11
# Args		: x = a vector of filters
#		: y = any of All/StockFlows/China/Japan/CSI300/Energy
#		: n = vector of flow dates in YYYYMMDD (known two days later)
#		: w = the delay in knowing allocations
#		: h = share-class filter (one of All/Inst/Retail)
# Output	: Generates the SQL query to get the data for 1dFloTrend
# -----------------------------------------------------------------
#
# BASIC TABLES
	u <- sql.DailyFlo(paste0("'", n, "'"),,, h)
	n <- yyyymmdd.to.AllocMo.unique(n, w, F)
	n <- c(n, yyyymm.lag(n))
	n <- yyyymm.to.day(n)
	#
	z <- c("create table #NEWHLD (FundId int not null, HFundId int not null, HSecurityId int not null, HoldingValue float)")
	z <- c(z, sql.index("#NEWHLD", "FundId, HSecurityId"))
	z <- c(z, "insert into", "\t#NEWHLD (FundId, HFundId, HSecurityId, HoldingValue)", sql.unbracket(sql.MonthlyAlloc(paste0("'", n[1], "'"))))
	z <- c(z, "", sql.into(sql.MonthlyAssetsEnd(paste0("'", n[1], "'"), F, T), "#NEWAUM"))
	z <- c(z, "delete from #NEWHLD where FundId not in (select FundId from #NEWAUM)")
	z <- c(z, "update #NEWHLD set HoldingValue = HoldingValue/AssetsEnd from #NEWAUM where #NEWAUM.FundId = #NEWHLD.FundId")
	#
	z <- c(z, "", "create table #OLDHLD (FundId int not null, HFundId int not null, HSecurityId int not null, HoldingValue float)")
	z <- c(z, sql.index("#OLDHLD", "FundId, HSecurityId"))
	z <- c(z, "insert into", "\t#OLDHLD (FundId, HFundId, HSecurityId, HoldingValue)", sql.unbracket(sql.MonthlyAlloc(paste0("'", n[2], "'"))))
	z <- c(z, "", sql.into(sql.MonthlyAssetsEnd(paste0("'", n[2], "'"), F, T), "#OLDAUM"))
	z <- c(z, "delete from #OLDHLD where FundId not in (select FundId from #OLDAUM)")
	z <- c(z, "update #OLDHLD set HoldingValue = HoldingValue/AssetsEnd from #OLDAUM where #OLDAUM.FundId = #OLDHLD.FundId")
#
# BULK UP HOLDINGS
	if (y != "All") z <- c(z, "", "delete from #NEWHLD where", paste0("\t", sql.in("HSecurityId", sql.RDSuniv(y), F)), "")
	h <- c(sql.drop(c("#NEWHLD", "#NEWAUM", "#OLDHLD", "#OLDAUM")), "", z, "")
#
# FINAL STATEMENT
	z <- sql.label(sql.FundHistory(x, T, "FundId"), "his")
	z <- c(z, "inner join", sql.label(u, "flo on flo.HFundId = his.HFundId"))
	z <- c(z, "inner join", "#NEWHLD n1 on n1.FundId = his.FundId")
	z <- c(z, "inner join", "SecurityHistory idn on idn.HSecurityId = n1.HSecurityId")
	z <- c(z, "inner join", "SecurityHistory ido on ido.SecurityId = idn.SecurityId")
	z <- c(z, "inner join", "#OLDHLD o1 on o1.FundId = his.FundId and o1.HSecurityId = ido.HSecurityId")
#
# CREATE RESULT
	z <- list(PRE = h, FINAL = z)
#
# RETURN RESULT
	z
}
sql.1dFundCt <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1dFundCt
# Author	: VKS
# Date		: 4/1/19,11/9/20,3/19/21,3/22/22
# Args		: x = vector of flow dates in YYYYMMDD (known two days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = one or more breakdown filters (e.g. All/GeoId/DomicileId)
# Output	: Generates FundCt, the ownership breadth measure set forth in
#		:	Chen, Hong & Stein (2001)"Breadth of ownership and stock returns"
# -----------------------------------------------------------------
#
# PARSE ARGUMENTS
	mo.end <- yyyymmdd.to.AllocMo.unique(x, 26, T)
	x <- paste0("'", x, "'")
	if (length(x) == 1) x <- paste("=", x) else x <- paste0("in (", paste(x, collapse = ", "), ")")
	x <- paste("flo.ReportDate", x)
	y <- sql.arguments(y)
#
# FILTERS
	if (n != "All") n <- list(A = sql.in("h.HSecurityId", sql.RDSuniv(n))) else n <- list()
	n[[char.ex.int(length(n) + 65)]] <- paste0("h.ReportDate = '", mo.end, "'")
	n[[char.ex.int(length(n) + 65)]] <- x
	if (y$filter != "All") n[[char.ex.int(length(n) + 65)]] <- sql.FundHistory.sf(y$filter)
	if (length(n) == 1) n <- n[[1]] else n <- sql.and(n)
#
# SELECT STATEMENT
	if (all(h == "GeoId")) z <- "GeoId = GeographicFocusId" else z <- setdiff(h, "All")
	if (w) z <- c(z, "HSecurityId") else z <- c("SecurityId", z)
	if (w) z <- c(sql.yyyymmdd("flo.ReportDate", "ReportDate", w), z)
	for (j in y$factor) {
		if (j == "FundCt") {
			z <- c(z, paste(j, "count(distinct flo.HFundId)", sep = " = "))
		} else {
			stop("Bad factor", j)
		}
	}
#
# FROM
	u <- c("inner join", "Holdings h on h.FundId = his.FundId")
	u <- c("DailyData flo", "inner join", "FundHistory his on his.HFundId = flo.HFundId", u)
	if (!w) u <- c(u, "inner join", "SecurityHistory id on id.HSecurityId = h.HSecurityId")
#
# GROUP BY CLAUSE
	if (w) w <- c("flo.ReportDate", "HSecurityId") else w <- "SecurityId"
	w <- paste(c(w, sql.breakdown(h)), collapse = ", ")
#
# FINAL RESULT
	z <- paste(sql.unbracket(sql.tbl(z, u, n, w)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1dFundRet <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1dFundRet
# Author	: VKS
# Date		: 11/1/17,1/5/18,1/11,1/12,11/23/20
# Args		: x = a list of fund identifiers
# Output	: Generates the SQL query to get monthly AIS for countries
# -----------------------------------------------------------------
	x <- sql.tbl("HFundId, FundId", "FundHistory", sql.in("FundId", paste0("(", paste(x, collapse = ", "), ")")))
	x <- c("DailyData t1", "inner join", sql.label(x, "t2"), "\ton t2.HFundId = t1.HFundId")
	z <- c(sql.yyyymmdd("DayEnding"), "FundId", "FundRet = sum(PortfolioChange)/sum(AssetsStart)")
	z <- paste(sql.unbracket(sql.tbl(z, x, , "DayEnding, FundId", "sum(AssetsStart) > 0")), collapse = "\n")
	z
}
sql.1dION <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1dION
# Author	: VKS
# Date		: 9/14/17,1/5/18,1/9,1/12,1/23,1/7/19,11/18/20
# Args		: x = data date (known two days later)
#		: y = a vector of variables, the last element of which is fund type used
#		: n = the delay in knowing allocations
#		: w = any of StockFlows/China/Japan/CSI300/Energy
#		: h = T/F depending on whether you are checking ftp
# Output	: Generates the SQL query to get the data for 1dION$ & 1dION%
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# # SELECT STATEMENT
	u <- vec.named(c("Flow * HoldingValue/AssetsEnd", "HoldingValue/AssetsEnd"), c("ION$", "ION%"))
	if (h) z <- c(sql.ReportDate(x), "t1.HSecurityId") else z <- "SecurityId"
	z <- c(z, paste0("[", y$factor, "] ", sql.ION("Flow", u[y$factor])))
#
# FROM STATEMENT
	y <- c(sql.label(sql.FundHistory(y$filter, T, "FundId"), "t0"), "inner join", sql.MonthlyAlloc("@allocDt"))
	y <- c(sql.label(y, "t1"), "\ton t1.FundId = t0.FundId", "inner join", sql.DailyFlo("@floDt"))
	y <- c(sql.label(y, "t2"), "\ton t2.HFundId = t0.HFundId", "inner join", sql.MonthlyAssetsEnd("@allocDt"))
	y <- c(sql.label(y, "t3"), "\ton t3.HFundId = t1.HFundId")
	if (!h) y <- c(y, "inner join", "SecurityHistory id", "\ton id.HSecurityId = t1.HSecurityId")
	x <- sql.declare(c("@floDt", "@allocDt"), "datetime", c(x, yyyymm.to.day(yyyymmdd.to.AllocMo(x, n))))
	h <- ifelse(h, "t1.HSecurityId", "SecurityId")
	z <- sql.unbracket(sql.tbl(z, y, sql.in("t1.HSecurityId", sql.RDSuniv(w)), h))
	z <- paste(c(x, z), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1mActWt <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1mActWt
# Author	: VKS
# Date		: 11/21/17,1/5/18,1/9,1/12
# Args		: x = the YYYYMM for which you want data (known 24 days later)
#		: y = a string vector, the elements of which are:
#		:	1) FundId for the fund used as the benchmark
#		:	2) BenchIndexId of the benchmark
# Output	: Generates the SQL query to get the following active weights:
#		:	a) EqlAct = equal weight average (incl 0) less the benchmark
#		:	b) CapAct = fund weight average (incl 0) less the benchmark
#		:	c) PosAct = fund weight average (incl 0) less the benchmark (positive flows only)
#		:	d) NegAct = fund weight average (incl 0) less the benchmark (negative flows only)
# -----------------------------------------------------------------
	w <- c("Eql", "Cap", "Pos", "Neg")
	w <- c("SecurityId", paste0(w, "Act = ", w, "Wt - BmkWt"))
	z <- c("SecurityId", "EqlWt = sum(HoldingValue/AssetsEnd)/count(AssetsEnd)", "CapWt = sum(HoldingValue)/sum(AssetsEnd)", "BmkWt = avg(BmkWt)")
	z <- c(z, "PosWt = sum(case when Flow > 0 then HoldingValue else NULL end)/sum(case when Flow > 0 then AssetsEnd else NULL end)")
	z <- c(z, "NegWt = sum(case when Flow < 0 then HoldingValue else NULL end)/sum(case when Flow < 0 then AssetsEnd else NULL end)")
	z <- sql.unbracket(sql.tbl(w, sql.label(sql.tbl(z, sql.1mActWt.underlying(0, "\t"), , "SecurityId"), "t")))
	z <- paste(c(sql.declare(c("@fundId", "@bmkId", "@allocDt"), c("int", "int", "datetime"), c(y, yyyymm.to.day(x))), z), collapse = "\n")
	z
}
sql.1mActWt.underlying <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1mActWt.underlying
# Author	: VKS
# Date		: 11/21/17,1/4/18,1/5,1/9,1/10,1/11
# Args		: x = the month for which you want data (0 = latest, 1 = lagged one month, etc.)
#		: y = characters you want put in front of the query
# Output	: Generates tail end of an SQL query
# -----------------------------------------------------------------
	w <- list(A = paste("datediff(month, ReportDate, @allocDt) =", x), B = sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "FundId = @fundId")))
	z <- c(sql.label(sql.tbl("HSecurityId, HoldingValue", "Holdings", sql.and(w)), "t1"), "cross join")
	w <- list(A = paste("datediff(month, ReportDate, @allocDt) =", x), B = sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "FundId = @fundId")))
	z <- c(z, sql.label(sql.tbl("AssetsEnd = sum(AssetsEnd)", "MonthlyData", sql.and(w)), "t2"))
	z <- sql.label(paste0("\t", sql.tbl("HSecurityId, BmkWt = HoldingValue/AssetsEnd", z)), "t0 -- Securities in the benchmark At Month End")
	w <- list(A = paste("datediff(month, ReportDate, @allocDt) =", x))
	w[["B"]] <- sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "BenchIndexId = @bmkId"))
	w[["C"]] <- sql.in("HFundId", sql.Holdings(paste("datediff(month, ReportDate, @allocDt) =", x), "HFundId"))
	w <- paste0("\t", sql.tbl("HFundId, Flow = sum(Flow), AssetsEnd = sum(AssetsEnd)", "MonthlyData", sql.and(w), "HFundId", "sum(AssetsEnd) > 0"))
	z <- c(z, "cross join", sql.label(w, "t1 -- Funds Reporting Both Monthly Flows and Allocations with the right benchmark"))
	z <- c(z, "left join", paste0("\t", sql.Holdings(paste("datediff(month, ReportDate, @allocDt) =", x), c("HSecurityId", "HFundId", "HoldingValue"))))
	z <- c(sql.label(z, "t2"), "\t\ton t2.HFundId = t1.HFundId and t2.HSecurityId = t0.HSecurityId", "inner join")
	z <- c(z, "\tSecurityHistory id on id.HSecurityId = t0.HSecurityId")
	z <- paste0(y, z)
	z
}
sql.1mActWtIncrPct <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1mActWtIncrPct
# Author	: VKS
# Date		: 8/26/21,11/24
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Generates the SQL query to get the data for 1mAllocMo
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# CURRENT WEIGHTS
	v <- sql.1mAllocD.from(yyyymm.to.day(x), y$filter, c("FundId", "[Index]", "BenchIndexId"))
	v <- c(v, "inner join", "BenchIndexes t4 on [Id] = BenchIndexId")
	u <- sql.and(list(A = paste0("ReportDate = '", yyyymm.to.day(x), "'"), B = "HoldingValue > 0"))
	z <- sql.into(sql.tbl(c("his.FundId", "SecurityId", "[Index]", "[Description]", "Allocation = HoldingValue/AssetsEnd"), v, u), "#NEW")
#
# PRIOR WEIGHTS
	v <- sql.1mAllocD.from(yyyymm.to.day(yyyymm.lag(x)), "All", "FundId")
	v <- c(v, "inner join", "BenchIndexes t4 on [Id] = BenchIndexId")
	u <- list(A = paste0("ReportDate = '", yyyymm.to.day(yyyymm.lag(x)), "'"), B = "HoldingValue > 0")
	u <- sql.and(u)
	u <- sql.into(sql.tbl(c("t2.FundId", "SecurityId", "[Index]", "[Description]", "Allocation = HoldingValue/AssetsEnd"), v, u), "#OLD")
	z <- c(sql.drop(c("#NEW", "#OLD")), "", z, "", u)
#
# THROW OUT CRAP
	for (j in c("#NEW", "#OLD")) {
		v <- sql.tbl("[Description]", j, "[Index] = 0", "[Description]")
		v <- sql.in("[Description]", v, F)
		v <- sql.unbracket(sql.tbl("", j, v))
		v[1] <- "delete from"
		v <- v[-2][-2]
		z <- c(z, "", v)
		#
		v <- sql.tbl("[Description]", j, "[Index] = 1", "[Description]")
		v <- sql.in("[Description]", v, F)
		v <- sql.unbracket(sql.tbl("", j, v))
		v[1] <- "delete from"
		v <- v[-2][-2]
		z <- c(z, "", v)
	}
#
# BENCHMARK NAMES NOT HELD
	for (j in c("#NEW", "#OLD")) {
		u <- c("FundId", "[Index]", "[Description]")
		u <- sql.label(sql.tbl(u, j,, paste(u, collapse = ", ")), "t1")
		#
		v <- c("SecurityId", "[Description]")
		v <- sql.tbl(v, j, "[Index] = 1", paste(v, collapse = ", "))
		v <- sql.label(v, "t2 on t2.[Description] = t1.[Description]")
		v <- c(u, "inner join", v)
		v <- sql.tbl(c("FundId", "SecurityId", "[Index]", "t1.[Description]", "Allocation = 0"), v)
		v <- sql.label(v, "t1")
		#
		u <- list(A = "t2.FundId = t1.FundId", B = "t2.SecurityId = t1.SecurityId")
		u <- sql.exists(sql.tbl(c("FundId", "SecurityId"), sql.label(j, "t2"), sql.and(u)), F)
		v <- sql.tbl(c("FundId", "SecurityId", "[Index]", "[Description]", "Allocation"), v, u)
		v <- sql.unbracket(v)
		v <- c("insert into", paste0("\t", j, " (FundId, SecurityId, [Index], [Description], Allocation)"), v)
		z <- c(z, "", v)
	}
#
# ACTIVE WEIGHT
	for (j in c("#NEW", "#OLD")) {
		u <- c("[Description]", "SecurityId", "Allocation = avg(Allocation)")
		u <- sql.label(sql.tbl(u, j, "[Index] = 1", "[Description], SecurityId"), "t")
		#
		v <- vec.to.list(c("[Description]", "SecurityId"))
		v <- lapply(v, function(x) paste0(j, ".", x, " = t.", x))
		v <- sql.tbl(paste0("Allocation = ", j, ".Allocation - t.Allocation"), u, sql.and(v))
		v <- sql.unbracket(v)
		v[1] <- paste("update", j, "set")
		z <- c(z, "", v)
		#
		z <- c(z, "", paste("delete from", j, "where [Index] = 1"))
	}
#
# COMMON FUNDS
	z <- c(z, "", "delete from #NEW where FundId not in (select FundId from #OLD)")
	z <- c(z, "", "delete from #OLD where FundId not in (select FundId from #NEW)")
	v <- paste(z, collapse = "\n")
#
# TOP-LINE SELECT
	if (!w) {
		z <- "SecurityId = isnull(t1.SecurityId, t2.SecurityId)"
	} else {
		z <- c(sql.ReportDate(yyyymm.to.day(x)), "HSecurityId")
	}
	u <- "sum(case when t1.Allocation > t2.Allocation then 1.0 else 0.0 end)"
	u <- paste0(u, "/count(isnull(t1.SecurityId, t2.SecurityId))")
	u <- paste("ActWtIncrPct =", u)
	z <- c(z, u)
	if (any(y$factor != "ActWtIncrPct")) stop("Can't handle this!")
#
# TOP-LINE FROM
	u <- sql.1mAllocD.topline.from(x, w)
#
# FINAL
	w <- ifelse(w, "HSecurityId", "isnull(t1.SecurityId, t2.SecurityId)")
	z <- sql.tbl(z, u,, w, "count(isnull(t1.SecurityId, t2.SecurityId)) > 1")
	z <- paste(sql.unbracket(z), collapse = "\n")
	z <- c(v, z)
#
# RETURN RESULT
	z
}
sql.1mActWtTrend <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1mActWtTrend
# Author	: VKS
# Date		: 10/22/20,3/8/21
# Args		: x = the YYYYMM for which you want data
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: the SQL query to get 1dActWtTrend
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# CREATE RESULT
	z <- sql.1mActWtTrend.underlying(x, y$filter, sql.RDSuniv(n))
	z <- c(z, sql.1dActWtTrend.topline(y$factor, w))
#
# RETURN RESULT
	z
}
sql.1mActWtTrend.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1mActWtTrend.underlying
# Author	: VKS
# Date		: 10/22/20,3/8/21,11/10,11/24
# Args		: x = the YYYYMM for which you want data
#		: y = the type of fund used in the computation
#		: n = "" or the SQL query to subset to securities desired
# Output	: the SQL query to get the data for 1dActWtTrend
# -----------------------------------------------------------------
	x <- yyyymm.to.day(x)
#
# BASIC TABLES
	z <- c("MonthlyData t1", "inner join", sql.label(sql.FundHistory(y, T, c("FundId", "GeographicFocusId")), "t2"), "on t2.HFundId = t1.HFundId")
	z <- sql.tbl("ReportDate, FundId, GeographicFocusId, Flow = sum(Flow), AssetsStart = sum(AssetsStart)", z, paste0("ReportDate = '", x, "'"), "ReportDate, FundId, GeographicFocusId")
	z <- sql.1dActWtTrend.underlying.basic(z, x)
#
# BULK UP HOLDINGS
	z <- c(z, sql.Holdings.bulk.wrapper("#HLD", y, x, "#BMKHLD", "#BMKAUM"))
#
# SUBSET HOLDINGS
	if (n[1] != "") z <- c(z, "", "delete from #HLD where", paste0("\t", sql.in("HSecurityId", n, F)))
	z <- c(z, "", "delete from #HLD where", paste0("\t", sql.in("FundId", sql.tbl("FundId", "#FLO"), F)), "")
#
# CREATE RESULT
	z <- paste(z, collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1mAllocD <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocD
# Author	: VKS
# Date		: 9/16/19,11/3/20,11/9,11/24/21,3/21/22
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = T/F depending on whether latest prices are being used
# Output	: Generates the SQL query to get the data for 1mAllocMo
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# CURRENT WEIGHTS
	v <- sql.1mAllocD.from(yyyymm.to.day(x), y$filter, "FundId")
	u <- sql.and(list(A = paste0("ReportDate = '", yyyymm.to.day(x), "'"), B = "HoldingValue > 0"))
	z <- sql.into(sql.tbl(c("his.FundId", "SecurityId", "HoldingValue", "SharesHeld", "Allocation = HoldingValue/AssetsEnd"), v, u), "#NEW")
#
# PRIOR WEIGHTS
	v <- sql.1mAllocD.from(yyyymm.to.day(yyyymm.lag(x)), "All", "FundId")
	u <- list(A = paste0("ReportDate = '", yyyymm.to.day(yyyymm.lag(x)), "'"), B = "HoldingValue > 0")
	u[["C"]] <- sql.in("t2.FundId", sql.tbl("FundId", "#NEW"))
	u <- sql.and(u)
	u <- sql.into(sql.tbl(c("t2.FundId", "SecurityId", "HoldingValue", "SharesHeld", "Allocation = HoldingValue/AssetsEnd"), v, u), "#OLD")
	z <- c(sql.drop(c("#NEW", "#OLD")), "", z, "", u)
#
# PRICE CONTROL (RE-WEIGHT OLD WITH LATEST PRICES)
	if (h) {
	#
	# RECORD CASH POSITION
		h <- c("MonthlyData t1", "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
		v <- c("FundId", "AUM = sum(AssetsEnd)")
		h <- sql.tbl(v, h, paste0("ReportDate = '", yyyymm.to.day(yyyymm.lag(x)), "'"), "FundId")
		#
		v <- c("FundId", "HoldingValue = sum(HoldingValue)", "Allocation = sum(Allocation)")
		v <- sql.label(sql.tbl(v, "#OLD",, "FundId"), "t2")
		v <- c(sql.label(h, "t1"), "inner join", v, "\ton t2.FundId = t1.FundId")
		#
		h <- c("t1.FundId", "SecurityId = -999", "HoldingValue = AUM - HoldingValue")
		h <- c(h, "SharesHeld = 1e6 * (AUM - HoldingValue)", "Allocation = 1 - Allocation")
		z <- c(z, "", "insert into #OLD", sql.unbracket(sql.tbl(h, v)))
	#
	# UPDATE PRICES
		h <- c("SecurityId", "Prc = 1e6 * sum(HoldingValue)/sum(SharesHeld)")
		v <- sql.label(sql.tbl(h, "#OLD",, "SecurityId", "sum(SharesHeld) > 0"), "t1")
		h <- sql.label(sql.tbl(h, "#NEW",, "SecurityId", "sum(SharesHeld) > 0"), "t2")
		v <- c(v, "left join", h, "\ton t2.SecurityId = t1.SecurityId")
		h <- c("t1.SecurityId", "Prc = isnull(t2.Prc, t1.Prc)")
		v <- sql.label(sql.tbl(h, v), "t")
		h <- "HoldingValue = 1e-6 * SharesHeld * Prc"
		v <- sql.tbl(h, v, "#OLD.SecurityId = t.SecurityId")
		v <- sql.unbracket(v)
		v[1] <- "update #OLD set"
		z <- c(z, "", v)
	#
	# UPDATE WEIGHTS
		v <- c("FundId", "AUM = sum(HoldingValue)")
		v <- sql.label(sql.tbl(v, "#OLD",, "FundId", "sum(HoldingValue) > 0"), "t")
		v <- sql.tbl("Allocation = HoldingValue/AUM", v, "#OLD.FundId = t.FundId")
		v <- sql.unbracket(v)
		v[1] <- "update #OLD set"
		z <- c(z, "", v)
	#
	# REMOVE CASH
		z <- c(z, "", "delete from #OLD where SecurityId = -999")
	}
#
# COMMON FUNDS
	v <- paste(c(z, "", "delete from #NEW where FundId not in (select FundId from #OLD)"), collapse = "\n")
#
# TOP-LINE SELECT
	if (!w) {
		z <- "SecurityId = isnull(t1.SecurityId, t2.SecurityId)"
	} else {
		z <- c(sql.ReportDate(yyyymm.to.day(x)), "HSecurityId")
	}
	for (i in y$factor) z <- c(z, sql.1mAllocD.select(i))
#
# TOP-LINE FROM
	u <- sql.1mAllocD.topline.from(x, w)
#
# FINAL
	if (w & n == "All") {
		w <- "HSecurityId"
		z <- sql.tbl(z, u,, w, "count(isnull(t1.SecurityId, t2.SecurityId)) > 1")
	} else if (w) {
		w <- "HSecurityId"
		z <- sql.tbl(z, u, sql.in("HSecurityId", sql.RDSuniv(n)), w, "count(isnull(t1.SecurityId, t2.SecurityId)) > 1")
	} else {
		w <- "isnull(t1.SecurityId, t2.SecurityId)"
		z <- sql.tbl(z, u,, w, "count(isnull(t1.SecurityId, t2.SecurityId)) > 1")
	}
	z <- paste(sql.unbracket(z), collapse = "\n")
	z <- c(v, z)
#
# RETURN RESULT
	z
}
sql.1mAllocD.from <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocD.from
# Author	: VKS
# Date		: 11/24/21
# Args		: x = the YYYYMMDD for which you want data (known 26 days later)
#		: y = the type of fund used in the computation
#		: n = columns needed in addition to HFundId
# Output	: Generates the SQL query to get the data for 1mAllocMo
# -----------------------------------------------------------------
	z <- paste0("'", x, "'")
	z <- sql.label(sql.MonthlyAssetsEnd(z), "t1")
	z <- c(z, "inner join", sql.label(sql.FundHistory(y, T, n), "his on his.HFundId = t1.HFundId"))
	z <- c(z, "inner join", "Holdings t2 on t2.FundId = his.FundId")
	z <- c(z, "inner join", "SecurityHistory t3 on t3.HSecurityId = t2.HSecurityId")
	z
}
sql.1mAllocD.select <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocD.select
# Author	: VKS
# Date		: 9/16/19,11/3/20,11/5,12/25
# Args		: x = the factor to be computed
# Output	: select term to compute <x>
# -----------------------------------------------------------------
	z <- vec.read(parameters("classif-AllocD"), "\t")
	if (any(x == names(z))) z <- as.character(z[x]) else stop("Bad Argument")
	z <- paste0("[", x, "] = ", z)
	z
}
sql.1mAllocD.topline.from <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocD.topline.from
# Author	: VKS
# Date		: 11/10/21
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = T/F depending on whether you are checking ftp
# Output	: from statement of final query of 1mAllocD
# -----------------------------------------------------------------
	if (y) {
		z <- sql.Holdings(paste0("ReportDate = '", yyyymm.to.day(x), "'"), "HSecurityId")
		z <- sql.in("HSecurityId", z)
		z <- sql.label(sql.tbl(c("SecurityId", "HSecurityId"), "SecurityHistory", z), "t3")
		z <- c("inner join", z, "\ton t3.SecurityId = isnull(t1.SecurityId, t2.SecurityId)")
		z <- c("#OLD t2 on t2.FundId = t1.FundId and t2.SecurityId = t1.SecurityId", z)
		z <- c("#NEW t1", "full outer join", z)
	} else {
		z <- c("#NEW t1", "full outer join")
		z <- c(z, "#OLD t2 on t2.FundId = t1.FundId and t2.SecurityId = t1.SecurityId")
	}
#
# RETURN RESULT
	z
}
sql.1mAllocMo <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocMo
# Author	: VKS
# Date		: 10/28/16,11/9,11/14,6/16/17,6/20,1/5/18,1/12,1/23,10/1,
#		: 10/9,10/22,11/2,11/9/20
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Generates the SQL query to get the data for 1mAllocMo
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# TOP-LINE
	if (w) z <- c(sql.ReportDate(yyyymm.to.day(x)), "n1.HSecurityId") else z <- "n1.SecurityId"
	for (i in y$factor) z <- c(z, sql.1mAllocMo.select(i, any(y$filter == "Num")))
#
# CREATE RESULT
	h <- sql.1mAllocMo.underlying.pre(y$filter, yyyymm.to.day(x), yyyymm.to.day(yyyymm.lag(x)))
	y <- sql.1mAllocMo.underlying.from(y$filter)
	if (w) {
		if (n == "All") {
			z <- sql.tbl(z, y, , "n1.HSecurityId")
		} else {
			z <- sql.tbl(z, y, sql.in("n1.HSecurityId", sql.RDSuniv(n)), "n1.HSecurityId")
		}
	} else {
		y <- c(y, "inner join", "SecurityHistory id on id.HSecurityId = n1.HSecurityId")
		z <- sql.tbl(z, y, sql.in("n1.HSecurityId", sql.RDSuniv(n)), "n1.SecurityId")
	}
	z <- paste(sql.unbracket(z), collapse = "\n")
	z <- c(paste(h, collapse = "\n"), z)
#
# RETURN RESULT
	z
}
sql.1mAllocMo.select <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocMo.select
# Author	: VKS
# Date		: 10/1/18,10/23
# Args		: x = the factor to be computed
#		: y = T/F depending on whether only the numerator is wanted
# Output	: select term to compute <x>
# -----------------------------------------------------------------
	if (x == "AllocMo") {
		z <- "2 * sum((AssetsStart + AssetsEnd) * (n1.HoldingValue/AssetsEnd - o1.HoldingValue/AssetsStart))"
		if (!y) z <- paste0(z, "/", sql.nonneg("sum((AssetsStart + AssetsEnd) * (n1.HoldingValue/AssetsEnd + o1.HoldingValue/AssetsStart))"))
	} else if (x == "AllocDiff" & y) {
		z <- "sum((AssetsStart + AssetsEnd) * sign(n1.HoldingValue/AssetsEnd - o1.HoldingValue/AssetsStart))"
	} else if (x == "AllocDiff" & !y) {
		z <- sql.Diff("AssetsStart + AssetsEnd", "n1.HoldingValue/AssetsEnd - o1.HoldingValue/AssetsStart")
		z <- txt.right(z, nchar(z) - nchar("= "))
	} else if (x == "AllocTrend") {
		z <- "sum((AssetsStart + AssetsEnd) * (n1.HoldingValue/AssetsEnd - o1.HoldingValue/AssetsStart))"
		if (!y) z <- paste0(z, "/", sql.nonneg("sum(abs((AssetsStart + AssetsEnd) * (n1.HoldingValue/AssetsEnd - o1.HoldingValue/AssetsStart)))"))
	} else stop("Bad Argument")
	z <- paste(x, z, sep = " = ")
#
# RETURN RESULT
	z
}
sql.1mAllocMo.underlying.from <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocMo.underlying.from
# Author	: VKS
# Date		: 10/28/16,11/9,11/25,1/18/17,1/4/18,1/5,1/9,1/10,1/12,
#		: 1/23,4/11,6/18,7/16,8/20,10/1
# Args		: x = filter list
# Output	: FROM for 1mAllocMo
# -----------------------------------------------------------------
	z <- c("#MOFLOW t", "inner join", sql.label(sql.FundHistory(x, T, "FundId"), "his"), "\ton his.HFundId = t.HFundId")
	#
	y <- c("#NEWHLD t", "inner join", "SecurityHistory id on id.HSecurityId = t.HSecurityId")
	y <- sql.label(sql.tbl("FundId, HFundId, t.HSecurityId, SecurityId, HoldingValue", y), "n1")
	z <- c(z, "inner join", y, "\ton n1.FundId = his.FundId")
	#
	y <- c("#OLDHLD t", "inner join", "SecurityHistory id on id.HSecurityId = t.HSecurityId")
	y <- sql.label(sql.tbl("FundId, HFundId, t.HSecurityId, SecurityId, HoldingValue", y), "o1")
	z <- c(z, "inner join", y, "\ton o1.FundId = his.FundId and o1.SecurityId = n1.SecurityId")
#
# RETURN RESULT
	z
}
sql.1mAllocMo.underlying.pre <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocMo.underlying.pre
# Author	: VKS
# Date		: 10/28/16,11/9,11/25,1/18/17,1/4/18,1/5,1/9,1/10,1/12,
#		: 1/23,4/11,6/18,7/16,8/20,10/1,10/3,11/2,3/13/19
# Args		: x = filter list
#		: y = date for new holdings in YYYYMMDD
#		: n = date for old holdings in YYYYMMDD
# Output	: FROM and WHERE for 1mAllocMo
# -----------------------------------------------------------------
#
# BASIC TABLES
	z <- sql.into(sql.MonthlyAssetsEnd(paste0("'", y, "'"), T), "#MOFLOW")
	if (any(x == "Up")) z <- c(z, "\tand", "\t\tsum(AssetsEnd - AssetsStart - Flow) > 0")
	#
	z <- c(z, "", sql.into(sql.MonthlyAlloc(paste0("'", y, "'")), "#NEWHLD"))
	z <- c(z, "", sql.into(sql.MonthlyAlloc(paste0("'", n, "'")), "#OLDHLD"))
#
# MODIFY HOLDINGS
	z <- c(z, sql.Holdings.bulk.wrapper("#NEWHLD", x, y, "#BMKHLD", "#BMKAUM"))
	z <- c(z, sql.Holdings.bulk.wrapper("#OLDHLD", x, n, "#OLDBMKHLD", "#OLDBMKAUM"))
	z <- c(sql.drop(c("#MOFLOW", "#NEWHLD", "#OLDHLD")), "", z, "")
#
# RETURN RESULT
	z
}
sql.1mAllocSkew <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocSkew
# Author	: VKS
# Date		: 11/8/16,11/28,3/2/17,6/16,9/29,1/5/18,1/9,1/11,1/23,
#		: 1/24/18,6/18,7/16,8/20,10/1,10/2,10/8,1/7/19,3/13,
#		: 11/9/20
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Generates the SQL query to get the data for 1mAllocTrend
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# BASICS
	x <- yyyymm.to.day(x)
	cols <- c("HFundId", "FundId", "HSecurityId", "HoldingValue")
	#
	z <- sql.into(sql.tbl("HFundId, PortVal = sum(AssetsEnd)", "MonthlyData", paste0("ReportDate = '", x, "'"), "HFundId", "sum(AssetsEnd) > 0"), "#AUM")
	z <- c(sql.drop(c("#AUM", "#HLD")), "", z, "")
	#
	h <- paste0("ReportDate = '", x, "'")
	if (n != "All") h <- sql.and(list(A = h, B = sql.in("HSecurityId", sql.RDSuniv(n))))
	z <- c(z, sql.Holdings(h, cols, "#HLD"), "")
#
# MODIFY HOLDINGS
	if (any(y$filter == "Pseudo")) z <- c(z, sql.Holdings.bulk("#HLD", cols, x, "#BMKHLD", "#BMKAUM"), "")
	if (any(y$filter == "Up")) {
		h <- sql.tbl("HFundId", "MonthlyData", paste0("ReportDate = '", x, "'"), "HFundId", "sum(AssetsEnd - AssetsStart - Flow) < 0")
		z <- c(z, c("delete from #HLD where", sql.in("HFundId", h)), "")
	}
#
# CREATE RESULT
	if (w) x <- c(sql.ReportDate(x), "n1.HSecurityId") else x <- "SecurityId"
	if (length(y$factor) != 1 | y$factor[1] != "AllocSkew") stop("Bad Argument")
	h <- "AllocSkew = sum(PortVal * sign(FundWtdExcl0 - n1.HoldingValue/PortVal))"
	x <- c(x, paste0(h, "/", sql.nonneg("sum(PortVal)")))
	h <- sql.1mAllocSkew.topline.from(y$filter)
	if (!w) h <- c(h, "inner join", "SecurityHistory id on id.HSecurityId = n1.HSecurityId")
#
# FINAL RESULT
	w <- ifelse(w, "n1.HSecurityId", "SecurityId")
	z <- c(paste(z, collapse = "\n"), paste(sql.unbracket(sql.tbl(x, h, , w)), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.1mAllocSkew.topline.from <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1mAllocSkew.topline.from
# Author	: VKS
# Date		: 11/8/16,11/28,3/2/17,6/16,9/29,1/5/18,1/9,1/11,1/23,
#		: 1/24/18,6/18,7/16,8/20,10/1,10/2
# Args		: x = filter to be applied All/Act/Pas/Mutual/Etf/xJP
# Output	: from part of the final select statement in 1mAllocTrend
# -----------------------------------------------------------------
	z <- c("HSecurityId", "GeographicFocusId", "FundWtdExcl0 = sum(HoldingValue)/sum(PortVal)")
	y <- c("#AUM t3", "inner join", sql.label(sql.FundHistory(x, T, c("FundId", "GeographicFocusId")), "t1"), "\ton t1.HFundId = t3.HFundId")
	y <- c(y, "inner join", "#HLD t2 on t2.FundId = t1.FundId")
	z <- sql.tbl(z, y, , "HSecurityId, GeographicFocusId")
	z <- c("inner join", sql.label(z, "mnW"), "\ton mnW.GeographicFocusId = his.GeographicFocusId and mnW.HSecurityId = n1.HSecurityId")
	z <- c("inner join", "#HLD n1 on n1.FundId = his.FundId", z)
	z <- c(sql.label(sql.FundHistory(x, T, c("FundId", "GeographicFocusId")), "his"), "\ton his.HFundId = t.HFundId", z)
	z <- c("#AUM t", "inner join", z)
#
# RETURN RESULT
	z
}
sql.1mBullish.Alloc <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1mBullish.Alloc
# Author	: VKS
# Date		: 11/24/21
# Args		: x = SQL statement
#		: y = SectorId/CountryId
#		: n = name of SQL temp table
# Output	: SQL query for monthly Bullish sector indicator
# -----------------------------------------------------------------
	z <- paste("create table", n, "(FundId int not null,", y, "int not null, BenchIndex int, Idx char(1), Allocation float)")
	z <- c(z, c(sql.index(n, paste("FundId,", y)), x))
	z <- c(z, "", sql.BenchIndex.duplication(n))
	z <- c(z, "", paste("update", n, "set Idx = 'N' where Idx is NULL"))
	z <- paste(c(sql.drop(n), "", z), collapse = "\n")
	z
}
sql.1mBullish.Final <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1mBullish.Final
# Author	: VKS
# Date		: 11/24/21
# Args		: x = SectorId/CountryId
#		: y = name of SQL temp table
# Output	: SQL query for monthly Bullish sector indicator
# -----------------------------------------------------------------
	r <- "Bullish = 100 * sum(case when t1.Allocation > t2.Allocation then 1.0 else 0.0 end)/count(t1.FundId)"
	r <- c(paste0("t1.", x), r)
	#
	z <- paste0("BenchIndex, ", x, ", Allocation = avg(Allocation)")
	z <- sql.tbl(z, y, "Idx = 'Y'", paste("BenchIndex,", x))
	z <- sql.label(z, paste0("t2 on t2.BenchIndex = t1.BenchIndex and t2.", x, " = t1.", x))
	z <- c(paste(y, "t1"), "inner join", z)
	z <- sql.unbracket(sql.tbl(r, z, "Idx = 'N'", paste0("t1.", x)))
	z <- paste(z, collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1mChActWt <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.1mChActWt
# Author	: VKS
# Date		: 11/22/17,1/5/18,1/12
# Args		: x = the YYYYMM for which you want data (known 24 days later)
#		: y = a string vector, the elements of which are:
#		:	1) FundId for the fund used as the benchmark
#		:	2) BenchIndexId of the benchmark
# Output	: Generates the SQL query to get the following active weights:
#		:	a) EqlChAct = equal weight average change in active weight
#		:	b) BegChAct = beginning-of-period-asset weighted change in active weight
#		:	c) EndChAct = end-of-period-asset weighted change in active weight
#		:	d) BegPosChAct = beginning-of-period-asset weighted change in active weight (positive flows only)
#		:	e) EndPosChAct = end-of-period-asset weighted change in active weight (positive flows only)
#		:	f) BegNegChAct = beginning-of-period-asset weighted change in active weight (negative flows only)
#		:	g) EndNegChAct = end-of-period-asset weighted change in active weight (negative flows only)
# -----------------------------------------------------------------
	x <- sql.declare(c("@fundId", "@bmkId", "@allocDt"), c("int", "int", "datetime"), c(y, yyyymm.to.day(x)))
#
# LATEST ACTIVE WEIGHT
	w <- sql.tbl("SecurityId, t1.HFundId, ActWt = isnull(HoldingValue, 0)/AssetsEnd - BmkWt, AssetsEnd, Flow", sql.1mActWt.underlying(0, ""))
	z <- c("FundHistory t1", "inner join", sql.label(w, "t2"), "\ton t2.HFundId = t1.HFundId", "inner join", "FundHistory t3")
#
# PREVIOUS-MONTH ACTIVE WEIGHT
	w <- sql.tbl("SecurityId, t1.HFundId, ActWt = isnull(HoldingValue, 0)/AssetsEnd - BmkWt, AssetsEnd", sql.1mActWt.underlying(1, ""))
	w <- c(z, "\ton t3.FundId = t1.FundId", "inner join", sql.label(w, "t4"), "\ton t4.HFundId = t3.HFundId and t4.SecurityId = t2.SecurityId")
#
# TOP LINE
	z <- c("t2.SecurityId", "EqlChAct = avg(t2.ActWt - t4.ActWt)")
	z <- c(z, "BegChAct = sum(t4.AssetsEnd * (t2.ActWt - t4.ActWt))/sum(t4.AssetsEnd)")
	z <- c(z, "EndChAct = sum(t2.AssetsEnd * (t2.ActWt - t4.ActWt))/sum(t2.AssetsEnd)")
	z <- c(z, "BegPosChAct = sum(case when Flow > 0 then t4.AssetsEnd else NULL end * (t2.ActWt - t4.ActWt))/sum(case when Flow > 0 then t4.AssetsEnd else NULL end)")
	z <- c(z, "EndPosChAct = sum(case when Flow > 0 then t2.AssetsEnd else NULL end * (t2.ActWt - t4.ActWt))/sum(case when Flow > 0 then t2.AssetsEnd else NULL end)")
	z <- c(z, "BegNegChAct = sum(case when Flow < 0 then t4.AssetsEnd else NULL end * (t2.ActWt - t4.ActWt))/sum(case when Flow < 0 then t4.AssetsEnd else NULL end)")
	z <- c(z, "EndNegChAct = sum(case when Flow < 0 then t2.AssetsEnd else NULL end * (t2.ActWt - t4.ActWt))/sum(case when Flow < 0 then t2.AssetsEnd else NULL end)")
#
# FINISH UP
	z <- paste(c(x, "", sql.unbracket(sql.tbl(z, w, , "t2.SecurityId"))), collapse = "\n")
	z
}
sql.1mFloMo <- function(x, y, n, w, h, u = "All") {
# -----------------------------------------------------------------
# Name		: sql.1mFloMo
# Author	: VKS
# Date		: 10/24/18,11/2,3/8/21,5/4/22
# Args		: x = the YYYYMM for which you want data
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
#		: u = share-class filter (one of All/Inst/Retail)
# Output	: Generates the SQL query to get the data for 1mFloMo for individual stocks
# -----------------------------------------------------------------
#
# SHARE CLASSES
	u <- sql.ShareClass("ReportDate = @dy", u)
	u <- sql.tbl("ReportDate, HFundId, Flow, AssetsStart", "MonthlyData", u)
#
# FROM STATEMENT
	z <- sql.tbl("HFundId, AssetsEnd = sum(AssetsEnd)", "MonthlyData", "ReportDate = @dy", "HFundId", "sum(AssetsEnd) > 0")
	z <- c(sql.label(z, "t3"), "inner join", sql.label(u, "t1"))
	z <- c(z, "\ton t1.HFundId = t3.HFundId", "inner join", sql.label(sql.1dFloMo.filter(y, h), "t0"), "\ton t0.HFundId = t1.HFundId")
	z <- c(z, "inner join", sql.label(sql.Holdings("ReportDate = @dy", c("HSecurityId", "FundId", "HoldingValue")), "t2 on t2.FundId = t0.FundId"))
	if (!w) z <- c(z, "inner join", "SecurityHistory id on id.HSecurityId = t2.HSecurityId")
#
# GROUP BY CLAUSE
	grp <- sql.1dFloMo.grp(w, h)
#
# SELECT STATEMENT
	y <- sql.1dFloMo.select.wrapper(y, w, h)
#
# FINAL RESULT
	if (n == "All") {
		z <- sql.tbl(y, z, , grp, "sum(HoldingValue/AssetsEnd) > 0")
	} else {
		z <- sql.tbl(y, z, sql.in("t2.HSecurityId", sql.RDSuniv(n)), grp, "sum(HoldingValue/AssetsEnd) > 0")
	}
	z <- paste(c(sql.declare("@dy", "datetime", yyyymm.to.day(x)), sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1mFloTrend <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1mFloTrend
# Author	: VKS
# Date		: 10/22/20,11/9
# Args		: x = the YYYYMM for which you want data
#		: y = a string vector of factors to be computed,
#		:       the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Generates the SQL query to get the data for 1mFloTrend
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# SELECT STATEMENT
	if (w) z <- c(sql.ReportDate(yyyymm.to.day(x)), "n1.HSecurityId") else z <- "n1.SecurityId"
	z <- c(z, sapply(vec.to.list(y$factor), sql.1dFloTrend.select))
#
# PRELIMINARY QUERY
	x <- sql.1mFloTrend.underlying(y$filter, n, x)
#
# FINAL QUERY
	w <- ifelse(w, "n1.HSecurityId", "n1.SecurityId")
	z <- c(paste(x$PRE, collapse = "\n"), paste(sql.unbracket(sql.tbl(z, x$FINAL, , w)), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.1mFloTrend.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.1mFloTrend.underlying
# Author	: VKS
# Date		: 10/22/20
# Args		: x = a vector of filters
#		: y = any of All/StockFlows/China/Japan/CSI300/Energy
#		: n = the YYYYMM for which you want data
# Output	: Generates the SQL query to get the data for 1mFloTrend
# -----------------------------------------------------------------
	vec <- vec.named(c("#NEW", "#OLD"), c("n", "o"))
#
# BASIC TABLES
	z <- c("HFundId, Flow = sum(Flow), AssetsStart = sum(AssetsStart)")
	z <- sql.tbl(z, "MonthlyData", paste0("ReportDate ='", yyyymm.to.day(n), "'"), "HFundId")
	z <- sql.into(z, "#FLO")
	n <- yyyymm.lag(n, 0:1)
	#
	z <- c(z, "", sql.into(sql.MonthlyAlloc(paste0("'", yyyymm.to.day(n[1]), "'")), "#NEWHLD"))
	z <- c(z, "", sql.into(sql.MonthlyAssetsEnd(paste0("'", yyyymm.to.day(n[1]), "'"), F, T), "#NEWAUM"))
	#
	z <- c(z, "", sql.into(sql.MonthlyAlloc(paste0("'", yyyymm.to.day(n[2]), "'")), "#OLDHLD"))
	z <- c(z, "", sql.into(sql.MonthlyAssetsEnd(paste0("'", yyyymm.to.day(n[2]), "'"), F, T), "#OLDAUM"))
#
# BULK UP HOLDINGS
	z <- c(z, sql.Holdings.bulk.wrapper("#NEWHLD", x, yyyymm.to.day(n[1]), "#NEWBMKHLD", "#NEWBMKAUM"))
	z <- c(z, sql.Holdings.bulk.wrapper("#OLDHLD", x, yyyymm.to.day(n[2]), "#OLDBMKHLD", "#OLDBMKAUM"))
	if (y != "All") z <- c(z, "", "delete from #NEWHLD where", paste0("\t", sql.in("HSecurityId", sql.RDSuniv(y), F)), "")
	h <- c(sql.drop(c("#FLO", txt.expand(vec, c("HLD", "AUM"), ""))), "", z, "")
#
# FINAL STATEMENT
	z <- c(sql.label(sql.FundHistory(x, T, "FundId"), "his"), "inner join", "#FLO flo on flo.HFundId = his.HFundId")
	for (i  in names(vec)) {
		y <- c(paste0(vec[i], "HLD t"), "inner join", "SecurityHistory id on id.HSecurityId = t.HSecurityId")
		y <- sql.label(sql.tbl("FundId, HFundId, t.HSecurityId, SecurityId, HoldingValue", y), paste0(i, "1"))
		z <- c(z, "inner join", y, paste0("\ton ", i, "1.FundId = his.FundId"))
	}
	z <- c(z, "\tand o1.SecurityId = n1.SecurityId")
	for (i  in names(vec)) z <- c(z, "inner join", paste0(vec[i], "AUM ", i, "2 on ", i, "2.FundId = ", i, "1.FundId"))
#
# CREATE RESULT
	z <- list(PRE = h, FINAL = z)
#
# RETURN RESULT
	z
}
sql.1mFundCt <- function(x, y, n, w, h, u = 0) {
# -----------------------------------------------------------------
# Name		: sql.1mFundCt
# Author	: VKS
# Date		: 5/9/19,8/21,9/26,11/9/20,12/2,4/2/21,3/21/22,5/4
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last elements of which are the types of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
#		: u = when non-zero only the biggest <u> funds for each security matter
# Output	: Generates FundCt, the ownership breadth measure set forth in
#		:	Chen, Hong & Stein (2001)"Breadth of ownership and stock returns"
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
	r <- yyyymm.to.day(x)
	x <- sql.declare("@dy", "datetime", r)
#
# FILTERS
	if (n != "All") n <- list(A = sql.in("h.HSecurityId", sql.RDSuniv(n))) else n <- list()
	n[[char.ex.int(length(n) + 65)]] <- "ReportDate = @dy"
	for (k in setdiff(y$filter, "All")) n[[char.ex.int(length(n) + 65)]] <- sql.FundHistory.sf(k)
	n[[char.ex.int(length(n) + 65)]] <- sql.in("his.HFundId", sql.tbl("HFundId", "MonthlyData", "ReportDate = @dy"))
	n <- sql.and(n)
#
# SELECT
	if (h == "GeoId") z <- "GeoId = GeographicFocusId" else z <- sql.breakdown(h)
	if (w) z <- c(sql.ReportDate(r), z, "HSecurityId") else z <- c("SecurityId", z)
	for (j in y$factor) {
		if (j == "FundCt") {
			z <- c(z, paste(j, "count(HoldingValue)", sep = " = "))
		} else if (j == "HoldSum" & u == 0) {
			z <- c(z, paste(j, "sum(HoldingValue)", sep = " = "))
		} else if (j == "SharesHeld" & u == 0) {
			z <- c(z, paste(j, "sum(SharesHeld)", sep = " = "))
		} else if (j == "HoldSum" & u > 0) {
			z <- c(z, paste0(j, "Top", toupper(latin.ex.arabic(u)), " = sum(HoldingValue)"))
		} else {
			stop("Bad factor", j)
		}
	}
#
# FROM (POTENTIAL DUPLICATES REMOVED IN WHERE CLAUSE)
	r <- c("Holdings h", "inner join", "FundHistory his on his.FundId = h.FundId")
	if (!w) r <- c(r, "inner join", "SecurityHistory id on id.HSecurityId = h.HSecurityId")
#
# FINAL RESULT
	w <- ifelse(w, "HSecurityId", "SecurityId")
	w <- paste(c(w, sql.breakdown(h)), collapse = ", ")
	if (u > 0 & h == "All") {
		v <- c(w, "HoldingValue")
		v <- c(v, "HVRnk = ROW_NUMBER() over (partition by h.HSecurityId order by HoldingValue desc)")
		v <- sql.label(sql.tbl(v, r, n), "t")
		z <- sql.tbl(z, v, paste("HVRnk <", u + 1), w)
	} else if (u > 0) {
		stop("Can't handle yet!")
	} else {
		z <- sql.tbl(z, r, n, w)
	}
	z <- paste(c(x, sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1mHoldAum <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.1mHoldAum
# Author	: VKS
# Date		: 3/28/22
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last elements of which are the types of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
# Output	: Total AUM of all funds owning a particular security
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
	r <- yyyymm.to.day(x)
	x <- sql.declare("@dy", "datetime", r)
#
# FILTERS
	if (n != "All") n <- list(A = sql.in("h.HSecurityId", sql.RDSuniv(n))) else n <- list()
	n[[char.ex.int(length(n) + 65)]] <- "ReportDate = @dy"
	for (k in setdiff(y$filter, "All")) n[[char.ex.int(length(n) + 65)]] <- sql.FundHistory.sf(k)
	n <- sql.and(n)
#
# SELECT
	if (h == "GeoId") z <- "GeoId = GeographicFocusId" else z <- sql.breakdown(h)
	if (w) z <- c(sql.ReportDate(r), z, "HSecurityId") else z <- c("SecurityId", z)
	for (j in y$factor) {
		if (j == "HoldAum") {
			z <- c(z, paste0(j, " = sum(AssetsEnd)"))
		} else {
			stop("Bad factor", j)
		}
	}
#
# FROM
	r <- c("Holdings h", "inner join", "FundHistory f on f.FundId = h.FundId")
	r <- c(r, "inner join", sql.label(sql.MonthlyAssetsEnd("@dy"), "t on t.HFundId = f.HFundId"))
	if (!w) r <- c(r, "inner join", "SecurityHistory id on id.HSecurityId = h.HSecurityId")
#
# FINAL RESULT
	w <- ifelse(w, "HSecurityId", "SecurityId")
	w <- paste(c(w, sql.breakdown(h)), collapse = ", ")
	z <- sql.tbl(z, r, n, w)
	z <- paste(c(x, sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1mSRIAdvisorPct <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.1mSRIAdvisorPct
# Author	: VKS
# Date		: 5/11/21
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Generates the SQL query to get the data for 1mSRIAdvisorPct
# -----------------------------------------------------------------
#
# PARSE ARGUMENTS
	y <- sql.arguments(y)
	x <- yyyymm.to.day(x)
#
# BASICS
	h <- sql.FundHistory(c(y$filter, "SRI"), T, "AdvisorId")
	h <- c("Holdings t1", "inner join", sql.label(h, "t2 on t2.HFundId = t1.HFundId"))
	#
	z <- c("HSecurityId", "Num = count(distinct AdvisorId)")
	z <- sql.label(sql.tbl(z, h, "ReportDate = @floDt", z[1]), "t1")
	h <- sql.tbl("Den = count(distinct AdvisorId)", h, "ReportDate = @floDt")
	z <- c(z, "cross join", sql.label(h, "t2"))
	#
	h <- sql.declare("@floDt", "datetime", yyyymm.to.day(x))
#
# CREATE RESULT
	if (w) x <- c(sql.ReportDate(x), "t1.HSecurityId") else x <- "SecurityId"
	if (length(y$factor) != 1 | y$factor[1] != "SRIAdvisorPct") stop("Bad Argument")
	x <- c(x, "SRIAdvisorPct = 100 * cast(sum(Num) as float)/max(Den)")
	if (!w) z <- c(z, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	w <- ifelse(w, "t1.HSecurityId", "SecurityId")
	z <- paste(c(h, "", sql.unbracket(sql.tbl(x, z,, w))), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.1wFlow.Corp <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.1wFlow.Corp
# Author	: VKS
# Date		: 12/5/19,12/6,11/23/20
# Args		: x = YYYYMMDD from which flows are to be computed
# Output	: Generates the SQL query to get weekly corporate flow ($MM)
# -----------------------------------------------------------------
#
# STYLE SECTORS
	h <- mat.read(parameters("classif-StyleSector"))
	h <- map.rname(h, c(136, 133, 140, 135, 132, 139, 142, 125))
	h$Domicile <- ifelse(dimnames(h)[[1]] == 125, "US", NA)
#
# SELECT
	z <- vec.named(paste("StyleSector", dimnames(h)[[1]], sep = " = "), h[, "Abbrv"])
	z[!is.na(h$Domicile)] <- paste(z[!is.na(h$Domicile)], "Domicile = 'US'", sep = " and ")
	names(z)[!is.na(h$Domicile)] <- paste(names(z)[!is.na(h$Domicile)], "US")
	z <- paste0("[", names(z), "] = sum(case when ", z, " then Flow else NULL end)")
	z <- c(sql.yyyymmdd("WeekEnding"), z)
#
# WHERE
	y <- list(A = "FundType = 'B'", B = "GeographicFocus = 77")
	y[["C"]] <- sql.in("StyleSector", paste0("(", paste(dimnames(h)[[1]], collapse = ", "), ")"))
	y[["D"]] <- paste0("WeekEnding >= '", x, "'")
#
# CREATE RESULT
	z <- sql.tbl(z, c("WeeklyData t1", "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId"), sql.and(y), "WeekEnding")
	z <- paste(sql.unbracket(z), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.ActWtDiff2 <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.ActWtDiff2
# Author	: VKS
# Date		: 12/17/18,3/8/21
# Args		: x = flow date
# Output	: ActWtDiff2 on R1 Materials for positioning
# -----------------------------------------------------------------
#
# SUBSET TO CURRENT RUSSELL 1000 MATERIALS STOCKS
	mo.end <- yyyymmdd.to.AllocMo(x, 26)
	#
	w <- sql.and(list(A = "StyleSectorId = 101", B = "GeographicFocusId = 77", C = "[Index] = 1"))
	w <- sql.in("HFundId", sql.tbl("HFundId", "FundHistory", w))
	w <- list(A = w, B = paste0("ReportDate = '", yyyymm.to.day(mo.end), "'"))
	#
	z <- sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "FundId = 5152"))
	z <- sql.and(list(A = z, B = paste0("ReportDate = '", yyyymm.to.day(mo.end), "'")))
	z <- sql.tbl("HSecurityId", "Holdings", z, "HSecurityId")
	w[["C"]] <- sql.in("HSecurityId", z)
	#
	w <- sql.tbl("HSecurityId", "Holdings", sql.and(w), "HSecurityId")
#
# COMPUTE ActWtDiff2
	z <- sql.1dActWtTrend.underlying(x, "All", w)
	z <- c(z, sql.1dActWtTrend.topline("ActWtDiff2", F))
#
# RETURN RESULT
	z
}
sql.Allocation <- function(x, y, n = NULL, w = "All", h, u, v) {
# -----------------------------------------------------------------
# Name		: sql.Allocation
# Author	: VKS
# Date		: 11/24/20
# Args		: x = needed columns
#		: y = one of Country/Sector/Industry
#		: n = columns needed from FundHistory besides HFundId/FundId
#		: w = a vector of FundHistory filters
#		: h = where clause (can be missing)
#		: u = group by clause (can be missing)
#		: v = having clause (can be missing)
# Output	: SQL query to fetch Country/Sector/Industry allocations
# -----------------------------------------------------------------
#
# FROM STATEMENT
	z <- paste0(y, "Allocations_FromAllocationFlows")
	z <- sql.label(z, paste0("t2 on ", y, "AllocationsHistoryId = [Id]"))
	z <- c(paste0(y, "AllocationsHistory_FromAllocationFlows t1"), "inner join", z)
	z <- c(z, "inner join", sql.label(sql.FundHistory(w, F, c("FundId", n)), "t3 on t3.HFundId = t1.HFundId"))
#
# CREATE RESULT
	z <- list(x = x, y = z)
	if (!missing(h)) z[["n"]] <- h
	if (!missing(u)) z[["w"]] <- u
	if (!missing(v)) z[["h"]] <- v
	z <- do.call(sql.tbl, z)
#
# RETURN RESULT
	z
}
sql.Allocation.Sec <- function(x, y = NULL, n = "All") {
# -----------------------------------------------------------------
# Name		: sql.Allocation.Sec
# Author	: VKS
# Date		: 11/25/20
# Args		: x = list object of individual where clauses
#		: y = columns needed from FundHistory besides HFundId/FundId
#		: n = a vector of FundHistory filters
# Output	: SQL query for sector allocations for month ending <x>
# -----------------------------------------------------------------
#
# ADD ESSENTIAL COLUMNS
	r <- c("FundId", "SectorId", y, "Allocation")
#
# SECTOR WEIGHTS
	z <- sql.unbracket(sql.Allocation(r, "Sector", y, n, sql.and(x)))
	z <- c("insert into", paste0("\t#SEC (", paste(r, collapse = ", "), ")"), z)
#
# INDUSTRY WEIGHTS
	x[[char.ex.int(length(x) + 65)]] <- "IndustryId = 20"
	h <- ifelse(r == "SectorId", "IndustryId", r)
	v <- sql.unbracket(sql.Allocation(h, "Industry", y, n, sql.and(x)))
	z <- c(z, "", "insert into", paste0("\t#SEC (", paste(r, collapse = ", "), ")"), v)
#
# FINS EX REAL ESTATE (MADE-UP SECTOR ID 30)
	z <- c(z, "", sql.Allocation.Sec.FinsExREst(r))
#
# RETURN RESULT
	z
}
sql.Allocation.Sec.FinsExREst <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.Allocation.Sec.FinsExREst
# Author	: VKS
# Date		: 3/30/22
# Args		: x = column names of table #SEC
# Output	: SQL query to add FinsExREst sector allocations
# -----------------------------------------------------------------
#
# IF NO REAL ESTATE REPORTED, SET TO ZERO
	v <- list(A = "SectorId = 7")
	v[["B"]] <- sql.in("FundId", sql.tbl("FundId", "#SEC", "SectorId = 20"), F)
	h <- ifelse(x == "SectorId", "SectorId = 20", x)
	h <- ifelse(h == "Allocation", "Allocation = 0", h)
	v <- sql.unbracket(sql.tbl(h, "#SEC", sql.and(v)))
	z <- c("insert into", paste0("\t#SEC (", paste(x, collapse = ", "), ")"), v)
#
# COMPUTE FinsExREst
	h <- ifelse(is.element(x, c("SectorId", "Allocation")), x, paste0("t1.", x))
	h <- ifelse(h == "SectorId", "SectorId = 30", h)
	h <- ifelse(h == "Allocation", "Allocation = t1.Allocation - t2.Allocation", h)
	#
	v <- sql.and(list(A = "t1.SectorId = 7", B = "t2.SectorId = 20"))
	v <- sql.unbracket(sql.tbl(h, c("#SEC t1", "inner join", "#SEC t2 on t2.FundId = t1.FundId"), v))
	z <- c(z, "", "insert into", paste0("\t#SEC (", paste(x, collapse = ", "), ")"), v)
#
# RETURN RESULT
	z
}
sql.Allocations.bulk.EqWtAvg <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.Allocations.bulk.EqWtAvg
# Author	: VKS
# Date		: 1/25/21
# Args		: x = name of column being bulked up
#		: y = vector of columns in addition to <w> within which averages are computed
#		: n = allocation table name
#		: w = primary grouping within which averages are computed
# Output	: Bulks up allocations with equal-weight averages
# -----------------------------------------------------------------
#
# EQUAL-WEIGHT AVERAGE BY GEOGRAPHIC FOCUS
	r <- c(w, y, paste0(x, " = avg(", x, ")"))
	r <- sql.label(sql.tbl(r, n,, paste(c(w, y), collapse = ", ")), "t2")
#
# FUNDS MISSING ALLOCATIONS
	z <- sql.in("FundId", sql.tbl("FundId", n), F)
	z <- sql.label(sql.tbl(c("FundId", paste0(w, " = max(", w, ")")), "#FLO", z, "FundId"), "t1")
#
# BULK UP ALLOCATIONS
	z <- c(z, "inner join", r, paste0("\ton t2.", w, " = t1.", w))
	z <- sql.unbracket(sql.tbl(c("FundId", paste0("t1.", w), y, x), z))
	r <- paste0("\t", n, " (", paste(c("FundId", w, y, x), collapse = ", "), ")")
	z <- c("insert into", r, z)
#
# RETURN RESULT
	z
}
sql.Allocations.bulk.Single <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.Allocations.bulk.Single
# Author	: VKS
# Date		: 1/25/21,1/27
# Args		: x = name of column being bulked up
#		: y = vector of columns in addition to <w> with which funds are tagged
#		: n = allocation table name
#		: w = allocation bulking group (e.g. GeographicFocus/BenchIndex)
#		: h = single-group column and value
# Output	: Bulks up allocations with single-group funds
# -----------------------------------------------------------------
#
# HANDLE ARGUMENTS
	r <- y[1]
	if (!is.null(y)) y <- paste(y, collapse = " = ")
#
# SELECT STATEMENT
	z <- paste0(w[1], " = max(", w[1], ")")
	z <- c("FundId", z, y, paste(x, "= 100"))
#
# WHERE CLAUSE
	if (h[1] != w & is.null(y)) {
		h <- paste0(h[1], " in (", h[2], ")")
		h <- sql.tbl("FundId", "FundHistory", h)
		h <- sql.in("FundId", h)
	} else {
		h <- paste0(h[1], " in (", h[2], ")")
	}
#
# CREATE RESULT
	z <- sql.tbl(z, "#FLO", h, "FundId")
	z <- c(paste0("\t", n, " (", paste(c("FundId", w, r, x), collapse = ", "), ")"), sql.unbracket(z))
	z <- c("insert into", z)
#
# RETURN RESULT
	z
}
sql.and <- function(x, y = "and") {
# -----------------------------------------------------------------
# Name		: sql.and
# Author	: VKS
# Date		: 1/10/18,12/21,1/25/19,2/1,11/17/20
# Args		: x = list object of individual where clauses
#		: y = logical operator to use
# Output	: and segment of an SQL statement
# Notes		: cannot replace unlist and lapply with sapply here
# -----------------------------------------------------------------
	m <- length(x)
	if (m > 1) {
		fcn <- function(x) c(y, paste0("\t", x))
		z <- unlist(lapply(x, fcn))[-1]
	} else {
		z <- x[[1]]
	}
	z
}
sql.arguments <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.arguments
# Author	: VKS
# Date		: 11/2/18,3/13/19,3/6/20,9/24,10/2
# Args		: x = a string vector of variables to build with the last elements
#		:	specifying the type of funds to use
# Output	: splits <x> into factor and filters
# -----------------------------------------------------------------
	filters <- c("All", "Num", "Pseudo", "Up", "CBE", names(vec.ex.filters("sf")))
#
# FIND LAST FACTOR
	m <- length(x)
	while (any(x[m] == filters)) m <- m - 1
#
# ENSURE THERE'S AT LEAST ONE FILTER
	if (m == length(x)) x <- c(x, "All")
#
# PERFORM SPLIT
	w <- seq(1, length(x)) > m
	z <- list(factor = x[!w], filter = x[w])
#
# RETURN RESULT
	z
}
sql.bcp <- function(x, y, n = "Quant", w = "EPFRUI", h = "dbo") {
# -----------------------------------------------------------------
# Name		: sql.bcp
# Author	: VKS
# Date		: 8/1/17,2/1/18
# Args		: x = SQL table to perform the bulk copy from
#		: y = the location of the output file
#		: n = One of "StockFlows", "Quant", "QuantSF" or "Regular"
#		: w = the database on which <x> resides
#		: h = the owner of <x>
# Output	: code to bcp data out of server
# -----------------------------------------------------------------
#
# FULL FORMAL TABLE NAME
	h <- paste(w, h, x, sep = ".")
#
# READ PARAMETERS FILE
	x <- parameters("SQL")
	x <- mat.read(x, "\t")
#
# BUILD BCP STRING
	z <- is.element(dimnames(x)[[1]], n)
	if (sum(z) != 1) stop("Bad type", n)
	if (sum(z) == 1) {
		z <- paste("-S", x[, "DB"], "-U", x[, "UID"], "-P", x[, "PWD"])[z]
		z <- paste("bcp", h, "out", y, z, "-c")
	}
#
# RETURN RESULT
	z
}
sql.BenchIndex.duplication <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.BenchIndex.duplication
# Author	: VKS
# Date		: 4/22/21
# Args		: x = name of table being updated
# Output	: updates BenchIndex field in table <x> to remove duplicates
# -----------------------------------------------------------------
#
# COUNT RECORDS BY BENCHINDEX
	z <- sql.tbl(c("BenchIndex", "obs = count(BenchIndex)"), x,, "BenchIndex")
#
# RANK BENCHINDEX WITHIN DESCRIPTIONS
	v <- c("BIDesc", "BenchIndex", "obs")
	v <- c(v, "Rnk = ROW_NUMBER() over (partition by BIDesc order by obs desc)")
	z <- sql.tbl(v, c(sql.label(z, "t1"), "inner join", "BenchIndexes t2 on BIID = BenchIndex"))
#
# FOR EACH DESCRIPTION, FIND THE BENCHINDEX WITH THE MOST RECORDS
	z <- sql.tbl(c("BIDesc", "BenchIndex"), sql.label(z, "t"), "Rnk = 1")
#
# DEFINE MAP FROM DESCRIPTION BACK TO MOST COMMON BENCHINDEX
	z <- c(sql.label(z, "t1"), "inner join", "BenchIndexes t2 on t2.BIDesc = t1.BIDesc")
	z <- sql.label(sql.tbl(c("BIID", "BenchIndex"), z, "not BIID = BenchIndex"), "t")
#
# PERFORM UPDATE
	z <- sql.unbracket(sql.tbl("BenchIndex = t.BenchIndex", z, paste0(x, ".BenchIndex = t.BIID")))
	z[1] <- paste("update", x, "set")
#
# RETURN RESULT
	z
}
sql.breakdown <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.breakdown
# Author	: VKS
# Date		: 12/3/20
# Args		: x = one or more breakdown filters (e.g. All/GeoId/DomicileId)
# Output	: Returns
# -----------------------------------------------------------------
	z <- setdiff(x, "All")
	z <- ifelse(z == "GeoId", "GeographicFocusId", x)
	z
}
sql.Bullish <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.Bullish
# Author	: VKS
# Date		: 5/7/20,11/9,8/4/21
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: SQL query for Bullish-sentiment factor
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
	x <- yyyymm.to.day(x)
#
# BASICS
	cols <- c("HFundId", "HSecurityId", "HoldingValue")
	#
	z <- c(sql.drop(c("#HLD", "#BMK")), "")
	#
	z <- c(z, "create table #HLD (HFundId int not null, HSecurityId int not null, HoldingValue float)")
	z <- c(z, sql.index("#HLD", "HFundId, HSecurityId"))
	z <- c(z, "insert into", paste0("\t#HLD (", paste(cols, collapse = ", "), ")"))
	#
	h <- list(A = paste0("ReportDate = '", x, "'"))
	if (n != "All") h[[char.ex.int(length(h) + 65)]] <- sql.in("HSecurityId", sql.RDSuniv(n))
	if (y$filter != "All") h[[char.ex.int(length(h) + 65)]] <- sql.in("HFundId", sql.FundHistory(y$filter, T))
	h <- sql.and(h)
	#
	z <- c(z, sql.unbracket(sql.tbl(cols, "Holdings", h)), "")
#
# WEIGHTS
	h <- sql.tbl("HFundId, PortVal = sum(AssetsEnd)", "MonthlyData", paste0("ReportDate = '", x, "'"), "HFundId", "sum(AssetsEnd) > 0")
	z <- c(z, "update #HLD", "\tset HoldingValue = 100 * HoldingValue/PortVal")
	z <- c(z, "from", sql.label(paste0("\t", h), "t"), "where", "\t#HLD.HFundId = t.HFundId", "")
#
# AVERAGE BENCHMARK WEIGHT
	u <- sql.and(list(A = "[Index] = 1", B = "HFundId in (select HFundId from #HLD)"))
	h <- c(sql.label(sql.tbl("HFundId, BenchIndexId", "FundHistory", u), "t1"), "inner join")
	h <- c(h, sql.label(sql.tbl("BenchIndexId, nFunds = count(HFundId)", "FundHistory", u, "BenchIndexId"), "t2"))
	h <- c(h, "\ton t2.BenchIndexId = t1.BenchIndexId", "inner join", "#HLD t3 on t3.HFundId = t1.HFundId")
	#
	u <- "t1.BenchIndexId, t3.HSecurityId, BmkWt = sum(HoldingValue)/nFunds"
	h <- sql.tbl(u, h,, "t1.BenchIndexId, t3.HSecurityId, nFunds")
	z <- c(z, sql.into(h, "#BMK"), "")
#
# TIDY UP
	z <- c(z, "delete from #HLD where HFundId in (select HFundId from FundHistory where [Index] = 1)")
#
# FINAL RESULT
	if (w) x <- c(sql.ReportDate(x), "t1.HSecurityId") else x <- "SecurityId"
	if (length(y$factor) != 1 | y$factor[1] != "Bullish") stop("Bad Argument")
	x <- c(x, "Bullish = 100 * sum(case when HoldingValue > isnull(BmkWt, 0) then 1.0 else 0.0 end)/FundCt")
	#
	h <- c("#HLD t1", "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
	if (!w) h <- c(h, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	h <- c(h, "cross join", "(select FundCt = count(distinct HFundId) from #HLD) t4", "left join")
	h <- c(h, "#BMK t3 on t3.BenchIndexId = t2.BenchIndexId and t3.HSecurityId = t1.HSecurityId")
	#
	w <- paste0(ifelse(w, "t1.HSecurityId", "SecurityId"), ", FundCt")
	z <- c(paste(z, collapse = "\n"), paste(sql.unbracket(sql.tbl(x, h, , w)), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.case <- function(x, y, n, w = T) {
# -----------------------------------------------------------------
# Name		: sql.case
# Author	: VKS
# Date		: 11/26/20
# Args		: x = final label
#		: y = a string vector of conditions
#		: n = a string vector of length one more than <y> of labels
#		: w = T/F depending on whether labels are numeric
# Output	: case statement assigning labels <n> based on conditions <y>
# -----------------------------------------------------------------
	if (!w) n <- paste0("'", n, "'")
	z <- n[length(y) + 1]
	z <- c(paste("when", y, "then", n[seq_along(y)]), paste("else", z, "end"))
	z <- c(paste(x, "= case"), paste0("\t", z))
	z
}
sql.close <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.close
# Author	: VKS
# Date		: 11/20/20
# Args		: x = output of sql.connect
# Output	: Closes an SQL connection (if needed)
# -----------------------------------------------------------------
	if (x[["close"]]) close(x[["conn"]])
	invisible()
}
sql.cross.border <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.cross.border
# Author	: VKS
# Date		: 5/22/17,11/24,1/10/18,12/20
# Args		: x = T/F depending on whether StockFlows data are being used
# Output	: Returns a list object of cross-border Geo. Foci and their names
# -----------------------------------------------------------------
	y <- parameters("classif-GeoId")
	y <- mat.read(y, "\t")
	y <- y[is.element(y$xBord, 1), ]
	if (x) x <- "GeographicFocusId" else x <- "GeographicFocus"
	z <- paste(x, "=", paste(dimnames(y)[[1]], y[, "Name"], sep = "--"))
	z <- split(z, y[, "Abbrv"])
	z
}
sql.DailyFlo <- function(x, y = T, n = T, w = "All", h = F) {
# -----------------------------------------------------------------
# Name		: sql.DailyFlo
# Author	: VKS
# Date		: 10/28/16,11/9,1/5/18,1/9,1/10,1/11,3/7/21,3/2/22,
#		: 4/11,5/4
# Args		: x = a vector of dates for which you want flows (known one day later)
#		: y = T/F depending on whether to group by HFundId
#		: n = T/F depending on whether StockFlows data are being used
#		: w = share-class filter (one of All/Inst/Retail)
#		: h = T/F depending on whether AssetsEnd is wanted
# Output	: Generates the SQL query to get the data for daily Flow
# -----------------------------------------------------------------
#
# DATES
	n <- ifelse(n, "ReportDate", "DayEnding")
	if (length(x) == 1) x <- paste("=", x) else x <- paste0("in (", paste(x, collapse = ", "), ")")
	x <- paste(n, x)
#
# SHARE CLASSES
	x <- sql.ShareClass(x, w)
#
# FINAL RESULT
	z <- c("Flow", "AssetsStart")
	if (h) z <- c(z, "AssetsEnd")
	if (y) z <- paste0(z, " = sum(", z, ")")
	z <- c(n, "HFundId", z)
	if (y) {
		z <- sql.tbl(z, "DailyData", x, paste(z[1:2], collapse = ", "))
	} else {
		z <- sql.tbl(z, "DailyData", x)
	}
#
# RETURN RESULT
	z
}
sql.datediff <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.datediff
# Author	: VKS
# Date		: 1/4/18
# Args		: x = column in the monthly table
#		: y = column in the daily table
#		: n = calendar day on which previous month's data available
# Output	: Before <n>, falls back two else one month
# -----------------------------------------------------------------
	paste0("datediff(month, ", x, ", ", y, ") = case when day(", y, ") < ", n, " then 2 else 1 end")
}
sql.declare <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.declare
# Author	: VKS
# Date		: 1/9/18
# Args		: x = variable names
#		: y = variable types
#		: n = values
# Output	: declare statement
# -----------------------------------------------------------------
	c(paste("declare", x, y), paste0("set ", x, " = '", n, "'"))
}
sql.delete <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.delete
# Author	: VKS
# Date		: 6/28/22
# Args		: x = table name
#		: y = where clause
# Output	: delete from <x> where <y>
# -----------------------------------------------------------------
	x <- paste0("\t", x)
	y <- paste0("\t", y)
	z <- c("delete from", x, "where", y)
	z
}
sql.Diff <- function(x, y, n = "") {
# -----------------------------------------------------------------
# Name		: sql.Diff
# Author	: VKS
# Date		: 11/16/16,11/29,1/4/18,10/23,6/14/21
# Args		: x = bit of SQL string
#		: y = bit of SQL string
#		: n = one of ""/"Num"/"Den"
# Output	: SQL statement for diffusion
# -----------------------------------------------------------------
	z <- paste0("= sum((", x, ") * cast(sign(", y, ") as float))")
	if (n == "") {
		z <- paste0(z, "/", sql.nonneg(paste0("sum(abs(", x, "))")))
	} else if (n == "Den") {
		z <- paste0("= sum(abs(", x, "))")
	}
#
# RETURN RESULT
	z
}
sql.Dispersion <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.Dispersion
# Author	: VKS
# Date		: 3/13/19,5/15,11/9/20
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
# Output	: Generates the dispersion measure set forth in Jiang & Sun (2011)
#		:	"Dispersion in beliefs among active mutual funds and the cross-section of stock returns"
# -----------------------------------------------------------------
#
# PRELIMINARIES
	x <- yyyymm.to.day(x)
	z <- sql.drop(c("#HLD", "#BMK"))
#
# BENCHMARK
	z <- c(z, "", "create table #BMK (BenchIndexId int not null, HSecurityId int not null, HoldingValue float not null)")
	z <- c(z, sql.index("#BMK", "BenchIndexId, HSecurityId"))
	u <- sql.and(list(A = paste0("ReportDate = '", x, "'"), B = "[Index] = 1"))
	h <- "Holdings t1 inner join FundHistory t2 on t2.HFundId = t1.HFundId"
	h <- sql.tbl("BenchIndexId, HSecurityId, HoldingValue = sum(HoldingValue)", h, u, "BenchIndexId, HSecurityId", "sum(HoldingValue) > 0")
	z <- c(z, "insert into #BMK", sql.unbracket(h))
	#
	h <- sql.label(sql.tbl("BenchIndexId, AUM = sum(HoldingValue)", "#BMK",, "BenchIndexId", "sum(HoldingValue) > 0"), "t")
	h <- sql.unbracket(sql.tbl("HoldingValue = HoldingValue/AUM", h, "#BMK.BenchIndexId = t.BenchIndexId"))
	z <- c(z, "", "update #BMK set", h[-1])
#
# HOLDINGS
	z <- c(z, "", "create table #HLD (HFundId int not null, HSecurityId int not null, HoldingValue float not null)")
	z <- c(z, sql.index("#HLD", "HFundId, HSecurityId"))
	u <- "BenchIndexId in (select BenchIndexId from #BMK)"
	u <- sql.and(list(A = paste0("ReportDate = '", x, "'"), B = "[Index] = 0", C = u, D = "HoldingValue > 0"))
	h <- "Holdings t1 inner join FundHistory t2 on t2.HFundId = t1.HFundId"
	h <- sql.tbl("t1.HFundId, HSecurityId, HoldingValue", h, u)
	z <- c(z, "insert into #HLD", sql.unbracket(h))
	#
	h <- sql.label(sql.tbl("HFundId, AUM = sum(HoldingValue)", "#HLD",, "HFundId", "sum(HoldingValue) > 0"), "t")
	h <- sql.unbracket(sql.tbl("HoldingValue = HoldingValue/AUM", h, "#HLD.HFundId = t.HFundId"))
	z <- c(z, "", "update #HLD set", h[-1])
#
# ACTIVE WEIGHTS
	h <- c("FundHistory t1", "inner join", "#BMK t2 on t2.BenchIndexId = t1.BenchIndexId")
	u <- "#HLD.HFundId = t1.HFundId and #HLD.HSecurityId = t2.HSecurityId"
	h <- sql.unbracket(sql.tbl("HoldingValue = #HLD.HoldingValue - t2.HoldingValue", h, u))
	z <- c(z, "", "update #HLD set", h[-1])
	#
	u <- sql.tbl("HFundId, HSecurityId", "#HLD t", "t1.HFundId = t.HFundId and t2.HSecurityId = t.HSecurityId")
	u <- sql.and(list(A = sql.exists(u, F), B = "t1.HFundId in (select HFundId from #HLD)"))
	h <- c("FundHistory t1", "inner join", "#BMK t2 on t2.BenchIndexId = t1.BenchIndexId")
	h <- sql.tbl("HFundId, HSecurityId, -HoldingValue", h, u)
	z <- c(z, "", "insert into #HLD", sql.unbracket(h))
#
# SUBSET
	if (n != "All") z <- c(z, "", "delete from #HLD where", sql.in("HSecurityId", sql.RDSuniv(n), F))
	z <- paste(z, collapse = "\n")
#
# FINAL STATEMENT
	h <- "#HLD hld"
	if (w) {
		u <- c(sql.ReportDate(x), "HSecurityId")
	} else {
		h <- c(h, "inner join", "SecurityHistory id on id.HSecurityId = hld.HSecurityId")
		u <- "SecurityId"
	}
	w <- ifelse(w, "HSecurityId", "SecurityId")
	u <- c(u, "Dispersion = 10000 * (avg(square(HoldingValue)) - square(avg(HoldingValue)))")
	z <- c(z, paste(sql.unbracket(sql.tbl(u, h,, w)), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.drop <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.drop
# Author	: VKS
# Date		: 4/11/18
# Args		: x = a vector of temp-table names
# Output	: drops the elements of <x> if they exist
# -----------------------------------------------------------------
	paste0("IF OBJECT_ID('tempdb..", x, "') IS NOT NULL DROP TABLE ", x)
}
sql.exists <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: sql.exists
# Author	: VKS
# Date		: 5/10/18,12/31/19
# Args		: x = SQL statement
#		: y = T/F depending on whether exists/not exists
# Output	: <x> in <y> if <n> or <x> not in <y> otherwise
# -----------------------------------------------------------------
	c(ifelse(y, "exists", "not exists"), paste0("\t", x))
}
sql.extra.domicile <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.extra.domicile
# Author	: VKS
# Date		: 6/4/21
# Args		: x = flowdate/YYYYMMDD depending on whether daily/weekly
#		: y = column in classif-Ctry corresponding to names of <x>
#		: n = column in FundHistory corresponding to names of <x>
# Output	: where clauses to ensure foreign flow
# -----------------------------------------------------------------
#
# MAP OF COUNTRY ID TO DOMICILE
	z <- mat.read(parameters("classif-Ctry"))
	z <- z[is.element(z[, y], names(x)) & !is.na(z$DomicileId), ]
	z <- vec.named(z$DomicileId, z[, y])
#
# DOMICILES THAT MAP TO EACH CURRENCY/COUNTRY/GROUP
	z <- split(as.character(z), x[names(z)])
#
# ALLOCATIONS THAT MAP TO EACH CURRENCY/COUNTRY/GROUP
	z <- list(Domicile = z, Allocation = x[is.element(x, names(z))])
	z[["Allocation"]] <- split(names(z$Allocation), z$Allocation)
#
# SQL STATEMENTS: DOMICILE
	for (j in names(z[["Domicile"]])) {
		if (length(z[["Domicile"]][[j]]) == 1) {
			z[["Domicile"]][[j]] <- paste0("Domicile = '", z[["Domicile"]][[j]], "'")
		} else {
			z[["Domicile"]][[j]] <- paste(z[["Domicile"]][[j]], collapse = "', '")
			z[["Domicile"]][[j]] <- paste0("Domicile in ('", z[["Domicile"]][[j]], "')")
		}
	}
#
# SQL STATEMENTS: ALLOCATION
	for (j in names(z[["Allocation"]])) {
		if (length(z[["Allocation"]][[j]]) == 1) {
			z[["Allocation"]][[j]] <- paste0(n, " = ", z[["Allocation"]][[j]])
		} else {
			z[["Allocation"]][[j]] <- paste(z[["Allocation"]][[j]], collapse = ", ")
			z[["Allocation"]][[j]] <- paste0(n, " in (", z[["Allocation"]][[j]], ")")
		}
	}
#
# FINAL RESULT
	z <- lapply(z, unlist)
	z <- array.ex.list(z, F, T)
	z <- vec.named(paste0("not (", z[, 1], " and ", z[, 2], ")"), dimnames(z)[[1]])
	z <- split(z, names(z))
#
# RETURN RESULT
	z
}
sql.FloMo.Funds <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.FloMo.Funds
# Author	: VKS
# Date		: 1/26/17,4/14,11/14,1/5/18,1/11,1/12
# Args		: x = the month/day for which you want % flow, % portfolio change, & assets end
# Output	: Generates the SQL query to get monthly/daily data for Funds
# -----------------------------------------------------------------
#
# MONTHLY OR DAILY ?
	if (nchar(x) == 6) {
		sql.table <- "MonthlyData"
		flo.dt <- yyyymm.to.day(x)
		dt.col <- "MonthEnding"
	} else {
		sql.table <- "DailyData"
		flo.dt <- x
		dt.col <- "DayEnding"
	}
#
# QUERY
	flo.dt <- sql.declare("@floDt", "datetime", flo.dt)
	z <- c("SecurityId = FundId", "PortfolioChangePct = 100 * sum(PortfolioChange)/sum(AssetsStart)")
	z <- c(z, "FlowPct = 100 * sum(Flow)/sum(AssetsStart)", "AssetsEnd = sum(AssetsEnd)")
	x <- c(sql.label(sql.table, "t1"), "inner join", "FundHistory t2 on t1.HFundId = t2.HFundId")
	z <- paste(sql.unbracket(sql.tbl(z, x, paste(dt.col, "= @floDt"), "FundId", "sum(AssetsStart) > 0")), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.Flow <- function(x, y, n = "All", w = NULL, h = T, u, v) {
# -----------------------------------------------------------------
# Name		: sql.Flow
# Author	: VKS
# Date		: 11/26/20,1/16/21
# Args		: x = needed columns
#		: y = list of where clauses, first being the flow date restriction
#		: n = a vector of FundHistory filters
#		: w = columns needed from FundHistory besides HFundId/FundId
#		: h = T/F for daily/weekly
#		: u = group by clause (can be missing)
#		: v = having clause (can be missing)
# Output	: SQL query to fetch daily/weekly flows
# -----------------------------------------------------------------
#
# FROM STATEMENT
	z <- sql.label(sql.FundHistory(n, F, c("FundId", w)), "t2")
	z <- c(z, "\ton t2.HFundId = t1.HFundId")
	z <- c(paste(ifelse(h, "DailyData", "WeeklyData"), "t1"), "inner join", z)
#
# WHERE CLAUSE
	if (length(y[[1]]) == 1) {
		y[[1]] <- paste(ifelse(h, "DayEnding", "WeekEnding"), "=", y[[1]])
	} else {
		y[[1]] <- paste(y[[1]], collapse = ", ")
		y[[1]] <- paste0(ifelse(h, "DayEnding", "WeekEnding"), " in (", y[[1]], ")")
	}
#
# CREATE RESULT
	z <- list(x = x, y = z, n = sql.and(y))
	if (!missing(u)) z[["w"]] <- u
	if (!missing(v)) z[["h"]] <- v
	z <- do.call(sql.tbl, z)
#
# RETURN RESULT
	z
}
sql.Foreign <- function() {
# -----------------------------------------------------------------
# Name		: sql.Foreign
# Author	: VKS
# Date		: 10/14/22
# Args		: none
# Output	: list object of foreign-fund restrictions
# -----------------------------------------------------------------
	x <- mat.read(parameters("classif-Ctry"))[, c("GeoId", "DomicileId")]
	x <- x[apply(x, 1, function(x) sum(!is.na(x))) == 2, ]
#
# WHERE CLAUSES
	x[, "DomicileId"] <- paste0("Domicile = '", x[, "DomicileId"], "'")
	x[, "DomicileId"] <- paste("Domicile is not NULL and", x[, "DomicileId"])
	#
	x[, "GeoId"] <- paste("GeographicFocus =", x[, "GeoId"])
	x[, "GeoId"] <- paste("GeographicFocus is not NULL and", x[, "GeoId"])
#
# CREATE RESULT
	z <- split(paste0("not (", x[, "DomicileId"], " and ", x[, "GeoId"], ")"), dimnames(x)[[1]])
#
# RETURN RESULT
	z
}
sql.FundHistory <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.FundHistory
# Author	: VKS
# Date		: 11/7/16,11/30,1/18/17,5/22,1/4/18,1/9,1/10,1/11,4/11,
#		: 5/16,6/18,7/16,8/20,9/21,10/23,11/2,12/6,3/13/19,10/23,
#		: 11/12,11/23,11/24,11/14/22
# Args		: x = a vector of filters
#		: y = T/F depending on whether StockFlows data are being used
#		: n = columns needed in addition to HFundId
# Output	: SQL query to restrict to Global and Regional equity funds
# -----------------------------------------------------------------
	if (any(x[1] == c("Pseudo", "Up"))) x <- ifelse(y, "All", "E")
	x <- setdiff(x, c("Aggregate", "All"))
	if (missing(n)) n <- "HFundId" else n <- c("HFundId", n)
#
# FILTERS
	if (length(x) == 0) {
		z <- sql.tbl(n, "FundHistory")
	} else {
		if (y) x <- sql.FundHistory.sf(x) else x <- sql.FundHistory.macro(x)
		z <- sql.tbl(n, "FundHistory", sql.and(x))
	}
#
# RETURN RESULT
	z
}
sql.FundHistory.macro <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.FundHistory.macro
# Author	: VKS
# Date		: 12/6/18,1/8/20,3/6,5/5,10/1,10/2,10/14/22
# Args		: x = a vector of filters
# Output	: SQL query where clause
# -----------------------------------------------------------------
	n <- vec.ex.filters("macro")
#
# CREATE RESULT
	z <- list()
	for (y in x) {
		if (any(y == names(n))) {
			z[[char.ex.int(length(z) + 65)]] <- n[y]
		} else if (y == "CB") {
			z[[char.ex.int(length(z) + 65)]] <- c("(", sql.and(sql.cross.border(F), "or"), ")")
		} else if (y == "UI") {
			z[[char.ex.int(length(z) + 65)]] <- sql.ui()
		} else if (y == "Foreign") {
			z <- c(z, sql.Foreign())
		} else {
			z[[char.ex.int(length(z) + 65)]] <- y
		}
	}
#
# RETURN RESULT
	z
}
sql.FundHistory.sf <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.FundHistory.sf
# Author	: VKS
# Date		: 12/6/18,10/23/19,3/6/20,5/7,9/24,10/2
# Args		: x = a vector of filters
# Output	: SQL query where clause
# -----------------------------------------------------------------
	n <- vec.ex.filters("sf")
#
# CREATE RESULT
	z <- list()
	for (h in x) {
		if (any(h == names(n))) {
			z[[char.ex.int(length(z) + 65)]] <- n[h]
		} else if (h == "CBE") {
			z[[char.ex.int(length(z) + 65)]] <- c("(", sql.and(sql.cross.border(T), "or"), ")")
		} else {
			z[[char.ex.int(length(z) + 65)]] <- h
		}
	}
#
# RETURN RESULT
	z
}
sql.HerdingLSV <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.HerdingLSV
# Author	: VKS
# Date		: 2/7/19
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = any of StockFlows/China/Japan/CSI300/Energy
# Output	: Generates ingredients of the herding measure set forth in LSV's 1991
#		:	paper "Do institutional investors destabilize stock prices?"
# -----------------------------------------------------------------
	z <- sql.drop(c("#NEW", "#OLD", "#FLO"))
#
# BASIC TABLES
	z <- c(z, "", sql.into(sql.tbl("HSecurityId, HFundId, FundId, HoldingValue", "Holdings", paste0("ReportDate = '", yyyymm.to.day(x), "'")), "#NEW"))
	z <- c(z, "", sql.into(sql.tbl("HSecurityId, FundId, HoldingValue", "Holdings", paste0("ReportDate = '", yyyymm.to.day(yyyymm.lag(x)), "'")), "#OLD"))
#
	w <- list(A = paste0("ReportDate = '", yyyymm.to.day(x), "'"))
	w[["B"]] <- "t1.HFundId in (select HFundId from FundHistory where [Index] = 0)"
	w[["C"]] <- "t1.HFundId in (select HFundId from #NEW)"
	w[["D"]] <- "FundId in (select FundId from #OLD)"
	w <- sql.tbl("t1.HFundId, FundId, Flow = sum(Flow)", "MonthlyData t1 inner join FundHistory t2 on t2.HFundId = t1.HFundId", sql.and(w), "t1.HFundId, FundId")
	z <- paste(c(z, "", sql.into(w, "#FLO")), collapse = "\n")
#
# FINAL RESULT
	h <- c("t1.HSecurityId", "prcRet = sum(t1.HoldingValue)/sum(t2.HoldingValue)")
	h <- sql.tbl(h, "#NEW t1 inner join #OLD t2 on t2.FundId = t1.FundId and t2.HSecurityId = t1.HSecurityId", "t1.HFundId in (select HFundId from FundHistory where [Index] = 1)", "t1.HSecurityId", "sum(t2.HoldingValue) > 0")
	h <- c("#FLO t0", "cross join", sql.label(h, "t1"), "cross join")
	h <- c(h, sql.label(sql.tbl("expPctBuy = sum(case when Flow > 0 then 1.0 else 0.0 end)/count(HFundId)", "#FLO"), "t4"))
	h <- c(h, "left join", "#NEW t2 on t2.HFundId = t0.HFundId and t2.HSecurityId = t1.HSecurityId")
	h <- c(h, "left join", "#OLD t3 on t3.FundId = t0.FundId and t3.HSecurityId = t1.HSecurityId")
	h <- c(h, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	#
	w <- c("SecurityId", "B = sum(case when isnull(t2.HoldingValue, 0) > isnull(t3.HoldingValue, 0) * prcRet then 1 else 0 end)")
	w <- c(w, "S = sum(case when isnull(t2.HoldingValue, 0) < isnull(t3.HoldingValue, 0) * prcRet then 1 else 0 end)", "expPctBuy = avg(expPctBuy)")
	w <- sql.tbl(w, h, sql.in("t1.HSecurityId", sql.RDSuniv(y)), "SecurityId")
	#
	z <- c(z, paste(sql.unbracket(w), collapse = "\n"))
#
# RETURN RESULT
	z
}
sql.Herfindahl <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.Herfindahl
# Author	: VKS
# Date		: 3/5/19,3/15,4/1,4/4,11/3/20,11/9,2/17/21
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of factors to be computed,
#		:	the last element of which is the type of fund used.
#		: n = any of StockFlows/China/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
# Output	: Generates Herfindahl dispersion and FundCt, the ownership breadth measure set forth in
#		:	Chen, Hong & Stein (2001)"Breadth of ownership and stock returns"
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
	z <- yyyymm.to.day(x)
	x <- sql.declare("@mo", "datetime", z)
#
# FILTERS
	if (n != "All") n <- list(A = sql.in("h.HSecurityId", sql.RDSuniv(n))) else n <- list()
	n[["B"]] <- "ReportDate = @mo"
	if (any(y$filter != "All")) n[["C"]] <- sql.in("h.HFundId", sql.FundHistory(y$filter, T))
	if (length(n) == 1) n <- n[[1]] else n <- sql.and(n)
#
# SELECT
	if (h == "GeoId") r <- "GeoId = GeographicFocusId" else r <- sql.breakdown(h)
	if (w) z <- c(sql.ReportDate(z), r, "HSecurityId") else z <- "SecurityId"
	for (j in y$factor) {
		if (j == "Herfindahl") {
			z <- c(z, paste(j, "1 - sum(square(HoldingValue))/square(sum(HoldingValue))", sep = " = "))
		} else if (j == "HerfindahlEq") {
			z <- c(z, paste(j, "1 - sum(square(HoldingValue/AssetsEnd))/square(sum(HoldingValue/AssetsEnd))", sep = " = "))
		} else if (j == "FundCt") {
			z <- c(z, paste(j, "count(h.HFundId)", sep = " = "))
		} else {
			stop("Bad factor", j)
		}
	}
#
# FROM
	r <- "Holdings h"
	if (!is.null(sql.breakdown(h))) r <- c(r, "inner join", "FundHistory t on t.HFundId = h.HFundId")
	if (!w) r <- c(r, "inner join", "SecurityHistory id on id.HSecurityId = h.HSecurityId")
	if (any(y$factor == "HerfindahlEq")) {
		r <- c(r, "inner join", sql.label(sql.MonthlyAssetsEnd("@mo"), "t on t.HFundId = h.HFundId"))
	}
#
# FINAL RESULT
	z <- sql.tbl(z, r, n, sql.1dFloMo.grp(w, h), "sum(HoldingValue) > 0")
	z <- paste(c(x, sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.Holdings <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.Holdings
# Author	: VKS
# Date		: 1/4/18,1/9,1/11,1/12
# Args		: x = where clause
#		: y = columns you want fetched
#		: n = the temp table for the output
# Output	: query to access stock-holdings data
# -----------------------------------------------------------------
	z <- sql.tbl(y, "Holdings", x)
	if (!missing(n)) z <- sql.into(z, n)
	z
}
sql.Holdings.bulk <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.Holdings.bulk
# Author	: VKS
# Date		: 4/10/18
# Args		: x = name of temp table with holdings
#		: y = columns of <x> (in order)
#		: n = the holdings date in YYYYMMDD
#		: w = unused temp table name for benchmark holdings
#		: h = unused temp table name for benchmark AUM
# Output	: query to bulk data with known benchmark holdings
# -----------------------------------------------------------------
	vec <- c(w, h)
#
# SUBSET TO INDEX FUNDS
	z <- sql.tbl("HFundId", "MonthlyData", paste0("ReportDate = '", n, "'"), "HFundId", "sum(AssetsEnd) > 0")
	z <- list(A = sql.in("HFundId", z), B = sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "[Index] = 1")))
	z <- sql.into(sql.tbl(y, x, sql.and(z)), vec[1])
#
# AUM OF SUCH FUNDS
	h <- list(A = sql.in("HFundId", sql.tbl("HFundId", vec[1])), B = paste0("ReportDate = '", n, "'"))
	z <- c(z, "", sql.into(sql.tbl("HFundId, AUM = sum(AssetsEnd)", "MonthlyData", sql.and(h), "HFundId"), vec[2]))
#
# FIND BIGGEST INDEX FUND BY BENCHMARK
	h <- sql.tbl("BenchIndexId, AUM = max(AUM)", c(paste(vec[2], "t1"), "inner join", "FundHistory t2 on t1.HFundId = t2.HFundId"),, "BenchIndexId")
	h <- c("FundHistory t1", "inner join", sql.label(h, "t2 on t1.BenchIndexId = t2.BenchIndexId"))
	h <- sql.tbl("HFundId, AUM", h, sql.and(list(A = paste(vec[2], "HFundId = t1.HFundId", sep = "."), B = paste(vec[2], "AUM = t2.AUM", sep = "."))))
	z <- c(z, "", paste("delete from", vec[2], "where not exists"), paste0("\t", h))
#
# SUBSET TO HOLDINGS OF BIGGEST INDEX FUNDS
	z <- c(z, "", paste("delete from", vec[1], "where HFundId not in (select HFundId from", vec[2], ")"), "")
#
# SCALE BY AUM
	z <- c(z, paste0("update ", vec[1], " set HoldingValue = HoldingValue/AUM from ", vec[2], " where ", vec[1], ".HFundId = ", vec[2], ".HFundId"))
	z <- c(z, "", sql.drop(vec[2]))
#
# BULK UP HOLDINGS
	w <- sql.tbl("HFundId, AUM = sum(AssetsEnd)", "MonthlyData", paste0("ReportDate = '", n, "'"), "HFundId", "sum(AssetsEnd) > 0")
	w <- c(sql.label(w, "t1"), "inner join", "FundHistory t2 on t1.HFundId = t2.HFundId")
	w <- c(w, "inner join", "FundHistory t3 on t2.BenchIndexId = t3.BenchIndexId")
	w <- c(w, "inner join", paste(vec[1], "t4 on t4.HFundId = t3.HFundId"))
	#
	h <- sql.and(list(A = "t2.[Index] = 1", B = sql.in("t1.HFundId", sql.tbl("HFundId", x), F)))
	#
	y <- ifelse(y == "FundId", "t2.FundId", y)
	y <- ifelse(y == "HFundId", "t1.HFundId", y)
	y <- ifelse(y == "HoldingValue", "HoldingValue = t4.HoldingValue * t1.AUM", y)
	z <- c(z, "", "insert into", paste0("\t", x), sql.unbracket(sql.tbl(y, w, h)), "", sql.drop(vec[1]))
#
# RETURN RESULT
	z
}
sql.Holdings.bulk.wrapper <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.Holdings.bulk.wrapper
# Author	: VKS
# Date		: 11/24/21
# Args		: x = name of temp table with holdings
#		: y = the type of fund used in the computation
#		: n = the holdings date in YYYYMMDD
#		: w = unused temp table name for benchmark holdings
#		: h = unused temp table name for benchmark AUM
# Output	: query to bulk data with known benchmark holdings
# -----------------------------------------------------------------
	z <- NULL
	if (any(y == "Pseudo")) {
		cols <- c("FundId", "HFundId", "HSecurityId", "HoldingValue")
		z <- c("", sql.Holdings.bulk(x, cols, n, w, h), "")
	}
#
# RETURN RESULT
	z
}
sql.HSIdmap <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.HSIdmap
# Author	: VKS
# Date		: 5/15/19
# Args		: x = the YYYYMM for which you want data (known 26 days later)
# Output	: Generates the SQL query to map SecurityId to HSecurityId
# -----------------------------------------------------------------
	z <- sql.in("HSecurityId", sql.tbl("HSecurityId", "Holdings", "ReportDate = @mo", "HSecurityId"))
	z <- sql.unbracket(sql.tbl(c("SecurityId", "HSecurityId"), "SecurityHistory", z))
	z <- paste(c(sql.declare("@mo", "datetime", yyyymm.to.day(x)), z), collapse = "\n")
	z
}
sql.in <- function(x, y, n = T) {
# -----------------------------------------------------------------
# Name		: sql.in
# Author	: VKS
# Date		: 1/11/18,4/10,12/31/19
# Args		: x = column
#		: y = SQL statement
#		: n = T/F depending on whether <x> is in <y>
# Output	: <x> in <y> if <n> or <x> not in <y> otherwise
# -----------------------------------------------------------------
	c(paste(x, ifelse(n, "in", "not in")), paste0("\t", y))
}
sql.index <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.index
# Author	: VKS
# Date		: 1/28/21
# Args		: x = table name
#		: y = string of column labels to index by (e.g. "DayEnding, FundId")
# Output	: SQL for primary key on <x> by columns <y>
# -----------------------------------------------------------------
	paste0("create unique clustered index ", substring(x, 2, nchar(x)), "Index ON ", x, " (", y, ")")
}
sql.into <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.into
# Author	: VKS
# Date		: 1/12/18,9/16/19
# Args		: x = SQL statement
#		: y = the temp table for the output
# Output	: unbrackets and selects into <y>
# -----------------------------------------------------------------
	z <- sql.unbracket(x)
	n <- length(z)
	w <- z == "from"
	w <- w & !duplicated(w)
	if (sum(w) != 1) stop("Failure in sql.into!")
	w <- c(1:n, (1:n)[w] + 1:2/3 - 1)
	z <- c(z, "into", paste0("\t", y))[order(w)]
	z
}
sql.ION <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.ION
# Author	: VKS
# Date		: 9/14/17
# Args		: x = bit of SQL string
#		: y = bit of SQL string
# Output	: sum(case when <x> > 0 then <y> else 0 end)/case when sum(abs(<y>)) > 0 then sum(abs(<y>)) else NULL end
# -----------------------------------------------------------------
	z <- paste0("= sum(case when ", x, " > 0 then ", y, " else 0 end)")
	z <- paste0(z, "/", sql.nonneg(paste0("sum(abs(", y, "))")))
	z
}
sql.isin.old.to.new <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.isin.old.to.new
# Author	: VKS
# Date		: 12/14/16,1/5/18,1/9,1/10,1/11
# Args		: x = Historical Isin
# Output	: Returns the latest isin
# -----------------------------------------------------------------
	z <- sql.tbl("Id", "SecurityCode", sql.and(list(A = "SecurityCodeTypeId = 1", B = "SecurityCode = @isin")))
	z <- sql.tbl("HSecurityId", "SecurityCodeMapping", sql.in("SecurityCodeId", z))
	z <- sql.tbl("SecurityId", "SecurityHistory", sql.in("HSecurityId", z))
	z <- sql.tbl("HSecurityId", "SecurityHistory", sql.and(list(A = "EndDate is NULL", B = sql.in("SecurityId", z))))
	z <- sql.tbl("SecurityCodeId", "SecurityCodeMapping", sql.and(list(A = "SecurityCodeTypeId = 1", B = sql.in("HSecurityId", z))))
	z <- sql.tbl("SecurityCode", "SecurityCode", sql.and(list(A = "SecurityCodeTypeId = 1", B = sql.in("Id", z))))
	z <- paste(c(sql.declare("@isin", "char(12)", x), z), collapse = "\n")
	z
}
sql.label <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.label
# Author	: VKS
# Date		: 1/5/18,1/12
# Args		: x = SQL statement
#		: y = label
# Output	: labels <x> as <y>
# -----------------------------------------------------------------
	z <- length(x)
	if (z == 1) z <- paste(x, y) else z <- c(x[-z], paste(x[z], y))
	z
}
sql.map.classif <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.map.classif
# Author	: VKS
# Date		: 11/28/16,1/23/18,1/24,3/14/21
# Args		: x = SQL queries to be submitted
#		: y = a connection, the output of odbcDriverConnect
#		: n = classif file
# Output	: Returns flow variables with the same row space as <w>
# Import	: @importFrom RODBC sqlQuery
# -----------------------------------------------------------------
	z <- sql.query.underlying(x, y, F)
	z <- map.rname(mat.index(z, "SecurityId"), dimnames(n)[[1]])
	if (is.null(dim(z))) z <- as.numeric(z)
	z
}
sql.mat.cofactor <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.mat.cofactor
# Author	: VKS
# Date		: 12/19/19
# Args		: x = square character matrix
# Output	: SQL for the cofactor matrix
# -----------------------------------------------------------------
#
# CREATE RESULT
	z <- matrix("", dim(x)[1], dim(x)[2], F, dimnames(x))
#
# POPULATE RESULT
	for (i in 1:dim(z)[1]) {
		for (j in 1:dim(z)[2]) {
			z[i, j] <- sql.mat.determinant(x[-i, -j])
			if ((i + j) %% 2 == 1) z[i, j] <- sql.mat.flip(z[i, j])
		}
	}
#
# RETURN RESULT
	z
}
sql.mat.crossprod <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.mat.crossprod
# Author	: VKS
# Date		: 12/18/19
# Args		: x = vector of names
#		: y = T/F depending on whether there's an intercept term
# Output	: SQL for entries of X'X
# -----------------------------------------------------------------
#
# PRELIMINARIES
	m <- length(x)
	names(x) <- 1:m
#
# PAIRS
	z <- rep(1:m, m)
	w <- z[order(rep(1:m, m))]
#
# ORDERED PAIRS
	h <- vec.max(w, z)
	z <- vec.min(w, z)
#
# MAP NAMES
	z <- map.rname(x, z)
	h <- map.rname(x, h)
#
# RESULT
	z <- ifelse(z == h, paste0("sum(square(", z, "))"), paste0("sum(", z, " * ", h, ")"))
	z <- matrix(z, m, m, F, list(x, x))
#
# ADD INTERCEPT
	if (y) {
		z <- map.rname(z, c("Unity", x))
		z <- t(map.rname(t(z), c("Unity", x)))
		z[1, -1] <- z[-1, 1] <- paste0("sum(", x, ")")
		z[1, 1] <- paste0("count(", x[1], ")")
	}
#
# RETURN RESULT
	z
}
sql.mat.crossprod.vector <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.mat.crossprod.vector
# Author	: VKS
# Date		: 12/19/19
# Args		: x = vector of names
#		: y = a string
#		: n = T/F depending on whether there's an intercept term
# Output	: SQL for entries of X'Y
# -----------------------------------------------------------------
#
# CREATE RESULT
	z <- vec.named(paste0("sum(", x, " * ", y, ")"), x)
#
# INTERCEPT
	if (n) {
		z["Unity"] <- paste0("sum(", y, ")")
		w <- length(z)
		z <- z[order(1:w %% w)]
	}
#
# RETURN RESULT
	z
}
sql.mat.determinant <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.mat.determinant
# Author	: VKS
# Date		: 12/18/19,12/19,1/9/20
# Args		: x = square character matrix
# Output	: SQL for the determinant
# -----------------------------------------------------------------
	n <- dim(x)[2]
	if (is.null(n)) {
		z <- x
	} else if (n == 2) {
		z <- sql.mat.multiply(x[1, 2], x[2, 1])
		z <- paste0(sql.mat.multiply(x[1, 1], x[2, 2]), " - ", z)
	} else {
		i <- 1
		z <- paste0(x[1, i], " * (", sql.mat.determinant(x[-1, -i]), ")")
		for (i in 2:n) {
			h <- ifelse(i %% 2 == 0, " - ", " + ")
			z <- paste(z, paste0(x[1, i], " * (", sql.mat.determinant(x[-1, -i]), ")"), sep = h)
		}
	}
#
# RETURN RESULT
	z
}
sql.mat.flip <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.mat.flip
# Author	: VKS
# Date		: 12/19/19
# Args		: x = square character matrix
# Output	: flips the sign for a term in a matrix
# -----------------------------------------------------------------
#
# FIND THE TERMS
	h <- NULL
	n <- nchar(x)
	i <- 1
	m <- 0
	while (i <= n) {
		if (m == 0 & is.element(substring(x, i, i), c("+", "-"))) {
			h <- c(h, i)
		} else if (substring(x, i, i) == "(") {
			m <- m + 1
		} else if (substring(x, i, i) == ")") {
			m <- m - 1
		}
		i <- i + 1
	}
#
# CREATE RESULT
	if (!is.null(h)) {
		h <- c(-1, h, n + 2)
		i <- 2
		z <- substring(x, h[i] + 2, h[i + 1] - 2)
		while (i + 3 <= length(h)) {
			i <- i + 2
			z <- paste(z, substring(x, h[i] + 2, h[i + 1] - 2), sep = " + ")
		}
		i <- -1
		while (i + 3 <= length(h)) {
			i <- i + 2
			z <- paste(z, substring(x, h[i] + 2, h[i + 1] - 2), sep = " - ")
		}
	} else {
		z <- paste0("(-", x, ")")
	}
#
# RETURN RESULT
	z
}
sql.mat.multiply <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.mat.multiply
# Author	: VKS
# Date		: 12/18/19
# Args		: x = string
#		: y = string
# Output	: SQL for the determinant
# -----------------------------------------------------------------
	if (x == y) {
		z <- paste0("square(", x, ")")
	} else {
		z <- paste(x, y, sep = " * ")
	}
#
# RETURN RESULT
	z
}
sql.median <- function(x, y, n, w = 0.5) {
# -----------------------------------------------------------------
# Name		: sql.median
# Author	: VKS
# Date		: 11/13/20
# Args		: x = column on which computation is run
#		: y = column on which partitioning is performed
#		: n = SQL statement
#		: w = desired ptile break point
# Output	: median (or alternate ptile point) of <x> within <y>
# -----------------------------------------------------------------
#
# PTILES
	z <- paste0("Ptile = PERCENT_RANK() over (partition by ", y, " order by ", x, ")")
	z <- sql.label(sql.tbl(c(x, y, z), sql.label(n, "t")), "t")
#
# GROUP
	h <- paste0(c("Mx", "Mn"), " = ", c("max", "min"), "(case when Ptile ", c("<= ", ">= "), w, " then ", x, " else NULL end)")
	z <- sql.label(sql.tbl(c(y, h), z,, y), "t")
#
# CREATE RESULT
	z <- sql.tbl(c(y, "Stat = (Mx + isnull(Mn, Mx))/2"), z)
#
# RETURN RESULT
	z
}
sql.Mo <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.Mo
# Author	: VKS
# Date		: 1/5/18,11/1,1/2/20
# Args		: x = vector of "flow"
#		: y = isomekic vector of "assets"
#		: n = isomekic vector of "weights" (can be NULL)
#		: w = T/F depending on whether to handle division by zero
# Output	: SQL statement for momentum
# -----------------------------------------------------------------
#
# DENOMINATOR
	if (is.null(n)) {
		z <- paste0("sum(", y, ")")
	} else {
		z <- paste0("sum(", y, " * cast(", n, " as float))")
	}
	if (w) {
		w <- sql.nonneg(z)
	} else {
		w <- z
	}
#
# CREATE RESULT
	if (is.null(n)) {
		z <- paste0("sum(", x, ")")
	} else {
		z <- paste0("sum(", x, " * cast(", n, " as float))")
	}
	z <- paste0("= 100 * ", z, "/", w)
#
# RETURN RESULT
	z
}
sql.MonthlyAlloc <- function(x, y = "All") {
# -----------------------------------------------------------------
# Name		: sql.MonthlyAlloc
# Author	: VKS
# Date		: 10/28/16,11/9,1/18/17,1/4/18,1/9,1/11,10/8,11/16/20
# Args		: x = YYYYMMDD for which you want allocations
#		: y = any of StockFlows/China/Japan/CSI300/Energy
# Output	: Generates the SQL query to get the data for monthly allocations for StockFlows
# -----------------------------------------------------------------
	z <- paste("ReportDate =", x)
	if (y != "All") z <- sql.and(list(A = z, B = sql.in("HSecurityId", sql.RDSuniv(y))))
	z <- sql.Holdings(z, c("FundId", "HFundId", "HSecurityId", "HoldingValue"))
	z
}
sql.MonthlyAssetsEnd <- function(x, y = F, n = F) {
# -----------------------------------------------------------------
# Name		: sql.MonthlyAssetsEnd
# Author	: VKS
# Date		: 10/28/16,11/1,11/9,1/9/18,1/11,10/1,11/17/20
# Args		: x = YYYYMMDD for which you want flows (known one day later)
#		: y = T/F variable depending on whether you want AssetsStart/AssetsEnd or just AssetsEnd
#		: n = T/F depending on whether data are indexed by FundId
# Output	: Generates the SQL query to get the data for monthly Assets End
# -----------------------------------------------------------------
#
# PRELIMINARIES
	z <- ifelse(n, "FundId", "HFundId")
	z <- c(z, "AssetsEnd = sum(AssetsEnd)")
	h <- "sum(AssetsEnd) > 0"
	if (y) {
		z <- c(z, "AssetsStart = sum(AssetsStart)")
		h <- sql.and(list(A = h, B = "sum(AssetsStart) > 0"))
	}
#
# CREATE RESULT
	if (n) {
		z <- sql.tbl(z, "MonthlyData t1 inner join FundHistory t2 on t2.HFundId = t1.HFundId", paste("ReportDate =", x), "FundId", h)
	} else {
		z <- sql.tbl(z, "MonthlyData", paste("ReportDate =", x), "HFundId", h)
	}
#
# RETURN RESULT
	z
}
sql.nonneg <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.nonneg
# Author	: VKS
# Date		: 11/14/16,12/18/17
# Args		: x = bit of sql string
# Output	: case when <x> > 0 then <x> else NULL end
# -----------------------------------------------------------------
	paste("case when", x, "> 0 then", x, "else NULL end")
}
sql.Overweight <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.Overweight
# Author	: VKS
# Date		: 11/12/20
# Args		: x = a YYYYMMDD date
# Output	: weight/shares normalized across stocks, then funds
# -----------------------------------------------------------------
#
# SHARES
	z <- c("HSecurityId", "HFundId", "SharesHeld")
	z <- sql.Holdings(paste0("ReportDate = '", x, "'"), z)
#
# TOTAL SHARES
	h <- c("HSecurityId", "TotShs = sum(SharesHeld)")
	h <- sql.tbl(h, "Holdings", paste0("ReportDate = '", x, "'"), "HSecurityId", "sum(SharesHeld) > 0")
	h <- c(sql.label(z, "t1"), "inner join", sql.label(h, "t2 on t2.HSecurityId = t1.HSecurityId"))
#
# NORMALIZED SHARES
	z <- sql.tbl(c("t1.HSecurityId", "HFundId", "NormShs = SharesHeld/TotShs"), h)
	h <- sql.tbl(c("HFundId", "TotNormShs = sum(SharesHeld/TotShs)"), h,, "HFundId", "sum(SharesHeld/TotShs) > 0")
	z <- c(sql.label(z, "t1"), "inner join", sql.label(h, "t2 on t2.HFundId = t1.HFundId"))
#
# OVERWEIGHT FACTOR (EXCLUDING ZERO)
	z <- sql.tbl(c("t1.HSecurityId", "t1.HFundId", "Overweight = NormShs/TotNormShs"), z)
#
# RETURN RESULT
	z
}
sql.query.underlying <- function(x, y, n = T) {
# -----------------------------------------------------------------
# Name		: sql.query.underlying
# Author	: VKS
# Date		: 10/23/18,11/21/19,5/7/20
# Args		: x = query needed for the update
#		: y = a connection, the output of odbcDriverConnect
#		: n = T/F depending on whether you wish to output number of rows of data got
# Output	: opens a connection, executes sql query, then closes the connection
# Import	: @importFrom RODBC sqlQuery
# -----------------------------------------------------------------
	for (i in x) z <- sqlQuery(y, i, stringsAsFactors = F)
	if (n) cat("Getting", txt.ex.int(dim(z)[1]), "new", ifelse(dim(z)[1] != 1, "rows", "row"), "of data ...\n")
	z
}
sql.RDSuniv <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.RDSuniv
# Author	: VKS
# Date		: 10/28/16,3/30/17,5/25,5/26,7/6,9/21,9/28,9/29,1/9/18,
#		: 1/11,1/23,10/1,8/13/19,8/30,11/4,2/18/20,12/7,3/17/21,
#		: 10/25,3/21/22
# Args		: x = any of StockFlows/China/Japan/CSI300/Energy
# Output	: Generates the SQL query to get the row space for a
#		:	stock flows research data set
# -----------------------------------------------------------------
	if (any(x == c("StockFlows", "StockFlowsJP", "US", "Japan", "CSI300"))) {
		if (x == "CSI300") {
			bmks <- vec.named("CSI300", 31873)
		} else if (x == "StockFlowsJP") {
			bmks <- vec.named(c("Kokusai", "Topix"), c(14601, 17558))
		} else if (x == "Japan") {
			bmks <- vec.named(c("Nikkei", "Topix"), c(13667, 17558))
		} else if (x == "US") {
			bmks <- vec.named(c("S&P500", "R3"), c(5164, 5158))
		} else if (x == "StockFlows") {
			bmks <- c("S&P500", "Eafe", "Gem", "R3", "EafeSc", "GemSc", "Canada", "CanadaSc", "R1", "R2", "Nikkei", "Topix", "CSI300")
			names(bmks) <- c(5164, 4430, 4835, 5158, 14602, 16621, 7744, 29865, 5152, 5155, 13667, 17558, 31873)
		}
		z <- sql.and(vec.to.list(paste("FundId =", paste(names(bmks), bmks, sep = " --"))), "or")
		z <- sql.in("HFundId", sql.tbl("HFundId", "FundHistory", z))
		z <- sql.tbl("HSecurityId", "Holdings", z, "HSecurityId")
	} else if (x == "File") {
		z <- paste0("(", paste(readLines("C:\\temp\\crap\\ids.txt"), collapse = ", "), ")")
	} else if (x == "China") {
		z <- sql.tbl("HCompanyId", "CompanyHistory", "CountryCode = 'CN'")
		z <- sql.tbl("HSecurityId", "SecurityHistory", sql.in("HCompanyId", z))
		z <- sql.in("HSecurityId", z)
		#
		z <- list(A = z, B = sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "GeographicFocusId = 16")))
		#
		z <- sql.and(z, "or")
		z <- sql.tbl("HSecurityId", "Holdings", z, "HSecurityId")
	} else if (x == "R1") {
		z <- sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "FundId = 5152"))
		z <- sql.tbl("HSecurityId", "Holdings", z, "HSecurityId")
	} else if (x == "R3") {
		z <- sql.in("HFundId", sql.tbl("HFundId", "FundHistory", "FundId = 5158"))
		z <- sql.tbl("HSecurityId", "Holdings", z, "HSecurityId")
	} else if (x == "All") {
		z <- ""
	} else {
		stop("Unknown universe!")
	}
	z
}
sql.regr <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: sql.regr
# Author	: VKS
# Date		: 12/19/19
# Args		: x = a string vector (independent variable(s))
#		: y = a string (dependent variable)
#		: n = T/F depending on whether there's an intercept term
# Output	: SQL for regression coefficients
# -----------------------------------------------------------------
#
# PIECES
	y <- sql.mat.crossprod.vector(x, y, n) # X'Y
	x <- sql.mat.crossprod(x, n) # X'X
	h <- sql.mat.cofactor(x)
	n <- sql.mat.determinant(x)
#
# CREATE RESULT
	z <- NULL
	for (j in seq_along(y)) {
		w <- paste(paste0(y, " * (", h[, j], ")"), collapse = " + ")
		w <- paste0("(", w, ")/(", n, ")")
		z <- c(z, paste(names(y)[j], w, sep = " = "))
	}
#
# RETURN RESULT
	z
}
sql.ReportDate <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.ReportDate
# Author	: VKS
# Date		: 11/9/20
# Args		: x = a YYYYMMDD date
# Output	: SQL select statement for constant date <x>
# -----------------------------------------------------------------
	paste0("ReportDate = '", yyyymmdd.to.txt(x), "'")
}
sql.ShareClass <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.ShareClass
# Author	: VKS
# Date		: 5/4/22
# Args		: x = date restriction
#		: y = share-class filter (one of All/Inst/Retail)
# Output	: Generates where clause for share-class filter
# -----------------------------------------------------------------
	if (any(y == c("Inst", "Retail"))) {
		z <- sql.tbl("[Id]", "ShareClasses", "Institutional = 1")
		z <- sql.in("ShareClassId", z, y == "Inst")
		z <- sql.and(list(A = x, B = z))
	} else z <- x
#
# RETURN RESULT
	z
}
sql.SRI <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.SRI
# Author	: VKS
# Date		: 2/7/19
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = any of StockFlows/Japan/CSI300/Energy
# Output	: number of SRI funds holding the stock at time <x>
# -----------------------------------------------------------------
	w <- list(A = "ReportDate = @holdDt", B = "HFundId in (select HFundId from FundHistory where SRI = 1)")
	z <- sql.label(sql.tbl("HSecurityId, Ct = count(HFundId)", "Holdings", sql.and(w), "HSecurityId"), "t1")
	z <- c(z, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
	z <- sql.tbl("SecurityId, Ct = sum(Ct)", z, sql.in("t1.HSecurityId", sql.RDSuniv(y)), "SecurityId")
	z <- c(sql.declare("@holdDt", "datetime", yyyymm.to.day(x)), sql.unbracket(z))
	z <- paste(z, collapse = "\n")
	z
}
sql.tbl <- function(x, y, n, w, h, u) {
# -----------------------------------------------------------------
# Name		: sql.tbl
# Author	: VKS
# Date		: 1/11/18,1/12,1/16,6/8,1/23/19,10/13/22
# Args		: x = needed columns
#		: y = table
#		: n = where clause
#		: w = "group by" clause
#		: h = having clause
#		: u = order by clause
# Output	: Full SQL statement
# -----------------------------------------------------------------
	m <- length(x)
#
# APPEND COMMAS: COMPLEXITY USED IN sql.1dFloMo.FI.underlying
	z <- c(txt.left(x[-1], 1) != "\t", F)
	z <- paste0(x, ifelse(z, ",", ""))
#
# INDENTATION
	z <- c("(select", paste0("\t", txt.replace(z, "\n", "\n\t")))
#
# INDENT TABLES
	x <- txt.right(y, 5) == " join"
	x <- x & txt.left(c(y[-1], ""), 1) != "\t"
	x <- ifelse(x, "", "\t")
	z <- c(z, "from", paste0(x, txt.replace(y, "\n", "\n\t")))
#
# FINISH UP
	if (!missing(n)) z <- c(z, "where", paste0("\t", n))
	if (!missing(w)) z <- c(z, "group by", paste0("\t", w))
	if (!missing(h)) z <- c(z, "having", paste0("\t", h))
	if (!missing(u)) z <- c(z, "order by", paste0("\t", u))
	z <- c(z, ")")
#
# RETURN RESULT
	z
}
sql.TopDownAllocs <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.TopDownAllocs
# Author	: VKS
# Date		: 2/14/17,1/4/18,1/9,1/10,1/11,1/12,10/8,10/9,11/1,11/2,
#		: 11/16/20,12/3
# Args		: x = the YYYYMM for which you want data (known 26 days later)
#		: y = a string vector of top-down allocations wanted,
#		:	the last element of which is the type of fund to be used.
#		: n = any of StockFlows/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
# Output	: Generates the SQL query to get Active/Passive Top-Down Allocations
# -----------------------------------------------------------------
	z <- sql.TopDownAllocs.underlying(yyyymm.to.day(x), y, n, w, h)
	z <- paste(sql.unbracket(z), collapse = "\n")
	z
}
sql.TopDownAllocs.items <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: sql.TopDownAllocs.items
# Author	: VKS
# Date		: 5/10/18,9/17
# Args		: x = a string vector specifying types of allocation wanted
#		: y = T/F depending on whether select item or having entry is desired
# Output	: allocations to select in Top-Down Allocations SQL query
# -----------------------------------------------------------------
	if (y) {
		z <- NULL
		for (i in x) {
			if (i == "SwtdEx0") {
				z <- c(z, "SwtdEx0 = 100 * avg(HoldingValue/AssetsEnd)")
			} else if (i == "SwtdIn0") {
				z <- c(z, "SwtdIn0 = 100 * sum(HoldingValue/AssetsEnd)/count(AssetsEnd)")
			} else if (i == "FwtdEx0") {
				z <- c(z, "FwtdEx0 = 100 * sum(HoldingValue)/sum(case when HoldingValue is not null then AssetsEnd else NULL end)")
			} else if (i == "FwtdIn0") {
				z <- c(z, "FwtdIn0 = 100 * sum(HoldingValue)/sum(AssetsEnd)")
			} else {
				stop("Bad Argument")
			}
		}
	} else if (length(x) > 1) {
		stop("Element expected, not vector")
	} else {
		if (x == "SwtdEx0") {
			z <- "count(HoldingValue/AssetsEnd) > 0"
		} else if (x == "SwtdIn0") {
			z <- "count(AssetsEnd) > 0"
		} else if (x == "FwtdEx0") {
			z <- "sum(case when HoldingValue is not null then AssetsEnd else NULL end) > 0"
		} else if (x == "FwtdIn0") {
			z <- "sum(AssetsEnd) > 0"
		} else {
			stop("Bad Argument")
		}
	}
#
# RETURN RESULT
	z
}
sql.TopDownAllocs.underlying <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: sql.TopDownAllocs.underlying
# Author	: VKS
# Date		: 11/16/20,12/3
# Args		: x = the YYYYMMDD for which you want data
#		: y = a string vector of top-down allocations wanted,
#		:	the last element of which is the type of fund to be used.
#		: n = any of StockFlows/Japan/CSI300/Energy
#		: w = T/F depending on whether you are checking ftp
#		: h = breakdown filter (e.g. All/GeoId/DomicileId)
# Output	: Generates the SQL query to get Active/Passive Top-Down Allocations
# -----------------------------------------------------------------
#
# PARSE OPTIONAL ARGUMENTS
	y <- sql.arguments(y)
#
# PRELIMINARIES
	u <- paste0("ReportDate = '", x, "'")
	if (n == "All") n <- list() else n <- list(A = sql.in("HSecurityId", sql.RDSuniv(n)))
	n[[char.ex.int(length(n) + 65)]] <- u
	n <- sql.and(n)
#
# FROM PART OF SQL STATEMENT
	r <- sql.FundHistory(y$filter, T, c("FundId", sql.breakdown(h)))
	r <- c("inner join", sql.label(r, "t2"), "\ton t2.HFundId = t1.HFundId")
	r <- c(sql.label(sql.tbl("HFundId, AssetsEnd = sum(AssetsEnd)", "MonthlyData", u, "HFundId", "sum(AssetsEnd) > 0"), "t1"), r)
	r <- sql.tbl(c("FundId", sql.breakdown(h), "AssetsEnd"), r, sql.in("FundId", sql.tbl("FundId", "Holdings h", u)))
	r <- sql.label(r, "t2")
	if (h != "All") {
		v <- c("HSecurityId", sql.breakdown(h))
		u <- c("Holdings t1", "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
		u <- sql.tbl(v, u, n, paste(v, collapse = ", "))
		r <- c(r, "inner join", sql.label(u, "t1"))
		r <- c(r, paste0("\ton t1.", sql.breakdown(h), " = t2.", sql.breakdown(h)))
	} else {
		r <- c(r, "cross join", sql.label(sql.tbl("HSecurityId", "Holdings", n, "HSecurityId"), "t1"))
	}
	r <- c(r, "left join", sql.label(sql.Holdings(n, c("FundId", "HSId = HSecurityId", "HoldingValue")), "t3"))
	r <- c(r, "\ton t3.FundId = t2.FundId and HSId = HSecurityId")
	if (!w) r <- c(r, "inner join", "SecurityHistory id on id.HSecurityId = t1.HSecurityId")
#
# FINAL GROUP BY
	u <- ifelse(w, "HSecurityId", "SecurityId")
	if (h != "All") u <- paste(c(paste("t2.", sql.breakdown(h), sep = ""), u), collapse = ", ")
#
# SELECT STATEMENT
	if (h == "GeoId") {
		z <- "GeoId = t2.GeographicFocusId"
	} else if (h == "All") {
		z <- NULL
	} else {
		z <- paste0("t2.", h)
	}
	if (w) {
		z <- c(sql.ReportDate(x), z, "HSecurityId")
	} else {
		z <- c("SecurityId", z)
	}
	if (length(y$factor) == 1) {
		if (w) {
			n <- sql.TopDownAllocs.items(y$factor)
			n <- txt.right(n, nchar(n) - nchar(y$factor) - 1)
			n <- paste("AverageAllocation", n)
			z <- c(z, n)
		} else {
			z <- c(z, sql.TopDownAllocs.items(y$factor))
		}
		z <- sql.tbl(z, r,, u, sql.TopDownAllocs.items(y$factor, F))
	} else {
		z <- c(z, sql.TopDownAllocs.items(y$factor))
		z <- sql.tbl(z, r,, u)
	}
#
# RETURN RESULT
	z
}
sql.Trend <- function(x, y = "") {
# -----------------------------------------------------------------
# Name		: sql.Trend
# Author	: VKS
# Date		: 11/16/16,6/20/17,6/22,9/21/18,10/1,6/14/21
# Args		: x = bit of SQL string
#		: y = one of ""/"Num"/"Den"
# Output	:  = sum(<x>)/case when sum(<x>) = 0 then NULL else sum(<x>) end
# -----------------------------------------------------------------
	z <- paste0("= sum(", x, ")")
	if (y == "") {
		z <- paste0(z, "/", sql.nonneg(paste0("sum(abs(", x, "))")))
	} else if (y == "Den") {
		z <- paste0("= sum(abs(", x, "))")
	}
#
# RETURN RESULT
	z
}
sql.ui <- function() {
# -----------------------------------------------------------------
# Name		: sql.ui
# Author	: VKS
# Date		: 9/10/18,9/13
# Args		: none
# Output	: funds to be displayed on the UI
# -----------------------------------------------------------------
#
# ALL FUNDS
	z <- list()
	z[["A"]] <- "FundType in ('M', 'A', 'Y', 'B', 'E')"
	z[["B"]] <- "GeographicFocus not in (0, 18, 48)"
	z[["C"]] <- "Category >= '1'"
	z[["D"]] <- "isActive = 'Y'"
	z <- c("(", sql.and(z), ")")
#
# COMMODITY-FUND BACK DOOR
	x <- list()
	x[["A"]] <- "Commodity = 'Y'"
	x[["B"]] <- "StyleSector in (101, 103)"
	x[["C"]] <- "FundType in ('Y', 'E')"
	x[["D"]] <- "isActive = 'Y'"
	x <- c("(", sql.and(x), ")")
#
# BRING TOGETHER
	z <- list(A = z, B = x)
	z <- c("(", sql.and(z, "or"), ")")
#
# RETURN RESULT
	z
}
sql.unbracket <- function(x) {
# -----------------------------------------------------------------
# Name		: sql.unbracket
# Author	: VKS
# Date		: 1/12/18
# Args		: x = string vector
# Output	: removes brackets around an SQL block
# -----------------------------------------------------------------
	n <- length(x)
	if (txt.left(x[1], 1) != "(" | x[n] != ")") stop("Can't unbracket!")
	x[1] <- txt.right(x[1], nchar(x[1]) - 1)
	z <- x[-n]
	z
}
sql.update <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.update
# Author	: VKS
# Date		: 6/28/22
# Args		: x = table name
#		: y = set argument
#		: n = from argument
#		: w = where clause
# Output	: update <x> set <y> from <n> where <w>
# -----------------------------------------------------------------
	c("update", paste0("\t", x), "set", paste0("\t", y), "from", paste0("\t", n), "where", paste0("\t", w))
}
sql.yield.curve <- function(x, y, n, w = "General", h = "FundId") {
# -----------------------------------------------------------------
# Name		: sql.yield.curve
# Author	: VKS
# Date		: 4/12/22,5/25,6/15
# Args		: x = value to match CompanyName field
#		: y = value to match SecurityType field
#		: n = value to match BondCurrency field
#		: w = type of maturity buckets
#		: h = fund identifier
# Output	: buckets holdings by maturities
# -----------------------------------------------------------------
#
# DETERMINE SUBSET
	z <- list(A = "ReportDate = @date")
	z[["BondMaturity"]] <- "BondMaturity is not null"
	z[["Future"]] <- "BondMaturity > @date"
	z[["CompanyName"]] <- paste0("CompanyName = '", x, "'")
	z[["SecurityType"]] <- paste0("SecurityType = '", y, "'")
	z[["BondCurrency"]] <- paste0("BondCurrency = '", n, "'")
#
# BUCKETS
	if (w == "US") {
		v <- vec.named(c(0, 500, 2500), c("ST", "IT", "LT"))
	} else {
		v <- vec.named(c(0, 730, 1826, 3652), c("y0-2", "y2-5", "y5-10", "y10+"))
	}
	v <- maturity.bucket(v)
	v <- sql.case("grp", v, c(names(v), "OTHER"), F)
#
# HOLDINGS
	y <- c(h, v, "HoldingValue")
	z <- sql.label(sql.tbl(y, "vwBondMonthlyHoldingsReport_WithoutEmbargo", sql.and(z)), "t")
	z <- sql.tbl(c(h, "grp", "HoldingValue = sum(HoldingValue)"), z,, paste0(h, ", grp"))
#
# RETURN RESULT
	z
}
sql.yield.curve.1dFloMo <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: sql.yield.curve.1dFloMo
# Author	: VKS
# Date		: 4/12/22,5/25
# Args		: x = value to match CompanyName field
#		: y = value to match SecurityType field
#		: n = value to match BondCurrency field
#		: w = vector of YYYYMMDD
# Output	: daily FloMo by yield-curve bucket
# -----------------------------------------------------------------
#
# FLOWS
	z <- c("Flow", "AssetsStart")
	z <- paste0(z, " = sum(", z, ")")
	z <- c("DayEnding", "FundId", z)
	z <- sql.Flow(z, list(A = paste0("'", w, "'")),,, T, paste(z[1:2], collapse = ", "))
#
# ALLOCATION DATE
	w <- yyyymm.to.day(yyyymmdd.to.AllocMo.unique(flowdate.lag(w, 5), 26, F))
	w <- sql.declare("@date", "datetime", w)
#
# HOLDINGS
	x <- sql.yield.curve(x, y, n)
	z <- c(sql.label(z, "t1"), "inner join", sql.label(x, "t2 on t2.FundId = t1.FundId"))
#
# AUM
	x <- c("FundId", "AssetsEnd = sum(AssetsEnd)")
	y <- c("MonthlyData t1", "inner join")
	y <- c(y, "FundHistory t2 on t2.HFundId = t1.HFundId")
	x <- sql.tbl(x, y, "MonthEnding = @date", "FundId", "sum(AssetsEnd) > 0")
	z <- c(z, "inner join", sql.label(x, "t3 on t3.FundId = t1.FundId"))
#
# FINAL
	x <- c(sql.yyyymmdd("DayEnding"), "grp", sql.1dFloMo.select("FloMo"))
	z <- sql.tbl(x, z,, "DayEnding, grp")
	z <- paste(c(w, "", sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sql.yyyymm <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sql.yyyymm
# Author	: VKS
# Date		: 11/23/20
# Args		: x = name of a datetime column in an SQL table
#		: y = label after conversion (defaults to <x> if missing)
# Output	: SQL code to convert to YYYYMM
# -----------------------------------------------------------------
	if (missing(y)) z <- x else z <- y
	z <- paste0(z, " = convert(char(6), ", x, ", 112)")
	z
}
sql.yyyymmdd <- function(x, y, n = F) {
# -----------------------------------------------------------------
# Name		: sql.yyyymmdd
# Author	: VKS
# Date		: 11/23/20,3/14/21
# Args		: x = name of a datetime column in an SQL table
#		: y = label after conversion (defaults to <x> if missing)
#		: n = T/F depending on whether you are checking ftp
# Output	: SQL code to convert to YYYYMMDD
# -----------------------------------------------------------------
	if (missing(y)) z <- x else z <- y
	if (n) {
		z <- paste0(z, " = convert(char(10), ", x, ", 101) + ' 12:00:00 AM'")
	} else {
		z <- paste0(z, " = convert(char(8), ", x, ", 112)")
	}
	z
}
sqlts.FloDollar.daily <- function(x) {
# -----------------------------------------------------------------
# Name		: sqlts.FloDollar.daily
# Author	: VKS
# Date		: 5/10/18,10/8,11/23/20
# Args		: x = the security id for which you want data
# Output	: SQL query for daily dollar flow
# -----------------------------------------------------------------
	x <- sql.declare("@secId", "int", x)
#
# DAILY FLOWS
	z <- sql.tbl(c("ReportDate", "HFundId", "Flow = sum(Flow)"), "DailyData", , "ReportDate, HFundId")
#
# FUND ID FROM FUND HISTORY
	z <- c(sql.label(z, "t1"), "inner join", "FundHistory t2 on t2.HFundId = t1.HFundId")
#
# HOLDINGS
	z <- c(z, "inner join", "Holdings t3 on t3.FundId = t2.FundId", paste("\tand", sql.datediff("t3.ReportDate", "t1.ReportDate", 26)))
#
# AUM
	h <- sql.tbl("ReportDate, HFundId, AUM = sum(AssetsEnd)", "MonthlyData",, "ReportDate, HFundId", "sum(AssetsEnd) > 0")
	z <- c(z, "inner join", sql.label(h, "t4"), "\ton t4.HFundId = t3.HFundId and t4.ReportDate = t3.ReportDate")
#
# DETERMINE SUBSET
	h <- sql.in("HSecurityId", sql.tbl("HSecurityId", "SecurityHistory", "SecurityId = @secId"))
#
# CREATE RESULT
	z <- sql.tbl(c(sql.yyyymmdd("t1.ReportDate", "yyyymmdd"), "FloDlr = sum(Flow * HoldingValue/AUM)"), z, h, "t1.ReportDate")
	z <- paste(c(x, "", sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sqlts.FloDollar.monthly <- function(x) {
# -----------------------------------------------------------------
# Name		: sqlts.FloDollar.monthly
# Author	: VKS
# Date		: 5/10/18,11/23/20
# Args		: x = the security id for which you want data
# Output	: SQL query for monthly dollar flow
# -----------------------------------------------------------------
	x <- sql.declare("@secId", "int", x)
#
# FLOW & AUM
	z <- sql.tbl(c("ReportDate", "HFundId", "Flow = sum(Flow)", "AUM = sum(AssetsEnd)"), "MonthlyData",, "ReportDate, HFundId", "sum(AssetsEnd) > 0")
#
# HOLDINGS
	z <- c(sql.label(z, "t1"), "inner join", "Holdings t2 on t2.HFundId = t1.HFundId and t2.ReportDate = t1.ReportDate")
#
# DETERMINE SUBSET
	h <- sql.in("HSecurityId", sql.tbl("HSecurityId", "SecurityHistory", "SecurityId = @secId"))
#
# CREATE RESULT
	z <- sql.tbl(c(sql.yyyymm("t1.ReportDate", "yyyymm"), "FloDlr = sum(Flow * HoldingValue/AUM)"), z, h, "t1.ReportDate")
	z <- paste(c(x, "", sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
sqlts.TopDownAllocs <- function(x, y) {
# -----------------------------------------------------------------
# Name		: sqlts.TopDownAllocs
# Author	: VKS
# Date		: 5/10/18,11/23/20
# Args		: x = the security id for which you want data
#		: y = a string vector specifying types of allocation wanted
# Output	: SQL query for Top-Down Allocations
# -----------------------------------------------------------------
	if (missing(y)) y <- paste0(txt.expand(c("S", "F"), c("Ex", "In"), "wtd"), "0")
	x <- sql.declare("@secId", "int", x)
#
# SUBSET TO FUNDS REPORTING HOLDINGS FOR A PARTICULAR MONTH
	z <- sql.and(list(A = "h.ReportDate = t.ReportDate", B = "h.HFundId = t.HFundId"))
	z <- sql.exists(sql.tbl("ReportDate, HFundId", "Holdings h", z))
	z <- sql.tbl("ReportDate, HFundId, AssetsEnd = sum(AssetsEnd)", "MonthlyData t", z, "ReportDate, HFundId", "sum(AssetsEnd) > 0")
	z <- sql.label(z, "t1")
#
# BRING IN HOLDINGS
	h <- sql.in("HSecurityId", sql.tbl("HSecurityId", "SecurityHistory", "SecurityId = @secId"))
	h <- sql.label(sql.Holdings(h, c("ReportDate", "HFundId", "HoldingValue")), "t2")
	z <- c(z, "left join", h, "\ton t2.HFundId = t1.HFundId and t2.ReportDate = t1.ReportDate")
#
# CREATE RESULT
	z <- sql.tbl(c(sql.yyyymm("t1.ReportDate", "yyyymm"), sql.TopDownAllocs.items(y)), z,, "t1.ReportDate")
	z <- paste(c(x, "", sql.unbracket(z)), collapse = "\n")
#
# RETURN RESULT
	z
}
straight <- function(x) {
# -----------------------------------------------------------------
# Name		: straight
# Author	: VKS
# Date		: 4/10/20
# Args		: x = a logical vector
# Output	: the number of elements equalling the first
# -----------------------------------------------------------------
	seq(1, 1 + length(x))[!duplicated(c(x, !x[1]))][2] - 1
}
strat.dir <- function(x) {
# -----------------------------------------------------------------
# Name		: strat.dir
# Author	: VKS
# Date		: 5/16/17,8/28,12/15
# Args		: x = frequency (e.g. "daily", "weekly" or "monthly")
# Output	: the folder where <x> factors live
# -----------------------------------------------------------------
	paste(dir.parameters("data"), x, sep = "\\")
}
strat.file <- function(x, y) {
# -----------------------------------------------------------------
# Name		: strat.file
# Author	: VKS
# Date		: 5/16/17
# Args		: x = vector of strategy names (e.g. "FX" or "FloPctCtry")
#		: y = frequency (e.g. "daily", "weekly" or "monthly")
# Output	: the path to the factor file
# -----------------------------------------------------------------
	paste0(x, "-", y, ".csv")
}
strat.list <- function(x) {
# -----------------------------------------------------------------
# Name		: strat.list
# Author	: VKS
# Date		: 1/27/20
# Args		: x = frequency (e.g. "daily", "weekly" or "monthly")
# Output	: lists strategies of that frequency
# -----------------------------------------------------------------
#
# DIRECTORY LISTING
	z <- dir(strat.dir(x))
	x <- paste0("-", x, ".csv")
#
# CREATE RESULT
	z <- z[txt.right(z, nchar(x)) == x]
	z <- txt.left(z, nchar(z) - nchar(x))
#
# RETURN RESULT
	z
}
strat.path <- function(x, y) {
# -----------------------------------------------------------------
# Name		: strat.path
# Author	: VKS
# Date		: 5/15/17,5/16
# Args		: x = name of the strategy (e.g. "FX" or "FloPctCtry")
#		: y = frequency (e.g. "daily", "weekly" or "monthly")
# Output	: Returns the full path to the factor file
# -----------------------------------------------------------------
	paste(strat.dir(y), strat.file(x, y), sep = "\\")
}
stratrets <- function(x) {
# -----------------------------------------------------------------
# Name		: stratrets
# Author	: VKS
# Date		: 10/27/22
# Args		: x = variable being back-tested (e.g. FloPct/AIS)
# Output	: data frame of TxB return spreads
# -----------------------------------------------------------------
#
# PARAMETERS
	z <- mat.read(parameters("classif-strat"), "\t", NULL)
	z <- vec.to.list(z[is.element(z[, "vbl"], x), "strat"], T)
#
# RUN BACK-TESTS
	z <- lapply(z, function(y) stratrets.bbk(y, x))
#
# CREATE RESULT
	z <- array.ex.list(z, T, T)
	z <- z[order(dimnames(z)[[1]]), ]
#
# LABEL BY END OF PERIOD
	z <- mat.ex.matrix(mat.lag(z, 1))
#
# REMOVE NA ROWS
	x <- min(sapply(z, function(x) find.data(!is.na(x), T)))
	x <- c(x, max(sapply(z, function(x) find.data(!is.na(x), F))))
	z <- z[seq(x[1], x[2]), ]
#
# RETURN RESULT
	z
}
stratrets.bbk <- function(x, y) {
# -----------------------------------------------------------------
# Name		: stratrets.bbk
# Author	: VKS
# Date		: 10/27/22
# Args		: x = strategy
#		: y = variable being back-tested (e.g. FloPct/AIS)
# Output	: named vector of TxB return spreads indexed by BoP
# -----------------------------------------------------------------
	cat("\t", x, y, "..\n")
#
# INPUTS
	x <- stratrets.data(x, y)
	x[["retW"]] <- ifelse(nchar(dimnames(x[["x"]])[[1]][1]) == 8, 5, 1)
#
# BACK-TEST
	z <- do.call(bbk, x)[["rets"]]
	z <- z[order(dimnames(z)[[1]]), ]
	z <- as.matrix(z)[, "TxB"]
#
# RETURN RESULT
	z
}
stratrets.beta <- function(x, y, n, w) {
# -----------------------------------------------------------------
# Name		: stratrets.beta
# Author	: VKS
# Date		: 10/26/22
# Args		: x = indicators
#		: y = total return indices
#		: n = benchmark (e.g. "ACWorld")
#		: w = lookback over which beta is computed
# Output	: beta-adjusted indicator
# -----------------------------------------------------------------
	portfolio.residual(x, map.rname(portfolio.beta.wrapper(y, n, w), dimnames(x)[[1]]))
}
stratrets.data <- function(x, y) {
# -----------------------------------------------------------------
# Name		: stratrets.data
# Author	: VKS
# Date		: 10/26/22,11/14
# Args		: x = strategy
#		: y = variable being back-tested (e.g. FloPct/AIS)
# Output	: list object containing arguments needed for function <bbk>
# -----------------------------------------------------------------
#
# PARAMETERS
	h <- mat.read(parameters("classif-strat"), "\t", NULL)
	h <- mat.index(h[is.element(h[, "vbl"], y), dimnames(h)[[2]] != "vbl"], "strat")
#
# INDICATOR
	if (x == "Multi") z <- c("Rgn-Act", "FI") else z <- x
	z <- paste0(fcn.dir(), "\\", h[z, "path"])
	z <- stratrets.indicator(z, h[x, "lkbk"], h[x, "comp"] == 0, h[x, "sec"] == 1, h[x, "delay"])
#
# SUBSET
	if (!is.na(h[x, "sub"])) z <- stratrets.subset(z, h[x, "sub"])
#
# TOTAL RETURN INDICES
	z <- list(x = z, y = stratrets.returns(h[x, "rets"])[, dimnames(z)[[2]]])
	if (nchar(dimnames(z[["x"]])[[1]][1]) == 6) z[["y"]] <- mat.daily.to.monthly(z[["y"]], T)
#
# BETA ADJUSTMENT
	if (!is.na(h[x, "beta"])) z[["x"]] <- stratrets.beta(z[["x"]], z[["y"]], h[x, "beta"], h[x, "lkbk"])
#
# FINAL RESULT
	h <- h[, !is.element(dimnames(h)[[2]], c("path", "lkbk", "comp", "beta", "sec", "sub", "rets"))]
	for (j in dimnames(h)[[2]]) if (!is.na(h[x, j])) z[[j]] <- h[x, j]
#
# RETURN RESULT
	z
}
stratrets.indicator <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: stratrets.indicator
# Author	: VKS
# Date		: 10/26/22
# Args		: x = paths to strategy files
#		: y = lookback
#		: n = if T, flows get summed. Otherwise they get compounded
#		: w = if T, sector-adjustment is performed
#		: h = delay in knowing data (only used when <w>)
# Output	: data frame compounded across <y>
# -----------------------------------------------------------------
#
# COMPOUNDED INDICATOR
	z <- compound.flows(multi.asset(x), y, n)
#
# SECTOR ADJUSTMENT
	if (w) {
		w <- dimnames(z)[[1]] >= yyyymmdd.lag("20160831", h)
		z[w, "Fins"] <- z[w, "FinsExREst"]
		z <- z[, dimnames(z)[[2]] != "FinsExREst"]
	}
#
# RETURN RESULT
	z
}
stratrets.path <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: stratrets.path
# Author	: VKS
# Date		: 11/15/22,11/16,11/17,11/22
# Args		: x = return type (e.g. Ctry/FX/SectorUK/Multi)
#		: y = FundType (e.g. E/B)
#		: n = filter (e.g. Aggregate, Act, SRI. etc.)
#		: w = item (e.g. Flow/AssetsStart/Result)
#		: h = CB/SG/CBSG
# Output	: path to strategy indicators
# -----------------------------------------------------------------
#
# SPECIAL OPERATIONS
	z <- NULL
	if (x == "FX" & y == "E" & n == "Aggregate" & w == "Flow" & h == "CB") {
		z <- strat.path("FX$", "daily")
	} else if (x == "FX" & y == "E" & n == "Aggregate" & w == "Result" & h == "CB") {
		z <- strat.path("FX", "daily")
	} else if (x == "Ctry" & y == "E" & n == "Aggregate" & w == "Result" & h == "CB") {
		z <- strat.path("FloPctCtry", "daily")
	} else if (y == "E" & n == "Act" & w == "Result" & h == "CB") {
		if (is.element(x, paste0("Sector", c("EM", "JP", "US", "UK", "Eurozone")))) {
			z <- txt.replace(x, "Sector", "FloPctSector-")
			z <- strat.path(z, "daily")
		}
	}
#
# ADDITIONAL TAGS
	if (is.null(z)) {
		if (y == "E") y <- NULL else y <- paste0("-FundType", y)
		if (n == "Aggregate") n <- NULL else n <- paste0("-", n)
		if (w == "Result") w <- NULL else w <- paste0("-", w)
		if (h == "CB") h <- NULL else h <- paste0("-", h)
	}
#
# CREATE RESULT
	if (is.null(z)) {
		z <- paste0(fcn.dir(), "\\New Model Concept\\", x, "\\FloMo\\csv")
		z <- paste0(z, "\\oneDayFloMo", h, y, n, w, ".csv")
	}
#
# RETURN RESULT
	z
}
stratrets.returns <- function(x) {
# -----------------------------------------------------------------
# Name		: stratrets.returns
# Author	: VKS
# Date		: 10/25/22,10/26,10/27
# Args		: x = return type (e.g. Ctry/FX/SectorUK/Multi)
# Output	: data frame of daily returns
# -----------------------------------------------------------------
	if (x == "Ctry") {
		z <- paste0(fcn.dir(), "\\New Model Concept\\Ctry\\FloMo\\csv\\OfclMsciTotRetIdx.csv")
		z <- mat.read(z)
	} else if (x == "China") {
		z <- paste0(fcn.dir(), "\\New Model Concept\\ChinaShareClass\\csv\\OfclMsciTotRetIdx.csv")
		z <- mat.read(z)
		z <- z[, c("CHINA A", "CHINA B", "CHINA H", "CHINA RED CHIP", "CHINA P CHIP", "OVERSEAS CHINA (US)", "OVERSEAS CHINA (SG)")]
		dimnames(z)[[2]] <- c("A Share", "B Share", "H Share", "Red Chip", "P Chip", "ADR", "S Chip")
	} else if (x == "Commodity") {
		z <- paste0(fcn.dir(), "\\New Model Concept\\Commodity\\FloMo\\csv\\S&P GSCI ER.csv")
		z <- mat.read(z)[, c("SPGSENP", "SPGSGCP", "SPGSSIP", "SPGSAGP")]
		dimnames(z)[[2]] <- c("Energy", "Gold", "Silver", "AG")
	} else if (x == "FX") {
		z <- paste0(fcn.dir(), "\\New Model Concept\\FX\\FloMo\\csv\\ExchRates.csv")
		z <- 1/mat.read(z)
		z$CNY <- ifelse(is.na(z$CNH), z$CNY, z$CNH)
		z$USD <- rep(1, dim(z)[1])
		z <- z/z[, "XDR"]
	} else if (x == "Multi") {
		x <- c("Ctry", "FI")
		x <- paste0(fcn.dir(), "\\New Model Concept\\", x, "\\FloMo\\csv\\")
		x <- paste0(x, c("OfclMsciTotRetIdx.csv", "pseudoReturns.csv"))
		z <- mat.read(x[1])[, c("JP", "GB", "US")]
		dimnames(z)[[2]] <- c("Japan", "UK", "USA")
		#
		x <- ret.to.idx(map.rname(mat.read(x[2]), dimnames(z)[[1]]))
		z <- data.frame(z, x, stringsAsFactors = F)
		#
		x <- paste(dir.parameters("csv"), "IndexReturns-Daily.csv", sep = "\\")
		x <- map.rname(mat.read(x), dimnames(z)[[1]])
		z <- data.frame(z, x[, c("LatAm", "EurXGB", "PacXJP", "AsiaXJP")], stringsAsFactors = F)
		#
		x <- max(sapply(z, function(x) find.data(!is.na(x), T)))
		x <- x:min(sapply(z, function(x) find.data(!is.na(x), F)))
		z <- z[x, ]
	} else {
		x <- txt.right(x, nchar(x) - nchar("Sector"))
		y <- mat.read(parameters("classif-GSec"), "\t")
		if (any(dimnames(y)[[2]] == x)) {
			z <- paste0(fcn.dir(), "\\New Model Concept\\Sector\\FloMo\\csv\\OfclMsciTotRetIdx.csv")
			z <- mat.subset(mat.read(z), y[, x])
			dimnames(z)[[2]] <- dimnames(y)[[1]]
		} else {
			z <- paste0(fcn.dir(), "\\New Model Concept\\Sector", x, "\\FloMo\\csv\\WeeklyRets.csv")
			z <- mat.read(z)
		}
	}
#
# RETURN RESULT
	z
}
stratrets.subset <- function(x, y) {
# -----------------------------------------------------------------
# Name		: stratrets.subset
# Author	: VKS
# Date		: 10/26/22
# Args		: x = indicators
#		: y = index to subset to
# Output	: subsets to columns used in the back-test
# -----------------------------------------------------------------
#
# DETERMINE SUBSET
	if (txt.right(y, 2) == "FX") {
		y <- txt.left(y, nchar(y) - nchar("FX"))
		z <- stratrets.subset.Ctry(x, y)
		z <- unique(Ctry.info(z, "Curr"))
		if (is.element(y, "EM")) z <- setdiff(z, c("USD", "EUR"))
	} else {
		z <- stratrets.subset.Ctry(x, y)
	}
#
# PERFORM SUBSET
	z <- x[, is.element(dimnames(x)[[2]], z)]
#
# RETURN RESULT
	z
}
stratrets.subset.Ctry <- function(x, y) {
# -----------------------------------------------------------------
# Name		: stratrets.subset.Ctry
# Author	: VKS
# Date		: 10/26/22
# Args		: x = indicators
#		: y = index to subset to
# Output	: determine which countries to subset to
# -----------------------------------------------------------------
	z <- NULL
#
# GROUPINGS WITH INDEX CHANGES
	w <- c("ACWI", "EAFE", "EM", "Frontier")
	if (is.element(y, w)) {
		z <- dimnames(x)[[1]][c(1, dim(x)[1])]
		z <- Ctry.msci.members.rng(y, z[1], z[2])
	} else {
		w <- dimnames(mat.read(parameters("MsciCtry2016"), ","))[[2]]
	}
#
# STATIC GROUPINGS
	if (length(z) == 0 & is.element(y, w)) z <- Ctry.msci.members(y, "")
#
# RETURN RESULT
	z
}
stunden <- function(x = 5, y = 8) {
# -----------------------------------------------------------------
# Name		: stunden
# Author	: VKS
# Date		: 1/13/20,2/5,4/24,1/15/21,3/5
# Args		: x = integer
#		: y = integer
# Output	: vector of <x> random numbers within +/-1 of, and averaging to, <y>
# -----------------------------------------------------------------
#
# RE-SET RANDOM SEED
	set.seed(seed = NULL)
#
# CREATE RESULT
	z <- y - 1
	while (mean(z) != y) {
		z <- NULL
		while (length(z) < x) z <- c(z, sample(seq(-1, 1) + y, 1))
	}
#
# RETURN RESULT
	z
}
summ.multi <- function(fcn, x, y) {
# -----------------------------------------------------------------
# Name		: summ.multi
# Author	: VKS
# Date		: 1/9/19,1/25
# Args		: fcn = a function that summarizes the data
#		: x = a df of bin returns indexed by time
#		: y = forward return horizon size
# Output	: summarizes the multi-period back test
# -----------------------------------------------------------------
	if (y == 1) {
		z <- fcn(x)
	} else {
		z <- split(x, 1:dim(x)[1] %% y)
		z <- sapply(z, fcn, simplify = "array")
		z <- apply(z, 2:length(dim(z)) - 1, mean)
	}
	z
}
today <- function() {
# -----------------------------------------------------------------
# Name		: today
# Author	: VKS
# Date		: 11/22/16,10/18/20
# Args		: none
# Output	: returns current flow date
# -----------------------------------------------------------------
	z <- day.ex.date(Sys.Date())
	while (!flowdate.exists(z)) z <- day.lag(z, 1)
	z
}
tstat <- function(x, y) {
# -----------------------------------------------------------------
# Name		: tstat
# Author	: VKS
# Date		: 6/3/20
# Args		: x = a matrix/data-frame
#		: y = a vector corresponding to the columns of <x>
# Output	: t-statistic associated with the regression of each row of <x> on <y>
# -----------------------------------------------------------------
#
# MEAN ADJUST
	x <- x - rowMeans(x)
	y <- as.matrix(y - mean(y))
#
# ESTIMATE
	z <- (x %*% y)/crossprod(y)[1, 1]
#
# t-STATISTIC
	n <- x - tcrossprod(z, y)
	n <- rowSums(n^2)/(dim(n)[2] - 2)
	n <- sqrt(n/crossprod(y)[1, 1])
	z <- z/n
#
# RETURN RESULT
	z
}
txt.anagram <- function(x, y, n = 0) {
# -----------------------------------------------------------------
# Name		: txt.anagram
# Author	: VKS
# Date		: 2/1/18,6/21,11/27,4/25/19
# Args		: x = a SINGLE string
#		: y = a file of potentially-usable capitalized words
#		: n = vector of minimum number of characters for first few words
# Output	: all possible anagrams
# -----------------------------------------------------------------
#
# CONVERT <x> TO USABLE FORM
	x <- toupper(x)
	x <- txt.to.char(x)
	x <- x[is.element(x, LETTERS)]
	x <- paste(x, collapse = "")
#
# ALL CAPITALIZED POTENTIALLY-USABLE WORDS
	if (missing(y)) y <- txt.words() else y <- txt.words(y)
	y <- y[order(y, decreasing = T)]
	y <- y[order(nchar(y))]
#
# CREATE RESULT
	z <- txt.anagram.underlying(x, y, n)
#
# RETURN RESULT
	z
}
txt.anagram.underlying <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: txt.anagram.underlying
# Author	: VKS
# Date		: 2/1/18
# Args		: x = a SINGLE string
#		: y = potentially-usable capitalized words
#		: n = vector of minimum number of characters for first few words
# Output	: all possible anagrams
# -----------------------------------------------------------------
#
# SUBSET TO ACTUALLY-USABLE WORDS
	y <- y[txt.excise(y, txt.to.char(x)) == ""]
#
# CREATE RESULT
	z <- NULL
	m <- length(y)
	proceed <- m > 0
	if (proceed) proceed <- nchar(y[m]) >= n[1]
	while (proceed) {
		w <- txt.excise(x, txt.to.char(y[m]))
		if (nchar(w) == 0) {
			z <- c(z, y[m])
		} else if (m > 1) {
			w <- txt.anagram.underlying(w, y[2:m - 1], c(n, 0)[-1])
			if (!is.null(w)) z <- c(z, paste(y[m], w))
		}
		m <- m - 1
		proceed <- m > 0
		if (proceed) proceed <- nchar(y[m]) >= n[1]
	}
#
# RETURN RESULT
	z
}
txt.core <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.core
# Author	: VKS
# Date		: 3/21/18,4/25/19
# Args		: x = a vector
# Output	: renders with upper-case letters, spaces and numbers only
# -----------------------------------------------------------------
#
# UPPER CASE
	x <- toupper(x)
#
# REPLACE STRANGE CHARACTERS WITH SPACES
	m <- nchar(x)
	n <- max(m)
	while (n > 0) {
		w <- m >= n
		w[w] <- !is.element(substring(x[w], n, n), c(" ", LETTERS, 0:9))
		#
		h <- w & m == n
		if (any(h)) {
			x[h] <- txt.left(x[h], n - 1)
			m[h] <- m[h] - 1
		}
		#
		h <- w & m > n
		if (any(h)) x[h] <- paste(txt.left(x[h], n - 1), substring(x[h], n + 1, m[h]))
		#
		n <- n - 1
	}
#
# EXTERNAL AND INTERNAL TRIMS
	x <- txt.trim(x)
	z <- txt.itrim(x)
#
# RETURN RESULTS
	z
}
txt.count <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.count
# Author	: VKS
# Date		: 1/5/17
# Args		: x = a vector of strings
#		: y = a substring
# Output	: counts the number of occurences of <y> in each element of <x>
# -----------------------------------------------------------------
	z <- txt.replace(x, y, "")
	z <- nchar(z)
	z <- nchar(x) - z
	z <- z/nchar(y)
	z
}
txt.ex.file <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.ex.file
# Author	: VKS
# Date		: 11/21/16,2/1/18,12/25/20
# Args		: x = path to a text file
# Output	: reads in the file as a single string
# -----------------------------------------------------------------
	paste(readLines(x), collapse = "\n")
}
txt.ex.int <- function(x, y = F) {
# -----------------------------------------------------------------
# Name		: txt.ex.int
# Author	: VKS
# Date		: 4/9/20,4/24
# Args		: x = a vector of integers
#		: y = T/F depending on whether ordinal numbers are wanted
# Output	: a string vector describing <x> in words
# -----------------------------------------------------------------
	if (y) txt.ex.int.ordinal.wrapper(x) else txt.ex.int.cardinal.wrapper(x)
}
txt.ex.int.cardinal <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.ex.int.cardinal
# Author	: VKS
# Date		: 4/9/20,11/10/21
# Args		: x = a vector of integers
# Output	: a string vector describing <x> in words (cardinal numbers)
# -----------------------------------------------------------------
	y <- vec.named(c("zero", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"), c(0, 10:19))
	n <- vec.named(c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"), 1:9)
	w <- vec.named(c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"), 2:9)
	z <- txt.ex.int.underlying(x, y, n, w, w)
#
# RETURN RESULT
	z
}
txt.ex.int.cardinal.wrapper <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.ex.int.cardinal.wrapper
# Author	: VKS
# Date		: 4/24/20
# Args		: x = a vector of integers
# Output	: a string vector describing <x> in words (cardinal numbers)
# -----------------------------------------------------------------
	z <- ifelse(x %/% 10000 > 0, x, NA)
	z <- ifelse(is.na(z) & x %/% 100 == 0, txt.ex.int.cardinal(x), z)
	z <- ifelse(is.na(z) & x %% 1000 == 0, paste(txt.ex.int.cardinal(x %/% 1000), "thousand"), z)
	z <- ifelse(is.na(z) & x %% 100 == 0, paste(txt.ex.int.cardinal(x %/% 100), "hundred"), z)
	z <- ifelse(is.na(z) & (x %/% 100) %% 10 == 0, paste(txt.ex.int.cardinal(x %/% 1000), "thousand and", txt.ex.int.cardinal(x %% 100)), z)
	z <- ifelse(is.na(z), paste(txt.ex.int.cardinal(x %/% 100), "hundred and", txt.ex.int.cardinal(x %% 100)), z)
	z
}
txt.ex.int.ordinal <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.ex.int.ordinal
# Author	: VKS
# Date		: 4/9/20,11/10/21
# Args		: x = a vector of integers
# Output	: a string vector describing <x> in words (cardinal numbers)
# -----------------------------------------------------------------
	y <- vec.named(c("tenth", "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth"), 10:19)
	n <- vec.named(c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth"), 1:9)
	w <- vec.named(c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"), 2:9)
	h <- vec.named(c("twentieth", "thirtieth", "fortieth", "fiftieth", "sixtieth", "seventieth", "eightieth", "ninetieth"), 2:9)
	z <- txt.ex.int.underlying(x, y, n, w, h)
#
# RETURN RESULT
	z
}
txt.ex.int.ordinal.wrapper <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.ex.int.ordinal.wrapper
# Author	: VKS
# Date		: 4/24/20
# Args		: x = a vector of integers
# Output	: a string vector describing <x> in words (cardinal numbers)
# -----------------------------------------------------------------
	z <- ifelse(x %/% 10000 > 0, x, NA)
	z <- ifelse(is.na(z) & x %/% 100 == 0, txt.ex.int.ordinal(x), z)
	z <- ifelse(is.na(z) & x %% 1000 == 0, paste(txt.ex.int.cardinal(x %/% 1000), "thousandth"), z)
	z <- ifelse(is.na(z) & x %% 100 == 0, paste(txt.ex.int.cardinal(x %/% 100), "hundredth"), z)
	z <- ifelse(is.na(z) & (x %/% 100) %% 10 == 0, paste(txt.ex.int.cardinal(x %/% 1000), "thousand and", txt.ex.int.ordinal(x %% 100)), z)
	z <- ifelse(is.na(z), paste(txt.ex.int.cardinal(x %/% 100), "hundred and", txt.ex.int.ordinal(x %% 100)), z)
	z
}
txt.ex.int.underlying <- function(x, y, n, w, h) {
# -----------------------------------------------------------------
# Name		: txt.ex.int.underlying
# Author	: VKS
# Date		: 11/10/21
# Args		: x = a vector of integers
#		: y = odds & ends
#		: n = units
#		: w = tens
#		: h = tens ordinal
# Output	: string vector describing <x> in words
# -----------------------------------------------------------------
#
# PRELIMINARIES
	z <- ifelse(x %/% 100 > 0, x, NA)
#
# ODDS AND ENDS
	z <- ifelse(is.element(x, names(y)), y[as.character(x)], z)
	y <- is.na(z)
#
# UNITS PLACE
	z <- ifelse(is.element(x %% 10, names(n)) & y, map.rname(n, x %% 10), z)
	y <- y & !is.element(x, 1:9)
#
# TENS PLACE
	z <- ifelse(y & !is.na(z), paste(map.rname(w, (x %/% 10) %% 10), z, sep = "-"), z)
	z <- ifelse(y & is.na(z), map.rname(h, (x %/% 10) %% 10), z)
#
# RETURN RESULT
	z
}
txt.excise <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.excise
# Author	: VKS
# Date		: 2/1/18
# Args		: x = a vector
#		: y = a vector
# Output	: cuts out elements of <y> from <x> wherever found
# -----------------------------------------------------------------
	z <- x
	for (j in y) {
		m <- nchar(j)
		j <- as.numeric(regexpr(j, z, fixed = T))
		n <- nchar(z)
		z <- ifelse(j == 1, substring(z, m + 1, n), z)
		z <- ifelse(j == n - m + 1, substring(z, 1, j - 1), z)
		z <- ifelse(j > 1 & j < n - m + 1, paste0(substring(z, 1, j - 1), substring(z, j + m, n)), z)
	}
	z
}
txt.expand <- function(x, y, n = "-", w = F) {
# -----------------------------------------------------------------
# Name		: txt.expand
# Author	: VKS
# Date		: 9/29/16,1/30/19,6/13
# Args		: x = a vector of strings
#		: y = a vector of strings
#		: n = paste separator
#		: w = T/F variable controlling paste order
# Output	: Returns all combinations OF <x> and <y> pasted together
# -----------------------------------------------------------------
#
# EXPAND
	z <- list(x = x, y = y)
	if (w) z <- expand.grid(z, stringsAsFactors = F) else z <- rev(expand.grid(rev(z), stringsAsFactors = F))
#
# CREATE RESULT
	z[["sep"]] <- n
	z <- do.call(paste, z)
#
# RETURN RESULT
	z
}
txt.gunning <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: txt.gunning
# Author	: VKS
# Date		: 6/21/18,6/27,6/28,6/29,11/27,4/25/19
# Args		: x = a string representing a text passage
#		: y = a file of potentially-usable capitalized words
#		: n = a file of potentially-usable capitalized words considered "simple"
# Output	: the Gunning fog index measuring the number of years of
#		:  schooling beyond kindergarten needed to comprehend <x>
# -----------------------------------------------------------------
#
# CAPITALIZE
	x <- toupper(x)
#
# TREAT HYPHENS AS SPACES
	x <- txt.replace(x, "-", " ")
#
# TREAT SENTENCE ENDERS AS PERIODS
	x <- txt.replace(x, "?", ".")
	x <- txt.replace(x, "!", ".")
#
# EXCISE STRANGE CHARACTERS
	x <- txt.to.char(x)
	x <- x[is.element(x, c(LETTERS, " ", "."))]
	x <- paste(x, collapse = "")
#
# SEPARATE OUT PERIODS
	x <- txt.replace(x, ".", " . ")
#
# REMOVE EXTRANEOUS SPACES
	x <- txt.trim(x)
	while (x != txt.replace(x, txt.space(2), txt.space(1))) x <- txt.replace(x, txt.space(2), txt.space(1))
#
# REMOVE TERMINAL PERIOD
	if (txt.right(x, 1) == ".") x <- txt.left(x, nchar(x) - 1)
	x <- txt.trim(x)
#
# ALL CAPITALIZED POTENTIALLY-USABLE WORDS
	if (missing(y)) y <- txt.words() else y <- txt.words(y)
#
# SUBSET TO PERIODS AND STANDARD WORDS
	x <- txt.parse(x, " ")
	x <- x[is.element(x, c(y, "."))]
#
# WORDS PER SENTENCE
	z <- 1 + sum(x == ".")
	x <- x[x != "."]
	h <- length(x)
	if (h < 100) cat("Passage needs to have at least a 100 words.\nNeed at least", 100 - h, "more words ...\n")
	z <- h/nonneg(z)
#
# ALL CAPITALIZED POTENTIALLY-USABLE SIMPLE WORDS
	if (missing(n)) {
		n <- union(txt.words(1), txt.words(2))
	} else {
		n <- txt.words(n)
	}
#
# PERCENTAGE OF COMPLEX WORDS
	if (any(!is.element(x, n))) {
		x <- x[!is.element(x, n)]
		n <- length(x)/nonneg(h)
		x <- x[!duplicated(x)]
		x <- x[order(nchar(x))]
	} else {
		n <- 0
		x <- NULL
	}
#
# CREATE RESULT
	z <- list(result = 0.4 * (z +  100 * n), complex = x)
#
# RETURN RESULT
	z
}
txt.has <- function(x, y, n = F) {
# -----------------------------------------------------------------
# Name		: txt.has
# Author	: VKS
# Date		: 12/1/16,12/7/17,12/18,1/4/18,1/31
# Args		: x = a vector of strings
#		: y = a single string
#		: n = T/F depending on whether a logical vector is desired
# Output	: the elements of <x> that contain <y> if <n> is F or a
#		:	logical vector otherwise
# -----------------------------------------------------------------
	z <- grepl(y, x, fixed = T)
	if (!n) z <- x[z]
	z
}
txt.hdr <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.hdr
# Author	: VKS
# Date		: 9/30/16
# Args		: x = any string
# Output	: nice-looking header
# -----------------------------------------------------------------
	n <- nchar(x)
	if (n %% 2 == 1) {
		x <- paste0(x, " ")
		n <- n + 1
	}
	n <- 100 - n
	n <- n/2
	z <- paste0(txt.space(n, "*"), x, txt.space(n, "*"))
	z
}
txt.itrim <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.itrim
# Author	: VKS
# Date		: 1/19/17
# Args		: x = a vector of strings
# Output	: gets rid of multiple consecutive spaces
# -----------------------------------------------------------------
	z <- txt.replace(x, txt.space(2), txt.space(1))
	w <- z != x
	while (any(w)) {
		x[w] <- z[w]
		z[w] <- txt.replace(x[w], txt.space(2), txt.space(1))
		w[w] <- z[w] != x[w]
	}
	z
}
txt.left <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.left
# Author	: VKS
# Date		: 10/25/16
# Args		: x = a vector of string
#		: y = a positive integer
# Output	: Returns the left <y> characters
# -----------------------------------------------------------------
	substring(x, 1, y)
}
txt.levenshtein <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.levenshtein
# Author	: VKS
# Date		: 10/26/18,8/11/20
# Args		: x = a string
#		: y = a string
# Output	: Levenshtein distance between <x> and <y>. Similar to dtw
# Notes		: Similar algorithm to dtw
# -----------------------------------------------------------------
	n <- nchar(x)
	m <- nchar(y)
	if (min(m, n) == 0) {
		z <- max(m, n)
	} else {
	#
	# SPLIT INTO CHARACTERS
		x <- c("", txt.to.char(x))
		y <- c("", txt.to.char(y))
	#
	# INSTANTIATE RESULT
		z <- matrix(NA, n + 1, m + 1, F, list(x, y))
		z[1, ] <- 0:m
		z[, 1] <- 0:n
	#
	# POPULATE RESULT
		for (i in 1:m + 1) {
			for (j in 1:n + 1) {
				z[j, i] <- min(z[j - 1, i], z[j, i - 1]) + 1
				z[j, i] <- min(z[j, i], z[j - 1, i - 1] + as.numeric(x[j] != y[i]))
			}
		}
		z <- z[n + 1, m + 1]
	}
#
# RETURN RESULT
	z
}
txt.na <- function() {
# -----------------------------------------------------------------
# Name		: txt.na
# Author	: VKS
# Date		: 11/23/16,1/11/17,3/2,12/1/20,6/21/21
# Args		: none
# Output	: Returns a list of strings considered NA
# -----------------------------------------------------------------
	c("#N/A", "NA", "N/A", "NULL", "<NA>", "--", "#N/A N/A", "#VALUE!")
}
txt.name.format <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.name.format
# Author	: VKS
# Date		: 9/19/17,12/20,1/31/18,12/20,1/24/19
# Args		: x = a string vector
# Output	: capitalizes first letter of each word, rendering remaining letters in lower case
# -----------------------------------------------------------------
	if (any(txt.has(x, " ", T))) {
		z <- txt.parse(x, " ")
		z <- fcn.mat.vec(txt.name.format, z,, T)
		z <- do.call(paste, mat.ex.matrix(z))
		z <- txt.trim(z)
	} else {
		x <- tolower(x)
		z <- txt.left(x, 1)
		x <- txt.right(x, nchar(x) - 1)
		z <- paste0(toupper(z), x)
	}
	z
}
txt.parse <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.parse
# Author	: VKS
# Date		: 11/11/16,1/18/17,12/15,12/25/20
# Args		: x = a vector of strings
#		: y = a string that serves as a delimiter
# Output	: breaks up string <x> by <y>
# -----------------------------------------------------------------
	if (length(x) == 1) {
		z <- strsplit(x, y, fixed = T)[[1]]
	} else {
		x <- strsplit(x, y, fixed = T)
		n <- max(sapply(x, length))
		y <- rep("", n)
		z <- sapply(x, function(x) c(x, y)[1:n], simplify = "array")
		if (is.null(dim(z))) z <- as.matrix(z) else z <- t(z)
	}
#
# RETURN RESULT
	z
}
txt.prepend <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: txt.prepend
# Author	: VKS
# Date		: 12/4/17
# Args		: x = a vector of strings
#		: y = number of characters to add
#		: n = the characters to add at the beginning
# Output	: bulks up each string to have at least <y> characters by adding
#		:	<n> to the beginning of each string
# -----------------------------------------------------------------
	z <- x
	w <- nchar(z) < y
	while (any(w)) {
		z[w] <- paste0(n, z[w])
		w <- nchar(z) < y
	}
	z
}
txt.regr <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: txt.regr
# Author	: VKS
# Date		: 9/23/16,12/27,1/3/18
# Args		: x = a vector of column names
#		: y = T/F depending on whether regression has an intercept
# Output	: returns the string you need to regress the first column on the others
# -----------------------------------------------------------------
	z <- x[1]
	x <- x[-1]
	if (!y) x <- c("-1", x)
	x <- paste(x, collapse = " + ")
	z <- paste(z, x, sep = " ~ ")
	z
}
txt.replace <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: txt.replace
# Author	: VKS
# Date		: 10/25/16,11/11
# Args		: x = a vector of strings
#		: y = a string to be swapped out
#		: n = a string to replace <txt.out> with
# Output	: replaces all instances of <txt.out> by <txt.by>
# -----------------------------------------------------------------
	gsub(y, n, x, fixed = T)
}
txt.reverse <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.reverse
# Author	: VKS
# Date		: 1/19/17,10/25,12/18,1/30/19
# Args		: x = vector of strings
# Output	: reverses the constitutent characters of <x>
# -----------------------------------------------------------------
	fcn <- function(x) paste(rev(txt.to.char(x)), collapse = "")
	z <- fcn.vec.num(fcn, x)
	z
}
txt.right <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.right
# Author	: VKS
# Date		: 10/25/16
# Args		: x = a vector of string
#		: y = a positive integer
# Output	: Returns the right <y> characters
# -----------------------------------------------------------------
	substring(x, nchar(x) - y + 1, nchar(x))
}
txt.space <- function(x, y = " ") {
# -----------------------------------------------------------------
# Name		: txt.space
# Author	: VKS
# Date		: 9/30/16
# Args		: x = any integer
#		: y = a single character
# Output	: returns <x> iterations of <y> pasted together
# -----------------------------------------------------------------
	z <- ""
	while (x > 0) {
		z <- paste0(z, y)
		x <- x - 1
	}
	z
}
txt.to.char <- function(x) {
# -----------------------------------------------------------------
# Name		: txt.to.char
# Author	: VKS
# Date		: 1/19/17
# Args		: x = a SINGLE string
# Output	: a vector of the constitutent characters of <x>
# -----------------------------------------------------------------
	strsplit(x, "")[[1]]
}
txt.trim <- function(x, y = " ") {
# -----------------------------------------------------------------
# Name		: txt.trim
# Author	: VKS
# Date		: 10/25/16,12/22/17,1/16/18
# Args		: x = a vector of string
#		: y = a vector of verboten strings, each of the same length
# Output	: trims leading/trailing spaces
# -----------------------------------------------------------------
	txt.trim.right(txt.trim.left(x, y), y)
}
txt.trim.end <- function(fcn, x, y, n) {
# -----------------------------------------------------------------
# Name		: txt.trim.end
# Author	: VKS
# Date		: 12/22/17,1/16/18,1/17
# Args		: fcn = a function that returns characters from the bad end
#		: x = a vector of string
#		: y = a vector of verboten strings, each of the same length
#		: n = a functon that returns characters from the opposite end
# Output	: trims off leading or trailing elements of <y>
# -----------------------------------------------------------------
	h <- nchar(y[1])
	z <- x
	w <- nchar(z) > h - 1 & is.element(fcn(z, h), y)
	while (any(w)) {
		z[w] <- n(z[w], nchar(z[w]) - h)
		w <- nchar(z) > h - 1 & is.element(fcn(z, h), y)
	}
	z
}
txt.trim.left <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.trim.left
# Author	: VKS
# Date		: 12/22/17,1/16/18,1/17
# Args		: x = a vector of string
#		: y = a vector of verboten strings, each of the same length
# Output	: trims off leading elements of <y>
# -----------------------------------------------------------------
	txt.trim.end(txt.left, x, y, txt.right)
}
txt.trim.right <- function(x, y) {
# -----------------------------------------------------------------
# Name		: txt.trim.right
# Author	: VKS
# Date		: 12/22/17,1/16/18
# Args		: x = a vector of string
#		: y = a vector of verboten strings, each of the same length
# Output	: trims off trailing elements of <y>
# -----------------------------------------------------------------
	txt.trim.end(txt.right, x, y, txt.left)
}
txt.words <- function(x = "All") {
# -----------------------------------------------------------------
# Name		: txt.words
# Author	: VKS
# Date		: 6/21/18,11/27,12/25/20
# Args		: x = missing or an integer
# Output	: a vector of capitalized words
# -----------------------------------------------------------------
#
# DETERMINE PATH
	if (any(x == c("All", 1:2))) {
		if (x == "All") {
			z <- "EnglishWords.txt"
		} else if (x == 1) {
			z <- "EnglishWords-1syllable.txt"
		} else if (x == 2) {
			z <- "EnglishWords-2syllables.txt"
		}
		z <- paste(dir.parameters("data"), z, sep = "\\")
	} else {
		z <- x
	}
#
# READ IN THE FILE
	z <- readLines(z)
#
# RETURN RESULT
	z
}
urn.exact <- function(x, y) {
# -----------------------------------------------------------------
# Name		: urn.exact
# Author	: VKS
# Date		: 11/2/18
# Args		: x = a vector of integers
#		: y = an isomekic vector of integers that is pointwise greater than or equal to <x>
# Output	: probability of drawing precisely <x> balls from an urn containing <y> balls
# Notes		: the elements of <x> and <y>represent balls of different colours
# -----------------------------------------------------------------
#
# NUMBER OF WAYS TO DRAW BALLS
	z <- 1
	for (i in seq_along(x)) z <- z * factorial(y[i])/(factorial(x[i]) * factorial(y[i] - x[i]))
#
# COMPUTE PROBABILITY
	z <- (z/factorial(sum(y))) * factorial(sum(x)) * factorial(sum(y - x))
#
# RETURN RESULT
	z
}
utf8.to.quoted.printable <- function(x) {
# -----------------------------------------------------------------
# Name		: utf8.to.quoted.printable
# Author	: VKS
# Date		: 4/7/21
# Args		: x = a single character
# Output	: quoted-printable representation of <x>
# -----------------------------------------------------------------
#
# PRELIMINARIES
	y <- c(0:9, char.seq("A", "F"))
	h <- c(8, 9, "A", "B")
	r <- c("E", "F", "G", "H")
#
# CONVERT TO INTEGER
	x <- utf8ToInt(x)
#
# BASE 64
	x <- base.ex.int(x, 64)
	x <- split(x, 1:3)
#
# BASE 16
	x <- lapply(x, function(x) base.ex.int(x, 16))
	x <- lapply(x, function(x) c(rep(0, 2 - length(x)), x))
	x <- lapply(x, function(x) x + 1)
#
# CREATE RESULT
	x <- lapply(x, function(x) c(x[1], y[x[2]]))
	x[[1]][1] <- r[as.numeric(x[[1]][1])]
	x[[2]][1] <- h[as.numeric(x[[2]][1])]
	x[[3]][1] <- h[as.numeric(x[[3]][1])]
	x <- sapply(x, function(x) paste(x, collapse = ""))
	z <- paste(x, collapse = "=")
#
# RETURN RESULT
	z
}
variance.ratio.test <- function(x, y) {
# -----------------------------------------------------------------
# Name		: variance.ratio.test
# Author	: VKS
# Date		: 10/9/17
# Args		: x = vector
#		: y = an integer greater than 1
# Output	: tests whether <x> follows a random walk (i.e. <x> independent of prior values)
# -----------------------------------------------------------------
#
# CHECK <y>
	y <- as.numeric(y)
	if (is.na(y) | y == 1) stop("Bad value of y ...")
#
# MEAN-ADJUST <x>
	x <- x - mean(x)
#
# TOTAL NUMBER OF OBESERVATIONS
	T <- length(x)
#
# SAMPLE SINGLE-PERIOD VARIANCE
	sd.1 <- sum(x^2)/(T - 1) # SAME AS sd(x)
#
# SAMPLE <y>-PERIOD VARIANCE
	z <- x[y:T]
	for (i in 2:y - 1) z <- z + x[y:T - i]
	sd.y <- sum(z^2)/(T - y - 1)
#
# FINAL RESULT
	z <- sd.y/(y * sd.1 * (1 - y/T))
#
# RETURN RESULT
	z
}
vec.cat <- function(x) {
# -----------------------------------------------------------------
# Name		: vec.cat
# Author	: VKS
# Date		: 1/10/18
# Args		: x = vector
# Output	: displays on screen
# -----------------------------------------------------------------
	cat(paste(x, collapse = "\n"), "\n")
}
vec.count <- function(x) {
# -----------------------------------------------------------------
# Name		: vec.count
# Author	: VKS
# Date		: 9/12/16,9/16,9/19,10/31
# Args		: x = a numeric vector
# Output	: Counts unique instances of <x>
# -----------------------------------------------------------------
	pivot.1d(sum, x, rep(1, length(x)))
}
vec.cum <- function(x) {
# -----------------------------------------------------------------
# Name		: vec.cum
# Author	: VKS
# Date		: 10/22/22
# Args		: x = a numeric vector
# Output	: cumulative sum
# -----------------------------------------------------------------
	cumsum(c(0, x))
}
vec.diff <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.diff
# Author	: VKS
# Date		: 10/22/22
# Args		: x = a numeric vector
#		: y = an integer
# Output	: difference between <x> and itself lagged <y>
# -----------------------------------------------------------------
	c(rep(NA, y), diff(x, y))
}
vec.ex.filters <- function(x) {
# -----------------------------------------------------------------
# Name		: vec.ex.filters
# Author	: VKS
# Date		: 10/2/20
# Args		: x = either "sf" or "macro"
# Output	: SQL query where clauses associated with filters
# -----------------------------------------------------------------
	z <- mat.read(parameters("classif-filters"), "\t", NULL)
	z <- z[is.element(z$type, x), ]
	z <- as.matrix(mat.index(z, "filter"))[, "SQL"]
	z
}
vec.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.lag
# Author	: VKS
# Date		: 12/9/20,10/22/22
# Args		: x = a vector indexed by time running FORWARDS
#		: y = number of periods over which to lag
# Output	: <x> lagged <y> periods (simple positional lag)
# -----------------------------------------------------------------
	x[nonneg(seq_along(x) - y)]
}
vec.max <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.max
# Author	: VKS
# Date		: 9/7/16,12/15/17,1/24/19,10/21/22
# Args		: x = a vector/matrix/dataframe
#		: y = a number/vector or matrix/dataframe with the same dimensions as <x>
# Output	: Returns the piecewise maximum of <x> and <y>
# -----------------------------------------------------------------
	fcn.mat.vec(function(x, y) ifelse(!is.na(x) & !is.na(y) & x < y, y, x), x, y, T)
}
vec.min <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.min
# Author	: VKS
# Date		: 9/7/16,12/15/17,1/24/19,10/21/22
# Args		: x = a vector/matrix/dataframe
#		: y = a number/vector or matrix/dataframe with the same dimensions as <x>
# Output	: Returns the piecewise minimum of <x> and <y>
# -----------------------------------------------------------------
	fcn.mat.vec(function(x, y) ifelse(!is.na(x) & !is.na(y) & x > y, y, x), x, y, T)
}
vec.named <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.named
# Author	: VKS
# Date		: 9/12/16
# Args		: x = a vector
#		: y = an isomekic vector
# Output	: Returns a vector with values <x> and names <y>
# -----------------------------------------------------------------
	if (missing(x)) z <- rep(NA, length(y)) else z <- x
	names(z) <- y
	z
}
vec.read <- function(x, y = ",") {
# -----------------------------------------------------------------
# Name		: vec.read
# Author	: VKS
# Date		: 11/3/17,1/23/18,2/1,4/25,11/3/20,12/25
# Args		: x = path to a vector
#		: y = separator (defaults to comma)
# Output	: reads into <x> a named vector
# -----------------------------------------------------------------
	as.matrix(mat.read(x, y, , F))[, 1]
}
vec.same <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.same
# Author	: VKS
# Date		: 1/24/18
# Args		: x = a vector
#		: y = an isomekic vector
# Output	: T/F depending on whether <x> and <y> are identical
# -----------------------------------------------------------------
	z <- all(is.na(x) == is.na(y))
	if (z) {
		w <- !is.na(x)
		if (any(w)) z <- all(abs(x[w] - y[w]) < 1e-6)
	}
	z
}
vec.swap <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: vec.swap
# Author	: VKS
# Date		: 1/29/18,10/21/22
# Args		: x = a vector
#		: y = an integer between 1 and length(<x>)
#		: n = an integer between 1 and length(<x>)
# Output	: swaps elements <y> and <n> of vector <x>
# -----------------------------------------------------------------
	x[ifelse(seq_along(x) == y, n, ifelse(seq_along(x) == n, y, seq_along(x)))]
}
vec.to.list <- function(x, y = F) {
# -----------------------------------------------------------------
# Name		: vec.to.list
# Author	: VKS
# Date		: 1/11/18,12/20,12/21/20
# Args		: x = string vector
#		: y = T/F depending on whether to use <x> as group vector
# Output	: list object
# -----------------------------------------------------------------
	if (y) split(x, x) else split(x, seq_along(x))
}
vec.unique <- function(x) {
# -----------------------------------------------------------------
# Name		: vec.unique
# Author	: VKS
# Date		: 9/19/16,10/31
# Args		: x = a numeric vector
# Output	: returns unique values of <x> in ascending order
# -----------------------------------------------------------------
	z <- unlist(x)
	z <- z[!is.na(z)]
	z <- z[!duplicated(z)]
	z <- z[order(z)]
	z
}
vec.write <- function(x, y) {
# -----------------------------------------------------------------
# Name		: vec.write
# Author	: VKS
# Date		: 2/7/20
# Args		: x = a named vector
#		: y = path to a vector
# Output	: writes <x> to <y>
# -----------------------------------------------------------------
	write.table(matrix(c(names(x), x), length(x), 2), y, row.names = F, col.names = F, quote = F, sep = ",")
}
versionR <- function() {
# -----------------------------------------------------------------
# Name		: versionR
# Author	: VKS
# Date		: 2/6/20
# Args		: none
# Output	: current version of R
# -----------------------------------------------------------------
	version[["version.string"]]
}
weekday.to.name <- function(x) {
# -----------------------------------------------------------------
# Name		: weekday.to.name
# Author	: VKS
# Date		: 9/14/16
# Args		: x = a vector of numbers between 0 and 6
# Output	: Converts to 0 = Sun, 1 = Mon, ..., 6 = Sat
# -----------------------------------------------------------------
	y <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
	y <- vec.named(y, 0:6)
	z <- map.rname(y, x)
	z <- as.character(z)
	z
}
yyyy.ex.period <- function(x, y) {
# -----------------------------------------------------------------
# Name		: yyyy.ex.period
# Author	: VKS
# Date		: 12/21/18
# Args		: x = vector of trade dates
#		: y = return window in days or months depending on whether <x> is YYYYMMDD or YYYYMM
# Output	: the year in which the return window ends
# -----------------------------------------------------------------
	txt.left(yyyymm.lag(x, -y), 4)
}
yyyy.ex.yy <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyy.ex.yy
# Author	: VKS
# Date		: 12/14/17
# Args		: x = a vector of non-negative integers
# Output	: returns a vector of YYYY
# -----------------------------------------------------------------
	x <- as.numeric(x)
	z <- ifelse(x < 100, ifelse(x < 50, 2000, 1900), 0) + x
	z
}
yyyy.periods.count <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyy.periods.count
# Author	: VKS
# Date		: 12/21/18
# Args		: x = a string vector
# Output	: the number of periods that typically fall in a year
# -----------------------------------------------------------------
	ifelse(all(nchar(x) == 6), ifelse(all(substring(x, 5, 5) == "Q"), 4, 12), 260)
}
yyyymm.diff <- function(x, y) {
# -----------------------------------------------------------------
# Name		: yyyymm.diff
# Author	: VKS
# Date		: 3/10/17
# Args		: x = a vector of YYYYMM
#		: y = an isomekic vector of YYYYMM
# Output	: returns <x - y> in terms of YYYYMM
# -----------------------------------------------------------------
	obj.diff(yyyymm.to.int, x, y)
}
yyyymm.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymm.ex.int
# Author	: VKS
# Date		: 9/15/16,12/4/17
# Args		: x = a vector of integers
# Output	: returns a vector of <yyyymm> months
# -----------------------------------------------------------------
	z <- (x - 1) %/% 12
	x <- x - 12 * z
	z <- 100 * z + x
	z <- as.character(z)
	z <- txt.prepend(z, 6, 0)
	z
}
yyyymm.ex.qtr <- function(x, y = 3) {
# -----------------------------------------------------------------
# Name		: yyyymm.ex.qtr
# Author	: VKS
# Date		: 6/26/17
# Args		: x = a vector of quarters
#		: y = month, in the quarter, to return (defaults to the third)
# Output	: returns a specific yyyymm within the quarter
# -----------------------------------------------------------------
	z <- qtr.to.int(x)
	z <- yyyymm.ex.int(z * 3)
	z <- yyyymm.lag(z, 3 - y)
	z
}
yyyymm.exists <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymm.exists
# Author	: VKS
# Date		: 4/23/20
# Args		: x = a vector of strings
# Output	: returns T if <x> is a month expressed in YYYYMM format
# -----------------------------------------------------------------
	z <- is.element(nchar(x), 6)
	j <- 1
	while (j < 7 & any(z)) {
		z[z] <- is.element(substring(x[z], j, j), 0:9)
		j <- j + 1
	}
	z
}
yyyymm.lag <- function(x, y = 1, n = T) {
# -----------------------------------------------------------------
# Name		: yyyymm.lag
# Author	: VKS
# Date		: 9/15/16,11/4,1/3/18,5/9,4/12/19
# Args		: x = a vector of <yyyymm> months or <yyyymmdd> days
#		: y = an integer or an isomekic vector of integers
#		: n = T/F depending on whether you wish to lag by yyyymmdd or flowdate
# Output	: lags <x> by <y> periods
# -----------------------------------------------------------------
	if (nchar(x[1]) == 8 & n) {
		z <- yyyymmdd.lag(x, y)
	} else if (nchar(x[1]) == 8 & !n) {
		z <- flowdate.lag(x, y)
	} else if (substring(x[1], 5, 5) == "Q") {
		z <- qtr.lag(x, y)
	} else {
		z <- obj.lag(x, y, yyyymm.to.int, yyyymm.ex.int)
	}
#
# RETURN RESULT
	z
}
yyyymm.seq <- function(x, y, n = 1) {
# -----------------------------------------------------------------
# Name		: yyyymm.seq
# Author	: VKS
# Date		: 9/15/16,11/7,1/3/18,1/23/19
# Args		: x = a YYYYMM or YYYYMMDD or YYYY
#		: y = an isotypic element
#		: n = quantum size in YYYYMM or YYYYMMDD or YYYY
# Output	: returns a sequence between (and including) x and y
# -----------------------------------------------------------------
	if (nchar(x) == 4) {
		z <- seq(x, y, n)
	} else if (nchar(x) == 8) {
		z <- yyyymmdd.seq(x, y, n)
	} else {
		z <- obj.seq(x, y, yyyymm.to.int, yyyymm.ex.int, n)
	}
	z
}
yyyymm.to.day <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymm.to.day
# Author	: VKS
# Date		: 11/8/16,1/3/18
# Args		: x = a vector of months in yyyymm format
# Output	: Returns the last day in the month whether weekend or not.
# -----------------------------------------------------------------
	day.lag(paste0(yyyymm.lag(x, -1), "01"), 1)
}
yyyymm.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymm.to.int
# Author	: VKS
# Date		: 9/15/16
# Args		: x = a vector of <yyyymm> months
# Output	: returns a vector of integers
# -----------------------------------------------------------------
	z <- as.numeric(substring(x, 1, 4))
	z <- 12 * z + as.numeric(substring(x, 5, 6))
	z
}
yyyymm.to.qtr <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymm.to.qtr
# Author	: VKS
# Date		: 6/26/17
# Args		: x = a vector of yyyymm
# Output	: returns associated quarters
# -----------------------------------------------------------------
	z <- yyyymm.to.int(x)
	z <- z + (3 - z) %% 3
	z <- qtr.ex.int(z/3)
	z
}
yyyymm.to.yyyy <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymm.to.yyyy
# Author	: VKS
# Date		: 10/26/16
# Args		: x = a vector of dates in yyyymm format
# Output	: Converts to yyyy years
# -----------------------------------------------------------------
	z <- as.numeric(x)
	z <- z %/% 100
	z
}
yyyymmdd.bulk <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.bulk
# Author	: VKS
# Date		: 12/14/17,12/19
# Args		: x = a matrix/df indexed by YYYYMMDD
# Output	: Eliminates YYYYMMDD gaps
# -----------------------------------------------------------------
#
# FULL YYYYMMDD SEQUENCE
	z <- dimnames(x)[[1]]
	z <- yyyymm.seq(z[1], z[dim(x)[1]])
#
# HIGHLIGHT GAPS
	w <- !is.element(z, dimnames(x)[[1]])
	if (any(w)) err.raise(z[w], F, "Following weekdays missing from data")
#
# PERFORM BULK UP
	z <- map.rname(x, z)
#
# RETURN RESULT
	z
}
yyyymmdd.diff <- function(x, y) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.diff
# Author	: VKS
# Date		: 3/14/18
# Args		: x = a vector of weekdays
#		: y = an isomekic vector of weekdays
# Output	: returns <x - y> in terms of weekdays
# -----------------------------------------------------------------
	obj.diff(yyyymmdd.to.int, x, y)
}
yyyymmdd.ex.AllocMo <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.ex.AllocMo
# Author	: VKS
# Date		: 11/15/16
# Args		: x = an object indexed by allocation months
# Output	: Returns an object indexed by flow dates
# -----------------------------------------------------------------
	y <- dimnames(x)[[1]]
	y <- y[order(y)]
	begPrd <- yyyymmdd.ex.yyyymm(y[1], F)[1]
	endPrd <- yyyymmdd.ex.yyyymm(yyyymm.lag(y[dim(x)[1]], -2), T)
	y <- yyyymmdd.seq(begPrd, endPrd)
	y <- vec.named(yyyymmdd.to.AllocMo(y), y)
	y <- y[is.element(y, dimnames(x)[[1]])]
	z <- map.rname(x, y)
	dimnames(z)[[1]] <- names(y)
	z
}
yyyymmdd.ex.day <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.ex.day
# Author	: VKS
# Date		: 10/26/16,10/27,1/3/18
# Args		: x = a vector of calendar dates
# Output	: Falls back to the closest weekday
# -----------------------------------------------------------------
	z <- day.to.int(x)
	z <- z - vec.max(z %% 7 - 4, 0)
	z <- day.ex.int(z)
	z
}
yyyymmdd.ex.int <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.ex.int
# Author	: VKS
# Date		: 1/3/18
# Args		: x = an integer or vector of integers
# Output	: the <x>th weekday after Monday, January 1, 2018
# -----------------------------------------------------------------
	day.ex.int(x + 2 * (x %/% 5))
}
yyyymmdd.ex.txt <- function(x, y = "/", n = "MDY") {
# -----------------------------------------------------------------
# Name		: yyyymmdd.ex.txt
# Author	: VKS
# Date		: 9/15/16,11/21,11/22,12/14/17,4/25/22
# Args		: x = a vector of dates in some format
#		: y = separators used within <x>
#		: n = order in which month, day and year are represented
# Output	: a vector of calendar dates in YYYYMMDD format
# -----------------------------------------------------------------
#
# EXCISE TIME
	m <- as.numeric(regexpr(" ", x, fixed = T))
	m <- ifelse(m == -1, 1 + nchar(x), m)
	x <- substring(x, 1, m - 1)
#
# FIRST PIECE
	z <- list()
	z[[txt.left(n, 1)]] <- substring(x, 1, as.numeric(regexpr(y, x, fixed = T)) - 1)
	x <- substring(x, 2 + nchar(z[[1]]), nchar(x))
#
# SECOND & THIRD PIECES
	z[[substring(n, 2, 2)]] <- substring(x, 1, as.numeric(regexpr(y, x, fixed = T)) - 1)
	z[[substring(n, 3, 3)]] <- substring(x, 2 + nchar(z[[2]]), nchar(x))
#
# RECONSTITUTE THE PIECES
	x <- yyyy.ex.yy(z[["Y"]])
	z <- 10000 * x + 100 * as.numeric(z[["M"]]) + as.numeric(z[["D"]])
	z <- as.character(z)
#
# RETURN RESULT
	z
}
yyyymmdd.ex.yyyymm <- function(x, y = T) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.ex.yyyymm
# Author	: VKS
# Date		: 10/26/16
# Args		: x = a vector/single YYYYMM depending on if y is T/F
#		: y = T/F variable depending on whether the last or
#		:	all trading days in that month are desired
# Output	: last/all weekdays in <x>
# -----------------------------------------------------------------
#
# LAST DAY IN MONTH OF INTEREST
	z <- paste0(yyyymm.lag(x, -1), "01")
	z <- yyyymmdd.ex.day(z)
	w <- yyyymmdd.to.yyyymm(z) != x
	if (any(w)) z[w] <- yyyymm.lag(z[w])
#
# FIRST DAY AS NEEDED
	if (!y & length(x) > 1) stop("You can't do this ...\n")
	if (!y) {
		x <- paste0(x, "01")
		x <- yyyymmdd.ex.day(x)
		if (yyyymmdd.to.yyyymm(x) != yyyymmdd.to.yyyymm(z)) x <- yyyymm.lag(x, -1)
		z <- yyyymm.seq(x, z)
	}
#
# RETURN RESULT
	z
}
yyyymmdd.exists <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.exists
# Author	: VKS
# Date		: 1/3/18
# Args		: x = a vector of calendar dates
# Output	: returns T if <x> is a weekday
# -----------------------------------------------------------------
	is.element(day.to.weekday(x), 1:5)
}
yyyymmdd.lag <- function(x, y) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.lag
# Author	: VKS
# Date		: 9/14/16,1/3/18
# Args		: x = a vector of weekdays
#		: y = an integer
# Output	: lags <x> by <y> weekdays
# -----------------------------------------------------------------
	obj.lag(x, y, yyyymmdd.to.int, yyyymmdd.ex.int)
}
yyyymmdd.seq <- function(x, y, n = 1) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.seq
# Author	: VKS
# Date		: 9/15/16,9/16,1/3/18
# Args		: x = a single weekday
#		: y = a single weekday
#		: n = a positive integer
# Output	: a sequence of weekdays starting at <x> and, if possible, ending at <y>
# -----------------------------------------------------------------
	if (any(!yyyymmdd.exists(c(x, y)))) stop("Inputs are not weekdays")
	z <- obj.seq(x, y, yyyymmdd.to.int, yyyymmdd.ex.int, n)
	z
}
yyyymmdd.to.AllocMo <- function(x, y = 23) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.AllocMo
# Author	: VKS
# Date		: 11/9/16,3/1/17,3/6,6/1
# Args		: x = the date for which you want flows (known one day later)
#		: y = calendar day in the next month when allocations are known (usually the 23rd)
# Output	: Returns the month for which you need to get allocations
#		: Flows as of the 23rd of each month are known by the 24th. By this time allocations from
#		:	the previous month are known
# -----------------------------------------------------------------
#
# DETERMINE MONTHLY LAG
	n <- txt.right(x, 2) # CALENDAR DAY
	n <- as.numeric(n) # WEIRD SHIT HAPPENS OTHERWISE WHEN <y> IS BELOW 10
	n <- ifelse(n < y, 2, 1) # COMPARE TO <y>
#
# CONVERT TO yyyymm
	z <- yyyymmdd.to.yyyymm(x)
#
# IMPLEMENT LAG
	z <- yyyymm.lag(z, n)
#
# RETURN RESULT
	z
}
yyyymmdd.to.AllocMo.unique <- function(x, y, n) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.AllocMo.unique
# Author	: VKS
# Date		: 11/10/21
# Args		: x = the date(s) for which you want flows
#		: y = calendar day allocations are known the next month
#		: n = T/F if month should be converted to day
# Output	: Checks each day in <x> has same allocation month. Error otherwise
# -----------------------------------------------------------------
	z <- yyyymmdd.to.AllocMo(x, y)
	if (all(z == z[1])) z <- z[1] else stop("Bad Allocation Month")
	if (n) z <- yyyymm.to.day(z)
	z
}
yyyymmdd.to.CalYrDyOfWk <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.CalYrDyOfWk
# Author	: VKS
# Date		: 10/4/16
# Args		: x = a vector of dates in yyyymmdd format
# Output	: Converts to 0 = Sun, 1 = Mon, ..., 6 = Sat
# -----------------------------------------------------------------
	z <- day.to.weekday(x)
	z <- as.numeric(z)
	z <- z/10
	x <- substring(x, 1, 4)
	x <- as.numeric(x)
	z <- x + z
	z
}
yyyymmdd.to.int <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.int
# Author	: VKS
# Date		: 1/3/18
# Args		: x = a vector of weekdays in YYYYMMDD format
# Output	: number of weekdays after Monday, January 1, 2018
# -----------------------------------------------------------------
	z <- day.to.int(x)
	z <- z - 2 * (z %/% 7)
	z
}
yyyymmdd.to.txt <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.txt
# Author	: VKS
# Date		: 4/4/19,12/6
# Args		: x = a vector of YYYYMMDD
# Output	: Engineering date format
# -----------------------------------------------------------------
	paste(format(day.to.date(x), "%m/%d/%Y"), "12:00:00 AM")
}
yyyymmdd.to.unity <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.unity
# Author	: VKS
# Date		: 9/20/16
# Args		: x = a vector of dates in yyyymmdd format
# Output	: returns a vector of 1's corresponding to the length of <x>
# -----------------------------------------------------------------
	rep(1, length(x))
}
yyyymmdd.to.weekofmonth <- function(x) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.weekofmonth
# Author	: VKS
# Date		: 9/19/16
# Args		: x = a vector of dates in yyyymmdd format
# Output	: returns 1 if the date fell in the first week of the month, 2 if it fell in the second, etc.
# -----------------------------------------------------------------
	z <- substring(x, 7, 8)
	z <- as.numeric(z)
	z <- (z - 1) %/% 7 + 1
	z
}
yyyymmdd.to.yyyymm <- function(x, y = F) {
# -----------------------------------------------------------------
# Name		: yyyymmdd.to.yyyymm
# Author	: VKS
# Date		: 10/26/16,10/27
# Args		: x = a vector of dates in yyyymmdd format
#		: y = if T then falls back one month
# Output	: Converts to yyyymm format
# -----------------------------------------------------------------
	z <- substring(x, 1, 6)
	if (y) z <- yyyymm.lag(z, 1)
	z
}
zav <- function(x) {
# -----------------------------------------------------------------
# Name		: zav
# Author	: VKS
# Date		: 9/12/16,12/15/17,1/24/19
# Args		: x = a vector/matrix/dataframe
# Output	: Coverts NA's to zero
# -----------------------------------------------------------------
	fcn <- function(x) ifelse(is.na(x), 0, x)
	z <- fcn.mat.vec(fcn, x,, T)
	z
}
zScore <- function(x) {
# -----------------------------------------------------------------
# Name		: zScore
# Author	: VKS
# Date		: 9/26/16,11/3,12/15/17
# Args		: x = a vector/matrix/data-frame
# Output	: Converts <x>, if a vector, or the rows of <x> otherwise, to a zScore
# -----------------------------------------------------------------
	fcn.mat.vec(mat.zScore, x, , F)
}
zScore.underlying <- function(x) {
# -----------------------------------------------------------------
# Name		: zScore.underlying
# Author	: VKS
# Date		: 12/20/18
# Args		: x = a vector/matrix/data-frame. The first columns are
#		: numeric while the last column is logical without NA's
# Output	: zScores the first columns of <x> using the last column as weight
# -----------------------------------------------------------------
	m <- dim(x)[1]
	n <- dim(x)[2]
#
# RECOVER PIECES
	y <- x[, n]
	x <- x[, -n]
#
# COMPUTE RESULT
	if (sum(y) > 1 & n == 2) {
		mx <- mean(x[y], na.rm = T)
		sx <- nonneg(sd(x[y], na.rm = T))
		z <- (x - mx)/sx
	} else if (n == 2) {
		z <- rep(NA, length(x))
	} else if (sum(y) > 1) {
		mx <- colMeans(x[y, ], na.rm = T)
		sx <- apply(x[y, ], 2, sd, na.rm = T)
		z <- t(x)
		z <- (z - mx)/nonneg(sx)
		z <- mat.ex.matrix(t(z))
	} else {
		z <- matrix(NA, m, n - 1, F, dimnames(x))
		z <- mat.ex.matrix(z)
	}
#
# RETURN RESULT
	z
}
