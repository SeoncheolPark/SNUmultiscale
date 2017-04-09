predict.rqss <- function (object, newdata, interval = "none", level = 0.95, ...) 
{
  ff <- object$fake.formula
  Terms <- delete.response(terms(object$formula, "qss"))
  Names <- all.vars(parse(text = ff))
  if (any(!(Names %in% names(newdata)))) 
    stop("newdata doesn't include some model variables")
  ff <- reformulate(attr(ff, "term.labels"))
  nd <- eval(model.frame(ff, data = newdata), parent.frame())
  qssterms <- attr(Terms, "specials")$qss
  if (length(qssterms)) {
    tmp <- untangle.specials(Terms, "qss")
    dropv <- tmp$terms
    m <- length(dropv)
    if (length(dropv)) 
      PLTerms <- Terms[-dropv]
    attr(PLTerms, "specials") <- tmp$vars
  }
  else {
    PLTerms <- Terms
    m <- 0
  }
  if (requireNamespace("MatrixModels") && requireNamespace("Matrix")) 
    X <- as(MatrixModels::model.Matrix(PLTerms, data = nd, 
                                       sparse = TRUE), "matrix.csr")
  else X <- model.matrix(PLTerms, data = nd)
  p <- ncol(X)
  y <- X %*% object$coef[1:p]
  X <- as.matrix.csr(X)
  if (m > 0) {
    for (i in 1:m) {
      qss <- object$qss[[i]]
      names <- all.vars(Terms[dropv[i]])
      names <- names[names %in% Names]
      dimnames(qss$xyz)[[2]] <- c(names, "zfit")
      newd <- nd[names]
      if (ncol(qss$xyz) == 3) {
        g <- predict.qss2(qss$xyz, newdata = newd, ...)
        y <- y + g$z
        if (interval == "confidence") 
          X <- cbind(X, g$D)
      }
      else if (ncol(qss$xyz) == 2) {
        g <- predict(qss, newdata = newd, ...)
        y <- y + g$y
        if (interval == "confidence") 
          X <- cbind(X, g$D)
      }
      else stop("invalid fitted qss object")
    }
  }
  if (interval == "confidence") {
    v <- sqrt(diag(X %*% summary(object, cov = TRUE)$V %*% 
                     t(X)))
    calpha <- qnorm(1 - (1 - level)/2)
    y <- cbind(y, y - v * calpha, y + v * calpha)
    dimnames(y)[[2]] <- c("yhat", "ylower", "yupper")
  }
  y
}