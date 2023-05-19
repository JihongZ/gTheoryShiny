#' @title Generate the variance components from a G-study
#' @import lme4
#' @description
#' Generates the variance components and the relative magnitude (percent of total variance) of each variance component.
#'
#' @param x An lmer fitted object
#'
#' @examples
#' lme4.res<-lmer(Score ~ (1 | Person) + (1 | Task) +(1 | Rater:Task) + (1 | Person:Task),data = Brennan.3.2)
#' one_facet <- lmer(Score ~ (1 | Participants) + (1 | Items), data = efData)
#' two_facet <- lmer(scores ~ (1 | students) + (1  | prompts) + (1 | raters) + (1 | students:prompts) +  (1 | students:raters) + (1 | prompts:raters), data = writing)
#' gstudy(one_facet)
#' gstudy(two_facet)
#' # Two facet with prompts fixed
#' gstudy(two_facet, fixed = "prompts")
#' @export



#function
gstudy <- function(x, fixed = NULL) {
  tmp <- as.data.frame(VarCorr(x))
  tmp <- tmp[c(1,4)] # select out variance components
  no.match <- function(x) {x[-match(fixed, x)]}
  if(!is.null(fixed)){
    n_adj <- length(unique(x@frame[,grep(fixed, names(x@frame))]))
    fixed_vars <- tmp[grep(fixed, tmp$grp),]
    fixed_vars <- fixed_vars[-match(fixed, fixed_vars$grp),]
    fixed_vars$adj_vcov <- fixed_vars$vcov/n_adj; fixed_vars$vcov <- NULL
    add_back <- strsplit(fixed_vars$grp, ":")
    fixed_vars$grp <- sapply(add_back, no.match)
    two.way <- data.frame(grp = paste(fixed_vars$grp, collapse = ":"), adj_vcov = tmp[nrow(tmp),2]/n_adj)
    fixed_vars <- rbind(fixed_vars, two.way)
    tmp <- merge(tmp, fixed_vars)
    tmp[,2] <- tmp[,2] + tmp[,3]; tmp[,3] <- NULL
    tmp$grp[length(tmp$grp)] <- "Residual"
  }
  colnames(tmp) <- c("Source", "Est.Variance")
  tmp$Percent.Variance <- tmp$Est.Variance/sum(tmp$Est.Variance)
  tmp[,2] <- round(tmp[,2], 4)
  tmp[,3] <- paste0(round(tmp[,3] * 100,1), "%")
  N <- length(x@resp$y)
  output <- list(gstudy.out = tmp, nobs = N)
  class(output) <- "gStudy"
  return(output)
}

#variable
# gstudy.res <- gstudy(lme4.res)
cbind_dif <- function(x = list()){
  # Find max length
  max_length <- max(unlist(lapply(x, length)))
  
  # Set length of each vector as
  res <- lapply(x, function(x){
    length(x) <- max_length
    return(x)
  })
  
  return(as.data.frame(res))
}

extractTheta <- function(x) {
  est <- coef(x)
  est <- sapply(est, function(x) unlist(x))
  cbind_dif(est)
}


#function
gstudy.forboot <- function(x, fixed = NULL) {
  tmp <- as.data.frame(VarCorr(x))
  tmp <- tmp[c(1,4)]
  no.match <- function(x) {x[-match(fixed, x)]}
  if(!is.null(fixed)){
    n_adj <- length(unique(x@frame[,grep(fixed, names(x@frame))]))
    fixed_vars <- tmp[grep(fixed, tmp$grp),]
    fixed_vars <- fixed_vars[-match(fixed, fixed_vars$grp),]
    fixed_vars$adj_vcov <- fixed_vars$vcov/n_adj;fixed_vars$vcov <- NULL
    add_back <- strsplit(fixed_vars$grp, ":")
    fixed_vars$grp <- sapply(add_back, no.match)
    two.way <- data.frame(grp = paste(fixed_vars$grp, collapse = ":"), adj_vcov = tmp[nrow(tmp),2]/n_adj)
    fixed_vars <- rbind(fixed_vars, two.way)
    tmp <- merge(tmp, fixed_vars)
    tmp[,2] <- tmp[,2] + tmp[,3]; tmp[,3] <- NULL
    tmp$grp[length(tmp$grp)] <- "Residual"
  }
  colnames(tmp) <- c("Source", "Est.Variance")
  tmp$Percent.Variance <- tmp$Est.Variance/sum(tmp$Est.Variance)
  tmp[,2] <- round(tmp[,2], 4)
  tmp[,3] <- paste0(round(tmp[,3] * 100,1), "%")
  N <- length(x@resp$y)
  output <- list(gstudy.out = tmp, nobs = N)
  class(output) <- "gStudy"
  output$gstudy.out[,2]
}

#function and variable
# boot.gstudy<-lme4::bootMer(lme4.res, gstudy.forboot, nsim=nboot, use.u=FALSE, type="parametric",parallel = "snow",ncpus =2) 
# boot.gstudy.res<-cbind(gstudy.res$gstudy.out,t(boot.gstudy$t))
# gstudy.res.CI<-t(apply(t(boot.gstudy$t),1,function(x){quantile(x, probs = c(.025, .975))}))


#' Prints the results from the G-study
#'
#' Prints  output from fitting the \code{gstudy} function.
#' @param x object of class \code{gStudy} returned from the \code{gstudy} function
#' @param ... additional objects of the same type.
#' @method print gStudy
#' @seealso \code{\link{gstudy}}
#' @export
print.gStudy <- function(x, ...){
  print(x$gstudy.out)
}






#' @title Conduct a dependability study
#' @import lme4
#' @description
#' Generates the variance components and the relative magnitude (percent of total variance) of each variance component.
#'
#' @param x A gstudy object
#' @param n A vector with facets named
#' @param unit The label for the unit of measurement
#'
#' @examples
#' ## One facet design
#' one.facet <- lmer(Score ~ (1 | Participants) + (1 | Items), data = efData)
#' one.facet.gstudy <- gstudy(one.facet)
#' dstudy(one.facet.gstudy, n = c("Items" = 4), unit = "Participants")
#'
#' ## Two facet design
#' two_facet <- lmer(scores ~ (1 | students) + (1  | prompts) + (1 | raters) + (1 | students:prompts) +  (1 | students:raters) + (1 | prompts:raters), data = writing)
#' two_facet_gstudy <- gstudy(two_facet)
#' dstudy(two_facet_gstudy, n = c("raters" = 2, "prompts" = 5), unit = "students")
#'
#' ## Two facet fixed design wtih prompt
#' gstudy_fixed_prompt <- gstudy(two.facet, fixed = "prompts")
#' dstudy(gstudy_fixed_prompt, n = c("raters" = 2), unit = "students")
#' @export


#function
dstudy <- function(x, n, unit) {
  tmp <- x$gstudy.out
  tmp <- tmp[c(1,2)]
  us.var <- tmp[tmp$Source %in% unit,2]
  n.matrix <- matrix(nrow = nrow(tmp), ncol = length(n))
  for(i in 1:length(n)) n.matrix[grep(names(n)[i], tmp$Source),i] <- n[i]
  n.matrix[nrow(n.matrix),] <- n
  tmp$n <- apply(n.matrix, 1, prod, na.rm = T)
  tmp[match(unit, tmp$Source), "n"] <- x$nobs
  tmp$vcov.n <- tmp$Est.Variance/tmp$n
  tmp[match(unit, tmp$Source), "vcov.n"] <- tmp[match(unit, tmp$Source), "Est.Variance"]
  
  # relative variance ----
  rel.var <- tmp$vcov.n[nrow(tmp)]
  if(length(n)>1){
    for(i in 1:length(n)) {
      tmp.names <- c(paste0(unit, ":", names(n)[i]), paste0(names(n)[i], ":", unit))
      tmp.var <- tmp[tmp$Source %in% tmp.names,"vcov.n"]
      rel.var <- sum(rel.var, tmp.var)
    }
  }
  
  # absolute variance ----
  tmp.abs <- tmp[-nrow(tmp),]
  abs.var <- sum(tmp.abs[-grep(unit, tmp.abs$Source),"vcov.n"], rel.var)
  
  # generalizability coefficient
  g.coef <- us.var/(us.var + rel.var)
  
  # dependability coefficient
  d.coef <- us.var/(us.var + abs.var)
  
  output <- list(ds.df = tmp, relvar = rel.var, absvar = abs.var, gcoef = g.coef, dcoef = d.coef)
  class(output) <- "dStudy"
  return(output)
}

#variable
# dstudy.res<-dstudy(gstudy.res,n, unit)  # Task 3:  self-defined n and unit

#function
dstudy.forboot <- function(x, n, unit) {
  tmp <- x$gstudy.out
  tmp <- tmp[c(1,2)]
  us.var <- tmp[tmp$Source %in% unit,2]
  n.matrix <- matrix(nrow = nrow(tmp), ncol = length(n))
  for(i in 1:length(n)) n.matrix[grep(names(n)[i], tmp$Source),i] <- n[i]
  n.matrix[nrow(n.matrix),] <- n
  tmp$n <- apply(n.matrix, 1, prod, na.rm = T)
  tmp[match(unit, tmp$Source), "n"] <- x$nobs
  tmp$vcov.n <- tmp$Est.Variance/tmp$n
  tmp[match(unit, tmp$Source), "vcov.n"] <- tmp[match(unit, tmp$Source), "Est.Variance"]
  
  # relative variance ----
  rel.var <- tmp$vcov.n[nrow(tmp)]
  if(length(n)>1){
    for(i in 1:length(n)) {
      tmp.names <- c(paste0(unit, ":", names(n)[i]), paste0(names(n)[i], ":", unit))
      tmp.var <- tmp[tmp$Source %in% tmp.names,"vcov.n"]
      rel.var <- sum(rel.var, tmp.var)
    }
  }
  
  # absolute variance ----
  tmp.abs <- tmp[-nrow(tmp),]
  abs.var <- sum(tmp.abs[-grep(unit, tmp.abs$Source),"vcov.n"], rel.var)
  
  # generalizability coefficient
  g.coef <- us.var/(us.var + rel.var)
  
  # dependability coefficient
  d.coef <- us.var/(us.var + abs.var)
  
  output <- list(ds.df = tmp, relvar = rel.var, absvar = abs.var, gcoef = g.coef, dcoef = d.coef)
  class(output) <- "dStudy"
  output
}


# boot.dstudy.res<-NULL
# for(i in 1:nboot){
#   temp<-gstudy.res
#   temp[1]$gstudy.out[,2]<-boot.gstudy.res[,-(1:3)][,i]
#   temp.dstudy<-dstudy(temp,n,unit)
#   
#   boot.dstudy.res<-rbind(boot.dstudy.res,
#                          c(temp.dstudy$ds.df[,4],temp.dstudy$relvar,temp.dstudy$absvar,temp.dstudy$gcoef,temp.dstudy$dcoef)
#                          )   
# }
# dstudy.res.CI<-t(apply(t(boot.dstudy.res),1,function(x){quantile(x, probs = c(.025, .975))}))




#If boot is on
# gstudy.res_boot <- gstudy.res
# dstudy.res_boot <- dstudy.res
# 
# 
# gstudy.res_boot$gstudy.out <-
#   cbind(gstudy.res_boot$gstudy.out, gstudy.res.CI)
# 
# dstudy.res_boot$dcoef[2] <-
#   dstudy.res.CI[nrow(dstudy.res.CI), 1]
# dstudy.res_boot$dcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI), 2]
# names(dstudy.res_boot$dcoef) <- c("Est", "2.5%", "97.5%")
# dstudy.res_boot$gcoef[2] <-
#   dstudy.res.CI[nrow(dstudy.res.CI) - 1, 1]
# dstudy.res_boot$gcoef[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 1, 2]
# names(dstudy.res_boot$gcoef) <- c("Est", "2.5%", "97.5%")
# dstudy.res_boot$absvar[2] <-
#   dstudy.res.CI[nrow(dstudy.res.CI) - 2, 1]
# dstudy.res_boot$absvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 2, 2]
# names(dstudy.res_boot$absvar) <- c("Est", "2.5%", "97.5%")
# dstudy.res_boot$relvar[2] <-
#   dstudy.res.CI[nrow(dstudy.res.CI) - 3, 1]
# dstudy.res_boot$relvar[3] <- dstudy.res.CI[nrow(dstudy.res.CI) - 3, 2]
# names(dstudy.res_boot$relvar) <- c("Est", "2.5%", "97.5%")
# dstudy.res_boot$ds.df <-
#   cbind(dstudy.res_boot$ds.df, dstudy.res.CI[1:(-4 + nrow(dstudy.res.CI)), ])

#' Prints the results from the D-study
#'
#' Prints  output from fitting the \code{dstudy} function.
#' @param x object of class \code{dStudy} returned from the \code{dstudy} function
#' @param ... additional objects of the same type.
#' @method print dStudy
#' @seealso \code{\link{dstudy}}
#' @export

print.dStudy <- function(x, ...){
  if(length(colnames(x$ds.df))==4){
    colnames(x$ds.df) <- c("Source", "Est.Variance", "N", "Est.(Var/N)")
    cat("\nThe generalizability coefficient is:", x$gcoef)
    cat(".\nThe dependability coefficient is: ", x$dcoef,sep = "")
    cat(".\nThe relative error  is: ", x$relvar, sep = "")
    cat(".\nThe absolute error is: ", x$absvar, sep = "")
  }else{colnames(x$ds.df) <- c("Source", "Est.Variance", "N", "Est.(Var/N)","2.5%","97.5%")
  cat("\nThe generalizability coefficient is:", x$gcoef[1],"; Its 95% CI is [",x$gcoef[2],",",x$gcoef[3],"]",sep='')
  cat(".\nThe dependability coefficient is: ", x$dcoef[1],"; Its 95% CI is [",x$dcoef[2],",",x$dcoef[3], "]",sep='')
  cat(".\nThe relative error  is: ", x$relvar[1],"; Its 95% CI is [",x$relvar[2],",",x$relvar[3],    "]",sep='')
  cat(".\nThe absolute error is: ", x$absvar[1],".; Its 95% CI is [",x$absvar[2],",",x$absvar[3],    "]",sep='')
  
  }
  cat(".\n")
  cat(".\n")
  print(x$ds.df)
  
}

# print.dStudy(dstudy.res_boot)

# formular1 <- "Score ~ (1 | Person_ID) +  (1 | Item_ID) "
# formular2 <- "Score ~ us(Occasion + 0 | Person_ID) +  us( Occasion + 0 | Item_ID) "
# easyformular <- "Score = Occasion | Person_ID + Occasion | Item_ID "
# makeeasyformular(formular1)
# makeeasyformular(formular2)
# 
# makehardformular(makeeasyformular(formular1))
# makehardformularMGtheory(makeeasyformular(formular2))
# makeeasyformular(formular2)

makeeasyformular <- function(formular) {
  formular <- str_replace_all(formular, pattern = "\\~", replacement = "=")
  formular <- str_replace_all(formular, pattern = "\\(1 \\|", replacement = "")
  formular <- str_replace_all(formular, pattern = "us\\(", replacement = "")
  formular <- str_replace_all(formular, pattern = "\\+ 0", replacement = "")
  str_replace_all(formular, pattern = "\\)", replacement = "")
}

makehardformular <- function(formular) {
  formular <- str_replace_all(formular, pattern = "\\=", replacement = "~(1|")
  formular <- str_replace_all(formular, pattern = "\\+", replacement = ")+(1|")
  paste0(formular, ")")
}
makehardformularMGtheory <- function(formular) {
  formular <- str_replace_all(formular, pattern = "\\+", replacement = ") + ( 0 +")
  formular <- str_replace_all(formular, pattern = "\\=", replacement = "~ ( 0 +")
  paste0(formular, ")")
}


# multivariate G-theory functions -----------------------------------------
extract.VarCorr.glmmTMB <- function (x, row.names = NULL, optional = FALSE, 
                                     order = c("cov.last", "lower.tri"), residCor) 
{
  order <- match.arg(order)
  tmpf <- function(v, grp) {
    lt.v <- lower.tri(v, diag = FALSE)
    # vcov <- c(diag(v), v[lt.v <- lower.tri(v, diag = FALSE)])
    sdcor <- c(attr(v, "stddev"), attr(v, "correlation")[lt.v])
    nm <- rownames(v)
    n <- nrow(v)
    dd <- data.frame(grp = grp, 
                     var1 = nm[c(seq(n), col(v)[lt.v])], 
                     var2 = c(rep(NA, n), nm[row(v)[lt.v]]), 
                     sdcor, 
                     stringsAsFactors = FALSE)
    if (order == "lower.tri") {
      m <- matrix(NA, n, n)
      diag(m) <- seq(n)
      m[lower.tri(m)] <- (n + 1):(n * (n + 1)/2)
      dd <- dd[m[lower.tri(m, diag = TRUE)], ]
    }
    dd
  }
  r <- do.call(rbind, c(mapply(tmpf, x, names(x), SIMPLIFY = FALSE), 
                        deparse.level = 0))
  if (attr(x, "useSc")) {
    ss <- attr(x, "sc")
    r <- rbind(r, data.frame(grp = "Residual", var1 = NA, 
                             var2 = NA, vcov = ss^2, sdcor = ss), deparse.level = 0)
  }
  rownames(r) <- NULL
  r$sdcor[r$sdcor == 0] = residCor[lower.tri(residCor)]
  
  ## adjust table
  resTable <- r |> 
    mutate(var2 = ifelse(is.na(var2), "Std.Dev", var2)) |> 
    pivot_wider(names_from = var2, values_from = sdcor)
  resTable
}


### Extract var-cov from glmmTMB object ----------------------------------------


extract.VarCorr.glmmTMB <- function (x, row.names = NULL, optional = FALSE, 
                                     order = c("cov.last", "lower.tri"), residCor) 
{
  order <- match.arg(order)
  tmpf <- function(v, grp) {
    lt.v <- lower.tri(v, diag = FALSE)
    # vcov <- c(diag(v), v[lt.v <- lower.tri(v, diag = FALSE)])
    sdcor <- c(attr(v, "stddev"), attr(v, "correlation")[lt.v])
    nm <- rownames(v)
    n <- nrow(v)
    dd <- data.frame(grp = grp, 
                     var1 = nm[c(seq(n), col(v)[lt.v])], 
                     var2 = c(rep(NA, n), nm[row(v)[lt.v]]), 
                     sdcor, 
                     stringsAsFactors = FALSE)
    if (order == "lower.tri") {
      m <- matrix(NA, n, n)
      diag(m) <- seq(n)
      m[lower.tri(m)] <- (n + 1):(n * (n + 1)/2)
      dd <- dd[m[lower.tri(m, diag = TRUE)], ]
    }
    dd
  }
  r <- do.call(rbind, c(mapply(tmpf, x, names(x), SIMPLIFY = FALSE), 
                        deparse.level = 0))
  if (attr(x, "useSc")) {
    ss <- attr(x, "sc")
    r <- rbind(r, data.frame(grp = "Residual", var1 = NA, 
                             var2 = NA, vcov = ss^2, sdcor = ss), deparse.level = 0)
  }
  rownames(r) <- NULL
  r$sdcor[r$sdcor == 0] = residCor[lower.tri(residCor)]
  
  ## adjust table
  resTable <- r |> 
    mutate(var2 = ifelse(is.na(var2), "Std.Dev", var2)) |> 
    pivot_wider(names_from = var2, values_from = sdcor)
  resTable
}