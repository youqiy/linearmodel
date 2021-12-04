#'lr
#'
#'Generate linear regression model and perform basic analysis
#'
#'@param formula the formula of your model
#'@param data the name of your data
#'@param intercept logical; if `TRUE`, intercept is included in your model
#'@param interact logical; if `TRUE`, interactions between variables are included in your model
#'@param category is the variable categorical in your model? Possible values are `1`, `2`, and `3`.
#' `1` means using the reference cell coding. `2` means using the cell means coding.
#' `3` is the default, meaning that the variable is continuous.
#'
#'@return `lr` returns a list of results regarding the linear model
#'@return y.fitted: a vector of fitted values of the dependent variables
#'@return residual: a matrix includes conventional residuals and internally studentized residuals
#'@return cov.matrix: a matrix includes the estimates, standard errors, t-test for each coefficient
#'@return df: the value of degrees of freedom in the model
#'@return r.squared: the value of r-squared in the model
#'@return sigma: the value of residual standard error in the model
#'@return f.result: a vector of F-test regarding the whole model
#'
#'@examples
#'data(Angell)
#'# By default, the intercept is included, while the interactions are not.
#'lr(moral~hetero+mobility, data = Angell)
#'
#'# When intercept = FALSE, the model would not include the intercept.
#'lr(moral~hetero+mobility, data = Angell, intercept = FALSE)
#'
#'# When interaction = TRUE, the model would include the interactions between covariates.
#'lr(moral~hetero+mobility, data = Angell, interact = TRUE)
#'
#'# Regarding the categorical variables, if category = 1, a reference cell coding is performed.
#'lr(moral~region, data = Angell, category = 1)
#'
#'# If category = 2, then a cell means coding is performed.
#'lr(moral~region, data = Angell, category = 2)
#'
#'@export
#'
lr <- function(formula, data, intercept = T, interact = F, category = 3){
  # generate design matrix
  dep = deparse(formula[[2]])
  ind = setdiff(as.character(formula[[3]]), c("+"))
  Y = as.matrix(data[, dep])
  cov.matrix = as.matrix(data[, ind])
  X = cov.matrix
  n = nrow(X)
  ## reference cell coding
  if(category == 1){
    type = unique(data[, ind])
    type = as.matrix(type)
    type = sort(type)
    type = as.character(type)
    ref = type[1]
    type = type[-1]
    n_type = length(type)
    for(i in seq_len(n_type)){
      index_col <- as.numeric(rep(0, n))
      for(k in seq_len(n)){
        if(type[i] == cov.matrix[k, 1]){
          index_col[k] = 1
        }
      }
      X = cbind(X, index_col)
    }
    X = X[, -1]
    X = unname(X)
    X = matrix(as.numeric(X), nrow(X), ncol(X))
    ind = type
  }
  ## cell means coding
  if(category == 2){
    type = unique(data[, ind])
    type = as.matrix(type)
    n_type = length(type)
    type = sort(type)
    type = as.character(type)
    for(i in seq_len(n_type)){
      index_col <- rep(0, n)
      for(k in seq_len(n)){
        if(type[i] == cov.matrix[k, 1]){
          index_col[k] = 1
        }
      }
      X = cbind(X, index_col)
    }
    X = X[, -1]
    X = unname(X)
    X = matrix(as.numeric(X), nrow(X), ncol(X))
    ind = type
  }
  ## whether the interaction form is considered or not
  if(interact == T){
    n_cov = ncol(X)
    n_inter = n_cov*(n_cov - 1)/2
    name_inter <- rep(0, n_inter)
    l = 1
    for(i in seq_len(n_cov - 1)){
      for(k in c((i+1):n_cov)){
        index_col <- X[, i]*X[, k]
        X = cbind(X, index_col)
        name_inter[l] = paste(ind[c(i, k)], collapse = "*")
        l = l + 1
      }
    }
    ind = c(ind, name_inter)
  }
  ## whether the intercept is included or not
  if(intercept == T && category != 2){
    X = cbind(1, X)
  }
  p = ncol(X)

  # estimation
  betahat = solve(crossprod(X)) %*% crossprod(X, Y)
  Yhat = X %*% betahat
  epsilonhat = Y - Yhat ## conventional residuals
  sigma_squared = as.numeric(crossprod(epsilonhat)/(n-p))

  # variance of beta hat
  var_betahat = diag(solve(crossprod(X))) * c(sigma_squared)
  se_betahat = sqrt(var_betahat)

  # inference
  t_statistic = c(betahat/se_betahat)
  p_value = c(2 * (1 - pt(q = abs(t_statistic), df = n-p)))

  # r_squared
  if(intercept == F || category == 2){
    Y_bar = 0
  }else{
    Y_bar = mean(Y)
  }
  SSY = sum((Y - Y_bar)^2)
  SSE = crossprod(epsilonhat)
  r_squared = 1 - SSE/SSY

  # f-test
  SSR = SSY - SSE
  df_no = n - p
  if(intercept == F || category == 2){
    df_de = p
  }else{
    df_de = p - 1
  }
  f_statistic = SSR/(df_de)/(SSE/df_no)
  pf_value = 1 - pf(q =  f_statistic, df1 = df_de, df2 = df_no)

  # studendized residuals
  sig = sqrt(sigma_squared)
  hat_matrix =  X %*% solve(crossprod(X)) %*% t(X)
  r_stud = epsilonhat/(sig * sqrt(1 - diag(hat_matrix)))
  residual_matrix = cbind(epsilonhat, r_stud)

  # output
  if(intercept == T && category != 2){
    ind = c("(Intercept)", ind)
  }
  cov_matrix = matrix(c(betahat, se_betahat, t_statistic, p_value), p, 4)
  rownames(cov_matrix) = ind
  colnames(cov_matrix) = c("Estimate", "Std.Error", "t value", "Pr(>|t|)")
  output <- list(y.fitted = Yhat, residual = residual_matrix, cov.matrix = cov_matrix,
                 df = (n - p), r.squared = r_squared, sigma = sqrt(sigma_squared),
                 f.result = c(f_statistic, df_de, df_no, pf_value))
  return(output)
}

