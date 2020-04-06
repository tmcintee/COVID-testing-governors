log_fit_function <- function(yourdata,dep,ind,mode = "function")
{
  yourdata <- data.frame(yourdata)
  y <- yourdata[,dep]
  x <- yourdata[,ind]
  log.ss <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
  Asym <- summary(log.ss)$coef[1]
  xmid <- summary(log.ss)$coef[2]
  scal <- summary(log.ss)$coef[3]
  f <- function(x)
  {
    Asym/(1+exp((xmid-x)/scal))
  }
  if(mode == "coefficients")
  {
    return(list(Asym = Asym, xmid = xmid, scal = scal))
  }
  else
  {
    return(f)
  }
}

exp_fit_function <- function(yourdata,dep,ind,mode = "function")
{
  yourdata <- data.frame(yourdata)
  y <- yourdata[,dep]
  x <- yourdata[,ind]
  exp.ss <- glm(log(y) ~ x)
  intercept <- summary(exp.ss)$coef[1]
  slope <- summary(exp.ss)$coef[2]
  f <- function(x)
  {
    exp(intercept+slope*x)
  }
  if(mode == "coefficients")
  {
    return(list(Initial = exp(intercept),Growth = slope))
  }
  else
  {
    return(f)
  }
}
