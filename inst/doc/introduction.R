## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(metasplines)
library(splines2)

# Estimate a linear regression model using an individual participant data (IPD):
res <- lm(
  Petal.Width ~
    Species +
    nsk(Sepal.Length, Boundary.knots = c(4.5, 7.5), knots = c(5, 6, 6.5)),
  data=iris)
summary(res)

# How is the estimated correlation matrix like?
cor.m <- cov2cor(vcov(res))
colnames(cor.m) <- NULL
round(cor.m, 3)

## -----------------------------------------------------------------------------
# "Literature-based" (LB) estimates:
lb.df <- read.table(text=
"variable,     cov.value,  est,  est.var
Sepal.Length,  4.5,	       0,     0   
Sepal.Length,  5,	         0.15,  0.01 
Sepal.Length,  5.5,	       0.25,  0.01 
Sepal.Length,  6,	         0.4,   0.01 
Sepal.Length,  6.5,	       0.5,   0.01 
Sepal.Length,  8,          0.25,  0.04 
", sep=",", header=TRUE)

lb.df

# Output table with the point estimates and the estimated variances:
pool_splines(v="Sepal.Length", meta.df=lb.df, glm.res=res)

# Full output containing also spline estimates and covariance matrix:
pooled <- pool_splines(v="Sepal.Length", meta.df=lb.df, glm.res=res, full.output = TRUE)
str(pooled)

## ----fig.width=7, fig.height=7------------------------------------------------
plot(0, 0, type="n", xlim=range(pooled$log.hr$cov.value), ylim=c(-0.2, 0.7),
     xlab="Covariate value", ylab="")
col.v <- rainbow(3)
names(col.v) <- unique(pooled$log.hr$model)
for (i in unique(pooled$log.hr$model)) {
  with(subset(pooled$log.hr, model==i), {
    lines(cov.value, est, col=col.v[i], lwd=2)
    lines(cov.value, ci.low, col=col.v[i], lty=2)
    lines(cov.value, ci.upp, col=col.v[i], lty=2)
  })
}
with(lb.df, {
  points(cov.value, est, pch=15, col=col.v["lb"])
  for (j in 2:nrow(lb.df))
    lines(rep(cov.value[j], 2), est[j] + 1.96 * c(-1, 1) * sqrt(est.var[j]),
          col=col.v["lb"], lty=2)
})
legend("bottomleft", legend=names(col.v), col=col.v, lwd=c(2,2,2))

