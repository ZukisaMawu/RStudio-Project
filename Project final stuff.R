setwd("~/NMU 2022/Semester 02/Rstudio stuff")
rm(list = ls())
graphics.off()

#Read in our data
data_test_scores = read.csv("TestScores.csv", sep = ',', header = TRUE)
attach(data_test_scores)

X1 = cbind(write, soci)
X2 = cbind(phy, bio)

LS1 = lm(read ~ X1)
summary(LS1)

LS2 = lm(math ~ X2)
summary(LS2)
remove(s_errors)
#------------------------------------------------------------------------
#Read in our data
data_test_scores = read.csv("TestScores.csv", sep = ',', header = TRUE)
attach(data_test_scores)

Ya = cbind(read, math)
Y = matrix(Ya, ncol = 1, nrow = 1000)

Xa = matrix(1, 500, 1)
Xb = matrix(0, 500, 1)
Xc = cbind(Xa, Xb)
Xd = cbind(Xb, Xa)

Xup = matrix(Xc, 1000, 1)
x1o = matrix(Xd, 1000, 1)

zero = matrix(0, 500, 1)
left = cbind(write, zero, soci, zero)
XLeft = matrix(left, 1000, 2)

right = cbind(zero, phy, zero, bio)
Xright = matrix(right, 1000, 2)

XFinal = cbind(Xup, XLeft, x1o, Xright)

LSlarge = lm(Y ~ 0 + XFinal)
summary(LSlarge)

s1 = summary(LS1)$sigma^2
s2 = summary(LS2)$sigma^2
r1 = summary(LS1)$resid
r2 = summary(LS2)$resid

R = as.numeric((r1 %*% r2)/(500 - 3))
Rdiag = diag(R, 500)

S1 = diag(s1, 500)
S2 = diag(s2, 500)

Top = cbind(S1, Rdiag)
bottom = cbind(Rdiag, S2)

G = rbind(Top, bottom)

FGLS = solve(t(XFinal) %*% solve(G) %*% XFinal) %*% t(XFinal) %*% solve(G) %*% Y
FGLS

err = Y - XFinal %*% FGLS

sigma2 = as.numeric(t(err) %*% solve(G) %*% err)/(500 - 6)
covFGLS = solve(t(XFinal) %*% solve(G) %*% XFinal)
sigma_covFGLS = sigma2 * covFGLS
sigma_covFGLS

std.err = sqrt(diag(sigma_covFGLS))
std.err
#-------------------------------------------------------------------------------
#Read in our data
data_test_scores = read.csv("TestScores.csv", sep = ',', header = TRUE)
attach(data_test_scores)

library(systemfit)
first_sample = read ~ write + soci
second_sample = math ~ phy + bio
sur_model = systemfit(list(first = first_sample, second = second_sample), method = "SUR",
                      data = data_test_scores, maxit = 1000)
summary(sur_model)
