## tst
## from https://emitanaka.org/slides/Auckland2021
library(agricolae)
## CRD
trt <- c("A", "B", "C")
d <- agricolae::design.crd(trt = trt, r = 2)
d## RCBD
trt <- c("A", "B", "C")
p <- agricolae::design.rcbd(trt = trt, r = 2)
## latin square
trt <- c("A", "B", "C")
l <- agricolae::design.lsd(trt = trt)
## balanced incomplete design
trt <- c("A", "B", "C")
b <- agricolae::design.bib(trt = trt, k = 2)
## factorial
f <- agricolae::design.ab(trt = c(3, 2), r = 2, design = "crd")
## split plot
trt1 <- c("I", "R"); trt2 <- LETTERS[1:4]
sp <- agricolae::design.split(trt1 = trt1, trt2 = trt2, r = 2, design = "crd")

library(ggplot2)
dta <- d$book
ggplot(dta, aes(block = rep(1, 6),id = 1:6)) +
    geom_design()


ggplot(dta, aes(x = rep(1, 6),y = 1:6, color = r)) +
    geom_point()


