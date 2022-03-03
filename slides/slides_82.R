### R code from vignette source 'slides_82.Rnw'

###################################################
### code chunk number 1: slides_82.Rnw:40-41
###################################################
options(scipen=1, digits=2)


###################################################
### code chunk number 2: packages
###################################################
set.seed(60637)
# For plotting:
library(ggplot2)
# library(devtools)
# devtools::install_github("wilkelab/ungeviz")
library(ungeviz)
library(ggridges)


###################################################
### code chunk number 3: slides_82.Rnw:552-556 (eval = FALSE)
###################################################
## ggplot(iris, aes(Species, Sepal.Length,fill = Species)) +
##     geom_violin(alpha = 0.25, color = NA) +
##     geom_point(position = position_jitter(width = 0.3, height = 0), size = 0.5) +
##     geom_hpline(aes(colour = Species), stat = "summary", width = 0.6, size = 1.5, fun = 'mean')


###################################################
### code chunk number 4: slides_82.Rnw:561-567
###################################################
ggplot(iris, aes(Species, Sepal.Length,fill = Species)) +
    geom_violin(alpha = 0.25, color = NA) +
    geom_point(position = position_jitter(width = 0.3, height = 0), size = 0.5) +
    geom_hpline(aes(colour = Species), stat = "summary", width = 0.6, size = 1.5, fun = 'mean') + 
    theme_bw() +
    ylab('Sepal Length')


###################################################
### code chunk number 5: slides_82.Rnw:591-595 (eval = FALSE)
###################################################
## ggplot(cacao_means, aes(x = estimate, y = location)) +
##     stat_confidence_density(aes(moe = std.error), confidence = 0.68, fill = "#81A7D6", height = 0.7) +
##     geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.3) +
##     geom_vpline(aes(x = estimate), size = 1.5, height = 0.7, color = "#D55E00")


###################################################
### code chunk number 6: slides_82.Rnw:600-621
###################################################
library(dplyr)
library(forcats)
library(broom)
library(emmeans)

cacao_lumped <- cacao %>%
    mutate(
        location = fct_lump(location, n = 10)
    )

cacao_means <- lm(rating ~ location, data = cacao_lumped) %>%
    emmeans("location") %>%
    tidy() %>%
    mutate(location = fct_reorder(location, estimate))

ggplot(cacao_means, aes(x = estimate, y = location)) +
    stat_confidence_density(aes(moe = std.error), confidence = 0.68, fill = "#81A7D6", height = 0.7) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.3) +
    geom_vpline(aes(x = estimate), size = 1.5, height = 0.7, color = "#D55E00") +
    xlim(2.8, 3.6) + 
    theme_bw() 


###################################################
### code chunk number 7: slides_82.Rnw:645-650 (eval = FALSE)
###################################################
## ggplot(cacao_means, aes(x = estimate, y = location)) +
##     stat_confidence_density(
##         aes(moe = std.error, height = stat(density)), geom = "ridgeline",
##         confidence = 0.68, fill = "#81A7D6", alpha = 0.8, scale = 0.08, min_height = 0.1) +
##     geom_vpline(aes(x = estimate), size = 1.5, height = 0.5, color = "#D55E00")


###################################################
### code chunk number 8: slides_82.Rnw:655-664
###################################################
ggplot(cacao_means, aes(x = estimate, y = location)) +
    stat_confidence_density(
        aes(moe = std.error, height = stat(density)), geom = "ridgeline",
        confidence = 0.68, fill = "#81A7D6", alpha = 0.8, scale = 0.08,
        min_height = 0.1
    ) +
    geom_vpline(aes(x = estimate), size = 1.5, height = 0.5, color = "#D55E00") +
    xlim(2.8, 3.6) + 
    theme_bw()


###################################################
### code chunk number 9: slides_82.Rnw:705-706
###################################################
hist(rnorm(10))


