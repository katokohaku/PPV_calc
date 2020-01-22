---
title: "PPV_calc"
output:
  html_document:
    fig_caption: yes
    pandoc_args:
      - --from
      - markdown+autolink_bare_uris+tex_math_single_backslash-implicit_figures
    keep_md: yes
    toc: yes
  word_document:
    toc: yes
    toc_depth: 3
  pdf_document:
    toc: yes
    toc_depth: 3
editor_options: 
  chunk_output_type: inline
---



PPV calculator for diagnostic tests with given sensitivity and specificity under different prevalense rate



```r
require(tidyverse)
require(magrittr)
require(ggplot2)
```


```r
getPPV <- function(sensitivity = 0.9, specifisity = 0.9, prevalence = 0.1, pop_size = 100000) {
  n_POS <-      prevalence  * pop_size
  n_NEG <- (1 - prevalence) * pop_size
  
  n_TP <- sensitivity     * n_POS
  n_FP <- (1 - specifisity) * n_NEG  # n_NEG - n_TN = n_NEG - specifisity * n_NEG
  
  PPV <- n_TP / (n_TP + n_FP) 
  return(PPV)
}
```


```r
getPPV(sensitivity = 0.8, specifisity = 0.8, prevalence = 1 / 5)
## [1] 0.5
getPPV(sensitivity = 0.9, specifisity = 0.9, prevalence = 1 / 10)
## [1] 0.5
getPPV(sensitivity = .99, specifisity = .99, prevalence = 1 /100)
## [1] 0.5
```


```r
drawPPV <- function(performance = list(c(sensitivity = 0.9, specifisity = 0.9)),
                    prevalences = c("1/5", "1/10", "1/50", "1/1000", "1/10000", "1/100000")) {
  ppvs <- data.frame(NULL)
  e <- strsplit(prevalences, "/")
  
  for(i in 1:length(performance)) {
    this <- performance[[i]]
    for (j in 1:length(e)) {
      p <- as.numeric(e[[j]][1]) / as.numeric(e[[j]][2])
      
      this_ppvs <- data.frame(
        sensitivity = this["sensitivity"], 
        specifisity   = this["specifisity"], 
        prevalence  = prevalences[j], 
        PPV = getPPV(
          sensitivity = this["sensitivity"],
          specifisity = this["specifisity"],
          prevalence = as.numeric(p)
        )
      )
      # print(this_ppvs)  
      ppvs <- rbind(ppvs, this_ppvs)
    }
  }
  rownames(ppvs) <- NULL
  ppvs$Sens_vs_Spec <- factor(sprintf("%.03f : %.03f", ppvs$sensitivity, ppvs$specifisity))
  ppvs
}
```


```r
res <- drawPPV()
str(res)
## 'data.frame':	6 obs. of  5 variables:
##  $ sensitivity : num  0.9 0.9 0.9 0.9 0.9 0.9
##  $ specifisity : num  0.9 0.9 0.9 0.9 0.9 0.9
##  $ prevalence  : Factor w/ 6 levels "1/5","1/10","1/50",..: 1 2 3 4 5 6
##  $ PPV         : num  0.692308 0.5 0.155172 0.008929 0.000899 ...
##  $ Sens_vs_Spec: Factor w/ 1 level "0.900 : 0.900": 1 1 1 1 1 1
```


```r
pc <- list(
  c(sensitivity = 0.8, specifisity = 0.8),
  c(sensitivity = 0.9, specifisity = 0.9),
  c(sensitivity = 0.99, specifisity = 0.9),
  c(sensitivity = 0.99, specifisity = 0.99),
  c(sensitivity = 0.999, specifisity = 0.999)
)
dp <- drawPPV(performance = pc)

gg <- ggplot(dp, aes(x = prevalence, y = PPV, colour = Sens_vs_Spec, group = Sens_vs_Spec)) +
  geom_line() +
  geom_point() +
  labs(title = "PPV with given sensitivity and specificity under different prevalense",
       color = "Sensitivity : Specificity") +
  theme_classic()

export::graph2png(gg, file = "compare.png")
## Exported graph as compare.png
```

![](./compare.png)


