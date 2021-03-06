#!/usr/bin/Rscript
# Knit R markdown files (.rmd) to html files
library(knitr)

knit2html(
'~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/nuggetsPreprocess.rmd',
'~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/1_nuggetsPreprocess.html'
)

knit2html(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/nuggetAnalysis1.rmd',
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/2_nuggetAnalysis1.html')
          
knit2html(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/nuggetAnalysis2.rmd',
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/3_nuggetAnalysis2.html')          
  
knit2html(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/nuggetAnalysis3.rmd',
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/4_nuggetAnalysis3.html')          
  
knit2html(
'~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/nuggetTreeAnalysis.rmd',
'~/Desktop/cbtnuggetsbusinessanalyticsproject/revenue/5_nuggetTreeAnalysis.html'
)          
