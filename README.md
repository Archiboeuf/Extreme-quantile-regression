# Extreme-Quantile-Regression

We studied an article from Victor Chernozhukov and Iván Fernández-Val on the inference for extremal conditional quantile models, Inference for Extremal Conditional Quantile Models, with an Application to Market and Birthweight Risks, 26 Dec 2009 https://arxiv.org/abs/0912.5013. This article uses extreme values (EV) theory in order to derive inference for quantile regression in extremal quantiles, that is to say when <a href="http://www.codecogs.com/eqnedit.php?latex=$$\tau&space;\sim&space;O(1/T)$$" target="_blank"><img src="http://latex.codecogs.com/gif.latex?$$\tau&space;\sim&space;O(1/T)$$" title="$$\tau \sim O(1/T)$$" /></a>, T being the number of individuals in the sample, and <a href="http://www.codecogs.com/eqnedit.php?latex=$$\tau$$" target="_blank"><img src="http://latex.codecogs.com/gif.latex?$$\tau$$" title="$$\tau$$" /></a> being the quantile of interests. 

We decided to apply these regressions techniques to study quantiles of heart rates from individuals part of the National Longitudinal Study of Adolescent to Adult Health.

In our research, we first show how extreme values are a good way to estimate coefficients in extreme quantile regression. As seen in te below graph, normal (or central) coefficients do not converge for very low quantiles : their coverage is smaller than the extreme values coverage. That means that the true value of the coefficient is not within the 90% confidence interval around the estimated value. 

![alt text](https://github.com/JM-data/Extreme-Quantile-Regression/blob/master/Coverages.png)

The above example is longly explained in the PDF file. An analysis on extreme quantiles for heart rates can also be found. We have found and quantified the effects of extreme quantiles of heart rates (in BPM). 
