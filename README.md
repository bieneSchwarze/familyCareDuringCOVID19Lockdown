Here you can find the source code of the R programs used for data analysis for the manuscript 
"Family care during the COVID-19 lockdown: determinants and consequences for subjective well-being and health" 
by Katja Möhring, Sabine Zinn, Ulrike Ehrlich.

The source codes can be found in two data files: 
one for data preparation and panel analysis for 2018/2019 (*dataPreparation_analysis_2018_2019.R*) and one for data preparation and panel analysis 2019/2020 (*dataPreparation_analysis_2019_2020.R*).

The data used for 2018 and 2019 is avaiable on request form the SOEP data center (https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html), SUF v36. 
The data used for 2019 and 2020 stems from a special Corona study of the SOEP (SOEP-CoV) and is not yet available. The reason is that the correspeonding project is third-party funded. 
The data will be made available to the scientific community in 2022 with the regular next data release of the annual SOEP waves.

***A methodological note:***
The proportion of complete cases in our two analytic data sets (2018-2019 and 2019-2020) is less than 95%. Little’s (1988) test shows that the missingness mechanism is not missing completely at random. To counteract selection bias and at the same time increase the statistical power, we therefore multiply imputed missing values. For this purpose, we used the multivariate imputation by chained equations (mice) algorithm by van Buuren and Groothuis-Oudshoorn (2011), applying classification and regression trees (CART) as the imputation routine. To improve the predictive power of the imputation routine, we used several auxiliary variables in addition to the focal variables of this study (namely, migration background, educational attainment, household type, federal state). As suggested by Kim et al. (2006), we entered survey weights into the corresponding imputation models as explanatory variables. To maintain their autocorrelation and serial correlation structure, the data were imputed in wide format. We imputed m=20 data sets with 20 iteration steps in the Gibbs sampler of mice. We checked the convergence and meaningfulness of the estimated imputation models by means of the associated mice diagnostics (e.g., contrasting distributions of observed and imputed data).

* Buuren, S. van, Groothuis-Oudshoorn, K., 2011. mice : Multivariate Imputation by Chained Equations in R. J. Stat. Soft. 45. https://doi.org/10.18637/jss.v045.i03
* Kim, J. K., Michael Brick, J., Fuller, W. A. & Kalton, G. (2006). On the bias of the multiple‑imputation variance estimator in survey sampling. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 68(3), 509-521. 
* Little, R.J.A., 1988. A Test of Missing Completely at Random for Multivariate Data with Missing Values. Journal of the American Statistical Association 83, 1198–1202. https://doi.org/10.1080/01621459.1988.10478722
