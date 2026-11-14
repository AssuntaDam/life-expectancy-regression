# Life Expectancy Regression Analysis

Multiple linear regression model of life expectancy across countries, built and validated in R, with a full diagnostic workflow (collinearity, normality, Box-Cox transformation, leverage, residuals, Cook's distance) and a prediction test on a different year.

> **Group project** (team of five) developed for the course *Statistical Inference*, BSc in Mathematical Engineering, Politecnico di Milano. 

## Goal

Explain the life expectancy of countries using health, economic and lifestyle indicators, choose a parsimonious and reliable model, check its assumptions, and test how well it predicts life expectancy in another year.

## Data

The data file is **not included** in this repository. The script expects `life_expectancy.csv` in the working directory: a country-level table with life expectancy and health, immunisation, economic and social indicators for several years.

- **Response:** `Life.expectancy`
- **Covariates (15):** `Status` (developed / developing), `Population`, `GDP`, `Total.expenditure` (government health spending), `Hepatitis.B`, `Measles`, `Polio`, `Diphtheria`, `HIV.AIDS`, `infant.deaths`, `under.five.deaths`, `BMI`, `thinness..1.19.years`, `Alcohol`, `Schooling`.
- **Sample:** year 2013 for fitting (countries with complete records, South Africa excluded) and year 2000 for the prediction test.

## Method

1. **Preparation.** Remove rows with missing values, set `Status` as a factor, select the year, use country names as row names.
2. **Full model and variable selection.** Fit the model with all 15 covariates, remove covariates with high p-values step by step (Polio, Measles, Alcohol, Hepatitis B), and compare models with AIC, BIC (`step`) and adjusted R-squared (`leaps`).
3. **Collinearity.** Variance inflation factors show a severe problem between `infant.deaths` and `under.five.deaths` (VIF above 200), so both are removed from the model.
4. **Homoscedasticity and normality.** Residuals against fitted values, QQ-plot and Shapiro-Wilk test. Normality is rejected on the original scale (p about 0.04).
5. **Box-Cox transformation.** The profile likelihood gives lambda close to 1.88, rounded to 2, so the response becomes (y^2 - 1) / 2. After the transformation the Shapiro-Wilk p-value rises to about 0.49.
6. **Leverage.** Hat values with threshold 2p/n identify high-leverage countries (for example Luxembourg, with the largest value). The model is refitted without them and the relative change of each coefficient is measured.
7. **Residuals and influence.** Standardised and studentised residuals (|r| > 2) and Cook's distance (threshold 4 / (n - p)).
8. **Final model.** Refit without the influential observations flagged by Cook's distance, drop the covariates with p-value above 0.1 (`GDP`, `thinness..1.19.years`), and check again normality and homoscedasticity.
9. **Prediction on a different year.** Predict life expectancy for 2000, invert the Box-Cox transformation (y = sqrt(2z + 1)), and compute MAE, MSE and RMSE. The RMSE is about 3.7 years. Predicted and actual means are compared for developed and developing countries.

## Outputs

- Pairwise relationships between the main covariates and the response.
- Diagnostic plots: residuals against fitted values, QQ-plot, histogram and boxplot of residuals, leverage, standardised and studentised residuals, Cook's distance.
- Box-Cox log-likelihood profile.
- Predicted against actual life expectancy for 2000, residuals of the predictions, and bar plots of predictions for developed and developing countries.

## How to run

**Requirements:** R with the packages `BAS`, `carData`, `car`, `corrplot`, `ellipse`, `faraway`, `GGally`, `leaps`, `MASS`, `Matrix`, `ggplot2`.

1. Put `life_expectancy.csv` in the same folder as the script.
2. Open `life_expectancy_regression.R` in RStudio. The first lines install the packages: run them once, then comment them out.
3. Run the script section by section.

## Repository content

```
life-expectancy-regression/
├── README.md
├── life_expectancy_regression.R
└── .gitignore
```

## Notes and limitations

- The model describes associations between indicators, not causal effects.
- Removing influential observations improves the fit but changes the sample; the choice involves a trade-off and should be reported.
- The prediction test uses a different year, but the same countries and the same dataset, so it is not a fully independent validation.
- Variable selection by p-values, AIC, BIC and adjusted R-squared can give different models; the final choice balances interpretation and prediction.
- The sample is limited to countries with complete records in the chosen years.
