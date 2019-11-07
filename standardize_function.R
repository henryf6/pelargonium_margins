## ******************************************************************** ##
## standardize_function.R
##
## Author: Henry Frye
## Date Created: 2015-05-13
##
## Purpose:
## Create function that standardizes columns (i.e. vectors) for shrinkage
## regressions.
## ******************************************************************** ##

standardize <- function(x) {
  new_vector <- 0
 for (i in 1:length(x)) {
    if (is.na(x[i]) == FALSE) {
   new_vector[i] <- ( x[i] / sd(x, na.rm =TRUE))
  } else new_vector[i] = NA
  }
return(new_vector)
  }




Both forward and bi-directional stepwise approaches conclude the same model with the least amount of variables. Though the $R^2$ value of the backward stepwise model is slightly higher than other two approaches, the forward/bi-directional stepwise approach is interpreted due to its parsimony in variable selection (i.e. it has less variables.)

Printed below are the variance inflation factors (VIF) for the favored forward/bi-directional stepwise model. The VIF values for the bio13 and bio16 climate variables are quite high with values over 100. Their collinearity is not surprising given that bio13 is the precipitation of the wettest month and that bio16 is the precipitation of the wettest quarter.  The wettest month of the year would be expected to occur also in the wettest quarter.  Furthermore, the VIF values for the categorical variables for growth form also have VIF values (i.e., greater than 10). The effect of whether or not a plant is a woody shrub or woody subshrub probably is quite similar.  Thus the categorial variable of woody subshrub may be a candidate of being dropped. 

```{r forward vif, echo  = FALSE, warning = FALSE, message= FALSE}
#vif(forward.mod)
```

Whether or not bio16 and the dummy variable coding for woody subshrub were tested if they could be dropped from the model in a general linear hypothesis test.  Formally stated, $H_0: \beta_8=\beta_{10}=0$.

```{r forward reduced vif, echo = FALSE, warning= FALSE, message = FALSE}
for_red.mod <- lm(formula = Number_of_Teeth._Perimeter ~ geophyte_y + annual_y + 
                    clade_A_y + bio11 + bio9 + scandent_y + bio13  + 
                    herbaceous_perennial_y + woody_subshrub_y + woody_shrub_y, 
                  data = margin_clim[, c(34, 61:88)])
vif(for_red.mod)
```


Adjusted $R^2$ = .3713 which is still quite high.

Test again to remove woody subshrub. $R^2$ = .337.  Reasonably woody subshrub and woody shrub are measring the same variable: whether something is woody or not. Thus it is expected that this should be dropped. 
```{r}
for_red2.mod <- lm(formula = Number_of_Teeth._Perimeter ~ geophyte_y + annual_y + 
                     clade_A_y + bio11 + bio9 + scandent_y + bio13  + 
                     herbaceous_perennial_y + woody_shrub_y, 
                   data = margin_clim[, c(34, 61:88)])
vif(for_red2.mod)
```

This results in a model with bullshit bullshit bullshit.


Move this to appendix:
  
  
  Using the leaps [@lumley_leaps_2017] R package, a best subset procedure was also used. This method searches through all possible models efficiently _(see description of algorithm in James et al 2013 text)_.  The following figure is a barcode chart of the best selected variables and the $R^2$ of the model on the y-axis. 






As suggested by the pair plot below, a full model with all the included variables may present problems with multicollinearity between the predictors.  A shrinkage method would be appropriate here to minimize the undue influence of collinear predictors.  A LASSO regression is chosen here as our goal is more in line with model interpretation rather than predictability.
```{r pairplot, echo = FALSE, message = FALSE, warning = FALSE}
ggscatmat(margin_clim, columns = c('Lam_area', 'Tooth_Area', 'Number_of_Teeth','Number_of_Teeth._Perimeter','Compactness','Tooth_Area._Blade_Area','Tooth_Area._Perimeter','Tooth_Area._Interior_Perimeter') , color = "Major_clade")
```


```{r lasso}
margin_clim_lasso <- margin_clim[,c(34,62:88)] %>% na.omit()
grid = 10^seq(10,-2,length=100)
x <-  margin_clim_lasso[,2:28] %>% as.matrix()
y <-  margin_clim_lasso[,1] %>% as.matrix()
lasso.mod = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x, y, alpha = 1)
plot(cv.out)
bestlam=cv.out$lambda.min

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef[lasso.coef!=0]


best.lasso.mod <- lm(Number_of_Teeth._Perimeter~ bio1 + bio3 + bio4 + bio9 + bio15 + bio16 + bio17 + bio18, data = margin_clim )
summary(best.lasso.mod)
Anova(best.lasso.mod, type = 3)
```


Running a vif on the best model from the stepwise regression. There are some strong signs of multicollinearity VIF > 10 .

```{r}
vif(best.lasso.mod)
```





