## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----message=F, warning=F-----------------------------------------------------
library(CSCNet)
library(riskRegression)
data(Melanoma)
as_tibble(Melanoma)
table(Melanoma$status)

## -----------------------------------------------------------------------------
vl <- list('1'=c('age','sex','invasion','thick'),
     
     '2'=~age+sex+epicel+ici+thick)

penfit <- penCSC(time = 'time',
                 
                 status = 'status',
                 
                 vars.list = vl,
                 
                 data = Melanoma,
                 
                 alpha.list = list('1'=0,'2'=.5),
                 
                 lambda.list = list('1'=.01,'2'=.02))

penfit

## -----------------------------------------------------------------------------
predict(penfit,Melanoma[1:5,],type='lp',event=1)

## -----------------------------------------------------------------------------
predict(penfit,Melanoma[1:5,],type='response')

## -----------------------------------------------------------------------------
predict(penfit,Melanoma[1:5,],type='absRisk',event=1,time=365*c(3,5))

## ----message=T, warning=F-----------------------------------------------------
#Writing a hypothetical pre-processing function

library(recipes)

std.fun <- function(data){

  cont_vars <- data %>% select(where(~is.numeric(.))) %>% names

  cont_vars <- cont_vars[-which(cont_vars %in% c('time','status'))]

  #External functions from recipes package are being used

  recipe(~.,data=data) %>%

    step_center(all_of(cont_vars)) %>%

    step_scale(all_of(cont_vars)) %>%

    prep(training=data) %>% juice

}

#Tuning a regularized cause-specific cox 

set.seed(455) #for reproducibility

tune_melanoma <- tune_penCSC(time = 'time',
                             
                             status = 'status',
                             
                             vars.list = vl,
                             
                             data = Melanoma,
                             
                             horizons = 365*5,
                             
                             event = 1,
                             
                             method = 'cv',
                             
                             k = 5,
                             
                             standardize = FALSE,
                             
                             metrics = 'AUC',
                             
                             alpha.grid = list('1'=0,'2'=c(.5,1)),
                             
                             preProc.fun = std.fun,
                             
                             parallel = TRUE,
                             
                             preProc.pkgs = 'recipes')

tune_melanoma$validation_result %>% arrange(desc(mean.AUC)) %>% head

tune_melanoma$final_params

tune_melanoma$final_fits


