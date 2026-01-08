## ----include = FALSE----------------------------------------------------------
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
#A pre-processing function that removes (near) zero-variance predictors

library('collinear')

zvr.fun <- function(data){

  zv_vars <- identify_zero_variance_variables(df = data,responses = c('time','status'))

  return(data %>% select(-all_of(zv_vars)))

}

#Tuning a regularized cause-specific Cox model

tune_melanoma <- tune_penCSC(time = 'time',
                             
                             status = 'status',
                             
                             vars.list = vl,
                             
                             data = Melanoma,
                             
                             horizons = 365*3,
                             
                             event = 1,
                             
                             method = 'cv',
                             
                             k = 3,
                             
                             metrics = 'AUC',
                             
                             alpha.grid = list('1'=0,'2'=c(.5,1)),
                             
                             preProc.fun = zvr.fun,
                             
                             parallel = TRUE,
                             
                             preProc.pkgs = 'collinear')

tune_melanoma$validation_result %>% arrange(desc(mean.AUC)) %>% head

tune_melanoma$final_params

tune_melanoma$final_fits


