#' Title
#'
#' @field formula formula.
#' @field data data.frame.
#' @field X matrix.
#' @field Regressions_coefficients vector.
#' @field The_fitted_values matrix.
#' @field The_residuals matrix.
#' @field The_degrees_of_freedom numeric.
#' @field The_residual_variance numeric.
#' @field The_variance_of_the_regression_coefficients matrix.
#' @field The_t_values matrix.
#'
#' @return
#' @export
#'
#' @examples

library(ggplot2)
library(gridExtra)
linreg <-setRefClass( Class = "linreg",
                      fields= list(formula= "formula",
                                   data= "data.frame",
                                   dta= "character",
                                   frmla= "formula",
                                   X= "matrix",
                                   y= "matrix",
                                   Regressions_coefficients = "matrix",
                                   The_fitted_values = "matrix",
                                   The_residuals= "matrix",
                                   The_degrees_of_freedom= "numeric",
                                   The_residual_variance = "numeric",
                                   The_variance_of_the_regression_coefficients= "matrix",
                                   The_t_values= "matrix"

                      ),

                      methods=list(
                        initialize= function(formula,data){
                          X <<- model.matrix(formula, data)
                          y <<- as.matrix(data[all.vars(formula)[1]])
                          stopifnot (class(formula) == "formula")
                          stopifnot (class(data) == "data.frame")
                          frmla<<- formula
                          dta <<- deparse(substitute(data))



                          #Regressions coefficients:
                          Regressions_coefficients<<-solve(t(X) %*% X) %*% t(X) %*% y


                          #The fitted values:
                          The_fitted_values<<-X %*% Regressions_coefficients



                          #The residuals:
                          The_residuals<<- y -The_fitted_values


                          #The degrees of freedom:
                          The_degrees_of_freedom<<- nrow(X)-ncol(X)


                          #The residual variance:****(numeric or vector?)
                          The_residual_variance<<-as.numeric((t(The_residuals) %*% The_residuals) / The_degrees_of_freedom)


                          #The variance of the regression coefficients:
                          The_variance_of_the_regression_coefficients<<-  The_residual_variance * solve(t(X) %*% X)

                          #t-values for each coefficient:
                          The_t_values <<- Regressions_coefficients / sqrt(diag(The_variance_of_the_regression_coefficients))

                          #P-values
                          #p_values <<- 2 * pt(abs(The_t_values), The_degrees_of_freedom, lower.tail = FALSE)




                        },
                        print = function(){
                          cat(paste("linreg(formula = ", format(frmla), ", data = ", dta , ")\n\n ", sep = ""))
                          setNames(round(Regressions_coefficients[1:nrow(Regressions_coefficients)],3),rownames(Regressions_coefficients))
                        },
                        ploting=function(){

                          p1<<-ggplot()+
                            #geom_line(data=data,aes(x=all.vars(formula)[2],y=all.vars(formula)[1]),color="red")+
                            geom_point(data=data,aes(x=The_fitted_values,y=The_residuals),shape=1)+
                            ylab("Residuals")+xlab("Fitted values")

                          p2<<-ggplot()+
                            #geom_line(data=data,aes(x=,y=),color="red")+
                            geom_point(data=data,aes(x=The_fitted_values,y=The_residual_variance),shape=1)+
                            ylab("sqrt(|Standardized residual|)")+xlab("Fitted values")


                          return(grid.arrange(p1, p2, ncol = 1))
                        }


                      )

)

linreg_mod <- linreg$new(Petal.Length~Species, data = iris)
linreg_mod$print()
linreg_mod$ploting()
