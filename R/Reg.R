#' @title Realiza resúmen de un modelo de regresión lineal multiple
#' @description realiza los test
#' @param x modelo de regresion
#' @return outputs con test de hipotesis
#' @export normalHist
#' @examples
#' u=rnorm(1000,100,12)
#' normalHist(u);
#' normalHist(u,dens=TRUE)
#' normalHist(u,dens=TRUE,col="lightcyan")
TestRegresion = function(x){
    normalidad = shapiro.test(x$residuals)
    homocedas = lmtest::bptest(x)
    VIF = car::vif(x)
    test = c(normalidad, homocedas, VIF)
    for (i in 1:length(VIF)){
        if (i > 6){
            return(FALSE)
        }
    }
    texto = c("Valor P normalidad","Valor P homocedasticidad", "FALSE")
    test2 = cbind(test,texto)
    return(test2)
}
