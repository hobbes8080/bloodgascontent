

## plot blood O2 and CO2 content as function of partial pressure

if(!require(ggplot2)){
	install.packages("ggplot2")
	library(ggplot2)
}

## plot functions

## define o2 function(ref:1. Siggaard-Andersen O, Wimberley PD, Göthgen I, Siggaard-Andersen M. A mathematical model of the hemoglobin-oxygen dissociation curve of human blood and of the oxygen partial pressure as a function of temperature. Clin. Chem. 1984; 30: 1646–1651.)
SatO2 <- function(x, pH=7.4, T=37, cbase=0, cdpg=1.5, y0=1.875, k=0.5343){
    ## x is PO2
    x <- log(x)
    a <- 1.04*(7.4-pH)+0.005*cbase+0.07*(cdpg-5)
    b <- 0.055*(T/(273.15-(-310.15)))
    h <- 3.5+a
    x0 <- 1.946+a+b
    y <- y0+x-x0+h*tanh(k*(x-x0))
    return(exp(y)/(exp(y)+1))
}
ConcO2 <- function(x, Hb=150, pH=7.4, T=37, cbase=0, cdpg=1.5, y0=1.875, k=0.5343){
    ## x is PO2
    x <- log(x)
    a <- 1.04*(7.4-pH)+0.005*cbase+0.07*(cdpg-5)
    b <- 0.055*(T/(273.15-(-310.15)))
    h <- 3.5+a
    x0 <- 1.946+a+b
    y <- y0+x-x0+h*tanh(k*(x-x0))
    return(exp(y)/(exp(y)+1)*1.34*Hb)
}
## define co2 function (ref: 1. Kelman GR. Digital computer procedure for the conversion of PCO2 into blood CO2 content. Respir Physiol 1967; 3: 111–115.)
ConcCO2 <- function(x, pH=7.4, SatO2=0.96, Hct=0.4, T=37){
    ## x is PCO2
    ## convert x from kPa to mmHg
    x <- x*7.50061682704
    alpha <- 0.0307+0.00057*(37-T)+0.00002*(37-T)^2
    pK <- 6.086+0.042*(7.4-pH)+(38-T)*(0.0047+0.0014*(7.4-pH))
    co2plasma <- alpha*x*(1+10^(pH-pK))
    dred <- 0.664+0.2275*(7.4-pH)-0.0938*(7.4-pH)^2
    doxy <- 0.590+0.2913*(7.4-pH)-0.0844*(7.4-pH)^2
    d <- doxy+(dred-doxy)*(1-SatO2)
    co2cells <- d*co2plasma
    return((Hct*co2cells+(1-Hct)*co2plasma)*2.22*10)
}

## -----------------------------
## plot curves
## -----------------------------
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p <- p + stat_function(fun = ConcO2, aes(linetype="line.1"))
p <- p + stat_function(fun = ConcCO2, aes(linetype="line.2"))
p <- p + scale_linetype_manual(name = "", values = c("longdash", "solid"), labels = c("Oxygen", "Carbon dioxide"))
p <- p + xlab(expression(paste("",O[2], " and ",CO[2], " partial pressure [kPa]")))
p <- p + ylab(expression(paste("",O[2], " and ",CO[2], " content [ml/l]")))
p <- p + xlim(0, 11) + ylim(0, 500)
print(p)
ggsave("FIGURE2.pdf", p)

## -----------------------------
## plot curves with pH range
## -----------------------------
ylimmax <- 500
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
## plot O2 for normal pH
p <- p + stat_function(fun = ConcO2, aes(linetype="line.1", color="col.1"))
## plot O2 for pH range
p <- p + stat_function(fun = ConcO2, args=list(Hb=150, pH=7.6, T=37, cbase=0, cdpg=1.5), aes(linetype="line.1", color="col.2"))
p <- p + stat_function(fun = ConcO2, args=list(Hb=150, pH=7.2, T=37, cbase=0, cdpg=1.5), aes(linetype="line.1", color="col.2"))
## plot CO2 for normal pH
p <- p + stat_function(fun = ConcCO2, aes(linetype="line.2", color="col.1"))
## plot CO2 for pH range
p <- p + stat_function(fun = ConcCO2, args = list(pH=7.6, SatO2=0.96, Hct=0.4, T=37), aes(color="col.2"))
p <- p + stat_function(fun = ConcCO2, args = list(pH=7.2, SatO2=0.96, Hct=0.4, T=37), aes(color="col.2"))
p <- p + xlim(0, 11) + ylim(0, ylimmax)
## render function points to data.frame in order to use it for the ribbons
prender <- ggplot_build(p)
o2 <- data.frame(x=prender$data[[2]]$x, ymin=prender$data[[2]]$y, ymax=prender$data[[3]]$y)
co2 <- data.frame(x=prender$data[[5]]$x, ymin=prender$data[[5]]$y, ymax=prender$data[[6]]$y)
## handle case that ribbon extends beyond upper limit of plot
co2$ymax[is.na(co2$ymax)] <- ylimmax
co2$ymin[is.na(co2$ymin)] <- ylimmax
## plot ribbons
p <- p + geom_ribbon(data=o2, aes(x=x, ymin=ymin, ymax=ymax), alpha=0.1)
p <- p + geom_ribbon(data=co2, aes(x=x, ymin=ymin, ymax=ymax), alpha=0.1)
## plot ribbon directly (without rendering the plot beforehand) is only possible with a fixed lower boundary)
## p <- p + stat_function(fun = ConcCO2, args = list(pH=7.6, SatO2=0.96, Hct=0.4, T=37), geom="ribbon", aes(color="col.1", ymin=3, ymax=..y..))
p <- p + scale_linetype_manual(name = "", values = c("longdash", "solid"), labels = c("Oxygen", "Carbon dioxide"))
p <- p + scale_color_manual(name = "", values = c("black", "grey"), labels = c("pH 7.4", "pH 7.6 - 7.2"))
p <- p + xlab(expression(paste("",O[2], " and ",CO[2], " partial pressure [kPa]")))
p <- p + ylab(expression(paste("",O[2], " and ",CO[2], " content [ml/l]")))
print(p)
ggsave("FIGURE2b.pdf", p)
ggsave("FIGURE2b.png", p)
