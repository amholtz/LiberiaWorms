ggvario <- function(coords, data, bins = 15, color = "royalblue1") {
  empvario <- variog(coords = coords, data = data, uvec = seq(0, max(dist(coords))/3, l = bins), messages = F)
  envmc <- variog.mc.env(geodata = as.geodata(cbind(coords, data)), obj.variog = empvario, nsim = 99, messages = F)
  dfvario <- data.frame(distance = empvario$u, empirical = empvario$v,
                        lowemp = envmc$v.lower, upemp = envmc$v.upper)
  ggplot(dfvario, aes(x = distance)) +
    geom_ribbon(aes(ymin = lowemp, ymax = upemp), fill = color, alpha = .3) +
    geom_point(aes(y = empirical), col = "black", fill = color, shape = 21, size = 3) +
    xlab("distance") +
    scale_y_continuous(name = "semivariance", breaks = seq(0, max(dfvario$upemp), .5), 
                       limits = c(0, max(dfvario$upemp))) +
    ggtitle("Empirical semivariogram") +
    theme_classic()  
}
