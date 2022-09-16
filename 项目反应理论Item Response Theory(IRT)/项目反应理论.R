###########单维多记分模型##########
library(ltm)
data("Environment")
descript(Environment)
#计算肯德尔相关系数
rcor.test(Environment, method = "kendall")
##建立模型
fit4 <- grm(Environment, constrained = TRUE)
coef(fit4, prob = TRUE, order = TRUE)
##分析结果
par(mfrow = c(2, 2))
plot(fit4, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
plot(fit4, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
plot(fit4, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
plot(fit4, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5,
     cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3,
     cex.axis = 1.1)
for (ctg in 2:3) 
   plot(fit4, category = ctg, lwd = 2, cex = 1.2, annot = FALSE,xlab = "Latent Trait",
        cex.main = 1.5, cex.lab = 1.3,cex.axis = 1.1)