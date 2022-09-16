###############单维双记分模型###############
library(ltm)
#读取并描述数据
data("LSAT")
descript(LSAT)
#####建立单参数模型#####
fit1 <- rasch(LSAT, constraint = cbind(length(LSAT) + 1, 1)) #LSAT为数据集
summary(fit1)
#参数解释：Dffclt为题目的难度，即模型中的b值，由结果可知，题目3和题目2的难度最高。Dscrmn为题目的区分度，即为模型中的α值。P(x=1|z=0)为各个被测试者答对的可能性。
#####建立双参数模型#####
fit2<-ltm(LSAT~z1,IRT.param = T) 
#data为我们使用的数据集，z1为设置双参数模型必须的设置，IRT.param表示项目反应理论的参数。
summary(fit2)
#查看结果
coef(fit2)
#三参数模型
fit3<-tpm(LSAT,type = "latent.trait",IRT.param = T)
#type为需要进行分析的类型
summary(fit3)
######分析结果—绘图######
#项目的特征曲线
plot(fit2, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
#项目的信息曲线
plot(fit2, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
#包括所有项目的信息曲线
plot(fit2, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)