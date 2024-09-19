#
# Hech21a-Kotlin-fishing.R, 17 Sep 24
#
# Data from:
# Quantifying the adoption of Kotlin on Android stores: Insight from the bytecode
# Geoffrey Hecht and Alexandre Bergel

library("colorspace")
library("plyr")


par(bty="l")
par(las=1)
par(pch="+")
pal_col=rainbow(4)


year_fit=function(year)
{
mid_year=as.Date(paste0(year, "-06-15")) # predict middle of year

pred=predict(jk_mod, newdata=data.frame(java=j_range, dex_date=mid_year))
lines(j_range, exp(pred), col=pal_col[year-2016], lwd=1.5)
}

jk=read.csv("../Hech21a-Kotlin.csv")

jk$kot_folder=(jk$kot_folder == "True")
jk$dex_date=as.Date(jk$dex_date, format="%Y-%m-%d")
start_date=min(jk$dex_date)

jk$year=as.numeric(format(jk$dex_date, "%Y"))
jk$days=as.numeric(jk$dex_date-start_date)
jk$day30=trunc(jk$days/30)

plot(jk$java, jk$kotlin, log="xy", col=pal_col[jk$year-2016], cex.lab=1.3,
	xlim=c(1e1, 1e5),
	xlab="Java classes", ylab="Kotlin classes")

jk_mod=glm(log(kotlin) ~ log(java)+dex_date, data=jk, subset=(kotlin > 0))
summary(jk_mod)

j_range=10^(1:5)
a_ply(2017:2020, .mar=1, year_fit)

legend("topleft", legend=c(2020:2017), cex=1.2,
                        fill=rev(pal_col), border="white", bty="n")

dev.copy(dev=jpeg, file="kotlin-java-classes.jpg")
dev.off()


plot(jk$dex_date, jk$kotlin/jk$java)

kotlin_month=ddply(jk, .(day30), function(df) sum(df$kot_folder)/nrow(df))

plot(30*kotlin_month$day30, kotlin_month$V1, col="red", cex=1.3, cex.lab=1.3,
	xlab="Release date (days since 2017-01-01)", ylab="Uses Kotlin (fraction of packages in bin)")

dev.copy(dev=jpeg, file="package-uses-kotlin.jpg")
dev.off()



