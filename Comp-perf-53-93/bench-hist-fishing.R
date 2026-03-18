#
# bench-hist-fishing.R, 18 Mar 26
#
#   original 29 Oct 23
#
# Data from:
# Early Knight data
# https://shape-of-code.com/2016/04/30/costperformance-analysis-of-1944-1967-computers-knights-data/
# 1985 Knight data
# A functional and structural measurement of technology
# by Kenneth Knight
# ATTRIBUTES OF THE PERFORMANCE OF CENTRAL PROCESSING UNITS: A RELATIVE PERFORMANCE PREDICTION MODEL
# by PHILLIP EIN-DOR and JACOB FELDMESSER
# KOP data
# TRACKING THE ELUSIVE KOPS
# by Edward J. Lias
# Whetstone data
# http://www.roylongbottom.org.uk/whetstone.htm
# Roy Longbottom's website
# Dhrystone data
# https://netlib.org/performance/html/dhrystone.data.col0.html
# Alfred Aburto / Naval Ocean Systems Center, San Diego / aburto@nosc.mil


library("colorspace")
library("lme4")
library("plyr")
library("deming")

par(bty="l")
par(las=1)
par(pch="+")
pal_col=rainbow(2)

# The deming function does not support a predict function :-O
# So roll our own
deming_log_pred=function(dmod, xvals)
{
pred=coef(dmod)[1] + coef(dmod)[2]*log(xvals)

return(pred)
}


# Map SPEC Result column using a fitted model
map_Result=function(res_mod, Result)
{
pred=exp(deming_log_pred(res_mod, Result))
return(pred)
}



k50=read.csv("../Benchmark-historical/knight1950s.csv", as.is=TRUE)
k50$Bench="Knight-50"
k50$metric=k50$Sci.ops.sec
k50$Year=as.numeric(substring(k50$Date.introduced, nchar(k50$Date.introduced)-3))
k50$Date=as.Date(paste0("01 ", k50$Date.introduced), format="%d %b %Y")
k60=read.csv("../Benchmark-historical/knight1960s.csv", as.is=TRUE)
k60$Bench="Knight-60"
k60$metric=k60$Sci.ops.sec
k60$Year=1900+as.numeric(substring(k60$Date.introduced, nchar(k60$Date.introduced)-1))
# Cannot use %y because year is before 1970
# Build full year value
k60$Date=as.Date(paste0("01/", substr(k60$Date.introduced, 1, 2), k60$Year), format="%d/%m/%Y")
k60$Sci.sec.dol=NA  # to match the k50 data

k5060=rbind(k50, k60)
k5060=subset(k5060, !duplicated(System))  # Merge the Knight 50/60 datasets
k5060$Bench="Knight-5060"

k85=read.csv("../Benchmark-historical/knight1985.csv", as.is=TRUE)
k85$Bench="Knight-85"
k85$metric=k85$op.sec
k85$Year=1900+as.numeric(substring(k85$Date, nchar(k85$Date)-1))
kop=read.csv("../Benchmark-historical/mainframe-bench-1980_KOPS.csv", as.is=TRUE)
kop$Bench="KOPS"
kop$metric=kop$KOPS
ein=read.csv("../Benchmark-historical/ein-dor.csv", as.is=TRUE)
ein$Bench="ein-dor"
ein$metric=ein$performance
whet=read.csv("../Benchmark-historical/WhetRoyLongbottom.csv", as.is=TRUE)
whet$Bench="Whetstone"
whet$metric=whet$MWIPS
whet$Year=whet$Year.intro
# No date in Whetstone csv
dhry=read.csv("../Benchmark-historical/drystone.csv", as.is=TRUE)
dhry$Bench="Dhrystone"
dhry$metric=dhry$V2.1
npl=read.csv("../Benchmark-historical/NPL-unpub.csv", as.is=TRUE)
npl$Bench="NPL"
# npl$metric=npl$???

k85op=merge(k85, kop, all=TRUE)
ke=merge(k85op, ein, all=TRUE)
kwhet=merge(ke, whet, all=TRUE)
kdhry=merge(kwhet, dhry, all=TRUE) # Not included in the analysis

k56ll=merge(kwhet, k5060, all=TRUE)

Sall=k56ll

# sort(Sall$System)

# kdhry=merge(Sall, dhry, all=TRUE)
# sort(kdhry$System)


# Plot paired benchmark results for the same System
s85op=merge(k85, kop, by="System", all=TRUE)
se=merge(s85op, ein, by="System", all=TRUE)
swhet=merge(se, whet, by="System", all=TRUE)
sdhry=merge(swhet, dhry, by="System", all=TRUE) # Not included in the analysis

s56ll=merge(swhet, k5060, by="System", all=TRUE)

sys_all=s56ll

plot(~ log(Sci.ops.sec)+log(op.sec)+log(KOPS)+log(performance)+log(MWIPS), data=sys_all, cex=1.4)


# Plots for talk: Computer performance 1953-1993

pal_col=rainbow(2)

# Knight's two sets of measurements

plot(k5060$Date, k5060$Sci.ops.sec, log="y", col=pal_col[2],
	xlab="Date", ylab="Ops per sec (scientific)")

k56_mod=glm(log(Sci.ops.sec) ~ Date, data=k5060)
summary(k56_mod)

d1953=as.Date("1953-01-01")
d1967=as.Date("1967-12-31")
xbounds=c(d1953, d1967)

pred=predict(k56_mod, newdata=data.frame(Date=xbounds))
lines(xbounds, exp(pred), col=pal_col[1])

dev.copy(dev=png, file="knight-5060-Sci.png")
dev.off()


# Whetstone measurements

whet_for=subset(whet, Lang == "For") # Use Fortran version

plot(whet_for$Year, whet_for$MWIPS, log="y", col=pal_col[2],
	xlab="Date", ylab="Whetstone (MWIPS)")

whet_mod=glm(log(MWIPS) ~ Year, data=whet_for)
summary(whet_mod)

# d1963=as.Date("1963-01-01")
# d1997=as.Date("1997-12-31")
xbounds=1963:1997

pred=predict(whet_mod, newdata=data.frame(Year=xbounds))
lines(xbounds, exp(pred), col=pal_col[1])

dev.copy(dev=png, file="whetstone_6397.png")
dev.off()

K_W=merge(k5060, whet, by="System")
K_W=subset(K_W, Lang == "For") # Keep the Fortran values

plot(K_W$Sci.ops.sec, K_W$MWIPS, log="xy", cex=1.2, col=pal_col[2],
 	xlab="Knight", ylab="Whetstone")

# Use deming regression to handle errors in boths sets of measurements
KW_mod=deming(log(MWIPS) ~ log(Sci.ops.sec), data=K_W)
coef(KW_mod)
# summary(KW_mod)

xbounds=exp(seq(6.5, 16.5, by=0.1))
pred=deming_log_pred(KW_mod, xbounds)

lines(xbounds, exp(pred), col=pal_col[1])

dev.copy(dev=png, file="Knight-vs-Whetstone.png")
dev.off()


#

pal_col=rainbow(3)

k5060$K_as_Whet=map_Result(KW_mod, k5060$Sci.ops.sec)

plot(k5060$Year, k5060$K_as_Whet, log="y", col=pal_col[2],
	xlim=c(1950, 1993), ylim=range(c(k5060$K_as_Whet, whet$MWIPS), na.rm=TRUE),
	xlab="Year", ylab="Whetstones (MWIPS)")
points(whet_for$Year, whet_for$MWIPS, col=pal_col[3])

kw_all=data.frame(Year=c(k5060$Year, whet_for$Year),
			MWIPS=c(k5060$K_as_Whet, whet_for$MWIPS)
			)

s_mod=loess.smooth(kw_all$Year, log(kw_all$MWIPS), span=0.3)
lines(s_mod$x, exp(s_mod$y), col=pal_col[1], lwd=1.2)


legend("bottomright", legend=c("Knight", "Whetstone"), cex=1.2,
                        fill=pal_col[-1], border="white", bty="n")

dev.copy(dev=png, file="Knight-Whetstone_5393.png")
dev.off()


# # Model pairs of benchmark results

# kops_perf_mod=glm(log(KOPS) ~ log(performance), data=Sall)
# summary(kops_perf_mod)
# 
# perf_mod=glm(log(performance) ~ log(1/MCYT)+CACH, data=Sall)
# summary(perf_mod)
# 
# MWIPS_Sci_mod=glm(log(MWIPS) ~ log(Sci.ops.sec), data=Sall)
# summary(MWIPS_Sci_mod)
# 
# MWIPS_KOPS_mod=glm(log(MWIPS) ~ log(KOPS), data=Sall)
# summary(MWIPS_KOPS_mod)
# 
# MWIPS_mod=glm(log(MWIPS) ~ log(Clock.MHz)+Lang, data=whet)
# summary(MWIPS_mod)

# Remove non-Fortran results from Whetstone data
Sall_clean=subset(Sall, is.na(Lang) | (Lang == "For"))

nrow(Sall_clean)
length(unique(Sall_clean$System))

# ben_mod=lmer(log(metric) ~ System+(System|Bench), data=Sall)
ben_mod=lmer(log(metric) ~ (1|System)+(1|Bench), data=Sall_clean)
summary(ben_mod)

# The Intercept values are relative performance values for
# System and Bench
# ranef(ben_mod)

relperf=exp(ranef(ben_mod)$System) # un-log performance result
relperf$System=rownames(relperf)
relperf$relperf=relperf$'(Intercept)'

t=merge(relperf, Sall_clean, all=TRUE)

pal_col=rainbow(1+length(unique(t$Bench)))
t$col_str=mapvalues(t$Bench, unique(t$Bench), pal_col[-1])

plot(t$Year, t$relperf, log="y", col=t$col_str,
	ylim=c(1e-4, 1e3),
	xlab="Year introduced", ylab="Performance (relative)")

xbounds=1960:2000

rp_mod=glm(log(relperf) ~ Year, data=t)
summary(rp_mod)

pred=predict(rp_mod, newdata=data.frame(Year=xbounds))
lines(xbounds, exp(pred), col=pal_col[1])

