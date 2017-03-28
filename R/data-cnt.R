#
# data-cnt.R, 26 Mar 17

source("ESEUR_config.r")


plot_wide()

pal_col=rainbow(4)

# bench=read.csv(paste0(ESEUR_dir, "data-detective/data-cnt.csv"), as.is=TRUE)
bench=read.csv(paste0(ESEUR_dir, "data-cnt.csv"), as.is=TRUE)

bench=bench[order(bench$year), ]

email=subset(bench, location == "email")
website=subset(bench, location == "website")
paper=subset(bench, location == "paper")
extracted=subset(bench, location == "extracted")
none=subset(bench, is.na(location))

plot(website$year, website$count, type="b", col=pal_col[1],
	cex=1.5, cex.lab=1.5, lty=1.3,
	xlim=c(1970, 2017),
	xlab="Year", ylab="Total")
points(email$year, email$count, type="b", col=pal_col[2])
points(paper$year, paper$count, type="b", col=pal_col[3])
points(extracted$year, extracted$count, type="b", col=pal_col[4])

legend(x="top", legend=c("Website", "Email", "Paper", "Extracted"), bty="n", fill=pal_col, cex=1.5)

# all=subset(bench, !is.na(location))
# sum(all$count)

