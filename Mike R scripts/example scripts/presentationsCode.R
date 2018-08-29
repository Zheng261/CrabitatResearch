############ area CDF, log-transformed  ##############
quartz()
plot(log2, main = "Cumulative distribution function of marine home ranges", xlab ="Activity space", ylab = "Percentile", xlim = c(-6,7), xaxt="n", yaxt="n", col = c("black","red","blue","forestgreen")[database$Group],pch = c(18,1,16,17)[database$Group], cex.main = 2)
axis(1, at = c(-6,-4,-2,0,2,4,6), labels = c("10 m2","1000 m2","0.01 km2","1 km2","100 km2","10,000 km2","1,000,000 km2"))
axis(2, at = c(0,0.2,0.4,0.6,0.8,1.0), labels = c(0,0.2,0.4,0.6,0.8,1.0))
abline(v = log10(4.5), col = "black", lwd = 2) # McCauley sup mats says median size is 4.5 km2
abline(v = log10(26938.40302), col = "red")  # original Palmyra
abline(v = log10(431014.4484), col = "blue")  # expanded Palmyra

abline(v = log10(360000), col = "green", lwd = 3, lty = 2)  # original NWHI
abline(v = log10(1510000), col = "purple", lwd = 2, lty = 3)  # expanded NWHI

#abline(v = log10(1551.65427), col = "green", lwd = 3, lty = 2)  # 1000 km2 MPA
#abline(v = log10(431014.4484), col = "purple", lwd = 2, lty = 3)  # draws a line for expanded 200nm radius reserve


legend("left",c("Seabirds","Fish","Marine mammals", "Marine reptiles"),pch=c(18,1,16,17),col=c("black","red","blue","forestgreen"), cex=1)
legend(x= -6.5, y = .41,c("Global median size","Territorial waters", "EEZ"),lwd=c(2,2,2),col=c("black","green","purple"),lty = c(1,2,3), cex=1)


############ area CDF, log-transformed  ############## FOR PRESENTATIONS LARGER TEXT
quartz()
plot(log2, main = "Cumulative distribution function of marine home ranges", xlab ="Activity space", ylab = "Percentile",  xaxt="n", yaxt="n", col = c("black","red","blue","forestgreen")[database$Group],pch = c(18,1,16,17)[database$Group], cex.main = 2, cex.lab = 1.5)

axis(1, at = c(-6,-4,-2,0,2,4,6), labels = c("10 m2","1000 m2","0.01 km2","1 km2","100 km2","10,000 km2","1,000,000 km2"), cex.axis = 1.5)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1.0), labels = c(0,0.2,0.4,0.6,0.8,1.0))
abline(v = log10(4.5), col = "black", lwd = 2) # McCauley sup mats says median size is 4.5 km2
abline(v = log10(1551.65427), col = "green", lwd = 3, lty = 2)  # 1000 km2 MPA
abline(v = log10(431014.4484), col = "purple", lwd = 2, lty = 3)  # draws a line for expanded 200nm radius reserve


legend("left",c("Seabirds","Fish","Marine mammals", "Marine reptiles"),pch=c(18,1,16,17),col=c("black","red","blue","forestgreen"), cex=1)
legend(x= 1, y = .38,c("Global median size","Territorial waters", "EEZ"),lwd=c(2,2,2),col=c("black","green","purple"),lty = c(1,2,3), cex=1)

