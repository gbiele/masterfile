MASTER_scores[MB3S.ADHD.SS > 22, MB3S.ADHD.SS := NA]

hmb = hist(MASTER_scores$MoBa.Q6.ADHD.SS, breaks = (0:23)-.5)
has = hist(MASTER_scores$MB3S.ADHD.SS, breaks = (0:23)-.5)

lo = matrix(c(4,1,1,
              2,3,3,
              2,3,3), ncol = 3)

layout(lo)
par(mar = c(0,2,0,0))
plot(-has$counts,has$mids,"s", bty = "n", yaxt = "n", xaxt = "n", xlab = "n")
mtext("sum score ADHD Study", side = 2)
par(mar = c(0,0,2,0))
plot(hmb$mids, hmb$counts,"s", bty = "n", yaxt = "n", xaxt = "n", xlab = "n")
mtext("sum score MoBa", side = 3)


smoothScatter(MASTER_scores$MoBa.Q6.ADHD.SS,
              MASTER_scores$MB3S.ADHD.SS,
              xaxt = "n",
              yaxt = "n",
              xlab = "", ylab = "", bty = "n")

abline(lm( MASTER_scores$MB3S.ADHD.SS  ~ MASTER_scores$MoBa.Q6.ADHD.SS), col = "red")
plot(0,"n", xlim = c(0,1), ylim = c(0,1), xaxt = "n", yaxt = "n", bty = "n")

text(.5,.5,
     paste0("r = ",
            round(cor(MASTER_scores$MoBa.Q6.ADHD.SS,
                      MASTER_scores$MB3S.ADHD.SS,
                      use = "pairwise.complete.obs"),
                  digits = 2)),
      cex = 2)