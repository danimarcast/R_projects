dev.new()
pairs(iris[1:4], main="Anderson's Iris Data -- 3 species",
      pch=21, bg=c("red", "green3", "blue")[iris$Species],
      oma=c(4, 4, 8, 15))
par(xpd=TRUE)
legend(0.7, 0.7, as.vector(unique(iris$Species)), bty='n', fill=c("red", "green3", "blue"))
       
