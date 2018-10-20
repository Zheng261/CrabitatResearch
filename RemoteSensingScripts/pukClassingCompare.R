pukOne <- brick("10-8-OneIslePukaruaClassed.tif")
pukMulti <- brick("8.28-SMPukaruaClassed.tif")
names(pukOne) <- "Classed"
names(pukMulti) <- "Classed"

corrPukMulti <- pukMulti[which(!is.na(values(pukOne)))]
pts <- extract(corrPukMulti,)
