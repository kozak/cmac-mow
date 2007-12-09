require(bitops);

hash <- function(virtual, aunum, pbits) {
	c1 = c(0xbaa96887, 0x1e17d32c, 0x03bcdc3c, 0x0f33d1b2)
	c2 = c(0x4b0f3b58, 0xe874f0c3, 0x6955c5a6, 0x55a7ca46);
	itmph <- 0; 
	itmpl <- 0;
	for (i in 1:4) {
		iswap <- aunum;
		cat("iswap ", iswap , "\n");
		ia <- bitXor((iswap), c1[i]);
		cat("ia ", ia , "\n");
		itmpl <- bitAnd(ia, 0xffff);
		cat("itmpl ", itmpl , "\n");
		itmph <- bitShiftR(ia, 16);
		cat("itmph ", itmph , "\n");

		itmplsq <- (itmpl * itmpl) %% (2^32);
		cat("itmplsq ", itmplsq, "\n")

		itmphsq <- (itmph * itmph) %% (2^32);
		cat("itmphsq ", itmphsq, "\n")
		
		itmphsqn <- bitFlip(itmphsq)
		cat("itmphsqn ", itmphsqn, "\n")
		
		ib <- (itmplsq + itmphsqn) %% (2^32)
		cat("ib ", ib , "\n");
		ia <- bitOr(bitShiftR(ib, 16), bitShiftL(bitAnd(ib, 0xffff), 16));
		cat("ia ", ia , "\n");
		temp <- bitXor(ia, c2[i]);
		cat("temp ", temp , "\n");
		aunum <- bitXor(virtual, (temp + itmpl*itmph) %% (2^32));
		cat("aunum ", aunum, "\n");
		virtual <- iswap;
		cat("virtual ", virtual , "\n");
		
		cat(" ************ ITER ", i, "***************\n");  

	}

	dupa <- bitAnd(virtual, bitShiftL(1, pbits) - 1);
}

ccc <- hash(1,2,3)