require(bitops);

hash = function(virtual, aunum, pbits) {
	c1 = c(0xbaa96887, 0x1e17d32c, 0x03bcdc3c, 0x0f33d1b2)
	c2 = c(0x4b0f3b58, 0xe874f0c3, 0x6955c5a6, 0x55a7ca46);
	itmph = 0; 
	itmpl = 0;
	for (i in 1:4) {
		iswap = aunum;

		ia = bitXor((iswap), c1[i]);

		itmpl = bitAnd(ia, 0xffff);

		itmph = bitShiftR(ia, 16);


		itmplsq = (itmpl * itmpl) %% (2^32);


		itmphsq = (itmph * itmph) %% (2^32);

		
		itmphsqn = bitFlip(itmphsq)

		
		ib = (itmplsq + itmphsqn) %% (2^32)

		ia = bitOr(bitShiftR(ib, 16), bitShiftL(bitAnd(ib, 0xffff), 16));

		temp = bitXor(ia, c2[i]);

		aunum = bitXor(virtual, (temp + itmpl*itmph) %% (2^32));

		virtual = iswap;

		


	}

	bitAnd(virtual, bitShiftL(1, pbits) - 1)
}
