#define	QFSEEK(afile, offset, pos, fname) \
		{if (fseek(afile, (long)(offset), pos)) \
		  error(EXIT_FAILURE,"*Error*: File positioning failed in ", \
			fname);;}
