/*
 				fitsutil.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	Specific definitions for FITS functions
*
*	Last modify:	21/12/95
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/


/*--------------------------- FITS BitPix coding ----------------------------*/

#define		BP_BYTE		8
#define		BP_SHORT	16
#define		BP_LONG		32
#define		BP_FLOAT	(-32)
#define		BP_DOUBLE	(-64)

/*--------------------------------- flags -----------------------------------*/

/* access flag within a catalog */

#define		FROM_START	0	/* operation should start from the
					beginning of the file */
#define		FROM_HERE	1	/* operation should start from the
					current location within the file */

/*-------------------------------- macros -----------------------------------*/

/* size (in bytes) of one FITS block */

#define		FBSIZE		2880L	

/* FITS size after adding padding */

#define		PADTOTAL(x)	(((x-1)/FBSIZE+1)*FBSIZE)

/* extra size to add for padding */

#define		PADEXTRA(x)	((FBSIZE - (x%FBSIZE))% FBSIZE)


