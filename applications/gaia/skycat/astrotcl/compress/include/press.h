/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	press/h/press.h
*
*   Purpose:
*	Public header file for the compress routines.
*	Whatever
*
*   Date		: Feb 23, 1993
*
*   SCCS data		: @(#)
*	Module Name	: press.h
*	Version Number	: 1.11
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/06/06 JSD  :	Changed extern definitions.
*	97/07/02 SEC  :	Mod'ed func prototypes for up-to-ANSI standard. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

/*
 *  Status codes.
 */

#define	PR_HCOMP_INFO	2	/* Print h-compress information.	*/
#define	PR_HDECOMP_INFO	1	/* Print h-decompress information.	*/

#define	PR_SUCCESS	0

#define	PR_E_BITPLANE	(-1)	/* Bad bit plane in decoded data.	*/
#define	PR_E_BITS	(-2)	/* Code size is larger than 16 bits.	*/
#define	PR_E_BLOCK	(-3)	/* Bad block type in gzip data.		*/
#define	PR_E_CODE	(-4)	/* Bad code during decode.		*/
#define	PR_E_CRC	(-5)	/* CRC does not match data.		*/
#define	PR_E_DATA	(-6)	/* Data had bad format.			*/
#define	PR_E_EOI	(-7)	/* End of input stream reached.		*/
#define	PR_E_FITS	(-8)	/* Bad fits header format.		*/
#define	PR_E_FITS_DIM	(-9)	/* Dimension of image is not 2.		*/
#define	PR_E_FITS_GRP	(-10)	/* Fits file contains groups.		*/
#define	PR_E_FITS_INC	(-11)	/* Fits header was incomplete.		*/
#define	PR_E_FITS_TYPE	(-12)	/* Fits image data type is not INTEGER*2*/
#define	PR_E_FORMAT	(-13)	/* Data has a  fromat.			*/
#define	PR_E_INC_LIT	(-14)	/* Incomplete literal set.		*/
#define	PR_E_IO		(-15)	/* Error durring io.			*/
#define	PR_E_MAGIC	(-16)	/* Magic number was not found.		*/
#define	PR_E_MEMORY	(-17)	/* Memory allocation failure.		*/
#define	PR_E_METHOD	(-18)	/* Compression method is unknown.	*/
#define	PR_E_NAXIS	(-19)	/* Image dimensions not in fits header.	*/
#define	PR_E_NO_SAVE	(-20)	/* Compressed data got larger.		*/
#define	PR_E_SIZE	(-21)	/* File size is wrong.			*/
#define	PR_E_UNSUPPORT  (-22)	/* Unsupported compression algorithm.	*/


/*
 *  Compression types supported.
 */

#define	PR_UNIX		"UCMP"
#define	PR_HCOMP	"HCMP"
#define	PR_ULDA		"ULDA"
#define	PR_GZIP		"GZIP"
#define	PR_NONE		"NONE"


/*
 *  The following #ifdef makes the interface to C++ cleaner. 
 */

#ifdef  __cplusplus
extern "C" {
#endif


/*
 *  Function prototypes and external declarations. 
 */
 
extern	int	press_f2f( int, int, char * );
extern	int	press_f2m( int, byte **, int *, char * );
extern	int	press_m2f( byte *, int, int, char * );
extern	int	press_m2m( byte *, int, byte **, int *, char * );
extern	int	press_setopt( char *, ... );
extern	int	unpress_f2f( int, int, char * );
extern	int	unpress_f2m( int, byte **, int *, char * );
extern	int	unpress_m2f( byte *, int, int, char * );
extern	int	unpress_m2m( byte *, int, byte **, int *, char * );
extern	int	unpress_fsize( int, int *, char * );
extern	int	unpress_msize( byte *, int, int *, char * );
 
extern char	pr_msg[];


/* 
 *  Ending the C++ compatibility from above. 
 */

#ifdef    __cplusplus
}
#endif

