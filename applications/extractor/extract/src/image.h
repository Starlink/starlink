 /*
 				image.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Include file for image.c.
*
*	Last modify:	29/08/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*----------------------------- Internal constants --------------------------*/

#define INTERPW		6	/* Interpolation function range (x) */
#define INTERPH		6	/* Interpolation function range (y) */

#define INTERPF(x)	(x==0.0?1.0:sin(PI*x)*sin(PI*x/3.0)/(PI*PI/3.0*x*x))
				/* Lanczos approximation */

/*--------------------------- structure definitions -------------------------*/


/*----------------------------- Global variables ----------------------------*/

/*------------------------------- functions ---------------------------------*/
extern void    	addimage(picstruct *field, float *psf,
			int w,int h, int ix,int iy, float amplitude),
		addimage_center(picstruct *field, float *psf,
			int w,int h, float x, float y, float amplitude),
		blankimage(picstruct *, PIXTYPE *, int,int, int,int, PIXTYPE),
		pasteimage(picstruct *, PIXTYPE *, int ,int, int, int);

extern int	copyimage(picstruct *, PIXTYPE *, int, int, int, int),
		copyimage_center(picstruct *, PIXTYPE *, int,int, float,float),
		vignet_resample(double *pix1, int w1, int h1, double *pix2,
			int w2, int h2, double dx, double dy, double step2);

