 /*
 				astrom.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*                       P.W.DRAPER Starlink & Durham University
*
*	Contents:	Astrometrical stuff.
*
*	Last modify:	01/06/97 (EB):
*                       17/12/98 (PWD):
*                          Removed unused parts of structures
*                       04/01/99 (PWD):
*                          Added cvt component for precession AstFrameSet.
*	Last modify:	24/01/2003
*                          (EB): 2.3.
*	Last modify:	15/07/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include        "ast.h"

/*----------------------------- Internal constants --------------------------*/

#define		DEG	(PI/180.0)	/* 1 deg in radians */
#define		ARCSEC	(DEG/3600.0)	/* 1 arcsec in radians */
#define		MJD2000	51544.50000	/* Modified Julian date for J2000.0 */
#define		MJD1950	33281.92346	/* Modified Julian date for B1950.0 */
#define		JU2TROP	1.0000214	/* 1 Julian century in tropical units*/

/*------------------------------- structures --------------------------------*/

typedef struct structastrom
  {
    int		naxis;			/* Number of image axes */
    double	linmat[4];		/* Local linear mapping matrix */
    double	lindet;			/* Determinant of the local matrix */
    double	pixscale;		/* (Local) pixel scale */
    double	ap2000,dp2000;		/* J2000 coordinates of pole */
    double	ap1950,dp1950;		/* B1950 coordinates of pole */
    double	equinox;		/* Equinox of observations */
    int		wcs_flag;		/* AST structure can it be used? */
    AstFrameSet *cvt;                   /* FrameSet for precessions */
  }	astromstruct;

/*------------------------------- functions ---------------------------------*/
extern void		astrom_errparam(picstruct *, objstruct *),
			astrom_winerrparam(picstruct *, objstruct *),
			astrom_shapeparam(picstruct *, objstruct *),
			astrom_winshapeparam(picstruct *, objstruct *),
			computeastrom(picstruct *, objstruct *),
			copyastrom(picstruct *infield, picstruct *outfield),
			endastrom(picstruct *),
			initastrom(picstruct *),
			j2b(double, double, double, double *, double *),
			precess(double,double,double,double,double *,double *);

extern double		*compute_wcs(picstruct *, double, double);
