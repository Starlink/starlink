 /*
 				astrom.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	Astrometrical stuff.
*
*	Last modify:	15/07/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

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
  char		ctype[2][9];		/* FITS CTYPE strings */
  char		cunit[2][32];		/* FITS CUNIT strings */
  double	crval[2];		/* FITS CRVAL parameters */
  double	cdelt[2];		/* FITS CDELT parameters */
  double	crpix[2];		/* FITS CRPIX parameters */
  double	projp[200];		/* FITS PROJP parameters */
  double	longpole,latpole;	/* FITS LONGPOLE and LATPOLE */
  double	pc[4];			/* FITS PC matrix */
  double	linmat[4];		/* Local linear mapping matrix */
  double	lindet;			/* Determinant of the local matrix */
  double	pixscale;		/* (Local) pixel scale */
  double	ap2000,dp2000;		/* J2000 coordinates of pole */
  double	ap1950,dp1950;		/* B1950 coordinates of pole */
  double	equinox;		/* Equinox of observations */
  enum {RDSYS_FK5, RDSYS_FK4, RDSYS_FK4_NO_E, RDSYS_GAPPT}
		radecsys;		/* FITS RADECSYS reference frame */
  int		wcs_flag;		/* WCSLIB: can it be used? */
  struct wcsprm	*wcs;			/* WCSLIB's wcsprm structure */
  struct linprm	*lin;			/* WCSLIB's linprm structure */
  struct celprm	*cel;			/* WCSLIB's celprm structure */
  struct prjprm *prj;			/* WCSLIB's prjprm structure */
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
