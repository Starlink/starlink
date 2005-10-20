 /*
 				types.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*                       P.W.DRAPER (STARLINK, Durham University)
*
*	Contents:	global type definitions.
*
*	Last modify:	11/11/99
*                       20/03/00 (PWD): Added various members to
*                                       support userradii function.
*                       20/02/02 (PWD): Added ndfposx and ndfposy
*                                       members.
*	Last modify:	16/12/2002
*                                (EB): 2.3.
*	Last modify:	25/08/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include <stdio.h>

#ifndef _FITSCAT_H_
#include "fits/fitscat.h"
#endif

/*-------------------------------- flags ------------------------------------*/

#define		OBJ_CROWDED	0x0001
#define		OBJ_MERGED	0x0002
#define		OBJ_SATUR	0x0004
#define		OBJ_TRUNC	0x0008
#define		OBJ_APERT_PB	0x0010
#define		OBJ_ISO_PB	0x0020
#define		OBJ_DOVERFLOW	0x0040
#define		OBJ_OVERFLOW	0x0080

/*---------------------------- preanalyse flags -----------------------------*/

#define		ANALYSE_FAST		0
#define		ANALYSE_FULL		1
#define		ANALYSE_ROBUST		2

/*--------------------------------- typedefs --------------------------------*/
typedef	unsigned char	BYTE;			/* a byte */
typedef	unsigned short	USHORT;			/* 0 to 65535 integers */
typedef unsigned int	FLAGTYPE;		/* flag type */
typedef	char		pliststruct;		/* Dummy type for plist */

typedef	int		LONG;
typedef	unsigned int	ULONG;

typedef  enum {BACK_RELATIVE, BACK_ABSOLUTE}
		backenum;				/* BACK_TYPE */

typedef  enum {CHECK_NONE, CHECK_IDENTICAL, CHECK_BACKGROUND,
        CHECK_BACKRMS, CHECK_MINIBACKGROUND, CHECK_MINIBACKRMS,
        CHECK_SUBTRACTED, CHECK_FILTERED, CHECK_OBJECTS, CHECK_APERTURES,
	CHECK_SEGMENTATION, CHECK_ASSOC, CHECK_SUBOBJECTS,
	CHECK_SUBPSFPROTOS, CHECK_PSFPROTOS,
	CHECK_SUBPCPROTOS, CHECK_PCPROTOS, CHECK_PCOPROTOS,
		CHECK_MAPSOM}	checkenum;
	/* CHECK_IMAGE type */

typedef  enum {WEIGHT_NONE, WEIGHT_FROMBACK, WEIGHT_FROMRMSMAP,
		WEIGHT_FROMVARMAP, WEIGHT_FROMWEIGHTMAP, WEIGHT_FROMINTERP}
		weightenum;				/* WEIGHT_IMAGE type */

/*--------------------------------- objects ---------------------------------*/
/* I: "PIXEL" parameters */

typedef struct
  {
/* ---- basic parameters */
  int		number;				/* ID */
  int		fdnpix;				/* nb of extracted pix */
  int		dnpix;				/* nb of pix above thresh  */
  int		npix;				/* "" in measured frame */
  float		fdflux;				/* integrated ext. flux */
  float		dflux;				/* integrated det. flux */
  float		flux;				/* integrated mes. flux */
  float		fluxerr;			/* integrated variance */
  float		flux_prof;			/* PROFILE flux*/
  float		fluxerr_prof;			/* PROFILE flux variance */
  PIXTYPE	fdpeak;				/* peak intensity (ADU) */
  PIXTYPE	dpeak;				/* peak intensity (ADU) */
  PIXTYPE	peak;				/* peak intensity (ADU) */
/* ---- astrometric data */
  int		peakx,peaky;			/* pos of brightest pix */
  double       	mx, my;				/* barycenter */
  double	poserr_mx2, poserr_my2,
		poserr_mxy;			/* Error ellips moments */
/* ---- morphological data */			
  int		xmin,xmax,ymin,ymax,ycmin,ycmax;/* x,y limits */
  PIXTYPE	*blank, *dblank; 	       	/* BLANKing sub-images  */
  int		*submap;			/* Pixel-index sub-map */
  int		subx,suby, subw,subh;		/* sub-image pos. and size */
  short		flag;				/* extraction flags */
  FLAGTYPE	imaflag[MAXFLAG];		/* flags from FLAG-images */
  BYTE		singuflag;			/* flags for singularities */
  int		imanflag[MAXFLAG];     		/* number of MOST flags */
  double	mx2,my2,mxy;			/* variances and covariance */
  float		a, b, theta, abcor;		/* moments and angle */
  float		cxx,cyy,cxy;			/* ellipse parameters */
  int		firstpix;			/* ptr to first pixel */
  int		lastpix;			/* ptr to last pixel */
  float		bkg, dbkg, sigbkg;		/* Background stats (ADU) */
  float		thresh;				/* measur. threshold (ADU) */
  float		dthresh;		       	/* detect. threshold (ADU) */
  float		mthresh;		       	/* max. threshold (ADU) */
  int		iso[NISO];			/* isophotal areas */
  float		fwhm;				/* IMAGE FWHM */
  float         rad[NRAD];                      /* PWD: average radii */
  }	objstruct;

/* II: "BLIND" parameters */
typedef struct
  {
/* ---- photometric data */
  float		flux_iso;			/* ISO integrated flux */
  float		fluxerr_iso;			/* RMS error on ISO flux */
  float		mag_iso;			/* ISO mag */
  float		magerr_iso;			/* ISO mag uncertainty */
  float		flux_isocor;			/* ISOCOR integrated flux */
  float		fluxerr_isocor;			/* RMS error on ISOCOR flux */
  float		mag_isocor;			/* ISOCOR mag */
  float		magerr_isocor;			/* ISOCOR mag uncertainty */
  float		kronfactor;			/* kron parameter */
  float		flux_auto;			/* AUTO integrated flux */
  float		fluxerr_auto;			/* RMS error on AUTO flux */
  float		mag_auto;			/* AUTO mag */
  float		magerr_auto;			/* AUTO mag uncertainty */
  float		petrofactor;			/* kron parameter */
  float		flux_petro;			/* AUTO integrated flux */
  float		fluxerr_petro;			/* RMS error on AUTO flux */
  float		mag_petro;			/* AUTO mag */
  float		magerr_petro;			/* AUTO mag uncertainty */
  float		flux_best;			/* BEST integrated flux */
  float		fluxerr_best;			/* RMS error on BEST flux */
  float		mag_best;			/* BEST mag */
  float		magerr_best;			/* BEST mag uncertainty */
  float		*flux_aper;			/* APER flux vector */
  float		*fluxerr_aper;			/* APER flux error vector  */
  float		*mag_aper;			/* APER magnitude vector */
  float		*magerr_aper;			/* APER mag error vector */
  float		flux_prof;			/* PROFILE flux*/
  float		fluxerr_prof;			/* PROFILE flux error */
  float		mag_prof;			/* PROFILE magnitude */
  float		magerr_prof;			/* PROFILE magnitude error */
  float		flux_win;			/* WINdowed flux*/
  float		fluxerr_win;			/* WINdowed flux error */
  float		mag_win;			/* WINdowed magnitude */
  float		magerr_win;			/* WINdowed magnitude error */
/* ---- astrometric data */
  double	posx,posy;			/* "FITS" pos. in pixels */
  double	mamaposx,mamaposy;		/* "MAMA" pos. in pixels */
  float		sposx,sposy;			/* single precision pos. */
  float		ndfposx,ndfposy;		/* PWD: single precision pos. 
						   NDF Pixel coordinates */
  float		poserr_a, poserr_b,
		poserr_theta;			/* Error ellips parameters */
  float		poserr_cxx, poserr_cyy,
		poserr_cxy;			/* pos. error ellipse */
  double	poserr_mx2w, poserr_my2w,
		poserr_mxyw;			/* WORLD error moments */
  float		poserr_aw, poserr_bw,
		poserr_thetaw;			/* WORLD error parameters */
  float		poserr_thetas;			/* native error pos. angle */
  float		poserr_theta2000;		/* J2000 error pos. angle */
  float		poserr_theta1950;		/* B1950 error pos. angle */
  float		poserr_cxxw, poserr_cyyw,
		poserr_cxyw;			/* WORLD error ellipse */
  double	mx2w,my2w,mxyw;			/* WORLD var. and covar. */
  double	peakxw, peakyw;			/* WORLD of brightest pix */
  double	mxw, myw;			/* WORLD barycenters */
  double	alphas, deltas;			/* native alpha, delta */
  float		thetas;				/* native position angle E/N*/
  double	peakalphas, peakdeltas;		/* native for brightest pix */
  double	peakalpha2000, peakdelta2000;	/* J2000 for brightest pix */
  double	peakalpha1950, peakdelta1950;	/* B1950 for brightest pix */
  double	alpha2000, delta2000;		/* J2000 alpha, delta */
  float		theta2000;			/* J2000 position angle E/N */
  double	alpha1950, delta1950;		/* B1950 alpha, delta */
  float		theta1950;			/* B1950 position angle E/N */
  float		aw, bw;				/* WORLD ellipse size */
  float		thetaw;				/* WORLD position angle */
  float		cxxw,cyyw,cxyw;			/* WORLD ellipse parameters */
  float		npixw, fdnpixw;			/* WORLD isophotal areas */
  float		threshmu;			/* det. surface brightnees */
  float		maxmu;				/* max. surface brightnees */
  float		elong;				/* elongation */
  float		ellip;				/* ellipticity */
  float		polar;				/* Kaiser's "polarization" */
  float		polarw;				/* WORLD "polarization" */
  float		sprob;				/* Stellarity index */
  float		fwhmw;				/* WORLD FWHM */
  float		*assoc;				/* ASSOCiated data */
  int		assoc_number;			/* nb of ASSOCiated objects */
  float		*vignet;			/* Pixel data */
  float		*vigshift;			/* (Shifted) pixel data */

/* Windowed measurements */
  double	winpos_x,winpos_y;		/* Windowed barycenter */
  double	winposerr_mx2, winposerr_my2,
		winposerr_mxy;			/* Error ellips moments */
  float		winposerr_a, winposerr_b,
		winposerr_theta;		/* Error ellips parameters */
  float		winposerr_cxx, winposerr_cyy,
		winposerr_cxy;			/* pos. error ellipse */
  double	winposerr_mx2w, winposerr_my2w,
		winposerr_mxyw;			/* WORLD error moments */
  float		winposerr_aw, winposerr_bw,
		winposerr_thetaw;		/* WORLD error parameters */
  float		winposerr_thetas;		/* native error pos. angle */
  float		winposerr_theta2000;		/* J2000 error pos. angle */
  float		winposerr_theta1950;		/* B1950 error pos. angle */
  float		winposerr_cxxw, winposerr_cyyw,
		winposerr_cxyw;			/* WORLD error ellipse */
  double	win_mx2, win_my2,
		win_mxy;			/* Windowed moments */
  float		win_a, win_b,
		win_theta;			/* Windowed ellipse parameters*/
  float		win_polar;			/* Windowed "polarization" */
  float		win_cxx, win_cyy,
		win_cxy;			/* Windowed ellipse parameters*/
  double	win_mx2w, win_my2w,
		win_mxyw;			/* WORLD windowed moments */
  float		win_aw, win_bw,
		win_thetaw;			/* WORLD ellipse parameters */
  float		win_polarw;			/* WORLD WIN "polarization" */
  float		win_thetas;		/* native error pos. angle */
  float		win_theta2000;		/* J2000 error pos. angle */
  float		win_theta1950;		/* B1950 error pos. angle */
  float		win_cxxw, win_cyyw,
		win_cxyw;			/* WORLD ellipse parameters */
  double	winpos_xw, winpos_yw;		/* WORLD coordinates */
  double	winpos_alphas, winpos_deltas;	/* native alpha, delta */
  double	winpos_alpha2000, winpos_delta2000;	/* J2000 alpha, delta */
  double	winpos_alpha1950, winpos_delta1950;	/* B1950 alpha, delta */
  short		winpos_niter;			/* Number of WIN iterations */
  short		win_flag;			/* 1:x2<0 2:xy=x2 4:flux<0 */

 /* ---- SOM fitting */
  float		flux_somfit;			/* Fitted amplitude */
  float		fluxerr_somfit;			/* RMS error on SOM flux */
  float		mag_somfit;			/* Magnitude from SOM fit */
  float		magerr_somfit;			/* Mag. err. from SOM fit */
  float		stderr_somfit;			/* Fitting reduced error */
  float		*vector_somfit;			/* SOM fit vector position */
/* ---- Growth curves and stuff */
  float		*flux_growth;			/* Cumulated growth_curve */
  float		flux_growthstep;		/* Growth-curve step */
  float		*mag_growth;			/* Cumulated growth_curve */
  float		mag_growthstep;			/* Growth-curve step */
  float		*flux_radius;			/* f-light-radii */
  float		hl_radius;			/* Scalar half-light radius */
/* ---- PSF-fitting */
  float		*flux_psf;			/* Flux from PSF-fitting */
  float		*fluxerr_psf;			/* RMS error on PSF flux */
  float		*mag_psf;			/* Mag from PSF-fitting */
  float		*magerr_psf;			/* RMS mag from PSF-fitting */
  float		*x_psf, *y_psf;			/* Coords from PSF-fitting */
  short		niter_psf;			/* # of PSF-fitting iterat. */
  short		npsf;				/* # of fitted PSFs */
  float		chi2_psf;			/* Red. chi2 of PSF-fitting */
  double	xw_psf, yw_psf;			/* WORLD coords */
  double	alphas_psf, deltas_psf;		/* native alpha, delta */
  double	alpha2000_psf, delta2000_psf;	/* J2000 alpha, delta */
  double	alpha1950_psf, delta1950_psf;	/* B1950 alpha, delta */
  double	poserrmx2_psf, poserrmy2_psf,
		poserrmxy_psf;			/* Error ellips moments */
  float		poserra_psf, poserrb_psf,
		poserrtheta_psf;		/* Error ellips parameters */
  float		poserrcxx_psf, poserrcyy_psf,
		poserrcxy_psf;			/* pos. error ellipse */
  double	poserrmx2w_psf, poserrmy2w_psf,
		poserrmxyw_psf;			/* WORLD error moments */
  float		poserraw_psf, poserrbw_psf,
		poserrthetaw_psf;		/* WORLD error parameters */
  float		poserrthetas_psf;		/* native error pos. angle */
  float		poserrtheta2000_psf;		/* J2000 error pos. angle */
  float		poserrtheta1950_psf;		/* B1950 error pos. angle */
  float		poserrcxxw_psf, poserrcyyw_psf,
		poserrcxyw_psf;			/* WORLD error ellipse */
/* ---- PC-fitting */
  double	mx2_pc,my2_pc,mxy_pc;		/* PC 2nd-order parameters */
  float		a_pc,b_pc,theta_pc;		/* PC shape parameters */
  float		*vector_pc;			/* Principal components */
  float		gdposang;			/* Gal. disk position angle */
  float		gdscale;			/* Gal. disk scalelength */
  float		gdaspect;			/* Gal. disk aspect-ratio */
  float		gde1,gde2;			/* Gal. disk ellipticities */
  float		gbratio;			/* Galaxy B/T */
  float		gbposang;			/* Gal. bulge position angle */
  float		gbscale;			/* Gal. bulge scalelength */
  float		gbaspect;			/* Gal. bulge aspect-ratio */
  float		gbe1,gbe2;			/* Gal. bulge ellipticities */
  float		flux_galfit;			/* Galaxy tot. flux from fit */
  float		fluxerr_galfit;			/* RMS error on galfit flux */
  float		mag_galfit;			/* Galaxy tot. mag from fit */
  float		magerr_galfit;			/* RMS error on galfit mag */
/* ---- MEF */
  short		ext_number;			/* FITS extension number */
  }	obj2struct;

/*----------------------------- lists of objects ----------------------------*/
typedef struct
  {
  int		nobj;			/* number of objects in list */
  objstruct	*obj;			/* pointer to the object array */
  int		npix;			/* number of pixels in pixel-list */
  pliststruct	*plist;			/* pointer to the pixel-list */
  PIXTYPE	dthresh;		/* detection threshold */
  PIXTYPE	thresh;			/* analysis threshold */
  }	objliststruct;


/*----------------------------- image parameters ----------------------------*/
typedef struct pic
  {
  char		filename[MAXCHAR];	/* pointer to the image filename */
  char		*rfilename;		/* pointer to the reduced image name */
  char		ident[MAXCHAR];		/* field identifier (read from FITS)*/
  char		rident[MAXCHAR];	/* field identifier (relative) */
  FILE		*file;			/* pointer the image file structure */
  char		*fitshead;		/* pointer to the FITS header */
  int		fitsheadsize;		/* FITS header size */
/* ---- main image parameters */
  int		bitpix, bytepix;	/* nb of bits and bytes per pixel */
  int		bitsgn;			/* non-zero if signed integer data */
  int		width, height;		/* x,y size of the field */
  KINGSIZE_T	npix;			/* total number of pixels */
  double	bscale, bzero;		/* FITS scale and offset */
  double	ngamma;			/* normalized photo gamma */
  int		nlevels;		/* nb of quantification levels */
  float		pixmin, pixmax;		/* min and max values in frame */
  int		y;			/* y current position in field */
  int		ymin;			/* y limit (lowest accessible) */
  int		ymax;			/* y limit (highest accessible+1) */
  int		yblank;			/* y blanking limit (highest+1) */
  PIXTYPE	*strip;			/* pointer to the image buffer */
  FLAGTYPE	*fstrip;		/* pointer to the FLAG buffer */
  int		stripheight;		/* height  of a strip (in lines) */
  int		stripmargin;		/* number of lines in margin */
  int		stripstep;		/* number of lines at each read */
  int		stripy;			/* y position in buffer */
  int		stripylim;		/* y limit in buffer */
  int		stripysclim;		/* y scroll limit in buffer */
/* ---- image (de-)compression */
  enum {ICOMPRESS_NONE, ICOMPRESS_BASEBYTE, ICOMPRESS_PREVPIX}
		compress_type;		/* image compression type */
  char		*compress_buf;		/* de-compression buffer */
  char		*compress_bufptr;	/* present pixel in buffer */
  int		compress_curval;	/* current pixel or checksum value */
  int		compress_checkval;	/* foreseen pixel or checksum value */
  int		compress_npix;		/* remaining pixels in buffer */
/* ---- basic astrometric parameters */
   double	pixscale;		/* pixel size in arcsec.pix-1 */
   double	epoch;			/* epoch of coordinates */
/* ---- background parameters */
  float		*back;			/* ptr to the background map in mem */
  float		*dback;			/* ptr to the background deriv. map */
  float		*sigma;			/* ptr to the sigma map */
  float		*dsigma;		/* Ptr to the sigma deriv. map */
  int		backw, backh;		/* x,y size of a bkgnd mesh */
  int		nbackp;			/* total nb of pixels per bkgnd mesh */
  int		nbackx, nbacky;		/* x,y number of bkgnd meshes */
  int		nback;			/* total number of bkgnd meshes */
  int		nbackfx, nbackfy;	/* x,y size of bkgnd filtering mask */
  float		backmean;		/* median bkgnd value in image */
  float		backsig;		/* median bkgnd rms in image */
  float		sigfac;			/* scaling RMS factor (for WEIGHTs) */
  PIXTYPE	*backline;		/* current interpolated bkgnd line */
  PIXTYPE	dthresh;		/* detection threshold */
  PIXTYPE	thresh;			/* analysis threshold */
  backenum	back_type;		/* Background type */
/* ---- astrometric parameters */
  struct structastrom	*astrom;	/* astrometric data */
  struct structassoc	*assoc;		/* ptr to the assoc-list */
  int		flags;			/* flags defining the field type */
/* ---- image interpolation */
  int		interp_flag;		/* interpolation for this field? */
  PIXTYPE	*interp_backup;		/* backup line for interpolation */
  PIXTYPE	weight_thresh;		/* interpolation threshold */
  int		*interp_ytimeoutbuf;	/* interpolation timeout line buffer */
  int		interp_xtimeout;	/* interpolation timeout value in x */
  int		interp_ytimeout;	/* interpolation timeout value in y */
  struct pic	*reffield;	       	/* pointer to a reference field */
  OFF_T		mefpos;			/* Position in a MEF file */
  }	picstruct;


/*-------------------------------- catalog  ---------------------------------*/

typedef struct
  {
  int		ndetect;				/* nb of detections */
  int		ntotal;					/* Total object nb */
  int		nparam;					/* Nb of parameters */
/*----- Misc. strings defining the extraction */
  char		prefs_name[MAXCHAR];			/* Prefs filename*/
  char		image_name[MAXCHAR];			/* image filename*/
  char		nnw_name[MAXCHAR];			/* NNW name */
  char		filter_name[MAXCHAR];			/* Filter name */
  char		soft_name[MAXCHAR];			/* Sextractor version*/
/*----- time */
  char		ext_date[16],ext_time[16];		/* date and time */
  double	ext_elapsed;				/* processing time */
/*----- MEF */
  int		currext;				/* current extension */
  int		next;					/* Nb of extensions */
  }		sexcatstruct;

