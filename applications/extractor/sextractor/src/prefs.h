 /*
 				prefs.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Keywords for the configuration file.
*
*	Last modify:	15/12/2002
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifndef _PREFS_H_
#define _PREFS_H_

/*----------------------------- Internal constants --------------------------*/

#define	MAXLIST		32		/* max. nb of list members */

/* NOTES:
One must have:	MAXLIST >= 1 (preferably >= 16!)
*/
/*------------------------------- preferences -------------------------------*/
typedef struct
  {
  char		prefs_name[MAXCHAR];			/* prefs filename*/
  char		*(image_name[2]);			/* image filenames */
  int		nimage_name;				/* nb of params */
  char		cat_name[MAXCHAR];			/* catalog filename*/
/*----- thresholding */
  double	dthresh[2];				/* detect. threshold */
  int		ndthresh;				/* (1 or 2 entries) */
  double	thresh[2];				/* analysis thresh. */
  int		nthresh;				/* (1 or 2 entries) */
  enum	{THRESH_RELATIVE, THRESH_ABSOLUTE}
					thresh_type[2];	/* bkgnd type */
  int		nthresh_type;				/* nb of params */
/*----- extraction */
  int		dimage_flag;				/* detect. image ? */
  int		ext_minarea;				/* min area in pix. */
  int		deb_maxarea;				/* max deblend. area */
  int		filter_flag;				/* smoothing on/off */
  char		filter_name[MAXCHAR];			/* mask filename */
  double	filter_thresh[2];			/* Filter thresholds */
  int		nfilter_thresh;				/* nb of params */
  int		deblend_nthresh;			/* threshold number */
  double	deblend_mincont;			/* minimum contrast */
  double	satur_level;				/* saturation level */
  enum	{CCD, PHOTO}			detect_type;	/* detection type */
/*----- Flagging */
  char		*(fimage_name[MAXFLAG]);		/* flagmap filenames */
  int		nfimage_name;				/* nb of params */
  enum	{FLAG_OR, FLAG_AND, FLAG_MIN, FLAG_MAX, FLAG_MOST}
				flag_type[MAXFLAG];	/* flag combination */
  int		imaflag_size;				/* requested iso nb1 */
  int		imanflag_size;				/* requested iso nb2 */
  int		nimaisoflag;				/* effective iso nb */
  int		nimaflag;				/* effective ima nb */
/*----- cleaning */
  int		clean_flag;				/* allow cleaning ? */
  double	clean_param;				/* cleaning effic. */
/*----- Weighting */
  char		*(wimage_name[2]);       		/* weight filenames */
  int		nwimage_name;				/* nb of params */
  weightenum	weight_type[2];				/* weighting scheme */
  int		nweight_type;				/* nb of params */
  int		weight_flag;				/* do we weight ? */
  int		dweight_flag;				/* detection weight? */
  int		weightgain_flag;			/* weight gain? */
/*----- photometry */
  enum	{CAT_NONE, ASCII, ASCII_HEAD, ASCII_SKYCAT,
	FITS_LDAC, FITS_BINIMHEAD, FITS_NOIMHEAD, FITS_10}
		cat_type;				/* type of catalog */
  enum	{PNONE, FIXED, AUTO}		apert_type;	/* type of aperture */
  double	apert[MAXNAPER];			/* apert size (pix) */
  int		naper;					/* effective apert. */
  int		flux_apersize, fluxerr_apersize;	/* requested apert. */
  int		mag_apersize, magerr_apersize;		/* requested apert. */
  double	autoparam[2];				/* Kron parameters */
  int		nautoparam;				/* nb of Kron params */
  double	autoaper[2];				/* minimum apertures */
  int		nautoaper;				/* nb of min. aperts */
  double	mag_zeropoint;				/* magnitude offsets */
  double	mag_gamma;				/* for emulsions */
  double	gain;					/* only for CCD */
/*----- S/G separation */
  double	pixel_scale;				/* in arcsec */
  double	seeing_fwhm;				/* in arcsec */
  char		nnw_name[MAXCHAR];			/* nnw filename */
/*----- background */
  enum	{IMAGE, AFILE}			back_origin;	/* origin of bkgnd */
  char		back_name[MAXCHAR];			/* bkgnd filename */
  backenum	back_type[2];				/* bkgnd type */
  int		nback_type;				/* nb of params */
  double	back_val[2];				/* user-def. bkg */
  int		nback_val;				/* nb of params */
  int		backsize[2];				/* bkgnd mesh size */
  int		nbacksize;				/* nb of params */
  int		backfsize[2];				/* bkgnd filt. size */
  int		nbackfsize;				/* nb of params */
  double	backfthresh;				/* bkgnd fil. thresh */
  enum	{GLOBAL, LOCAL}			pback_type;	/* phot. bkgnd type */
  int		pback_size;				/* rect. ann. width */
/*----- memory */
  int		clean_stacksize;			/* size of buffer */
  int		mem_pixstack;				/* pixel stack size */
  int		mem_bufsize;				/* strip height */
/*----- catalog output */
  char		param_name[MAXCHAR];			/* param. filename */
/*----- miscellaneous */
  int		pipe_flag;				/* allow piping ? */
  enum	{QUIET, NORM, WARN, FULL}      	verbose_type;	/* display type */
/*----- CHECK-images */
  int		check_flag;				/* CHECK-image flag */
  checkenum    	check_type[MAXCHECK];		       	/* check-image types */
  int		ncheck_type;				/* nb of params */
  char		*(check_name[MAXCHECK]);       		/* check-image names */
  int		ncheck_name;				/* nb of params */
  struct structcheck	*(check[MAXCHECK]);		/* check-image ptrs */
/*----- ASSOCiation */
  int		assoc_flag;				/* ASSOCiation flag */
  char		assoc_name[MAXCHAR];			/* ASSOC-list name */
  int		assoc_param[3];				/* ASSOC param cols */
  int		nassoc_param;				/* nb of params */
  int		assoc_data[MAXNASSOC];		       	/* ASSOC data cols */
  int		nassoc_data;				/* nb of params */
  enum	{ASSOC_FIRST, ASSOC_NEAREST, ASSOC_MEAN, ASSOC_MAGMEAN,
	 ASSOC_SUM, ASSOC_MAGSUM, ASSOC_MIN, ASSOC_MAX}
		assoc_type;				/* type of assoc. */
  enum	{ASSOCSELEC_ALL, ASSOCSELEC_MATCHED, ASSOCSELEC_NOMATCHED}
		assocselec_type;		       	/* type of assoc. */
  double	assoc_radius;				/* ASSOC range */
  int		assoc_size;				/* nb of parameters */
  char		retina_name[MAXCHAR];			/* retina filename */
  int		vignetsize[2];				/* vignet size */
  int		vigshiftsize[2];			/* shift-vignet size */
  int		cleanmargin;				/* CLEANing margin */
  char		som_name[MAXCHAR];			/* SOM filename */
  int		somfit_flag;				/* ASSOCiation flag */
  int		somfit_vectorsize;			/* SOMfit vec. size */
/*----- masking */
  enum {MASK_NONE, MASK_BLANK, MASK_CORRECT}
		mask_type;				/* type of masking */
  int		blank_flag;				/* BLANKing flag */
  double	weight_thresh[2];      			/* weight threshlds */
  int		nweight_thresh;				/* nb of params */
/*----- interpolation */
  enum	{INTERP_NONE, INTERP_VARONLY, INTERP_ALL}
		interp_type[2];				/* interpolat. type */
  int		ninterp_type;				/* nb of params */
  int		interp_xtimeout[2];   			/* interp. x timeout */
  int		ninterp_xtimeout;       	        /* nb of params */
  int		interp_ytimeout[2];   			/* interp. y timeout */
  int		ninterp_ytimeout;       		/* nb of params */
/*----- astrometry */
  int		world_flag;				/* WORLD required */
/*----- growth curve */
  int		growth_flag;				/* gr. curve needed */
  int		flux_growthsize;       			/* number of elem. */
  int		mag_growthsize;       			/* number of elem. */
  int		flux_radiussize;       			/* number of elem. */
  double	growth_step;				/* step size (pix) */
  double	flux_frac[MAXNAPER];			/* for FLUX_RADIUS */
  int		nflux_frac;       			/* number of elem. */
/*----- PSF-fitting */
  int		psf_flag;				/* PSF-fit needed */
  char		psf_name[MAXCHAR];			/* PSF filename */
  int		psf_npsfmax;				/* Max # of PSFs */
  enum	{PSFDISPLAY_SPLIT, PSFDISPLAY_VECTOR}
		psfdisplay_type;			/* PSF display type */
  int		psf_xsize,psf_ysize;			/* nb of params */
  int		psf_xwsize,psf_ywsize;			/* nb of params */
  int		psf_alphassize,psf_deltassize;		/* nb of params */
  int		psf_alpha2000size,psf_delta2000size;	/* nb of params */
  int		psf_alpha1950size,psf_delta1950size;	/* nb of params */
  int		psf_fluxsize;				/* nb of params */
  int		psf_fluxerrsize;			/* nb of params */
  int		psf_magsize;				/* nb of params */
  int		psf_magerrsize;				/* nb of params */
  int		pc_flag;				/* PC-fit needed */
  int		pc_vectorsize;				/* nb of params */
/*----- customize */
  double	mama_corflex;
  int		fitsunsigned_flag;			/* Force unsign FITS */
  int		next;			     /* Number of extensions in file */

  enum	{RAD_SB, RAD_INT}
		rad_type;				/* PWD: radii units */
  int           nrad_type;
  double        rad[3];                                 /* PWD: radii specs */
  int           nrad;                                   /* PWD: nb of params */
  }	prefstruct;

  prefstruct		prefs;

/*-------------------------------- protos -----------------------------------*/
extern int	cistrcmp(char *cs, char *ct, int mode);

extern void	dumpprefs(void),
		readprefs(char *filename,char **argkey,char **argval,int narg),
		useprefs(void);
#endif
