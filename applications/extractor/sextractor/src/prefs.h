 /*
 				prefs.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	Keywords for the configuration file.
*
*	Last modify:	14/07/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*----------------------------- Internal constants --------------------------*/

#define	MAXLIST		32		/* max. nb of list members */

/* NOTES:
One must have:	MAXLIST >= 1 (preferably >= 16!)
*/
/*-------------------------------- initialization ---------------------------*/
int	idummy;

pkeystruct key[] =
 {
  {"ANALYSIS_THRESH", P_FLOATLIST, prefs.thresh, 0,0, -1e31, 1e31,
    {""}, 1, 2, &prefs.nthresh},
  {"ASSOC_DATA", P_INTLIST, prefs.assoc_data, 0, 1000000,0.0,0.0,
    {""}, 1,MAXLIST, &prefs.nassoc_data},
  {"ASSOC_NAME", P_STRING, prefs.assoc_name},
  {"ASSOC_PARAMS", P_INTLIST, prefs.assoc_param, 1, 1000000,0.0,0.0,
    {""}, 2,3, &prefs.nassoc_param},
  {"ASSOC_RADIUS", P_FLOAT, &prefs.assoc_radius, 0,0, 1e-10,1e+10},
  {"ASSOC_TYPE", P_KEY, &prefs.assoc_type, 0,0, 0.0,0.0,
   {"FIRST", "MEAN", "MAG_MEAN", "SUM", "MAG_SUM", "MIN", "MAX", ""}},
  {"ASSOCSELEC_TYPE", P_KEY, &prefs.assocselec_type, 0,0, 0.0,0.0,
   {"ALL","MATCHED","-MATCHED",""}},
  {"BACK_FILTERSIZE", P_INTLIST, prefs.backfsize, 1,7, 0.0,0.0,
    {""}, 1,2, &prefs.nbackfsize},
  {"BACKPHOTO_THICK", P_INT, &prefs.pback_size, 1, 256},
  {"BACKPHOTO_TYPE", P_KEY, &prefs.pback_type, 0,0, 0.0,0.0,
   {"GLOBAL","LOCAL",""}},
  {"BACK_SIZE", P_INTLIST, prefs.backsize, 1,2000000000, 0.0,0.0,
    {""}, 1,2, &prefs.nbacksize},
  {"CATALOG_NAME", P_STRING, prefs.cat_name},
  {"CATALOG_TYPE", P_KEY, &prefs.cat_type, 0,0, 0.0,0.0,
   {"NONE", "ASCII","ASCII_HEAD", "ASCII_SKYCAT", "FITS_LDAC",
	"FITS_BINIMHEAD","FITS_NOIMHEAD","FITS_1.0",""}},
  {"CHECKIMAGE_NAME", P_STRINGLIST, prefs.check_name, 0,0,0.0,0.0,
    {""}, 0, MAXCHECK, &prefs.ncheck_name},
  {"CHECKIMAGE_TYPE", P_KEYLIST, prefs.check_type, 0,0, 0.0,0.0,
   {"NONE", "IDENTICAL",
   "BACKGROUND", "BACKGROUND_RMS", "MINIBACKGROUND",
   "MINIBACK_RMS", "-BACKGROUND",
   "FILTERED", "OBJECTS", "APERTURES", "SEGMENTATION", "ASSOC",
   "-OBJECTS", "-PROTOTYPES", "PROTOTYPES", "SOM_MAP",""},
   0, 13, &prefs.ncheck_type},
  {"CLEAN", P_BOOL, &prefs.clean_flag},
  {"CLEAN_PARAM", P_FLOAT, &prefs.clean_param, 0,0, 0.1,10.0},
  {"DEBLEND_MINCONT", P_FLOAT, &prefs.deblend_mincont, 0,0, 0.0,1.0},
  {"DEBLEND_NTHRESH", P_INT, &prefs.deblend_nthresh, 1,64},
  {"DETECT_MINAREA", P_INT, &prefs.ext_minarea, 1,1000000},
  {"DETECT_THRESH", P_FLOATLIST, prefs.dthresh, 0,0, -1e31, 1e31,
   {""}, 1, 2, &prefs.ndthresh},
  {"DETECT_TYPE", P_KEY, &prefs.detect_type, 0,0, 0.0,0.0,
   {"CCD","PHOTO",""}},
  {"FILTER", P_BOOL, &prefs.filter_flag},
  {"FILTER_NAME", P_STRING, prefs.filter_name},
  {"FILTER_THRESH", P_FLOATLIST, prefs.filter_thresh, 0,0,-1e31,1e31,
   {""}, 0, 2, &prefs.nfilter_thresh},
  {"FITS_UNSIGNED", P_BOOL, &prefs.fitsunsigned_flag},
  {"FLAG_IMAGE", P_STRINGLIST, prefs.fimage_name, 0,0,0.0,0.0,
    {""}, 0, MAXFLAG, &prefs.nfimage_name},
  {"FLAG_TYPE",  P_KEYLIST, prefs.flag_type, 0,0, 0.0,0.0,
   {"OR","AND","MIN", "MAX", "MOST",""}, 0, MAXFLAG, &idummy},
  {"GAIN", P_FLOAT, &prefs.gain, 0,0, 0.0, 1e+30},
  {"INTERP_MAXXLAG", P_INTLIST, prefs.interp_xtimeout, 1,1000000, 0.0,0.0,
   {""}, 1, 2, &prefs.ninterp_xtimeout},
  {"INTERP_MAXYLAG", P_INTLIST, prefs.interp_ytimeout, 1,1000000, 0.0,0.0,
   {""}, 1, 2, &prefs.ninterp_ytimeout},
  {"INTERP_TYPE", P_KEYLIST, prefs.interp_type, 0,0, 0.0,0.0,
   {"NONE","VAR_ONLY","ALL",""}, 1, 2, &prefs.ninterp_type},
  {"MAG_GAMMA", P_FLOAT, &prefs.mag_gamma, 0,0, 1e-10,1e+30},
  {"MAG_ZEROPOINT", P_FLOAT, &prefs.mag_zeropoint, 0,0, -100.0, 100.0},
  {"MAMA_CORFLEX", P_FLOAT, &prefs.mama_corflex, 0,0, -1.0,1.0},
  {"MASK_TYPE", P_KEY, &prefs.mask_type, 0,0, 0.0,0.0,
   {"NONE","BLANK","CORRECT",""}},
  {"MEMORY_BUFSIZE", P_INT, &prefs.mem_bufsize, 8, 65534},
  {"MEMORY_OBJSTACK", P_INT, &prefs.clean_stacksize, 16,65536},
  {"MEMORY_PIXSTACK", P_INT, &prefs.mem_pixstack, 1000, 10000000},
  {"PARAMETERS_NAME", P_STRING, prefs.param_name},
  {"PHOT_APERTURES", P_FLOATLIST, prefs.apert, 0,0, 0.0,2*MAXPICSIZE,
   {""}, 1, MAXNAPER, &prefs.naper},
  {"PHOT_AUTOPARAMS", P_FLOATLIST, prefs.autoparam, 0,0, 0.0,10.0,
   {""}, 2,2, &prefs.nautoparam},
  {"PHOT_AUTOAPERS", P_FLOATLIST, prefs.autoaper, 0,0, 0.0,1e6,
   {""}, 2,2, &prefs.nautoaper},
  {"PHOT_FLUXFRAC", P_FLOATLIST, prefs.flux_frac, 0,0, 1e-6, 1.0,
   {""}, 1, MAXNAPER, &prefs.nflux_frac},
  {"PIXEL_SCALE", P_FLOAT, &prefs.pixel_scale, 0,0, 0.0, 1e+10},
  {"PSF_NAME", P_STRING, prefs.psf_name},
  {"SATUR_LEVEL", P_FLOAT, &prefs.satur_level, 0,0, -1e+30, 1e+30},
  {"SEEING_FWHM", P_FLOAT, &prefs.seeing_fwhm, 0,0, 1e-10, 1e+10},

  {"SOM_NAME", P_STRING, prefs.som_name},
  {"STARNNW_NAME", P_STRING, prefs.nnw_name},
  {"VERBOSE_TYPE", P_KEY, &prefs.verbose_type, 0,0, 0.0,0.0,
   {"QUIET","NORMAL", "EXTRA_WARNINGS", "FULL",""}},
  {"WEIGHT_IMAGE", P_STRINGLIST, prefs.wimage_name, 0,0,0.0,0.0,
    {""}, 0, MAXIMAGE, &prefs.nwimage_name},
  {"WEIGHT_THRESH", P_FLOATLIST, prefs.weight_thresh, 0,0, 0.0, BIG,
   {""}, 0, 2, &prefs.nweight_thresh},
  {"WEIGHT_TYPE", P_KEYLIST, prefs.weight_type, 0,0, 0.0,0.0,
   {"NONE","BACKGROUND", "MAP_RMS", "MAP_VAR","MAP_WEIGHT", ""},
   0, MAXIMAGE, &prefs.nweight_type},
  {""}
 };

char		keylist[sizeof(key)/sizeof(pkeystruct)][16];

char *default_prefs[] =
 {
  "ASSOC_DATA   	2,3,4",
  "ASSOC_NAME		sky.list",
  "ASSOC_PARAMS 	2,3,4",
  "ASSOC_RADIUS		2.0",
  "ASSOC_TYPE		MAG_SUM",
  "ASSOCSELEC_TYPE	MATCHED",
  "BACKPHOTO_THICK	24",
  "BACKPHOTO_TYPE	GLOBAL",
  "CHECKIMAGE_NAME	check.fits",
  "CHECKIMAGE_TYPE	NONE",
  "DETECT_TYPE		CCD",
  "FILTER_THRESH	",
  "FITS_UNSIGNED	N",
  "FLAG_IMAGE		flag.fits",
  "FLAG_TYPE		OR",
  "INTERP_MAXXLAG	16",
  "INTERP_MAXYLAG	16",
  "INTERP_TYPE		ALL",
  "MAMA_CORFLEX		3.3e-5",
  "MASK_TYPE		CORRECT",
  "PHOT_AUTOAPERS	0.0,0.0",
  "PHOT_FLUXFRAC	0.5",
  "PSF_NAME		default.psf",
  "SOM_NAME		default.som",
  "VERBOSE_TYPE		NORMAL",
  "WEIGHT_IMAGE		weight.fits",
  "WEIGHT_TYPE      	NONE",
  "WEIGHT_THRESH	",
  ""
 };

