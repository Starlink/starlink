/*
 				fitshead.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, DeNIS/LDAC.
*
*	Contents:	header structure and templates for catalog data.
*
*	Last modify:	22/08/96
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

paramstruct	extpar[] = {
	{"XDATE   ", 0, sex.ext_date, "%-18s", H_STRING},
	{"XTIME   ", 0, sex.ext_time, "%-18s", H_STRING},
	{"XELAPSED", T_DOUBLE, &sex.ext_elapsed, "%7.1f", H_FLOAT},
	{"XSOFTVER", 0, sex.soft_name, "%-18s", H_STRING},
/*
	{"SEXSTRSY", T_LONG, &field.stripheight, "%5d", H_INT},
	{"SEXPIXS ", T_DOUBLE, &field.pixscale, "%-15G", H_EXPO},
*/
	{"SEXSFWHM", T_DOUBLE, &prefs.seeing_fwhm, "%-13G", H_EXPO},
	{"SEXNNWF ", 0, sex.nnw_name, "%-18s", H_STRING},
	{"SEXGAIN ", T_DOUBLE, &prefs.gain, "%6.2f", H_EXPO},
/*
	{"XDBKGLEV", T_FLOAT, &field.backmean, "%-13G", H_EXPO},
	{"XDBKGDEV", T_FLOAT, &field.backsig, "%-13G", H_EXPO},
	{"XDTHRESH", T_DOUBLE, &field.thresh, "%-15G", H_EXPO},
*/
	{"SEXCONFF", 0, sex.prefs_name, "%-18s", H_STRING},
	{"SEXDETT ", 0, &prefs.detect_type, "%-18s", H_KEY},
	{"SEXTHLDT", 0, &prefs.threshold_type, "-18s", H_KEY},
	{"SEXTHLD ", T_DOUBLE, &prefs.threshold, "%-13G", H_EXPO},
	{"XMINAREA", T_LONG, &prefs.ext_minarea, "%5d", H_INT},
	{"SEXCONV ", T_LONG, &prefs.filter_flag, "%1s", H_BOOL},
	{"SEXCONVN", T_LONG, &prefs.convnorm_flag, "%1s", H_BOOL},
	{"SEXCONVF", 0, sex.filter_name, "%-18s", H_STRING},
	{"XNTHRESH", T_LONG, &prefs.deblend_nthresh, "%3d", H_INT},
	{"XMINCONT", T_DOUBLE, &prefs.deblend_mincont, "%8f", H_FLOAT},
	{"SEXCLN  ", T_LONG, &prefs.clean_flag, "%1s", H_BOOL},
	{"SEXCLNPA", T_DOUBLE, &prefs.clean_param, "%5.2f", H_FLOAT},
	{"SEXCLNST", T_LONG, &prefs.clean_stacksize, "%6d", H_INT},
	{"XAPERTD ", T_LONG, &prefs.apert[0], "%7.1f", H_FLOAT},
	{"XAPERTD2", T_LONG, &prefs.apert[1], "%7.1f", H_FLOAT},
	{"SEXAPEK1", T_DOUBLE, &prefs.kron_fact, "%4.1f", H_FLOAT},
	{"SEXAPEK2", T_DOUBLE, &prefs.kron_nsig, "%4.1f", H_FLOAT},
	{"SEXAPEK3", T_DOUBLE, &prefs.kron_minsig, "%4.1f", H_FLOAT},
	{"XSATLEV ", T_DOUBLE, &prefs.satur_level, "%-13G", H_EXPO},
	{"XMAGZPT ", T_DOUBLE, &prefs.mag_zeropoint, "%8.4f", H_FLOAT},
	{"SEXMGGAM", T_DOUBLE, &prefs.mag_gamma, "%4.2f", H_FLOAT},
/*
	{"XBKGNDSX", T_LONG, &field.backw, "%5d", H_INT},
	{"XBKGNDSY", T_LONG, &field.backh, "%5d", H_INT},
	{"SEXBKGFX", T_LONG, &field.nbackfx, "%3d", H_INT},
	{"SEXBKGFY", T_LONG, &field.nbackfy, "%3d", H_INT},
*/
	{"SEXPBKGT", 0, &prefs.pback_type, "-18s", H_KEY},
	{"SEXPBKGS", T_LONG, &prefs.pback_size, "%3d", H_INT},
	{"SEXPIXSK", T_LONG, &prefs.mem_pixstack, "%8d", H_INT},
	{"SEXFBUFS", T_LONG, &prefs.mem_bufsize, "%5d", H_INT},
	{"SEXISAPR", T_DOUBLE, &prefs.scan_isoapratio, "%4.2f", H_FLOAT},
	{"SEXNDET ", T_LONG, &cat.ndetect, "%9d", H_INT},
	{"SEXNFIN ", T_LONG, &cat.ntotal, "%9d", H_INT},
	{"SEXNPARA", T_LONG, &cat.nparam, "%3d", H_INT},
	{""}
	};

/*------------------------- start of primary header -------------------------*/
char	primhead[36][80] = {
	"SIMPLE  =                    T / Well this is standard FITS",
	"BITPIX  =                    8 /",
	"NAXIS   =                    0 /",
	"EXTEND  =                    T / Extension(s) may be present",
	"COMMENT This is an individual LDAC-SEx catalog",
	"END     "};

/*---------------- classical `ASCFIELD' binary table header -----------------*/
char	ascfieldhead[36][80] = {
	"XTENSION= 'BINTABLE'           / This is a binary table",
	"BITPIX  =                    8 /",
	"NAXIS   =                    2 /",
	"NAXIS1  =                    0 / Number of FIELD entries",
	"NAXIS2  =                    1 / Only one big row",
	"PCOUNT  =                    0 /",
	"GCOUNT  =                    1 /",
	"TFIELDS =                    1 /",
	"EXTNAME = 'ASCFIELD'           / ASCII data for the current FIELD",
	"TTYPE1  = 'Field Header Card'  / Label",
	"TFORM1  = '0A80'               / Data format: ASCII",
	"END                             "};

/*----------------- SEx contribution to the `ASCFIELD' table ----------------*/
char	extparmodel[][80] = {
	"XSOFTVER= 'SExtractor 1.1'     / Software used for data reduction",
	"XDATE   = '00/00/00'           / Date at the end of extraction",
	"XTIME   = '00:00:00'           / Time at the end of extraction",
	"XELAPSED=                  0.0 / Elapsed time (s) during extraction",
        "XMINAREA=                    0 / Minimum area for extraction (pixels)",
        "XNTHRESH=                    0 / Number of sub-thresholds",
        "XMINCONT=                  0.0 / Contrast parameter for deblending",
        "XSATLEV =                  0.0 / Saturation (ADU)",
	"XBKGNDSX=                    0 / Background mesh width (pixels)",
	"XBKGNDSY=                    0 / Background mesh height (pixels)",
        "SEXPIXS =                    1 / Pixel scale used (arcsec)",
	"SEXSFWHM=                  1.0 / Seeing FWHM used (arcsec)",
	"SEXNNWF = 'Default.nnw'        / NNW filename for classification",
	"SEXGAIN =                  0.0 / Gain (in e- per ADU)",
	"XDBKGLEV=                  0.0 / Median background (ADU)",
	"XDBKGDEV=                  0.0 / Median background rms (ADU)",
	"XDTHRESH=                  0.0 / Threshold computed from background (ADU)",
	"SEXCONFF= 'Default.sex'        / Configuration filename",
	"SEXDET  = 'CCD'                / Detection type",
	"SEXTHLDT= 'SIGMA'              / Threshold type",
        "SEXTHLD =                  0.0 / Basic threshold used for extraction (ADU)",
	"SEXCONV =                    F / Convolution flag",
	"SEXCONVN=                    F / Convolution normalisation flag",
	"SEXCONVF= 'Default.conv'       / Convolution filename",
	"SEXCLN  =                    F / Cleaning flag",
	"SEXCLNPA=                  0.0 / Cleaning parameter",
	"SEXCLNST=                    0 / Cleaning object stack-size (# of objects)",
        "XAPERTD =                  0.0 / Fixed aperture diameter (pixels)",
        "XAPERTD2=                  0.0 / Second fixed aperture diameter (pixels)",
        "SEXAPEK1=                  0.0 / Kron parameter",
        "SEXAPEK2=                  0.0 / Kron radius for analysis",
        "SEXAPEK3=                  0.0 / Kron minimum radius",
        "XMAGZPT =                  0.0 / Magnitude zero-point",
        "SEXMGGAM=                  0.0 / Magnitude gamma",
	"SEXBKGFX=                    0 / Background filter width",
	"SEXBKGFY=                    0 / Background filter height",
	"SEXPBKGT= 'GLOBAL'             / Background type",
	"SEXPBKGS=                    0 / Local area thickness (pixels)",
	"SEXPIXSK=                    0 / Pixel stack-size (pixels)",
	"SEXFBUFS=                    0 / Frame-buffer size (scanlines)",
	"SEXISAPR=                    0 / Iso/aper ratio",
	"SEXNDET =                    0 / Number of detections",
	"SEXNFIN =                    0 / Number of final extracted objects",
	"SEXNPARA=                    0 / Number of parameters per object",
	"END                             "};	/* don't remove the END!!! */


/*----------------------- `OBJECTS' binary table header ---------------------*/

char	objheadmodel[][80] = {
	"XTENSION= 'BINTABLE'           / This is a binary table",
	"BITPIX  =                    8 /",
	"NAXIS   =                    2 /",
	"NAXIS1  =                    0 /",
	"NAXIS2  =                    0 / Number of catalog entries",
	"PCOUNT  =                    0 /",
	"GCOUNT  =                    1 /",
	"TFIELDS =                    0 /",
	"EXTNAME = 'OBJECTS'            / Object data",
	"END                             "};


/*---------------- FITS keyword to exclude from IMAGE header ----------------*/

char	blacklist[][16] = {
      
	"SIMPLE  ",
	"NAXIS   ",
	"PCOUNT  ",
	"GCOUNT  ",
	"TFIELDS ",
	"END     ",
	""
	};

