/*
 				sexhead.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, DeNIS/LDAC.
*
*	Contents:	header structure and templates for catalog data.
*
*	Last modify:	25/05/99
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

keystruct	headkey[] = {
  {"FITSFILE", "File name of the analysed image",
	cat.image_name, H_STRING, T_STRING, "%-18s"},
  {"SEXVERS ", "Extraction software",
	cat.soft_name, H_STRING, T_STRING, "%-18s"},
  {"SEXDATE ", "Extraction date",
	cat.ext_date, H_STRING, T_STRING, "%-18s"},
  {"SEXTIME ", "Extraction time",
	cat.ext_time, H_STRING, T_STRING, "%-18s"},
  {"SEXELAPS", "Elapsed time during extraction (s)",
	&cat.ext_elapsed, H_FLOAT, T_DOUBLE, "%7.1f"},
  {"SEXBKGND", "Median background level (ADU)",
	&thefield1.backmean, H_EXPO, T_FLOAT, "%-13G"},
  {"SEXBKDEV", "Median background RMS (ADU)",
	&thefield1.backsig, H_EXPO, T_FLOAT, "%-13G"},
  {"SEXTHLD ", "Extraction threshold (ADU)",
	&thefield2.dthresh, H_EXPO, T_FLOAT, "%-15G"},
  {"SEXATHLD", "Analysis threshold (ADU)",
	&thefield1.thresh, H_EXPO, T_FLOAT, "%-15G"},
  {"SEXNDET ", "Number of raw detections",
	&cat.ndetect, H_INT, T_LONG, "%9d"},
  {"SEXNFIN ", "Final number of extracted sources",
	&cat.ntotal, H_INT, T_LONG, "%9d"},
  {"SEXNPARA", "Number of parameters per source",
	&cat.nparam, H_INT, T_LONG, "%3d"},
  {"SEXPXSCL", "Pixel scale used for measurements (arcsec)",
	&thefield1.pixscale, H_EXPO, T_DOUBLE, "%-15G"},
  {"SEXSFWHM", "Source FWHM used for measurements (arcsec)",
	&prefs.seeing_fwhm, H_EXPO, T_DOUBLE, "%-13G"},
  {"SEXNNWF ", "S/G classification NNW filename",
	cat.nnw_name, H_STRING, T_STRING, "%-18s"},
  {"SEXGAIN ", "Gain used (e-/ADU)",
	&prefs.gain, H_EXPO, T_DOUBLE, "%6.2f"},
  {"SEXFLTR ", "Detection filtering activated (flag)",
	&prefs.filter_flag, H_BOOL, T_LONG, "%1s"},
  {"SEXFILTN", "Filter filename",
	cat.filter_name, H_STRING, T_STRING, "%-18s"},
/*
  {"SEXDETT ", "Detection type",
	&prefs.detect_type, H_STRING, T_STRING, "%-18s"},
*/
  {"SEXMINAR", "Minimum area used for detection (pixels)",
	&prefs.ext_minarea, H_INT, T_LONG, "%5d"},
  {"SEXDBLDN", "Number of deblending thresholds",
	&prefs.deblend_nthresh, H_INT, T_LONG, "%3d"},
  {"SEXDBLDC", "Minimum contrast used for deblending",
	&prefs.deblend_mincont, H_FLOAT, T_DOUBLE, "%8f"},
  {"SEXCLN  ", "Cleaning activated (flag)",
	&prefs.clean_flag, H_BOOL, T_LONG, "%1s"},
  {"SEXCLNPA", "Cleaning parameter",
	&prefs.clean_param, H_FLOAT, T_DOUBLE, "%5.2f"},
  {"SEXCLNST", "Cleaning stack-size",
	&prefs.clean_stacksize, H_INT, T_LONG, "%6d"},
  {"SEXAPED1", "Fixed photometric aperture #1 (pixels)",
	&prefs.apert[0], H_FLOAT, T_DOUBLE, "%7.1f"},
  {"SEXAPED2", "Fixed photometric aperture #2 (pixels)",
	&prefs.apert[1], H_FLOAT, T_DOUBLE, "%7.1f"},
  {"SEXAPED3", "Fixed photometric aperture #3 (pixels)",
	&prefs.apert[2], H_FLOAT, T_DOUBLE, "%7.1f"},
  {"SEXAPED4", "Fixed photometric aperture #4 (pixels)",
	&prefs.apert[3], H_FLOAT, T_DOUBLE, "%7.1f"},
  {"SEXAUTP1", "Parameter #1 used for automatic magnitudes",
	&prefs.autoparam[0], H_FLOAT, T_DOUBLE, "%4.1f"},
  {"SEXAUTP2", "Parameter #2 used for automatic magnitudes",
	&prefs.autoparam[1], H_FLOAT, T_DOUBLE, "%4.1f"},
  {"SEXSATLV", "Saturation level used for flagging (ADU)",
	&prefs.satur_level, H_EXPO, T_DOUBLE, "%-13G"},
  {"SEXMGZPT", "Zero-point used for magnitudes",
	&prefs.mag_zeropoint, H_FLOAT, T_DOUBLE, "%8.4f"},
  {"SEXMGGAM", "Gamma used for photographic photometry",
	&prefs.mag_gamma, H_FLOAT, T_DOUBLE, "%4.2f"},
  {"SEXBKGSX", "Mesh width used for background mapping (pixels)",
	&thefield1.backw, H_INT, T_LONG, "%5d"},
  {"SEXBKGSY", "Mesh height used for background mapping (pixels)",
	&thefield1.backh, H_INT, T_LONG, "%5d"},
  {"SEXBKGFX", "Mask width used for background map filtering",
	&thefield1.nbackfx, H_INT, T_LONG, "%3d"},
  {"SEXBKGFY", "Mask height used for background map filtering",
	&thefield1.nbackfy, H_INT, T_LONG, "%3d"},
/*
  {"SEXPBKGT", "Background type for photometry",
	 &prefs.pback_type, H_STRING, T_STRING, "-18s"},
*/
  {"SEXPBKGS", "Thickness used for local background (pixels)",
	&prefs.pback_size, H_INT, T_LONG, "%3d"},
  {"SEXPIXSK", "Pixel stack-size (pixels)",
	&prefs.mem_pixstack, H_INT, T_LONG, "%8d"},
  {"SEXFBUFS", "Image-buffer height (scanlines)",
	&prefs.mem_bufsize, H_INT, T_LONG, "%5d"},
  {"SEXMWSCL", "Measurement-weight re-scaling factor",
	&thewfield1.sigfac, H_EXPO, T_FLOAT, "%-13G"},
  {"SEXDWSCL", "Detection-weight re-scaling factor",
	&thewfield2.sigfac, H_EXPO, T_FLOAT, "%-13G"},
  {""}};

