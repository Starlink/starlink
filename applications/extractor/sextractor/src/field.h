 /*
 				field.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Handling of field structures.
*
*	Last modify:	22/03/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*------------------------------ field flags -------------------------------*/
#define		DETECT_FIELD	0x01	/* Detection */
#define		MEASURE_FIELD	0x02	/* Measurement */
#define		FLAG_FIELD	0x04	/* Flagging */
#define		RMS_FIELD	0x08	/* Weighting with std deviations */
#define		VAR_FIELD	0x10	/* Weighting with variances */
#define		WEIGHT_FIELD	0x20	/* Weighting with weights */
#define		BACKRMS_FIELD	0x40	/* Weighting from a backrms matrix */
#define		INTERP_FIELD	0x80	/* Purely interpolated data */


