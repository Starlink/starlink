/*
 				weight.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Include file for weight.c.
*
*	Last modify:	27/08/97
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*---------------------------------- protos --------------------------------*/

extern picstruct	*newweight(char *filename, picstruct *reffield,
				weightenum wtype);

void			rms_to_var(picstruct *wfield, PIXTYPE *data, int npix),
			weight_to_var(picstruct *wfield, PIXTYPE *data,
				int npix);

