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
*	Last modify:	27/04/99
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*---------------------------------- protos --------------------------------*/

extern picstruct	*newweight(char *filename, picstruct *reffield,
				weightenum wtype);

void			weight_to_var(picstruct *wfield, PIXTYPE *data,
				int npix);

