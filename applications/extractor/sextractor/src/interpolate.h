 /*
 				interpolate.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Include file for interpolate.c.
*
*	Last modify:	29/04/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*---------------------------------- protos --------------------------------*/

extern void		end_interpolate(picstruct *field),
			init_interpolate(picstruct *field,
				int xtimeout, int ytimeout),
			interpolate(picstruct *field, picstruct *wfield,
				PIXTYPE *data, PIXTYPE *wdata);

