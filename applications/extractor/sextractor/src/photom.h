 /*
 				photom.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	Include file for photom.h.
*
*	Last modify:	17/04/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*----------------------------- Internal constants --------------------------*/

#define	APER_OVERSAMP	5	/* oversampling in each dimension (MAG_APER) */
#define	KRON_NSIG	3*MARGIN_SCALE	/* MAG_AUTO analysis range (number */
					/* of sigma) */
#define	CROWD_THRESHOLD	0.1	/* The OBJ_CROWDED flag is set if photometric*/
				/* contamination may exceed this fraction of */
				/* flux */

/* NOTES:
One must have:	APER_OVERSAMP >= 1
		KRON_NSIG > 0.0
		CROWD_THRESHOLD >= 0
*/

/*------------------------------- functions ---------------------------------*/
extern void	computeaperflux(picstruct *, picstruct *, objstruct *, int),
		computeautoflux(picstruct *, picstruct *, picstruct *,
			picstruct *, objstruct *),
		computeisocorflux(picstruct *, objstruct *),
		computemags(picstruct *, objstruct *);

