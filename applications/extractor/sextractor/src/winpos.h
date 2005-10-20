 /*
 				winpos.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP
*
*	Contents:	Include file for winpos.c.
*
*	Last modify:	25/08/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*----------------------------- Internal constants --------------------------*/

#define	WINPOS_NITERMAX	16	/* Maximum number of steps */
#define	WINPOS_NSIG	4	/* Measurement radius */
#define	WINPOS_OVERSAMP	3	/* oversampling in each dimension */
#define	WINPOS_STEPMIN	0.001	/* Minimum change in position for continueing*/
#define	WINPOS_GRADFAC	2.0	/* Gradient descent acceleration factor */

/* NOTES:
One must have:
	WINPOS_NITERMAX >= 1
	WINPOS_OVERSAMP >= 1
*/

/*------------------------------- functions ---------------------------------*/
extern void	compute_winpos(picstruct *field, picstruct *wfield,
			       objstruct *obj);
