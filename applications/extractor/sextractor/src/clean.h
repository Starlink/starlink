 /*
 				clean.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory & ESO
*
*	Contents:	functions that remove spurious detections from the
*			catalog
*
*	Last modify:	27/03/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*------------------------------ definitions --------------------------------*/

#define		CLEAN_ZONE		10.0	/* zone (in sigma) to */
						/* consider for processing */

/*------------------------------- variables ---------------------------------*/

objliststruct	*cleanobjlist;		/* laconic, isn't it? */

/*------------------------------- functions ---------------------------------*/

extern void	addcleanobj(objstruct *),
		endclean(void),
		initclean(void),
		subcleanobj(int);

extern int	addobj(int, objliststruct *, objliststruct *),
		clean(int, objliststruct *);

