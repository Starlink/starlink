 /*
 				flag.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	include to flag.c (external flagging).
*
*	Last modify:	28/04/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*--------------------------- Internal constants ----------------------------*/

#define	FLAG_BUFSIZE	32	/* Flag-stacksize at start */

/*------------------------------- functions ---------------------------------*/

void	getflags(objstruct *obj, pliststruct *pixel),
	mergeflags(objstruct *objmaster, objstruct *objslave);

