 /*
 				assoc.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	Include file for assoc.c.
*
*	Last modify:	25/06/97
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/


#define		ASSOC_BUFINC	65536	/* Assoc buffer increment (bytes) */

/*--------------------------------- typedefs --------------------------------*/

typedef struct structassoc
  {
  float		*list;			/* Pointer to the list of data */
  int		nobj;			/* Number of data rows */
  int		ncol;			/* Total number of columns per row */
  int		ndata;			/* Number of retained cols per row */
  int		*hash;			/* Pointer to the hash table */
  float		*data;			/* Copy of current parameters */
  float		radius;			/* Radius of search for association */
  }             assocstruct;

/*------------------------------ Prototypes ---------------------------------*/

assocstruct	*load_assoc(char *filename);

int		do_assoc(picstruct *field, float x, float y);

void		init_assoc(picstruct *field),
		end_assoc(picstruct *field),
		sort_assoc(picstruct *field, assocstruct *assoc);
