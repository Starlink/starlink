 /*
 				key.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Keyword structure.
*
*	Last modify:	14/12/2004
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*--------------------------------- constants -------------------------------*/

#define         FIND_STRICT     0
#define         FIND_NOSTRICT   1

/*--------------------------- structure definitions -------------------------*/
/* Preference keyword */
typedef struct
  {
  char		name[32];
  enum  {P_FLOAT, P_INT, P_STRING, P_BOOL, P_KEY, P_INTLIST, P_FLOATLIST,
	P_BOOLLIST, P_KEYLIST, P_STRINGLIST} type;
  void		*ptr;			/* Pointer to the keyword value */
  int		imin, imax;		/* Range for int's */
  double	dmin, dmax;		/* Range for doubles */
  char		keylist[32][32];	/* List of keywords */
  int           nlistmin;		/* Minimum number of list members */
  int           nlistmax; 		/* Maximum number of list members */
  int		*nlistptr;		/* Ptr to store the nb of read params*/
  int		flag;
  }	pkeystruct;

/*---------------------------------- protos --------------------------------*/

int	findkeys(char *str, char key[][32], int mode);

