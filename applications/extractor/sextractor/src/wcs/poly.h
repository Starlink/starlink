 /*
 				poly.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	A program using polynomial fits
*
*	Author:		E.BERTIN (IAP) 
*
*	Contents:	Include for poly.c
*
*	Last modify:	05/04/99
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*--------------------------------- constants -------------------------------*/

#define	POLY_MAXDIM		4	/* Max dimensionality of polynom */
#define POLY_MAXDEGREE		10	/* Max degree of the polynom */

/*---------------------------------- macros ---------------------------------*/

/*--------------------------- structure definitions -------------------------*/

typedef struct poly
  {
  double	*basis;		/* Current values of the basis functions */
  double	*coeff;		/* Polynom coefficients */
  int		ncoeff;		/* Number of coefficients */
  int		*group;		/* Groups */
  int		ndim;		/* dimensionality of the polynom */
  int		*degree;	/* Degree in each group */
  int		ngroup;		/* Number of different groups */
  }	polystruct;

/*---------------------------------- protos --------------------------------*/

extern polystruct	*poly_init(int *group,int ndim,int *degree,int ngroup);

extern double			poly_func(polystruct *poly, double *pos);

extern void		cholsolve(double *a, double *b, int n),
			poly_end(polystruct *poly),
			poly_fit(polystruct *poly, double *x, double *y,
				double *w, int ndata, double *extbasis);

