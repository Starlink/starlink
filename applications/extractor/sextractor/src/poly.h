 /*
 				poly.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	A program using polynomial fits
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO) 
*
*	Contents:	Include for poly.c
*
*	Last modify:	16/07/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*--------------------------------- constants -------------------------------*/

#define	POLY_MAXDIM		4	/* Max dimensionality of polynom */
#define POLY_MAXDEGREE		6	/* Max degree of the polynom */

/*---------------------------------- macros ---------------------------------*/

/*--------------------------- structure definitions -------------------------*/

typedef struct poly
  {
  double	*basis;		/* Current values of the basis functions */
  double	*coeff;		/* Polynom coefficients */
  double	ncoeff;		/* Number of coefficients */
  int		ndim;		/* dimensionality of the polynom */
  int		*degree;	/* Degree in each dimension */
  }	polystruct;

/*---------------------------------- protos --------------------------------*/

extern polystruct	*poly_init(int *dim, int ndim);

extern double		poly_fit(polystruct *poly, double *x, double *y,
				double *w, int ndata, double *extbasis),
			poly_func(polystruct *poly, double *pos);

extern void		cholsolve(double *a, double *b, int n),
			poly_end(polystruct *poly);

