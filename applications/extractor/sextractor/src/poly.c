 /*
 				poly.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	A program using Polynomials
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Polynomial fitting
*
*	Last modify:	28/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>

#include	"define.h"
#include	"globals.h"
#include	"poly.h"


/****** poly_init ************************************************************
PROTO   polystruct *poly_init(int *dim, int ndim)
PURPOSE Allocate and initialize a polynom structure.
INPUT   1D array of degrees of the polynom,
        number of dimensions.
OUTPUT  polystruct pointer.
NOTES   -.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 16/07/98
 ***/
polystruct	*poly_init(int *dim, int ndim)
  {
   polystruct	*poly;
   char		str[MAXCHAR];
   int		d;

  QCALLOC(poly, polystruct, 1);
  if ((poly->ndim=ndim) > POLY_MAXDIM)
    {
    sprintf(str, "The dimensionality of the polynom (%d) exceeds the maximum\n"
		"allowed one (%d)", ndim, POLY_MAXDIM);
    error(EXIT_FAILURE, "*Error*: ", str);
    }

  QMALLOC(poly->degree, int, poly->ndim);

/* Fill the "degree" array and compute the total number of coefficients */
  poly->ncoeff = 1;
  for (d=0; d<ndim; d++)
    {
    if ((poly->degree[d]=*(dim++))>POLY_MAXDEGREE)
      {
      sprintf(str, "The degree of the polynom (%d) exceeds the maximum\n"
		"allowed one (%d)", poly->degree[d], POLY_MAXDEGREE);
      error(EXIT_FAILURE, "*Error*: ", str);
      }
    poly->ncoeff *= (poly->degree[d]+1);
    }

  QMALLOC(poly->basis, double, poly->ncoeff);
  QMALLOC(poly->coeff, double, poly->ncoeff);

  return poly;
  }


/****** poly_end *************************************************************
PROTO   void poly_end(polystruct *poly)
PURPOSE Free a polynom structure and everything it contains.
INPUT   polystruct pointer.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 01/07/98
 ***/
void	poly_end(polystruct *poly)
  {
  free(poly->coeff);
  free(poly->basis);
  free(poly->degree);
  free(poly);
  }


/****** poly_func ************************************************************
PROTO   double poly_func(polystruct *poly, double *pos)
PURPOSE Evaluate a multidimensional polynom.
INPUT   polystruct pointer,
        pointer to the 1D array of input vector data.
OUTPUT  Polynom value.
NOTES   Values of the basis functions are updated in poly->basis.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 02/07/98
 ***/
double	poly_func(polystruct *poly, double *pos)
  {
   static double	xpol[POLY_MAXDIM+1];
   double		*post, *xpolt, *basis, *coeff, val;
   static int		expo[POLY_MAXDIM+1];
   int			*expot, *degree,*degreet,
			d,t;

/* Prepare the vectors and counters */
  basis = poly->basis;
  coeff = poly->coeff;
  degree = poly->degree;
  xpolt = xpol;
  post = pos;
  expot = expo;
  for (d=poly->ndim; d--;)
    {
    *(xpolt++) = *(post++);
    *(expot++) = 0;
    }

/* The constant term is handled separately */
  val = *(coeff++);
  *(basis++) = 1.0;

/* Compute the rest of the polynom */
  for (t=poly->ncoeff; --t; )
    {
/*-- xpol[0] contains the current product of the x^n's */
    val += *xpol**(coeff++);
    *(basis++) = *xpol;

/*-- A complex recursion between terms of the polynom speeds up computations */
    post = pos;
    expot = expo;
    xpolt = xpol;
    degreet = degree;
    for (d=poly->ndim; d--; expot++)
      if ((++*expot) < *(degreet++))
        {
        *(xpolt++) *= *(post++);
        break;
        }
      else
        {
        *expot = -1;
        *xpolt = *(xpolt+1);
        xpolt++;
        post++;
        }
    }

  return val;
  }


/****** poly_fit *************************************************************
PROTO   void poly_fit(polystruct *poly, double *x, double *y, double *w,
        int ndata, double *extbasis)
PURPOSE Least-Square fit of a multidimensional polynom to weighted data.
INPUT   polystruct pointer,
        pointer to the (pseudo)2D array of inputs to basis functions,
        pointer to the 1D array of data values,
        pointer to the 1D array of data weights,
        number of data points,
        pointer to a (pseudo)2D array of computed basis function values.
OUTPUT  Chi2 of the fit.
NOTES   If different from NULL, extbasis can be provided to store the
        values of the basis functions. If x==NULL and extbasis!=NULL, the
        precomputed basis functions stored in extbasis are used (which saves
        CPU).
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 28/11/98
 ***/
void	poly_fit(polystruct *poly, double *x, double *y, double *w, int ndata,
		double *extbasis)
  {
   double	*alpha,*alphat, *beta,*betat, *basis,*basis1,*basis2, *coeff,
		*extbasist,
		val,wval,yval;
   int		ncoeff, ndim, matsize,
		i,j,n;

  if (!x && !extbasis)
    error(EXIT_FAILURE, "*Internal Error*: One of x or extbasis should be "
	"different from NULL\nin ", "poly_func()");
  ncoeff = poly->ncoeff;
  ndim = poly->ndim;
  matsize = ncoeff*ncoeff;
  basis = poly->basis;
  extbasist = extbasis;
  QCALLOC(alpha, double, matsize);
  QCALLOC(beta, double, ncoeff);

/* Build the covariance matrix */
  for (n=ndata; n--;)
    {
    if (x)
      {
/*---- If x!=NULL, compute the basis functions */
      poly_func(poly, x);
      x+=ndim;
/*---- If, in addition, extbasis is provided, then fill it */
      if (extbasis)
        for (basis1=basis,j=ncoeff; j--;)
          *(extbasist++) = *(basis1++);
      }
    else
/*---- If x==NULL, then rely on pre-computed basis functions */
      for (basis1=basis,j=ncoeff; j--;)
        *(basis1++) = *(extbasist++);

    basis1 = basis;
    wval = *(w++);
    yval = *(y++);
    betat = beta;
    alphat = alpha;
    for (j=ncoeff; j--;)
      {
      val = *(basis1++)*wval;
      *(betat++) += val*yval;
      for (basis2=basis,i=ncoeff; i--;)
        *(alphat++) += val**(basis2++);
      }
    }

/* Solve the system */
  cholsolve(alpha,beta,ncoeff);

  free(alpha);

/* Now fill the coeff array with the result of the fit */
  betat = beta;
  coeff = poly->coeff;
  for (j=ncoeff; j--;)
    *(coeff++) = *(betat++);

  free(beta);

  return;
  }


/****** cholsolve *************************************************************
PROTO   void cholsolve(double *a, double *b, int n)
PURPOSE Solve a system of linear equations, using Cholesky decomposition.
INPUT   Pointer to the (pseudo 2D) matrix of coefficients,
        pointer to the 1D column vector,
        matrix size.
OUTPUT  -.
NOTES   Based on Numerical Recipes, 2nd ed. (Chap 2.9). The matrix of
        coefficients must be symmetric and positive definite.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 02/07/98
 ***/
void	cholsolve(double *a, double *b, int n)
  {
   double	*p, *x, sum;
   int		i,j,k;

/* Allocate memory to store the diagonal elements */
  QMALLOC(p, double, n);

/* Cholesky decomposition */
  for (i=0; i<n; i++)
    for (j=0; j<n; j++)
      {
      for (sum=a[i*n+j],k=i-1; k>=0; k--)
        sum -= a[i*n+k]*a[j*n+k];
      if (i==j)
        {
        if (sum <= 0.0)
          error(EXIT_FAILURE, "*Error*: Non positive definite matrix in ",
				"cholsolve()");
        p[i] = sqrt(sum);
        }
      else
        a[j*n+i] = sum/p[i];
      }

/* Solve the system */
  x = b;		/* Just to save memory:  the solution replaces b */
  for (i=0; i<n; i++)
    {
    for (sum=b[i],k=i-1; k>=0; k--)
      sum -= a[i*n+k]*x[k];
    x[i] = sum/p[i];
    }

  for (i=n-1; i>=0; i--)
    {
    for (sum=x[i],k=i+1; k<n; k++)
      sum -= a[k*n+i]*x[k];
    x[i] = sum/p[i];
    }

  free(p);

  return;
  }

