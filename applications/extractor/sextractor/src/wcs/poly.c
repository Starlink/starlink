 /*
 				poly.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	A program using Polynomials
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Polynomial fitting
*
*	Last modify:	25/05/2000
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"poly.h"


#define	QCALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)calloc((size_t)(nel),sizeof(typ)))) \
		  qerror("Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define	QMALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)malloc((size_t)(nel)*sizeof(typ)))) \
		  qerror("Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

/********************************* qerror ************************************/
/*
I hope it will never be used!
*/
void	qerror(char *msg1, char *msg2)
  {
  fprintf(stderr, "\n> %s%s\n\n",msg1,msg2);
  exit(-1);
  }


/****** poly_init ************************************************************
PROTO   polystruct *poly_init(int *group, int ndim, int *degree, int ngroup)
PURPOSE Allocate and initialize a polynom structure.
INPUT   1D array containing the group for each parameter,
        number of dimensions (parameters),
        1D array with the polynomial degree for each group,
        number of groups.
OUTPUT  polystruct pointer.
NOTES   -.
AUTHOR  E. Bertin (IAP)
VERSION 07/04/2000
 ***/
polystruct	*poly_init(int *group, int ndim, int *degree, int ngroup)
  {
   void	qerror(char *msg1, char *msg2);
   polystruct	*poly;
   char		str[512];
   int		nd[POLY_MAXDIM];
   int		*groupt,
		d,g,n,num,den;

  QCALLOC(poly, polystruct, 1);
  if ((poly->ndim=ndim) > POLY_MAXDIM)
    {
    sprintf(str, "The dimensionality of the polynom (%d) exceeds the maximum\n"
		"allowed one (%d)", ndim, POLY_MAXDIM);
    qerror("*Error*: ", str);
    }

  if (ndim)
    QMALLOC(poly->group, int, poly->ndim);
    for (groupt=poly->group, d=ndim; d--;)
      *(groupt++) = *(group++)-1;

  poly->ngroup = ngroup;
  if (ngroup)
    {
    group = poly->group;	/* Forget the original *group */

    QMALLOC(poly->degree, int, poly->ngroup);

/*-- Compute the number of context parameters for each group */
    memset(nd, 0, ngroup*sizeof(int));
    for (d=0; d<ndim; d++)
      {
      if ((g=group[d])>ngroup)
        qerror("*Error*: polynomial GROUP out of range", "");
      nd[g]++;
      }
    }

/* Compute the total number of coefficients */
  poly->ncoeff = 1;
  for (g=0; g<ngroup; g++)
    {
    if ((d=poly->degree[g]=*(degree++))>POLY_MAXDEGREE)
      {
      sprintf(str, "The degree of the polynom (%d) exceeds the maximum\n"
		"allowed one (%d)", poly->degree[g], POLY_MAXDEGREE);
      qerror("*Error*: ", str);
      }

/*-- There are (n+d)!/(n!d!) coeffs per group, that is Prod_(i<=d) (n+i)/i */
    for (num=den=1, n=nd[g]; d; num*=(n+d), den*=d--);
    poly->ncoeff *= num/den;
    }

  QMALLOC(poly->basis, double, poly->ncoeff);
  QCALLOC(poly->coeff, double, poly->ncoeff);

  return poly;
  }


/****** poly_end *************************************************************
PROTO   void poly_end(polystruct *poly)
PURPOSE Free a polynom structure and everything it contains.
INPUT   polystruct pointer.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 09/04/2000
 ***/
void	poly_end(polystruct *poly)
  {
  if (poly)
    {
    free(poly->coeff);
    free(poly->basis);
    free(poly->degree);
    free(poly->group);
    free(poly);
    }
  }


/****** poly_func ************************************************************
PROTO   double poly_func(polystruct *poly, double *pos)
PURPOSE Evaluate a multidimensional polynom.
INPUT   polystruct pointer,
        pointer to the 1D array of input vector data.
OUTPUT  Polynom value.
NOTES   Values of the basis functions are updated in poly->basis.
AUTHOR  E. Bertin (IAP)
VERSION 07/04/2000
 ***/
double	poly_func(polystruct *poly, double *pos)
  {
   double	xpol[POLY_MAXDIM+1];
   double      	*post, *xpolt, *basis, *coeff, val, xval;
   int		expo[POLY_MAXDIM+1], gexpo[POLY_MAXDIM+1];
   int	       	*expot, *degree,*degreet, *group,*groupt, *gexpot,
			d,g,t, ndim;

/* Prepare the vectors and counters */
  ndim = poly->ndim;
  basis = poly->basis;
  coeff = poly->coeff;
  group = poly->group;
  degree = poly->degree;
  if (ndim)
    {
    for (xpolt=xpol, expot=expo, post=pos, d=ndim; --d;)
      {
      *(++xpolt) = 1.0;
      *(++expot) = 0;
      }
    for (gexpot=gexpo, degreet=degree, g=poly->ngroup; g--;)
      *(gexpot++) = *(degreet++);
    if (gexpo[*group])
      gexpo[*group]--;
    }

/* The constant term is handled separately */
  val = *(coeff++);
  *(basis++) = 1.0;
  *expo = 1;
  *xpol = *pos;

/* Compute the rest of the polynom */
  for (t=poly->ncoeff; --t; )
    {
/*-- xpol[0] contains the current product of the x^n's */
    val += (*(basis++)=*xpol)**(coeff++);
/*-- A complex recursion between terms of the polynom speeds up computations */
    post = pos;
    groupt = group;
    expot = expo;
    xpolt = xpol;
    for (d=0; d<ndim; d++, groupt++)
      if (gexpo[*groupt]--)
        {
        ++*(expot++);
        xval = (*(xpolt--) *= *post);
        while (d--)
          *(xpolt--) = xval;
        break;
        }
      else
        {
        gexpo[*groupt] = *expot;
        *(expot++) = 0;
        *(xpolt++) = 1.0;
        post++;
        }
    }

  return val;
  }


/****** poly_fit *************************************************************
PROTO   double poly_fit(polystruct *poly, double *x, double *y, double *w,
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
        CPU). If w is NULL, all points are given identical weight.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 07/04/2000
 ***/
void	poly_fit(polystruct *poly, double *x, double *y, double *w, int ndata,
		double *extbasis)
  {
   void	qerror(char *msg1, char *msg2);
   double	*alpha,*alphat, *beta,*betat, *basis,*basis1,*basis2, *coeff,
		*extbasist,
		val,wval,yval;
   int		ncoeff, ndim, matsize,
		i,j,n;

  if (!x && !extbasis)
    qerror("*Internal Error*: One of x or extbasis should be "
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
    wval = w? *(w++) : 1.0;
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
VERSION 07/04/2000
 ***/
void	cholsolve(double *a, double *b, int n)
  {
   void	qerror(char *msg1, char *msg2);
   double	*p, *x, sum;
   int		i,j,k;

/* Allocate memory to store the diagonal elements */
  QMALLOC(p, double, n);

/* Cholesky decomposition */
  for (i=0; i<n; i++)
    for (j=i; j<n; j++)
      {
      for (sum=a[i*n+j],k=i-1; k>=0; k--)
        sum -= a[i*n+k]*a[j*n+k];
      if (i==j)
        {
        if (sum <= 0.0)
          qerror("*Error*: Non positive definite matrix in ",
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

