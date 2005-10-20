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
*	Last modify:	21/09/2004
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
VERSION 08/03/2003
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
      if ((g=group[d])>=ngroup)
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
VERSION 03/03/2004
 ***/
double	poly_func(polystruct *poly, double *pos)
  {
   double	xpol[POLY_MAXDIM+1];
   double      	*post, *xpolt, *basis, *coeff, xval;
   long double	val;
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
/*-- Not too good for roundoff errors (prefer Horner's), but much easier for */
/*-- multivariate polynomials: this is why we use a long double accumulator */
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

  return (double)val;
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
   double	offset[POLY_MAXDIM],
		*alpha,*alphat, *beta,*betat, *basis,*basis1,*basis2, *coeff,
		*extbasist,*xt,
		val,wval,yval;
   int		ncoeff, ndim, matsize,
		d,i,j,n;

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

/* Subtract an average offset to maintain precision */
  if (x)
    {
    for (d=0; d<ndim; d++)
      offset[d] = 0.0;
    xt = x;
    for (n=ndata; n--;)
      for (d=0; d<ndim; d++)
        offset[d] += *(xt++);
    for (d=0; d<ndim; d++)
      offset[d] /= (double)ndata;    
    xt = x;
    for (n=ndata; n--;)
      for (d=0; d<ndim; d++)
        *(xt++) -= offset[d];
    }
 
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
  poly_solve(alpha,beta,ncoeff);

  free(alpha);

/* Now fill the coeff array with the result of the fit */
  betat = beta;
  coeff = poly->coeff;
  for (j=ncoeff; j--;)
    *(coeff++) = *(betat++);

  poly_addcste(poly, offset);

  free(beta);

  return;
  }


/****** poly_addcste *********************************************************
PROTO   void poly_addcste(polystruct *poly, double *cste)
PURPOSE Modify matrix coefficients to mimick the effect of adding a cst to
	the input of a polynomial.
INPUT   Pointer to the polynomial structure,
        Pointer to the vector of cst.
OUTPUT  -.
NOTES   Requires quadruple-precision.
AUTHOR  E. Bertin (IAP)
VERSION 03/03/2004
 ***/
void	poly_addcste(polystruct *poly, double *cste)
  {
   long double	*acoeff;
   double	*coeff,*mcoeff,*mcoefft,
		val;
   int		*mpowers,*powers,*powerst,*powerst2,
		i,j,n,p, denum, flag, maxdegree, ncoeff, ndim;

  ncoeff = poly->ncoeff;
  ndim = poly->ndim;
  maxdegree = 0;
  for (j=0; j<poly->ngroup; j++)
    if (maxdegree < poly->degree[j])
      maxdegree = poly->degree[j];
  maxdegree++;		/* Actually we need maxdegree+1 terms */
  QCALLOC(acoeff, long double, ncoeff);
  QCALLOC(mcoeff, double, ndim*maxdegree);
  QCALLOC(mpowers, int, ndim);
  mcoefft = mcoeff;		/* To avoid gcc -Wall warnings */
  powerst = powers = poly_powers(poly);
  coeff = poly->coeff;
  for (i=0; i<ncoeff; i++)
    {
    for (j=0; j<ndim; j++)
      {
      mpowers[j] = n = *(powerst++);
      mcoefft = mcoeff+j*maxdegree+n;
      denum = 1;
      val = 1.0;
      for (p=n+1; p--;)
        {
        *(mcoefft--) = val;
        val *= (cste[j]*(n--))/(denum++);	/* This is C_n^p X^(n-p) */
        }
      }
/*-- Update all valid coefficients */
    powerst2 = powers;
    for (p=0; p<ncoeff; p++)
      {
/*---- Check that this combination of powers is included in the series above */
      flag = 0;
      for (j=0; j<ndim; j++)
        if (mpowers[j] < powerst2[j])
	  {
          flag = 1;
          powerst2 += ndim;
          break;
          }
      if (flag == 1)
        continue;
      val = 1.0;
      mcoefft = mcoeff;
      for (j=ndim; j--; mcoefft += maxdegree)
        val *= mcoefft[*(powerst2++)];
      acoeff[i] += val*coeff[p];
/*
printf("%g \n", val);
*/
      }
    }

/* Add the new coefficients to the previous ones */

  for (i=0; i<ncoeff; i++)
{
/*
printf("%g %g\n", coeff[i], (double)acoeff[i]);
*/
    coeff[i] = (double)acoeff[i];
}

  free(acoeff);
  free(mcoeff);
  free(mpowers);
  free(powers);

  return;
  }

/****** poly_solve ************************************************************
PROTO   void poly_solve(double *a, double *b, int n)
PURPOSE Solve a system of linear equations, using Cholesky decomposition or
	SVD (if the former fails due to hidden correlation between variables).
INPUT   Pointer to the (pseudo 2D) matrix of coefficients,
        pointer to the 1D column vector,
        matrix size.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 21/09/2004
 ***/
void	poly_solve(double *a, double *b, int n)
  {
   double	*vmat,*wmat;

  if (cholsolve(a,b,n))
    {
    QMALLOC(vmat, double, n*n);
    QMALLOC(wmat, double, n);
    svdsolve(a, b, n,n, vmat,wmat);
    free(vmat);
    free(wmat);
    }

  return;
  }

/****** cholsolve *************************************************************
PROTO   void cholsolve(double *a, double *b, int n)
PURPOSE Solve a system of linear equations, using Cholesky decomposition.
INPUT   Pointer to the (pseudo 2D) matrix of coefficients,
        pointer to the 1D column vector,
        matrix size.
OUTPUT  -1 if the matrix is not positive-definite, 0 otherwise.
NOTES   Based on Numerical Recipes, 2nd ed. (Chap 2.9). The matrix of
        coefficients must be symmetric and positive definite.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION	28/10/2003
 ***/
int	cholsolve(double *a, double *b, int n)
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
	  {
          free(p);
          return -1;
          }
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

  return 0;
  }


/****** svdsolve *************************************************************
PROTO   void svdsolve(double *a, double *b, int m, int n, double *vmat,
		double *wmat)
PURPOSE General least-square fit A.x = b, based on Singular Value
	Decomposition (SVD).
	Loosely adapted from Numerical Recipes in C, 2nd Ed. (p. 671).
INPUT   Pointer to the (pseudo 2D) matrix of coefficients,
        pointer to the 1D column vector (replaced by solution in output),
        number of matrix rows,
	number of matrix columns,
	pointer to the (pseudo 2D) SVD matrix,
	pointer to the diagonal (1D) matrix of singular values.	
OUTPUT  -.
NOTES   Loosely adapted from Numerical Recipes in C, 2nd Ed. (p. 671). The a
	and v matrices are transposed with respect to the N.R. convention.
AUTHOR  E. Bertin (IAP)
VERSION 26/12/2003
 ***/
void svdsolve(double *a, double *b, int m, int n, double *vmat, double *wmat)
  {
#define MAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
        (maxarg1) : (maxarg2))
#define PYTHAG(a,b)     ((at=fabs(a)) > (bt=fabs(b)) ? \
                                  (ct=bt/at,at*sqrt(1.0+ct*ct)) \
                                : (bt ? (ct=at/bt,bt*sqrt(1.0+ct*ct)): 0.0))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))
#define TOL             1.0e-11
   void	qerror(char *msg1, char *msg2);

   int                  flag,i,its,j,jj,k,l,mmi,nm, nml;
   double               *w,*ap,*ap0,*ap1,*ap10,*rv1p,*vp,*vp0,*vp1,*vp10,
                        *bp,*tmpp, *rv1,*tmp, *sol,
			c,f,h,s,x,y,z,
                        anorm, g, scale,
                        at,bt,ct,maxarg1,maxarg2,
                        thresh, wmax;

  anorm = g = scale = 0.0;
  if (m < n)
    qerror("*Error*: Not enough rows for solving the system ", "in svdfit()");
  
  sol = b;	/* The solution overwrites the input column matrix */
  QMALLOC(rv1, double, n);
  QMALLOC(tmp, double, n);
  l = nm = nml = 0;	/* To avoid gcc -Wall warnings */
  for (i=0;i<n;i++)
    {
    l = i+1;
    nml = n-l;
    rv1[i] = scale*g;
    g = s = scale = 0.0;
    if ((mmi = m - i) > 0)
      {
      ap = ap0 = a+i*(m+1);
      for (k=mmi;k--;)
        scale += fabs(*(ap++));
      if (scale)
        {
        for (ap=ap0,k=mmi; k--; ap++)
          {
          *ap /= scale;
          s += *ap**ap;
          }
        f = *ap0;
        g = -SIGN(sqrt(s),f);
        h = f*g-s;
        *ap0 = f-g;
        ap10 = a+l*m+i;
        for (j=nml;j--; ap10+=m)
          {
          for (s=0.0,ap=ap0,ap1=ap10,k=mmi; k--;)
            s += *(ap1++)**(ap++);
          f = s/h;
          for (ap=ap0,ap1=ap10,k=mmi; k--;)
            *(ap1++) += f**(ap++);
          }
        for (ap=ap0,k=mmi; k--;)
          *(ap++) *= scale;
        }
      }
    wmat[i] = scale*g;
    g = s = scale = 0.0;
    if (i < m && i+1 != n)
      {
      ap = ap0 = a+i+m*l;
      for (k=nml;k--; ap+=m)
        scale += fabs(*ap);
      if (scale)
        {
        for (ap=ap0,k=nml;k--; ap+=m)
          {
          *ap /= scale;
          s += *ap**ap;
          }
        f=*ap0;
        g = -SIGN(sqrt(s),f);
        h=f*g-s;
        *ap0=f-g;
        rv1p = rv1+l;
        for (ap=ap0,k=nml;k--; ap+=m)
          *(rv1p++) = *ap/h;
        ap10 = a+l+m*l;
        for (j=m-l; j--; ap10++)
          {
          for (s=0.0,ap=ap0,ap1=ap10,k=nml; k--; ap+=m,ap1+=m)
            s += *ap1**ap;
          rv1p = rv1+l;
          for (ap1=ap10,k=nml;k--; ap1+=m)
            *ap1 += s**(rv1p++);
          }
        for (ap=ap0,k=nml;k--; ap+=m)
          *ap *= scale;
        }
      }
    anorm=MAX(anorm,(fabs(wmat[i])+fabs(rv1[i])));
    }

  for (i=n-1;i>=0;i--)
    {
    if (i < n-1)
      {
      if (g)
        {
        ap0 = a+l*m+i;
        vp0 = vmat+i*n+l;
        vp10 = vmat+l*n+l;
        g *= *ap0;
        for (ap=ap0,vp=vp0,j=nml; j--; ap+=m)
          *(vp++) = *ap/g;
        for (j=nml; j--; vp10+=n)
          {
          for (s=0.0,ap=ap0,vp1=vp10,k=nml; k--; ap+=m)
            s += *ap**(vp1++);
          for (vp=vp0,vp1=vp10,k=nml; k--;)
            *(vp1++) += s**(vp++);
          }
        }
      vp = vmat+l*n+i;
      vp1 = vmat+i*n+l;
      for (j=nml; j--; vp+=n)
        *vp = *(vp1++) = 0.0;
      }
    vmat[i*n+i]=1.0;
    g=rv1[i];
    l=i;
    nml = n-l;
    }

  for (i=(m<n?m:n); --i>=0;)
    {
    l=i+1;
    nml = n-l;
    mmi=m-i;
    g=wmat[i];
    ap0 = a+i*m+i;
    ap10 = ap0 + m;
    for (ap=ap10,j=nml;j--;ap+=m)
      *ap=0.0;
    if (g)
      {
      g=1.0/g;
      for (j=nml;j--; ap10+=m)
        {
        for (s=0.0,ap=ap0,ap1=ap10,k=mmi; --k;)
              s += *(++ap)**(++ap1);
        f = (s/(*ap0))*g;
        for (ap=ap0,ap1=ap10,k=mmi;k--;)
          *(ap1++) += f**(ap++);
        }
      for (ap=ap0,j=mmi;j--;)
        *(ap++) *= g;
      }
    else
      for (ap=ap0,j=mmi;j--;)
        *(ap++)=0.0;
    ++(*ap0);
    }

  for (k=n; --k>=0;)
      {
      for (its=0;its<100;its++)
        {
        flag=1;
        for (l=k;l>=0;l--)
          {
          nm=l-1;
          if (fabs(rv1[l])+anorm == anorm)
            {
            flag=0;
            break;
            }
          if (fabs(wmat[nm])+anorm == anorm)
            break;
          }
        if (flag)
          {
          c=0.0;
          s=1.0;
          ap0 = a+nm*m;
          ap10 = a+l*m;
          for (i=l; i<=k; i++,ap10+=m)
            {
            f=s*rv1[i];
            if (fabs(f)+anorm == anorm)
              break;
            g=wmat[i];
            h=PYTHAG(f,g);
            wmat[i]=h;
            h=1.0/h;
            c=g*h;
            s=(-f*h);
            for (ap=ap0,ap1=ap10,j=m; j--;)
              {
              z = *ap1;
              y = *ap;
              *(ap++) = y*c+z*s;
              *(ap1++) = z*c-y*s;
              }
            }
          }
        z=wmat[k];
        if (l == k)
          {
          if (z < 0.0)
            {
            wmat[k] = -z;
            vp = vmat+k*n;
            for (j=n; j--; vp++)
              *vp = (-*vp);
            }
          break;
          }
        if (its == 99)
          qerror("*Error*: No convergence in 100 SVD iterations ",
                "in svdfit()");
        x=wmat[l];
        nm=k-1;
        y=wmat[nm];
        g=rv1[nm];
        h=rv1[k];
        f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
        g=PYTHAG(f,1.0);
        f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
        c=s=1.0;
        ap10 = a+l*m;
        vp10 = vmat+l*n;
        for (j=l;j<=nm;j++,ap10+=m,vp10+=n)
          {
          i=j+1;
          g=rv1[i];
          y=wmat[i];
          h=s*g;
          g=c*g;
          z=PYTHAG(f,h);
          rv1[j]=z;
          c=f/z;
          s=h/z;
          f=x*c+g*s;
          g=g*c-x*s;
          h=y*s;
          y=y*c;
          for (vp=(vp1=vp10)+n,jj=n; jj--;)
            {
            z = *vp;
            x = *vp1;
            *(vp1++) = x*c+z*s;
            *(vp++) = z*c-x*s;
            }
          z=PYTHAG(f,h);
          wmat[j]=z;
          if (z)
            {
            z=1.0/z;
            c=f*z;
            s=h*z;
            }
          f=c*g+s*y;
          x=c*y-s*g;
          for (ap=(ap1=ap10)+m,jj=m; jj--;)
            {
            z = *ap;
            y = *ap1;
            *(ap1++) = y*c+z*s;
            *(ap++) = z*c-y*s;
            }
          }
        rv1[l]=0.0;
        rv1[k]=f;
        wmat[k]=x;
        }
      }

  wmax=0.0;
  w = wmat;
  for (j=n;j--; w++)
    if (*w > wmax)
      wmax=*w;
  thresh=TOL*wmax;
  w = wmat;
  for (j=n;j--; w++)
    if (*w < thresh)
      *w = 0.0;

  w = wmat;
  ap = a;
  tmpp = tmp;
  for (j=n; j--; w++)
    {
    s=0.0;
    if (*w)
      {
      bp = b;
      for (i=m; i--;)
        s += *(ap++)**(bp++);
      s /= *w;
      }
    else
      ap += m;
    *(tmpp++) = s;
    }

  vp0 = vmat;
  for (j=0; j<n; j++,vp0++)
    {
    s=0.0;
    tmpp = tmp;
    for (vp=vp0,jj=n; jj--; vp+=n)
      s += *vp**(tmpp++);
    sol[j]=s;
    }
/* Free temporary arrays */
  free(tmp);
  free(rv1);

  return;
  }

#undef SIGN
#undef MAX
#undef PYTHAG
#undef TOL

/****** poly_powers ***********************************************************
PROTO   int *poly_powers(polystruct *poly)
PURPOSE	Return an array of powers of polynom terms
INPUT   polystruct pointer,
OUTPUT  Pointer to an array of polynom powers (int *), (ncoeff*ndim numbers).
NOTES   The returned pointer is mallocated.
AUTHOR  E. Bertin (IAP)
VERSION 23/10/2003
 ***/
int	*poly_powers(polystruct *poly)
  {
   int		expo[POLY_MAXDIM+1], gexpo[POLY_MAXDIM+1];
   int	       	*expot, *degree,*degreet, *group,*groupt, *gexpot,
		*powers, *powerst,
		d,g,t, ndim;

/* Prepare the vectors and counters */
  ndim = poly->ndim;
  group = poly->group;
  degree = poly->degree;
  QMALLOC(powers, int, ndim*poly->ncoeff);
  if (ndim)
    {
    for (expot=expo, d=ndim; --d;)
      *(++expot) = 0;
    for (gexpot=gexpo, degreet=degree, g=poly->ngroup; g--;)
      *(gexpot++) = *(degreet++);
    if (gexpo[*group])
      gexpo[*group]--;
    }

/* The constant term is handled separately */
  powerst = powers;
  for (d=0; d<ndim; d++)
    *(powerst++) = 0;
  *expo = 1;

/* Compute the rest of the polynom */
  for (t=poly->ncoeff; --t; )
    {
    for (d=0; d<ndim; d++)
      *(powerst++) = expo[d];
/*-- A complex recursion between terms of the polynom speeds up computations */
    groupt = group;
    expot = expo;
    for (d=0; d<ndim; d++, groupt++)
      if (gexpo[*groupt]--)
        {
        ++*(expot++);
        break;
        }
      else
        {
        gexpo[*groupt] = *expot;
        *(expot++) = 0;
        }
    }

  return powers;
  }

