 /*
 				som.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	A program using neural networks.
*
*	Author:		E.BERTIN, Institut d'Astrophysique de Paris.
*
*	Contents:	Implementation of Kohonen's Self Organizing Map (V3.0).
*
*	Last modify:	28/05/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"fitscat.h"
#include	"som.h"

/********************************* som_phot **********************************/
/*
Perform SOM-fitting on a detected source: returns node number of the
best-fitting prototype.
*/
void	som_phot(somstruct *som, float back, float backnoise, float gain,
		float dx0, float dy0, float *vector, float clip)
  {
   float	err, errmin, *xnt, *xn0, scale;
   int		i,j,n,nd,jmin,jmin0, *nx, *mul, pos;

  nd = som->neurdim;
/* First we compute the error map */
  if (clip!=0.0)
    if (!som_mkweight(som, back, backnoise, gain))
      {
/*---- If all weights to zero, don't go further! */
      som->stderror = -1.0;
      som->amp = som->sigamp = 0.0;
      if (vector)
        {
        xnt = som->vector;
        for (i=nd; i--;)
          *(vector++) = 99.0;
        }
      return;
      }

/* Use a sound starting point for the gradient search */
/*
  i = nd-1;
  som->vector[i--] = (dy0+0.5)*som->neursize[i]+0.4999;
  som->vector[i] = (dx0+0.5)*som->neursize[i]+0.4999;
  while (i--)
    som->vector[i] = (som->neursize[i]-1)/2.0;
*/
  errmin=BIG;
  for (j=0; j<som->nneur; j++)
    {
    if ((err=som_err(som, (float)j, SOM_NODE))<errmin)
      {
      errmin = err;
      jmin = j;
      }
    }

  mul = som->neurstep;
  nx = som->neursize;
  xnt = som->vector;
  for (i=nd; i--;)
    *(xnt++) = (float)((jmin/(*(mul++)))%(*(nx++)));

/* Gradient search */
  som_conjgrad(som, 1e-6);

/* Now perform the true photometry */
  som->stderror = (float)sqrt(som_err(som, 0.0, SOM_PHOTOM));

/* Store final position vector if requested */
  if (vector)
    {
    xnt = som->vector;
    for (i=nd; i--;)
    *(vector++) = *(xnt++);
    }

  if (clip>0.0)
    {
/*-- Clip deviant pixels if requested */
     float	*input, *inputw, *proto, diff;

    proto = som->proto;
    input = som->input;
    inputw = som->inputw;
    for (i=som->ninput-som->nextrainput;i--; inputw++)
      {
      diff = *(input++)-*(proto++);
      if (*inputw>0.0 && (diff*diff>clip/(*inputw)))
        *inputw /= clip;
      }
    }

  return;
  }


/****************************** som_mkweight *********************************/
/*
Compute weights associated to pixels in a vignet.
*/
int	som_mkweight(somstruct *som, float back, float backnoise, float gain)
  {
   float	*yt, *wt, wstiff, pix, llim,hlim;
   int		i, nima, ngood;

  yt = som->input;
  wt = som->inputw;
  nima = som->ninput-som->nextrainput;
  llim = -5.0*backnoise;
  hlim = prefs.satur_level-back;
  backnoise *= backnoise;
  ngood = 0;
  for (i=nima; i--;)
    {
    pix = *(yt++);
/*-- look if pixel is in the "reasonable" range */
    if (pix>llim && pix<hlim)
      {
      *(wt++) = 1/((pix>0.0?pix/gain:0.0)+backnoise);
      ngood++;
      }
    else
      *(wt++) = 0.0;
    }

  wstiff = 1/(som->xy_stiff*som->xy_stiff);
  for (i=som->nextrainput; i--;)
    *(wt++) = wstiff;

  return ngood;
  }


/********************************** som_err **********************************/
/*
Return the reduced RMS error at some (non-integer) position in the SOM.
1 degree of freedom is left: the amplitude of the prototype.
*/
float	som_err(somstruct *som, float dist, int flags)
  {
   static float	dx[SOM_MAXDIM];
   double	s,sx,sxx,sy,syy,sxy,b,err,ds;
   float	*psf, *psft, *dxt, *wt,*xt,*yt,xi,yi,wi,wxi,wyi, diff,dd;	
   int		i,j,k,n, nd, *nx, *mult, ix, pos,post, nima;

/* Is the requested position lying on a node? */
  if (flags & SOM_NODE)
/*-- Yes: just use the prototype at that node */
    xt = som->weight+som->ninput*(int)dist;
  else
    {
/*-- ...No: compute offsets and fractional parts for each dimension */
    nd = som->neurdim;
    nx = som->neursize;
    xt = som->vector;
    if (flags & SOM_LINE)
      yt = som->dvector;
    pos = 0;
    dxt = dx;
    mult = som->neurstep;

    for (i=nd; i--; nx++)
      {
      xi = *(xt++);
      if (flags & SOM_LINE)
        xi += dist**(yt++);
      ix = (int)xi;
      if (ix<0)
        ix = 0;
      else if (ix>=*nx-1)
        ix = *nx-2;
      if (ix<0)
        {
        ix = 0;
        *(dxt++) = 0.0;
        }
      else
        *(dxt++) = xi - ix;
      pos += ix**(mult++);
      }

    memset(som->proto, 0, som->ninput*sizeof(float));
    n = 1<<nd;	/* OK until the SOM has less than 32 dimensions... */
    for (j=0; j<n; j++)
      {
      post = pos;
      dd = 1.0;
      dxt = dx;
      nx = som->neursize;
      mult = som->neurstep;
      for (i=0; i<nd; mult++, nx++)
        {
        if (((1<<(i++)) & j) && *nx>1)
          {
          post += *mult;
          dd *= *(dxt++);
          }
        else
          dd *= (1-*(dxt++));
        }
      psft = som->proto;
      wt = som->weight + som->ninput*post;
      for (i=som->ninput; i--;)
        *(psft++) += dd**(wt++);
      }
    xt = som->proto;
    }

  yt = som->input;
  wt = som->inputw;
  nima = som->ninput-som->nextrainput;

/* Test if we need to derive photometry, or just compute the error */
  if (flags & SOM_PHOTOM)
    {
/*-- Yes: photometry */
    s = sx = sy = sxx = syy = sxy = 0.0;
    for (i=nima; i--;)
      {
      s += (wi = *(wt++));
      sx += (wxi = wi*(xi=*(xt++)));
      sxx += wxi*xi;
      sy += (wyi = wi*(yi=*(yt++)));
      syy += wyi*yi;
      sxy += wxi*yi;
      }

/*-- First, the error from the image-fitting */
    som->amp = b = sxy/sxx;
    err = (syy - b*sxy)/(nima-1);
    som->sigamp = sqrt(err*s/(s*sxx-sx*sx));

/*-- Second, the error of non-pixel parameters */
    for (i=som->nextrainput; i--;)
      {
      diff = *(yt++) - *(xt++);
      err += (diff*diff*(double)*(wt++))/(double)som->nextrainput;
      }
    }
  else
    {
/*-- No: just an estimate of error */
    sxx = sxy = 0.0;
    for (i=nima; i--;)
      {
      sxy += (wxi = *(wt++)*(xi=*(xt++)))**(yt++);
      sxx += wxi*xi;
      }

/*-- First, the error from the image-fitting */
    err = -sxy*sxy/sxx/(nima-1);

/*-- Second, the error of non-pixel parameters */
    for (i=som->nextrainput; i--;)
      {
      diff = *(yt++) - *(xt++);
      err += (diff*diff*(double)*(wt++))/(double)som->nextrainput;
      }
    }

/* Compute error gradients if requested */
  if (flags & SOM_GRADIENT)
    {
    for (k=0; k<nd; k++)
      {
      memset(som->dproto, 0, som->ninput*sizeof(float));
      for (j=0; j<n; j++)
        {
        dd = 1.0;
        post = pos;
        dxt = dx;
        mult = som->neurstep;
        nx = som->neursize;
        for (i=0; i<nd; mult++, dxt++, nx++)
          {
          if (((1<<i) & j) && *nx>1)
            {
            post += *mult;
            if ((i++)!=k)
              dd *= *dxt;
            }
          else
            dd *= ((i++)==k ? (*nx>1?-1.0:0.0) : (1-*dxt));
          }
        psft = som->dproto;
        wt = som->weight + som->ninput*post;
        for (i=som->ninput; i--;)
          *(psft++) += dd**(wt++);
        }
      ds = 0.0;
      psft = som->dproto;
      xt = som->proto;
      yt = som->input;
      wt = som->inputw;
      for (i=nima; i--;)
        ds += *(wt++)*(sxy**(xt++)-sxx**(yt++))**(psft++);
      ds *= 2*sxy/(sxx*sxx)/(nima-1);
      for (i=som->nextrainput; i--;)
        ds += 2**(wt++)**(psft++)*(*(xt++)-*(yt++))/(double)som->nextrainput;
      som->dvector[k] = (float)ds;
      }
    }

  return (float)err;
  }


/******************************** som_linmin *********************************/
/*
Perform minimisation through line-search using two routines from Numerical
Recipes in C: mnbrak() and brent() (pp. 297 and 301).
*/

#define SHFT(a,b,c,d)   {(a)=(b);(b)=(c);(c)=(d);}      /* For line-search */
#define SIGN(a,b)       ((b)>0.0? fabs(a) : -fabs(a))   /* For line-search */

#define GOLD		1.6180340	/* Golden section for line-search */
#define CGOLD		0.3819660	/* Complement to the golden section */
#define TINY		1e-20		/* Almost nothing */
#define GLIMIT		100.0		/* Max. magnification in line-search */
#define ITMAX		100		/* Max. nb of iter. in line-search */
#define TOL		1e-1		/* Fract. tolerance in line-search */

float	som_linmin(somstruct *som)
  {
   float	ax,bx,cx, fa,fb,fc, u,r,q,fu,dum,ulim, qmr, a,b,d,e,etemp,
		fv,fw,fx, p, tol1,tol2,v,w,x,xm, *vt,*dvt;
   int		i,iter;

/* Normalize the gradient */
/*
  dvt = som->dvector;
  for (i=som->neurdim; i--; dvt++)
    dum += *dvt**dvt;
  if (dum>0.0)
    {
    dum = sqrt(dum);
    dvt = som->dvector;
    for (i=som->neurdim; i--;)
      *(dvt++) /= dum;
    }
*/
/* Begin by bracketing a minimum of the function */
  ax = 0.0;	/* Initial guesses */
  bx = 1.0;
  if ((fb=som_err(som, bx, SOM_LINE)) > (fa=som_err(som, ax, SOM_LINE)))
    {
    SHFT(dum, ax, bx, dum);
    SHFT(dum, fb, fa, dum);
    }
  fc = som_err(som, cx = bx+GOLD*(bx-ax), SOM_LINE);
  while (fb > fc)
    {
    r = (bx-ax)*(fb-fc);
    q = (bx-cx)*(fb-fa);
    if (fabs(qmr = q-r)<TINY)
      qmr = qmr>0.0?TINY:-TINY;
    u = bx-((bx-cx)*q - (bx-ax)*r) / (2.0*qmr);
    ulim= bx + GLIMIT*(cx-bx);
    if ((bx-u)*(u-cx) > 0.0)
      {
      if ((fu=som_err(som, u, SOM_LINE)) < fc)
        {
        ax = bx;
        bx = u;
        fa = fb;
        fb = fu;
        break;
        }
      else if (fu > fb)
        {
        cx = u;
        fc = fu;
        break;
        }
      fu = som_err(som, u = cx + GOLD*(cx-bx), SOM_LINE);
      }
    else if ((cx-u)*(u-ulim) > 0.0)
      {
      if ((fu=som_err(som, u, SOM_LINE)) < fc)
        {
        SHFT(bx, cx, u, cx+GOLD*(cx-bx));
        SHFT(fb, fc, fu, som_err(som, u, SOM_LINE));
        }
      }
    else if ((u-ulim)*(ulim-cx) >= 0.0)
      fu = som_err(som, u=ulim, SOM_LINE);
    else
      fu = som_err(som, u = cx + GOLD*(cx-bx), SOM_LINE);
    SHFT(ax, bx, cx, u);
    SHFT(fa, fb, fc, fu);
    }

/* Now we step to Brent's algorithm for finding the minimum */
  e = 0.0;
  a = (ax < cx) ? ax : cx;
  b = (ax > cx) ? ax : cx;
  x = w = v = bx;
  fw = fv = fx = som_err(som, x, SOM_LINE);
  for (iter=ITMAX; iter--;)
    {
    xm = 0.5*(a+b);
    tol2 = 2 * (tol1=TOL*fabs(x)+TINY);
    if (fabs(x-xm) <= (tol2-0.5*(b-a)))
      goto linmin_end;
    if (fabs(e) > tol1)
      {
      r = (x-w) * (fx-fv);
      q = (x-v) * (fx-fw);
      p = (x-v)*q - (x-w)*r;
      q = 2*(q-r);
      if (q > 0.0)
        p = -p;
      q = fabs(q);
      etemp = e;
      e = d;
      if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x))
        d = CGOLD*(e=(x >= xm ? a-x : b-x));
      else
        {
        d = p/q;
        u = x+d;
        if (u-a < tol2 || b-u < tol2)
          d = SIGN(tol1,xm-x);
        }
      }
    else
      d = CGOLD*(e=(x >= xm ? a-x : b-x));
    u = (fabs(d) >= tol1 ? x+d : x+SIGN(tol1,d));
    if ((fu=som_err(som, u, SOM_LINE)) <= fx)
      {
      if (u >= x)
        a = x;
      else
        b = x;
      SHFT(v, w, x, u);
      SHFT(fv, fw, fx, fu);
      }
    else
      {
      if (u < x)
        a = u;
      else
        b = u;
    if (fu <= fw || w == x)
      {
      v = w;
      w = u;
      fv = fw;
      fw = fu;
      }
    else if (fu <= fv || v == x || v == w)
      {
      v = u;
      fv = fu;
      }
    }
  }

  warning("Too many iterations in ", "som_linmin()");

/* Finally we set the SOM vector to the new minimum */
linmin_end:
  vt = som->vector;
  dvt = som->dvector;
  for (i=som->neurdim; i--;)
    *(vt++) += x**(dvt++);

  return fx;
  }

#undef	SHFT
#undef	SIGN
#undef	GOLD
#undef	CGOLD
#undef	TINY
#undef	GLIMIT
#undef	ITMAX
#undef	TOL

/******************************** som_conjgrad *******************************/
/*
Perform Polak-Ribiere minimization (adapted from Numerical Recipes in C,p.432).
*/

#define ITMAX 100
#define EPS 1.0e-10

void som_conjgrad(somstruct *som, float ftol)
  {
   static float	g[SOM_MAXDIM], h[SOM_MAXDIM];
   int		j, nd, its;
   float	*xi, *xit,*gt,*ht,tmp,tmp2,
		gg,gam,fp,fret,dgg;

  nd = som->neurdim;
  xi = som->dvector;
  fp = som_err(som, 0.0, SOM_GRADIENT);
  gt = g;
  xit = xi;
  ht = h;
  for (j=nd;j--;)
    tmp = -*xit, *(xit++) = *(ht++)= *(gt++) = tmp;
  for (its=ITMAX;its--;)
    {
    fret = som_linmin(som);
    if (2.0*fabs(fret-fp) <= ftol*(fabs(fret)+fabs(fp)+EPS))
      return;
    fp=som_err(som, 0.0, SOM_GRADIENT);
    dgg=gg=0.0;
    gt = g;
    xit = xi;
    for (j=nd;j--; xit++)
      {
      gg += *gt**gt;
      dgg += (*xit+*(gt++))**xit;
      }
    if (gg == 0.0)
      return;
    gam=dgg/gg;
    gt = g;
    xit = xi;
    ht = h;
    for (j=nd;j--;)
      tmp = -*xit,tmp2 = *ht, *(xit++) = *(ht++) = (*(gt++) = tmp) + gam*tmp2;
    }
  warning("Too many iterations during SOM-Fitting","");
  }

#undef	ITMAX
#undef	EPS

/********************************* som_end ***********************************/
/*
Terminate SOM.
*/
void	som_end(somstruct *som)
  {
/* Free memory*/
  free(som->weight);
  free(som->input);
  free(som->inputw);
  free(som->proto);
  free(som->dproto);
  free(som->vector);
  free(som->dvector);
  free(som->freq);
  free(som->inputsize);
  free(som->neursize);
  free(som->neurstep);
  free(som);

/* locals */

  return;
  }


/********************************* som_load **********************************/
/*
Read the SOM weights in a FITS file.
*/
somstruct	*som_load(char *filename)
  {
   somstruct	*som;
   catstruct	*cat;
   tabstruct	*tab;
   keystruct	*key;
   char		*head, str[80];
   int		i;

/* Open the cat (well it is not a "cat", but simply a FITS file */
  if (!(cat = read_cat(filename)))
    error(EXIT_FAILURE, "*Error*: SOM file not found: ", filename);

  if (!(tab = name_to_tab(cat, "SOM", 0)))
    error(EXIT_FAILURE, "*Error*: SOM table not found in catalog ",
	filename);

/* OK, we now allocate memory for the SOM structure itself */
  QCALLOC(som, somstruct, 1);

/* Load important scalars (which are stored as FITS keywords) */
  head = tab->headbuf;

/* Dimensionality of the input */
  if (fitsread(head, "INPNAXIS", &som->inputdim, H_INT, T_LONG) != RETURN_OK)
    goto headerror;
  if (som->inputdim>INPUT_MAXDIM)
    {
    sprintf(str, "%d", INPUT_MAXDIM);
    error(EXIT_FAILURE, "*Error*: This package is presently limited to inputs"
	"with dimensionality less or equal to ", str);
    }
  QMALLOC(som->inputsize, int, INPUT_MAXDIM);
  for (i=0; i<INPUT_MAXDIM; i++)
    som->inputsize[i] = 1;
  som->ninput = 1;
  for (i=0; i<som->inputdim; i++)
    {
    sprintf(str, "INPAXIS%1d", i+1);
    if (fitsread(head, str, &som->inputsize[i], H_INT,T_LONG) != RETURN_OK)
      goto headerror;
    som->ninput *= som->inputsize[i];
    }

    if (fitsread(head,"INPNEXTR",&som->nextrainput,H_INT,T_LONG) != RETURN_OK)
      som->nextrainput = 0;
    som->ninput += som->nextrainput;

/* Dimensionality of the SOM */
  if (fitsread(head, "SOMNAXIS", &som->neurdim, H_INT, T_LONG) != RETURN_OK)
    goto headerror;
  QMALLOC(som->neursize, int, som->neurdim);
  QMALLOC(som->neurstep, int, som->neurdim);
  QCALLOC(som->vector, float, som->neurdim);
  QCALLOC(som->dvector, float, som->neurdim);
  for (i=0; i<som->neurdim; i++)
    som->neursize[i] = 1;
  som->nneur = 1;
  for (i=0; i<som->neurdim; i++)
    {
    sprintf(str, "SOMAXIS%1d", i+1);
    if (fitsread(head, str, &som->neursize[i], H_INT,T_LONG) != RETURN_OK)
      goto headerror;
    som->neurstep[i] = som->nneur;
    som->nneur *= som->neursize[i];
    }

/* Other scalars */
  if (fitsread(head, "SOMLRATE", &som->learnrate,H_FLOAT,T_FLOAT) != RETURN_OK)
    goto headerror;
  som->clearnrate = som->learnrate;
  if (fitsread(head, "SOMKERNW", &som->kernw, H_FLOAT,T_FLOAT) != RETURN_OK)
    goto headerror;
  som->ckernw = som->kernw;
  if (fitsread(head, "SOMNPASS", &som->ntrain , H_INT, T_LONG) != RETURN_OK)
    goto headerror;
  if (fitsread(head, "SOMNSWEE", &som->nsweep , H_INT, T_LONG) != RETURN_OK)
    goto headerror;

  som->nweight = som->nneur*som->ninput;
  QMALLOC(som->weight, float, som->nneur*som->ninput);
  QMALLOC(som->input, float, som->ninput);
  QMALLOC(som->inputw, float, som->ninput);
  QMALLOC(som->proto, float, som->ninput);
  QMALLOC(som->dproto, float, som->ninput);
  QCALLOC(som->freq, int, som->nneur);
/* Locals */

/* Load the weight vector */
  key = read_key(tab, "WEIGHTS");
  som->weight = key->ptr;

/* But don't touch my arrays!! */
  blank_keys(tab);
  free_cat(cat, 1);

  return som;

headerror:
  error(EXIT_FAILURE, "*Error*: Incorrect or obsolete SOM data in ", filename);
  }


