 /*
 				growth.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Make growth curves.
*
*	Last modify:	28/11/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"
#include	"growth.h"

/*------------------------------- variables ---------------------------------*/

static double	*growth;
static int	ngrowth;
static obj2struct	*obj2 = &outobj2;

/******************************** initgrowth *********************************/
/*
Allocate memory for growth curve stuff.
*/
void	initgrowth()
  {

  QMALLOC(growth, double, GROWTH_NSTEP);

  return;
  }  


/******************************** endgrowth **********************************/
/*
Free memory occupied by growth curve stuff.
*/
void	endgrowth()
  {
  free(growth);	

  return;
  }


/****************************** makeavergrowth *******************************/
/*
Build growth curve based on averages.
*/
void	makeavergrowth(picstruct *field, picstruct *wfield, objstruct *obj)

  {
   float		*fgrowth;
   double		*growtht,
			dx,dx1,dy,dy2,mx,my, r2,r,rlim, d, rextlim2, raper,
			offsetx,offsety,scalex,scaley,scale2, ngamma, locarea,
			tv, sigtv, area, pix, var, gain, dpos,step,step2, dg,
			stepdens, backnoise2, prevbinmargin, nextbinmargin;
   int			i,j,n, x,y, x2,y2, xmin,xmax,ymin,ymax, sx,sy, w,h,
			fymin,fymax, pflag,corrflag, ipos;
   LONG			pos;
   PIXTYPE		*strip,*stript, *wstrip,*wstript,
			pdbkg, wthresh;
  

  if (wfield)
    wthresh = wfield->weight_thresh;
  else
    wthresh = 0.0;		/* To avoid gcc -Wall warnings */

/* Clear the growth-curve buffer */
  memset(growth, 0, (size_t)(GROWTH_NSTEP*sizeof(double)));

  mx = obj->mx;
  my = obj->my;
  w = field->width;
  h = field->stripheight;
  fymin = field->ymin;
  fymax = field->ymax;
  pflag = (prefs.detect_type==PHOTO)? 1:0;
  corrflag = (prefs.mask_type==MASK_CORRECT);
  var = backnoise2 = field->backsig*field->backsig;
  gain = prefs.gain;

/* Integration radius */
  rlim = GROWTH_NSIG*obj->a;
  if (rlim<prefs.autoaper[0])
    rlim = prefs.autoaper[0];
  raper = rlim+1.5;		/* margin for interpolation */
/* External radius */
  rextlim2 = raper*raper;

  stepdens = GROWTH_NSTEP/rlim;
/* It is useless to oversample too much! */
  if (0/*stepdens>GROWTH_OVERSAMP*/)
    {
    ngrowth = (int)(rlim*GROWTH_OVERSAMP);
    stepdens = ngrowth/rlim;
    }
  else
    ngrowth = GROWTH_NSTEP;
  step = 1/stepdens;

/* Critical distances (in pixels) from bin boundaries */
  prevbinmargin = 0.75;		/* > 1/sqrt(2) */
  nextbinmargin = step - 0.75;	/* <step - sqrt(2) */
/* For photographic data */
  if (pflag)
    {
    ngamma = field->ngamma;
    pdbkg = exp(obj->dbkg/ngamma);
    }
  else
    {
    ngamma = 0.0;
    pdbkg = 0.0;
    }
  tv = sigtv = area = 0.0;
  scaley = scalex = 1.0/GROWTH_OVERSAMP;
  scale2 = scalex*scaley;
  offsetx = 0.5*(scalex-1.0);
  offsety = 0.5*(scaley-1.0);
  xmin = (int)(mx-raper+0.499999);
  xmax = (int)(mx+raper+1.499999);
  ymin = (int)(my-raper+0.499999);
  ymax = (int)(my+raper+1.499999);

  if (xmin < 0)
    {
    xmin = 0;
    obj->flag |= OBJ_APERT_PB;
    }
  if (xmax > w)
    {
    xmax = w;
    obj->flag |= OBJ_APERT_PB;
    }
  if (ymin < fymin)
    {
    ymin = fymin;
    obj->flag |= OBJ_APERT_PB;
    }
  if (ymax > fymax)
    {
    ymax = fymax;
    obj->flag |= OBJ_APERT_PB;
    }

  strip = field->strip;
  wstrip = wstript = NULL;		/* To avoid gcc -Wall warnings */
  if (wfield)
    wstrip = wfield->strip;
  for (y=ymin; y<ymax; y++)
    {
    stript = strip + (pos = (y%h)*w + xmin);
    if (wfield)
      wstript = wstrip + pos;
    for (x=xmin; x<xmax; x++, stript++, wstript++)
      {
      dx = x - mx;
      dy = y - my;
      if ((r2=dx*dx+dy*dy)<rextlim2)
        {
/*------ Here begin tests for pixel and/or weight overflows. Things are a */
/*------ bit intricated to have it running as fast as possible in the most */
/*------ common cases */
        if ((pix=*stript)<=-BIG || (wfield && (var=*wstript)>=wthresh))
          {
          if (corrflag
		&& (x2=(int)(2*mx+0.49999-x))>=0 && x2<w
		&& (y2=(int)(2*my+0.49999-y))>=fymin && y2<fymax
		&& (pix=*(strip + (pos = (y2%h)*w + x2)))>-BIG)
            {
            if (wfield)
              {
              var = *(wstrip + pos);
              if (var>=wthresh)
                pix = var = 0.0;
              }
            }
          else
            {
            pix = 0.0;
            if (wfield)
              var = 0.0;
            }
          }
        if (pflag)
          pix = exp(pix/ngamma) - pdbkg;

/*------ Check if oversampling is needed (close enough to a bin boundary) */
        d = fmod(r=sqrt(r2),step);
        if (d<prevbinmargin || d>nextbinmargin)
          {
          dx += offsetx;
          dy += offsety;
          locarea = 0.0;
          for (sy=GROWTH_OVERSAMP; sy--; dy+=scaley)
            {
            dx1 = dx;
            dy2 = dy*dy;
            for (sx=GROWTH_OVERSAMP; sx--; dx1+=scalex)
              {
              j = (int)(sqrt(dx1*dx1+dy2)*stepdens);
              if (j<ngrowth)
                {
                growth[j] += scale2*pix;
                locarea += scale2;
                }
              }
            }
          }
        else
          {
          j = (int)(r*stepdens);
         if (j<ngrowth)
            {
            growth[j] += pix;
            locarea = 1.0;
            }
          }
        area += locarea;
/*
        if (pflag)
          sigtv += var*locarea*pix*pix;
        else
          sigtv += var*locarea;
        tv += locarea*pix;
        if (wfield && pix>0.0 && gain>0.0)
          sigtv += pix/gain*var/backnoise2;
*/
        }
      }
    }

/*
  if (pflag)
    {
    tv = ngamma*(tv-area*exp(obj->dbkg/ngamma));
    sigtv /= ngamma*ngamma;
    }
  else
    {
    tv -= area*obj->dbkg;
    if (!wfield && gain > 0.0 && tv>0.0)
      sigtv += tv/gain;
    }
*/

/* Integrate the growth curve */
  pix = 0.0;
  growtht = growth;
  for (i=ngrowth; i--;)
    {
    *growtht += pix;
    pix = *(growtht++);
    }

/* Now let's remap the growth-curve to match user's choice */
  if (FLAG(obj2.flux_growth))
    {
    n = prefs.flux_growthsize;
    if (FLAG(obj2.flux_growthstep))
      obj2->flux_growthstep = rlim/n;
    fgrowth = obj2->flux_growth;
    step2 = (double)GROWTH_NSTEP/n;
    j = 1;
    for (i=n; i--; j++)
      {
      ipos = (int)(dpos=step2*j-0.99999);
      if (dpos<0.0)
        *(fgrowth++) = (float)(*growth*(0.99999+dpos));
      else
        {
        growtht = growth + ipos;
        *(fgrowth++) = (float)(*growtht+(*(growtht+1)-*growtht)*(dpos-ipos));
        }
      }
    }

  if (FLAG(obj2.mag_growth))
    {
    n = prefs.mag_growthsize;
    if (FLAG(obj2.mag_growthstep))
      obj2->mag_growthstep = rlim/n;
    fgrowth = obj2->mag_growth;
    step2 = (double)GROWTH_NSTEP/n;
    j = 1;
    for (i=n; i--; j++)
      {
      ipos = (int)(dpos=step2*j-0.99999);
      if (dpos<0.0)
        pix = *growth*(0.99999+dpos);
      else
        {
        growtht = growth + ipos;
        pix = *growtht+(*(growtht+1)-*growtht)*(dpos-ipos);
        }
      *(fgrowth++) = pix>0.0?(prefs.mag_zeropoint-2.5*log10(pix)):99.0;
      }
    }

  if (FLAG(obj2.flux_radius))
    {
    n = ngrowth-1;
    for (j=0; j<prefs.nflux_frac; j++)
      {
      tv = prefs.flux_frac[j]*obj2->flux_auto;
      growtht = growth-1;
      for (i=0; i<n && *(++growtht)<tv; i++);
      obj2->flux_radius[j] = step
		*(i? ((dg=*growtht - *(growtht-1)) != 0.0 ?
		  	i + (tv - *(growtht-1))/dg
			: i)
		: (*growth !=0.0 ?tv/(*growth) : 0.0));
      }
    }

  return;
  }



