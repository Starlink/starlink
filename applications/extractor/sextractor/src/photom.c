 /*
 				photom.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Compute magnitudes and other photometrical parameters.
*
*	Last modify:	13/12/2002
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<math.h>
#include	<stdlib.h>

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"
#include	"photom.h"
#include	"plist.h"

/***************************** computeaperflux********************************/
/*
Compute the total flux within a circular aperture.
*/
void  computeaperflux(picstruct *field, picstruct *wfield,
	objstruct *obj, int i)

  {
   float		r2, raper,raper2, rintlim,rintlim2,rextlim2,
			mx,my,dx,dx1,dy,dy2,
			offsetx,offsety,scalex,scaley,scale2, ngamma, locarea;
   double		tv, sigtv, area, pix, var, backnoise2, gain;
   int			x,y, x2,y2, xmin,xmax,ymin,ymax, sx,sy, w,h,
			fymin,fymax, pflag,corrflag, gainflag;
   long			pos;
   PIXTYPE		*strip,*stript, *wstrip,*wstript,
			wthresh;

  if (wfield)
    wthresh = wfield->weight_thresh;
  mx = obj->mx;
  my = obj->my;
  w = field->width;
  h = field->stripheight;
  fymin = field->ymin;
  fymax = field->ymax;
  ngamma = field->ngamma;
  pflag = (prefs.detect_type==PHOTO)? 1:0;
  corrflag = (prefs.mask_type==MASK_CORRECT);
  gainflag = wfield && prefs.weightgain_flag;
  var = backnoise2 = field->backsig*field->backsig;
  gain = prefs.gain;
/* Integration radius */
  raper = prefs.apert[i]/2.0;
  raper2 = raper*raper;
/* Internal radius of the oversampled annulus (<r-sqrt(2)/2) */
  rintlim = raper - 0.75;
  rintlim2 = (rintlim>0.0)? rintlim*rintlim: 0.0;
/* External radius of the oversampled annulus (>r+sqrt(2)/2) */
  rextlim2 = (raper + 0.75)*(raper + 0.75);
  tv = sigtv = area = 0.0;
  scaley = scalex = 1.0/APER_OVERSAMP;
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
      if ((r2=dx*dx+dy*dy) < rextlim2)
        {
        if (r2> rintlim2)
          {
          dx += offsetx;
          dy += offsety;
          locarea = 0.0;
          for (sy=APER_OVERSAMP; sy--; dy+=scaley)
            {
            dx1 = dx;
            dy2 = dy*dy;
            for (sx=APER_OVERSAMP; sx--; dx1+=scalex)
              if (dx1*dx1+dy2<raper2)
                locarea += scale2;
            }
          }
        else
          locarea = 1.0;
        area += locarea;
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
          {
          pix=exp(pix/ngamma);
          sigtv += var*locarea*pix*pix;
          }
        else
          sigtv += var*locarea;
        tv += locarea*pix;
        if (gainflag && pix>0.0 && gain>0.0)
          sigtv += pix/gain*var/backnoise2;
        }
      }
    }

  if (pflag)
    {
    tv = ngamma*(tv-area*exp(obj->dbkg/ngamma));
    sigtv /= ngamma*ngamma;
    }
  else
    {
    tv -= area*obj->dbkg;
    if (!gainflag && gain > 0.0 && tv>0.0)
      sigtv += tv/gain;
    }

  if (i<prefs.flux_apersize)
    obj2->flux_aper[i] = tv;
  if (i<prefs.fluxerr_apersize)
    obj2->fluxerr_aper[i] = sqrt(sigtv);
  if (i<prefs.mag_apersize)
    obj2->mag_aper[i] = tv>0.0? -2.5*log10(tv) + prefs.mag_zeropoint : 99.0;
  if (i<prefs.magerr_apersize)
    obj2->magerr_aper[i] = tv>0.0? 1.086*sqrt(sigtv)/tv:99.0;

  return;
  }


/***************************** computeautoflux********************************/
/*
Compute the total flux within an automatic elliptical aperture.
*/
void  computeautoflux(picstruct *field, picstruct *dfield, picstruct *wfield,
	picstruct *dwfield, objstruct *obj)

  {
   double		sigtv, tv, r1, v1,var,gain,backnoise2;
   float		bkg, ngamma, mx,my, dx,dy, cx2,cy2,cxy, r2,klim2,
			dxlim, dylim;
   int			area,areab, x,y, x2,y2, xmin,xmax,ymin,ymax,
			fymin,fymax, w,h,
			pflag, corrflag, gainflag, pos;
   PIXTYPE		*strip,*stript, *dstrip,*dstript, *wstrip,*wstript,
			*dwstrip,*dwstript,
			pix, wthresh,dwthresh;


/* Let's initialize some variables */
  if (!dfield)
    dfield = field;
  if (dwfield)
    dwthresh = dwfield->weight_thresh;
  if (wfield)
    wthresh = wfield->weight_thresh;
  wstript = dwstript = NULL;
  w = field->width;
  h = field->stripheight;
  fymin = field->ymin;
  fymax = field->ymax;
  ngamma = field->ngamma;
  bkg = (double)obj->dbkg;
  mx = obj->mx;
  my = obj->my;
  var = backnoise2 = field->backsig*field->backsig;
  gain = prefs.gain;
  pflag = (prefs.detect_type==PHOTO)? 1:0;
  corrflag = (prefs.mask_type==MASK_CORRECT);
  gainflag = wfield && prefs.weightgain_flag;

/* First step: find the extent of the ellipse (the kron factor r1) */
/* Clip boundaries in x and y */
/* We first check that the search ellipse is large enough... */
  if (KRON_NSIG*sqrt(obj->a*obj->b)>prefs.autoaper[0]/2.0)
    {
    cx2 = obj->cxx;
    cy2 = obj->cyy;
    cxy = obj->cxy;
    dxlim = cx2 - cxy*cxy/(4.0*cy2);
    dxlim = dxlim>0.0 ? KRON_NSIG/sqrt(dxlim) : 0.0;
    dylim = cy2 - cxy*cxy/(4.0*cx2);
    dylim = dylim > 0.0 ? KRON_NSIG/sqrt(dylim) : 0.0;
    klim2 = KRON_NSIG*KRON_NSIG;
    }
  else
/*-- ...if not, use the circular aperture provided by the user */
    {
    cx2 = cy2 = 1.0;
    cxy = 0.0;
    dxlim = dylim = prefs.autoaper[0]/2.0;
    klim2 =  dxlim*dxlim;
    }

  if ((xmin = RINT(mx-dxlim)) < 0)
    {
    xmin = 0;
    obj->flag |= OBJ_APERT_PB;
    }
  if ((xmax = RINT(mx+dxlim)+1) > w)
    {
    xmax = w;
    obj->flag |= OBJ_APERT_PB;
    }
  if ((ymin = RINT(my-dylim)) < field->ymin)
    {
    ymin = field->ymin;
    obj->flag |= OBJ_APERT_PB;
    }
  if ((ymax = RINT(my+dylim)+1) > field->ymax)
    {
    ymax = field->ymax;
    obj->flag |= OBJ_APERT_PB;
    }

  v1 = r1 = 0.0;
  area = areab = 0;
  dstrip = dfield->strip;
  if (dwfield)
    dwstrip = dwfield->strip;
  for (y=ymin; y<ymax; y++)
    {
    dstript = dstrip + (pos = xmin + (y%h)*w);
    if (dwfield)
      dwstript = dwstrip + pos;
    for (x=xmin; x<xmax; x++, dstript++, dwstript++)
      {
      dx = x - mx;
      dy = y - my;
      if ((r2=cx2*dx*dx + cy2*dy*dy + cxy*dx*dy) <= klim2)
        {
        if ((pix=*dstript)>-BIG && (!dwfield || (dwfield&&*dwstript<dwthresh)))
          {
          area++;
          r1 += sqrt(r2)*pix;
          v1 += pix;
          }
        else
          areab++;
        }
      }
    }

  area += areab;
  if (area)
    {
/*-- Go further only if some pixels are available !! */
    if (r1>0.0 && v1>0.0)
      {
      obj2->kronfactor = prefs.autoparam[0]*r1/v1;
      if (obj2->kronfactor < prefs.autoparam[1])
        obj2->kronfactor = prefs.autoparam[1];
      }
    else
      obj2->kronfactor = prefs.autoparam[1];

/*-- Flag if the Kron photometry can be strongly affected by neighhours */
    if ((float)areab/area > CROWD_THRESHOLD)
      obj->flag |= OBJ_CROWDED;

/*-- Second step: integrate within the ellipse */
/*-- Clip boundaries in x and y (bis) */
/*-- We first check that the derived ellipse is large enough... */
    if (obj2->kronfactor*sqrt(obj->a*obj->b)>prefs.autoaper[1]/2.0)
      {
      cx2 = obj->cxx;
      cy2 = obj->cyy;
      cxy = obj->cxy;
      dxlim = cx2 - cxy*cxy/(4.0*cy2);
      dxlim = dxlim>0.0 ? obj2->kronfactor/sqrt(dxlim) : 0.0;
      dylim = cy2 - cxy*cxy/(4.0*cx2);
      dylim = dylim > 0.0 ? obj2->kronfactor/sqrt(dylim) : 0.0;
      klim2 = obj2->kronfactor*obj2->kronfactor;
      }
    else
/*---- ...if not, use the circular aperture provided by the user */
      {
      cx2 = cy2 = 1.0;
      cxy = 0.0;
      dxlim = dylim = prefs.autoaper[1]/2.0;
      klim2 =  dxlim*dxlim;
      obj2->kronfactor = 0.0;
      }

    if ((xmin = RINT(mx-dxlim)) < 0)
      {
      xmin = 0;
      obj->flag |= OBJ_APERT_PB;
      }
    if ((xmax = RINT(mx+dxlim)+1) > w)
      {
      xmax = w;
      obj->flag |= OBJ_APERT_PB;
      }
    if ((ymin = RINT(my-dylim)) < field->ymin)
      {
      ymin = field->ymin;
      obj->flag |= OBJ_APERT_PB;
      }
    if ((ymax = RINT(my+dylim)+1) > field->ymax)
      {
      ymax = field->ymax;
      obj->flag |= OBJ_APERT_PB;
      }

    area = areab = 0;
    tv = sigtv = 0.0;
    strip = field->strip;
    if (wfield)
      wstrip = wfield->strip;
    for (y=ymin; y<ymax; y++)
      {
      stript = strip + (pos = xmin + (y%h)*w);
      if (wfield)
        wstript = wstrip + pos;
      for (x=xmin; x<xmax; x++, stript++, wstript++)
        {
        dx = x - mx;
        dy = y - my;
        if ((cx2*dx*dx + cy2*dy*dy + cxy*dx*dy) <= klim2)
          {
          area++;
/*-------- Here begin tests for pixel and/or weight overflows. Things are a */
/*-------- bit intricated to have it running as fast as possible in the most */
/*-------- common cases */
          if ((pix=*stript)<=-BIG || (wfield && (var=*wstript)>=wthresh))
            {
            areab++;
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
            {
            pix = exp(pix/ngamma);
            sigtv += var*pix*pix;
            }
          else
            sigtv += var;
          tv += pix;
          if (gainflag && pix>0.0 && gain>0.0)
            sigtv += pix/gain*var/backnoise2;
          }
        }
      }

/*-- Flag if the Kron photometry can be strongly affected by neighhours */
    if ((float)areab > CROWD_THRESHOLD*area)
      obj->flag |= OBJ_CROWDED;

    if (pflag)
      {
      tv = ngamma*(tv-area*exp(bkg/ngamma));
      sigtv /= ngamma*ngamma;
      }
    else
      {
      tv -= area*bkg;
      if (!gainflag && gain > 0.0 && tv>0.0)
        sigtv += tv/gain;
      }
    }
  else
/*-- No available pixels: set the flux to zero */
    tv = sigtv = 0.0;


  obj2->flux_auto = tv;
  obj2->fluxerr_auto = sqrt(sigtv);

  if (FLAG(obj2.mag_auto))
    obj2->mag_auto = obj2->flux_auto>0.0?
			 -2.5*log10(obj2->flux_auto) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_auto))
    obj2->magerr_auto = obj2->flux_auto>0.0?
			 1.086*obj2->fluxerr_auto/obj2->flux_auto
			:99.0;
  if (tv<=0.0)
    obj2->kronfactor = 0.0;

  return;
  }


/****************************** computeisocorflux ****************************/
/*
Compute the (corrected) isophotal flux.
*/
void  computeisocorflux(picstruct *field, objstruct *obj)

  {
   double	ati;

  ati = (obj->flux>0.0)? (obj->fdnpix*obj->dthresh/obj->flux) : 0.0;
  if (ati>1.0)
    ati = 1.0;
  else if (ati<0.0)
    ati = 0.0;
  obj2->flux_isocor = obj->flux/(1.0-0.196099*ati-0.751208*ati*ati);
  if (FLAG(obj2.fluxerr_isocor))
    {
    if (obj->flux>0.0)
      {
       double	dati, sigtv;

      sigtv = obj->fluxerr/(obj->flux*obj->flux);
      dati = obj->fdnpix?ati*sqrt(sigtv+1.0/obj->fdnpix): 0.0;
      dati = 0.196099*dati + 0.751208*2*ati*dati;
      obj2->fluxerr_isocor = sqrt(sigtv+dati*dati)*obj->flux;
      }
    else
      obj2->fluxerr_isocor = sqrt(obj->fluxerr);
    }

  return;
  }


/******************************* computemags *********************************/
/*
Compute magnitude parameters.
*/
void  computemags(picstruct *field, objstruct *obj)

  {
/* Mag. isophotal */
  if (FLAG(obj2.mag_iso))
    obj2->mag_iso = obj2->flux_iso>0.0?
			 -2.5*log10(obj2->flux_iso) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_iso))
    obj2->magerr_iso = obj2->flux_iso>0.0?
			 1.086*obj2->fluxerr_iso/obj2->flux_iso
			:99.0;

/* Mag. isophotal corrected */
  if (FLAG(obj2.mag_isocor))
    obj2->mag_isocor = obj2->flux_isocor>0.0?
			 -2.5*log10(obj2->flux_isocor) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_isocor))
    obj2->magerr_isocor = obj2->flux_isocor>0.0?
			 1.086*obj2->fluxerr_isocor/obj2->flux_isocor
			:99.0;

/* Choose the ``best'' flux according to the local crowding */

  if (FLAG(obj2.flux_best))
    {
    if (obj->flag&OBJ_CROWDED)
      {
      obj2->flux_best = obj2->flux_isocor;
      obj2->fluxerr_best = obj2->fluxerr_isocor;
      }
    else
      {
      obj2->flux_best = obj2->flux_auto;
      obj2->fluxerr_best = obj2->fluxerr_auto;
      }
    }

/* Mag. Best */
  if (FLAG(obj2.mag_best))
    obj2->mag_best = obj2->flux_best>0.0?
			 -2.5*log10(obj2->flux_best) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_best))
    obj2->magerr_best = obj2->flux_best>0.0?
			 1.086*obj2->fluxerr_best/obj2->flux_best
			:99.0;

/* Mag. SOM-fit */
  if (FLAG(obj2.mag_somfit))
    obj2->mag_somfit = obj2->flux_somfit>0.0?
			 -2.5*log10(obj2->flux_somfit) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_somfit))
    obj2->magerr_somfit = obj2->flux_somfit>0.0?
			 1.086*obj2->fluxerr_somfit/obj2->flux_somfit
			:99.0;

/* Mag. PROFILE */
  if (FLAG(obj2.mag_prof))
    obj2->mag_prof = obj2->flux_prof>0.0?
			 -2.5*log10(obj2->flux_prof) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_prof))
    obj2->magerr_prof = obj2->flux_prof>0.0?
			 1.086*obj2->fluxerr_prof/obj2->flux_prof
			:99.0;

/* Mag. GALFIT */
  if (FLAG(obj2.mag_galfit))
    obj2->mag_galfit = obj2->flux_galfit>0.0?
			 -2.5*log10(obj2->flux_galfit) + prefs.mag_zeropoint
			:99.0;
  if (FLAG(obj2.magerr_galfit))
    obj2->magerr_galfit = obj2->flux_galfit>0.0?
			 1.086*obj2->fluxerr_galfit/obj2->flux_galfit
			:99.0;

/* SB units */
  if (FLAG(obj2.maxmu))
    outobj2.maxmu = obj->peak > 0.0 ?
		-2.5*log10((obj->peak)
		 / (field->pixscale * field->pixscale)) + prefs.mag_zeropoint
		: 99.0;

  if (FLAG(obj2.threshmu))
    obj2->threshmu = obj->thresh > 0.0 ?
		-2.5*log10((obj->thresh)
		 / (field->pixscale * field->pixscale)) + prefs.mag_zeropoint
		: 99.0;

  return;
  }


