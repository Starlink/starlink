 /*
 				analyse.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*                       P.W.DRAPER (STARLINK, Durham University)
*
*	Contents:	analyse(), endobject()...: measurements on detections.
*
*	Last modify:	12/11/99
*                       20/03/00 (PWD): Added userradii function.
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
#include	"back.h"
#include	"check.h"
#include	"assoc.h"
#include	"astrom.h"
#include	"plist.h"
#include	"flag.h"
#include	"growth.h"
#include	"image.h"
#include	"photom.h"
#include	"psf.h"
#include	"retina.h"

/********************************* analyse ***********************************/
void  analyse(picstruct *field, picstruct *dfield, int objnb,
		objliststruct *objlist)

  {
   objstruct	*obj = objlist->obj+objnb;

/* Do photometry on the detection image if no other image available */
  obj->number = ++cat.ndetect;
  obj->bkg = (float)back(field, (int)(obj->mx+0.5), (int)(obj->my+0.5));
  obj->dbkg = 0.0;
  if (prefs.pback_type == LOCAL)
    localback(field, obj);
  else
    obj->sigbkg = field->backsig;

  examineiso(field, dfield, obj, objlist->plist);


/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/
/* Put here your calls to custom functions related to isophotal measurements.
Ex:

compute_myparams(obj); 

*/
  /*  PWD: add userradii function to calculate radii at various
      intensity levels */
  userradii(field, dfield, obj, objlist->plist);

/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/


  return;
  }


/***************************** examineiso ********************************/
/*
compute some isophotal parameters IN THE MEASUREMENT image.
*/
void  examineiso(picstruct *field, picstruct *dfield, objstruct *obj,
		pliststruct *pixel)

  {
   checkstruct		*check;
   pliststruct		*pixt;
   int			i,j,k,h, photoflag,area,errflag, cleanflag,
			pospeakflag, profflag, minarea, gainflag;
   double		tv,sigtv, ngamma,
			esum, emx2,emy2,emxy, err,gain,backnoise2,dbacknoise2,
			xm,ym, x,y,var,var2, profflux,proffluxvar;
   float		*heap,*heapt,*heapj,*heapk, swap;
   PIXTYPE		pix, cdpix, tpix, peak,cdpeak, thresh,dthresh;
   static PIXTYPE	threshs[NISO];


  if (!dfield)
    dfield = field;

/* Prepare computation of positional error */
  if (errflag=FLAG(obj.poserr_mx2))
    {
    dbacknoise2 = dfield->backsig*dfield->backsig;
    esum = emx2 = emy2 = emxy = 0.0;
    xm = obj->mx;
    ym = obj->my;
    }

  pospeakflag = FLAG(obj.peakx);
  profflag = FLAG(obj.flux_prof);
  gain = prefs.gain;
  ngamma = field->ngamma;
  photoflag = (prefs.detect_type==PHOTO);
  gainflag = PLISTEXIST(var) && prefs.weightgain_flag;

/* Prepare selection of the heap selection for CLEANing */
  if (cleanflag = prefs.clean_flag)
    {
    if (obj->fdnpix < (minarea = prefs.ext_minarea))
      {
      obj->mthresh = 0.0;
      cleanflag = 0;
      }
    else
      {
      h = minarea = prefs.ext_minarea;
      QMALLOC(heap, float, minarea);
      heapt = heap;
      }
    }
  else
    obj->mthresh = 0.0;

/* Measure essential isophotal parameters in the measurement image... */
  tv = sigtv = profflux = proffluxvar = 0.0;
  var = backnoise2 = field->backsig*field->backsig;
  peak = -BIG;
  cdpeak = -BIG;
  thresh = field->thresh;
  dthresh = dfield->dthresh;
  area = 0;
  for (pixt=pixel+obj->firstpix; pixt>=pixel; pixt=pixel+PLIST(pixt,nextpix))
    {
    pix = PLIST(pixt,value);
    if (pix>peak)
      peak = pix;

    cdpix=PLISTPIX(pixt,cdvalue);
    if (pospeakflag && cdpix>cdpeak)
      {
      cdpeak=cdpix;
      obj->peakx =  PLIST(pixt,x) + 1;
      obj->peaky =  PLIST(pixt,y) + 1;
      }
    if (PLISTEXIST(var))
      var = PLISTPIX(pixt, var);
    if (photoflag)
      {
      pix = exp(pix/ngamma);
      var2 = pix*pix*var;
      }
    else
      var2 = var;

    if (gainflag && pix>0.0 && gain>0.0)
      var2 += pix/gain*var/backnoise2;

    sigtv += var2;

    if (profflag && cdpix>0.0)
      {
      profflux += cdpix*pix;
      proffluxvar += cdpix*var2;
      }

    if (pix>thresh)
      area++;
    tv += pix;
    if (errflag)
      {
      err = dbacknoise2;
      if (gain>0.0 && cdpix>0.0)
        err += cdpix/gain;
      x = PLIST(pixt,x) - xm;
      y = PLIST(pixt,y) - ym;
      esum += err;
      emx2 += err*x*x;
      emy2 += err*y*y;
      emxy += err*x*y;
      }

/*-- Find the minareath pixel in decreasing intensity for CLEANing */
    if (cleanflag)
      {
      tpix = PLISTPIX(pixt, cdvalue) - (PLISTEXIST(dthresh)?
		PLISTPIX(pixt, dthresh):dthresh);
      if (h>0)
        *(heapt++) = (float)tpix;
      else if (h)
        {
        if ((float)tpix>*heap)
          {
          *heap = (float)tpix;
          for (j=0; (k=(j+1)<<1)<=minarea; j=k)
            {
            heapk = heap+k;
            heapj = heap+j;
            if (k != minarea && *(heapk-1) > *heapk)
              {
              heapk++;
              k++;
              }
            if (*heapj <= *(--heapk))
              break;
            swap = *heapk;
            *heapk = *heapj;
            *heapj = swap;
            }
          }
        }
      else
        hmedian(heap, minarea);
      h--;
      }
    }

/* Flagging from the flag-map */
  if (PLISTEXIST(flag))
    getflags(obj, pixel);

  if (cleanflag)
    {
    obj->mthresh = *heap;
    free(heap);
    }

  if (profflag)
    {
    obj->flux_prof = obj->fdflux>0.0? (float)(profflux/obj->fdflux) : 0.0;
    obj->fluxerr_prof = obj->fdflux>0.0? (float)(proffluxvar/obj->fdflux):0.0;
    }

  if (errflag)
    {
     double	flux2;

    flux2 = obj->fdflux*obj->fdflux;
/*-- Estimation of the error ellipse moments: we re-use previous variables */
    emx2 /= flux2;	/* variance of xm */
    emy2 /= flux2;	/* variance of ym */
    emxy /= flux2;	/* covariance */

/*-- Handle fully correlated profiles (which cause a singularity...) */
    esum *= 0.08333/flux2;
    if (obj->singuflag && (emx2*emy2-emxy*emxy) < esum*esum)
      {
      emx2 += esum;
      emy2 += esum;
      }

    obj->poserr_mx2 = emx2;
    obj->poserr_my2 = emy2;
    obj->poserr_mxy = emxy;
    }
 
  if (photoflag)
    {
    tv = ngamma*(tv-obj->fdnpix*exp(obj->dbkg/ngamma));
    sigtv /= ngamma*ngamma;
    }
  else
    {
    tv -= obj->fdnpix*obj->dbkg;
    if (!gainflag && gain > 0.0 && tv>0.0)
      sigtv += tv/gain;
    }

  obj->npix = area;
  obj->flux = tv;
  obj->fluxerr = sigtv;
  obj->peak = peak;

  obj->thresh -= obj->dbkg;
  obj->peak -= obj->dbkg;

/* Initialize isophotal thresholds so as to sample optimally the full profile*/

  if (FLAG(obj.iso[0]))
    {
     int	*iso;
     PIXTYPE	*thresht;

    memset(obj->iso, 0, NISO*sizeof(int));
    if (prefs.detect_type == PHOTO)
      for (i=0; i<NISO; i++)
        threshs[i] = obj->thresh + (obj->peak-obj->thresh)*i/NISO;
    else
      {
      if (obj->peak>0.0)
        for (i=0; i<NISO; i++)
          threshs[i] = obj->thresh*pow(obj->peak/obj->thresh,
		(double)i/NISO);
      else
        for (i=0; i<NISO; i++)
          threshs[i] = 0.0;
      }
    for (pixt=pixel+obj->firstpix; pixt>=pixel; pixt=pixel+PLIST(pixt,nextpix))
       for (i=NISO,iso=obj->iso,thresht=threshs;
            i-- && PLIST(pixt,value)>*(thresht++);)
          (*(iso++))++;
    }

/* Put objects in "segmentation check-image" */

  if (check = prefs.check[CHECK_SEGMENTATION])
    for (pixt=pixel+obj->firstpix; pixt>=pixel; pixt=pixel+PLIST(pixt,nextpix))
      ((USHORT *)check->pix)[check->width*PLIST(pixt,y)+PLIST(pixt,x)]
		= (USHORT)obj->number;

  if (check = prefs.check[CHECK_OBJECTS])
    for (pixt=pixel+obj->firstpix; pixt>=pixel; pixt=pixel+PLIST(pixt,nextpix))
      ((PIXTYPE *)check->pix)[check->width*PLIST(pixt,y)+PLIST(pixt,x)]
		= PLIST(pixt,value);

/* Compute the FWHM of the object */
  if (FLAG(obj.fwhm))
    {
     PIXTYPE	thresh0;

    thresh0 = obj->peak/5.0;
    if (thresh0<obj->thresh)
      thresh0 = obj->thresh;
    if (thresh0>0.0)
      {
       double	mx,my, s,sx,sy,sxx,sxy, dx,dy,d2, lpix,pix, b, inverr2, sat,
		dbkg, d, bmax;

      mx = obj->mx;
      my = obj->my;
      dbkg = obj->dbkg;
      sat = (double)(prefs.satur_level - obj->bkg);
      s = sx = sy = sxx = sxy = 0.0;
      for (pixt=pixel+obj->firstpix;pixt>=pixel;pixt=pixel+PLIST(pixt,nextpix))
        {
        pix = PLIST(pixt,value)-dbkg;
        if (pix>thresh0 && pix<sat)
          {
          dx = PLIST(pixt,x) - mx;
          dy = PLIST(pixt,y) - my;
          lpix = log(pix);
          inverr2 = pix*pix;
          s += inverr2;
          d2 = dx*dx+dy*dy;
          sx += d2*inverr2;
          sxx += d2*d2*inverr2;
          sy += lpix*inverr2;
          sxy += lpix*d2*inverr2;
          }        
        }
      d = s*sxx-sx*sx;
      if (fabs(d)>0.0)
        {
        b = -(s*sxy-sx*sy)/d;
        if (b<(bmax = 1/(13*obj->a*obj->b)))	/* to have FWHM # 6 sigma */
          b = bmax;
        obj->fwhm = (float)(1.6651/sqrt(b));
/*----- correction for undersampling effects (established from simulations) */
        if (obj->fwhm>0.0)
          obj->fwhm -= 1/(4*obj->fwhm);
        }
      else
        obj->fwhm = 0.0;
      }
    else
      obj->fwhm = 0.0;
    }

  return;
  }


/******************************* endobject **********************************/
/*
Final processing of object data, just before saving it to the catalog.
*/
void	endobject(picstruct *field, picstruct *dfield, picstruct *wfield,
		picstruct *dwfield, int n, objliststruct *objlist)
  {
   checkstruct	*check;
   int		i,j, ix,iy,selecflag, newnumber,nsub;
   objstruct	*obj;

  obj = &objlist->obj[n];

/* Source position */
  obj2->sposx = (float)(obj2->posx = obj->mx+1.0); /* That's standard FITS */
  obj2->sposy = (float)(obj2->posy = obj->my+1.0);

/* Integer coordinates */
  ix=(int)(obj->mx+0.49999);
  iy=(int)(obj->my+0.49999);

/* Association */
  if (prefs.assoc_flag)
    obj2->assoc_number = do_assoc(field, obj2->sposx, obj2->sposy);

  if (prefs.assoc_flag && prefs.assocselec_type!=ASSOCSELEC_ALL)
    selecflag = (prefs.assocselec_type==ASSOCSELEC_MATCHED)?
		obj2->assoc_number:!obj2->assoc_number;
  else
    selecflag = 1;

  if (selecflag)
    {
/*-- Paste back to the image the object's pixels if BLANKing is on */
    if (prefs.blank_flag)
      {
      pasteimage(field, obj->blank, obj->subw, obj->subh,
		obj->subx, obj->suby);
      if (obj->dblank)
        pasteimage(dfield, obj->dblank, obj->subw, obj->subh,
		obj->subx, obj->suby);
      }


/*------------------------------- Photometry -------------------------------*/

/*-- Convert the father of photom. error estimates from variance to RMS */
    obj2->flux_iso = obj->flux;
    obj2->fluxerr_iso = sqrt(obj->fluxerr);
    if (FLAG(obj.flux_prof))
      {
      obj2->flux_prof = obj->flux_prof;
      obj2->fluxerr_prof = sqrt(obj->fluxerr_prof);
      }

    if (FLAG(obj2.flux_isocor))
      computeisocorflux(field, obj);

    if (FLAG(obj2.flux_aper))
      for (i=0; i<prefs.naper; i++)
        computeaperflux(field, wfield, obj, i);

    if (FLAG(obj2.flux_auto))
      computeautoflux(field, dfield, wfield, dwfield, obj);

/*-- Growth curve */
    if (prefs.growth_flag)
      makeavergrowth(field, wfield, obj);

/*-- What about the peak of the profile? */
    if (obj->peak+obj->bkg >= prefs.satur_level)
      obj->flag |= OBJ_SATUR;

/*-- Check-image CHECK_APERTURES option */

    if (check = prefs.check[CHECK_APERTURES])
      {
      if (FLAG(obj2.flux_aper))
        for (i=0; i<prefs.naper; i++)
          sexcircle(check->pix, check->width, check->height,
		obj->mx, obj->my, prefs.apert[i]/2.0, check->overlay);

      if (FLAG(obj2.flux_auto))
        sexellips(check->pix, check->width, check->height,
	obj->mx, obj->my, obj->a*obj2->kronfactor,
	obj->b*obj2->kronfactor, obj->theta,
	check->overlay, obj->flag&OBJ_CROWDED);
      }

/*-- Error ellipse parameters */
    if (FLAG(obj2.poserr_a))
      {
       double	pmx2,pmy2,temp,theta;

      if (fabs(temp=obj->poserr_mx2-obj->poserr_my2) > 0.0)
        theta = atan2(2.0 * obj->poserr_mxy,temp) / 2.0;
      else
        theta = PI/4.0;

      temp = sqrt(0.25*temp*temp+obj->poserr_mxy*obj->poserr_mxy);
      pmy2 = pmx2 = 0.5*(obj->poserr_mx2+obj->poserr_my2);
      pmx2+=temp;
      pmy2-=temp;

      obj2->poserr_a = (float)sqrt(pmx2);
      obj2->poserr_b = (float)sqrt(pmy2);
      obj2->poserr_theta = theta*180.0/PI;
      }

    if (FLAG(obj2.poserr_cxx))
      {
       double	xm2,ym2, xym, temp;

      xm2 = obj->poserr_mx2;
      ym2 = obj->poserr_my2;
      xym = obj->poserr_mxy;
      obj2->poserr_cxx = (float)(ym2/(temp=xm2*ym2-xym*xym));
      obj2->poserr_cyy = (float)(xm2/temp);
      obj2->poserr_cxy = (float)(-2*xym/temp);
      }

/* ---- Aspect ratio */

    if (FLAG(obj2.elong))
      obj2->elong = obj->a/obj->b;

    if (FLAG(obj2.ellip))
      obj2->ellip = 1-obj->b/obj->a;

/* ---- Star/Galaxy classification */

    if (FLAG(obj2.sprob))
      {
       int	j;
       double	fac2, input[10], output, fwhm;

      fwhm = prefs.seeing_fwhm;

      fac2 = fwhm/field->pixscale;
      fac2 *= fac2;
      input[j=0] = log10(obj->iso[0]? obj->iso[0]/fac2: 0.01);
      input[++j] = field->thresh>0.0?
		  log10(obj->peak>0.0? obj->peak/field->thresh: 0.1)
		 :-1.0;
      for (i=1; i<NISO; i++)
        input[++j] = log10(obj->iso[i]? obj->iso[i]/fac2: 0.01);
      input[++j] = log10(fwhm);
      neurresp(input, &output);
      obj2->sprob = (float)output;
      }

/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/
/*-- Put here your calls to "BLIND" custom functions. Ex:

    compute_myotherparams(obj); 

--*/

/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/

    newnumber = ++cat.ntotal;
/*-- update segmentation map */
    if (check=prefs.check[CHECK_SEGMENTATION])
      {
       USHORT	*pix;
       USHORT	newsnumber = newnumber,
		oldsnumber = obj->number;
       int	dx,dx0,dy,dpix;

      pix = (USHORT *)check->pix + check->width*obj->ymin + obj->xmin;
      dx0 = obj->xmax-obj->xmin+1;
      dpix = check->width-dx0;
      for (dy=obj->ymax-obj->ymin+1; dy--; pix += dpix)
        for (dx=dx0; dx--; pix++)
          if (*pix==oldsnumber)
            *pix = newsnumber;
      }
    obj->number = newnumber;

    if (FLAG(obj2.vignet))
      copyimage(field,outobj2.vignet,prefs.vignetsize[0],prefs.vignetsize[1],
	ix,iy);

    if (FLAG(obj2.vigshift))
      copyimage_center(field, outobj2.vigshift, prefs.vigshiftsize[0],
		prefs.vigshiftsize[1], obj->mx, obj->my);

/*--- Express everything in magnitude units */
    computemags(field, obj);

/*------------------------------- PSF fitting ------------------------------*/
    nsub = 1;
    if (prefs.psf_flag)
      {
      psf_fit(thepsf, field, wfield, obj);
      obj2->npsf = thepsfit->npsf;
      if (prefs.psfdisplay_type == PSFDISPLAY_SPLIT)
        {
        nsub = thepsfit->npsf;
        if (nsub<1)
          nsub = 1;
        }
      else
        for (j=0; j<thepsfit->npsf; j++)
          {
          if (FLAG(obj2.x_psf) && j<prefs.psf_xsize)
            obj2->x_psf[j] = thepsfit->x[j];
          if (FLAG(obj2.y_psf) && j<prefs.psf_ysize)
            obj2->y_psf[j] = thepsfit->y[j];
          if (FLAG(obj2.flux_psf) && j<prefs.psf_fluxsize)
            obj2->flux_psf[j] = thepsfit->flux[j];
          if (FLAG(obj2.mag_psf) && j<prefs.psf_magsize)
            obj2->mag_psf[j] = thepsfit->flux[j]>0.0?
		prefs.mag_zeropoint -2.5*log10(thepsfit->flux[j]) : 99.0;
          }
      }

/*-------------------------------- Astrometry ------------------------------*/
    if (prefs.world_flag)
      computeastrom(field, obj);

/*-- Go through each newly identified component */
    for (j=0; j<nsub; j++)
      {
      if (prefs.psf_flag && prefs.psfdisplay_type == PSFDISPLAY_SPLIT)
        {
        if (FLAG(obj2.x_psf))
          obj2->x_psf[0] = thepsfit->x[j];
        if (FLAG(obj2.y_psf))
          obj2->y_psf[0] = thepsfit->y[j];
        if (FLAG(obj2.flux_psf))
          obj2->flux_psf[0] = thepsfit->flux[j];
        if (FLAG(obj2.mag_psf))
          obj2->mag_psf[0] = thepsfit->flux[j]>0.0?
		prefs.mag_zeropoint -2.5*log10(thepsfit->flux[j]) : 99.0;
        if (j)
          obj->number = ++cat.ntotal;
        }

      FPRINTF(OUTPUT, "%8d %6.1f %6.1f %5.1f %5.1f %12g "
			"%c%c%c%c%c%c%c%c\n",
	obj->number, obj->mx+1.0, obj->my+1.0,
	obj->a, obj->b,
	obj->flux,
	obj->flag&OBJ_CROWDED?'C':'_',
	obj->flag&OBJ_MERGED?'M':'_',
	obj->flag&OBJ_SATUR?'S':'_',
	obj->flag&OBJ_TRUNC?'T':'_',
	obj->flag&OBJ_APERT_PB?'A':'_',
	obj->flag&OBJ_ISO_PB?'I':'_',
	obj->flag&OBJ_DOVERFLOW?'D':'_',
	obj->flag&OBJ_OVERFLOW?'O':'_');
      writecat(n, objlist);
      }
    }

/* Remove again from the image the object's pixels if BLANKing is on ... */
/*-- ... and free memory */

  if (prefs.blank_flag && obj->blank)
    {
    if (selecflag)
      {
      if (prefs.somfit_flag && (check=prefs.check[CHECK_MAPSOM]))
        blankcheck(check, obj->blank, obj->subw, obj->subh,
		obj->subx, obj->suby, (PIXTYPE)*(obj2->vector_somfit));

      }
    blankimage(field, obj->blank, obj->subw, obj->subh,
		obj->subx, obj->suby, -BIG);
    free(obj->blank);
    if (obj->dblank)
      {
      blankimage(dfield, obj->dblank, obj->subw, obj->subh,
		obj->subx, obj->suby, -BIG);
      free(obj->dblank);
      }
    }

  return;
  }

