/*
 				astrom.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*                       P.W.DRAPER, Starlink & Durham University
*
*	Contents:	Astrometrical computations.
*
*	Last modify:	05/04/99: (EB)
*	Last modify:	13/06/98 (EB)
*                       04/01/99 (PWD): Converted to use NDF AST
*                       information for coordinate transformations
*                       (including precession).  Model of local affine
*                       transform is retained for modifying
*                       measurements based on area and angle etc.
*                       25/09/99 (PWD): Added astNorm calls after all
*                       astTran2s.
*                       Added R2D and D2R as these have been lost from
*                       AST (2.0).
*	Last modify:	03/04/2003
*                       (EB): 2.3.
*	Last modify:	26/11/2003
*	Last modify:	24/08/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<math.h>
#include	<stdlib.h>
#include        <string.h>

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"
#include	"astrom.h"

static obj2struct	*obj2 = &outobj2;

#ifndef PI
#define PI 3.14159265358979323846
#endif
#ifndef R2D
#define R2D (180.0/PI)
#endif
#ifndef D2R
#define D2R (PI/180.0)
#endif

static void fk5( picstruct *field, double inalp, double indec,
                 double *outalp, double *outdec );
static void fk4( picstruct *field, double inalp, double indec,
                 double *outalp, double *outdec );


/****************************** initastrom **********************************/
/*
  Initialize astrometrical structures.
*/
void initastrom( picstruct *field ) {
   astromstruct	*as;
   double	*lm;
   AstSkyFrame  *current, *blank;

   as = field->astrom;

   /* Test if the WCS is in use */
   if ( as->wcs_flag ) {

     /*  Create a FrameSet, which has a SkyFrame with the input
         celestial coordinate system as its base and a blank SkyFrame
         as current. This is used for precessing coordinates. */
     current = astGetFrame( field->astwcs, AST__CURRENT );
     blank = astSkyFrame( "" );
     as->cvt = astConvert( current, blank, "" );
     current = astAnnul( current );
     blank = astAnnul( blank );

     /*-- Compute an "average linear matrix" (at field center) */
     compute_wcs( field, (field->width+1)/2.0, (field->height+1)/2.0 );

     /*---- Compute Pole coordinates in J2000 and/or B1950 for THETAs */
     if (FLAG(obj2.theta2000) || FLAG(obj2.theta1950)
	|| FLAG(obj2.poserr_theta2000) || FLAG(obj2.poserr_theta1950)
	|| FLAG(obj2.win_theta2000) || FLAG(obj2.win_theta1950)
        || FLAG(obj2.winposerr_theta2000) || FLAG(obj2.winposerr_theta1950))
      {
       fk5( field, 0.0, 90.0, &as->ap2000, &as->dp2000);
       if (FLAG(obj2.theta1950) || FLAG(obj2.poserr_theta1950)) {
         fk4( field, 0.0, 90.0, &as->ap1950, &as->dp1950);
       }
     }

   } else {

     /*  No WCS structure. Implement a unit identity mapping to be used
         for parameters that are required. */
     lm = as->linmat;
     lm[0] = 0.0;
     lm[1] = 1.0;
     lm[2] = 0.0;
     lm[3] = 1.0;
     as->lindet = lm[0]*lm[3] - lm[1]*lm[2];
     warning ( "WORLD-parameters", " will be incorrect as your image does "
               "not have a valid WCS" );
   }

   /* Override astrometric definitions only if user supplies a pixel-scale */
   if ( prefs.pixel_scale == 0.0 ) {
     as->pixscale = sqrt(fabs(as->lindet));
     field->pixscale = 3600.0*as->pixscale;	/* in arcsec2 */
   } else {
     as->pixscale = (field->pixscale=prefs.pixel_scale)/3600.0;
   }
   return;
 }


/**************************** computeastrom *********************************/
/*
  Compute real WORLD coordinates and dimensions according to WCS info.
*/
void computeastrom(picstruct *field, objstruct *obj) {
  astromstruct	*as;
  double	*lm, *wcspos;

  as = field->astrom;
  lm = as->linmat;

  /* If working with WCS, compute WORLD coordinates and local matrix */
  if (FLAG(obj2.mxw))
    {
    if (as->wcs_flag)
      {
       wcspos = compute_wcs(field, obj2->posx, obj2->posy);
       obj2->alphas = obj2->mxw = wcspos[0];
       obj2->deltas = obj2->myw = wcspos[1];
       if (FLAG(obj2.alpha2000))
         {
          /*  Get coordinates in FK5, if not already available */
          fk5(field, wcspos[0], wcspos[1], &obj2->alpha2000, &obj2->delta2000);

          if (FLAG(obj2.alpha1950))
            {
             /*  Get coordinates in FK4 */
             fk4(field, wcspos[0], wcspos[1], &obj2->alpha1950, &obj2->delta1950);
            }
         }
      }
    else
      {
       /*  Request for world coordinates, but no WCS.*/
       double	dx,dy;
       dx = obj2->posx;
       dy = obj2->posy;
       obj2->mxw = lm[0]*dx + lm[1]*dy;	/* CDELT included! */
       obj2->myw = lm[2]*dx + lm[3]*dy;	/* CDELT included! */
      }
    }

/* Same for peak-flux positions */
  if (FLAG(obj2.peakxw))
    {
     if (as->wcs_flag)
       {
        wcspos = compute_wcs( field, (double)obj->peakx, (double)obj->peaky );
        obj2->peakalphas = obj2->peakxw = wcspos[0];
        obj2->peakdeltas = obj2->peakyw = wcspos[1];
        if ( FLAG(obj2.peakalpha2000 ) )
          {
           fk5( field, wcspos[0], wcspos[1], &obj2->peakalpha2000, &obj2->peakdelta2000);
           if ( FLAG(obj2.peakalpha1950 ) )
             {
              fk4( field, wcspos[0], wcspos[1], &obj2->peakalpha1950, &obj2->peakdelta1950);
             }
          }
       }
     else
       {
        /* Need WCS positions, but no WCS available. */
        double	dx,dy;
        dx = obj->peakx;
        dy = obj->peaky;
        obj2->peakxw = lm[0]*dx + lm[1]*dy;	/* CDELT included! */
        obj2->peakyw = lm[2]*dx + lm[3]*dy;	/* CDELT included! */
       }
    }

/* Idem for Windowed positions */
  if (FLAG(obj2.winpos_xw))
    {
    if (as->wcs_flag)
      {
      wcspos = compute_wcs(field, obj2->winpos_x, obj2->winpos_y);
      obj2->winpos_alphas = obj2->winpos_xw = wcspos[0];
      obj2->winpos_deltas = obj2->winpos_yw = wcspos[1];
      if (FLAG(obj2.winpos_alpha2000))
        {
        if (fabs(as->equinox-2000.0)>0.003)
          fk5( field, wcspos[0], wcspos[1], 
                &obj2->winpos_alpha2000, &obj2->winpos_delta2000);
        else
          {
          obj2->winpos_alpha2000 = obj2->winpos_xw;
          obj2->winpos_delta2000 = obj2->winpos_yw;
          }
        if (FLAG(obj2.winpos_alpha1950))
          fk4( field, obj2->winpos_alpha2000, obj2->winpos_delta2000,
                &obj2->winpos_alpha1950, &obj2->winpos_delta1950);
        }
      }
    else
      {
       double	dx,dy;

      dx = obj2->winpos_x;
      dy = obj2->winpos_y;
      obj2->winpos_xw = lm[0]*dx + lm[1]*dy;/* CDELT included! */
      obj2->winpos_yw = lm[2]*dx + lm[3]*dy;/* CDELT included! */
      }
    }

  /* Custom coordinate system for the MAMA machine */
  if ( FLAG(obj2.mamaposx ) ) {
    double	dx,dy;

    dx = obj2->posx - 0.5;
    dy = obj2->posy - 0.5;
    obj2->mamaposx = (lm[2]*dx+lm[3]*dy)
      *(prefs.mama_corflex+1.0);	                /* CDELT included! */
    obj2->mamaposy = (lm[0]*dx+lm[1]*dy);	/* CDELT included! */
  }

/* Express shape parameters in WORLD frame */
  if ( FLAG(obj2.mx2w) )
    astrom_shapeparam(field, obj);
  if (FLAG(obj2.win_mx2w))
    astrom_winshapeparam(field, obj);

/* Express position error parameters in WORLD frame */
  if (FLAG(obj2.poserr_mx2w))
    astrom_errparam(field, obj);
  if (FLAG(obj2.winposerr_mx2w))
    astrom_winerrparam(field, obj);

  if (FLAG(obj2.npixw))
    obj2->npixw = obj->npix*as->pixscale*as->pixscale;
  if (FLAG(obj2.fdnpixw))
    obj2->fdnpixw = obj->fdnpix*as->pixscale*as->pixscale;

  if (FLAG(obj2.fwhmw))
    obj2->fwhmw = obj->fwhm*as->pixscale;

  return;
}

/***************************************************************************/
/*
  Normalise position into correct RA/Dec range
*/
static void norm_wcs( AstFrameSet *fset, double *x, double *y )
{
  double point[2];
  point[0] = *x;
  point[1] = *y;
  astNorm( fset, point );
  *x = point[0];
  *y = point[1];
}


/****************************** compute_wcs *********************************/
/*
  Compute real WORLD coordinates and local distortion matrix according to the
  WCS info.
*/
double  *compute_wcs(picstruct *field, double mx, double my)
{
  astromstruct	*as;
  static double	wcspos[2], wcspos0[2];
  double        xin[1], yin[1], xout[1], yout[1];
  double	*lm, al, da, de, cde;

  as = field->astrom;
  lm = as->linmat;

  /*  Transform position to world coordinates */
  xin[0] = mx;
  yin[0] = my;
  astTran2( field->astwcs, 1, xin, yin, 1, xout, yout );
  norm_wcs( field->astwcs, xout, yout );
  if ( ! astOK ) {
    error(EXIT_FAILURE, "*Error*"," transforming to world coordinates: " );
  }
  /* Compute the local distortion matrix, note results are in radians */
  al = wcspos0[0] = xout[0] * R2D;
  de = wcspos0[1] = yout[0] * R2D;

  /* Get world coordinates for vector 1,0 */
  xin[0] = mx + 1;
  yin[0] = my;
  astTran2( field->astwcs, 1, xin, yin, 1, xout, yout );
  norm_wcs( field->astwcs, xout, yout );
  if ( ! astOK ) {
    error(EXIT_FAILURE, "*Error*"," transforming to world coordinates: " );
  }
  wcspos[0] = xout[0] * R2D;
  wcspos[1] = yout[0] * R2D;

  da = wcspos[0]-al;
  if (da>180.0)
    da -= 360.0;
  else if (da<-180.0)
    da += 360.0;

  lm[0] = da*(cde=cos(de*DEG));
  lm[1] = wcspos[1] - de;

  /* Get world coordinates for vector 0,1 */
  /* Second one */
  xin[0] = mx;
  yin[0] = my + 1;
  astTran2( field->astwcs, 1, xin, yin, 1, xout, yout );
  norm_wcs( field->astwcs, xout, yout );
  if ( ! astOK ) {
    error(EXIT_FAILURE, "*Error*"," transforming to world coordinates: " );
  }
  wcspos[0] = xout[0] * R2D;
  wcspos[1] = yout[0] * R2D;

  da = wcspos[0]-al;
  if (da>180.0)
    da -= 360.0;
  else if (da<-180.0)
    da += 360.0;

  lm[2] = da*cde;
  lm[3] = wcspos[1] - de;

  as->lindet = lm[0]*lm[3] - lm[1]*lm[2];
  if (as->lindet == 0.0)
    warning ("Null determinant in the local distortion matrix:\n",
             "         Some WORLD-parameters will be incorrect");

  if (prefs.pixel_scale == 0.0)
    as->pixscale = sqrt(fabs(as->lindet));

  return wcspos0;
}


/****************************** astrom_shapeparam ****************************/
/*
Compute shape parameters in WORLD and SKY coordinates.
*/
void	astrom_shapeparam(picstruct *field, objstruct *obj)
  {
   astromstruct	*as;
   double	*lm,
		dx2,dy2,dxy, xm2,ym2,xym, temp,pm2;

  as = field->astrom;
  lm = as->linmat;

/* All WORLD params based on 2nd order moments have to pass through here */
  dx2 = obj->mx2;
  dy2 = obj->my2;
  dxy = obj->mxy;
  obj2->mx2w = xm2 = lm[0]*lm[0]*dx2 + lm[1]*lm[1]*dy2 + lm[0]*lm[1]*dxy;
  obj2->my2w = ym2 = lm[2]*lm[2]*dx2 + lm[3]*lm[3]*dy2 + lm[2]*lm[3]*dxy;
  obj2->mxyw = xym = lm[0]*lm[2]*dx2 + lm[1]*lm[3]*dy2
			+ (lm[0]*lm[3]+lm[1]*lm[2])*dxy;
  temp=xm2-ym2;
  if (FLAG(obj2.thetaw))
    {
    obj2->thetaw = (temp == 0.0)? (45.0) : (0.5*atan2(2.0 * xym,temp)/DEG);
    if (as->wcs_flag && FLAG(obj2.thetas))
      obj2->thetas = obj2->thetaw + (obj2->thetaw>0.0?-90:90.0);

/*-- Compute position angles in J2000 or B1950 reference frame */
    if (as->wcs_flag)
      {
       double	da,dd;

      if (FLAG(obj2.theta2000))
        {
        da = as->ap2000 - obj2->alpha2000;
        dd = (sin(as->dp2000*DEG)
		-sin(obj2->delta2000*DEG)*sin(obj2->deltas*DEG))
		/(cos(obj2->delta2000*DEG)*cos(obj2->deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->theta2000 = obj2->thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }

      if (FLAG(obj2.theta1950))
        {
        da = as->ap1950 - obj2->alpha1950;
        dd = (sin(as->dp1950*DEG)
		-sin(obj2->delta1950*DEG)*sin(obj2->deltas*DEG))
		/(cos(obj2->delta1950*DEG)*cos(obj2->deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->theta1950 = obj2->thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }
      }
    }

  if (FLAG(obj2.aw))
    {
    temp = sqrt(0.25*temp*temp+xym*xym);
    pm2 = 0.5*(xm2+ym2);
    obj2->aw = (float)sqrt(pm2+temp);
    obj2->bw = (float)sqrt(pm2-temp);
    obj2->polarw = temp / pm2;
    }

  if (FLAG(obj2.cxxw))
    {
/*-- Handle large, fully correlated profiles (can cause a singularity...) */
    if ((temp=xm2*ym2-xym*xym)<1e-6)
      {
      temp = 1e-6;
      xym *= 0.99999;
      }
    obj2->cxxw = (float)(ym2/temp);
    obj2->cyyw = (float)(xm2/temp);
    obj2->cxyw = (float)(-2*xym/temp);
    }

  return;
  }

/**************************** astrom_winshapeparam ***************************/
/*
Compute shape parameters in WORLD and SKY coordinates.
*/
void	astrom_winshapeparam(picstruct *field, objstruct *obj)
  {
   astromstruct	*as;
   double	*lm,
		dx2,dy2,dxy, xm2,ym2,xym, temp,pm2;

  as = field->astrom;
  lm = as->linmat;

/* All WORLD params based on 2nd order moments have to pass through here */
  dx2 = obj2->win_mx2;
  dy2 = obj2->win_my2;
  dxy = obj2->win_mxy;
  obj2->win_mx2w = xm2 = lm[0]*lm[0]*dx2 + lm[1]*lm[1]*dy2 + lm[0]*lm[1]*dxy;
  obj2->win_my2w = ym2 = lm[2]*lm[2]*dx2 + lm[3]*lm[3]*dy2 + lm[2]*lm[3]*dxy;
  obj2->win_mxyw = xym = lm[0]*lm[2]*dx2 + lm[1]*lm[3]*dy2
			+ (lm[0]*lm[3]+lm[1]*lm[2])*dxy;
  temp=xm2-ym2;
  if (FLAG(obj2.win_thetaw))
    {
    obj2->win_thetaw = (temp == 0.0)? (45.0) : (0.5*atan2(2.0*xym,temp)/DEG);
    if (as->wcs_flag && FLAG(obj2.win_thetas))
      obj2->win_thetas = obj2->win_thetaw +
	(obj2->win_thetaw>0.0?-90:90.0);

/*-- Compute position angles in J2000 or B1950 reference frame */
    if (as->wcs_flag)
      {
       double	da,dd;

      if (FLAG(obj2.win_theta2000))
        {
        da = as->ap2000 - obj2->winpos_alpha2000;
        dd = (sin(as->dp2000*DEG)
		-sin(obj2->winpos_delta2000*DEG)*sin(obj2->winpos_deltas*DEG))
		/(cos(obj2->winpos_delta2000*DEG)*cos(obj2->winpos_deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->win_theta2000 = obj2->win_thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }

      if (FLAG(obj2.win_theta1950))
        {
        da = as->ap1950 - obj2->winpos_alpha1950;
        dd = (sin(as->dp1950*DEG)
		-sin(obj2->winpos_delta1950*DEG)*sin(obj2->winpos_deltas*DEG))
		/(cos(obj2->winpos_delta1950*DEG)*cos(obj2->winpos_deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->win_theta1950 = obj2->win_thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }
      }
    }

  if (FLAG(obj2.win_aw))
    {
    temp = sqrt(0.25*temp*temp+xym*xym);
    pm2 = 0.5*(xm2+ym2);
    obj2->aw = (float)sqrt(pm2+temp);
    obj2->bw = (float)sqrt(pm2-temp);
    obj2->win_polarw = temp / pm2;
    }

  if (FLAG(obj2.win_cxxw))
    {
/*-- Handle large, fully correlated profiles (can cause a singularity...) */
    if ((temp=xm2*ym2-xym*xym)<1e-6)
      {
      temp = 1e-6;
      xym *= 0.99999;
      }
    obj2->cxxw = (float)(ym2/temp);
    obj2->cyyw = (float)(xm2/temp);
    obj2->cxyw = (float)(-2*xym/temp);
    }

  return;
  }

/******************************* astrom_errparam *****************************/
/*
Compute error ellipse parameters in WORLD and SKY coordinates.
*/
void	astrom_errparam(picstruct *field, objstruct *obj)
  {
   astromstruct	*as;
   double	*lm,
		dx2,dy2,dxy, xm2,ym2,xym, temp,pm2;

  as = field->astrom;
  lm = as->linmat;

/* All WORLD params based on 2nd order moments have to pass through here */
  dx2 = obj->poserr_mx2;
  dy2 = obj->poserr_my2;
  dxy = obj->poserr_mxy;
  obj2->poserr_mx2w = xm2 = lm[0]*lm[0]*dx2+lm[1]*lm[1]*dy2+lm[0]*lm[1]*dxy;
  obj2->poserr_my2w = ym2 = lm[2]*lm[2]*dx2+lm[3]*lm[3]*dy2+lm[2]*lm[3]*dxy;
  obj2->poserr_mxyw = xym = lm[0]*lm[2]*dx2+lm[1]*lm[3]*dy2
				+ (lm[0]*lm[3]+lm[1]*lm[2])*dxy;
  temp=xm2-ym2;
  if (FLAG(obj2.poserr_thetaw))
    {
    obj2->poserr_thetaw = (temp==0.0)? (45.0):(0.5*atan2(2.0*xym,temp)/DEG);
    if (as->wcs_flag && FLAG(obj2.poserr_thetas))
      obj2->poserr_thetas = obj2->poserr_thetaw
				+ (obj2->poserr_thetaw>0.0? -90:90.0);

/*-- Compute position angles in J2000 or B1950 reference frame */
    if (as->wcs_flag)
      {
       double	da,dd;

      if (FLAG(obj2.poserr_theta2000))
        {
        da = as->ap2000 - obj2->alpha2000;
        dd = (sin(as->dp2000*DEG)
		-sin(obj2->delta2000*DEG)*sin(obj2->deltas*DEG))
		/(cos(obj2->delta2000*DEG)*cos(obj2->deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->poserr_theta2000 = obj2->poserr_thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }

      if (FLAG(obj2.poserr_theta1950))
        {
        da = as->ap1950 - obj2->alpha1950;
        dd = (sin(as->dp1950*DEG)
		-sin(obj2->delta1950*DEG)*sin(obj2->deltas*DEG))
		/(cos(obj2->delta1950*DEG)*cos(obj2->deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->poserr_theta1950 = obj2->poserr_thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }
      }
    }

  if (FLAG(obj2.poserr_aw))
    {
    temp = sqrt(0.25*temp*temp+xym*xym);
    pm2 = 0.5*(xm2+ym2);
    obj2->poserr_aw = (float)sqrt(pm2+temp);
    obj2->poserr_bw = (float)sqrt(pm2-temp);
    }

  if (FLAG(obj2.poserr_cxxw))
    {
/*-- Handle large, fully correlated profiles (can cause a singularity...) */
    if ((temp=xm2*ym2-xym*xym)<1e-6)
      {
      temp = 1e-6;
      xym *= 0.99999;
      }
    obj2->poserr_cxxw = (float)(ym2/temp);
    obj2->poserr_cyyw = (float)(xm2/temp);
    obj2->poserr_cxyw = (float)(-2*xym/temp);
    }

  return;
  }

/***************************** astrom_winerrparam ***************************/
/*
Compute error ellipse parameters in WORLD and SKY coordinates.
*/
void	astrom_winerrparam(picstruct *field, objstruct *obj)
  {
   astromstruct	*as;
   double	*lm,
		dx2,dy2,dxy, xm2,ym2,xym, temp,pm2;

  as = field->astrom;
  lm = as->linmat;

/* All WORLD params based on 2nd order moments have to pass through here */
  dx2 = obj2->winposerr_mx2;
  dy2 = obj2->winposerr_my2;
  dxy = obj2->winposerr_mxy;
  obj2->winposerr_mx2w = xm2 = lm[0]*lm[0]*dx2+lm[1]*lm[1]*dy2+lm[0]*lm[1]*dxy;
  obj2->winposerr_my2w = ym2 = lm[2]*lm[2]*dx2+lm[3]*lm[3]*dy2+lm[2]*lm[3]*dxy;
  obj2->winposerr_mxyw = xym = lm[0]*lm[2]*dx2+lm[1]*lm[3]*dy2
				+ (lm[0]*lm[3]+lm[1]*lm[2])*dxy;
  temp=xm2-ym2;
  if (FLAG(obj2.winposerr_thetaw))
    {
    obj2->winposerr_thetaw = (temp==0.0)? (45.0):(0.5*atan2(2.0*xym,temp)/DEG);
    if (as->wcs_flag && FLAG(obj2.winposerr_thetas))
      obj2->winposerr_thetas = obj2->winposerr_thetaw
				+ (obj2->winposerr_thetaw>0.0? -90:90.0);

/*-- Compute position angles in J2000 or B1950 reference frame */
    if (as->wcs_flag)
      {
       double	da,dd;

      if (FLAG(obj2.winposerr_theta2000))
        {
        da = as->ap2000 - obj2->winpos_alpha2000;
        dd = (sin(as->dp2000*DEG)
		-sin(obj2->winpos_delta2000*DEG)*sin(obj2->winpos_deltas*DEG))
		/(cos(obj2->winpos_delta2000*DEG)*cos(obj2->winpos_deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->winposerr_theta2000 = obj2->winposerr_thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }

      if (FLAG(obj2.winposerr_theta1950))
        {
        da = as->ap1950 - obj2->winpos_alpha1950;
        dd = (sin(as->dp1950*DEG)
		-sin(obj2->winpos_delta1950*DEG)*sin(obj2->winpos_deltas*DEG))
		/(cos(obj2->winpos_delta1950*DEG)*cos(obj2->winpos_deltas*DEG));
        dd = dd<1.0? (dd>-1.0?acos(dd)/DEG:180.0) : 0.0;
        obj2->winposerr_theta1950 = obj2->winposerr_thetas
		+ (((da>0.0 && da<180.0) || da<-180.0)?-dd:dd);
        }
      }
    }

  if (FLAG(obj2.winposerr_aw))
    {
    temp = sqrt(0.25*temp*temp+xym*xym);
    pm2 = 0.5*(xm2+ym2);
    obj2->winposerr_aw = (float)sqrt(pm2+temp);
    obj2->winposerr_bw = (float)sqrt(pm2-temp);
    }

  if (FLAG(obj2.winposerr_cxxw))
    {
/*-- Handle large, fully correlated profiles (can cause a singularity...) */
    if ((temp=xm2*ym2-xym*xym)<1e-6)
      {
      temp = 1e-6;
      xym *= 0.99999;
      }
    obj2->winposerr_cxxw = (float)(ym2/temp);
    obj2->winposerr_cyyw = (float)(xm2/temp);
    obj2->winposerr_cxyw = (float)(-2*xym/temp);
    }

  return;
  }


/******************************* copyastrom *********************************/
/*
Copy astrometrical structures.
*/
void	copyastrom(picstruct *infield, picstruct *outfield)
{
  astromstruct	*inas, *outas;

  if (infield->astrom) {
    QMEMCPY(infield->astrom, outfield->astrom, astromstruct, 1);
    inas = infield->astrom;
    outas = outfield->astrom;

    /*  Copy precession FrameSet */
    if ( inas->cvt ) {
      outas->cvt = astCopy( inas->cvt );
    }
  }

  return;
}


/******************************* endastrom ***********************************/
/*
Free astrometrical structures.
*/
void	endastrom(picstruct *field)
{
  astromstruct	*as;
  as = field->astrom;

  /* Release precession FrameSet */
  if ( as->cvt ) {
    as->cvt = astAnnul( as->cvt );
  }

  free(as);
  return;
}


/****************************** fk5 *****************************************/
/*
 *  Tranform image world coordinates (in degrees) to FK5/J2000.
 */
static void fk5( picstruct *field, double inalp, double indec,
                 double *outalp, double *outdec )
{
  astromstruct	*as;
  int isfk5;           /*  True if field coordinates are already FK5 */
  double equinox;      /*  Equinox of field celestial coordinates */
  AstFrameSet *cvt;    /*  FrameSet for converting coordinates */
  double a[1], b[1], x[1], y[1];   /*  Input/output coordinates in radians */
  double point[2];     /*  Coordinates of a position */

  as = field->astrom;

  /*  If image is in FK5/J2000, then do nothing */
  isfk5 = ( strcmp( astGetC( field->astwcs, "System" ), "FK5" ) == 0 );
  equinox = astGetD( field->astwcs, "Equinox" );
  if ( ! isfk5 | ( isfk5 && ( equinox != 2000.0 ) ) ) {

    /* Need to precess coordinates... */
    cvt = (AstFrameSet *)as->cvt;
    astSet( cvt, "System=FK5, Equinox=J2000" );
    a[0] = inalp * D2R;
    b[0] = indec * D2R;
    astTran2( cvt, 1, a, b, 1, x, y );
    point[0] = x[0];
    point[1] = y[0];
    astNorm( cvt, point );
    *outalp = point[0] * R2D;
    *outdec = point[1] * R2D;
  } else {
    /*  Input coordinates are already correct */
    *outalp = inalp;
    *outdec = indec;
  }
}

/****************************** fk4 *****************************************/
/*
 *  Tranform image world coordinates (in degrees) to FK4/B1950.
 */
static void fk4( picstruct *field, double inalp, double indec,
                 double *outalp, double *outdec )
{
  astromstruct	*as;
  int isfk4;           /*  True if field coordinates are already FK4 */
  double equinox;      /*  Equinox of field celestial coordinates */
  double epoch;        /*  Epoch of field celestial coordinates */
  AstFrameSet *cvt;    /*  FrameSet for converting coordinates */
  double a[1], b[1], x[1], y[1];   /*  Input/output coordinates in radians */
  double point[2];     /*  Coordinates of a position */

  as = field->astrom;

  /*  If image is in FK4/B1950/B1950, then do nothing */
  isfk4 = ( strcmp( astGetC( field->astwcs, "System" ), "FK4" ) == 0 );
  equinox = astGetD( field->astwcs, "Equinox" );
  epoch = astGetD( field->astwcs, "Epoch" );
  if ( ! isfk4 | ( isfk4 && ( equinox != 1950.0 ) && ( epoch != 1950.0 ) ) ) {

    /* Need to precess coordinates... */
    cvt = as->cvt;
    astSet( cvt, "System=FK4, Equinox=B1950, Epoch=B1950" );
    a[0] = inalp * D2R;
    b[0] = indec * D2R;
    astTran2( cvt, 1, a, b, 1, x, y );
    point[0] = x[0];
    point[1] = y[0];
    astNorm( cvt, point );
    *outalp = point[0] * R2D;
    *outdec = point[1] * R2D;
  } else {
    /*  Input coordinates are already correct */
    *outalp = inalp;
    *outdec = indec;
  }
}
