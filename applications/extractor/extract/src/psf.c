 /*
 				psf.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Fit the PSF to a detection.
*
*	Last modify:	27/11/2003
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
#include	"fits/fitscat.h"
#include	"check.h"
#include	"filter.h"
#include	"image.h"
#include	"poly.h"
#include	"psf.h"

/*------------------------------- variables ---------------------------------*/


extern keystruct	objkey[];
extern objstruct	outobj;

/********************************* psf_init **********************************/
/*
Allocate memory and stuff for the PSF-fitting.
*/
void	psf_init(psfstruct *psf)
  {
  QMALLOC(thepsfit, psfitstruct, 1);
  QMALLOC(thepsfit->x, float, prefs.psf_npsfmax);
  QMALLOC(thepsfit->y, float, prefs.psf_npsfmax);
  QMALLOC(thepsfit->flux, float, prefs.psf_npsfmax);

  return;
  }  


/********************************* psf_end ***********************************/
/*
Free memory occupied by the PSF-fitting stuff.
*/
void	psf_end(psfstruct *psf)
  {
   int	d, ndim;

  if (psf->pc)
    pc_end(psf->pc);

  ndim = psf->poly->ndim;
  for (d=0; d<ndim; d++)
    free(psf->contextname[d]);
  free(psf->context);
  free(psf->contextname);
  free(psf->contextoffset);
  free(psf->contextscale);
  free(psf->contexttyp);
  poly_end(psf->poly);
  free(psf->maskcomp);
  free(psf->maskloc);
  free(psf->masksize);
  free(psf);

  free(thepsfit->x);
  free(thepsfit->y);
  free(thepsfit->flux);
  free(thepsfit);

  return;
  }


/********************************* psf_load *********************************/
/*
Read the PSF data from a FITS file.
*/
psfstruct	*psf_load(char *filename)
  {
   static objstruct	saveobj;
   static obj2struct	saveobj2;
   psfstruct		*psf;
   catstruct		*cat;
   tabstruct		*tab;
   keystruct		*key;
   char			*head, *ci,*co;
   int			deg[POLY_MAXDIM], group[POLY_MAXDIM], ndim, ngroup,
			i,k;

/* Open the cat (well it is not a "cat", but simply a FITS file */
  if (!(cat = read_cat(filename)))
    error(EXIT_FAILURE, "*Error*: PSF file not found: ", filename);

/* OK, we now allocate memory for the PSF structure itself */
  QCALLOC(psf, psfstruct, 1);

/* Store a short copy of the PSF filename */
  if ((ci=strrchr(filename, '/')))
    strcpy(psf->name, ci+1);
  else
    strcpy(psf->name, filename);

  if (!(tab = name_to_tab(cat, "PSF_DATA", 0)))
    error(EXIT_FAILURE, "*Error*: PSF_DATA table not found in catalog ",
	filename);

  head = tab->headbuf;

/*-- Dimension of the polynomial */
  if (fitsread(head, "POLNAXIS", &ndim, H_INT,T_LONG) == RETURN_OK
	&& ndim)
    {
/*-- So we have a polynomial description of the PSF variations */
    if (ndim > POLY_MAXDIM)
        {
        sprintf(gstr, "*Error*: The POLNAXIS parameter in %s exceeds %d",
		psf->name, POLY_MAXDIM);
        error(EXIT_FAILURE, gstr, "");
        }

    QMALLOC(psf->contextname, char *, ndim);
    QMALLOC(psf->context, double *, ndim);
    QMALLOC(psf->contexttyp, t_type, ndim);
    QMALLOC(psf->contextoffset, double, ndim);
    QMALLOC(psf->contextscale, double, ndim);

/*-- We will have to use the outobj structs, so we first save their content */
    saveobj = outobj;
    saveobj2 = outobj2;
/*-- outobj's are used as FLAG arrays, so we initialize them to 0 */
    memset(&outobj, 0, sizeof(outobj));
    memset(&outobj2, 0, sizeof(outobj2));
    for (i=0; i<ndim; i++)
      {
/*---- Polynomial groups */
      sprintf(gstr, "POLGRP%1d", i+1);
      if (fitsread(head, gstr, &group[i], H_INT,T_LONG) != RETURN_OK)
        goto headerror;

/*---- Contexts */
      QMALLOC(psf->contextname[i], char, 80);
      sprintf(gstr, "POLNAME%1d", i+1);
      if (fitsread(head,gstr,psf->contextname[i],H_STRING,T_STRING)!=RETURN_OK)
        goto headerror;
      if (*psf->contextname[i]==(char)':')
/*------ It seems we're facing a FITS header parameter */
        psf->context[i] = NULL;	/* This is to tell we'll have to load */
				/* a FITS header context later on */
      else
/*------ The context element is a dynamic object parameter */
        {
        if ((k = findkey(psf->contextname[i], (char *)objkey,
		sizeof(keystruct)))==RETURN_ERROR)
          {
          sprintf(gstr, "*Error*: %s CONTEXT parameter in %s unknown",
		psf->contextname[i], psf->name);
          error(EXIT_FAILURE, gstr, "");
          }
        key = objkey+k;
        psf->context[i] = key->ptr;
        psf->contexttyp[i] = key->ttype;
/*------ Declare the parameter "active" to trigger computation by SExtractor */
        *((char *)key->ptr) = (char)'\1';
        }
/*---- Scaling of the context parameter */
      sprintf(gstr, "POLZERO%1d", i+1);
      if (fitsread(head, gstr, &psf->contextoffset[i], H_EXPO, T_DOUBLE)
		!=RETURN_OK)
        goto headerror;
      sprintf(gstr, "POLSCAL%1d", i+1);
      if (fitsread(head, gstr, &psf->contextscale[i], H_EXPO, T_DOUBLE)
		!=RETURN_OK)
        goto headerror;
      }

/*-- Number of groups */
    if (fitsread(head, "POLNGRP ", &ngroup, H_INT, T_LONG) != RETURN_OK)
      goto headerror;

    for (i=0; i<ngroup; i++)
      {
/*---- Polynomial degree for each group */
      sprintf(gstr, "POLDEG%1d", i+1);
      if (fitsread(head, gstr, &deg[i], H_INT,T_LONG) != RETURN_OK)
        goto headerror;
      }

    psf->poly = poly_init(group, ndim, deg, ngroup);

/*-- Update the permanent FLAG arrays (that is, perform an "OR" on them) */
    for (ci=(char *)&outobj,co=(char *)&flagobj,i=sizeof(objstruct); i--;)
      *(co++) |= *(ci++);
    for (ci=(char *)&outobj2,co=(char *)&flagobj2,i=sizeof(obj2struct); i--;)
      *(co++) |= *(ci++);

/*-- Restore previous outobj contents */
    outobj = saveobj;
    outobj2 = saveobj2;
    }
  else
    {
/*-- This is a simple, constant PSF */
    psf->poly = poly_init(group, 0, deg, 0);
    psf->context = NULL;
    }

/* Dimensionality of the PSF mask */
  if (fitsread(head, "PSFNAXIS", &psf->maskdim, H_INT, T_LONG) != RETURN_OK)
    goto headerror;
  if (psf->maskdim<2 || psf->maskdim>3)
    error(EXIT_FAILURE, "*Error*: wrong dimensionality for the PSF "
	"mask in ", filename);
  QMALLOC(psf->masksize, int, psf->maskdim);
  for (i=0; i<psf->maskdim; i++)
    psf->masksize[i] = 1;
  psf->masknpix = 1;
  for (i=0; i<psf->maskdim; i++)
    {
    sprintf(gstr, "PSFAXIS%1d", i+1);
    if (fitsread(head, gstr, &psf->masksize[i], H_INT,T_LONG) != RETURN_OK)
      goto headerror;
    psf->masknpix *= psf->masksize[i];
    }

/* PSF FWHM: defaulted to 3 pixels */
 if (fitsread(head, "PSF_FWHM", &psf->fwhm, H_FLOAT,T_DOUBLE) != RETURN_OK)
    psf->fwhm = 3.0;

/* PSF oversampling: defaulted to 1 */
  if (fitsread(head, "PSF_SAMP", &psf->pixstep,H_FLOAT,T_FLOAT) != RETURN_OK)
    psf->pixstep = 1.0;

/* Load the PSF mask data */
  key = read_key(tab, "PSF_MASK");
  psf->maskcomp = key->ptr;

  psf->pc = pc_load(cat);

  QMALLOC(psf->maskloc, double, psf->masksize[0]*psf->masksize[1]);

/* But don't touch my arrays!! */
  blank_keys(tab);

  free_cat(&cat, 1);

  return psf;

headerror:
  error(EXIT_FAILURE, "*Error*: Incorrect or obsolete PSF data in ", filename);
  return NULL;
  }


/***************************** psf_readcontext *******************************/
/*
Read the PSF context parameters in the FITS header.
*/
void	psf_readcontext(psfstruct *psf, picstruct *field)
  {
   static double	contextval[POLY_MAXDIM];
   int			i, ndim;

  ndim = psf->poly->ndim;
  for (i=0; i<ndim; i++)
    if (!psf->context[i])
      {
      psf->context[i] = &contextval[i];
      psf->contexttyp[i] = T_DOUBLE;
      if (fitsread(field->fitshead, psf->contextname[i]+1, &contextval[i],
		H_FLOAT,T_DOUBLE) == RETURN_ERROR)
        {
        sprintf(gstr, "*Error*: %s parameter not found in the header of ",
		psf->contextname[i]+1);
        error(EXIT_FAILURE, gstr, field->rfilename);
        }
      }

  return;
  }


/******************************** psf_fit ***********************************/
void	psf_fit(psfstruct *psf, picstruct *field, picstruct *wfield,
		objstruct *obj)
  {
   checkstruct		*check;
   static double	/* x2[PSF_NPSFMAX],y2[PSF_NPSFMAX],xy[PSF_NPSFMAX],
			sum[PSF_NPSFMAX], */ deltax[PSF_NPSFMAX],
			deltay[PSF_NPSFMAX],flux[PSF_NPSFMAX],
			deltaxb[PSF_NPSFMAX],deltayb[PSF_NPSFMAX],
			fluxb[PSF_NPSFMAX],
			sol[PSF_NTOT], /*covmat[PSF_NTOT*PSF_NTOT], */
			vmat[PSF_NTOT*PSF_NTOT], wmat[PSF_NTOT];
   double		**psfmasks, **psfmaskx,**psfmasky,
			*data, *data2, *data3, *weight, *mat, *checkdata,
			*d, *m, *w, *ps, /* *dps,*pps, */ *px, *py,
			dx,dy, /* x1,y1, mx,my,mflux, */
			pix,pix2, wthresh,val, /* dflux, */
			backnoise2, gain, radmin2,radmax2,satlevel,
			/* chi2, mr, */
			r2, valmax, psf_fwhm;
   float		*dh, *wh,
			pixstep;
   PIXTYPE		*datah, *weighth, *cpix;
   int			i,j,k,p, npsf,npsfmax, npix, nppix, ix,iy,niter,
			width, height, pwidth,pheight, /* hw,hh, */
			x,y, /* yb, */
			xmax,ymax, wbad, gainflag, convflag, npsfflag,
			ival;

  checkdata = NULL;			/* To avoid gcc -Wall warnings */
  dx = dy = 0.0;
  niter = 0;
  npsfmax = prefs.psf_npsfmax;
  pixstep = 1.0/psf->pixstep;
  gain = prefs.gain;
  backnoise2 = field->backsig*field->backsig;
  satlevel = prefs.satur_level - obj->bkg;
  wthresh = wfield?wfield->weight_thresh:BIG;
  gainflag = prefs.weightgain_flag;
  psf_fwhm = psf->fwhm*psf->pixstep;

/* Initialize outputs */
  thepsfit->niter = 0;
  thepsfit->npsf = 0;
  for (j=0; j<npsfmax; j++)
    {
    thepsfit->x[j] = 999999.0;
    thepsfit->y[j] = 999999.0;
    thepsfit->flux[j] = 0.0;
    }

/* Scale data area with object "size" */
  ix = (obj->xmax+obj->xmin+1)/2;
  iy = (obj->ymax+obj->ymin+1)/2;
  width = obj->xmax-obj->xmin+1+psf_fwhm;
  if (width < (ival=(int)(psf_fwhm*2)))
    width = ival;
  height = obj->ymax-obj->ymin+1+psf_fwhm;
  if (height < (ival=(int)(psf_fwhm*2)))
    height = ival;
  npix = width*height;
  radmin2 = PSF_MINSHIFT*PSF_MINSHIFT;
  radmax2 = npix/2.0;

/* Scale total area with PSF FWHM */
  pwidth = (int)(psf->masksize[0]*psf->pixstep)+width;;
  pheight = (int)(psf->masksize[1]*psf->pixstep)+height;
  nppix = pwidth*pheight;

/* Allocate working space */
  QMALLOC(weighth, PIXTYPE, npix);
  QMALLOC(weight, double, npix);
  QMALLOC(datah, PIXTYPE, npix);
  QMALLOC(data, double, npix);
  QMALLOC(data2, double, npix);
  QMALLOC(data3, double, npix);
  QMALLOC(mat, double, npix*PSF_NTOT);
  if (prefs.check[CHECK_SUBPSFPROTOS] || prefs.check[CHECK_PSFPROTOS]
	|| prefs.check[CHECK_SUBPCPROTOS] || prefs.check[CHECK_PCPROTOS]
	|| prefs.check[CHECK_PCOPROTOS])
    {
    QMALLOC(checkdata, double, nppix);
    QMALLOC(checkmask, PIXTYPE, nppix);
    }

  QMALLOC(psfmasks, double *, npsfmax);
  QMALLOC(psfmaskx, double *, npsfmax);
  QMALLOC(psfmasky, double *, npsfmax);
  for (i=0; i<npsfmax; i++)
    {
    QMALLOC(psfmasks[i], double, npix);
    QMALLOC(psfmaskx[i], double, npix);
    QMALLOC(psfmasky[i], double, npix);
    }

  copyimage(field, datah, width, height, ix, iy);

/* Compute weights */
  wbad = 0;
  if (wfield)
    {
    copyimage(wfield, weighth, width, height, ix, iy);
    for (wh=weighth, w=weight, dh=datah,p=npix; p--;)
      if ((pix=*(wh++)) < wthresh && pix>0
		&& (pix2=*(dh++))>-BIG
		&& pix2<satlevel)
        *(w++) = 1/sqrt(pix+(pix2>0.0?
		(gainflag? pix2*pix/backnoise2:pix2)/gain
		:0.0));
      else
        {
        *(w++) = 0.0;
        wbad++;
        }
    }
  else
    for (w=weight, dh=datah, p=npix; p--;)
      if ((pix=*(dh++))>-BIG && pix<satlevel)
        *(w++) = 1.0/sqrt(backnoise2+(pix>0.0?pix/gain:0.0));
      else
        {
        *(w++) = 0.0;
        wbad++;
        }

/* Special action if most of the weights are zero!! */
  if (wbad>=npix-3)
    return;

/* Weight the data */
  dh = datah;
  val = obj->dbkg;	/* Take into account a local background change */
  d = data;
  w = weight;
  for (p=npix; p--;)
    *(d++) = (*(dh++)-val)**(w++);

/* Get the local PSF */
  psf_build(psf);

  npsfflag = 1;
  r2 = psf_fwhm*psf_fwhm/2.0;
  fluxb[0] = deltaxb[0] = deltayb[0] = 0.0;
  for (npsf=1; npsf<=npsfmax && npsfflag; npsf++)
    {
/*-- First compute an optimum initial guess for the positions of components */
    if (npsf>1)
      {
/*---- Subtract previously fitted components */
      d = data2;
      dh = datah;
      for (p=npix; p--;)
        *(d++) = (double)*(dh++);
      for (j=0; j<npsf-1; j++)
        {
        d = data2;
        ps = psfmasks[j];
        for (p=npix; p--;)
          *(d++) -= flux[j]**(ps++);
        }
      convolve_image(field, data2, data3, width,height);
/*---- Ignore regions too close to stellar cores */
      for (j=0; j<npsf-1; j++)
        {
        d = data3;
        dy = -((double)(height/2)+deltay[j]);
        for (y=height; y--; dy += 1.0)
          {
          dx = -((double)(width/2)+deltax[j]);
          for (x=width; x--; dx+= 1.0, d++)
            if (dx*dx+dy*dy<r2)
              *d = -BIG;
          }
	}
/*---- Now find the brightest pixel (poor man's guess, to be refined later) */
      d = data3;
      valmax = -BIG;
      xmax = width/2;
      ymax = height/2;
      for (y=0; y<height; y++)
        for (x=0; x<width; x++)
          {
          if ((val = *(d++))>valmax)
            {
            valmax = val;
            xmax = x;
            ymax = y;
            }
          }
      deltax[npsf-1] = (double)(xmax - width/2);
      deltay[npsf-1] = (double)(ymax - height/2);
      }
    else
      {
/*---- Only one component to fit: simply use the barycenter as a guess */
      deltax[npsf-1] = obj->mx - ix;
      deltay[npsf-1] = obj->my - iy;
      }
    niter = 0;
    convflag = 1;
    for (i=0; i<PSF_NITER && convflag; i++)
      {
      convflag = 0;
      niter++;
      m = mat;
      for (j=0; j<npsf; j++)
        {
/*------ Resample the PSFs here for the 1st iteration */
        vignet_resample(psf->maskloc, psf->masksize[0], psf->masksize[1],
			psfmasks[j], width, height,
			-deltax[j]*pixstep, -deltay[j]*pixstep,
			pixstep);
/*------ 0th order (weighted) */
        ps = psfmasks[j];
        w = weight;
        for (y=0; y<height; y++)
          for (x=0; x<width; x++, ps++, w++)
            *(m++) = y?(y>=(height-1)?0:(x?(x>=(width-1)?0:*ps**w):0)):0;
/*------ 1st order (weighted) moment in x */
        ps = psfmasks[j];
        px = psfmaskx[j];
        w = weight;
        for (y=0; y<height; y++)
          for (x=0; x<width; x++, ps++, w++)
            *(m++) = ((*px++) = (x?(x>=(width-1)?0:*(ps+1)-*(ps-1)):0))**w/2;
/*------ 1st order (weighted) moment in y */
        ps = psfmasks[j];
        py = psfmasky[j];
        w = weight;
        for (y=0; y<height; y++)
          for (x=0; x<width; x++, ps++, w++)
            *(m++) = (*(py++)=(y?(y>=(height-1)?0:*(ps+width)-*(ps-width)):0))
		**w/2;
        }

      svdfit(mat, data, npix, npsf*PSF_NA, sol, vmat, wmat);

      for (j=0; j<npsf; j++)
        {
        flux[j] = sol[j*PSF_NA];
/*------ Update the PSF shifts */
        if (fabs(flux[j])>0.0)
          {
          dx = -sol[j*PSF_NA+1]/((npsf>1?2:1)*flux[j]);
          dy = -sol[j*PSF_NA+2]/((npsf>1?2:1)*flux[j]);
          }

        deltax[j] += dx;
        deltay[j] += dy;
/*------ Continue until all PSFs have come to a complete stop */
        if ((dx*dx+dy*dy) > radmin2)
          convflag = 1;

/*------ Exit if too much decentering or negative flux */
        for (k=j+1; k<npsf; k++)
          {
          dx = deltax[j]-deltax[k];
          dy = deltay[j]-deltay[k];
          if (dx*dx+dy*dy<r2/4.0)
            {
            flux[j] = -BIG;
            break;

            }
          }
        if (flux[j]<0.0
		|| (deltax[j]*deltax[j] + deltay[j]*deltay[j]) > radmax2)
          {
          npsfflag = 0;
          convflag = 0;
          npsf--;
          break;
          }
        }
      }
    if (npsfflag)
      {
/*--- If we reach this point we know the data are worth backuping */
      for (j=0; j<npsf; j++)
        {
        deltaxb[j] = deltax[j];
        deltayb[j] = deltay[j];
        fluxb[j] = flux[j];
        }
      }
    }

  npsf--;
/* Now keep only fitted stars that fall within the current detection area */
  i = 0;
  for (j=0; j<npsf; j++)
    {
    x = (int)(deltaxb[j]+0.4999)+width/2;
    y = (int)(deltayb[j]+0.4999)+height/2;
    if (x<0 || x>=width || y<0 || y>=height)
      continue;
    if (weight[y*width+x] < 1/BIG)
      continue;
    deltax[i] = deltaxb[j];
    deltay[i] = deltayb[j];
    flux[i++] = fluxb[j];
    }
  npsf = i;

/* Compute chi2 if asked to */
/*
  if (FLAG(obj2.chi2_psf))
    {
    chi2 = 0.0;
    val = obj2->flux_psf;
    for (d=data,w=weight,p=0; p<npix; p++)
      {
      pix = *(d++);
      val = *(w++);
      for (j=0; j<npsf; j++)
        pix -=  psfmasks[j][p]*flux[j];
      chi2 += pix*pix;
      obj2->chi2_psf = chi2/(npix - 3*npsf);
      }
    }
*/
/* Error estimates on parameters */
/*
  if (FLAG(obj2.fluxerr_psf))
    {
     double	*var, vara,varb,covab;

-- Compute variances and covariances if asked to 
    svdvar(vmat, wmat, npsf*PSF_NA, covmat);
    var = covmat;
    for (j=0; j<npsf; j++)
      {
---- First, the error on the flux estimate 
      obj2->fluxerr_psf = sqrt(*var);
      if (FLAG(obj2.poserrmx2_psf))
        {
------ Variances and covariance along x and y 
        vara = *(var += PSF_NA+1);
        covab = *(++var);
        varb = *(var += PSF_NA);
        var += PSF_NA+1;
        obj2->poserrmx2_psf = (vara*x2[j]*x2[j]+varb*xy[j]*xy[j]
				+2*covab*x2[j]*xy[j])/(sol[0]*sol[0]);
        obj2->poserrmy2_psf = (varb*y2[j]*y2[j]+vara*xy[j]*xy[j]
				+2*covab*y2[j]*xy[j])/(sol[0]*sol[0]);
        obj2->poserrmxy_psf = (vara*x2[j]*xy[j]+varb*y2[j]*xy[j]
				+covab*(x2[j]*y2[j]+xy[j]*xy[j]))
				/(sol[0]*sol[0]);

------ If requested, translate variances to major and minor error axes... 
        if (FLAG(obj2.poserra_psf))
          {
           double	pmx2,pmy2,temp,theta;

          if (fabs(temp=obj2->poserrmx2_psf-obj2->poserrmy2_psf) > 0.0)
            theta = atan2(2.0 * obj2->poserrmxy_psf,temp) / 2.0;
          else
            theta = PI/4.0;

          temp = sqrt(0.25*temp*temp+obj2->poserrmxy_psf*obj2->poserrmxy_psf);
          pmy2 = pmx2 = 0.5*(obj2->poserrmx2_psf+obj2->poserrmy2_psf);
          pmx2+=temp;
          pmy2-=temp;

          obj2->poserra_psf = (float)sqrt(pmx2);
          obj2->poserrb_psf = (float)sqrt(pmy2);
          obj2->poserrtheta_psf = theta*180.0/PI;
          }

------ ...Or ellipse parameters 
        if (FLAG(obj2.poserr_cxx))
          {
           double	xm2,ym2, xym, temp;

          xm2 = obj2->poserrmx2_psf;
          ym2 = obj2->poserrmy2_psf;
          xym = obj2->poserrmxy_psf;
          obj2->poserrcxx_psf = (float)(ym2/(temp=xm2*ym2-xym*xym));
          obj2->poserrcyy_psf = (float)(xm2/temp);
          obj2->poserrcxy_psf = (float)(-2*xym/temp);
          }
        }
      else
        var += 3*PSF_NA+3;
      }
    }
*/
/* CHECK images */
  if (prefs.check[CHECK_SUBPSFPROTOS] || prefs.check[CHECK_PSFPROTOS])
    for (j=0; j<npsf; j++)
      {
      vignet_resample(psf->maskloc, psf->masksize[0], psf->masksize[1],
		checkdata, pwidth, pheight,
		-deltax[j]*pixstep, -deltay[j]*pixstep, pixstep);
      cpix = checkmask;
      d = checkdata;
      for (p=nppix; p--;)
        *(cpix++) = (PIXTYPE)*(d++);
      if ((check = prefs.check[CHECK_SUBPSFPROTOS]))
        addcheck(check, checkmask, pwidth,pheight, ix,iy,-flux[j]);
      if ((check = prefs.check[CHECK_PSFPROTOS]))
        addcheck(check, checkmask, pwidth,pheight, ix,iy,flux[j]);
      }

  thepsfit->niter = niter;
  thepsfit->npsf = npsf;
  for (j=0; j<npsf; j++)
    {
    thepsfit->x[j] = ix+deltax[j]+1.0;
    thepsfit->y[j] = iy+deltay[j]+1.0;
    thepsfit->flux[j] = flux[j];
    }

/* Now the morphology stuff */
  if (prefs.pc_flag)
    {
    width = pwidth-1;
    height = pheight-1;
    npix = width*height;
    copyimage(field, datah, width, height, ix, iy);

/*-- Re-compute weights */
    if (wfield)
      {
      copyimage(wfield, weighth, width, height, ix, iy);
      for (wh=weighth ,w=weight, p=npix; p--;)
        *(w++) = (pix=*(wh++))<wthresh? sqrt(pix): 0.0;
      }
    else
      for (w=weight, dh=datah, p=npix; p--;)
        *(w++) = ((pix = *(dh++))>-BIG && pix<satlevel)?
		1.0/sqrt(backnoise2+(pix>0.0?pix/gain:0.0))
		:0.0;

/*-- Weight the data */
    dh = datah;
    d = data;
    w = weight;
    for (p=npix; p--;)
      *(d++) = *(dh++)*(*(w++));

    pc_fit(psf, data, weight, width, height, ix,iy, dx,dy, npix,
	field->backsig);

    }

  for (i=0; i<npsfmax; i++)
    {
    QFREE(psfmasks[i]);
    QFREE(psfmaskx[i]);
    QFREE(psfmasky[i]);
    }

  QFREE(psfmasks);
  QFREE(psfmaskx);
  QFREE(psfmasky);
  QFREE(datah);
  QFREE(data);
  QFREE(data2);
  QFREE(data3);
  QFREE(weighth);
  QFREE(weight);
  QFREE(data);
  QFREE(mat);	

  if (prefs.check[CHECK_SUBPSFPROTOS] || prefs.check[CHECK_PSFPROTOS]
	|| prefs.check[CHECK_SUBPCPROTOS] || prefs.check[CHECK_PCPROTOS]
	|| prefs.check[CHECK_PCOPROTOS])
    {
    QFREE(checkdata);
    QFREE(checkmask);
    }

  return;
  }


/******************************* psf_build **********************************/
/*
Build the local PSF (function of "context").
*/
void	psf_build(psfstruct *psf)
  {
   static double	pos[POLY_MAXDIM];
   double	*pl, *basis, fac;
   float	*ppc;
   int		i,n,p, ndim, npix;

  npix = psf->masksize[0]*psf->masksize[1];

/* Reset the Local PSF mask */
  memset(psf->maskloc, 0, npix*sizeof(double));

/* Grab the context vector */
  ndim = psf->poly->ndim;
  for (i=0; i<ndim; i++)
    pos[i] = (*(double *)ttypeconv(psf->context[i],psf->contexttyp[i],T_DOUBLE)
	- psf->contextoffset[i]) / psf->contextscale[i];

  poly_func(psf->poly, pos);

  basis = psf->poly->basis;

  ppc = psf->maskcomp;
/* Sum each component */
  for (n = (psf->maskdim>2?psf->masksize[2]:1); n--;)
    {
    pl = psf->maskloc;
    fac = *(basis++);
    for (p=npix; p--;)
      *(pl++) +=  fac**(ppc++);
    }

  return;
  }


/******************************** svdfit ************************************/
/*
General least-square fit A.x = b, based on Singular Value Decomposition (SVD).
Loosely adapted from Numerical Recipes in C, 2nd Ed. (p. 671).
Note: the a and v matrices are transposed with respect to the N.R. convention.
*/
void svdfit(double *a, double *b, int m, int n, double *sol,
	double *vmat, double *wmat)
  {
#define MAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
        (maxarg1) : (maxarg2))
#define	PYTHAG(a,b)	((at=fabs(a)) > (bt=fabs(b)) ? \
				  (ct=bt/at,at*sqrt(1.0+ct*ct)) \
				: (bt ? (ct=at/bt,bt*sqrt(1.0+ct*ct)): 0.0))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))
#define	TOL		1.0e-11

   int			flag,i,its,j,jj,k,l,nm,mmi,nml;
   double		c,f,h,s,x,y,z,
			anorm, g, scale,
			at,bt,ct,maxarg1,maxarg2,
			thresh, wmax,
			*w,*ap,*ap0,*ap1,*ap10,*rv1p,*vp,*vp0,*vp1,*vp10,
			*bp,*tmpp, *rv1,*tmp;

  anorm = g = scale = 0.0;
  if (m < n)
    error(EXIT_FAILURE, "*Error*: Not enough rows for solving the system ",
	"in svdfit()");
  
  QMALLOC(rv1, double, n);
  QMALLOC(tmp, double, n);
  l = nm = nml = 0;			/* To avoid gcc -Wall warnings */
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
          error(EXIT_FAILURE, "*Error*: No convergence in 100 SVD iterations ",
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

/******************************** svdvar ************************************/
/*
Computation of the covariance matrix from the SVD vmat and wmat matrices.A
dapted from Numerical Recipes in C, 2nd Ed. (p. 679).
*/
void svdvar(double *v, double *w, int n, double *cov)
  {
   static double	wti[PSF_NTOT];
   double		sum;
   int			i,j,k;

  for (i=0; i<n; i++)
    wti[i] = w[i]? 1.0/(w[i]*w[i]) : 0.0;

  for (i=0; i<n; i++)
    for (j=0; j<=i; j++)
      {
      for (sum=0.0,k=0; k<n; k++)
        sum += v[k*n+i]*v[k*n+j]*wti[k];
      cov[j*n+i] = cov[i*n+j] = sum;
      }

  return;
  }

