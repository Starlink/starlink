  /*
 				pc.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Stuff related to Principal Component Analysis (PCA).
*
*	Last modify:	18/12/98
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
#include	"check.h"
#include	"image.h"
#include	"poly.h"
#include	"psf.h"

/****** pc_end ***************************************************************
PROTO   void pc_end(pcstruct *pc)
PURPOSE Free a PC structure and everything it contains.
INPUT   pcstruct pointer.
OUTPUT  -.
NOTES   -.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 28/08/98
 ***/
void	pc_end(pcstruct *pc)
  {

  free(pc->maskcomp);
  free(pc->omaskcomp);
  free(pc->maskcurr);
  free(pc->masksize);
  free(pc->mx2);
  free(pc->my2);
  free(pc->mxy);
  free(pc->flux);
  free(pc->bt);
  free(pc);

  return;
  }


/********************************** pc_load **********************************/
/*
Load the PC data from a FITS file.
*/
pcstruct	*pc_load(catstruct *cat)
  {
   pcstruct	*pc;
   tabstruct	*tab;
   keystruct	*key;
   char		*head, str[80], *ci, *filename;
   int		i;

  if (!(tab = name_to_tab(cat, "PC_DATA", 0)))
    return NULL;

  filename = cat->filename;

/* OK, we now allocate memory for the PC structure itself */
  QCALLOC(pc, pcstruct, 1);

/* Store a short copy of the PC filename */
  if (ci=strrchr(filename, '/'))
    strcpy(pc->name, ci+1);
  else
    strcpy(pc->name, filename);

/* Load important scalars (which are stored as FITS keywords) */
  head = tab->headbuf;

/* Dimensionality of the PC mask */
  if (fitsread(head, "PCNAXIS", &pc->maskdim, H_INT, T_LONG) != RETURN_OK)
    return NULL;
  if (pc->maskdim<2 || pc->maskdim>4)
    error(EXIT_FAILURE, "*Error*: wrong dimensionality for the PC "
	"mask in ", filename);
  QMALLOC(pc->masksize, int, pc->maskdim);
  for (i=0; i<pc->maskdim; i++)
    pc->masksize[i] = 1;
  pc->masknpix = 1;
  for (i=0; i<pc->maskdim; i++)
    {
    sprintf(str, "PCAXIS%1d ", i+1);
    if (fitsread(head, str, &pc->masksize[i], H_INT,T_LONG) != RETURN_OK)
      goto headerror;
    pc->masknpix *= pc->masksize[i];
   }

  pc->npc = pc->masksize[pc->maskdim-1];

/* Load the PC mask data */
  key = read_key(tab, "PC_CONVMASK");
  pc->maskcomp = key->ptr;

  key = read_key(tab, "PC_MASK");
  pc->omaskcomp = key->ptr;
  pc->omaskdim = key->naxis;
  pc->omasknpix = 1;
  QMALLOC(pc->omasksize, int, pc->omaskdim);
  for (i=0; i<pc->omaskdim; i++)
    pc->omasknpix *= (pc->omasksize[i] = key->naxisn[i]);

  key = read_key(tab, "PC_MX2");
  pc->mx2 = key->ptr;

  key = read_key(tab, "PC_MY2");
  pc->my2 = key->ptr;

  key = read_key(tab, "PC_MXY");
  pc->mxy = key->ptr;

  key = read_key(tab, "PC_FLUX");
  pc->flux = key->ptr;

  key = read_key(tab, "PC_BRATIO");
  pc->bt = key->ptr;

  QMALLOC(pc->maskcurr, double, pc->masksize[0]*pc->masksize[1]*pc->npc);

/* But don't touch my arrays!! */
  blank_keys(tab);

  return pc;

headerror:
  error(EXIT_FAILURE, "*Error*: Incorrect or obsolete PC data in ", filename);
  }


/********************************** pc_fit **********************************/
/*
Fit the PC data to the current data.
*/
void	pc_fit(psfstruct *psf, double *data, double *weight,
		int width, int height,int ix, int iy,
		double dx, double dy, int npc)
  {
   pcstruct	*pc;
   checkstruct	*check;
   double	*basis,*basis0, *cpix,*cpix0, *pcshift,*wpcshift,
		*spix,*wspix, *w,
		*vmat, *wmat, *sol,*solt, *cxm2,*cym2,*cxym,*cflux,
		val, xm2,ym2,xym,flux, temp,temp2, theta, pmx2,pmy2;
   float	*ppix, *ospix,
		pixstep;
   int		c,n,p, npix,npix2, ncoeff;

  pc = psf->pc;
/* Build the "local PCs", using the basis func. coeffs computed in psf_fit() */
  if (npc > pc->npc)
    npc = pc->npc;
  npix = pc->masksize[0]*pc->masksize[1];
  npix2 = width*height;
  ncoeff = psf->poly->ncoeff;
  pixstep = 1.0/psf->pixstep;
  dx *= pixstep;
  dy *= pixstep;

  memset(pc->maskcurr, 0, npix*npc*sizeof(double));
  basis0 = psf->poly->basis;
  cpix0 = pc->maskcurr;
  ppix = pc->maskcomp;

/* Sum each component */
  for (c=npc; c--; cpix0 += npix)
    {
    basis = basis0;
    for (n = ncoeff; n--;)
      {
      cpix = cpix0;
      val = *(basis++);
      for (p=npix; p--;)
        *(cpix++) += val*(double)*(ppix++);
      }
    }

/* Allocate memory for temporary buffers */
  QMALLOC(pcshift, double, npix2*npc);
  QMALLOC(wpcshift, double, npix2*npc);
  QMALLOC(vmat, double, npc*npc);
  QMALLOC(wmat, double, npc);
  QMALLOC(sol, double, npc);

/* Now shift and scale to the right position, and weight the PCs */
  cpix = pc->maskcurr;
  spix = pcshift;
  wspix = wpcshift;
  for (c=npc; c--; cpix += npix)
    {
    vignet_resample(cpix, pc->masksize[0], pc->masksize[1],
		spix, width, height, -dx, -dy, pixstep);
    w = weight;
    for (p=npix2; p--;)
      *(wspix++) = *(spix++)**(w++);
    }

  svdfit(wpcshift, data, npix2, npc, sol, vmat, wmat);

  xm2 = ym2 = xym = flux = 0.0;
  cxm2 = pc->mx2;
  cym2 = pc->my2;
  cxym = pc->mxy;
  cflux = pc->flux;
  solt = sol;
  for (c=npc; c--;)
    {
    val = *(solt++);
    xm2 += val**(cxm2++);
    ym2 += val**(cym2++);
    xym += val**(cxym++);
    flux += val**(cflux++);
    }

  if (flux==0.0)
    xm2 = ym2 = xym = 0.0;
  else
    {
    xm2 /= flux;
    ym2 /= flux;
    xym /= flux;
    }

  obj2->mx2_pc = xm2;
  obj2->my2_pc = ym2;
  obj2->mxy_pc = xym;

  if (FLAG(obj2.a_pc))
    {
/* Handle fully correlated x/y (which cause a singularity...) */
    if ((temp2=xm2*ym2-xym*xym)<0.00694)
      {
      xm2 += 0.0833333;
      ym2 += 0.0833333;
      temp2 = xm2*ym2-xym*xym;
      }

    if ((fabs(temp=xm2-ym2)) > 0.0)
      theta = atan2(2.0 * xym,temp) / 2.0;
    else
      theta = PI/4.0;

    temp = sqrt(0.25*temp*temp+xym*xym);
    pmy2 = pmx2 = 0.5*(xm2+ym2);
    pmx2 += temp;
    pmy2 -= temp;

    obj2->a_pc = (float)sqrt(pmx2);
    obj2->b_pc = (float)sqrt(pmy2);
    obj2->theta_pc = (float)(theta*180.0/PI);
    }

/* CHECK-Images */
  if (prefs.check[CHECK_SUBPCPROTOS] || prefs.check[CHECK_PCPROTOS]
	|| prefs.check[CHECK_PCOPROTOS])
    {
    spix = pcshift;
    ospix = pc->omaskcomp;
    solt = sol;
    for (c=npc; c--; solt++)
      {
      ppix = checkmask;
      for (p=npix2; p--;)
        *(ppix++) = (PIXTYPE)*(spix++);
      if (check = prefs.check[CHECK_SUBPCPROTOS])
        addcheck(check, checkmask, width,height, ix,iy, -*solt);
      if (check = prefs.check[CHECK_PCPROTOS])
/*
        addcheck(check, checkmask, width,height, ix,iy, *solt);
*/
	{
        addcheck(check, ospix, pc->omasksize[0],pc->omasksize[1],ix,iy, *solt);
        ospix += pc->omasksize[0]*pc->omasksize[1];
	}
      if (check = prefs.check[CHECK_PCOPROTOS])
        {
        addcheck(check, checkmask, width,height, ix,iy, -*solt);
        addcheck(check, ospix, pc->omasksize[0],pc->omasksize[1],ix,iy, *solt);
        ospix += pc->omasksize[0]*pc->omasksize[1];
        }
      }
    }

/* Free memory */
  free(pcshift);
  free(wpcshift);
  free(vmat);
  free(wmat);
  free(sol);

  return;
  }

