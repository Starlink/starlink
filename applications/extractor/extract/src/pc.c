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
*	Last modify:	06/09/99
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
VERSION 15/07/99
 ***/
void	pc_end(pcstruct *pc)
  {
   int	i;

  free(pc->maskcomp);
  free(pc->omaskcomp);
  free(pc->omasksize);
  free(pc->maskcurr);
  free(pc->masksize);
  free(pc->mx2);
  free(pc->my2);
  free(pc->mxy);
  free(pc->flux);
  free(pc->bt);
  if (pc->code)
    {
    free(pc->code->pc);
    for (i=0; i<pc->code->nparam;i++)
      free(pc->code->param[i]);
    free(pc->code->param);
    free(pc->code);
    }
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
   codestruct	*code;
   char		*head, str[80], *ci, *filename;
   int		i, ncode,nparam;

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

  ncode = 0;
  fitsread(head, "NCODE", &ncode, H_INT, T_LONG);
  fitsread(head, "NCODEPAR", &nparam, H_INT, T_LONG);

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

  if (ncode)
    {
    QMALLOC(pc->code, codestruct, 1);
    code = pc->code;
    QMALLOC(code->param, float *, nparam);
    QMALLOC(code->parammod, int, nparam);
    code->ncode = ncode;
    code->nparam = nparam;
    key = read_key(tab, "CODE_PC");
    code->pc = (float *)key->ptr;
    for (i=0; i<nparam; i++)
      {
      sprintf(str, "CODE_P%d", i+1);
      key = read_key(tab, str);
      code->param[i] = (float *)key->ptr;
      sprintf(str, "CODE_M%d", i+1);
      fitsread(head, str, &code->parammod[i], H_INT, T_LONG);
      }
    }

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
		double dx, double dy, int npc, float backrms)
  {
   pcstruct	*pc;
   checkstruct	*check;
   codestruct	*code;
   double	*basis,*basis0, *cpix,*cpix0, *pcshift,*wpcshift,
		*spix,*wspix, *w, *sumopc,*sumopct, *sumwspix, *checkbuf,
		*sol,*solt, *cxm2,*cym2,*cxym,*cflux, *datat,
		*mx2t, *my2t, *mxyt,
		val,val2, xm2,ym2,xym,flux, temp,temp2, theta, pmx2,pmy2,
		wnorm, ellip, norm, snorm;
   float	**param, *ppix, *ospix, *cpc,*cpc2, *fparam,
		pixstep, fval, fvalmax, fscale, dparam;
   int		*parammod,
		i,c,n,p, npix,npix2,nopix, ncoeff, nparam, nmax,nmax2, ncode;

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

/* Sum each (PSF-dependent) component */
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

/* Compute the weight normalization */
  wnorm = 0.0;
  w = weight;
  for (p=npix2; p--;)
    {
    val = *(w++);
    wnorm += val*val;
    }

/* Scalar product of data and (approximately orthogonal) basis functions */
  wspix = wpcshift;
  solt = sol;
  snorm = 0.0;
  for (c=npc; c--;)
    {
    datat = data;
    val = 0.0;
    for (p=npix2; p--;)    
      val += *(datat++)**(wspix++);
    val2 = *(solt++) = val*npix2/wnorm;
    snorm += val2*val2;
    }


/* Normalize solution vector */
  snorm = sqrt(snorm);
  solt = sol;
  for (c=npc; c--;)
    *(solt++) /= snorm;

  if (code = pc->code)
    {
    ncode = code->ncode;
/*-- Codebook search */
    cpc = code->pc;
    fvalmax = -BIG;
    nmax = 0;
    for (n=ncode; n--;)
      {
      fval = 0.0;
      solt = sol;
      for (p=npc; p--;)
        fval += *(solt++)**(cpc++);
      if (fval>fvalmax)
        {
        fvalmax = fval;
        nmax = n;
        }
      }
    nmax = ncode - 1 - nmax;

/*-- Interpolation */
    param = code->param;
    parammod = code->parammod;
    nparam = code->nparam;
    QMALLOC(fparam, float, nparam);
    for (p=0; p<nparam; p++)
      {
      dparam = 0.0;
      if (parammod[p])
        {
        val2 = 0.0;
        if ((nmax2 = nmax+parammod[p]) < ncode)
          {
          cpc = code->pc+npc*nmax;
          cpc2 = code->pc+npc*nmax2;
          solt = sol;
          norm = 0.0;
          for (c=npc; c--;)
            {
            val = *(cpc2++)-*cpc;
            val2 += val*(*(solt++) - *(cpc++));
            norm += val*val;
            }
          if (norm>0.0)
            dparam = val2/norm*(param[p][nmax2]-param[p][nmax]);
          else
            val2 = 0.0;
          }
/*------ If dot product negative of something went wrong, try other side */
        if (val2<=0.0 && (nmax2 = nmax-parammod[p]) >= 0)
          {
          cpc = code->pc+npc*nmax;
          cpc2 = code->pc+npc*nmax2;
          solt = sol;
          norm = val2 = 0.0;
          for (c=npc; c--;)
            {
            val = *(cpc2++)-*cpc;
            val2 += val*(*(solt++) - *(cpc++));
            norm += val*val;
            }          
          if (norm>0.0)
            dparam = val2/norm*(param[p][nmax2]-param[p][nmax]);
          }
        fparam[p] = param[p][nmax] + dparam;
        }
      }

    solt = sol;
    cpc = code->pc+npc*nmax;
    fscale = fvalmax*code->param[0][nmax]*snorm;
    for (p=npc; p--;)
      *(solt++) = fscale**(cpc++);

/*-- Copy the derived physical quantities to output parameters */
/*-- (subject to changes) */
    obj2->flux_galfit = fscale;
    obj2->gdposang = fparam[1];
    if (obj2->gdposang>90.0)
      obj2->gdposang -= 180.0;
    else if (obj2->gdposang<-90.0)
      obj2->gdposang += 180.0;
    obj2->gdscale = fparam[2];
    obj2->gdaspect = fparam[3];
    ellip = (1.0 - obj2->gdaspect)/(1.0 + obj2->gdaspect);
    obj2->gde1 = (float)(ellip*cos(2*obj2->gdposang*PI/180.0));
    obj2->gde2 = (float)(ellip*sin(2*obj2->gdposang*PI/180.0));
/*---- Copy the best-fitting PCs to the VECTOR_PC output vector */
    if (FLAG(obj2.vector_pc))
      {
      solt = sol;
      ppix = obj2->vector_pc;
      for (c=prefs.pc_vectorsize>npc?npc:prefs.pc_vectorsize; c--;)
        *(ppix++) = *(solt++);
      }

    free(fparam);
    }

  xm2 = ym2 = xym = flux = 0.0;
  solt = sol;
  mx2t = pc->mx2;
  my2t = pc->my2;
  mxyt = pc->mxy;
  for (c=npc; c--;)
    {
    val = *(solt++);
    xm2 += val**(mx2t++);
    ym2 += val**(my2t++);
    xym += val**(mxyt++);
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
  if (prefs.check[CHECK_SUBPCPROTOS] || prefs.check[CHECK_PCPROTOS])
    {
    spix = pcshift;
    solt = sol;
    for (c=npc; c--; solt++)
      {
      ppix = checkmask;
      for (p=npix2; p--;)
        *(ppix++) = (PIXTYPE)*(spix++);
      if (check = prefs.check[CHECK_SUBPCPROTOS])
        addcheck(check, checkmask, width,height, ix,iy, -*solt);
      if (check = prefs.check[CHECK_PCPROTOS])
        addcheck(check, checkmask, width,height, ix,iy, *solt);
      }
    }
  if (check = prefs.check[CHECK_PCOPROTOS])
    {
/*- Reconstruct the unconvolved profile */
    nopix = pc->omasksize[0]*pc->omasksize[1];
    QCALLOC(sumopc, double, nopix);
    solt = sol;
    ospix = pc->omaskcomp;
    for (c=npc; c--;)
      {
      val = *(solt++);
      sumopct = sumopc;
      for (p=nopix; p--;)
        *(sumopct++) += val*(double)*(ospix++);
      }
    QMALLOC(checkbuf, double, npix2);
    vignet_resample(sumopc, pc->omasksize[0], pc->omasksize[1],
		checkbuf, width, height, -dx, -dy, pixstep);
    ppix = checkmask;
    spix = checkbuf;
    for (p=npix2; p--;)
      *(ppix++) = (PIXTYPE)*(spix++);
    addcheck(check, checkmask, width,height, ix,iy, 1.0);
    free(checkbuf);
    free(sumopc);
    }

/* Free memory */
  free(pcshift);
  free(wpcshift);
  free(sol);

  return;
  }

