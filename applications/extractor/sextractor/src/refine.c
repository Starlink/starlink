 /*
 				refine.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP & Leiden observatory
*
*	Contents:	functions to refine extraction of objects.
*
*	Last modify:	15/03/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdlib.h>

#include	"define.h"
#include	"globals.h"
#include	"plist.h"
#include	"extract.h"

#ifndef	RAND_MAX
#define	RAND_MAX	2147483647
#endif
#define	NSONMAX			1024	/* max. number per level */
#define	NBRANCH			16	/* starting number per branch */

static objliststruct	*objlist = NULL;
static short		*son = NULL, *ok = NULL;

/******************************** parcelout **********************************
PROTO   parcelout(objliststruct *objlistin, objliststruct *objlistout)
PURPOSE Divide a list of isophotal detections in several parts (deblending).
INPUT   input objlist,
        output objlist,
OUTPUT  RETURN_OK if success, RETURN_FATAL_ERROR otherwise (memory overflow).
NOTES   Even if the object is not deblended, the output objlist threshold is
        recomputed if a variable threshold is used.
AUTHOR  E. Bertin (IAP, Leiden & ESO)
VERSION 15/03/98
 ***/
int	parcelout(objliststruct *objlistin, objliststruct *objlistout)

  {
   objstruct		*obj;
   static objliststruct	debobjlist, debobjlist2;
   double		dthresh, dthresh0, value0;
   int			h,i,j,k,l,m,
			xn,
			nbm = NBRANCH,
			out;


  out = RETURN_OK;

  xn = prefs.deblend_nthresh;

/*----- allocate some memory */

  if (!son)
    if (!(son = (short *)malloc(xn*NSONMAX*nbm*sizeof(short))))
      error(EXIT_FAILURE, "Not enough memory in ", "parcelout()");
  if (!ok)
    if (!(ok = (short *)malloc(xn*NSONMAX*sizeof(short))))
      error(EXIT_FAILURE, "Not enough memory in ", "parcelout()");

  if (!objlist)
    if (!(objlist = (objliststruct *)malloc(xn*sizeof(objliststruct))))
      error(EXIT_FAILURE, "Not enough memory in ", "parcelout()");


/* ---- initialize lists of objects */

  debobjlist.obj = debobjlist2.obj =  NULL;
  debobjlist.plist = debobjlist2.plist = NULL;
  debobjlist.nobj = debobjlist2.nobj = 0;
  debobjlist.npix = debobjlist2.npix = 0;
  objlistout->thresh = debobjlist2.thresh = objlistin->thresh;
  memset(objlist, 0, xn*sizeof(objliststruct));

  for (l=0; l<objlistin->nobj && out==RETURN_OK; l++)
      {
      dthresh0 = objlistin->obj[l].dthresh;

      objlistout->dthresh = debobjlist2.dthresh = dthresh0;
      if ((out = addobj(l, objlistin, &objlist[0])) == RETURN_FATAL_ERROR)
        goto exit_parcelout;
      if ((out = addobj(l, objlistin, &debobjlist2)) == RETURN_FATAL_ERROR)
        goto exit_parcelout;
      value0 = objlist[0].obj[0].fdflux*prefs.deblend_mincont;
      ok[0] = (short)1;
      for (k=1; k<xn; k++)
        {
/*------ Calculate threshold */
        dthresh = objlistin->obj[l].fdpeak;
        if (dthresh>0.0)
          {
          if (prefs.detect_type == PHOTO)
            debobjlist.dthresh= dthresh0 + (dthresh-dthresh0) * (double)k/xn;
          else
            debobjlist.dthresh = dthresh0 * pow(dthresh/dthresh0,(double)k/xn);
          }
        else
          debobjlist.dthresh = dthresh0;

/*--------- Build tree (bottom->up) */
        if (objlist[k-1].nobj>=NSONMAX)
          {
          out = RETURN_FATAL_ERROR;
          goto exit_parcelout;
          }

        for (i=0; i<objlist[k-1].nobj; i++)
          {
          if (out=lutz(objlistin,l,&objlist[k-1].obj[i], &debobjlist)
			==RETURN_FATAL_ERROR)
            goto exit_parcelout;

          for (j=h=0; j<debobjlist.nobj; j++)
            if (belong(j, &debobjlist, i, &objlist[k-1]))
              {
              debobjlist.obj[j].dthresh = debobjlist.dthresh;
              m = addobj(j, &debobjlist, &objlist[k]);
              if (m==RETURN_FATAL_ERROR || m>=NSONMAX)
                {
                out = RETURN_FATAL_ERROR;
                goto exit_parcelout;
                }
              if (h>=nbm-1)
                if (!(son = (short *)realloc(son,
			xn*NSONMAX*(nbm+=16)*sizeof(short))))
                  {
                  out = RETURN_FATAL_ERROR;
                  goto exit_parcelout;
                  }
              son[k-1+xn*(i+NSONMAX*(h++))] = (short)m;
              ok[k+xn*m] = (short)1;
              }
          son[k-1+xn*(i+NSONMAX*h)] = (short)-1;
          }
        }

/*------- cut the right branches (top->down) */

      for (k = xn-2; k>=0; k--)
        {
        obj = objlist[k+1].obj;
        for (i=0; i<objlist[k].nobj; i++)
          {
          for (m=h=0; (j=(int)son[k+xn*(i+NSONMAX*h)])!=-1; h++)
            {
            if (obj[j].fdflux - obj[j].dthresh*obj[j].fdnpix > value0)
              m++;
            ok[k+xn*i] &= ok[k+1+xn*j];
            }
          if (m>1)	
            {
            for (h=0; (j=(int)son[k+xn*(i+NSONMAX*h)])!=-1; h++)
              if (ok[k+1+xn*j] && obj[j].fdflux-obj[j].dthresh*obj[j].fdnpix
			> value0)
                {
                objlist[k+1].obj[j].flag |= OBJ_MERGED	/* Merge flag on */
			| ((OBJ_ISO_PB|OBJ_APERT_PB|OBJ_OVERFLOW)
			&debobjlist2.obj[0].flag);
                if ((out = addobj(j, &objlist[k+1], &debobjlist2))
			== RETURN_FATAL_ERROR)
                  goto exit_parcelout;
                }
            ok[k+xn*i] = (short)0;
            }
          }
        }

      if (ok[0])
        out = addobj(0, &debobjlist2, objlistout);
      else
        out = gatherup(&debobjlist2, objlistout);

exit_parcelout:

      free(debobjlist2.obj);
      free(debobjlist2.plist);

      for (k=0; k<xn; k++)
        {
        free(objlist[k].obj);
        free(objlist[k].plist);
        }
      }

  free(debobjlist.obj);
  free(debobjlist.plist);

  return out;
  }

/******************************* freeparcelout *******************************/
/*
free the memory allocated by global pointers in refine.c
*/
void	freeparcelout(void)
  {
  free(son);
  free(ok);
  free(objlist);
  return;
  }

/********************************* gatherup **********************************/
/*
Collect faint remaining pixels and allocate them to their most probable
progenitor.
*/
int	gatherup(objliststruct *objlistin, objliststruct *objlistout)

  {
   char		*bmp;
   float	*amp, *p, dx,dy, drand, dist, distmin;
   objstruct	*objin = objlistin->obj, *objout, *objt;

   pliststruct	*pixelin = objlistin->plist, *pixelout, *pixt,*pixt2;

   int		i,j,k,l, *n, iclst, npix, bmwidth,
		nobj = objlistin->nobj, xs,ys, x,y, out;

  out = RETURN_OK;

  objlistout->dthresh = objlistin->dthresh;
  objlistout->thresh = objlistin->thresh;

  QMALLOC(amp, float, nobj);
  QMALLOC(p, float, nobj);
  QMALLOC(n, int, nobj);

  for (i=1; i<nobj; i++)
    preanalyse(i, objlistin, ANALYSE_FULL);

  p[0] = 0.0;
  bmwidth = objin->xmax - (xs=objin->xmin) + 1;
  npix = bmwidth * (objin->ymax - (ys=objin->ymin) + 1);
  if (!(bmp = (char *)calloc(1, npix*sizeof(char))))
    {
    bmp = 0;
    out = RETURN_FATAL_ERROR;
    goto exit_gatherup;
    }

  for (objt = objin+(i=1); i<nobj; i++, objt++)
    {
/*-- Now we have passed the deblending section, reset thresholds */
    objt->dthresh = objlistin->dthresh;
    objt->thresh = objlistin->thresh;

/* ------------	flag pixels which are already allocated */
    for (pixt=pixelin+objin[i].firstpix; pixt>=pixelin;
	pixt=pixelin+PLIST(pixt,nextpix))
      bmp[(PLIST(pixt,x)-xs) + (PLIST(pixt,y)-ys)*bmwidth] = '\1';

    if ((n[i] = addobj(i, objlistin, objlistout)) == RETURN_FATAL_ERROR)
      {
      out = RETURN_FATAL_ERROR;
      goto exit_gatherup;
      }
    dist = objt->fdnpix/(2*PI*objt->abcor*objt->a*objt->b);
    amp[i] = dist<70.0? objt->thresh*exp(dist) : 4.0*objt->fdpeak;

/* ------------ limitate expansion ! */
    if (amp[i]>4.0*objt->fdpeak)
      amp[i] = 4.0*objt->fdpeak;
    }

  objout = objlistout->obj;		/* DO NOT MOVE !!! */

  if (!(pixelout=(pliststruct *)realloc(objlistout->plist,
	(objlistout->npix + npix)*plistsize)))
    {
    out = RETURN_FATAL_ERROR;
    goto exit_gatherup;
    }

  objlistout->plist = pixelout;
  k = objlistout->npix;
  for (pixt=pixelin+objin->firstpix; pixt>=pixelin;
	pixt=pixelin+PLIST(pixt,nextpix))
    {
    x = PLIST(pixt,x);
    y = PLIST(pixt,y);
    if (!bmp[(x-xs) + (y-ys)*bmwidth])
      {
      pixt2 = pixelout + (l=(k++*plistsize));
      memcpy(pixt2, pixt, plistsize);
      PLIST(pixt2, nextpix) = -1;
      distmin = 1e+31;
      for (objt = objin+(i=1); i<nobj; i++, objt++)
        {
        dx = x - objt->mx;
        dy = y - objt->my;
        dist=0.5*(objt->cxx*dx*dx+objt->cyy*dy*dy+objt->cxy*dx*dy)/objt->abcor;
        p[i] = p[i-1] + (dist<70.0?amp[i]*exp(-dist) : 0.0);
        if (dist<distmin)
          {
          distmin = dist;
          iclst = i;
          }
        }			
      if (p[nobj-1] > 1.0e-31)
        {
        drand = p[nobj-1]*rand()/RAND_MAX;
        for (i=1; p[i]<drand; i++);
	}
      else
        i = iclst;
      objout[n[i]].lastpix=PLIST(pixelout+objout[n[i]].lastpix,nextpix)=l;
      }
    }

  objlistout->npix = k;
  if (!(objlistout->plist = (pliststruct *)realloc(pixelout,
	objlistout->npix*plistsize)))
    error (-1, "Not enough memory to update pixel list in ", "gatherup()");

exit_gatherup:

  free(bmp);
  free(amp);
  free(p);
  free(n);

  return out;
  }

