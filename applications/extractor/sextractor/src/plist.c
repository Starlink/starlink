 /*
 				plist.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, (IAP, Leiden observatory & ESO)
*
*	Contents:	functions dealing with the handling of pixel lists.
*
*	Last modify:	28/05/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<stdio.h>
#include	<stdlib.h>

#include	"define.h"
#include	"globals.h"
#include	"plist.h"


/******************************** createblank *******************************
PROTO   int createblank(int no, objliststruct *objlist)
PURPOSE Create pixel map for BLANKing.
INPUT   objlist number,
        objlist pointer,
OUTPUT  RETURN_OK if success, RETURN_FATAL_ERROR otherwise (memory overflow).
NOTES   -.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 21/10/97
 ***/
int	createblank(objliststruct *objlist, int no)

  {
   objstruct	*obj;
   pliststruct	*pixel, *pixt;
   int		i, n, pos, xmin,ymin, w, dflag;
   PIXTYPE	*pix, *dpix, *pt;

  obj = objlist->obj+no;
  pixel = objlist->plist;
  dflag = prefs.dimage_flag;

  obj->subx = xmin = obj->xmin;
  obj->suby = ymin = obj->ymin;
  obj->subw = w = obj->xmax - xmin + 1;
  obj->subh = obj->ymax - ymin + 1;

  n = w*obj->subh;
  if (!(obj->blank = pix = (PIXTYPE *)malloc(n*sizeof(PIXTYPE))))
    return RETURN_FATAL_ERROR;
  pt = pix;
  for (i=n; i--;)
    *(pt++) = -BIG;

  if (dflag)
    {
    if (!(obj->dblank = dpix = (PIXTYPE *)malloc(n*sizeof(PIXTYPE))))
      return RETURN_FATAL_ERROR;
    pt = dpix;
    for (i=n; i--;)
      *(pt++) = -BIG;
    }
  else
    obj->dblank = NULL;

  for (i=obj->firstpix; i!=-1; i=PLIST(pixt,nextpix))
    {
    pixt = pixel+i;
    pos = (PLIST(pixt,x)-xmin) + (PLIST(pixt,y)-ymin)*w;
    *(pix+pos) = PLIST(pixt, value);
    if (dflag)
      *(dpix+pos) = PLISTPIX(pixt, dvalue);
    }

  return RETURN_OK;
  }


/******************************** createsubmap *******************************
PROTO   int createpixmap(int no, objliststruct *objlist)
PURPOSE Create pixel-index submap for deblending.
INPUT   objlist number,
        objlist pointer,
OUTPUT  RETURN_OK if success, RETURN_FATAL_ERROR otherwise (memory overflow).
NOTES   -.
AUTHOR  E. Bertin (IAP & Leiden & ESO)
VERSION 08/10/97
 ***/
int	createsubmap(objliststruct *objlist, int no)

  {
   objstruct	*obj;
   pliststruct	*pixel, *pixt;
   int		i, n, xmin,ymin, w, *pix, *pt;

  obj = objlist->obj+no;
  pixel = objlist->plist;

  obj->subx = xmin = obj->xmin;
  obj->suby = ymin = obj->ymin;
  obj->subw = w = obj->xmax - xmin + 1;
  obj->subh = obj->ymax - ymin + 1;
  n = w*obj->subh;
  if (!(obj->submap = pix = (int *)malloc(n*sizeof(int))))
    return RETURN_FATAL_ERROR;
  pt = pix;
  for (i=n; i--;)
    *(pt++) = -1;

  for (i=obj->firstpix; i!=-1; i=PLIST(pixt,nextpix))
    {
    pixt = pixel+i;
    *(pix+(PLIST(pixt,x)-xmin) + (PLIST(pixt,y)-ymin)*w) = i;
    }

  return RETURN_OK;
  }


/****************************** init_plist ************************************
PROTO	pliststruct *init_plist(void)
PURPOSE	initialize a pixel-list and its components.
INPUT	-.
OUTPUT  -.
NOTES   The preparation of components relies on the preferences.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 29/04/98
 ***/
void	init_plist(void)

  {
   pbliststruct	*pbdum = NULL;
   int		i;

  plistsize = sizeof(pbliststruct);
  plistoff_value = (char *)&pbdum->value - (char *)pbdum;

  if (prefs.dimage_flag)
    {
    plistexist_dvalue = 1;
    plistoff_dvalue = plistsize;
    plistsize += sizeof(PIXTYPE);
    }
  else
    {
    plistexist_dvalue = 0;
    plistoff_dvalue = plistoff_value;
    }

  if (prefs.filter_flag)
    {
    plistexist_cdvalue = 1;
    plistoff_cdvalue = plistsize;
    plistsize += sizeof(PIXTYPE);
    }
  else
    {
    plistexist_cdvalue = 0;
    plistoff_cdvalue = plistoff_dvalue;
    }

  if (VECFLAG(obj.imaflag))
    {
    plistexist_flag = 1;
    for (i=0; i<prefs.nimaisoflag; i++)
      {
      plistoff_flag[i] = plistsize;
      plistsize += sizeof(FLAGTYPE);
      }
    }
  else
    plistexist_flag = 0;

  if (prefs.weight_flag)
    {
    plistexist_var = 1;
    plistoff_var = plistsize;
    plistsize += sizeof(PIXTYPE);
    if (prefs.dweight_flag)
      {
      plistexist_dthresh = 1;
      plistoff_dthresh = plistsize;
      plistsize += sizeof(PIXTYPE);
      }
    else
      plistexist_dthresh = 0;
    }
  else
    plistexist_var = 0;

  return;
  }

