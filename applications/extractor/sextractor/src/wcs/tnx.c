/*
 				tnx.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	WCSlib
*
*	Author:		E.BERTIN (IAP), based on D.Mink (SAO) WCSTools
*
*	Contents:       Handle TNX astrometric format (from IRAF).
*
*
*	Last modify:	11/04/2000
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"tnx.h"

/******* read_tnxaxis *********************************************************
PROTO	tnxaxisstruct *read_tnxaxis(char *tnxstr)
PURPOSE	Read a TNX axis mapping structure.
INPUT	String containing the TNX info.
OUTPUT	TNXAXIS structure if OK, or NULL in case of error.
NOTES	-.
AUTHOR	E. Bertin (IAP)
VERSION	28/03/2000
 ***/

tnxaxisstruct	*read_tnxaxis(char *tnxstr)

  {
   tnxaxisstruct	*tnxaxis;
   char		*pstr;
   double	min, max;
   int		i, order;

  if (pstr=strpbrk(tnxstr, "1234567890-+."))
    {
    if (!(tnxaxis=malloc(sizeof(tnxaxisstruct))))
      return NULL;
    tnxaxis->type = (int)(atof(strtok(pstr, " "))+0.5);
    tnxaxis->xorder = (pstr=strtok(NULL, " "))? (int)(atof(pstr)+0.5) : 0;
    tnxaxis->yorder = (pstr=strtok(NULL, " "))? (int)(atof(pstr)+0.5) : 0;
    tnxaxis->xterms = (pstr=strtok(NULL, " "))? (int)(atof(pstr)+0.5) : 0;
    min = (pstr=strtok(NULL, " "))? atof(pstr) : 0.0;
    max = (pstr=strtok(NULL, " "))? atof(pstr) : 0.0;
    if (max <= min)
      return NULL;
    tnxaxis->xrange = 2.0 / (max - min);
    tnxaxis->xmaxmin =  - (max + min) / 2.0;
    min = (pstr=strtok(NULL, " "))? atof(pstr) : 0.0;
    max = (pstr=strtok(NULL, " "))? atof(pstr) : 0.0;
    if (max <= min)
      return NULL;
    tnxaxis->yrange = 2.0 / (max - min);
    tnxaxis->ymaxmin =  - (max + min) / 2.0;
    switch (tnxaxis->xterms)
      {
      case TNX_XNONE:
        tnxaxis->ncoeff = tnxaxis->xorder + tnxaxis->yorder - 1;
        break;
      case TNX_XHALF:
        order = tnxaxis->xorder<tnxaxis->yorder?
			tnxaxis->xorder : tnxaxis->yorder;
        tnxaxis->ncoeff = tnxaxis->xorder*tnxaxis->yorder - order*(order-1)/2;
        break;
      case TNX_XFULL:
        tnxaxis->ncoeff = tnxaxis->xorder * tnxaxis->yorder;
        break;
      default:
        return NULL;
      }
/*-- Now read the mapping coefficients */
    if (!(tnxaxis->coeff=malloc(tnxaxis->ncoeff*sizeof(double))))
      return NULL;
    for (i=0; i<tnxaxis->ncoeff && (pstr=strtok(NULL, " ")); i++)
      tnxaxis->coeff[i] = atof(pstr);
    if (i!=tnxaxis->ncoeff)
      return NULL;
    if (!(tnxaxis->xbasis=malloc(tnxaxis->xorder*sizeof(double))))
      return NULL;
    if (!(tnxaxis->ybasis=malloc(tnxaxis->yorder*sizeof(double))))
      return NULL;
    return tnxaxis;
    }
  else
    return NULL;
  }


/******* free_tnxaxis *********************************************************
PROTO	void free_tnxaxis(tnxaxisstruct *axis)
PURPOSE	Free a TNX axis mapping structure.
INPUT	TNXAXIS structure pointer.
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP)
VERSION	09/04/2000
 ***/

void	free_tnxaxis(tnxaxisstruct *axis)

  {
  if (axis)
    {
    free(axis->coeff);
    free(axis->xbasis);
    free(axis->ybasis);
    free(axis);
    }

  return;
  }


/******* raw_to_tnxaxis *******************************************************
PROTO	double raw_to_tnxaxis(tnxaxisstruct *axis, double x, double y)
PURPOSE	Compute the correction value on a TNX axis at current position.
INPUT	TNXAXIS structure pointer,
	x coordinate,
	y coordinate.
OUTPUT	Value on the TNXaxis.
NOTES	-.
AUTHOR	E. Bertin (IAP)
VERSION	11/04/2000
 ***/

double	raw_to_tnxaxis(tnxaxisstruct *axis, double x, double y)

  {
   double	*xbasis, *ybasis,*coeff,
		norm, accum, val;
   int		i, j, xorder,xorder0,yorder,maxorder,xterms;

  xbasis = axis->xbasis;
  ybasis = axis->ybasis;
  xorder = axis->xorder;
  yorder = axis->yorder;
  xterms = axis->xterms;

  switch (axis->type)
    {
    case TNX_CHEBYSHEV:
      xbasis[0] = 1.0;
      if (xorder > 1)
        {
        xbasis[1] = norm = (x + axis->xmaxmin)*axis->xrange;
        if (xorder > 2)
          for (i = 2; i < xorder; i++)
	    xbasis[i] = 2.0*norm*xbasis[i-1] - xbasis[i-2];
        }
      ybasis[0] = 1.0;
      if (yorder > 1)
        {
        ybasis[1] = norm = (y + axis->ymaxmin)*axis->yrange;
        if (yorder > 2)
          for (i = 2; i < yorder; i++)
	    ybasis[i] = 2.0*norm*xbasis[i-1] - ybasis[i-2];
        }
      break;

    case TNX_LEGENDRE:
      xbasis[0] = 1.0;
      if (xorder > 1)
        {
        xbasis[1] = norm = (x + axis->xmaxmin)*axis->xrange;
        if (xorder > 2)
          for (i = 2; (j=i) < xorder; i++)
            xbasis[i] = ((2.0*j - 3.0) * norm * xbasis[i-1] -
                       (j - 2.0) * xbasis[i-2]) / (j - 1.0);
        }
      ybasis[0] = 1.0;
      if (yorder > 1)
        {
        ybasis[1] = norm = (y + axis->ymaxmin)*axis->yrange;
        if (yorder > 2)
          for (i = 2; (j=i) < xorder; i++)
            ybasis[i] = ((2.0*j - 3.0) * norm * ybasis[i-1] -
                       (j - 2.0) * ybasis[i-2]) / (j - 1.0);
        }
      break;

    case TNX_POLYNOMIAL:
      xbasis[0] = 1.0;
      if (xorder > 1)
        {
        xbasis[1] = x;
        if (xorder > 2)
          for (i = 2; i < xorder; i++)
            xbasis[i] = x * xbasis[i-1];
        }
      ybasis[0] = 1.0;
      if (yorder > 1)
        {
        ybasis[1] = y;
        if (yorder > 2)
          for (i = 2; i < yorder; i++)
            ybasis[i] = y * ybasis[i-1];
        }
      break;

    default:
      return 0.0;
    }

/* Loop over y basis functions */
  maxorder = xorder > yorder ? xorder : yorder;
  xorder0 = xorder;
  coeff = axis->coeff;
  val = 0.0;
  for (i = 0; i<yorder; i++)
    {
/*-- Loop over the x basis functions */
    accum = 0.0;
    xbasis = axis->xbasis;
    for (j = xorder; j--;)
      accum += *(coeff++) * *(xbasis++);
    val += accum**(ybasis++);

/*-- Elements of the coefficient vector where neither k = 1 or i = 1
           are not calculated if sf->xterms = no. */
    if (xterms == TNX_XNONE)
      xorder = 1;
    else if (xterms == TNX_XHALF && (i + 1 + xorder0) > maxorder)
      xorder--;
    }

  return val;
  }


