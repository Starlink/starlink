/*
 				weight.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Handling of weight maps.
*
*	Last modify:	21/07/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"field.h"
#include	"weight.h"

/******************************* newweight **********************************/
/*
Load a weight map and initialize relevant parameters.
*/
picstruct	*newweight(char *filename, picstruct *reffield,
			weightenum wtype)

  {
   picstruct	*wfield;
   float	*ratio,*ratiop, *weight,*sigma,
		sratio;
   int		i, nr;

  switch(wtype)
    {
    case WEIGHT_FROMINTERP:
      wfield = inheritfield(reffield, INTERP_FIELD);
      break;

    case WEIGHT_FROMBACK:
      wfield = inheritfield(reffield, BACKRMS_FIELD);
      wfield->sigfac = 1.0;
      break;

    case WEIGHT_FROMRMSMAP:
      wfield = newfield(filename, RMS_FIELD);
      if((wfield->width!=reffield->width)||(wfield->height!=reffield->height))
        error(EXIT_FAILURE,
	"*Error*: measured frame and weight map have different sizes","");
      wfield->sigfac = 1.0;
      break;

    case WEIGHT_FROMVARMAP:
      wfield = newfield(filename, VAR_FIELD);
      if((wfield->width!=reffield->width)||(wfield->height!=reffield->height))
        error(EXIT_FAILURE,
	"*Error*: measured frame and weight map have different sizes","");
      nr = 0;
      QMALLOC(ratio, float, wfield->nback);
      ratiop = ratio;
      weight = wfield->back;
      sigma = reffield->sigma;
      for (i=wfield->nback; i--; sigma++)
        if ((sratio=*(weight++)) > 0.0
		&& (sratio = *sigma/sqrt(sratio)) > 0.0)
          {
          *(ratiop++) = sratio;
          nr++;
          }
      wfield->sigfac = hmedian(ratio, nr);
      for (i=0; i<nr && ratio[i]<=0.0; i++);
      if (i<nr)
        wfield->sigfac = hmedian(ratio+i, nr-i);
      else
        {
        warning("Null or negative global weighting factor:","defaulted to 1");
        wfield->sigfac = 0.0;
        } 

      free(ratio);
      break;

    case WEIGHT_FROMWEIGHTMAP:
      wfield = newfield(filename, WEIGHT_FIELD);
      if((wfield->width!=reffield->width)||(wfield->height!=reffield->height))
        error(EXIT_FAILURE,
	"*Error*: measured frame and weight map have different sizes","");
      nr = 0;
      QMALLOC(ratio, float, wfield->nback);
      ratiop = ratio;
      weight = wfield->back;
      sigma = reffield->sigma;
      for (i=wfield->nback; i--; sigma++)
        if ((sratio=*(weight++)) >= 0.0
		&& (sratio = *sigma*sqrt(sratio)) > 0.0)
          {
          *(ratiop++) = sratio;
          nr++;
          }
      wfield->sigfac = hmedian(ratio, nr);
      for (i=0; i<nr && ratio[i]<=0.0; i++);
      if (i<nr)
        wfield->sigfac = hmedian(ratio+i, nr-i);
      else
        {
        warning("Null or negative global weighting factor:","defaulted to 1");
        wfield->sigfac = 0.0;
        } 

     free(ratio);
      break;
    default:
      error(EXIT_FAILURE,
	"*Internal Error*: Unknown weight-map type in ", "makeit()");
      break;
    }

  return wfield;
  }


/******************************** rms_to_var *********************************/
/*
Transform an RMS map into a variance map.
*/
void	rms_to_var(picstruct *wfield, PIXTYPE *data, int npix)

  {
   PIXTYPE	*datap;
   int		i;

  datap = data;
  for (i=npix; i--; datap++)
    *datap *= *datap;

  return;
  }


/******************************* weight_to_var *******************************/
/*
Transform an array of unnormalized weights into a variance map.
*/
void	weight_to_var(picstruct *wfield, PIXTYPE *data, int npix)

  {
   PIXTYPE	*datap, pix;
   float	sigfac2;
   int		i;

  sigfac2 = wfield->sigfac*wfield->sigfac;
  datap = data;
  for (i=npix; i--;)
    if ((pix=*datap) > 0.0)
      *(datap++) = sigfac2/pix;
    else
      *(datap++) = BIG;

  return;
  }

