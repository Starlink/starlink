 /*
 				field.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Handling of field structures.
*
*	Last modify:	02/02/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"assoc.h"
#include	"back.h"
#include	"field.h"
#include	"filter.h"
#include	"interpolate.h"
#include        "astrom.h"

/********************************* newfield **********************************/
/*
Returns a pointer to a new field, ready to go!
*/
picstruct	*newfield(char *filename, int flags)

  {
   picstruct	*field;

/* First allocate memory for the new field (and nullify pointers) */
  QCALLOC(field, picstruct, 1);
  field->flags = flags;
  strcpy (field->filename, filename);
/* A short, "relative" version of the filename */
  if (!(field->rfilename = strrchr(field->filename, '/')))
    field->rfilename = field->filename;
  else
    field->rfilename++;

  sprintf(gstr, "Looking for %s", field->rfilename);
  NFPRINTF(OUTPUT, gstr);
/* Check the image exists and read important info (image size, etc...) */
  readimagehead(field);
  if (prefs.verbose_type != QUIET)
    fprintf(OUTPUT, "%s \"%.20s\" / %d x %d / %d bits %s data\n",
	flags&FLAG_FIELD?   "Flagging  from:" :
       (flags&(RMS_FIELD|VAR_FIELD|WEIGHT_FIELD)?
			     "Weighting from:" :
       (flags&MEASURE_FIELD? "Measuring from:" :
			     "Detecting from:")),
	field->ident,
	field->width, field->height, field->bytepix*8,
	field->bitpix>0?
		(field->compress_type!=COMPRESS_NONE?"COMPRESSED":"INTEGER")
		:"FLOATING POINT");

/* Provide a buffer for compressed data */
  if (field->compress_type != COMPRESS_NONE)
    QMALLOC(field->compress_buf, char, FBSIZE);

/* Check the astrometric system and do the setup of the astrometric stuff */
  if (prefs.world_flag && (flags & (MEASURE_FIELD|DETECT_FIELD)))
    initastrom(field);
  else
    field->pixscale=prefs.pixel_scale;

/* Background */
  if (flags & (DETECT_FIELD|MEASURE_FIELD|WEIGHT_FIELD|VAR_FIELD))
    {
    field->ngamma = prefs.mag_gamma/log(10.0);

    field->backw = prefs.backsize[0]<field->width ? prefs.backsize[0]
						  : field->width;
    field->backh = prefs.backsize[1]<field->height ? prefs.backsize[1]
						   : field->height;
    field->nbackp = field->backw * field->backh;
    if ((field->nbackx = (field->width-1)/field->backw + 1) < 1)
      field->nbackx = 1;
    if ((field->nbacky = (field->height-1)/field->backh + 1) < 1)
      field->nbacky = 1;
    field->nback = field->nbackx * field->nbacky;
    field->nbackfx = field->nbackx>1 ? prefs.backfsize[0] : 1;
    field->nbackfy = field->nbacky>1 ? prefs.backfsize[1] : 1;
/*-- Now make the background map */
    makeback(field);
    }

/* Prepare the image buffer */
/* Basically, only one margin line is sufficient... */
  field->stripmargin = 1;
/* ...but : */
  field->stripheight = prefs.mem_bufsize;
  if (field->stripheight>field->height)
    field->stripheight = field->height;
  if (prefs.filter_flag)
    {
/*-- If filtering is on, one should consider the height of the conv. mask */
     int	margin;

    if (field->stripheight < thefilter->convh)
      field->stripheight = thefilter->convh;
    if (field->stripmargin < (margin = (thefilter->convh-1)/2))
      field->stripmargin = margin;
    }

/* Allocate space for the frame-buffer */
  if (flags ^ FLAG_FIELD)
    {
    if (!(field->strip=(PIXTYPE *)malloc(field->stripheight*field->width
        *sizeof(PIXTYPE))))
      error(EXIT_FAILURE,"Not enough memory for the image buffer of ",
	field->rfilename);
    }
  else
    {
    if (!(field->fstrip=(FLAGTYPE *)malloc(field->stripheight*field->width
        *sizeof(FLAGTYPE))))
      error(EXIT_FAILURE,"Not enough memory for the flag buffer of ",
	field->rfilename);
    }

  if ((flags & DETECT_FIELD) || (flags & MEASURE_FIELD))
    {

    if (prefs.ndthresh > 1)
      {
       double	dval;

      if (fabs(dval=prefs.dthresh[0] - prefs.dthresh[1])> 70.0)
        error(EXIT_FAILURE,
	"*Error*: I cannot deal with such extreme thresholds!", "");

      field->dthresh = field->pixscale*field->pixscale
		*pow(10.0, -0.4*dval);
      }
    else
      field->dthresh = prefs.dthresh[0]*field->backsig;
    if (prefs.nthresh > 1)
      {
       double	dval;

      if (fabs(dval=prefs.thresh[0] - prefs.thresh[1]) > 70.0)
        error(EXIT_FAILURE,
	"*Error*: I cannot deal with such extreme thresholds!", "");

      field->thresh = field->pixscale*field->pixscale
	*pow(10.0, -0.4*dval);
      }
    else
      field->thresh = prefs.thresh[0]*field->backsig;
    if (prefs.verbose_type != QUIET)
      fprintf(OUTPUT, "    Background: %-10g RMS: %-10g / Threshold: %-10g \n",
	field->backmean, field->backsig, (flags & DETECT_FIELD)?
	field->dthresh: field->thresh);

#ifdef	QUALITY_CHECK
    printf("%-10g %-10g %-10g\n", field->backmean, field->backsig,
	(flags & DETECT_FIELD)? field->dthresh : field->thresh);
#endif

    if (field->dthresh<=0.0 || field->thresh<=0.0)
      error(EXIT_FAILURE,
	"*Error*: I cannot deal with zero or negative thresholds!", "");

    if (prefs.detect_type == PHOTO
	&& field->backmean+3*field->backsig > 50*field->ngamma)
      error(EXIT_FAILURE,
	"*Error*: The density range of this image is too large for ",
	"PHOTO mode");
    }

/* Prepare learn and/or associations */
  if (flags & MEASURE_FIELD && prefs.assoc_flag)
    init_assoc(field);			/* initialize assoc tasks */

  return field;
  }


/******************************* inheritfield *******************************/
/*
Make a copy of a field structure, e.g. for interpolation purposes.
*/
picstruct	*inheritfield(picstruct *infield, int flags)

  {
   picstruct	*field;

/* First allocate memory for the new field (and nullify pointers) */
  QCALLOC(field, picstruct, 1);

/* Copy what is important and reset the remaining */
  *field = *infield;
  field->flags = flags;
  copyback(infield, field);
  copyastrom(infield, field);
  QMEMCPY(infield->fitshead, field->fitshead, char, infield->fitsheadsize);
  field->assoc = NULL;
  field->strip = NULL;
  field->fstrip = NULL;
  field->copystrip = infield->strip;
  field->compress_buf = NULL;
  field->compress_type = COMPRESS_NONE;
  field->file = NULL;

/* Allocate space for the frame-buffer */
  if (flags ^ FLAG_FIELD)
    {
    if (!(field->strip=(PIXTYPE *)malloc(field->stripheight*field->width
        *sizeof(PIXTYPE))))
      error(EXIT_FAILURE,"Not enough memory for the image buffer of ",
	field->rfilename);
    }
  else
    {
    if (!(field->fstrip=(FLAGTYPE *)malloc(field->stripheight*field->width
        *sizeof(FLAGTYPE))))
      error(EXIT_FAILURE,"Not enough memory for the flag buffer of ",
	field->rfilename);
    }

  return field;
  }


/********************************* endfield **********************************/
/*
Free and close everything related to a field structure.
*/
void	endfield(picstruct *field)

  {
  if (field->file)
    fclose(field->file);

  free(field->fitshead);
  free(field->strip);
  free(field->fstrip);
  free(field->compress_buf);
  if (field->astrom)
    endastrom(field);
  if (field->interp_flag)
    end_interpolate(field);
  end_assoc(field);			/* close learning routines */
  endback(field);
  free(field);

  return;
  }

