 /*
 				field.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Handling of field structures.
*
*	Last modify:	14/12/2002
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
#include	"assoc.h"
#include	"astrom.h"
#include	"back.h"
#include	"field.h"
#include	"filter.h"
#include	"interpolate.h"

/********************************* newfield **********************************/
/*
Returns a pointer to a new field, ready to go!
*/
picstruct	*newfield(char *filename, int flags, int nok)

  {
   picstruct	*field;
   catstruct	*cat;
   tabstruct	*tab;
   OFF_T	mefpos;
   int		nok2, ntab;

/* Move to nok'th valid FITS image extension */
  if (!(cat = read_cat(filename)))
    error(EXIT_FAILURE, "*Error*: cannot open ", filename);
  close_cat(cat);
  tab = cat->tab;
  nok++;	/* At least one pass through the loop */
  nok2 = nok;
  for (ntab=cat->ntab; nok && ntab--; tab=tab->nexttab)
    {
    if ((tab->naxis < 2)
	|| !strncmp(tab->xtension, "BINTABLE", 8)
	|| !strncmp(tab->xtension, "ASCTABLE", 8))
      continue;
    mefpos = tab->headpos;
    nok--;
    }
  if (ntab<0)
    error(EXIT_FAILURE, "Not enough valid FITS image extensions in ",filename);

/* First allocate memory for the new field (and nullify pointers) */
  QCALLOC(field, picstruct, 1);
  field->mefpos = mefpos;
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
  if (cat->ntab>1)
    sprintf(gstr, "[%d/%d]", nok2, cat->ntab-1);
  QPRINTF(OUTPUT, "%s \"%.20s\" %s / %d x %d / %d bits %s data\n",
	flags&FLAG_FIELD?   "Flagging  from:" :
       (flags&(RMS_FIELD|VAR_FIELD|WEIGHT_FIELD)?
			     "Weighting from:" :
       (flags&MEASURE_FIELD? "Measuring from:" :
			     "Detecting from:")),
	field->ident,
        cat->ntab>1? gstr : "",
	field->width, field->height, field->bytepix*8,
	field->bitpix>0?
		(field->compress_type!=ICOMPRESS_NONE?"COMPRESSED":"INTEGER")
		:"FLOATING POINT");

/* Provide a buffer for compressed data */
  if (field->compress_type != ICOMPRESS_NONE)
    QMALLOC(field->compress_buf, char, FBSIZE);

/* Check the astrometric system and do the setup of the astrometric stuff */
  if (prefs.world_flag && (flags & (MEASURE_FIELD|DETECT_FIELD)))
    initastrom(field);
  else
    field->pixscale=prefs.pixel_scale;

/* Background */
  if (flags & (DETECT_FIELD|MEASURE_FIELD|WEIGHT_FIELD|VAR_FIELD|RMS_FIELD))
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
/*--  Set the back_type flag if absolute background is selected */
    if (((flags & DETECT_FIELD) && prefs.back_type[0]==BACK_ABSOLUTE)
	|| ((flags & MEASURE_FIELD) && prefs.back_type[1]==BACK_ABSOLUTE))
      field->back_type = BACK_ABSOLUTE;
    }

/* Compute the image buffer size */
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

  free_cat(&cat, 1);

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
  copyastrom(infield, field);
  QMEMCPY(infield->fitshead, field->fitshead, char, infield->fitsheadsize);
  field->interp_flag = 0;
  field->assoc = NULL;
  field->strip = NULL;
  field->fstrip = NULL;
  field->reffield = infield;
  field->compress_buf = NULL;
  field->compress_type = ICOMPRESS_NONE;
  field->file = NULL;

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
  endback(field);
  free(field);

  return;
  }

