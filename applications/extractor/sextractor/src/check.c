/*
 				check.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	handling of "check-images".
*
*	Last modify:	26/11/2003
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
#include	"fits/fitscat.h"
#include	"astrom.h"
#include	"check.h"

/********************************* addcheck **********************************/
/*
Add a PSF to a CHECK-image (with a multiplicative factor).
Outside boundaries are taken into account.
*/
void	addcheck(checkstruct *check, float *psf,
			int w,int h, int ix,int iy, float amplitude)
  {
   PIXTYPE	*pix;
   int		x,y, xmin,xmax,ymin,ymax,w2, dwpsf;

/* Set the image boundaries */
  w2 = w;
  ymin = iy-h/2;
  ymax = ymin + h;
  if (ymin<0)
    {
    psf -= ymin*w;
    ymin = 0;
    }
  if (ymax>check->height)
    ymax = check->height;

  xmin = ix-w/2;
  xmax = xmin + w;
  if (xmax>check->width)
    {
    w2 -= xmax-check->width;
    xmax = check->width;
    }
  if (xmin<0)
    {
    psf += -xmin;
    w2 -= -xmin;
    xmin = 0;
    }

  dwpsf = w-w2;
/* Subtract the right pixels to the destination */
  for (y=ymin; y<ymax; y++, psf += dwpsf)
    {
    pix = (float *)check->pix+y*check->width+xmin;
    for (x=w2; x--;)
      *(pix++) += amplitude**(psf++);
    }

  return;
  }


/********************************* blankcheck *******************************/
/*
Blank a part of the CHECK-image according to a mask.
*/
void	blankcheck(checkstruct *check, PIXTYPE *mask, int w,int h,
		int xmin,int ymin, PIXTYPE val)
  {
   PIXTYPE	*pixt;
   int		x,y, xmax,ymax,w2,wc;

/* Don't go further if out of frame!! */
  if (xmin+w<0 || xmin>=check->width
	|| ymin+h<0 || ymin>=check->height)
    return;
 
/* Set the image boundaries */
  w2 = w;
  ymax = ymin + h;
  if (ymin<0)
    {
    mask -= ymin*w;
    ymin = 0;
    }
  if (ymax>check->height)
    ymax = check->height;

  xmax = xmin + w;
  if (xmax>check->width)
    {
    w2 -= xmax - check->width;
    xmax = check->width;
    }
  if (xmin<0)
    {
    mask += -xmin;
    w2 -= -xmin;
    xmin = 0;
    }

  w -= w2;
  wc = check->width;
  ymin = ymin*wc+xmin;
  ymax = ymax*wc+xmin;

/* Blank the right pixels in the image */
  for (y=ymin; y<ymax; y+=wc, mask += w)
    {
    pixt = (float *)check->pix + y;
    for (x=w2; x--; pixt++)
      if (*(mask++) > -BIG)
        *pixt = val;
    }

  return;
  }


/******************************** initcheck **********************************/
/*
initialize check-image.
*/
checkstruct	*initcheck(char *filename, checkenum check_type, int next)

  {
   catstruct	*fitscat;
   checkstruct	*check;

  QCALLOC(check, checkstruct, 1);

  strcpy(check->filename, filename);
  check->type = check_type;

  if (next>1)
/*-- Create a "pure" primary HDU */
    {
    fitscat = new_cat(1);
    init_cat(fitscat);
    strcpy(fitscat->filename, filename);
    fitsadd(fitscat->tab->headbuf, "NEXTEND ", "Number of extensions");
    fitswrite(fitscat->tab->headbuf, "NEXTEND ", &next, H_INT, T_LONG);
    if (open_cat(fitscat, WRITE_ONLY) != RETURN_OK)
      error(EXIT_FAILURE,"*Error*: cannot open for writing ", filename);
    save_tab(fitscat, fitscat->tab);
    check->file = fitscat->file;
    fitscat->file = NULL;
    free_cat(&fitscat, 1);
    }
  else
    if (!(check->file = fopen(check->filename, "wb")))
      error(EXIT_FAILURE, "*Error*: Cannot open for output ", check->filename);

  return check;
  }


/******************************** reinitcheck ********************************/
/*
initialize check-image (for subsequent writing).
*/
void	reinitcheck(picstruct *field, checkstruct *check)

  {
   astromstruct	*as;
   char		*buf;
   int		i, ival;
   size_t	padsize;
   double	dval;
   USHORT	*ptri;
   PIXTYPE	*ptrf;

/* Inherit the field FITS header */
  check->fitsheadsize = field->fitsheadsize;
  QMALLOC(check->fitshead, char, check->fitsheadsize);
  memcpy(check->fitshead, field->fitshead, check->fitsheadsize);
  check->y = 0;
/* Neutralize possible scaling factors */
  dval = 1.0;fitswrite(check->fitshead, "BSCALE  ", &dval, H_FLOAT, T_DOUBLE);
  dval = 0.0;fitswrite(check->fitshead, "BZERO   ", &dval, H_FLOAT, T_DOUBLE);
  ival = 1;fitswrite(check->fitshead, "BITSGN  ", &ival, H_INT, T_LONG);
  if (field->compress_type != ICOMPRESS_NONE)
    fitswrite(check->fitshead, "IMAGECOD", "NONE", H_STRING, T_STRING);
  fitswrite(check->fitshead, "ORIGIN  ", BANNER, H_STRING, T_STRING);

  switch(check->type)
    {
    case CHECK_IDENTICAL:
    case CHECK_BACKGROUND:
    case CHECK_FILTERED:
    case CHECK_SUBTRACTED:
      ival = -32;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->width;
      check->height = field->height;
      check->npix = field->npix;
      QMALLOC(ptrf, PIXTYPE, check->width);
      check->pix = (void *)ptrf;
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
      break;

    case CHECK_BACKRMS:
    case CHECK_SUBOBJECTS:
      ival = -32;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->width;
      check->height = field->height;
      check->npix = field->npix;
      QMALLOC(ptrf, PIXTYPE, check->width);
      check->pix = (void *)ptrf;
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
/*---- Allocate memory for replacing the blanked pixels by 0 */
      if (!check->line)
        QMALLOC(check->line, PIXTYPE, field->width);
      break;

    case CHECK_OBJECTS:
    case CHECK_APERTURES:
    case CHECK_SUBPSFPROTOS:
    case CHECK_PSFPROTOS:
    case CHECK_SUBPCPROTOS:
    case CHECK_PCPROTOS:
    case CHECK_PCOPROTOS:
      ival = -32;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->width;
      check->height = field->height;
      check->npix = field->npix;
      check->overlay = 30*field->backsig;
      QCALLOC(ptrf, PIXTYPE, check->npix);
      check->pix = (void *)ptrf;
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
      break;

    case CHECK_SEGMENTATION:
      ival = 16;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->width;
      check->height = field->height;
      check->npix = field->npix;
      QCALLOC(ptri, USHORT, check->npix);
      check->pix = (void *)ptri;
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
      break;

    case CHECK_ASSOC:
      ival = -32;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->width;
      check->height = field->height;
      check->npix = field->npix;
      QMALLOC(ptrf, PIXTYPE, check->npix);
      check->pix = (void *)ptrf;
/*---- Initialize the pixmap to IEEE NaN */
      memset(ptrf, 0xFF, check->npix*sizeof(LONG));
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
      break;

    case CHECK_MINIBACKGROUND:
    case CHECK_MINIBACKRMS:
      ival = -32;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->nbackx;
      fitswrite(check->fitshead, "NAXIS1  ", &check->width, H_INT, T_LONG);
      check->height = field->nbacky;
      fitswrite(check->fitshead, "NAXIS2  ", &check->height, H_INT, T_LONG);
/*---- Scale the WCS information if present */
      if ((as=field->astrom))
        {
        dval = as->cdelt[0]*field->backw;
        fitswrite(check->fitshead, "CDELT1  ", &dval, H_EXPO, T_DOUBLE);
        dval = as->cdelt[1]*field->backh;
        fitswrite(check->fitshead, "CDELT2  ", &dval, H_EXPO, T_DOUBLE);
        dval = (as->crpix[0]-0.5)/field->backw + 0.5;
        fitswrite(check->fitshead, "CRPIX1  ", &dval, H_EXPO, T_DOUBLE);
        dval = (as->crpix[1]-0.5)/field->backh + 0.5;
        fitswrite(check->fitshead, "CRPIX2  ", &dval, H_EXPO, T_DOUBLE);

        dval = as->pc[0]*as->cdelt[0]*field->backw;
        fitswrite(check->fitshead, "CD1_1   ", &dval, H_EXPO, T_DOUBLE);
        dval = as->pc[1]*as->cdelt[1]*field->backh;
        fitswrite(check->fitshead, "CD1_2  ", &dval, H_EXPO, T_DOUBLE);
        dval = as->pc[2]*as->cdelt[0]*field->backw;
        fitswrite(check->fitshead, "CD2_1   ", &dval, H_EXPO, T_DOUBLE);
        dval = as->pc[3]*as->cdelt[1]*field->backh;
        fitswrite(check->fitshead, "CD2_2  ", &dval, H_EXPO, T_DOUBLE);
        }
      check->npix = check->width*check->height;
      QMALLOC(ptrf, PIXTYPE, check->npix);
      check->pix = (void *)ptrf;
      if (check->type==CHECK_MINIBACKRMS)
        memcpy(check->pix, field->sigma, check->npix*sizeof(float));
      else
        memcpy(check->pix, field->back, check->npix*sizeof(float));
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
      if (bswapflag)
        swapbytes(check->pix, sizeof(float), (int)check->npix);
      QFWRITE(check->pix,check->npix*sizeof(float),check->file,
	check->filename);
/*---- Put the buffer back to its original state */
      if (bswapflag)
        swapbytes(check->pix, sizeof(float), (int)check->npix);
      free(check->pix);
      QCALLOC(buf, char, FBSIZE);
      padsize = (FBSIZE -((check->npix*sizeof(PIXTYPE))%FBSIZE))% FBSIZE;
      if (padsize)
        QFWRITE (buf, padsize, check->file, check->filename);
      free(buf);
      break;

    case CHECK_MAPSOM:
      ival = -32;
      fitswrite(check->fitshead, "BITPIX  ", &ival, H_INT, T_LONG);
      check->width = field->width;
      check->height = field->height;
      check->npix = field->npix;
      QMALLOC(ptrf, PIXTYPE, check->npix);
      check->pix = (void *)ptrf;
      for (i=check->npix; i--;)
        *(ptrf++) = -10.0;
      QFWRITE(check->fitshead,check->fitsheadsize,check->file,check->filename);
      free(check->fitshead);
      break;

    default:
      error(EXIT_FAILURE, "*Internal Error* in ", "initcheck()!");
    }

  return;
  }


/******************************** writecheck *********************************/
/*
Write ONE line of npix pixels of a check-image.
*/
void	writecheck(checkstruct *check, PIXTYPE *data, int w)

  {
  if (check->type == CHECK_APERTURES || check->type == CHECK_SUBPSFPROTOS
	|| check->type == CHECK_SUBPCPROTOS || check->type == CHECK_PCOPROTOS)
    {
    memcpy((PIXTYPE *)check->pix + w*(check->y++), data, w*sizeof(PIXTYPE));
    return;
    }
  else if (check->type == CHECK_SUBOBJECTS)
    {
     int	i;
     PIXTYPE	*pixt;

    pixt = check->line;
    for (i=w; i--; data++)
      *(pixt++) = (*data>-BIG)? *data:0.0;
    data = check->line;
    }

  if (bswapflag)
    swapbytes(data, sizeof(PIXTYPE), w);
  QFWRITE(data, w*sizeof(PIXTYPE), check->file, check->filename);
  if (bswapflag)
/*-- Put the buffer back to its original state */
    swapbytes(data, sizeof(PIXTYPE), w);

  return;
  }


/********************************* reendcheck ********************************/
/*
Finish current check-image.
*/
void	reendcheck(picstruct *field, checkstruct *check)
  {
   char		*buf;
   size_t	padsize;

  padsize = 0;				/* To avoid gcc -Wall warnings */
  switch(check->type)
    {
    case CHECK_MINIBACKGROUND:
    case CHECK_MINIBACKRMS:
      return;

    case CHECK_IDENTICAL:
    case CHECK_BACKGROUND:
    case CHECK_BACKRMS:
    case CHECK_FILTERED:
    case CHECK_SUBTRACTED:
      free(check->pix);
      free(check->line);
      check->line = NULL;
      padsize = (FBSIZE -((check->npix*sizeof(PIXTYPE))%FBSIZE)) % FBSIZE;
      break;

    case CHECK_OBJECTS:
    case CHECK_APERTURES:
    case CHECK_SUBPSFPROTOS:
    case CHECK_PSFPROTOS:
    case CHECK_SUBPCPROTOS:
    case CHECK_PCPROTOS:
    case CHECK_PCOPROTOS:
    case CHECK_ASSOC:
      if (bswapflag)
        swapbytes(check->pix, sizeof(PIXTYPE), (int)check->npix);
      QFWRITE(check->pix,check->npix*sizeof(PIXTYPE),
		check->file,check->filename);
      free(check->pix);
      padsize = (FBSIZE-((check->npix*sizeof(PIXTYPE))%FBSIZE)) % FBSIZE;
      break;

    case CHECK_SEGMENTATION:
      if (bswapflag)
        swapbytes(check->pix, sizeof(USHORT), (int)check->npix);
      QFWRITE(check->pix,check->npix*sizeof(USHORT),
		check->file,check->filename);
      free(check->pix);
      padsize = (FBSIZE -((check->npix*sizeof(USHORT))%FBSIZE)) % FBSIZE;
      break;

    case CHECK_SUBOBJECTS:
      {
       int	y;

      for (y=field->ymin; y<field->ymax; y++)
        writecheck(check, &PIX(field, 0, y), field->width);
      free(check->pix);
      free(check->line);
      check->line = NULL;
      padsize = (FBSIZE -((check->npix*sizeof(PIXTYPE))%FBSIZE)) % FBSIZE;
      break;
      }

    case CHECK_MAPSOM:
      if (bswapflag)
        swapbytes(check->pix, sizeof(PIXTYPE), (int)check->npix);
      QFWRITE(check->pix,check->npix*sizeof(PIXTYPE),
		check->file,check->filename);
      free(check->pix);
      padsize = (FBSIZE -((check->npix*sizeof(USHORT))%FBSIZE)) % FBSIZE;
      break;

    default:
      error(EXIT_FAILURE, "*Internal Error* in ", "endcheck()!");
    }

  QCALLOC(buf, char, FBSIZE);
  if (padsize)
    QFWRITE (buf, padsize, check->file, check->filename);
  free(buf);

  return;
  }

/********************************* endcheck **********************************/
/*
close check-image.
*/
void	endcheck(checkstruct *check)
  {

  fclose(check->file);
  free(check);

  return;
  }

