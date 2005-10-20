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
*	Last modify:	10/05/99 (EB):
*                       28/10/98 (AJC):
*                          Major remodel to produce NDFs
*                          N.B. Header information lost.
*                       22/10/99 (PWD):
*                          Added initialisation of overlay, this fixes
*                          a problem with aperture display.
*	Last modify:	15/12/2002
*                          (EB): 2.3
*	Last modify:	26/11/2003
*	Last modify:	15/06/2004
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

#include        "sae_par.h"
#include        "ndf.h"
#include        "dat_par.h"

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
  checkstruct	*check;

  QCALLOC(check, checkstruct, 1);

  strcpy(check->filename, filename);
  check->type = check_type;

  return check;
}

/******************************** reinitcheck ********************************/
/*
initialize check-image (for subsequent writing).
*/
void	reinitcheck(picstruct *field, checkstruct *check)
{
  int		i;
  PIXTYPE	*pntrf;
  void         *pntr[3];
  int 		placehldr;
  int          lbnd[2], ubnd[2];
  int          status = SAI__OK;
  
  ndfOpen( DAT__ROOT, check->filename, "WRITE", "UNKNOWN",
           &check->ndf, &placehldr, &status );
  if ( status != SAI__OK ) 
    error(EXIT_FAILURE, "*Error*: Cannot open for output ", check->filename);

  if ( check->ndf != NDF__NOID ) {
/* Delete an existing file */
     ndfDelet( &check->ndf, &status );
/* And get a placeholder */
     ndfOpen( DAT__ROOT, check->filename, "WRITE", "UNKNOWN",
              &check->ndf, &placehldr, &status );
     if ( status != SAI__OK )
        error(EXIT_FAILURE, "*Error*: Deleting existing file ", check->filename);
  }     

/* We should now have a placeholder */
  if ( placehldr == NDF__NOPL )
     error(EXIT_FAILURE, "*Error*: Getting NDF placeholder ", check->filename);

/* Create an NDF of the required type and size */
  lbnd[0]=lbnd[1]=1;
  switch(check->type)
  {
  case CHECK_BACKRMS:
  case CHECK_SUBOBJECTS:
/*---- Allocate memory for replacing the blanked pixels by 0 */
     if (!check->line)
        QMALLOC(check->line, PIXTYPE, field->width);
/*---- Fall through here */
  case CHECK_IDENTICAL:
  case CHECK_BACKGROUND:
  case CHECK_FILTERED:
  case CHECK_SUBTRACTED:
  case CHECK_OBJECTS:
  case CHECK_APERTURES:
  case CHECK_SUBPSFPROTOS:
  case CHECK_PSFPROTOS:
  case CHECK_SUBPCPROTOS:
  case CHECK_PCPROTOS:
  case CHECK_PCOPROTOS:
  case CHECK_ASSOC:
  case CHECK_MAPSOM:
     ubnd[0] = check->width = field->width;
     ubnd[1] = check->height = field->height;
     check->npix = field->npix;
     check->overlay = 30*field->backsig;
/*     ndfBound( field->ndf, 2, lbnd, ubnd, &ndim, &status );*/
     ndfNew( "_REAL", 2, lbnd, ubnd, &placehldr, &check->ndf, &status );
     break;
  case CHECK_SEGMENTATION:
     ubnd[0] = check->width = field->width;
     ubnd[1] = check->height = field->height;
     check->npix = field->npix;
     ndfNew( "_INTEGER", 2, lbnd, ubnd, &placehldr, &check->ndf, &status );
     break;
  case CHECK_MINIBACKGROUND:
  case CHECK_MINIBACKRMS:
     ubnd[0] = check->width = field->nbackx;
     ubnd[1] = check->height = field->nbacky;
     check->npix = check->width * check->height;
     ndfNew( "_REAL", 2, lbnd, ubnd, &placehldr, &check->ndf, &status );
     break;
  default:
     error(EXIT_FAILURE, "*Error* Invalid check_type in ", "initcheck()!");
  }         

/* Propagate the original NDF title, if available. */
  if ( field->ident[0] != '\0' ) {
    ndfCput( field->ident, check->ndf, "TITLE", &status );
  }

/* Now map and initialise appropriately */
  check->pix = NULL;
  switch (check->type)
  {
  case CHECK_IDENTICAL:
  case CHECK_BACKGROUND:
  case CHECK_FILTERED:
  case CHECK_SUBTRACTED:
  case CHECK_BACKRMS:
  case CHECK_SUBOBJECTS:
     QMALLOC( pntrf, PIXTYPE, check->width);
     check->pix = (void *)pntrf;
/*----- Fall through here */
  case CHECK_OBJECTS:
  case CHECK_APERTURES:
  case CHECK_SUBPSFPROTOS:
  case CHECK_PSFPROTOS:
  case CHECK_SUBPCPROTOS:
  case CHECK_PCPROTOS:
  case CHECK_PCOPROTOS:
     ndfMap( check->ndf, "DATA", "_REAL", "WRITE/ZERO", 
             pntr, &check->nel, &status );
     check->map = pntr[0];
     if ( check->pix == NULL ) {
       check->pix = check->map;
     }
     break;
  case CHECK_ASSOC:
     ndfMap( check->ndf, "DATA", "_REAL", "WRITE/BAD", 
             pntr, &check->nel, &status );
     check->pix = check->map = pntr[0];
     break;
  case CHECK_MAPSOM:
     ndfMap( check->ndf, "DATA", "_REAL", "WRITE", 
             pntr, &check->nel, &status );
     check->pix = check->map = pntr[0];
     for (i=check->nel,pntrf=(PIXTYPE *)check->map;i--;)
        *(pntrf++) = -10.0;
     break;
  case CHECK_SEGMENTATION:
     ndfMap( check->ndf, "DATA", "_INTEGER", "WRITE/ZERO", 
             pntr, &check->nel, &status );
     check->pix = check->map = pntr[0];
     break;
  case CHECK_MINIBACKGROUND:
  case CHECK_MINIBACKRMS:
     ndfMap( check->ndf, "DATA", "_REAL", "WRITE", 
             pntr, &check->nel, &status );
     check->map = pntr[0];
     if (check->type==CHECK_MINIBACKRMS)
        memcpy(check->map, field->sigma, check->npix*sizeof(float));
     else
        memcpy(check->map, field->back, check->npix*sizeof(float));
     ndfAnnul(&check->ndf,&status);
     break;
  default:
     error(EXIT_FAILURE, "*Error* Invalid check_type in ", "initcheck()!");
  }
  if ( status != SAI__OK )
    error(EXIT_FAILURE, "*Error*: Creating check image ", check->filename);

  return;
  }


/******************************** writecheck *********************************/
/*
Write ONE line of pixels of a check-image.
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
  else if (check->type == CHECK_BACKRMS)
    {
     int	i;
     PIXTYPE	*pixt;

    pixt = check->line;
    for (i=w; i--;)
      *(pixt++) = (PIXTYPE)sqrt(*(data++));
    data = check->line;
    }

/* Write line into mapped NDF */
  memcpy( (PIXTYPE *)check->map+check->pos, data, w*sizeof(PIXTYPE) );
  check->pos += w;
  return;
  }


/********************************* reendcheck **********************************/
/*
 Finish current check-image.
*/
void	reendcheck(picstruct *field, checkstruct *check)
  {
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
      break;

    case CHECK_SEGMENTATION:
      break;

    case CHECK_SUBOBJECTS:
      {
       int	y;

      for (y=field->ymin; y<field->ymax; y++)
        writecheck(check, &PIX(field, 0, y), field->width);
      free(check->pix);
      free(check->line);
      check->line = NULL;
      break;
      }

    case CHECK_MAPSOM:
      break;

    default:
      error(EXIT_FAILURE, "*Internal Error* in ", "endcheck()!");
    }

  return;
  }

/********************************* endcheck **********************************/
/*
close check-image.
*/
void	endcheck(checkstruct *check)
  {
  int status = SAI__OK;
  free(check);
  ndfAnnul( &check->ndf, &status );
  return;
  }

