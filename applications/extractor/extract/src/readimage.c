/*
 				readimage.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	ADAM SExtractor
*
*	Author:		A. Chipperfield (Starlink, CCLRC, RAL)
*
*	Contents:	functions for input of image data.
*
*	Last modify:	28/10/98 (AJC)
*                          In line with V2.0.15
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include        "ast.h"

/*#include	"wcs/wcs.h"*/
/* Special to avoid re-defining prjprm */
#include        "wcsmod/wcs.h"
#include	"define.h"
#include	"globals.h"
#include	"check.h"
#include	"field.h"
#include	"fitscat.h"
#include	"interpolate.h"
#include	"back.h"
#include	"astrom.h"
#include	"weight.h"

#include        "sae_par.h"
#include        "dat_par.h"
#include        "ndf.h"

/******************************* readimagehead *******************************/
/*
extract some data from the FITS-file header
*/
void	readimagehead(picstruct *field)
  {
   int          status = SAI__OK;
   int		i,j,l, n;
   int          ndims, dims[NDF__MXDIM];
   int          there;
   char		*buf, st[80], str[80], *point;
   char         type[20];
   void         *pntr[3];
   int          nel;
   int          placehldr;

/* Open the file */
  field->file = 0;
  ndfOpen( DAT__ROOT, field->filename, "READ", "OLD",
           &field->ndf, &placehldr, &status );

  if (status != SAI__OK)
    error(EXIT_FAILURE,"*Error*: Failed to open ", field->filename);
    
/*---------------------------- Basic keywords ------------------------------*/
  ndfType( field->ndf, "DATA", type, 20, &status );
  if (!(status == SAI__OK))
    error(EXIT_FAILURE,"*Error*: Failed to get data type. ", field->filename);
  if (!strcmp(type,"_UBYTE")) {
     field->bitpix = BP_BYTE;
  } else if (!strcmp(type,"_WORD")) {
     field->bitpix = BP_SHORT;
  } else if (!strcmp(type,"_INTEGER")) {
     field->bitpix = BP_LONG;
  } else if (!strcmp(type,"_REAL")) {
     field->bitpix = BP_FLOAT;
  } else if (!strcmp(type,"_DOUBLE")) {
     field->bitpix = BP_DOUBLE;
  } else error(EXIT_FAILURE, "Sorry, I don't know that kind of data.", "");

/*  field->bytepix = (field->bitpix>0?field->bitpix:-field->bitpix)>>3;*/
  field->bytepix = 4;

  ndfDim( field->ndf, NDF__MXDIM, dims, &ndims, &status );
  if ( !(ndims==2)) 
    error( EXIT_FAILURE, field->filename, " does NOT contain 2D data." );

  field->width = dims[0];
  field->height = dims[1];
  field->npix = (size_t)field->width*field->height;

  field->bscale = 1.0;
  field->bzero = 0.0;
  field->bitsgn = 1;
  if (field->bitsgn && prefs.fitsunsigned_flag)
    field->bitsgn = 0;

  ndfCget( field->ndf, "TITLE", field->ident, MAXCHAR, &status );

/*----------------------------- Astrometry ---------------------------------*/
/* Presently, astrometry is done only on the measurement and detect images */
  if (field->flags&(MEASURE_FIELD|DETECT_FIELD)) {
     astromstruct	*as;
     double		drota, s;
     int		naxis;
     int                ifits;
     AstFrameSet        *frame;
     AstFitsChan        *fitschan;
     int nobj;
     int ncards;
     char card[81];

    QCALLOC(as, astromstruct, 1);
    field->astrom = as;

/* Create a FitsChan */
    fitschan = astFitsChan( NULL, NULL, "Encoding=FITS-WCS" );

/* Read the WCS component into it */
/*    ndfGtwcs( field->ndf, &ifits, &status );
    frame = astI2P( ifits );
*/
    ndfGtwcs( field->ndf, &frame, &status );
    nobj = astWrite( fitschan, frame );
/*ncards = astGetI(fitschan,"Ncard");
printf("fitschan has %d cards\n",ncards);
astClear( fitschan, "Card" );
while ( astFindFits( fitschan, "%f", card, 1 ) ) printf("%s\n",card);
*/
    if ( nobj ) {

    naxis = as->naxis = 2;
    for (l=0; l<naxis; l++) {
/*  See if there is an axis structure */
/*      ndfAstat( field->ndf, "Centre", l, &there, &status );
      if ( there ) {
         ndfAtype( field->ndf, "Centre", l, type, 20, &status );
*/
/*
 *  The axis structure is found, so map it using an appropriate data
 *  type.  Use _REAL for all but double-precision centres.  See if the
 *  axis is linear.
*/
/*         if ( !strcmp(type, "_DOUBLE")) {
            ndfMap( field->ndf, 'Centre', l, "_DOUBLE", "READ",
            apntr[l], &nel, &status );
         } else {
            ndfMap( field->ndf, 'Centre', l, "_REAL", "READ",
            apntr[l], &nel, &status );
         }
*/
/*      strcpy(as->ctype[l], "");
      strcpy(as->cunit[l], "deg");
      as->crval[l] = 0.0;
      as->crpix[l] = 1.0;
      as->cdelt[l] = 1.0;
*/

      sprintf(str, "CTYPE%d", l+1);
      FITSTOS(str, str, "");
      strncpy(as->ctype[l], str, 8);
      sprintf(str, "CUNIT%d", l+1);
      FITSTOS(str, str, "deg");
      strncpy(as->cunit[l], str, 32);
      sprintf(str, "CRVAL%d", l+1);
      as->crval[l] = FITSTOF(str, 0.0);
      sprintf(str, "CRPIX%d", l+1);
      as->crpix[l] = FITSTOF(str, 1.0);
      sprintf(str, "CDELT%d", l+1);
      as->cdelt[l] = FITSTOF(str, 1.0);
      if (fabs(as->cdelt[l]) < 1/BIG)
        error(EXIT_FAILURE, "*Error*: CDELT parameters out of range in ", 
		field->filename);
      }
/*    if (fitsnfind(buf, "CD1_1", n))*/
    if ( astClear( fitschan, "Card" ),
         astFindFits( fitschan, "CD1_1", NULL,0) )
      {
/*---- If CD keywords exist, use them for the linear mapping terms... */

      for (l=0; l<naxis; l++)
        for (j=0; j<naxis; j++)
          {
          sprintf(str, "CD%d_%d", l+1, j+1);
          as->pc[l*naxis+j] = FITSTOF(str, l==j?1.0:0.0)/as->cdelt[j];
          }
      }
/*    else if (fitsnfind(buf, "PC001001", n))*/
    else if ( astClear( fitschan, "Card" ),
              astFindFits( fitschan, "PC001001", NULL, 0) )
/*---- ...If PC keywords exist, use them for the linear mapping terms... */
      for (l=0; l<naxis; l++)
        for (j=0; j<naxis; j++)
          {
          sprintf(str, "PC%03d%03d", l+1, j+1);
          as->pc[l*naxis+j] = FITSTOF(str, l==j?1.0:0.0);
          }
    else
      {
/*---- ...otherwise take the obsolete CROTA2 parameter */
      s = as->cdelt[1]/as->cdelt[0];
      drota = FITSTOF("CROTA2  ", 0.0);
      as->pc[3] = as->pc[0] = cos(drota*DEG);
      as->pc[1] = -(as->pc[2] = sin(drota*DEG));
      as->pc[1] *= s;
      as->pc[2] /= s;
      }

    QMALLOC(as->wcs, struct wcsprm, 1);
/*-- Test if the WCS is recognized and a celestial pair is found */
    l = wcsset(as->naxis,(const char(*)[9])as->ctype, as->wcs);
    if (prefs.world_flag
	&& !l
	&& as->wcs->flag<999)
      {
       char	*pstr;
       double	date;
       int	biss, dpar[3];
      as->wcs_flag = 1;
/*---- Coordinate reference frame */
/*---- Search for an observation date expressed in Julian days */
      date = FITSTOF("MJD-OBS ",  -1.0);
/*---- Precession date (defined from Ephemerides du Bureau des Longitudes) */
/*---- in Julian years from 2000.0 */
      if (date>0.0)
        as->equinox = 2000.0 - (MJD2000 - date)/365.25;
      else
        {
/*------ Search for an observation date expressed in "civil" format */
        FITSTOS("DATE-OBS", str, "");
        if (*str)
          {
/*-------- Decode DATE-OBS format: DD/MM/YY or YYYY-MM-DD */
          for (l=0; l<3 && (pstr = strtok(l?NULL:str,"/- ")); l++)
            dpar[l] = atoi(pstr);
          if (l<3 || !dpar[0] || !dpar[1] || !dpar[2])
            {
/*---------- If DATE-OBS value corrupted or incomplete, assume 2000-1-1 */
            warning("Invalid DATE-OBS value in header: ", str);
            dpar[0] = 2000; dpar[1] = 1; dpar[2] = 1;
            }
          else if (strchr(str, '/') && dpar[0]<32 && dpar[2]<100)
            {
            j = dpar[0];
            dpar[0] = dpar[2]+1900;
            dpar[2] = j;
            }

          biss = (dpar[0]%4)?0:1;
/*-------- Convert date to MJD */
          date = -678956 + (365*dpar[0]+dpar[0]/4) - biss
		+ ((dpar[1]>2?((int)((dpar[1]+1)*30.6)-63+biss)
			:((dpar[1]-1)*(63+biss))/2) + dpar[2]);
          as->equinox = 2000.0 - (MJD2000 - date)/365.25;
          }
        else
/*-------- Well if really no date is found */
          as->equinox = 2000.0;
        }
      as->equinox = FITSTOF("EQUINOX ", FITSTOF("EPOCH  ", as->equinox));
      FITSTOS("RADECSYS", str, as->equinox<1984.0?"FK4":"FK5");
      if (!strcmp(str, "FK5"))
        as->radecsys = RDSYS_FK5;
      else if (!strcmp(str, "FK4"))
        {
        if (as->equinox == 2000.0)
          as->equinox = FITSTOF("EQUINOX ", FITSTOF("EPOCH  ", 1950.0));
        as->radecsys = RDSYS_FK4;
        warning("FK4 precession formulae not yet implemented:\n",
		"            Astrometry may be slightly inaccurate");
        }
      else if (!strcmp(str, "FK4-NO-E"))
        {
        if (as->equinox == 2000.0)
          as->equinox = FITSTOF("EQUINOX ", FITSTOF("EPOCH  ", 1950.0));
        as->radecsys = RDSYS_FK4_NO_E;
        warning("FK4 precession formulae not yet implemented:\n",
		"            Astrometry may be slightly inaccurate");
        }
      else if (!strcmp(str, "GAPPT"))
        {
        as->radecsys = RDSYS_GAPPT;
        warning("GAPPT reference frame not yet implemented:\n",
		"            Astrometry may be slightly inaccurate");
        }
      else
        {
        warning("Using FK5 instead of unknown astrometric reference frame: ",
		str);
        as->radecsys = RDSYS_FK5;
        }

/*---- Projection parameters */
      as->longpole = FITSTOF("LONGPOLE", 999.0);
      as->latpole = FITSTOF("LATPOLE ", 999.0);
/*      if (fitsnfind(buf, "PROJP1  ", n))*/
      if ( astClear( fitschan, "Card" ),
           astFindFits( fitschan, "PROJP1", NULL, 0) )
        for (l=0; l<10; l++)
          {
          sprintf(str, "PROJP%d", l);
          as->projp[l] = FITSTOF(str, 0.0);
          }
      }
    else
      {
/*---- No need to keep memory allocated for a useless WCS structure */
      free(as->wcs);
      as->wcs_flag = 0;
      }

/*---------------------------------------------------------------------------*/

      } /* end if fitchan found there */


  } /* end if MEASURE or DETECT field */

/*---------------------------------------------------------------------------*/
  
/*  field->fitshead = NULL;
  field->fitsheadsize = n*FBSIZE;
*/

/*- Map the NDF in the appropriate type */
    if (field->flags ^ FLAG_FIELD)
      ndfMap( field->ndf, "DATA", "_REAL", "READ", pntr, &nel, &status );
    else
      ndfMap( field->ndf, "DATA", "_REAL", "READ", pntr, &nel, &status );
    field->map = pntr[0];
    field->file = 0;
    field->nel = nel;

  return;
  }

/******************************* loadstrip ***********************************/
/*
Load a new strip of pixel data into the buffer.
*/
void	*loadstrip(picstruct *field, picstruct *wfield)

  {
   checkstruct	*check;
   int		y, w, flags, interpflag;
   PIXTYPE	*data, *wdata, *rmsdata, *checklinebuf;

  w = field->width;
  flags = field->flags;
  interpflag = (wfield && wfield->interp_flag);

  if (!field->y)
    {
/*- First strip */
     int	nbpix;

    nbpix = w*field->stripheight;

    if (flags ^ FLAG_FIELD)
      {

      data = field->strip;
/*---- We assume weight data have been read just before */
      if (interpflag)
        wdata = wfield->strip;
      if (flags & BACKRMS_FIELD)
        for (y=0, rmsdata=data; y<field->stripheight; y++, rmsdata += w)
          backrmsline(field, y, rmsdata);
      else if (flags & INTERP_FIELD)
        copydata(field, 0, nbpix);
      else
        readdata(field, data, nbpix);
      if (flags & WEIGHT_FIELD)
        weight_to_var(field, data, nbpix);
      else if (flags & (RMS_FIELD|BACKRMS_FIELD))
        rms_to_var(field, data, nbpix);

      if ((flags & MEASURE_FIELD) && (check=prefs.check[CHECK_IDENTICAL]))
        writecheck(check, data, nbpix);
      for (y=0; y<field->stripheight; y++, data += w)
        {
/*------ This is the only place where one can pick-up safely the current bkg */
        if (flags & (MEASURE_FIELD|DETECT_FIELD))
          subbackline(field, y, data);
/*------ Go to interpolation process */
        if (interpflag)
          {
          interpolate(field,wfield, data, wdata);
          wdata += w;
          }
/*------ Check-image stuff */
        if (prefs.check_flag)
          {
          if (flags & MEASURE_FIELD)
            {
            if (check = prefs.check[CHECK_BACKGROUND])
              writecheck(check, field->backline, w);
            if (check = prefs.check[CHECK_SUBTRACTED])
              writecheck(check, data, w);
            if (check = prefs.check[CHECK_APERTURES])
              writecheck(check, data, w);
            if (check = prefs.check[CHECK_SUBPROTOS])
              writecheck(check, data, w);
            }
          else if ((check = prefs.check[CHECK_BACKRMS])
		&& (flags & (WEIGHT_FIELD|VAR_FIELD|RMS_FIELD|BACKRMS_FIELD)))
            writecheck(check, data, w);
          }
        }
      }
    else
      readidata(field, field->fstrip, nbpix);

    field->ymax = field->stripheight;
    if (field->ymax < field->height)
      field->stripysclim = field->stripheight - field->stripmargin;
    }
  else
    {
/*- other strips */
    if (flags ^ FLAG_FIELD)
      {
      data = field->strip + field->stripylim*w;
/*---- We assume weight data have been read just before */
      if (interpflag)
        wdata = wfield->strip + field->stripylim*w;

/*---- copy to Check-image the "oldest" line before it is replaced */
      if ((flags & MEASURE_FIELD) && (check=prefs.check[CHECK_SUBOBJECTS]))
        writecheck(check, data, w);

      if (flags & BACKRMS_FIELD)
        backrmsline(field, field->ymax, data);
      else if (flags & INTERP_FIELD)
        copydata(field, field->stripylim*w, w);
      else
        readdata(field, data, w);

      if (flags & WEIGHT_FIELD)
        weight_to_var(field, data, w);
      else if (flags & (RMS_FIELD|BACKRMS_FIELD))
        rms_to_var(field, data, w);

      if ((flags & MEASURE_FIELD) && (check=prefs.check[CHECK_IDENTICAL]))
        writecheck(check, data, w);
/*---- Interpolate and subtract the background at current line */
      if (flags & (MEASURE_FIELD|DETECT_FIELD))
        subbackline(field, field->ymax, data);
      if (interpflag)
        interpolate(field,wfield, data, wdata);
/*---- Check-image stuff */
      if (prefs.check_flag)
        {
        if (flags & MEASURE_FIELD)
          {
          if (check = prefs.check[CHECK_BACKGROUND])
            writecheck(check, field->backline, w);
          if (check = prefs.check[CHECK_SUBTRACTED])
            writecheck(check, data, w);
          if (check = prefs.check[CHECK_APERTURES])
            writecheck(check, data, w);
          if (check = prefs.check[CHECK_SUBPROTOS])
            writecheck(check, data, w);
          }
        else if ((check = prefs.check[CHECK_BACKRMS])
		&& (flags & (WEIGHT_FIELD|VAR_FIELD|RMS_FIELD|BACKRMS_FIELD)))
          writecheck(check, data, w);
        }
      }
    else
      readidata(field, field->fstrip + field->stripylim*w, w);

    field->stripylim = (++field->ymin)%field->stripheight;
    if ((++field->ymax)<field->height)
      field->stripysclim = (++field->stripysclim)%field->stripheight;
    }

  return (flags ^ FLAG_FIELD)?
		  (void *)(field->strip + field->stripy*w)
		: (void *)(field->fstrip + field->stripy*w);
  }


/******************************** copydata **********************************/
/*
Copy image data from one field to the other.
*/
void	copydata(picstruct *field, int offset, int size)
  {
  memcpy(field->strip+offset, field->copystrip+offset, size*sizeof(PIXTYPE));

  return;
  }


/******************************** readdata **********************************/
/*
read and convert input data stream in PIXTYPE (float) format.
  field    is pointer to the image picstruct
    field->map     pointer to mapped data
    field->file    number of next pixel to be read (from 0)
    field->nel     number of pixels in image
  ptr      is pointer to the strip buffer
  size     is the number of pixels to read
*/
void	readdata(picstruct *field, PIXTYPE *ptr, int size)
  {
  int		i,left;
  PIXTYPE	bs,bz;

  bs = (PIXTYPE)field->bscale;
  bz = (PIXTYPE)field->bzero;

  left = field->nel - field->file;
  for (i=0; i<(size<left?size:left); i++) 
     *(ptr++) = *((PIXTYPE *)field->map + field->file++)*bs + bz;

/* Reset field->file if at end */
/* This prevents endfield from crashing if field->file is used for NDF */
  field->file = (field->file >= field->nel)?0:field->file;
  return;
  }


/******************************** readidata **********************************/
/*
read and convert input data stream in FLAGTYPE (unsigned int) format.
  field    is pointer to the image picstruct
    field->file   number of next pixel to be read (from 0)
    field->nel    number of pixels in image
  ptr      is pointer to the strip buffer
  size     is the number of pixels to read
*/
void	readidata(picstruct *field, FLAGTYPE *ptr, int size)
  {
  int		i, left;

  left = field->nel - field->file;
  for (i=0; i<(size<left?size:left); i++) 
     *(ptr++) = *((FLAGTYPE *)field->map + field->file++);

/* Reset field->file if at end */
/* This prevents endfield from crashing if field->file is used for NDF */
  field->file = (field->file >= field->nel)?0:field->file;
  return;
  }
