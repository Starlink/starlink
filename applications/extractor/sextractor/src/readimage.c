/*
 				readimage.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	functions for input of image data.
*
*	Last modify:	28/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"wcs/wcs.h"
#include	"define.h"
#include	"globals.h"
#include	"check.h"
#include	"field.h"
#include	"fitscat.h"
#include	"interpolate.h"
#include	"back.h"
#include	"astrom.h"
#include	"weight.h"

/******************************* loadstrip ***********************************/
/*
Load a new strip of pixel data into the buffer.
*/
void	*loadstrip(picstruct *field, picstruct *wfield)

  {
   checkstruct	*check;
   int		y, w, flags, interpflag;
   PIXTYPE	*data, *wdata, *rmsdata;

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
            if (check = prefs.check[CHECK_SUBPSFPROTOS])
              writecheck(check, data, w);
            if (check = prefs.check[CHECK_SUBPCPROTOS])
              writecheck(check, data, w);
            if (check = prefs.check[CHECK_PCOPROTOS])
              writecheck(check, data, w);
            }
          else if ((flags&DETECT_FIELD) && (check=prefs.check[CHECK_BACKRMS]))
            {
            backrmsline(field, y, (PIXTYPE *)check->pix);
            writecheck(check, check->pix, w);
            }
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
          if (check = prefs.check[CHECK_SUBPSFPROTOS])
            writecheck(check, data, w);
          if (check = prefs.check[CHECK_SUBPCPROTOS])
            writecheck(check, data, w);
          if (check = prefs.check[CHECK_PCOPROTOS])
            writecheck(check, data, w);
          }
        else if ((flags&DETECT_FIELD) && (check=prefs.check[CHECK_BACKRMS]))
          {
          backrmsline(field, y, (PIXTYPE *)check->pix);
          writecheck(check, check->pix, w);
          }
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
*/
void	readdata(picstruct *field, PIXTYPE *ptr, int size)
  {
  static char	bufdata0[DATA_BUFSIZE];
  char		*bufdata;
  short		val16;
  int		i, bowl, spoonful, npix, curval, dval;
  PIXTYPE	bs,bz;

  bs = (PIXTYPE)field->bscale;
  bz = (PIXTYPE)field->bzero;
  switch(field->compress_type)
    {
/*-- Uncompressed image */
    case COMPRESS_NONE:
      bowl = DATA_BUFSIZE/field->bytepix;
      spoonful = size<bowl?size:bowl;
      for(; size>0; size -= spoonful)
        {
        if (spoonful>size)
          spoonful = size;
        bufdata = bufdata0;
        QFREAD(bufdata, spoonful*field->bytepix, field->file,field->filename);
        switch(field->bitpix)
          {
          case BP_BYTE:
            if (field->bitsgn)
              for (i=spoonful; i--;)
                *(ptr++) = *(bufdata++)*bs + bz;
            else
              for (i=spoonful; i--;)
                *(ptr++) = *((unsigned char *)bufdata++)*bs + bz;
            break;

          case BP_SHORT:
#          ifdef BSWAP
            swapbytes(bufdata, 2, spoonful);
#          endif
            if (field->bitsgn)
              for (i=spoonful; i--; bufdata += sizeof(short))
                *(ptr++) = *((short *)bufdata)*bs + bz;
            else
              for (i=spoonful; i--; bufdata += sizeof(USHORT))
                *(ptr++) = *((USHORT *)bufdata)*bs + bz;
            break;

          case BP_LONG:
#          ifdef BSWAP
            swapbytes(bufdata, 4, spoonful);
#          endif
            if (field->bitsgn)
              for (i=spoonful; i--; bufdata += sizeof(LONG))
                *(ptr++) = *((LONG *)bufdata)*bs + bz;
            else
              for (i=spoonful; i--; bufdata += sizeof(ULONG))
                *(ptr++) = *((ULONG *)bufdata)*bs + bz;
              break;

          case BP_FLOAT:
#          ifdef BSWAP
            swapbytes(bufdata, 4, spoonful);
#          endif
            for (i=spoonful; i--; bufdata += sizeof(float))
              *(ptr++) = *((float *)bufdata)*bs + bz;
            break;

          case BP_DOUBLE:
#          ifdef BSWAP
            swapbytes(bufdata, 8, spoonful);
#          endif
            for (i=spoonful; i--; bufdata += sizeof(double))
              *(ptr++) = *((double *)bufdata)*bs + bz;
            break;

          default:
            error(EXIT_FAILURE,"*FATAL ERROR*: unknown BITPIX type in ",
				"readdata()");
            break;
          }
        }
      break;

/*-- Compressed image */
    case COMPRESS_BASEBYTE:
      bufdata = field->compress_bufptr;
      curval = field->compress_curval;
      npix = field->compress_npix;
      while (size--)
        {
        if (!(npix--))
          {
          if (curval != field->compress_checkval)
            error(EXIT_FAILURE, "*Error*: invalid BASEBYTE checksum in ",
		field->filename);
          bufdata = field->compress_buf;
          QFREAD(bufdata, FBSIZE, field->file, field->filename);
          curval = 0;
#        ifdef BSWAP
          swapbytes(bufdata, 4, 1);
#        endif
          field->compress_checkval = *((int *)bufdata);
         bufdata += 4;
#        ifdef BSWAP
          swapbytes(bufdata, 2, 1);
#        endif
          npix = (int)(*((short *)bufdata))-1;
          bufdata+=2;
          }
        if ((dval=(int)*(bufdata++))==-128)
          {
#        ifdef BSWAP
          swapbytes(bufdata, 2, 1);
#        endif
          memcpy(&val16, bufdata, 2);
          dval = (int)val16;
          if (dval==-32768)
            {
            bufdata += 2;
#          ifdef BSWAP
            swapbytes(bufdata, 4, 1);
#          endif
            memcpy(&dval,bufdata,4);
            bufdata += 4;
            }
          else
            bufdata += 2;
          }
        *(ptr++) = dval*bs + bz;
        curval += dval;
        }
      field->compress_curval = curval;
      field->compress_bufptr = bufdata;
      field->compress_npix = npix;
      break;

    case COMPRESS_PREVPIX:
      bufdata = field->compress_bufptr;
      curval = field->compress_curval;
      npix = field->compress_npix;
      while (size--)
        {
        if (!(npix--))
          {
          if (curval != field->compress_checkval)
            error(EXIT_FAILURE, "*Error*: invalid PREV_PIX checksum in ",
		field->filename);
          bufdata = field->compress_buf;
          QFREAD(bufdata, FBSIZE, field->file, field->filename);
#        ifdef BSWAP
          swapbytes(bufdata, 2, 3);
#        endif
          curval = (int)*(short *)bufdata;
          npix = (int)*(short *)(bufdata+=2)-1;
          field->compress_checkval = (int)(*(short *)(bufdata+=2));
          bufdata+=4;
          }
        if ((dval=(int)*(bufdata++))==-128)
          {
#        ifdef BSWAP
          swapbytes(bufdata, 2, 1);
#        endif
          memcpy(&val16, bufdata, 2);
          curval = (int)val16;
          bufdata += 2;
          }
        else
          curval += dval;
        *(ptr++) = curval*bs + bz;
        }
      field->compress_curval = curval;
      field->compress_bufptr = bufdata;
      field->compress_npix = npix;
      break;

    default:
      error(EXIT_FAILURE,"*Internal Error*: unknown compression mode in ",
				"readdata()");
    }

  return;
  }


/******************************** readidata *********************************/
/*
read and convert input data stream in FLAGTYPE (unsigned int) format.
*/
void	readidata(picstruct *field, FLAGTYPE *ptr, int size)
  {
  static char	bufdata0[DATA_BUFSIZE];
  char		*bufdata;
  short		val16;
  int		i, bowl, spoonful, npix, curval, dval;

  switch(field->compress_type)
    {
/*-- Uncompressed image */
    case COMPRESS_NONE:
      bowl = DATA_BUFSIZE/field->bytepix;
      spoonful = size<bowl?size:bowl;
      for(; size>0; size -= spoonful)
        {
        if (spoonful>size)
          spoonful = size;
        bufdata = bufdata0;
        QFREAD(bufdata, spoonful*field->bytepix, field->file, field->filename);
        switch(field->bitpix)
          {
          case BP_BYTE:
            for (i=spoonful; i--;)
              *(ptr++) = (FLAGTYPE)*((unsigned char *)bufdata++);
            break;

          case BP_SHORT:
#          ifdef BSWAP
            swapbytes(bufdata, 2, spoonful);
#          endif
            for (i=spoonful; i--; bufdata += sizeof(USHORT))
              *(ptr++) = (FLAGTYPE)*((USHORT *)bufdata);
            break;

          case BP_LONG:
#          ifdef BSWAP
            swapbytes(bufdata, 4, spoonful);
#          endif
            for (i=spoonful; i--; bufdata += sizeof(ULONG))
              *(ptr++) = (FLAGTYPE)*((ULONG *)bufdata);
            break;

          case BP_FLOAT:
          case BP_DOUBLE:
            error(EXIT_FAILURE,"*Error*: I was expecting integers in ",
				field->filename);
            break;
          default:
            error(EXIT_FAILURE,"*FATAL ERROR*: unknown BITPIX type in ",
				"readdata()");
            break;
          }
        }
      break;

/*-- Compressed image */
    case COMPRESS_BASEBYTE:
      bufdata = field->compress_bufptr;
      curval = field->compress_curval;
      npix = field->compress_npix;
      while (size--)
        {
        if (!(npix--))
          {
          if (curval != field->compress_checkval)
            error(EXIT_FAILURE, "*Error*: invalid BASEBYTE checksum in ",
		field->filename);
          bufdata = field->compress_buf;
          QFREAD(bufdata, FBSIZE, field->file, field->filename);
          curval = 0;
#        ifdef BSWAP
          swapbytes(bufdata, 4, 1);
#        endif
          field->compress_checkval = *((int *)bufdata);
         bufdata += 4;
#        ifdef BSWAP
          swapbytes(bufdata, 2, 1);
#        endif
          npix = (int)(*((short *)bufdata))-1;
          bufdata+=2;
          }
        if ((dval=(int)*(bufdata++))==-128)
          {
#        ifdef BSWAP
          swapbytes(bufdata, 2, 1);
#        endif
          memcpy(&val16, bufdata, 2);
          dval = (int)val16;
          if (dval==-32768)
            {
            bufdata += 2;
#          ifdef BSWAP
            swapbytes(bufdata, 4, 1);
#          endif
            memcpy(&dval,bufdata,4);
            bufdata += 4;
            }
          else
            bufdata += 2;
          }
        *(ptr++) = (FLAGTYPE)dval;
        curval += dval;
        }
      field->compress_curval = curval;
      field->compress_bufptr = bufdata;
      field->compress_npix = npix;
      break;

    case COMPRESS_PREVPIX:
      bufdata = field->compress_bufptr;
      curval = field->compress_curval;
      npix = field->compress_npix;
      while (size--)
        {
        if (!(npix--))
          {
          if (curval != field->compress_checkval)
            error(EXIT_FAILURE, "*Error*: invalid PREV_PIX checksum in ",
		field->filename);
          bufdata = field->compress_buf;
          QFREAD(bufdata, FBSIZE, field->file, field->filename);
#        ifdef BSWAP
          swapbytes(bufdata, 2, 3);
#        endif
          curval = (int)*(short *)bufdata;
          npix = (int)*(short *)(bufdata+=2)-1;
          field->compress_checkval = (int)(*(short *)(bufdata+=2));
          bufdata+=4;
          }
        if ((dval=(int)*(bufdata++))==-128)
          {
#        ifdef BSWAP
          swapbytes(bufdata, 2, 1);
#        endif
          memcpy(&val16, bufdata, 2);
          curval = (int)val16;
          bufdata += 2;
          }
        else
          curval += dval;
        *(ptr++) = (FLAGTYPE)curval;
        }
      field->compress_curval = curval;
      field->compress_bufptr = bufdata;
      field->compress_npix = npix;
      break;

    default:
      error(EXIT_FAILURE,"*Internal Error*: unknown compression mode in ",
				"readdata()");
    }

  return;
  }


/******************************* readimagehead *******************************/
/*
extract some data from the FITS-file header
*/
void	readimagehead(picstruct *field)
  {
   int		j,l, n;
   char		*buf, st[80], str[80], *point;

/* Open the file */
  if (!(field->file = fopen(field->filename, "rb")))
    error(EXIT_FAILURE,"*Error*: cannot open ", field->filename);
  buf = readfitshead(field->file, field->filename, &n);
  if(FITSTOI("NAXIS   ", 0) < 2)
    error(EXIT_FAILURE, field->filename, " does NOT contain 2D-data!");

/*---------------------------- Basic keywords ------------------------------*/
  field->bitpix = FITSTOI("BITPIX  ", 0);
  if (field->bitpix != BP_BYTE
	&& field->bitpix != BP_SHORT
	&& field->bitpix != BP_LONG
	&& field->bitpix != BP_FLOAT
	&& field->bitpix != BP_DOUBLE)
    error(EXIT_FAILURE, "Sorry, I don't know that kind of data.", "");

  field->bytepix = (field->bitpix>0?field->bitpix:-field->bitpix)>>3;
  field->width = FITSTOI("NAXIS1  ", 0);
  field->height = FITSTOI("NAXIS2  ", 0);
  field->npix = (size_t)field->width*field->height;
  field->bscale = FITSTOF("BSCALE  ", 1.0);

  field->bzero = FITSTOF("BZERO   ", 0.0);
  field->bitsgn = FITSTOI("BITSGN  ", 1);
  if (field->bitsgn && prefs.fitsunsigned_flag)
    field->bitsgn = 0;

  FITSTOS("OBJECT  ", field->ident, "Unnamed");

/*----------------------------- Compression --------------------------------*/
  if (fitsread(buf, "IMAGECOD", st, H_STRING, T_STRING)==RETURN_OK)
    {
    if (!strcmp(st, "NONE"))
      field->compress_type = COMPRESS_NONE;
    else if (!strcmp(st, "BASEBYTE"))
      field->compress_type = COMPRESS_BASEBYTE;
    else if (!strcmp(st, "PREV_PIX"))
      field->compress_type = COMPRESS_PREVPIX;
    else
      warning("Compression skipped: unknown IMAGECOD parameter:", st);
    }

/*----------------------------- Astrometry ---------------------------------*/
/* Presently, astrometry is done only on the measurement and detect images */
  if (field->flags&(MEASURE_FIELD|DETECT_FIELD))
    {
     astromstruct	*as;
     double		drota, s;
     int		naxis;

    QCALLOC(as, astromstruct, 1);
    field->astrom = as;

    naxis = as->naxis = 2;
    for (l=0; l<naxis; l++)
      {
      sprintf(str, "CTYPE%-3d", l+1);
      FITSTOS(str, str, "");
      strncpy(as->ctype[l], str, 8);
      sprintf(str, "CUNIT%-3d", l+1);
      FITSTOS(str, str, "deg");
      strncpy(as->cunit[l], str, 32);
      sprintf(str, "CRVAL%-3d", l+1);
      as->crval[l] = FITSTOF(str, 0.0);
      sprintf(str, "CRPIX%-3d", l+1);
      as->crpix[l] = FITSTOF(str, 1.0);
      sprintf(str, "CDELT%-3d", l+1);
      as->cdelt[l] = FITSTOF(str, 1.0);
      if (fabs(as->cdelt[l]) < 1/BIG)
        error(EXIT_FAILURE, "*Error*: CDELT parameters out of range in ", 
		field->filename);
      }
    if (fitsnfind(buf, "CD1_1", n))
      {
/*---- If CD keywords exist, use them for the linear mapping terms... */

      for (l=0; l<naxis; l++)
        for (j=0; j<naxis; j++)
          {
          sprintf(str, "CD%d_%d", l+1, j+1);
          as->pc[l*naxis+j] = FITSTOF(str, l==j?1.0:0.0)/as->cdelt[j];
          }
      }
    else if (fitsnfind(buf, "PC001001", n))
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
    if (prefs.world_flag
	&& !wcsset(as->naxis,(const char(*)[9])as->ctype, as->wcs)
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
      if (fitsnfind(buf, "PROJP1  ", n))
        for (l=0; l<10; l++)
          {
          sprintf(str, "PROJP%-3d", l);
          as->projp[l] = FITSTOF(str, 0.0);
          }
      }
    else
      {
/*---- No need to keep memory allocated for a useless WCS structure */
      free(as->wcs);
      as->wcs_flag = 0;
      }
    }

/*---------------------------------------------------------------------------*/

  field->fitshead = buf;
  field->fitsheadsize = n*FBSIZE;

  return;
  }


/******************************* readfitshead ********************************/
/*
read data from the FITS-file header
*/
char    *readfitshead(FILE *file, char *filename, int *nblock)

  {
   int     n;
   char    *buf;

  if (!(buf=(char *)malloc((size_t)FBSIZE)))
    error(EXIT_FAILURE, "*Error*: Not enough memory in ", "readfitshead()");

/* Find the number of FITS blocks of the header while reading it */
  QFREAD(buf, FBSIZE, file, filename);

  if (strncmp(buf, "SIMPLE  ", 8))
    {
/* Ugly but necessary patch to handle this stupid DeNIS compressed format! */
    if (strncmp(buf, "XTENSION", 8))
      error(EXIT_FAILURE, filename, " is NOT a FITS file!");
    else
      {
      memset(buf, ' ', 80);
      strncpy(buf,
	"SIMPLE  =                    T / Decompressed by SExtractor", 59);
      }
    }

  for (n=1; !fitsnfind(buf,"END     ", n); n++)
    {
    if (!(buf=(char *)realloc(buf, (size_t)(FBSIZE*(n+1)))))
      error(EXIT_FAILURE, "*Error*: Not enough memory in ", "readfitshead()");
    QFREAD(buf+n*FBSIZE, FBSIZE, file, filename);
    }

  *nblock = n;
  return  buf;
  }


