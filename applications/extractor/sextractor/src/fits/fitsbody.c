/*
 				fitsbody.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:       Handle memory allocation for FITS bodies.
*
*	Last modify:	03/02/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>
#include	<sys/types.h>

#ifdef	HAVE_SYS_MMAN_H
#include	<sys/mman.h>
#endif
#include	"fitscat_defs.h"
#include	"fitscat.h"

size_t	body_maxram = BODY_DEFRAM,
	body_maxvram = BODY_DEFVRAM,
	body_ramleft, body_vramleft, body_ramflag;

int	body_vmnumber;

double	bufdata0[DATA_BUFSIZE/sizeof(double)];
char	body_swapdirname[MAXCHARS] = BODY_DEFSWAPDIR;

/******* alloc_body ***********************************************************
PROTO	PIXTYPE *alloc_body(tabstruct *tab,
		void (*func)(PIXTYPE *ptr, int npix))
PURPOSE	Allocate memory for and read a FITS data body (read-only). If not
	enough RAM is available, a swap file is created.
INPUT	Table (tab) structure.
OUTPUT	Pointer to the mapped data if OK, or NULL otherwise.
NOTES	The file pointer must be positioned at the beginning of the data.
AUTHOR	E. Bertin (IAP)
VERSION	05/05/2001
 ***/
PIXTYPE	*alloc_body(tabstruct *tab, void (*func)(PIXTYPE *ptr, int npix))
  {
   FILE		*file;
   PIXTYPE	*buffer;
   size_t	npix, size, sizeleft, spoonful;

  if (!body_ramflag)
    {
    body_ramleft = body_maxram;
    body_vramleft = body_maxvram;
    body_ramflag = 1;
    }

/* Return a NULL pointer if size is zero */
  if (!tab->tabsize)
    return (PIXTYPE *)NULL;

/* Check that there is a cat parent structure and that the file is open */
   if (tab->cat && !tab->cat->file)
     error(EXIT_FAILURE, "*Internal Error*: Cannot access table: ",
			tab->extname);

/* Decide if the data will go in physical memory or on swap-space */
  npix = tab->tabsize/tab->bytepix;
  size = npix*sizeof(PIXTYPE);
  if (size < body_ramleft)
    {
/*-- There should be enough RAM left: try to do a malloc() */
    if (tab->bodybuf = malloc(size))
      {
      QFSEEK(tab->cat->file, tab->bodypos, SEEK_SET, tab->cat->filename);
      read_body(tab, (PIXTYPE *)tab->bodybuf, npix);
/*---- Apply pixel processing */
      if (func)
        (*func)((PIXTYPE *)tab->bodybuf, npix);
      body_ramleft -= size;

      return (PIXTYPE *)tab->bodybuf;
      }
    else
      tab->bodybuf = NULL;
    }

  if (size < body_vramleft)
    {
/*-- Convert and copy the data to a swap file, and mmap() it */
    if (!(buffer = malloc(DATA_BUFSIZE)))
      return NULL;
    sprintf(tab->swapname, "%s/vm%05ld_%05x.tmp",
		body_swapdirname, (long)getpid(),
		(unsigned int)++body_vmnumber) ;
    if (!(file=fopen(tab->swapname, "wb+")))
      error(EXIT_FAILURE, "*Error*: cannot create swap-file ", tab->swapname);
    add_cleanupfilename(tab->swapname);
    spoonful = (size%DATA_BUFSIZE);
    if (!spoonful)
      spoonful = DATA_BUFSIZE;
    QFSEEK(tab->cat->file, tab->bodypos, SEEK_SET, tab->cat->filename);
    read_body(tab, buffer, spoonful/sizeof(PIXTYPE));
/*-- Apply pixel processing */
    if (func)
      (*func)(buffer, spoonful/sizeof(PIXTYPE));
    QFWRITE(buffer, spoonful, file, tab->swapname);
    for (sizeleft = size; sizeleft -= spoonful;)
      {
      read_body(tab, buffer, (spoonful=DATA_BUFSIZE)/sizeof(PIXTYPE));
/*--- Apply pixel processing */
      if (func)
        (*func)(buffer, spoonful/sizeof(PIXTYPE));
      QFWRITE(buffer, spoonful, file, tab->swapname);
      }
    free(buffer);
    tab->bodybuf = mmap(NULL,size,PROT_READ,MAP_SHARED,fileno(file),(off_t)0);
    fclose(file);
    tab->swapflag = 1;
    body_vramleft -= size;

/*-- Memory mapping problem */
    if (tab->bodybuf == (void *)-1)
      return NULL;
    return (PIXTYPE *)tab->bodybuf;
    }

/* If no memory left at all: forget it! */
  return NULL;
  }


/******* free_body ************************************************************
PROTO	void free_body(tabstruct *tab)
PURPOSE	Free FITS body data.
INPUT	Tab structure.
OUTPUT	-.
NOTES	.
AUTHOR	E. Bertin (IAP)
VERSION	04/03/2000
 ***/
void	free_body(tabstruct *tab)

  {
   size_t	size;

/* Free the body! (if allocated) */
  if (tab->bodybuf)
    {
    size = (tab->tabsize/tab->bytepix)*sizeof(PIXTYPE);
    if (tab->swapflag)
      {
      if (munmap(tab->bodybuf, size))
        warning("Can't unmap ", tab->cat->filename);
      tab->swapflag = 0;
      tab->bodybuf = NULL;
      body_vramleft += size;
      if (unlink(tab->swapname))
        warning("Can't delete ", tab->swapname);
      remove_cleanupfilename(tab->swapname);
      *tab->swapname = '\0';
      }
    else
      {
      QFREE(tab->bodybuf);
      body_ramleft += size;
      }
    }

/* Free the decompression buffer if allocated */
  if (tab->compress_buf)
    QFREE(tab->compress_buf);

  return;
  }


/******* read_body ************************************************************
PROTO	read_body(tabstruct *tab, PIXTYPE *ptr, long size)
PURPOSE	Read values from the body of a FITS table.
INPUT	A pointer to the tab structure,
	a pointer to the array in memory,
	the number of elements to be read.
OUTPUT	-.
NOTES	.
AUTHOR	E. Bertin (IAP)
VERSION	03/02/2003
 ***/
void	read_body(tabstruct *tab, PIXTYPE *ptr, size_t size)
  {
  catstruct	*cat;
  char		*bufdata;
  short		val16;
  int		curval, dval;
  size_t	i, bowl, spoonful, npix;
  PIXTYPE	bs,bz;

  bs = (PIXTYPE)tab->bscale;
  bz = (PIXTYPE)tab->bzero;
/* a NULL cat structure indicates that no data can be read */
  if (!(cat = tab->cat))
    return;

  switch(tab->compress_type)
    {
/*-- Uncompressed image */
    case COMPRESS_NONE:
      bowl = DATA_BUFSIZE/tab->bytepix;
      spoonful = size<bowl?size:bowl;
      for(; size>0; size -= spoonful)
        {
        if (spoonful>size)
          spoonful = size;
        bufdata = (char *)bufdata0;
        QFREAD(bufdata, spoonful*tab->bytepix, cat->file, cat->filename);
        switch(tab->bitpix)
          {
          case BP_BYTE:
            if (tab->bitsgn)
              for (i=spoonful; i--;)
                *(ptr++) = *(bufdata++)*bs + bz;
            else
              for (i=spoonful; i--;)
                *(ptr++) = *((unsigned char *)bufdata++)*bs + bz;
            break;

          case BP_SHORT:
            if (bswapflag)
              swapbytes(bufdata, 2, spoonful);
            if (tab->bitsgn)
              for (i=spoonful; i--; bufdata += sizeof(short))
                *(ptr++) = *((short *)bufdata)*bs + bz;
            else
              for (i=spoonful; i--; bufdata += sizeof(unsigned short))
                *(ptr++) = *((unsigned short *)bufdata)*bs + bz;
            break;

          case BP_LONG:
            if (bswapflag)
              swapbytes(bufdata, 4, spoonful);
            if (tab->bitsgn)
              for (i=spoonful; i--; bufdata += sizeof(int))
                *(ptr++) = *((int *)bufdata)*bs + bz;
            else
              for (i=spoonful; i--; bufdata += sizeof(unsigned int))
                *(ptr++) = *((unsigned int *)bufdata)*bs + bz;
              break;

          case BP_FLOAT:
            if (bswapflag)
              swapbytes(bufdata, 4, spoonful);
            for (i=spoonful; i--; bufdata += sizeof(float))
              *(ptr++) = *((float *)bufdata)*bs + bz;
            break;

          case BP_DOUBLE:
            if (bswapflag)
              swapbytes(bufdata, 8, spoonful);
            for (i=spoonful; i--; bufdata += sizeof(double))
              *(ptr++) = *((double *)bufdata)*bs + bz;
            break;

          default:
            error(EXIT_FAILURE,"*FATAL ERROR*: unknown BITPIX type in ",
                                "read_body()");
            break;
          }
        }
      break;

/*-- Compressed image */
    case COMPRESS_BASEBYTE:
      if (!tab->compress_buf)
        QMALLOC(tab->compress_buf, char, FBSIZE);
      bufdata = tab->compress_bufptr;
      curval = tab->compress_curval;
      npix = tab->compress_npix;
      while (size--)
        {
        if (!(npix--))
          {
          if (curval != tab->compress_checkval)
            error(EXIT_FAILURE, "*Error*: invalid BASEBYTE checksum in ",
                cat->filename);
          bufdata = tab->compress_buf;
          QFREAD(bufdata, FBSIZE, cat->file, cat->filename);
          curval = 0;
          if (bswapflag)
            swapbytes(bufdata, 4, 1);
          tab->compress_checkval = *((int *)bufdata);
         bufdata += 4;
          if (bswapflag)
            swapbytes(bufdata, 2, 1);
          npix = (int)(*((short *)bufdata))-1;
          bufdata+=2;
          }
        if ((dval=(int)*(bufdata++))==-128)
          {
          if (bswapflag)
            swapbytes(bufdata, 2, 1);
          memcpy(&val16, bufdata, 2);
          dval = (int)val16;
          if (dval==-32768)
            {
            bufdata += 2;
            if (bswapflag)
              swapbytes(bufdata, 4, 1);
            memcpy(&dval,bufdata,4);
            bufdata += 4;
            }
          else
            bufdata += 2;
          }
        *(ptr++) = dval*bs + bz;
        curval += dval;
        }
      tab->compress_curval = curval;
      tab->compress_bufptr = bufdata;
      tab->compress_npix = npix;
      break;

    case COMPRESS_PREVPIX:
      if (!tab->compress_buf)
        QMALLOC(tab->compress_buf, char, FBSIZE);
      bufdata = tab->compress_bufptr;
      curval = tab->compress_curval;
      npix = tab->compress_npix;
      while (size--)
        {
        if (!(npix--))
          {
          if (curval != tab->compress_checkval)
            error(EXIT_FAILURE, "*Error*: invalid PREV_PIX checksum in ",
                tab->cat->filename);
          bufdata = tab->compress_buf;
          QFREAD(bufdata, FBSIZE, cat->file, cat->filename);
          if (bswapflag)
            swapbytes(bufdata, 2, 3);
          curval = (int)*(short *)bufdata;
          npix = (int)*(short *)(bufdata+=2)-1;
          tab->compress_checkval = (int)(*(short *)(bufdata+=2));
          bufdata+=4;
          }
        if ((dval=(int)*(bufdata++))==-128)
          {
          if (bswapflag)
            swapbytes(bufdata, 2, 1);
          memcpy(&val16, bufdata, 2);
          curval = (int)val16;
          bufdata += 2;
          }
        else
          curval += dval;
        *(ptr++) = curval*bs + bz;
        }
      tab->compress_curval = curval;
      tab->compress_bufptr = bufdata;
      tab->compress_npix = npix;
      break;

    default:
      error(EXIT_FAILURE,"*Internal Error*: unknown compression mode in ",
                                "read_body()");
    }

  return;
  }


/******* write_body ***********************************************************
PROTO	write_body(tabstruct *tab, PIXTYPE *ptr, long size)
PURPOSE	Write values to a FITS body.
INPUT	A pointer to the tab structure,
	a pointer to the array in memory,
	the number of elements to be written.
OUTPUT	-.
NOTES	.
AUTHOR	E. Bertin (IAP)
VERSION	28/02/2000
 ***/
void	write_body(tabstruct *tab, PIXTYPE *ptr, size_t size)
  {
  catstruct	*cat;
  size_t	i, bowl, spoonful;
  PIXTYPE	bs,bz;

  bs = (PIXTYPE)tab->bscale;
  bz = (PIXTYPE)tab->bzero;
  cat = tab->cat;
  if (!cat)
    error(EXIT_FAILURE, "*Internal Error*: no parent cat structure for table ",
		tab->extname);

  switch(tab->compress_type)
    {
/*-- Uncompressed image */
    case COMPRESS_NONE:
      bowl = DATA_BUFSIZE/tab->bytepix;
      spoonful = size<bowl?size:bowl;
      for(; size>0; size -= spoonful)
        {
        if (spoonful>size)
          spoonful = size;
        switch(tab->bitpix)
          {
          case BP_BYTE:
            if (tab->bitsgn)
              {
               char	*bufdata = (char *)bufdata0;
              for (i=spoonful; i--;)
                *(bufdata++) = (char)((*(ptr++)-bz)/bs+0.49999);
              }
            else
              {
               unsigned char	*bufdata = (unsigned char *)bufdata0;
              for (i=spoonful; i--;)
                *(bufdata++) = (unsigned char)((*(ptr++)-bz)/bs+0.49999);;
              }
            break;

          case BP_SHORT:
            if (tab->bitsgn)
              {
               short	*bufdata = (short *)bufdata0;
              for (i=spoonful; i--;)
                *(bufdata++) = (short)((*(ptr++)-bz)/bs+0.49999);
              }
            else
              {
               unsigned short	*bufdata = (unsigned short *)bufdata0;
              for (i=spoonful; i--;)
                *(bufdata++) = (unsigned short)((*(ptr++)-bz)/bs+0.49999);
              }
            if (bswapflag)
              swapbytes(bufdata0, 2, spoonful);
            break;

          case BP_LONG:
           if (tab->bitsgn)
              {
               int	*bufdata = (int *)bufdata0;
              for (i=spoonful; i--;)
                *(bufdata++) = (int)((*(ptr++)-bz)/bs+0.49999);
              }
            else
              {
               unsigned int	*bufdata = (unsigned int *)bufdata0;
              for (i=spoonful; i--;)
                *(bufdata++) = (unsigned int)((*(ptr++)-bz)/bs+0.49999);
              }
            if (bswapflag)
              swapbytes(bufdata0, 4, spoonful);
            break;

          case BP_FLOAT:
            {
             float	*bufdata = (float *)bufdata0;
            for (i=spoonful; i--;)
              *(bufdata++) = (*(ptr++)-bz)/bs;
            if (bswapflag)
              swapbytes(bufdata0, 4, spoonful);
            }
            break;

          case BP_DOUBLE:
            {
             double	*bufdata = (double *)bufdata0;
            for (i=spoonful; i--;)
              *(bufdata++) = (double)(*(ptr++)-bz)/bs;
            if (bswapflag)
              swapbytes(bufdata0, 8, spoonful);
            }
            break;

          default:
            error(EXIT_FAILURE,"*FATAL ERROR*: unknown BITPIX type in ",
                                "read_body()");
            break;
          }
        QFWRITE(bufdata0, spoonful*tab->bytepix, cat->file, cat->filename);
        }
      break;

/*-- Compressed image */
    case COMPRESS_BASEBYTE:
      break;

    case COMPRESS_PREVPIX:
      break;

    default:
      error(EXIT_FAILURE,"*Internal Error*: unknown compression mode in ",
                                "read_body()");
    }

  return;
  }


/******* set_maxram ***********************************************************
PROTO	int set_maxram(size_t maxram)
PURPOSE	Set the maximum amount of silicon memory that can be allocated for
	storing FITS body data.
INPUT	The maximum amount of RAM (in bytes).
OUTPUT	RETURN_OK if within limits, RETURN_ERROR otherwise.
NOTES	.
AUTHOR	E. Bertin (IAP)
VERSION	19/12/99
 ***/
int set_maxram(size_t maxram)
  {

  if (maxram<1)
    return RETURN_ERROR;

  body_maxram = maxram;

  return RETURN_OK;
  }


/******* set_maxvram **********************************************************
PROTO	int set_maxvram(size_t maxram)
PURPOSE	Set the maximum amount of total virtual memory that can be used for
	storing FITS body data.
INPUT	The maximum amount of VRAM (in bytes).
OUTPUT	RETURN_OK if within limits, RETURN_ERROR otherwise.
NOTES	.
AUTHOR	E. Bertin (IAP)
VERSION	08/02/2000
 ***/
int set_maxvram(size_t maxvram)
  {

  if (maxvram<1)
    return RETURN_ERROR;

  body_maxvram = maxvram;

  return RETURN_OK;
  }


/******* set_swapdir **********************************************************
PROTO	int set_swapdir(char *dirname)
PURPOSE	Set the path name of the directory that will be used for storing
	memory swap files.
INPUT	The pointer to the path string.
OUTPUT	RETURN_OK if path appropriate, RETURN_ERROR otherwise.
NOTES	.
AUTHOR	E. Bertin (IAP)
VERSION	19/12/99
 ***/
int set_swapdir(char *dirname)
  {

  if (!dirname)
    return RETURN_ERROR;

  strcpy(body_swapdirname, dirname);

  return RETURN_OK;
  }


