/*
 				fitsread.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	low-level functions for reading LDAC FITS catalogs.
*
*	Last modify:	29/06/2002
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef	HAVE_CONFIG_H
#include "config.h"
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"fitscat_defs.h"
#include	"fitscat.h"

char	*linein_buf, padbuf[FBSIZE];
int	linein_size, nlinein;

/****** read_cat ***************************************************************
PROTO	catstruct read_cat(char *filename)
PURPOSE	``Read'' a FITS catalog with name filename.
INPUT	Filename,
OUTPUT	catstruct pointer.
NOTES	Returns NULL if no file with name \<filename\> is found.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	07/05/2002
 ***/
catstruct	*read_cat(char *filename)

  {
   catstruct *cat;

  if (!(cat = new_cat(1)))
    error (EXIT_FAILURE, "Not enough memory to read ", filename);

  strcpy(cat->filename, filename);
  if (open_cat(cat, READ_ONLY) != RETURN_OK)
    {
    free_cat(&cat, 1);
    return NULL;
    }

  if (map_cat(cat) != RETURN_OK)
    {
    free_cat(&cat, 1);
    return NULL;
    }

  return cat;
  }


/****** read_cats **************************************************************
PROTO	read_cats(char **filenames, int ncat)
PURPOSE	``Read'' several FITS catalogs.
INPUT	A pointer to pointers of char,
	The number of catalogs.
OUTPUT	catstruct pointer.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
catstruct	*read_cats(char **filenames, int ncat)

  {
   catstruct	*cat, *ccat;
   int		i;

  if (!(cat = new_cat(ncat)))
    error (EXIT_FAILURE, "Not enough memory to read ", "catalogs");

  for (i=ncat, ccat = cat; i--; ccat++, filenames++)
    {
    strcpy(ccat->filename, *filenames);
    if (open_cat(ccat, READ_ONLY) != RETURN_OK)
      error (EXIT_FAILURE, "Cannot open ", *filenames);
    if (map_cat(ccat) != RETURN_OK)
      error (EXIT_FAILURE, "Cannot map ", *filenames);
    close_cat(ccat);
    }

  return cat;
  }


/****** init_readobj **********************************************************
PROTO	tabstruct *init_readobj(tabstruct *tab)
PURPOSE	Prepare the reading of individual sources in a FITS table
INPUT	Table structure.
OUTPUT	Pointer to the table structure from which the data will be read.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	07/04/97
 ***/
tabstruct	*init_readobj(tabstruct *tab)

  {
   catstruct	*tabcat;
   tabstruct	*keytab;
   keystruct	*key;
   int		k;

/* Scan keys to find the reference tab and other things*/
  keytab = NULL;
  key = tab->key;
  for (k=tab->nkey; k--; key = key->nextkey)
    if (!key->ptr)
      {
      keytab = key->tab;
      tabcat = keytab->cat;
      QMALLOC(key->ptr, char, key->nbytes);
      }
    else
      key->pos = -1;

  if (!keytab)
    error(EXIT_FAILURE,"*Error*: no original table found among keys in table ",
	tab->extname);

  if (open_cat(tabcat, READ_ONLY) != RETURN_OK)
    error(EXIT_FAILURE, "*Error*: Cannot access ", tabcat->filename);
  QFSEEK(tabcat->file, keytab->bodypos, SEEK_SET, tabcat->filename);

/* Allocate memory for the input buffer (or increase it if done already) */
  if (linein_buf)
    {
    if (linein_size < tab->naxisn[0])
      {
      QREALLOC(linein_buf, char, tab->naxisn[0]);
      linein_size = tab->naxisn[0];
      }
    }
  else
    {
    QMALLOC(linein_buf, char, tab->naxisn[0]);
    linein_size = tab->naxisn[0];
    }

  nlinein++;

  return keytab;
  }


/****** read_obj **************************************************************
PROTO	int read_obj(tabstruct *keytab, tabstruct *tab)
PURPOSE	Read one individual source at the current position in a FITS table.
INPUT	Table which will be accessed from disk (provided by init_readobj()),
	Table containing the keys that will be read.
OUTPUT	The number of table lines that remain to be read.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	18/02/2000
 ***/
int	read_obj(tabstruct *keytab, tabstruct *tab)

  {
   keystruct	*key;
   char		*pin, *pout;
   int		b,k;
   int		esize;

  QFREAD(linein_buf,keytab->naxisn[0],keytab->cat->file,keytab->cat->filename);
  key = tab->key;
  for (k=tab->nkey; k--; key = key->nextkey)
    if (key->pos>=0)
      {
      pin = linein_buf+key->pos;
      pout = key->ptr;
      if (bswapflag)
        {
        esize = t_size[key->ttype];
        swapbytes(pin, esize, key->nbytes/esize);
        }
      for (b=key->nbytes; b--;)
        *(pout++) = *(pin++);
      }

  return --keytab->naxisn[1];
  }


/****** read_obj_at ***********************************************************
PROTO	int read_obj_at(tabstruct *keytab, tabstruct *tab, long pos)
PURPOSE Get one source at a specific position in a FITS table.
INPUT	Table which will be accessed from disk (provided by init_readobj()),
	Table containing the keys that will be read.
	Position number in table.
OUTPUT	RETURN_OK if the object has been accessed, RETURN_ERROR otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	28/02/2000
 ***/
int	read_obj_at(tabstruct *keytab, tabstruct *tab, long pos)

  {
   keystruct	*key;
   char		*pin, *pout;
   size_t	n;
   int		b,k;
   int		esize;

  if ((n=keytab->naxisn[0]*pos) >= keytab->tabsize)
    return RETURN_ERROR;
  QFSEEK(keytab->cat->file,keytab->bodypos+n, SEEK_SET, keytab->cat->filename);
  QFREAD(linein_buf,keytab->naxisn[0],keytab->cat->file,keytab->cat->filename);
  key = tab->key;
  for (k=tab->nkey; k--; key = key->nextkey)
    if (key->pos>=0)
      {
      pin = linein_buf+key->pos;
      pout = key->ptr;
      if (bswapflag)
        {
        esize = t_size[key->ttype];
        swapbytes(pin, esize, key->nbytes/esize);
        }
      for (b=key->nbytes; b--;)
        *(pout++) = *(pin++);
      }

  return RETURN_OK;
  }


/****** end_readobj **********************************************************
PROTO	void end_readobj(tabstruct *keytab, tabstruct *tab)
PURPOSE	End the writing of individual sources in a FITS table
INPUT	Table which will be accessed from disk (provided by init_readobj()),
	Table containing the keys that have been read.
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	13/11/97
 ***/
void	end_readobj(tabstruct *keytab, tabstruct *tab)

  {

  if (close_cat(keytab->cat) != RETURN_OK)
    error(EXIT_FAILURE,"*Error*: Problem while closing",keytab->cat->filename);

/* Recover the original state of the original table */
  update_tab(keytab);
  if (!(--nlinein))
    {
    free(linein_buf);
    linein_buf = NULL;
    linein_size = 0;
    }

  return;
  }

