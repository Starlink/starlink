/*
 				fitsfield.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	Functions related to the management of fields.
*
*	Last modify:	25/04/97
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"fitscat_defs.h"
#include	"fitscat.h"

/****** read_field *************************************************************
PROTO	int read_field(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, int field, tabstruct *ftab)
PURPOSE	Read several columns from a FITS binary table for only one field.
INPUT	pointer to the OBJECTS table,
	pointer to an array of char *,
	pointer to an array of keystruct * (memory must have been allocated),
	number of keys to read,
	the field to be read,
	pointer to the FIELDS table.
OUTPUT	The number of objects read.
NOTES	The array of pointers pointed by keys is filled with pointers
	to the relevant keys (a NULL means NO key with such name was found).
	A NULL keys pointer can be given (no info returned of course).
	A NULL keynames pointer means read ALL keys belonging to the table.
	A NULL FIELDS table pointer means sequential access to the OBJECTS
	table (much slower).
	Obviously, the FIELD_NUMBER key is required in the table to be read.
AUTHOR	E. Bertin (IAP & Leiden observatory)
        E.R. Deul (Leiden Observatory)
VERSION	25/04/97
        28/08/97  Extended read_field() warning message to print field nr
 ***/
int	read_field(tabstruct *tab, char **keynames, keystruct **keys,
		int nkeys, int field, tabstruct *ftab)

  {
#define		DEFAULT_NUMBER	2048	/* default number of objects */
					/* to use when allocating memory */
   catstruct	*cat;
   keystruct	*key, *fkey, *fposkey, **ckeys;
   char		*buf, *ptr, *fptr;
   int		rfield, ok;
   int		i,j,k,n, larray,narray,nfields, nb, kflag = 0, size;
#ifdef  BSWAP
   int		esize;
#endif

/*!! It is not necessarily the original table */
  tab = tab->key->tab;
  cat = tab->cat;

/* We are expecting a 2D binary-table, and nothing else*/
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    error(EXIT_FAILURE, "*Error*: No binary table in ", cat->filename);

  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    error(EXIT_FAILURE, "*Error*: No binary table in ", cat->filename);

/* Size and number of lines in the binary table*/
  larray = tab->naxisn[0];
  nfields= tab->tfields;
  narray = tab->naxisn[1];

/* Check if field is not too large*/
  if (ftab && (field>ftab->naxisn[1] || field<1))
    error(EXIT_FAILURE, "*Error*: field not found in ", cat->filename);

  if (!keynames)
    nkeys = tab->nkey;

/* Allocate memory to store the list of keys to be read */
  if (!keys)
    {
    QMALLOC(keys, keystruct *, nkeys);
    kflag = 1;
    }

  if (!(fposkey = name_to_key(tab, "FIELD_POS"))) {
     if (!(fposkey = name_to_key(tab, "FIELD_NUMBER"))) {
        error(EXIT_FAILURE, "*Error*: No Field number in ", cat->filename);
     }
  }
/* Get information from the FIELDS table if possible and if necessary*/
  fkey = name_to_key(ftab, "OBJECT_COUNT");
  if (fkey == NULL || fkey->ptr == NULL) {
     if ((key=read_key(ftab, "OBJECT_COUNT")) == NULL) {
        if ((key=read_key(ftab, "OBJECT_NUMBER")) == NULL) {
           error(EXIT_FAILURE,"*Error*: No OBJECT_COUNT in ", cat->filename);
        }
     }
  } else {
     key = fkey;
  }

  if (ftab)
    nb = narray = ((int *)key->ptr)[field-1];
  else
    nb = DEFAULT_NUMBER;	/* Some value to start with */

/*allocate memory for the arrays*/
  ckeys = keys;
  if (keynames)
    for (i=nkeys; i--;)
      {
      if ((key = name_to_key(tab, *(keynames++))))
        {
        QMALLOC(key->ptr, char, key->nbytes*nb);
        *(ckeys++) = key;
        }
      else
        *(ckeys++) = NULL;
      }
  else
    {
    key = tab->key;
    for (i=nkeys; i--;)
      {
      QMALLOC(key->ptr, char, key->nbytes*nb);
      *(ckeys++) = key;
      key = key->nextkey;
      }
    }

/*allocate memory for the buffer where we put one line of data*/
  QMALLOC(buf, char, larray);

/*Positioning to the first element*/
  open_cat(cat, READ_ONLY);

/*Get information from the FIELDS table if possible*/
  if (ftab)
    {
    fkey = name_to_key(ftab, "OBJECT_POS");
    key = (fkey && fkey->ptr) ? fkey : read_key(ftab, "OBJECT_POS");
    QFSEEK(cat->file, tab->bodypos+((((int *)key->ptr)[field-1])-1)*larray,
		SEEK_SET, cat->filename)
    }
  else
    QFSEEK(cat->file, tab->bodypos , SEEK_SET, cat->filename);

/*read line by line*/
  n = 0;
  for (i=narray; i--;)
    {
    QFREAD(buf, larray, cat->file, cat->filename);
/*--Check that current line corresponds to the right field*/
 /*
  * Allow for short and long int FIELD_POS types
  */
    if (fposkey->ttype == T_LONG)
      {
      memcpy(&rfield,buf+fposkey->pos,sizeof(LONG));
#ifdef	BSWAP
      swapbytes(&rfield, sizeof(LONG), 1);
#endif
      ok = (field == *((LONG *)&rfield));
      }
    else
      {
      memcpy(&rfield,buf+fposkey->pos,sizeof(short));
#ifdef	BSWAP
      swapbytes(&rfield, sizeof(short), 1);
#endif
      ok = (field == *((short *)&rfield));
      }

    if (ok)
      {
/*---- Check if there's still enough memory*/
      if (n>=nb)
        {
        nb *= 2;	/* Geometric progression; can be put to a */
			/* milder progression if necessary */
        ckeys = keys;
        for (j=nkeys; j--;)
          if (key = *(ckeys++))
            QREALLOC(key->ptr, char, key->nbytes*nb);
        }

      ckeys = keys;
      for (k=nkeys; k--;)
        if (key = *(ckeys++))
          {
          fptr = buf+key->pos;
          ptr = (char *)key->ptr+n*(size=key->nbytes);
#ifdef	BSWAP
          esize = t_size[key->ttype];
          swapbytes(fptr, esize, size/esize);
#endif
          for (j=size; j--;)
            *(ptr++) = *(fptr++);
          }
      n++;
      }
    }

/* Finally, only keep what is needed*/
  ckeys = keys;
  if (!n)
    {
    char mess[80];
    sprintf(&mess[0],"No element read in field %d of ",field);
    warning(mess, cat->filename);
    ckeys = keys;
    for (i=nkeys; i--;)
      if (key = *(ckeys++))
        QFREE(key->ptr);
    }
  else {
    for (i=nkeys; i--;)
      if (key = *(ckeys++))
        QREALLOC(key->ptr, char, key->nbytes*n);
    }
  free(buf);
  if (kflag)
    free(keys);

  return n;
  }

