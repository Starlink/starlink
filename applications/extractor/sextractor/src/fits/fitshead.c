/*
 				fitshead.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	general functions for handling FITS file headers.
*
*	Last modify:	13/08/97
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"fitscat_defs.h"
#include	"fitscat.h"

/******* get_head **************************************************************
PROTO	int get_head(tabstruct *tab)
PURPOSE	Read a FITS header.
INPUT	Table structure.
OUTPUT	RETURN_OK if a FITS header has been found and loaded, or RETURN_ERROR
	otherwise.
NOTES	The file must be opened, and the file pointer must be located at
	the beginning of a header.
	The headbuf pointer in the catstruct is reallocated.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/02/96
 ***/
int	get_head(tabstruct *tab)

  {
   catstruct	*cat;
   int		i;
   char		*buf;

  buf = tab->headbuf;
  if (!(cat = tab->cat))
    error(EXIT_FAILURE, "*Internal Error*: Table has no parent catalog","!");

  QFREE(buf);
  QMALLOC(buf, char, FBSIZE);

/*Read the first block and check that it is FITS */
  if (!fread(buf, FBSIZE, 1, cat->file))
    {
    QFREE(buf);
    return RETURN_ERROR;
    }

  if (strncmp(buf, "SIMPLE  ", 8) && strncmp(buf, "XTENSION", 8))
    {
    QFREE(buf);
    return RETURN_ERROR;
    }

/*Find the number of FITS blocks of the header while reading it */
  for (i=1; !fitsnfind(buf,"END     ", i); i++)
    {
    QREALLOC(buf, char, FBSIZE*(i+1));
    QFREAD(&buf[FBSIZE*i], FBSIZE, cat->file, cat->filename);
    }

  tab->headnblock = i;
  tab->headbuf = buf;

  return  RETURN_OK;
  }


/****** readbasic_head *********************************************************
PROTO	void readbasic_head(tabstruct *tab)
PURPOSE	Read the current FITS header basic keywords.
INPUT	pointer to catstruct.
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/02/96
 ***/
void	readbasic_head(tabstruct *tab)

  {
   catstruct	*cat;
   char		key[12];
   int		i, tabsize;

  if (!(cat = tab->cat))
    error(EXIT_FAILURE, "*Internal Error*: Table has no parent catalog","!");

  if (fitsread(tab->headbuf, "BITPIX  ", &tab->bitpix, H_INT, T_LONG)
	==RETURN_ERROR)
    error(EXIT_FAILURE, "*Error*: Corrupted FITS header in ", cat->filename);

  tab->bytepix = tab->bitpix>0?(tab->bitpix/8):(-tab->bitpix/8);

  if (fitsread(tab->headbuf, "NAXIS   ", &tab->naxis, H_INT, T_LONG)
	==RETURN_ERROR)
    error(EXIT_FAILURE, "*Error*: Corrupted FITS header in ", cat->filename);

  tabsize = 0;
  if (tab->naxis>0)
    {
    QFREE(tab->naxisn);
    QMALLOC(tab->naxisn, int, tab->naxis);
/*--get the size of the array*/
    tabsize = 1;
    for (i=0; i<tab->naxis && i<999; i++)
      {
      sprintf(key,"NAXIS%-3d", i+1);
      if (fitsread(tab->headbuf, key, &tab->naxisn[i], H_INT, T_LONG)
		==RETURN_ERROR)
        error(EXIT_FAILURE, "*Error*: incoherent FITS header in ",
				cat->filename);
      tabsize *= tab->naxisn[i];
      }
    }

/*random groups parameters (optional)*/
  tab->pcount = 0;
  fitsread(tab->headbuf, "PCOUNT  ", &tab->pcount, H_INT, T_LONG);
  tab->gcount = 1;
  fitsread(tab->headbuf, "GCOUNT  ", &tab->gcount, H_INT, T_LONG);

/*number of fields (only for tables)*/
  tab->tfields = 0;
  fitsread(tab->headbuf, "TFIELDS ", &tab->tfields, H_INT, T_LONG);

/*in case of a non-primary header*/
  tab->xtension[0] = (char)'\0';
  fitsread(tab->headbuf, "XTENSION", tab->xtension, H_STRING, T_STRING);
  tab->extname[0] = (char)'\0';
  fitsread(tab->headbuf, "EXTNAME ", tab->extname, H_STRING, T_STRING);

  tab->tabsize = tab->bytepix*tab->gcount*(tab->pcount+tabsize);

  return;
  }


/******* readbintabparam_head **************************************************
PROTO	int readbintabparam_head(tabstruct *tab)
PURPOSE	Read the current FITS header parameters concerning the binary-table.
INPUT	pointer to tabstruct.
OUTPUT	RETURN_OK if a binary table was found and mapped, RETURN_ERROR
	otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	30/04/97
 ***/
int	readbintabparam_head(tabstruct *tab)

  {
   catstruct	*cat;
   keystruct	*key, *prevkey;
   static char	strf[82], strk[12];
   char		*str;
   static int	naxisn[32];
   int		i,j, larray, nfields,narray, pos;

  if (!(cat = tab->cat))
    error(EXIT_FAILURE, "*Internal Error*: Table has no parent catalog","!");

/*We are expecting a 2D binary-table, and nothing else*/
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    return RETURN_ERROR;

/*Size and number of lines in the binary table*/
  larray = tab->naxisn[0];
  nfields= tab->nkey = tab->tfields;
  narray = tab->naxisn[1];

  prevkey = NULL;
/*For each of the data fields...*/
  pos = 0;
  for (i=0; i<nfields; i++)
    {
/*--manage the chaining of keys*/
    QCALLOC(key, keystruct, 1);
    if (prevkey)
       {
       prevkey->nextkey = key;
       key->prevkey = prevkey;
       }
    else
       tab->key = key;
     prevkey = key;

/*--map binary-table fields*/

    sprintf(strk, "TTYPE%-3d", i+1);
    if (fitsread(tab->headbuf, strk, key->name, H_STRING, T_STRING)
	!= RETURN_OK)
      error(EXIT_FAILURE,
	"*Error*: Incorrect FITS binary-table header in ", cat->filename); 
    fitsread(tab->headbuf, strk, key->comment, H_HCOMMENT, T_STRING);

    sprintf(strk, "TUNIT%-3d", i+1);
    fitsread(tab->headbuf, strk, key->unit, H_STRING, T_STRING);
    sprintf(strk, "TDISP%-3d", i+1);
    fitsread(tab->headbuf, strk, key->printf, H_STRING, T_STRING);
    if (*key->printf)
      strcpy(key->printf,tdisptoprintf(key->printf));

    sprintf(strk, "TFORM%-3d", i+1);
    if (fitsread(tab->headbuf, strk, strf, H_STRING, T_STRING) != RETURN_OK)
      error(EXIT_FAILURE,
	"*Error*: Incorrect FITS binary-table header in ", cat->filename); 
    key->pos = pos;
    pos += (key->nbytes = tsizeof(strf));
    key->ttype = ttypeof(strf);
    switch(key->ttype)
      {
      case T_BYTE:
      case T_SHORT:
      case T_LONG:
        key->htype = H_INT;
        break;
      case T_FLOAT:
      case T_DOUBLE:
        key->htype = H_EXPO;
        break;
      case T_STRING:
        key->htype = H_STRING;
        break;
      default:
        error(EXIT_FAILURE, "*Internal Error*: Unkwown T_TYPE for ", str);
      }

/*--handle the special case of multimensional arrays*/
    if ((naxisn[0] = key->nbytes/t_size[key->ttype]) > 1)
      {
      sprintf(strk, "TDIM%-3d", i+1);
      if (fitsread(tab->headbuf, strk, strf, H_STRING, T_STRING) == RETURN_OK)
        {
        str = strf;
        for (j=0; naxisn[j]=(int)strtol(str+1, &str, 10); j++);
        key->naxis = j;
        }
      else
        key->naxis = 1;
      QMALLOC(key->naxisn, int, key->naxis);
      for (j=0; j<key->naxis; j++)
        key->naxisn[j] = naxisn[j];
      }
    else
      key->naxis = 0;

    key->nobj = narray;
    key->tab = tab;
    }

  if (pos != larray)
    error(EXIT_FAILURE,
	"*Error*: Malformed FITS binary-table header in ", cat->filename); 

/*make both ends of the chain meet*/
  prevkey->nextkey = tab->key;
  tab->key->prevkey = prevkey;

  return RETURN_OK;
  }


/****** update_head ************************************************************
PROTO	int update_head(tabstruct *tab)
PURPOSE	Update a FITS header according to what's in the table.
INPUT	Table structure.
OUTPUT	RETURN_OK if tab is a binary table, or RETURN_ERROR otherwise.
NOTES	The headbuf pointer in the tabstruct might be reallocated.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	12/05/97
 ***/
int	update_head(tabstruct *tab)

  {
   keystruct	*key;
   tabstruct	*ctab;
   int		i,j,n, endpos;
   static char	strk[82], str[82];

/* If not a binary table, do only a few basic things */
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    {
    fitsadd(tab->headbuf, "BITPIX  ", "BITS PER PIXEL");
    fitswrite(tab->headbuf, "BITPIX  ", &tab->bitpix, H_INT, T_LONG);
    fitsadd(tab->headbuf,"NAXIS   ", "NUMBER OF AXES");
    fitswrite(tab->headbuf, "NAXIS   ", &tab->naxis, H_INT, T_LONG);
    for (i=0; i<tab->naxis; i++)
      {
      sprintf(strk, "NAXIS%-3d", i+1);
      if (fitsfind(tab->headbuf, strk) == RETURN_ERROR
	&& (fitsfind(tab->headbuf, "END     ")+1)*80 >= tab->headnblock*FBSIZE)
        {
        tab->headnblock++;
        QREALLOC(tab->headbuf, char, tab->headnblock*FBSIZE);
        memset(tab->headbuf + (tab->headnblock-1)*FBSIZE, ' ', FBSIZE);
        }
      fitsadd(tab->headbuf, strk, "NUMBER OF ELEMENTS ALONG THIS AXIS");
      fitswrite(tab->headbuf, strk, &tab->naxisn[i], H_INT, T_LONG);
      }
    return RETURN_ERROR;
    }

/*First, remove all existing TTYPE, TFORM, etc...*/
  fitsremove(tab->headbuf, "TTYPE???");
  fitsremove(tab->headbuf, "TFORM???");
  fitsremove(tab->headbuf, "TUNIT???");
  fitsremove(tab->headbuf, "TZERO???");
  fitsremove(tab->headbuf, "TSCAL???");
  fitsremove(tab->headbuf, "TDIM???");
  fitsremove(tab->headbuf, "TDISP???");

/*Update EXTNAME, the table name */
  if (*tab->extname)
    {
    fitsadd(tab->headbuf, "EXTNAME ", "TABLE NAME");
    fitswrite(tab->headbuf, "EXTNAME ", tab->extname, H_STRING, T_STRING);
    }

/*Change NAXIS1 in order to take into account changes in width*/
  fitswrite(tab->headbuf, "NAXIS1  ", &tab->naxisn[0], H_INT, T_LONG);

/*Change NAXIS1 in the number of fields */
  fitswrite(tab->headbuf, "TFIELDS ", &tab->tfields, H_INT, T_LONG);

/*Changes in the number of elements (look for possible segments)*/
  for (ctab = tab, n = ctab->naxisn[1];
	(ctab=ctab->nexttab) && !ctab->nseg;)
    n += ctab->naxisn[1];
  fitswrite(tab->headbuf, "NAXIS2  ", &n, H_INT, T_LONG);

  key = tab->key;
  if (!key)
    return RETURN_ERROR;

  endpos = fitsfind(tab->headbuf, "END     ");
  for (i=0; i<tab->nkey; i++)
    {
/*--Check that the new parameters will fit in the allocated memory space*/
    endpos += 2;
    if (*key->unit)
      endpos++;
    if (*key->printf)
      endpos++;
    if (key->naxis>1)
      endpos++;
    if (endpos >= (n=tab->headnblock)*(FBSIZE/80))
      {
      QREALLOC(tab->headbuf, char, (n+1)*FBSIZE);
      memset(tab->headbuf + n*FBSIZE, ' ', FBSIZE);
      tab->headnblock++;
      }
    sprintf(strk, "TTYPE%-3d", i+1);
    fitsadd(tab->headbuf, strk, key->comment);
    fitswrite(tab->headbuf, strk, key->name, H_STRING, T_STRING);
    sprintf(strk, "TFORM%-3d", i+1);
    fitsadd(tab->headbuf, strk, "");
    tformof(str, key->ttype, key->nbytes/t_size[key->ttype]);
    fitswrite(tab->headbuf, strk, str, H_STRING, T_STRING);
    if (key->naxis>1)
      {
       char	*str2, *str2lim;

      sprintf(strk, "TDIM%-3d", i+1);
      fitsadd(tab->headbuf, strk, "");
      sprintf(str, "(");
      str2 = str+1;
      str2lim = str+70;	/* Prevent an excessively large string */
      for (n=0; n<key->naxis && str2<str2lim; n++)
        {
        sprintf(str2, n?", %d%n":"%d%n", key->naxisn[n],&j);
        str2 += j;
        }
      sprintf(str2, ")");
      fitswrite(tab->headbuf, strk, str, H_STRING, T_STRING);
      }
    if (*key->unit)
      {
      sprintf(strk, "TUNIT%-3d", i+1);
      fitsadd(tab->headbuf, strk, "");
      fitswrite(tab->headbuf, strk, key->unit, H_STRING, T_STRING);
      }
    if (*key->printf)
      {
      sprintf(strk, "TDISP%-3d", i+1);
      fitsadd(tab->headbuf, strk, "");
      fitswrite(tab->headbuf, strk, printftotdisp(key->printf),
	H_STRING, T_STRING);
      }
    key = key->nextkey;
    }

/*Remove any memory allocated in excess (and correct headnblock by the way)*/
  if ((n = endpos/(FBSIZE/80)+1) < tab->headnblock)
    QREALLOC(tab->headbuf, char, (tab->headnblock = n)*FBSIZE);

/*That may be enough for now; to be continued...*/

  return RETURN_OK;
  }


/****** addkeyto_head **********************************************************
PROTO	int addkeyto_head(tabstruct *tab, keystruct *key)
PURPOSE	Add a keyword and its value to a table header.
INPUT	Table structure,
	Key containing the keyword and its value.
OUTPUT	Line position in the FITS header.
NOTES	The headbuf pointer in the tabstruct might be reallocated.
	Pre-existing keywords are overwritten (but not their comments).
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	05/02/97
 ***/
int	addkeyto_head(tabstruct *tab, keystruct *key)

  {
   int	n;

  if (fitsfind(tab->headbuf, key->name) == RETURN_ERROR
	&& (fitsfind(tab->headbuf, "END     ")+1)*80 >= tab->headnblock*FBSIZE)
    {
    tab->headnblock++;
    QREALLOC(tab->headbuf, char, tab->headnblock*FBSIZE);
    memset(tab->headbuf + (tab->headnblock-1)*FBSIZE, ' ', FBSIZE);
    }

  n = fitsadd(tab->headbuf, key->name, key->comment);
  fitswrite(tab->headbuf, key->name, key->ptr, key->htype, key->ttype);

  return n;
  }


/****** tformof ****************************************************************
PROTO	int tformof(char *str, t_type ttype, int n)
PURPOSE	Return the ``TFORM'' string corresponding to a t_type
	and the number of elements.
INPUT	a char pointer (to be filled with the T_FORM string),
	t_type,
	Number of elements.
OUTPUT	RETURN_OK if everything went as expected, or RETURN_ERROR otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/02/96
 ***/
int	tformof(char *str, t_type ttype, int n)

  {
   char	t;

  switch (ttype)
    {
    case T_BYTE:	t = 'B';
			break;
    case T_SHORT:	t = 'I';
			break;
    case T_LONG:	t = 'J';
			break;
    case T_FLOAT:	t = 'E';
			break;
    case T_DOUBLE:	t = 'D';
			break;
    case T_STRING:	t = 'A';
			break;
    default:		return	RETURN_ERROR;
    }

  sprintf(str, "%d%c", n, t);

  return RETURN_OK;
  }


/****** tsizeof ****************************************************************
PROTO	int tsizeof(char *str)
PURPOSE	Return the size of a binary-table field from its ``TFORM''.
INPUT	TFORM string (see the FITS documentation).
OUTPUT	size in bytes, or RETURN_ERROR if the TFORM is unknown.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/02/96
 ***/
int	tsizeof(char *str)

  {
   int	n;
   char	*str2;

  str2 = str;
  if (!(n = strtol(str, &str2, 10)))
    n = 1;

  switch ((int)*str2)
    {
    case 'L': case 'B': case 'A':	return	n;
    case 'X':				return	(n-1)/8+1;
    case 'I':				return	2*n;
    case 'J': case 'E':			return	4*n;
    case 'C': case 'D': case 'P':	return	8*n;
    case 'M':				return	16*n;
    default:				return	RETURN_ERROR;
    }

  }


/****** ttypeof ****************************************************************
PROTO	t_type ttypeof(char *str)
PURPOSE	Give the ``t_type'' of a binary-table field from its ``TFORM''.
INPUT	TFORM string (see the FITS documentation).
OUTPUT	size in bytes, or RETURN_ERROR if the TFORM is unknown.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/02/96
 ***/
t_type	ttypeof(char *str)

  {
   int	n;
   char	*str2;

  str2 = str;
  n = strtol(str, &str2, 10);
  switch ((int)*str2)
    {
    case 'L': case 'B': case 'X':	return	T_BYTE;
    case 'I':				return	T_SHORT;
    case 'J':				return	T_LONG;
    case 'E':				return	T_FLOAT;
    case 'D':				return	T_DOUBLE;
    case 'A':				return	T_STRING;
    default:				return	RETURN_ERROR;
    }

  }


/****** tdisptoprintf **********************************************************
PROTO	char	*tdisptoprintf(char *tdisp)
PURPOSE	Convert the ``TDISP'' FITS format to the printf() format.
INPUT	TDISP format string (see the FITS documentation).
OUTPUT	printf() format string (see e.g.  K&R).
NOTES	The present conversion does not handle binary or engineer notations.
	A NULL vector is returned if the conversion was unsuccessful.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	13/08/97
 ***/
char	*tdisptoprintf(char *tdisp)

  {
   static char	str[16], control[4];
   int		w,d, n;

  w = d = 0;
  n = 0;
  n=sscanf(tdisp,"%[ALIBOZFENSGD]%d.%d", control, &w, &d)-1;
  if (!w)
    {
    warning("Strange TDISP format: ", tdisp);
    return NULL;
    }
  switch ((int)*control)
    {
    case 'A':
      sprintf(str, "%%%dc",w);
      break;
    case 'L':
      sprintf(str, "%%%dd",w);
      break;      
    case 'I':
      if (n>1)
        sprintf(str, "%%%d.%dd",w,d);
      else
        sprintf(str, "%%%dd",w);
      break;
    case 'B': case 'Z':
      if (n>1)
        sprintf(str, "%%%d.%dx",w,d);
      else
        sprintf(str, "%%%dx",w);
      break;
    case 'O':
      if (n>1)
        sprintf(str, "%%%d.%do",w,d);
      else
        sprintf(str, "%%%do",w);
      break;
    case 'F':
      if (n>1)
        sprintf(str, "%%%d.%df",w,d);
      else
        sprintf(str, "%%%df",w);
      break;
    case 'E': case 'D': 
      if (n>1)
        sprintf(str, "%%%d.%dE",w,d);
      else
        sprintf(str, "%%%dE",w);
      break;
    case 'G': 
      if (n>1)
        sprintf(str, "%%%d.%dG",w,d);
      else
        sprintf(str, "%%%dG",w);
      break;
    default:
      warning("Unknown TDISP format: ", tdisp);
      return NULL;
    }

  return str;
  }


/****** printftotdisp **********************************************************
PROTO	char	*printftotdisp(char *tdisp)
PURPOSE	Convert the printf() format to the ``TDISP'' FITS format.
INPUT	printf() format string (see e.g.  K&R).
OUTPUT	TDISP format string (see the FITS documentation).
NOTES	The handling of C string formatting does not include the precision.
	NULL is returned in case of unsucessful conversion.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	13/08/97
 ***/
char	*printftotdisp(char *cprintf)

  {
   static char	str[16], *control;
   int		w,d,n;

  *str = 0;
  w = d = 0;
  if (!(control = strpbrk(cprintf, "cdueERfFgGoOxXs")))
    {
    warning("Unknown printf() format: ", cprintf);
    return NULL;
    }

  n = sscanf(cprintf,"%%%d.%d", &w, &d);
  w = abs(w);
  if (!n)
    {
    warning("Unconvertible printf() format: ", cprintf);
    return NULL;
    }

  switch ((int)*control)
    {
    case 'c':
      sprintf(str, "A%d",w);
      break;
    case 's':
      sprintf(str, "A%d",w);
      break;
    case 'd': case 'u':
      if (n>1)
        sprintf(str, "I%d.%d",w,d);
      else
        sprintf(str, "I%d",w);
      break;
    case 'o': case 'O':
      if (n>1)
        sprintf(str, "O%d.%d",w,d);
      else
        sprintf(str, "O%d",w);
      break;
    case 'x': case 'X':
      if (n>1)
        sprintf(str, "Z%d.%d",w,d);
      else
        sprintf(str, "Z%d",w);
      break;
    case 'f': case 'F':
      if (n>1)
        sprintf(str, "F%d.%d",w,d);
      else
        sprintf(str, "F%d",w);
      break;
    case 'e': case 'E':
      if (n>1)
        sprintf(str, "E%d.%d",w,d);
      else
        sprintf(str, "E%d",w);
      break;
    case 'g': case 'G':
      if (n>1)
        sprintf(str, "G%d.%d",w,d);
      else
        sprintf(str, "G%d",w);
      break;
    default:
      warning("Unknown printf() format: ", cprintf);
      return NULL;
    }

  return str;
  }

