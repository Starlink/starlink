/*
 				fitskey.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	Functions related to the management of keys.
*
*	Last modify:	15/08/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"fitscat_defs.h"
#include	"fitscat.h"

/****** add_key ****************************************************************
PROTO	int add_key(keystruct *key, tabstruct *tab, int pos)
PURPOSE	Copy a key from one table to another.
INPUT	Pointer to the key,
        Pointer to the table,
	Pointer to the destination table,
	Position (1= first, <=0 = at the end)
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise.
NOTES	A preexisting key in the destination table yields a RETURN_ERROR.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	26/03/96
 ***/
int	add_key(keystruct *key, tabstruct *tab, int pos)

  {

/*Check if a similar key doesn't already exist in the dest. cat */
  if (name_to_key(tab, key->name))
    return RETURN_ERROR;

/*Update links (portion of code similar to that of copy_key below) */
  if ((key->nextkey = pos_to_key(tab, pos)))
    {
    (key->prevkey = key->nextkey->prevkey)->nextkey = key;
    key->nextkey->prevkey = key;
/*--the first place has a special meaning*/
    if (pos==1)
      tab->key = key;
    }
  else
/*There was no no key before*/
    tab->key = key->nextkey = key->prevkey = key;

  tab->nkey++;

  return RETURN_OK;
  }


/****** blank_keys *************************************************************
PROTO	int blank_keys(tabstruct *tab)
PURPOSE	Put the array pointers from all keys in a table to NULL.
INPUT	Pointer to the table.
OUTPUT	RETURN_OK if keys were found, and RETURN_ERROR otherwise.
Notes:	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
int	blank_keys(tabstruct *tab)

  {
   keystruct	*key;
   int		k;

  if (!(key = tab->key))
    return RETURN_ERROR;

  for (k=tab->nkey; k--;)
    {
    key->ptr = NULL;
    key = key->nextkey;
    }

  return RETURN_OK;
  }


/****** copy_key ***************************************************************
PROTO	int copy_key(tabstruct *tabin, char *keyname, tabstruct *tabout, int pos)
PURPOSE	Copy a key from one table to another.
INPUT	Pointer to the original table,
        Name of the key,
	Pointer to the destination table,
	Position (1= first, <=0 = at the end)
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise.
NOTES	A preexisting key in the destination table yields a RETURN_ERROR,
	the ptr member is NOT COPIED.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	19/08/96
 ***/
int	copy_key(tabstruct *tabin, char *keyname, tabstruct *tabout, int pos)

  {
   keystruct	*keyin, *keyout;

/*Convert the key name to a pointer*/
  if (!(keyin = name_to_key(tabin, keyname)))
    return RETURN_ERROR;

/*Check if a similar key doesn't already exist in the dest. cat */
  if (name_to_key(tabout, keyname))
    return RETURN_ERROR;

  tabout->nkey++;

/*First, allocate memory and copy data */
  QCALLOC(keyout, keystruct, 1);
  *keyout = *keyin;
  keyout->ptr = NULL;
  if (keyin->naxis)
    QMEMCPY(keyin->naxisn, keyout->naxisn, int, keyin->naxis);

/*Then, update the links */
  if ((keyout->nextkey = pos_to_key(tabout, pos)))
    {
    (keyout->prevkey = keyout->nextkey->prevkey)->nextkey = keyout;
    keyout->nextkey->prevkey = keyout;
/*--the first place has a special meaning*/
    if (pos==1)
      tabout->key = keyout;
    }
  else
/*There was no no key before*/
    tabout->key = keyout->nextkey = keyout->prevkey = keyout;

  return RETURN_OK;
  }


/****** free_key ***************************************************************
PROTO	void free_key(keystruct *key)
PURPOSE	Free memory associated to a key ptr.
INPUT	Pointer to the key.
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	19/08/96
 ***/
void	free_key(keystruct *key)

  {
  free(key->naxisn);
  free(key->ptr);
  free(key);

  return;
  }


/****** new_key ****************************************************************
PROTO	keystruct *new_key(char *keyname)
PURPOSE	Create a new key.
INPUT	Name of the key.
OUTPUT	A pointer to the new keystruct.
NOTES	This function is only provided as a counterpart to new_tab() and
	new_cat(): in order to be usable, other key parameters MUST be
	handled by the user.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	26/03/96
 ***/
keystruct	*new_key(char *keyname)

  {
   keystruct	*key;

  QCALLOC(key, keystruct, 1);
  strcpy(key->name, keyname);

  return key;
  }


/****** read_key ***************************************************************
PROTO	keystruct *read_key(tabstruct *tab, char *keyname)
PURPOSE	Read one simple column from a FITS binary table.
INPUT	pointer to the table,
	name of the key,
OUTPUT	A pointer to the relevant key, or NULL if the desired key is not
	found in the table.
NOTES	If key->ptr is not NULL, the function doesn't do anything.
AUTHOR	E. Bertin (IAP & Leiden observatory)
        E.R. Deul (Sterrewacht Leiden) (Added open_cat error checking)
VERSION	18/02/2000
 ***/
keystruct *read_key(tabstruct *tab, char *keyname)

  {
   catstruct	*cat;
   keystruct	*key;
   char		*buf, *ptr, *fptr,*fptr0;
   int		i,j, larray,narray,size;
   int		esize;

  if (!(key = name_to_key(tab, keyname)))
    return NULL;

/*If ptr is not NULL, there is already something loaded there: let's free mem */
  QFREE(key->ptr);

/*!! It is not necessarily the original table */
  tab = key->tab;
  cat = tab->cat;

/*We are expecting a 2D binary-table, and nothing else*/
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    error(EXIT_FAILURE, "*Error*: No binary table in ", cat->filename);

/*Size and number of lines in the binary table*/
  larray = tab->naxisn[0];
  narray = tab->naxisn[1];

/*Positioning to the first element*/
  if (open_cat(cat, READ_ONLY) == RETURN_ERROR)
     error(EXIT_FAILURE, "*Error*: opening catalog ",cat->filename);
  QFSEEK(cat->file, tab->bodypos , SEEK_SET, cat->filename);

/*allocate memory for the buffer where we put one line of data*/
  QMALLOC(buf, char, larray);

  fptr0 = buf+key->pos;
  size = key->nbytes;

/*allocate memory for the array*/
  QMALLOC(ptr, char, size*narray);
  key->ptr = ptr;

/*read line by line*/
  for (i=narray; i--;)
    {
    QFREAD(buf, larray, cat->file, cat->filename);
    fptr = fptr0;
    if (bswapflag)
      {
      esize = t_size[key->ttype];
      swapbytes(fptr0, esize, size/esize); 
      }
    for (j = size; j--;)
      *(ptr++) = *(fptr++);
    }

  free(buf);
  return key;
  }


/****** read_keys **************************************************************
PURPOSE	Read several columns from a FITS binary table.
INPUT	pointer to the table,
	pointer to an array of char *,
	pointer to an array of keystruct * (memory must have been allocated),
	number of keys to read,
	an optional mask pointer.
OUTPUT	-.
NOTES	The array of pointers pointed by keys is filled with pointers
	to the relevant keys (a NULL means NO key with such name was found).
	A NULL keys pointer can be given (no info returned of course).
	A NULL keynames pointer means read ALL keys belonging to the table.
	A NULL mask pointer means NO selection for reading.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	18/02/2000
 ***/
void	read_keys(tabstruct *tab, char **keynames, keystruct **keys, int nkeys,
		BYTE *mask)

  {
   catstruct	*cat;
   keystruct	*key, **ckeys;
   BYTE		*mask2;
   char		*buf, *ptr, *fptr;
   int		i,j,k,n, larray,narray, nb, kflag = 0, size;
   int		esize;

/*!! It is not necessarily the original table */
  tab = tab->key->tab;
  cat = tab->cat;

/*We are expecting a 2D binary-table, and nothing else*/
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    error(EXIT_FAILURE, "*Error*: No binary table in ", cat->filename);

/*Size and number of lines in the binary table*/
  larray = tab->naxisn[0];
  narray = tab->naxisn[1];

  nb = 0;
  if ((mask2 = mask))
    {
    for (i=narray; i--;)
      if (*(mask2++))
        nb++;
    }

  if (!keynames)
    nkeys = tab->nkey;

/*Allocate memory to store the list of keys to be read */
  if (!keys)
    {
    QMALLOC(keys, keystruct *, nkeys);
    kflag = 1;
    }

/*allocate memory for the arrays*/
  ckeys = keys;
  if (keynames)
    for (i=nkeys; i--;)
      {
      if ((key = name_to_key(tab, *(keynames++))))
        {
        QFREE(key->ptr);
        if (nb)
          key->nobj = nb;
        else
          nb=key->nobj;
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
      QFREE(key->ptr);
      if (nb)
        key->nobj = nb;
      else
        nb=key->nobj;
      QMALLOC(key->ptr, char, key->nbytes*nb);
      *(ckeys++) = key;
      key = key->nextkey;
      }
    }

/*allocate memory for the buffer where we put one line of data*/
  QMALLOC(buf, char, larray);

/*Positioning to the first element*/
  open_cat(cat, READ_ONLY);
  QFSEEK(cat->file, tab->bodypos , SEEK_SET, cat->filename);

/*read line by line*/
  n = 0;
  mask2 = mask;
  for (i=narray; i--;)
    {
    QFREAD(buf, larray, cat->file, cat->filename);
    if (!mask || *(mask2++))
      {
      ckeys = keys;
      for (j=nkeys; j--;)
        if ((key = *(ckeys++)))
          {
          fptr = buf+key->pos;
          ptr = (char *)key->ptr+n*(size=key->nbytes);
          if (bswapflag)
            {
            esize = t_size[key->ttype];
            swapbytes(fptr, esize, size/esize); 
            }
          for (k = size; k--;)
            *(ptr++) = *(fptr++);
          }
      n++;
      }
    }

  free(buf);
  if (kflag)
    free(keys);

  return;
  }

/****** remove_key *************************************************************
PROTO	int remove_key(tabstruct *tab, char *keyname)
PURPOSE	Remove a key from a table.
INPUT	Pointer to the table,
	Name of the key.
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise.
NOTES	If keyname = "", the last key from the list is removed.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/01/97
 ***/
int	remove_key(tabstruct *tab, char *keyname)

  {
   keystruct	*key, *prevkey, *nextkey;

  if (!keyname || !tab->nkey || !tab->key)
    return RETURN_ERROR;

  if (keyname[0])
    {
/*--Convert the key name to a pointer*/
    if (!(key = name_to_key(tab, keyname)))
      return RETURN_ERROR;
    }
  else
    key = tab->key->prevkey;

  prevkey = key->prevkey;
/*Free memory*/
  nextkey = key->nextkey;
  if (tab->key==key)
    tab->key = nextkey;
  free_key(key);

  if (--tab->nkey)
    {
/*--update the links of neighbours*/
    nextkey->prevkey = prevkey;
    prevkey->nextkey = nextkey;
    }
  else
    tab->key = NULL;

  return RETURN_OK;  
  }


/****** remove_keys ************************************************************
PROTO	int remove_keys(tabstruct *tab)
PURPOSE	Remove all keys from a table.
INPUT	Pointer to the table.
OUTPUT	RETURN_OK if keys were found, and RETURN_ERROR otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	13/03/99
 ***/
int	remove_keys(tabstruct *tab)

  {
   int	k;

  if (!tab->key)
    return RETURN_ERROR;

  for (k=tab->nkey; k--;)
    remove_key(tab, "");

  return RETURN_OK;
  }


/****** name_to_key ************************************************************
PROTO	keystruct *name_to_key(tabstruct *tab, char *keyname)
PURPOSE	Name search of a key in a table.
INPUT	Pointer to the table,
	Key name.
OUTPUT	The key pointer if the name was matched, and NULL otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
keystruct	*name_to_key(tabstruct *tab, char *keyname)

  {
   keystruct	*key;
   int		i;

  if (!(key=tab->key))
   return NULL;

  for (i=tab->nkey; strcmp(keyname, key->name) && i--; key=key->nextkey);

  return i<0? NULL:key;
  }

/****** keys_list **************************************************************
PROTO	char **keys_list(catstruct *tab, int *n)
PURPOSE	List all keys in a table.
INPUT	Pointer to the table,
	Pointer to the number of names in that list.
OUTPUT	A list of all key names.
NOTES	-.
AUTHOR	E.R. Deul (Leiden observatory)
VERSION	??/??/96
 ***/
char **keys_list(tabstruct *tab, int *n)

  {
   keystruct	*key;
   int		i;
   char 	**names;

  QCALLOC(names, char *, tab->nkey);
  key = tab->key;
  for (i=0; i<tab->nkey; i++) {
    QCALLOC(names[i], char, MAXCHARS);
    strcpy(names[i],key->name);
    key = key->nextkey;
  }
  *n = tab->nkey;
  return names;
  }


/****** pos_to_key *************************************************************
PROTO	keystruct *pos_to_key(tabstruct *tab, int pos)
PURPOSE	Position search of a key in a table.
INPUT	Pointer to the table,
	Position of the key.
OUTPUT	The key pointer if a key exists at the given position, and the
	pointer to the first key otherwise.
NOTES	pos = 0 or 1 means the first key.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	20/03/96
 ***/
keystruct	*pos_to_key(tabstruct *tab, int pos)

  {
   keystruct	*key;
   int		i;

  if (!(key=tab->key))
   return NULL;

  if ((pos--)==1)
    return tab->key;

  for (i=0; i!=pos && i<tab->nkey; i++, key=key->nextkey);

  return i<tab->nkey?key:tab->key;
  }


/****** show_keys **************************************************************
PROTO	void show_keys(tabstruct *tab, char **keynames,
			keystruct **keys, int nkeys,
			BYTE *mask, FILE *stream,
			int strflag, int banflag, int leadflag, 
                        output_type o_type)
PURPOSE	Convert a binary table to an ASCII file.
INPUT	pointer to the table,
	pointer to an array of char *,
	pointer to an array of keystruct * (memory must have been allocated),
	number of keys to read,
	an optional mask pointer,
	a stream,
	a flag to indicate if arrays should be displayed (0=NO),
	a flag to indicate if a banner with keynames should be added (0=NO).
	a flag to indicate if a leading row number should be added (0=NO).
        the output type
OUTPUT	-.
NOTES	This is approximately the same code as for read_keys.
	The array of pointers pointed by keys is filled with pointers
	to the relevant keys (a NULL means NO key with such name was found).
	A NULL keys pointer can be given (no info returned of course).
	A NULL keynames pointer means read ALL keys belonging to the table.
	A NULL mask pointer means NO selection for reading.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/08/2003
 ***/
void	show_keys(tabstruct *tab, char **keynames, keystruct **keys, int nkeys,
		BYTE *mask, FILE *stream,
		int strflag, int banflag, int leadflag, output_type o_type)

  {
   catstruct	*cat;
   keystruct	*key, **ckeys;
   BYTE		*mask2;
   char		*buf, *rfield, *ptr;
   int		i,j,k,n,c, larray,narray, nb, kflag, maxnbytes, nelem,
		esize, *key_col;
   typedef struct structreq_keyname
      {
      char    oldname[80];         /* Name of the original pipeline key */
      char    newname[80];         /* Name of the skycat required key   */
      } req_keynamestruct;

   req_keynamestruct objectmap[] =
      {
        {"SeqNr", "id"},
        {"Ra", "ra"},
        {"Dec", "dec"},
        {"MAG_ISO", "Mag"},
        {"", ""}
      };
   req_keynamestruct *map;

   char    skycathead[] = "QueryResult\n\n"
        "# Config entry for original catalog server:\n"
        "serv_type: catalog\n"
        "long_name: ldactoskycat catalog\n"
        "short_name: ldactoaskycat\n"
        "symbol: id circle %4.1f\n"
        "search_cols: mag {Brightest (min)} {Faintest (max)}\n"
        "# End config entry\n\n";

   char    *t, skycattail[] = "";


/* !! It is not necessarily the original table */
  if (tab->key)
    tab = tab->key->tab;
  cat = tab->cat;

/* We are expecting a 2D binary-table, and nothing else */
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| (tab->tfields == 0)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    error(EXIT_FAILURE, "*Error*: Not a binary table in ", cat->filename);

/* Size and number of lines in the binary table */
  larray = tab->naxisn[0];
  narray = tab->naxisn[1];

  nb = 0;
  if ((mask2 = mask))
    {
    for (i=narray; i--;)
      if (*(mask2++))
        nb++;
    }

  if (!keynames)
    nkeys = tab->nkey;
  QCALLOC(key_col, int, nkeys);
  if (keynames) {
    for (i=0;i<nkeys;i++)
       if ((t=strchr(keynames[i], ')'))!=NULL) {
          *t='\0'; 
          t=strchr(keynames[i], '(');
          *t='\0'; 
          key_col[i] = atoi(++t);
       }
  }
/* Allocate memory to store the list of keys to be read */
  kflag = 0;
  if (!keys)
    {
    QMALLOC(keys, keystruct *, nkeys);
    kflag = 1;
    }

  n=1;
  switch (o_type) {
  case SHOW_ASCII:
              if (leadflag)
                 fprintf(stream, "# %3d %-15.15s %.47s\n", n++, 
                                 "(row_pos)", "running row");
              break;
  case SHOW_SKYCAT:
              fprintf(stream, skycathead, 6.0);
              break;
  }

/* Allocate memory for the arrays */
  maxnbytes = 0;
  ckeys = keys;
  if (keynames)
    for (i=nkeys; i--;)
      {
      if ((key = name_to_key(tab, *(keynames++))))
        {
        for (map=objectmap; map->oldname[0]&&o_type == SHOW_SKYCAT; map++) {
            if (strcmp(key->name, map->oldname) == 0) {
	      strcpy(key->name,  map->newname);
           }
        }
        *(ckeys++) = key;
        switch (o_type) {
        case SHOW_ASCII:
              if (banflag)
                {
                if (*key->unit)
                  fprintf(stream, "# %3d %-19.19s %-47.47s [%s]\n",
                      n, key->name,key->comment, key->unit);
                else
                  fprintf(stream, "# %3d %-19.19s %.47s\n",
                      n, key->name,key->comment);
                n += key->nbytes/t_size[key->ttype];
              }
              break;
        case SHOW_SKYCAT:
              if (key->nbytes/t_size[key->ttype] > 1)
                 for (j=0;j<key->nbytes/t_size[key->ttype];j++)
                    fprintf(stream, "%s(%d)\t", key->name,j+1);
              else
                    fprintf(stream, "%s\t", key->name);
              break;
        }
        if (key->nbytes>maxnbytes)
          maxnbytes = key->nbytes;
        }
      else
        *(ckeys++) = NULL;
      }
  else
    {
    key = tab->key;
    for (i=nkeys; i--; key = key->nextkey)
      if (strflag || key->naxis==0)
        {
        for (map=objectmap; map->oldname[0]&&o_type == SHOW_SKYCAT; map++) {
           if (strcmp(key->name, map->oldname) == 0) {
              strcpy(key->name, map->newname);
           }
        }
        *(ckeys++) = key;
        switch (o_type) {
        case SHOW_ASCII:
              if (banflag)
                {
                if (*key->unit)
                  fprintf(stream, "# %3d %-19.19s %-47.47s [%s]\n",
                      n, key->name,key->comment, key->unit);
                else
                  fprintf(stream, "# %3d %-19.19s %.47s\n",
                      n, key->name,key->comment);
                n += key->nbytes/t_size[key->ttype];
                }
              break;
         case SHOW_SKYCAT:
              if (key->nbytes/t_size[key->ttype] > 1)
                 for (j=0;j<key->nbytes/t_size[key->ttype];j++)
                    fprintf(stream, "%s(%d)\t", key->name,j+1);
              else
                    fprintf(stream, "%s\t", key->name);
              break;
         }
           if (key->nbytes>maxnbytes)
             maxnbytes = key->nbytes;
           }
      else
        {
        switch (o_type) {
        case SHOW_ASCII:
              if (*key->unit)
                fprintf(stream, "#     %-19.19s %-47.47s [%s]\n",
	            	key->name,key->comment, key->unit);
              else
                fprintf(stream, "#     %-19.19s %.47s\n",
            		key->name,key->comment);
              break;
        case SHOW_SKYCAT:
              break;
        }
        *(ckeys++) = NULL;
        }
    }
   if (o_type == SHOW_SKYCAT)
      fprintf(stream, "\n------------------\n");

/* Allocate memory for the buffer where we put one line of data */
  QMALLOC(buf, char, larray);

/* Allocate memory for the buffer where we put one element */
  QMALLOC(rfield, char, maxnbytes);

/* Positioning to the first element */
  open_cat(cat, READ_ONLY);
  QFSEEK(cat->file, tab->bodypos , SEEK_SET, cat->filename);

/*read line by line*/
  n = 0;
  mask2 = mask;
  for (i=narray; i--;)
    {
    QFREAD(buf, larray, cat->file, cat->filename);
    if (!mask || *(mask2++))
      {
      ckeys = keys;
      if (leadflag)
        {
        fprintf(stream, "%d", ++n);
        if (nkeys)
          putc(' ', stream);
        }
      for (k=0; k<nkeys; k++)
        {
        if ((key = *(ckeys++)) && (strflag || key->naxis==0))
          {
          ptr = memcpy(rfield, buf+key->pos, key->nbytes);
          esize = t_size[key->ttype];
          nelem = key->nbytes/esize;
          if (bswapflag)
            swapbytes(ptr, esize, nelem);
          switch(key->ttype)
            {
            case T_SHORT:
              for (j = 0; j<nelem; j++, ptr += esize)
                {
                if (key_col[k] == 0 || key_col[k] == j+1) {
                   fprintf(stream, *key->printf?key->printf:"%d",
		   		*(short *)ptr);
                   if (j < nelem-1) {
                      switch (o_type) {
                      case SHOW_ASCII:
                         putc(' ', stream);
                         break;
                      case SHOW_SKYCAT:
                         putc('\t', stream);
                         break;
                      }
                      }
                   }
                }
              break;

            case T_LONG:
              for (j = 0; j<nelem; j++,ptr += esize)
                {
                if (key_col[k] == 0 || key_col[k] == j+1) {
                   fprintf(stream, *key->printf?key->printf:"%d",
		         		*(int *)ptr);
                   if (j < nelem-1) {
                      switch (o_type) {
                      case SHOW_ASCII:
                         putc(' ', stream);
                         break;
                      case SHOW_SKYCAT:
                         putc('\t', stream);
                         break;
                      }
                      }
                   }
                }
              break;

            case T_FLOAT:
              for (j = 0; j<nelem; j++,ptr += esize)
                {
                if (key_col[k] == 0 || key_col[k] == j+1) {
                    fprintf(stream, *key->printf?key->printf:"%g",
			*(float *)ptr);
                   if (j < nelem-1) {
                       switch (o_type) {
                       case SHOW_ASCII:
                          putc(' ', stream);
                          break;
                       case SHOW_SKYCAT:
                          putc('\t', stream);
                          break;
                       }
                       }
                   }
                }
              break;

            case T_DOUBLE:
              for (j = 0; j<nelem; j++,ptr += esize)
                {
                if (key_col[k] == 0 || key_col[k] == j+1) {
                   fprintf(stream, *key->printf?key->printf:"%f",
			*(double *)ptr);
                   if (j < nelem-1) {
                      switch (o_type) {
                      case SHOW_ASCII:
                         putc(' ', stream);
                         break;
                      case SHOW_SKYCAT:
                         putc('\t', stream);
                         break;
                      }
                      }
                   }
                }
              break;

            case T_BYTE:
              if (key->htype==H_BOOL)
                 for (j = 0; j<nelem; j++,ptr += esize)
                   {
                   if (key_col[k] == 0 || key_col[k] == j+1) {
                      if (*(char *)ptr)
                        fprintf(stream, "T");
                      else
                        fprintf(stream, "F");
                      }
                   }
              else
                for (j = 0; j<nelem; j++,ptr += esize)
                   {
                   if (key_col[k] == 0 || key_col[k] == j+1) {
                     fprintf(stream, *key->printf?key->printf:"%d",
			(int)*((unsigned char *)ptr));
                   if (j) {
                      switch (o_type) {
                      case SHOW_ASCII:
                         putc(' ', stream);
                         break;
                      case SHOW_SKYCAT:
                         putc('\t', stream);
                         break;
                      }
                      }
                    }
                 }
              break;

            case T_STRING:
              for (j = nelem; j-- && (c=(int)*ptr); ptr += esize)
                fprintf(stream, "%c", c);
              break;

            default:
              error(EXIT_FAILURE, "*FATAL ERROR*: Unknown FITS type in ",
			"show_keys()");
            break;
            }
          if (k < nkeys - 1) {
                   switch (o_type) {
                   case SHOW_ASCII:
                      putc(' ', stream);
                      break;
                   case SHOW_SKYCAT:
                      putc('\t', stream);
                      break;
                   }
                   }
          }
        }
      putc('\n', stream);
      }
    }
  free(key_col);
  free(buf);
  if (kflag)
    free(keys);
  if (o_type == SHOW_SKYCAT) 
     fprintf(stream, skycattail);
  return;
  }

