/*
 				fitstab.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	general functions for handling LDAC FITS catalogs.
*
*	Last modify:	30/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"fitscat_defs.h"
#include	"fitscat.h"

/****** about_tab **************************************************************
PROTO	int about_tab(catstruct *cat, char *tabname, FILE *stream)
PURPOSE	Print information concerning a tab structure.
INPUT	Pointer to the input catalog,
	table name,
	an output stream.
OUTPUT	RETURN_OK if the table was found, RETURN_ERROR otherwise.
NOTES	-.
AUTHOR	E.R. Deul(Leiden observatory),
	E. Bertin (IAP & Leiden observatory): return value modified.
	E.R. Deul(Leiden observatory): output units
VERSION	29/07/97
 ***/
int about_tab(catstruct *cat, char *tabname, FILE *stream)
{
   tabstruct *tab;
   keystruct *key;
   int		i, j;

   if (tab = name_to_tab(cat, tabname, 0)) {
       fprintf(stream, "Table %s\n", tabname);
      for (i=0, key=tab->key; i<tab->nkey; i++,key=key->nextkey)
    {
    fprintf(stream,
	"******	Key #%d\n", i+1);
    fprintf(stream,
	"	Key name:...............%s\n", key->name);
    fprintf(stream,
	"	Key comment:............%s\n", key->comment);
    fprintf(stream,
	"	Key type:...............");
    switch (key->ttype) {
    case T_BYTE: fprintf(stream,"Byte"); break;
    case T_SHORT: fprintf(stream,"Short Int"); break;
    case T_LONG: fprintf(stream,"Long Int"); break;
    case T_FLOAT: fprintf(stream,"Float"); break;
    case T_DOUBLE: fprintf(stream,"Double"); break;
    case T_STRING: fprintf(stream,"String"); break;
    }
    fprintf(stream,"\n");
    fprintf(stream,
	"	Key dimension:..........%d ", key->naxis);
    if (key->naxis) fprintf(stream, "(");
    for (j=0;j<key->naxis;j++) {
       if (j>0) fprintf(stream, " ");
       fprintf(stream, "%d", key->naxisn[j]);
    }
    if (key->naxis) fprintf(stream, ")");
    fprintf(stream, "\n");
    if (key->unit[0] != '\0') 
       fprintf(stream,
	"	Key unit:...............%s\n", key->unit);
    }
   } else return RETURN_ERROR;

   return RETURN_OK;
}

/****** add_tab ****************************************************************
PROTO	int add_tab(tabstruct *tab, catstruct *cat, int pos)
PURPOSE	Add a table to a catalog.
INPUT	Pointer to the table,
	Pointer to the destination catalog,
	Position (1= first after the primary HDU, <=0 = at the end).
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise.
NOTES	Only 1-segment tables are accepted. To copy multi-segment tables,
	use copy_tab() instead.
	If a table with the same name and basic attributes already exists in
	the destination catalog, then the new table is appended to it.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
int	add_tab(tabstruct *tab, catstruct *cat, int pos)

  {
   tabstruct	*outtab, *prevtab;
   int		i;

/*Check if a similar table doesn't already exist in the dest. cat */
  if ((outtab = name_to_tab(cat, tab->extname, 0)))
    {
    if ((outtab->naxis != 2)
	|| (outtab->bitpix!=8)
	|| strcmp(outtab->xtension,tab->xtension)
	|| (outtab->tfields != tab->tfields)
        || (outtab->naxisn[0] != tab->naxisn[0]))
      return RETURN_ERROR;

    prevtab = outtab;
    for (i=outtab->nseg-1; i--;)
      prevtab = prevtab->nexttab;
    tab->seg = prevtab->seg+1;
    tab->nseg = 0;
    outtab->nseg++;
    }
  else
    {
    prevtab = pos_to_tab(cat, pos, 0)->prevtab;
    cat->ntab++;
    }

  (tab->nexttab = (tab->prevtab = prevtab)->nexttab)->prevtab = tab;
  prevtab->nexttab = tab;

  return RETURN_OK;
  }


/****** copy_tab ***************************************************************
PROTO	int copy_tab(catstruct *catin, char *tabname, int seg,
		catstruct *catout, int pos)
PURPOSE	Copy a table from one catalog to another.
INPUT	Pointer to the original catalog,
	Name of the table,
	Table segment (0 = all),
	Pointer to the destination catalog,
	Position (1= first after the primary HDU, <=0 = at the end)
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise.
NOTES	If a table with the same name and basic attributes already exists in
	the destination catalog, then the original table is appended to it.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
int	copy_tab(catstruct *catin, char *tabname, int seg,
		catstruct *catout, int pos)

  {
   keystruct	*key;
   tabstruct	*outtab, *prevtab, *nexttab, *tabin,*tabout;
   int		i,j, nseg;

/*Convert the table name to a pointer*/
  if (!(tabin = name_to_tab(catin, tabname, seg)))
    return RETURN_ERROR;

  nseg = seg?1:tabin->nseg;

/*Check if a similar table doesn't already exist in the dest. cat */
  if ((outtab = name_to_tab(catout, tabname, 0)))
    {
    if ((outtab->naxis != 2)
	|| (outtab->bitpix!=8)
	|| strcmp(outtab->xtension,tabin->xtension)
	|| (outtab->tfields != tabin->tfields)
        || (outtab->naxisn[0] != tabin->naxisn[0]))
      return RETURN_ERROR;
    prevtab = outtab;
    for (i=0; i<outtab->nseg-1; i++)
      prevtab = prevtab->nexttab;
    nexttab = prevtab->nexttab;
    outtab->nseg += nseg;
    }
  else
    {
    prevtab = nexttab = NULL;
    catout->ntab++;
    }

/*Now copy each segment of the original table*/
  for (i=nseg; i--;)
     {
/*---First, allocate memory and copy data */
     QCALLOC(tabout, tabstruct, 1);
     *tabout = *tabin;
     if (tabin->naxis)
       QMEMCPY(tabin->naxisn, tabout->naxisn, int, tabin->naxis);
     if (tabin->headbuf)
       QMEMCPY(tabin->headbuf, tabout->headbuf, char, tabin->headnblock*FBSIZE);
     if (tabin->bodybuf)
       QMEMCPY(tabin->bodybuf, tabout->bodybuf, char, tabin->tabsize);

     key = tabin->key;
     tabout->key = NULL;
     tabout->nkey = 0;
     for (j=tabin->nkey; j--;)
       {
       copy_key(tabin, key->name, tabout, 0);
       key = key->nextkey;
       }

/*---Then, update the links */
     if (prevtab)
       {
       prevtab->nexttab = tabout;
       tabout->prevtab = prevtab;
       tabout->seg = prevtab->seg+1;
       tabout->nseg = 0;
       }
     else
       {
       outtab = tabout;
       outtab->prevtab = NULL;
       tabout->seg = 1;
       }
     tabin = tabin->nexttab;
     prevtab = tabout;
     }

/*place the new chain of table-segments within the catalog (tricky, isn't it?)*/
  if (!nexttab)
/*--if the table is new */
    {
    nexttab = pos_to_tab(catout, pos, 0);
    outtab->prevtab = nexttab->prevtab;
    nexttab->prevtab->nexttab = outtab;
    }

  prevtab->nexttab = nexttab;
  nexttab->prevtab = prevtab;

  return RETURN_OK;
  }


/****** copy_tabs **************************************************************
PROTO	int copy_tabs(catstruct *catin, catstruct *catout)
PURPOSE	Copy all tables from one catalog to another.
INPUT	Pointer to the original catalog,
	Pointer to the destination catalog,
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise
	(for instance if there were tabs that were not binary-tables, and
	therefore that were not copied).
NOTES	If a table with the same name and basic attributes already exists in
	the destination catalog, then the original table is appended to it.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
int	copy_tabs(catstruct *catin, catstruct *catout)

  {
   tabstruct	*tab;
   int		i, flag;

  if (!catin->tab)
    return RETURN_ERROR;

  tab = catin->tab->nexttab;	/* skip the primary header */
  flag = RETURN_OK;
  for (i=catin->ntab-1; i--;)
    {
    flag |= copy_tab(catin, tab->extname, 0, catout, 0);
    while (!(tab=tab->nexttab)->nseg);
    }

  return flag;
  }

/****** free_tab ***************************************************************
PROTO	void free_tab(tabstruct *tab)
PURPOSE	Free memory associated to a table pointer.
INPUT	Pointer to the table.
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/02/96
 ***/
void	free_tab(tabstruct *tab)

  {

  free(tab->naxisn);
  free(tab->headbuf);
  free(tab->bodybuf);
  remove_keys(tab);
  free(tab);

  return;
  }


/****** new_tab ****************************************************************
PROTO	tabstruct *new_tab(char *tabname)
PURPOSE	Create a new binary table.
INPUT	Name.
OUTPUT	A pointer to the new table.
NOTES	A defaut header is also created.
	No links are initialized.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
tabstruct	*new_tab(char *tabname)

  {
   static char	bintabtemplate[][80] = {
"XTENSION= 'BINTABLE'           / THIS IS A BINARY TABLE (FROM THE LDACTOOLS)",
"BITPIX  =                    8 / ",
"NAXIS   =                    2 / ",
"NAXIS1  =                    0 / BYTES PER ROW",
"NAXIS2  =                    0 / NUMBER OF ROWS",
"PCOUNT  =                    0 / RANDOM PARAMETER COUNT",
"GCOUNT  =                    1 / GROUP COUNT",
"TFIELDS =                    0 / FIELDS PER ROWS",
"EXTNAME = 'WHOCARES'           / TABLE NAME", 
"END                            "};
   tabstruct	*tab;
   char		*buf;
   int		i;

  QCALLOC(tab, tabstruct, 1);
  strcpy(tab->xtension, "BINTABLE");
  strcpy(tab->extname, tabname);
  tab->naxis = 2;
  QCALLOC(tab->naxisn, int, tab->naxis);
  tab->bitpix = 8;
  tab->bytepix = 1;
  tab->pcount = 0;
  tab->gcount = 1;
  tab->seg = 1;
  tab->nseg = 1;
/*Provide a new header*/
  QCALLOC(tab->headbuf, char, FBSIZE);
  memcpy(tab->headbuf, bintabtemplate, sizeof(bintabtemplate));
  for (buf = tab->headbuf, i=FBSIZE; i--; buf++)
    if (!*buf)
      *buf = ' ';
  tab->headnblock = 1;

  return tab;
  }


/****** remove_tab *************************************************************
PROTO	int remove_tab(catstruct *cat, char *tabname, int seg)
PURPOSE	Remove a table from a catalog.
INPUT	Pointer to the catalog,
	Name of the table,
	Table segment (0 = all).
OUTPUT	RETURN_OK if everything went as expected, and RETURN_ERROR otherwise.
NOTES	If tabname = "", the last table from the list is removed.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	30/11/98
 ***/
int	remove_tab(catstruct *cat, char *tabname, int seg)

  {
   tabstruct	*tab, *prevtab, *nexttab;
   int		i,nseg;

  if (!tabname || !cat->ntab || !cat->tab)
    return RETURN_ERROR;

  if (tabname[0])
    {
/*--Convert the table name to a pointer*/
    if (!(tab = name_to_tab(cat, tabname, seg)))
      return RETURN_ERROR;
/*--a small trick to simplify decisions afterwards*/
    if (seg && tab->nseg==1)
      seg = 0;
    }
  else
    {
    tab = cat->tab->prevtab;
    if (!seg)
      for (;!tab->nseg; tab = tab->prevtab);
    }

  prevtab = tab->prevtab;
  nseg = seg?1:tab->nseg;

/*Free memory for each table segment*/
  for (i=nseg; i--;)
    {
    nexttab = tab->nexttab;
    if (cat->tab == tab)
      cat->tab = nexttab;
    free_tab(tab);
    tab = nexttab;
    }

  if (!seg)
    if (!--cat->ntab)
      {
      cat->tab = NULL;
      return RETURN_OK;  
      }

/*update the links of neighbours*/
  nexttab->prevtab = prevtab;
  prevtab->nexttab = nexttab;

  if (seg)
/*--update status for each table segment*/
    {
    for (tab=prevtab;!tab->nseg; tab = tab->prevtab);
    for (nexttab=tab->nexttab,i=2;!nexttab->nseg;nexttab=nexttab->nexttab,i++);
      nexttab->seg = i;
    tab->nseg = i;
    tab->seg = 1;
    }

  return RETURN_OK;  
  }


/****** remove_tabs ************************************************************
PROTO	int remove_tabs(catstruct *cat)
PURPOSE	Remove all tables from a catalog.
INPUT	Pointer to the catalog.
OUTPUT	RETURN_OK if tabs were found, and RETURN_ERROR otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/05/97
 ***/
int	remove_tabs(catstruct *cat)

  {
   int	t;

  if (!cat->tab)
    return RETURN_ERROR;

  for (t=cat->ntab; t--;)
    remove_tab(cat, "",0);

  return RETURN_OK;
  }


/****** update_tab *************************************************************
PROTO	int update_tab(tabstruct *tab)
PURPOSE	Update a table according to what's in the keys.
INPUT	Table structure.
OUTPUT	RETURN_OK if tab is a binary table, or RETURN_ERROR otherwise.
NOTES	The headbuf pointer in the catstruct might be reallocated.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/02/97
 ***/
int	update_tab(tabstruct *tab)

  {
   tabstruct	*keytab;
   keystruct	*key;
   int		i,j, nobj, nbytes;

/*Just pass if not a binary table*/
  if ((tab->naxis != 2)
	|| (tab->bitpix!=8)
	|| strncmp(tab->xtension, "BINTABLE", 8))
    return RETURN_ERROR;

/*Well, not much to do if there are no keys!*/
  if (!(key = tab->key))
    return RETURN_OK;

  nobj = -1;
  keytab = NULL;
  nbytes = 0;
  for (i=tab->nkey; i--;)
    {
    if (keytab && !key->ptr && key->tab != keytab)
      error(EXIT_FAILURE, "*Error*: wrong reference table in ",
	key->name);
    if (nobj!=-1 && (nobj != key->nobj))
      error(EXIT_FAILURE, "*Error*: wrong number of elements in key ",
	key->name);
    keytab = key->tab;
    nobj = key->nobj;
/*-- If the number of bytes per element is not set, recover it */
    if (!key->nbytes)
      {
      key->nbytes = t_size[key->ttype];
      for (j=key->naxis; j--;)
        key->nbytes *= key->naxisn[j];
      }
    nbytes += key->nbytes;
    key = key->nextkey;
    }

  tab->tabsize = nobj*nbytes;
  tab->naxisn[0] = nbytes;
  tab->naxisn[1] = nobj;
  tab->tfields = tab->nkey;

  return RETURN_OK;
  }


/****** name_to_tab ************************************************************
PROTO	tabstruct *name_to_tab(catstruct *cat, char *tabname, int seg)
PURPOSE	Name search of a table in a catalog.
INPUT	Pointer to the catalog,
	Table name,
	Table segment (0 = first).
OUTPUT	The table pointer if the name was matched, and NULL otherwise.
NOTES	-
VERSION	25/04/97
 ***/
tabstruct	*name_to_tab(catstruct *cat, char *tabname, int seg)

  {
   tabstruct	*tab;
   int		i;

  tab = cat->tab;
  for (i=cat->ntab; strcmp(tabname,tab->extname) && i--;)
    while (!(tab=tab->nexttab)->nseg);

  if (i<0)
    return NULL;

  if (seg)
    {
    for (;tab->seg!=seg && !tab->nexttab->nseg; tab=tab->nexttab);
    return tab->seg==seg?tab:NULL;
    }

  return tab;
  }


/****** pos_to_tab *************************************************************
PROTO	tabstruct *pos_to_tab(catstruct *cat, int pos, int seg)
PURPOSE	Position search of a table in a catalog.
INPUT	Pointer to the catalog,
	Position of the table,
	Table segment (0 = first).
OUTPUT	The table pointer if the table exists at the given position, and the
	pointer to the primary ``table'' otherwise.
NOTES	pos = 1 means the first table after the primary one.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/02/96
 ***/
tabstruct	*pos_to_tab(catstruct *cat, int pos, int seg)

  {
   tabstruct	*tab;
   int		i;

  tab = cat->tab;
  for (i=0; i!=pos && i<cat->ntab; i++)
    while (!(tab=tab->nexttab)->nseg);

  if (seg)
    for (;tab->seg!=seg && !tab->nexttab->nseg; tab=tab->nexttab);

  return i<cat->ntab?tab:cat->tab;
  }

/****** tabs_list **************************************************************
PROTO	char **tabs_list(catstruct *cat, int *n)
PURPOSE	List all tables in a catalog.
INPUT	Pointer to the catalog,
	Pointer to the number of names in that list.
OUTPUT	A list of all table names.
NOTES	-.
AUTHOR	E.R. Deul (Leiden observatory)
VERSION	??/??/96
 ***/
char **tabs_list(catstruct *cat, int *n)

  {
   tabstruct	*tab;
   int		i;
   char 	**names;

  tab = cat->tab;
  QCALLOC(names, char *, cat->ntab);
  for (i=0; i<cat->ntab; i++) {
    QCALLOC(names[i], char, MAXCHARS);
    strcpy(names[i],tab->extname);
    while (!(tab=tab->nexttab)->nseg);
  }
  *n = cat->ntab;
  return names;
  }

/****** tabs_row_len ***********************************************************
PROTO	int tab_row_len(char *file, char *tabname)
PURPOSE	Return the row length in bytes of a given table in a given catalog.
INPUT	File pointer.
OUTPUT	Table size (bytes)
NOTES	-.
AUTHOR	E.R. Deul (Leiden observatory)
VERSION	??/??/96
 ***/
int tab_row_len(char *file, char *tabname)

{
    catstruct   *tcat;
    tabstruct   *tab;
    int		retcode = -1;

    if ((tcat = read_cat(file)) != NULL) {
       if ((tab = name_to_tab(tcat, tabname, 0)) != NULL) {
          retcode = tab->naxisn[0];
	  free_tab(tab);
       }
       close_cat(tcat);
       free_cat(tcat,1);
    }
    return retcode;
}

