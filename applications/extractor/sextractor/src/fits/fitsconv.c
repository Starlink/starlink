/*
 				fitsconv.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	functions for converting LDAC FITS catalogs.
*
*	Last modify:	28/11/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"fitscat_defs.h"
#include	"fitscat.h"

/****** asc2bin_tab ***********************************************************
PROTO	tabstruct *asc2bin_tab(catstruct *catin, char *tabinname, 
        catstruct *catout, char *taboutname)
PURPOSE	Convert an ASCII table to a BINARY table
	which is then stored in a destination catalog.
INPUT	Pointer to the input catalog,
	Name of the input ASCII table,
	Pointer to the output catalog,
	Name of the output binary table.
OUTPUT	RETURN_OK if the ASCII table was transformed, and RETURN_ERROR
	otherwise.
NOTES	This function can be used to stick the binary translation of
        similar ASCII tables.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	28/11/98
 ***/
tabstruct *asc2bin_tab(catstruct *catin, char *tabinname, catstruct *catout,
		char *taboutname)

  {
   catstruct	*tabcat;
   keystruct	*key;
   tabstruct	*tabin,*tabout;
   static char	comment[82], keyword[16], ptr[82];
   h_type	htype;
   t_type	ttype;
   char		*buf, *lptr;
   int		i;

/*Convert the table name to a pointer*/
  if (!(tabin = name_to_tab(catin, tabinname, 0)))
    return NULL;

/*Get the original catalog*/
  tabcat = tabin->cat;

/*Create a new binary table*/
  tabout = new_tab(taboutname);

/*Alloc. mem. for the whole ASCII table at once (should not be very large)*/
  QMALLOC(buf, char, tabin->tabsize);
/*Now read all the elements from the original table*/
  if (open_cat(tabcat, READ_ONLY) != RETURN_OK)
    error(EXIT_FAILURE, "*Error*: Cannot access ", tabcat->filename);
  QFSEEK(tabcat->file, tabin->bodypos, SEEK_SET, tabcat->filename);
  QFREAD(buf, tabin->tabsize, tabcat->file, tabcat->filename);
  if (close_cat(tabcat) != RETURN_OK)
    error(EXIT_FAILURE, "*Error*: Problem while closing", tabcat->filename);
  lptr = buf;
  for (i=tabin->tabsize/80; i-- && strncmp(lptr, "END     ", 8);)
    {
/*Interprete the next FITS line */
    if (fitspick(lptr, keyword, ptr, &htype, &ttype, comment) != RETURN_OK) {
      char line[81];
      int  qflag=1;
      strncpy(line, lptr, 80);
      line[80] = '\0';
      QFPRINTF(OUTPUT, line);
      error(EXIT_FAILURE, "*Error*: incorrect FITS field in ",
	tabcat->filename);
    }
    if (htype != H_COMMENT)
      {
/*----Create a new key and fill it with the right parameters*/
      key = new_key(keyword);
      strcpy(key->comment, comment+strspn(comment, " "));
      key->htype = htype;
      key->ttype = ttype;
      key->nbytes = t_size[ttype];
/*----!!Temporary (?)  solution for STRINGS*/
      if (htype==H_STRING)
        {
        key->naxis = 1;
        QMALLOC(key->naxisn, int, 1);
        key->naxisn[0] = 32;
        key->nbytes *= key->naxisn[0];
        }
      key->nobj = 1;
/*----Allocate memory and copy data in the same time*/
      QMEMCPY(ptr, key->ptr, char, key->nbytes);
      if (add_key(key, tabout, 0)==RETURN_ERROR)
        {
        sprintf(comment, "%s keyword found twice in ",
		keyword);
        warning(comment, tabcat->filename);
        }
      }
    lptr += 80;
    }

  free(buf);

  update_tab(tabout);
  return tabout;
  }


/****** ttypeconv ************************************************************
PROTO	void    *ttypeconv(void *ptr, t_type ttypein, t_type ttypeout)
PURPOSE	Convert data from one type to another.
INPUT	Pointer to element to convert,
	t_type of the element to convert,
	t_type of the converted element.
OUTPUT	Pointer to the converted element.
NOTES	ttypeconv does not yet handle arrays.
        A NULL vector is returned if the conversion was unsuccessful.
AUTHOR  E. Bertin (IAP, Leiden observatory & ESO)
VERSION 15/01/98
 ***/

void	*ttypeconv(void *ptr, t_type ttypein, t_type ttypeout)

  {
   static union {char tbyte; short tshort; int tlong; float tfloat;
		 double tdouble; char tstring;} ival, oval;

#define		OUTCONV(x, y)		\
      switch(y)				\
         {				\
         case T_BYTE:			\
         case T_STRING:			\
           oval.tbyte = (char)x;	\
         case T_SHORT:			\
           oval.tshort = (short)x;	\
           return (void *)&oval;	\
         case T_LONG:			\
           oval.tlong = (int)x;		\
           return (void *)&oval;	\
         case T_FLOAT:			\
           oval.tfloat = (float)x;	\
           return (void *)&oval;	\
         case T_DOUBLE:			\
           oval.tdouble = (double)x;	\
           return (void *)&oval;	\
         default:			\
           return NULL;			\
         }

  if (ttypein == ttypeout)
    return ptr;

  switch(ttypein)
    {
    case T_BYTE:
    case T_STRING:
      ival.tbyte = *(char *)ptr;
      OUTCONV(ival.tbyte, ttypeout);
    case T_SHORT:
      ival.tshort = *(short *)ptr;
      OUTCONV(ival.tshort, ttypeout);
    case T_LONG:
      ival.tlong = *(int *)ptr;
      OUTCONV(ival.tlong, ttypeout);
    case T_FLOAT:
      ival.tfloat = *(float *)ptr;
      OUTCONV(ival.tfloat, ttypeout);
    case T_DOUBLE:
      ival.tdouble = *(double *)ptr;
      OUTCONV(ival.tdouble, ttypeout);
    default:
      return NULL;
    }

  }

