/*
 				fitsutil.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	functions for handling FITS keywords.
*
*	Last modify:	13/06/2002
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

char	histokeys[][12] = {"COMMENT ", "HISTORY ", "        ", ""};

/****** fitsadd ***************************************************************
PROTO	int fitsadd(char *fitsbuf, char *keyword, char *comment)
PURPOSE	Write a FITS keyword in a fits header.
INPUT	pointer to the FITS buffer,
	name of the keyword to be created,
	a comment to put beyond the slash, or next to a COMMENT or HISTORY.
OUTPUT	line position or RETURN_ERROR if the keyword is invalid.
NOTES	For all keywords except commentary ones (like COMMENT, HISTORY or
	blank), it is checked that they do not exist already.
	Enough memory should be provided for the FITS header to contain one
	more line of 80 char.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	13/04/2002
 ***/
int	fitsadd(char *fitsbuf, char *keyword, char *comment)

  {
   char    	*key_ptr;
   char		str[82];
   int     	headpos, headpos2, commentflag,
		i, n;


  if (strcspn(keyword, "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -_"))
    return RETURN_ERROR;
  commentflag = findkey(keyword, (char *)histokeys, 12)==RETURN_ERROR?0:1;
  if (commentflag || (headpos = fitsfind(fitsbuf, keyword))==RETURN_ERROR)
    {
    headpos2 = headpos = fitsfind(fitsbuf, "END     ");
/*-- Special case of NAXIS parameters */
    if (!strncmp(keyword, "NAXIS", 5) && keyword[5] && keyword[5] != ' ')
      {
      sscanf(keyword, "NAXIS%d", &n);
/*---- Look for all previous NAXIS parameters */
      for (i=n; i--;)
        {
        sprintf(str, "NAXIS%-3d", i);
        headpos=fitsfind(fitsbuf, str);
        if (headpos>0)
          break;
        }
      if (headpos<0)
/*---- Most likely keyword is NAXIS1 */
        headpos=fitsfind(fitsbuf, "NAXIS   ");
      if (headpos>0)
        headpos++;
      else
        return RETURN_ERROR;
      }
    key_ptr = fitsbuf+80*headpos;
    memmove(key_ptr+80, key_ptr, 80*(headpos2-headpos+1));

    if (commentflag)
      sprintf(str, "%-8.8s %-71.71s",
	keyword, comment?comment:" ");
    else
      sprintf(str, "%-8.8s=                      / %-47.47s",
	keyword, comment?comment:" ");

    memcpy(key_ptr, str, 80);
    }

  return headpos;
  }


/****** fitsfind **************************************************************
PROTO	int fitsfind(char *fitsbuf, char *keyword)
PURPOSE	Search for a FITS keyword in a FITS header.
INPUT	pointer to the FITS buffer,
	name of the keyword to search for.
OUTPUT	position in lines  of 80 char (0=first) of the keyword if it was
	found, RETURN_ERROR otherwise.
NOTES	The buffer MUST contain the ``END     '' keyword.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/02/96
 ***/
int	fitsfind(char *fitsbuf, char *keyword)

  {
   char	*ptr;
   int	i, len;

  len = strlen(keyword);
  for (i=0; strncmp(ptr=&fitsbuf[80*i], "END     ", 8); i++)
    if (!wstrncmp(ptr, keyword, len))
      return i;
  if (strncmp(keyword, "END     ", 8))
    return RETURN_ERROR;
  else
    return i;
  }


/****** fitsnfind *************************************************************
PROTO	char    *fitsnfind(char *fitsbuf, char *str, int nblock)
PURPOSE	Search for a FITS keyword in a fits header of nblock blocks.
INPUT	pointer to the FITS buffer,
	name of the keyword to search for,
	number of FITS blocks (2880 bytes each).
OUTPUT	pointer at the keyword position if it was found, NULL otherwise.
NOTES	No need for an ``END     '' keyword.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
char    *fitsnfind(char *fitsbuf, char *str, int nblock)
  {
   int  i;

  for (i=36*nblock;i--; fitsbuf+=80)
    if (!strncmp(fitsbuf, str, strlen(str)))
      return fitsbuf;

  return        (char *)NULL;
  }


/****** fitspick **************************************************************
PROTO	int fitspick(char *fitsline, char *keyword, void *ptr, h_type *htype,
			t_type *ttype, char *comment)

PURPOSE	Pick up FITS keyword,content,type and comment in a fits header line.
INPUT	pointer to the current line of FITS buffer,
	pointer to a char * (where to put the keyword),
	pointer to ``where to put the data'',
	pointer to ``where to put the h_type'',
	pointer to ``where to put the t_type'',
	pointer to a char * (where to put the comment).
OUTPUT	RETURN_OK if something was found, RETURN_ERROR otherwise.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory),
	E.R. Deul
        E.R. Deul - Handling of NaN
VERSION	21/04/2003
 ***/
int	fitspick(char *fitsline, char *keyword, void *ptr, h_type *htype,
		t_type *ttype, char *comment)

  {
   char *fptr, *cptr, c, *lastspace;
   int	i, toggle;

  *((char *)ptr) = 0;
/*First, get the keyword*/
  memcpy(keyword, fitsline, 8);
  keyword[8] = 0;

/*Handle comments*/
  if ((int)fitsline[8] != '=')
    {
    if (strncmp(keyword, "COMMENT ", 8)
	&& strncmp(keyword, "HISTORY ", 8)
	&& strncmp(keyword, "HIERARCH", 8)
	&& strncmp(keyword, "        ", 8))
      return RETURN_ERROR;
    memcpy(comment, fitsline+9, 71);
    comment[71] = 0;
    *htype = H_COMMENT;
    *ttype = T_STRING;
    return RETURN_OK;
    }
  
/*Handle character strings*/
  if ((int)fitsline[10] == '\'')
    {
    cptr = ptr;
    for (fptr = fitsline + (i=11); i<80; i++)
      {
      if (*fptr==(char)'\'')
        {
        if (i++>=79 || *(fptr+1)!=(char)'\'')
          break;
        else
          fptr++;
        }
      *cptr++ = *fptr++;
      }
    *cptr = 0;
/*-- Check if there is a trailing space */
    *htype = (cptr != ptr && *(cptr-1)==' ') ? H_STRINGS: H_STRING;
    *ttype = T_STRING;
    }
/*Handle booleans*/
  else if (fitsline[29] == (char)'T' || fitsline[29] == (char)'F')
    {
    *((BYTE *)ptr) = fitsline[29]==(char)'T'?1:0;
    *htype = H_BOOL;
    *ttype = T_BYTE;
    }
  else
    {
    for (fptr = fitsline + (i=10); i<30 && *(fptr++)!=(char)'.'; i++);
/*Handle floats*/
    if (i<30
	|| !strncmp(keyword, "CRVAL",5)
	|| !strncmp(keyword, "2MULST",6)
	|| !strncmp(keyword, "2MUUTIME",8)
	|| !strncmp(keyword, "2MUAZIMU",8)
        || strncmp(fitsline+11, "NaN", 3) == 0) 
			/* ^--- He!! temporary fix EB 20/03/96*/
			/* ^--- He!! temporary fix ED 24/12/96*/
      {
      if (strncmp(fitsline+11, "NaN", 3) == 0) {
         *((double *)ptr) = -1.e-100;
      } else {
         fixexponent(fitsline);
         *((double *)ptr) = atof(fitsline+10);
      }
      *htype = H_EXPO;
      *ttype = T_DOUBLE;
      }
    else
/*Handle ints*/
      {
      *((int *)ptr) = atoi(fitsline+10);
      *htype = H_INT;
      *ttype = T_LONG;
      }
    }

/*Store comment if it is found*/
  toggle = 0;
  lastspace = NULL;
  for (fptr = fitsline + (i=10); i<80; i++)
    {
    if (*fptr == (char)'\'')
      toggle^=toggle;
    if (*(fptr++) == (char)'/' && !toggle)
      {
      while (++i<80 && *fptr<=' ')
        fptr++;
      i--;
      while (++i<80)
        if ((c=*(fptr++))>= ' ')
	  {
          *(comment++) = c;
          if (c>' ')
            lastspace = comment;
          }
      }
    }
  if (lastspace)
    *lastspace = '\0';
  else
    *comment = '\0';

  return RETURN_OK;
  }


/****** fitsread **************************************************************
PROTO	int fitsread(char *fitsbuf, char *keyword, void *ptr, h_type htype,
			t_type ttype)
PURPOSE	Read a FITS keyword in a fits header.
INPUT	pointer to the FITS buffer,
	name of the keyword to be read,
	pointer where to put the read data,
	h_type of the data to be read (see fitscat.h),
	t_type of the data to be read (see fitscat.h).
OUTPUT	RETURN_OK if the keyword was found, RETURN_ERROR otherwise.
NOTES	The buffer MUST contain the ``END     '' keyword.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/06/99
 ***/
int	fitsread(char *fitsbuf, char *keyword, void *ptr, h_type htype,
		t_type ttype)

  {
   int		i,pos;
   char		s[4], str[82];
   char		*st, *st2;

  if ((pos = fitsfind(fitsbuf, keyword)) < 0)
    return RETURN_ERROR;

  strncpy(str,fitsbuf+80*pos,80);
  str[80] = '\0';

  switch(htype)
    {
    case H_INT:		if (ttype == T_SHORT)
			  sscanf(str+10, "    %hd", (short *)ptr);
			else
			  sscanf(str+10, "    %d", (LONG *)ptr);
			break;

    case H_FLOAT:
    case H_EXPO:	fixexponent(str);
			if (ttype == T_DOUBLE)
			  sscanf(str+10, "    %lf", (double *)ptr);
			else
			  sscanf(str+10, "    %f", (float *)ptr);
			break;

    case H_BOOL:	sscanf(str+10, "%1s", s);
			*(int *)ptr = ((int)s[0] == 'T') ? 1 : 0;
			break;

    case H_STRING:	st = ptr;
			st2= str+10;
			for (i=70; i-- && *(st2++)!=(char)'\'';);
			while (i-->0)
			  {
			  if (*st2 == '\'' && *(++st2) != '\'')
			    break;
			  *(st++) = *(st2++);
			  }
			do
			  {
			  *(st--) = (char)'\0';
			  } while (st>(char *)ptr && (*st == (char)' '));
			break;

    case H_STRINGS:	st = ptr;
			st2= str+10;
			for (i=70; i-- && *(st2++)!=(char)'\'';);
			while (i-->0)
			  {
			  if (*st2 == '\'' && *(++st2) != '\'')
			    break;
			  *(st++) = *(st2++);
			  }
			*st = (char)'\0';
			break;

    case H_COMMENT:	strcpy(ptr,str+9);
			break;

    case H_HCOMMENT:	strcpy(ptr,str+33);
			break;

    default:		error(EXIT_FAILURE,
				"*Internal Error*: Unknown FITS type in ",
				"fitsread()");
			break;
    }

  return RETURN_OK;
  }


/****** fitsremove ************************************************************
PROTO	int fitsremove(char *fitsbuf, char *keyword)
PURPOSE	Remove one (or more) FITS keyword from a fits header.
INPUT	pointer to the FITS buffer,
	name of the keyword to be created.
OUTPUT	RETURN_OK if the keyword was found, RETURN_ERROR otherwise.
NOTES	'?' wildcard allowed;
	Don't remove the ``END'' keyword with this!!!
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	08/04/99
 ***/

int	fitsremove(char *fitsbuf, char *keyword)

  {
   char	*cp1,*cp2;
   int	endpos,pos, i,n;

  endpos = fitsfind(fitsbuf, "END     ");
  for (n=0; (pos = fitsfind(fitsbuf, keyword))>=0; n++, endpos--)
    for (cp1=fitsbuf+80*(pos+1), cp2=fitsbuf+80*pos, i=80*(endpos - pos); i--;)
      *(cp2++) = *(cp1++);

  if (!n)  
    return RETURN_ERROR;

  memset(fitsbuf+80*(endpos+1), ' ', 80*n);

  return RETURN_OK;
  }


/****** fitswrite *************************************************************
PROTO	int fitswrite(char *fitsbuf, char *keyword, void *ptr, h_type htype,
			t_type ttype)
PURPOSE	Write a FITS keyword in a fits header.
INPUT	pointer to the FITS buffer,
	name of the keyword to be written,
	pointer where to retrieve the  data,
	h_type of the data to be written (see fitscat.h),
	t_type of the data to be written (see fitscat.h).
OUTPUT	RETURN_OK if the keyword was found, RETURN_ERROR otherwise.
NOTES	The buffer MUST contain the ``END     '' keyword.
	The keyword must already exist in the buffer (use fitsadd()).
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	21/04/2003
 ***/
int	fitswrite(char *fitsbuf, char *keyword, void *ptr, h_type htype,
		t_type ttype)

  {
   int		i, l, pos, posoff, flag;
   char		str[81],str2[81];
   char		*cstr, *cstr1,*cstr2,
		c;

/* Ignore HISTORY and COMMENTS */
  if (findkey(keyword, (char *)histokeys, 12)!=RETURN_ERROR
	|| (pos = fitsfind(fitsbuf, keyword)) < 0)
    return RETURN_ERROR;
  posoff = 10;
  fitsbuf += 80*pos;
  switch(htype)
    {
    case H_INT:	sprintf(str, "%20d", (ttype==T_SHORT)?
				*(short *)ptr: *(int *)ptr);
			break;

    case H_FLOAT:	sprintf(str, "        %12.4f", (ttype==T_DOUBLE)?
				*(double *)ptr: *(float *)ptr);
			break;

    case H_EXPO:	sprintf(str, "    %16.9e", (ttype==T_DOUBLE)?
				*(double *)ptr: *(float *)ptr);
			break;

    case H_BOOL:	if (*(int *)ptr)
			  sprintf(str, "                   T");
			else
			  sprintf(str, "                   F");
			break;

    case H_STRING:	/* Handle the famous quote */
			cstr1 = (char *)ptr;
			cstr2 = str2;
			for (i=0; i<80; i++)
			  if (!(c=*(cstr2++) = *(cstr1++)))
			    break;
			  else if (c == '\'')
			    {
			    *(cstr2++) = '\'';
			    i++;
			    }
			if (strlen(str2)<=18)
			  {
			  sprintf(str, "'%-18.18s ", str2);
			  cstr = str+18;
			  i = 10;
			  }
			else
			  {
			  sprintf(str, "'%-68.68s ", str2);
			  cstr = str+68;
			  i = 60;
			  }
                        for (; i-- && *cstr==(char)' '; cstr--);
                        *(++cstr) = (char)'\'';
                        if (i>9)
                          *(++cstr) = 0;
			break;

    case H_STRINGS:	/* Handle the famous quote */
			cstr1 = (char *)ptr;
			cstr2 = str2;
			for (i=0; i<80; i++)
			  if (!(c=*(cstr2++) = *(cstr1++)))
			    break;
			  else if (c == '\'')
			    {
			    *(cstr2++) = '\'';
			    i++;
			    }
		        sprintf(str, "'%s'", str2);
                        for (i+=2;i<20; i++)
                          str[i]=' ';
                        str[i] = '\0';
			break;

    case H_COMMENT:	sprintf(str, "%-70s", (char *)ptr);
			posoff = 9;
			break;

			/* Special case of ``half-comments'' */
    case H_HCOMMENT:	sprintf(str, " / %-47s", (char *)ptr);
			posoff = 30;
			break;

    default:		error(EXIT_FAILURE,
				"*FATAL ERROR*: Unknown FITS type in ",
				"fitswrite()");
			break;
    }


/* Now the tricky problem of (former) comments */
  flag=1;
  cstr = fitsbuf+10;
  for (i=71; --i; cstr++)
    {
    if (*cstr=='\'')
      flag ^= 1;
    else if (flag && *cstr=='/')
      break;
    }
  if (posoff==10 && i && (l=69-strlen(str))>0)
    {
    strncpy(str2, cstr, i);
    str2[i+1] = 0;
    strcat(str, " ");
    strncat(str, str2, l);
    }

  memset(fitsbuf+9, ' ', 71);
  fitsbuf += posoff;

/* Finally copy the result to the right place (except the trailing zero) */
  for (cstr = str; *cstr; *(fitsbuf++) = *(cstr++));

  return RETURN_OK;
  }


/****** fixexponent ***********************************************************
PROTO	void fixexponent(char *s)
PURPOSE	Replaces the FORTRAN 'D' exponent sign to 'E' in a FITS line.
INPUT	FITS line
OUTPUT	-.
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	25/04/97
 ***/
void	fixexponent(char *s)

  {
   int	i;

  s += 9;
  for (i=71; (int)*s != '/' && i--; s++)
    if ((int)*s == 'D' || (int)*s == 'd')
      *s = (char)'E';

  return;
  }


