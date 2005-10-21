 /*
 				fitsmisc.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	miscellaneous functions.
*
*	Last modify:	13/06/2002
*                       15/03/2004 (PWD): Adam versions of error and warning.
*	Last modify:	14/05/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include        <setjmp.h>

#ifdef HAVE_MPI
#include	<mpi.h>
#endif

#include	"fitscat_defs.h"
#include	"fitscat.h"

#include        "merswrap.h"
#include        "sae_par.h"
#include        "adam_defs.h"

extern jmp_buf          env;

/********************************* error ************************************/
/*
Version for ADAM

I hope it will never be used!
*/

void    error(int num, char *msg1, char *msg2)
  {
  int status;

  status = SAI__ERROR;
  msgSetc( "TOK1", msg1 );
  msgSetc( "TOK2", msg2 );
  errRep( " ", "^TOK1 ^TOK2", &status );

#ifdef HAVE_MPI
  MPI_Finalize();
#endif

  longjmp( env, num );
  }

/********************************* warning **********************************/
/*
Print a warning message on screen.
*/
void    warning(char *msg1, char *msg2)
  {
  AFPRINTF(OUTPUT, "\n> WARNING: %s%s\n\n",msg1,msg2);
  return;
  }


/******************************* swapbytes **********************************/
/*
Swap bytes for doubles, longs and shorts (for DEC machines or PC for inst.).
*/
void    swapbytes(void *ptr, int nb, int n)
  {
   char *cp;
   int  j;

  cp = (char *)ptr;

  if (nb&4)
    {
    for (j=n; j--; cp+=4)
      {
      cp[0] ^= (cp[3]^=(cp[0]^=cp[3]));
      cp[1] ^= (cp[2]^=(cp[1]^=cp[2]));
      }
    return;
    }

  if (nb&2)
    {
    for (j=n; j--; cp+=2)
      cp[0] ^= (cp[1]^=(cp[0]^=cp[1]));
    return;
    }

  if (nb&1)
    return;

  if (nb&8)
    {
    for (j=n; j--; cp+=8)
      {
      cp[0] ^= (cp[7]^=(cp[0]^=cp[7]));
      cp[1] ^= (cp[6]^=(cp[1]^=cp[6]));
      cp[2] ^= (cp[5]^=(cp[2]^=cp[5]));
      cp[3] ^= (cp[4]^=(cp[3]^=cp[4]));
      }
    return;
    }

  error(EXIT_FAILURE, "*Internal Error*: Unknown size in ", "swapbytes()");

  return;
  }


/****** wstrncmp ***************************************************************
PROTO	int wstrncmp(char *cs, char *ct, int n)
PURPOSE	simple wildcard strcmp.
INPUT	character string 1,
	character string 2,
	maximum number of characters to be compared.
OUTPUT	comparison integer (same meaning as strcmp).
NOTES	-.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/02/96
 ***/
int	wstrncmp(char *cs, char *ct, int n)

  {
   int	diff,i;

  i = n;
  diff = 0;
  do
    {
    diff = ((*cs=='?'&&*ct)||(*ct=='?'&&*cs))?0:*cs-*ct;
    } while (!diff && --i && *(cs++) && *(ct++));

  return diff;
  }


/****** findkey ****************************************************************
PROTO	int findkey(char *str, char *key, int size)
PURPOSE	Find an item within a list of keywords.
INPUT	character string,
	an array of character strings containing the list of keywords,
	offset (in char) between each keyword.
OUTPUT	position in the list (0 = first) if keyword matched,
	RETURN_ERROR otherwise.
NOTES	the matching is case-sensitive.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/02/96
 ***/
int	findkey(char *str, char *key, int size)

  {
  int i;

  for (i=0; key[0]; i++, key += size)
    if (!strcmp(str, key))
      return i;

  return RETURN_ERROR;
  }


/********************************* findnkey **********************************
PROTO	int findnkey(char *str, char *key, int size, int nkey)
PURPOSE	Find an item within a list of nkey keywords.
INPUT	character string,
	an array of character strings containing the list of keywords,
	offset (in char) between each keyword.
	number of keywords.
OUTPUT	position in the list (0 = first) if keyword matched,
	RETURN_ERROR otherwise.
NOTES	the matching is case-sensitive.
AUTHOR	E. Bertin (IAP & Leiden observatory)
VERSION	15/02/96
 ***/
int	findnkey(char *str, char *key, int size, int nkey)

  {
  int i;

  for (i=0; i<nkey; i++, key += size)
    if (!strcmp(str, key))
      return i;

  return RETURN_ERROR;
  }


