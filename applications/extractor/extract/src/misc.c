 /*
 				misc.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP/ESO
*
*	Contents:	miscellaneous functions.
*
*	Last modify:	31/05/97
*                       28/10/98 (AJC)
*                         ADAM version of error
*                         Use AFPRINTF in warning
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>
#include        <setjmp.h>

#include	"define.h"
#include	"globals.h"
#include 	"merswrap.h"
#include        "sae_par.h"

jmp_buf		env;

/********************************* error ************************************/
/*
Version for ADAM

I hope it will never be used!
*/

void	error(int num, char *msg1, char *msg2)
  {
  int status;

  status = SAI__ERROR;
  msgSetc( "TOK1", msg1 );
  msgSetc( "TOK2", msg2 );
  errRep( " ", "^TOK1 ^TOK2", &status );
  longjmp( env, num );
  }

/******************************* cistrcmp ***********************************/
/*
case-insensitive strcmp.
*/
int	cistrcmp(char *cs, char *ct, int mode)

  {
   int	diff;

  if (mode)
    {
    while (*cs && *ct)
      if (diff=tolower((int)*(cs++))-tolower((int)*(ct++)))
        return diff;
    }
  else
    {
    while (*cs || *ct)
      if (diff=tolower((int)*(cs++))-tolower((int)*(ct++)))
        return diff;
    }

  return 0;
  }


/******************************** hmedian ***********************************/
/*
Median using Heapsort algorithm (for float arrays) (based on Num.Rec algo.).
Warning: changes the order of data!
*/
float	hmedian(float *ra, int n)

  {
   int		l, j, ir, i;
   float	rra;


  if (n<2)
    return *ra;
  ra--;
  for (l = ((ir=n)>>1)+1;;)
    {
    if (l>1)
      rra = ra[--l];
    else
      {
      rra = ra[ir];
      ra[ir] = ra[1];
      if (--ir == 1)
        {
        ra[1] = rra;
        return n&1? ra[n/2+1] : (float)((ra[n/2]+ra[n/2+1])/2.0);
        }
      }
    for (j = (i=l)<<1; j <= ir;)
      {
      if (j < ir && ra[j] < ra[j+1])
        ++j;
      if (rra < ra[j])
        {
        ra[i] = ra[j];
        j += (i=j);
        }
      else
        j = ir + 1;
      }
    ra[i] = rra;
    }

/* (the 'return' is inside the loop!!) */
  }


/********************************* warning **********************************/
/*
Print a warning message on screen.
*/
void	warning(char *msg1, char *msg2)
  {
  AFPRINTF(OUTPUT, "\n> WARNING: %s%s.\n\n",msg1,msg2);
  return;
  }


/******************************* swapbytes **********************************/
/*
Swap bytes for doubles, longs and shorts (for DEC machines or PC for inst.).
*/
void	swapbytes(void *ptr, int nb, int n)
  {
   char	*cp;
   int	j;

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

