#include <time.h>
#include <string.h>

#ifdef PG_PPU
#define GRDATE grdate_
#else
#define GRDATE grdate
#endif

/**GRDATE -- get date and time as character string (Cray)
 *+
 *    SUBROUTINE GRDATE(STRING, L)
 *    CHARACTER*(*) STRING
 *     INTEGER L
 *
 * Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
 * To receive the whole string, the STRING should be declared
 * CHARACTER*17.
 *
 * Arguments:
 *  STRING : receives date and time, truncated or extended with
 *           blanks as necessary.
 *  SLEN   : receives the number of characters in STRING, excluding
 *           trailing blanks. This will always be 17, unless the length
 *           of the string supplied is shorter.
 *--
 * 09-Nov-1994 - [mcs] Fortran callable C version for CRAY.
 *-----------------------------------------------------------------------
 */
void GRDATE(string, slen, maxlen)
     char *string; int *slen; int maxlen;
{
  char vtime[18];  /* Output string compilation buffer */
  char *utime;     /* Returned string from ctime() */
  time_t x;        /* Time returned by time() */
  int i;
/*
 * Get the standard C time string.
 */
  time(&x);
  utime = ctime(&x);
/*
 * Copy a re-organised version of the time string into vtime[].
 */
  vtime[0] = utime[8];
  vtime[1] = utime[9];
  vtime[2] = '-';
  vtime[3] = utime[4];
  vtime[4] = utime[5];
  vtime[5] = utime[6];
  vtime[6] = '-';
  vtime[7] = utime[20];
  vtime[8] = utime[21];
  vtime[9] = utime[22];
  vtime[10] = utime[23];
  vtime[11] = ' ';
  strncpy(vtime+12, utime+11, 5);
  vtime[17]='\0';
/*
 * Copy up to maxlen characters of vtime into the output FORTRAN string.
 */
  strncpy(string, vtime, maxlen);
  *slen = (maxlen < 17) ? maxlen : 17;
/*
 * Pad the FORTRAN string with spaces.
 */
  for(i=17; i<maxlen; i++)
    string[i] = ' ';
  return;
}
