/* aio_scrnsz
/*
/* compile with -I/star/include (use gcc on Sun)
/*
/* History:
/*   1-JAN-1992 (KFH):
/*     Original Version
/*  17-MAR-1993 (AJC):
/*     Added endwin()
/*     Correctly cast initscr
/*  21-JUL-1993 (AJC):
/*     Use ioctl to prevent problems on Solaris
*/

#ifndef vms

#include "sae_par.h"
#include "sys/termio.h"
#include "f77.h"
#include "asterix.h"

F77_INTEGER_FUNCTION(aio_scrnsz)(INTEGER(width),INTEGER(height),INTEGER(status))
  {
  GENPTR_INTEGER(width)
  GENPTR_INTEGER(height)
  GENPTR_INTEGER(status)

#ifdef TIOCGWINSZ

  struct winsize s;

  if ( ! _ok(status) )
    return 0;

/* this 1 is the file descriptor eqivalent of 'SYS$OUTPUT' on VMS.
*/
	if (ioctl (1, TIOCGWINSZ, &s))
		return 0;
	*height = s.ws_row;
	*width = s.ws_col;

	return 1;
#else
  if ( ! _ok(status) )
    return 0;

	*height = 0;
	*width = 80;
#endif
}
#endif
