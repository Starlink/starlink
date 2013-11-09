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
 *   3 Jan 1996 (DJA):
 *     Converted to program which prints result
*/

#ifndef vms

#include "stdio.h"
#include "termio.h"

int main() {

  int	width,height,ret;
 
#ifdef TIOCGWINSZ

  struct winsize s;

/* this 1 is the file descriptor eqivalent of 'SYS$OUTPUT' on VMS.
*/	
	if (ioctl (1, TIOCGWINSZ, &s))
		return 0;
	height = s.ws_row;
	width = s.ws_col;

        ret = 1;
#else
	height = 0;
	width = 80;
#endif
  
  printf( "%d %d\n", width, height );
  
  return 0;
}
#endif
