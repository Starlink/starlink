/*
 * RAL GKS System
 *
 * gktset_: Configures workstation for GKIOCI() and GKIOBI().
 *
 * Type of Function:  SYSTEM INTERFACE
 * Author:            PJWR
 *
 * Copyright (C) SERC 1986
 *
 * Maintenance Log:
 *
 *  11/08/86  PJWR  UNIX System V version stabilised.
 *  07/04/87  PJWR  Documentation updated.
 *  19/07/04  TIMJ  Autoconf version. ANSI C.
 *  13/07/04  PWD   POSIX termios version.
 */

/*
 * Inclusions:
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#else
# error "Need to have unistd.h for isatty"
#endif

#if HAVE_TERMIOS_H
#  include <termios.h>
#elif HAVE_TERMIO_H
#  include <termio.h>
#elif HAVE_SGTTY_H
#  include <sgtty.h>
#else
# error "Do not know how to do terminal control"
#endif

#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#include "f77_type.h"      /* For FORTRAN 77 type equivalencing. */
#include "gks.h"

/*
 * Comments:
 *
 *   This routine does no error checking or reporting.
 *
 *  *lun    Fortran logical unit number (Input)
 *  *echo   Echo control (Input)    [logical]
 *  *purge  Purge control (Input)   [logical]
 *
 *  Always returns 0
 */

f77_integer gktset_(f77_integer *lun,
		    f77_integer *echo,
		    f77_integer *purge)
{
#if HAVE_TERMIOS_H
  struct termios tty;
#elif HAVE_TERMIO_H
  struct termio  tty;               /* For basic mode details */
#elif HAVE_SGTTY_H
  struct sgttyb  tty;
#endif
  f77_integer    fd;                /* File descriptor associated with lun */

  /* Evaluate fd and proceed if it's ok ... */
  gks_getfd_( lun, &fd );

  if( isatty((int)fd) )
  {
    /* ... get the terminal details ... */

#if HAVE_TERMIOS_H
    (void)tcgetattr(fd, &tty);
#elif HAVE_TERMIO_H
    (void)ioctl(fd, TCGETA, &tty);
#elif HAVE_SGTTY_H
    (void)ioctl(fd, TIOCGETP, &tty);
#endif

    /* ... then modify terminal characteristics as requested. */

    if(!*echo) {
#if HAVE_TERMIO_H || HAVE_TERMIOS_H
      tty.c_lflag &= ~ECHO;
#elif HAVE_SGTTY_H
      tty.sg_flags &= ~ECHO;
#endif
    } else {
#if HAVE_TERMIO_H || HAVE_TERMIOS_H
      tty.c_lflag |= ECHO;
#elif HAVE_SGTTY_H
      tty.sg_flags |= ECHO;
#endif
    }

    if(*purge) {
#if HAVE_TERMIOS_H
      (void)tcsetattr(fd, TCSAFLUSH, &tty);
#elif HAVE_TERMIO_H
      (void) ioctl(fd, TCSETAF, &tty);
#elif HAVE_SGTTY_H
      (void) ioctl(fd, TIOCSETP, &tty);
#endif
    } else {
#if HAVE_TERMIOS_H
      (void)tcsetattr(fd, TCSADRAIN, &tty);
#elif HAVE_TERMIO_H
      (void) ioctl(fd, TCSETAW, &tty);
#elif HAVE_SGTTY_H
      (void) ioctl(fd, TIOCSETN, &tty);
#endif
    }
  }

  return((f77_integer)0);
}
