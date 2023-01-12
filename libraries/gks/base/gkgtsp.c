/*
 * RAL GKS System
 *
 * gkgtsp_: Returns the line speed of the connection between an interactive
 *          workstation and the host computer.
 *
 * Type of Function:  SYSTEM INTERFACE
 * Author:            PJWR
 *
 * Copyright (C) SERC 1986
 *
 * Maintenance Log:
 *
 *   30/07/86  PJWR  UNIX System V version stabilised.
 *   07/04/87  PJWR  Documentation updated.
 *   20/07/04  TIMJ  Autoconf version. ANSI C. Almagmation of Alpha and Sun
 *   13/07/04  PWD   POSIX termios version.
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

#include "f77_type.h" /* For FORTRAN 77 type equivalencing. */
#include "gks.h"

/*
 * Comments:
 *
 *   Some UNIX systems do not support the same range of baud rates  as  in
 *   "bauds" below.  In particular,  the EXTA and EXTB entries may contain
 *   different values.
 *
 *   If "*ichan" is not connected to a terminal,  "*ittspd" will be set to
 *   zero.
 */

f77_integer gkgtsp_(ichan, ittspd)
  f77_integer *ichan;		/* Internal channel number (Input). */
  f77_integer *ittspd;		/* Connection baud rate (Output). */
{
  static int bauds[16] =
  {
    0, 50, 75, 110, 134, 150, 200, 300, 600,
    1200, 1800, 2400, 4800, 9600, 19200, 38400
  };

#if HAVE_TERMIOS_H
  struct termios tty;
  speed_t speed;
#elif HAVE_TERMIO_H
  struct termio tty;            /* For terminal information */
#elif HAVE_SGTTY_H
  struct sgttyb  tty;
#endif

  f77_integer  fd;             /* File descriptor associated with "*ichan" */

  /* Need to get the FD associated with this fortran unit */
  gks_getfd_( ichan, &fd );

  /* Make sure we have a terminal attached */
  if(isatty((int)fd))
  {
#if HAVE_TERMIOS_H
    (void)tcgetattr(fd, &tty);
    speed = cfgetispeed(&tty);
    if ( speed < 16 ) {
       *ittspd = bauds[(int)speed];
    } else {
        *ittspd = bauds[15];
    }
#elif HAVE_TERMIO_H
    (void)ioctl(fd,TCGETA,&tty);
    *ittspd = bauds[(int)(tty.c_cflag & CBAUD)];
#elif HAVE_SGTTY_H
    (void)ioctl(fd,TIOCGETP,&tty);
    *ittspd = bauds[tty.sg_ispeed];
#endif
  }
  else
    *ittspd = 0;

  return((f77_integer)0);
}
