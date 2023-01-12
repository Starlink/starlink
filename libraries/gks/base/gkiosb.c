/*
 * RAL GKS System
 *
 * gkiosb_: Modifies the special characters of a terminal as necessary to
 *          ensure that the GKS break character is passed back to GKS.
 *
 * Type of Function:  SYSTEM INTERFACE
 * Author:            PJWR
 *
 * Copyright (C) SERC 1986
 *
 * Maintenance Log:
 *
 *  19/12/86  PJWR  UNIX System V version stabilised.
 *  07/04/87  PJWR  Documentation updated.
 *  09/07/87  PJWR  Updated error codes for GKS 7.4.
 *  20/06/04  TIMJ  Autoconf merge of SysV and UCB versions
 *  13/07/04  PWD   POSIX termios version.
 */

/*
 * Inclusions:
 */
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>			/* For stdio(3) */

#if HAVE_UNISTD_H
# include <unistd.h>
#else
# error "Need to have unistd.h for isatty"
#endif

#if HAVE_TERMIOS_H
#  include <termios.h>
#elif HAVE_TERMIO_H
#  include <termio.h>			/* For termio(7) */
#  define NCCS NCC
#elif HAVE_SGTTY_H
#  include <sgtty.h>
#else
# error "Do not know how to do terminal control"
#endif

#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#include "f77_type.h"	/* For type equivalencing with f77 */
#include "gkerr.h"		/* For KERROR */
#include "gkdt.h"		/* For KWK */
#include "gkwca.h"		/* For KWKIX */
#include "gks.h"

/*
 * Constants:
 */


#define GKS_BRK		'\032'		/* GKS break character (SUB) */

#if HAVE_TERMIO_H || HAVE_TERMIOS_H

#define DISABLE		'\0'		/* Use NUL to disable */
#define NONE		(char)(-1)	/* Initialisation value */

#elif HAVE_SGTTY_H

#define DISABLE         (char)-1        /* To disable special characters */

/*
 * Keys for tagging use of GKS break as a special character and which struct
 * was modified as a result:
 */

#define NONE            '\000'          /* No use of GKS break/changes */
#define V6              '\001'          /* Special chars from V6,  or ... */
#define V7              '\002'          /* V7,  or ... */
#define BSD             '\003'          /* BSD were changed. */
#define ERASE           '\004'          /* GKS break used as erase,  or ... */
#define KILL            '\005'          /* line kill,  or ... */
#define INTR            '\006'          /* interrupt,  or ... */
#define QUIT            '\007'          /* quit,  or ... */
#define EOFC            '\010'          /* end of file, or ... */
#define BREAK           '\011'          /* break, or ... */
#define START           '\012'          /* start output, or ... */
#define STOP            '\013'          /* stop output,  or ... */
#define TOGGLE          '\014'          /* a start/stop toggle, or ... */
#define SUSP            '\015'          /* suspend,  or ... */
#define DSUSP           '\016'          /* delayed suspend,  or ... */
#define RPRNT           '\017'          /* reprint line,  or ... */
#define FLUSH           '\020'          /* flush output,  or ... */
#define WERAS           '\021'          /* erase word,  or ... */
#define LNEXT           '\022'          /* literal next. */

#endif

/*
 * Errors:
 *
 * -2004 Documented condition to be satisfied by parameter(s) of internal
 *       routine is not satisfied
 * -2006 Value of internal enumerated type is invalid
 */

/*
 * Comments:
 *
 * This routine is called by GKIOOP() and GKIOCL().  It assumes that the GKS
 * break character will appear at most once in the terminal's special character
 * set.
 *
 * The logical unit whose number is passed in lun must be connected to a tty.
 *
 * Assumes the C implementation defaults to signed char.

 * *lun    - f77 logical unit number (Input)
 * *action - Modify/restore terminal characters (Input)

 */

f77_integer gkiosb_( f77_integer *lun, f77_integer *action )

{
  static char	  was_break[KWK] = /* Which special character was GKS_BRK */
		  {
		    NONE
		  };
#if HAVE_TERMIOS_H
  struct termios  parms;
#elif HAVE_TERMIO_H
  struct termio	  parms;	   /* For terminal parameters */
#elif HAVE_SGTTY_H
  static char     modified[KWK] =  /* Which structure was modified */
                  {
                    NONE
                  };
  struct sgttyb   tty;             /* For V6 characters */
  struct tchars   tch;             /* For V7 characters */
  struct ltchars  ltch;            /* For BSD chracters */
  int    ldisc;                    /* For tty line discipline */
#endif
  int		  index,	   /* Local copy of KWKIX */
		  i;		   /* Loop index */
  f77_integer	  fd;              /* File descriptor for '*lun' */


  /* Check that *lun is opened to a tty.  If not generate GKS error -2004 */

  gks_getfd_(lun, &fd);
  if(isatty((int)fd))
    {
      /* Make a local copy of the workstation index for C array access */

      index = gkywca_.kwkix - 1;

#if HAVE_TERMIOS_H || HAVE_TERMIO_H
      /* Either modify (KON) or restore (KOFF) characters according to *action */

      if(*action == KON)
	{

	  /* Get the terminal parameters. */
#if HAVE_TERMIOS_H
          (void)tcgetattr(fd, &parms);
#else
	  (void)ioctl(fd, TCGETA, &parms);
#endif
	  /* Scan special characters,  modify as appropriate. */

	  for(i = 0; i < NCCS; i++)
	    if(parms.c_cc[i] == GKS_BRK)
	      {
		was_break[index] = i;
		parms.c_cc[i] = DISABLE;
#if HAVE_TERMIOS_H
                (void)tcsetattr(fd, TCSANOW, &parms);
#else
		(void)ioctl(fd, TCSETA, &parms);
#endif
		break;
	      }

	}
      else if(*action == KOFF)
	{

	  /* Get the terminal parameters and restore iff appropriate. */

	  if(was_break[index] != NONE)
	    {
#if HAVE_TERMIOS_H
              (void)tcgetattr(fd, &parms);
#else
	      (void)ioctl(fd, TCGETA, &parms);
#endif
	      parms.c_cc[was_break[index]] = GKS_BRK;
	      was_break[index] = NONE;
#if HAVE_TERMIOS_H
	      (void)tcsetattr(fd, TCSANOW, &parms);
#else
	      (void)ioctl(fd, TCSETA, &parms);
#endif
	    }
	} else {
	  gkyerr_.kerror = -2006;
	}

#elif HAVE_SGTTY_H
      /* Either modify (KON) or restore (KOFF) characters according to *action */

      if(*action == KON)
	{

	  /* Check the V6 and V7 special characters */

	  (void)ioctl(fd, TIOCGETP, &tty);
	  (void)ioctl(fd, TIOCGETC, &tch);
	  if(tty.sg_erase == GKS_BRK)
	    {
	      was_break[index] = ERASE;
	      tty.sg_erase = DISABLE;
	      modified[index] = V6;
	    }
	  else if(tty.sg_kill == GKS_BRK)
	    {
	      was_break[index] = KILL;
	      tty.sg_kill = DISABLE;
	      modified[index] = V6;
	    }
	  else if(tch.t_intrc == GKS_BRK)
	    {
	      was_break[index] = INTR;
	      tch.t_intrc = DISABLE;
	      modified[index] = V7;
	    }
	  else if(tch.t_quitc == GKS_BRK)
	    {
	      was_break[index] = QUIT;
	      tch.t_intrc = DISABLE;
	      modified[index] = V7;
	    }
	  else if(tch.t_eofc == GKS_BRK)
	    {
	      was_break[index] = EOFC;
	      tch.t_eofc = DISABLE;
	      modified[index] = V7;
	    }
	  else if(tch.t_brkc == GKS_BRK)
	    {
	      was_break[index] = BREAK;
	      tch.t_brkc = DISABLE;
	      modified[index] = V7;
	    }
	  else if(tch.t_startc == GKS_BRK)
	    {

	      /* Check for use of GKS_BRK as XON/XOFF toggle */

	      if(tch.t_stopc == GKS_BRK)
		{
		  was_break[index] = TOGGLE;
		  tch.t_stopc = DISABLE;
		}
	      else
		was_break[index] = START;
	      tch.t_startc = DISABLE;
	      modified[index] = V7;
	    }
	  else if(tch.t_stopc == GKS_BRK)
	    {
	      was_break[index] = STOP;
	      tch.t_stopc = DISABLE;
	      modified[index] = V7;
	    }
	  else
	    {

	      /* Get line discipline and check BSD characters if appropriate */

	      (void)ioctl(fd, TIOCGETD, &ldisc);
	      if(ldisc == NTTYDISC)
		{
		  (void)ioctl(fd, TIOCGLTC, &ltch);
		  if(ltch.t_suspc == GKS_BRK)
		    {
		      was_break[index] = SUSP;
		      ltch.t_suspc = DISABLE;
		      modified[index] = BSD;
		    }
		  else if(ltch.t_dsuspc == GKS_BRK)
		    {
		      was_break[index] = DSUSP;
		      ltch.t_dsuspc = DISABLE;
		      modified[index] = BSD;
		    }
		  else if(ltch.t_rprntc == GKS_BRK)
		    {
		      was_break[index] = RPRNT;
		      ltch.t_rprntc = DISABLE;
		      modified[index] = BSD;
		    }
		  else if(ltch.t_flushc == GKS_BRK)
		    {
		      was_break[index] = FLUSH;
		      ltch.t_rprntc = DISABLE;
		      modified[index] = BSD;
		    }
		  else if(ltch.t_werasc == GKS_BRK)
		    {
		      was_break[index] = WERAS;
		      ltch.t_werasc = DISABLE;
		      modified[index] = BSD;
		    }
		  else if(ltch.t_lnextc == GKS_BRK)
		    {
		      was_break[index] = LNEXT;
		      ltch.t_lnextc = DISABLE;
		      modified[index] = BSD;
		    }
		}
	    }
	}
      else if(*action == KOFF)
	{

	  /* Get the set of special characters modified */

	  switch(modified[index])
	    {
	    case NONE:
	      break;
	    case V6:
	      (void)ioctl(fd, TIOCGETP, &tty);
	      break;
	    case V7:
	      (void)ioctl(fd, TIOCGETC, &tch);
	      break;
	    case BSD:
	      (void)ioctl(fd, TIOCGLTC, &ltch);
	      break;
	    default:
	      gkyerr_.kerror = -2006;
	      break;
	    }

	  /* Restore whichever character(s) changed */

	  switch(was_break[index])
	    {
	    case NONE:
	      break;
	    case ERASE:
	      tty.sg_erase = GKS_BRK;
	      break;
	    case KILL:
	      tty.sg_kill = GKS_BRK;
	      break;
	    case INTR:
	      tch.t_intrc = GKS_BRK;
	      break;
	    case QUIT:
	      tch.t_quitc = GKS_BRK;
	      break;
	    case EOFC:
	      tch.t_eofc = GKS_BRK;
	      break;
	    case BREAK:
	      tch.t_brkc = GKS_BRK;
	      break;
	    case START:
	      tch.t_startc = GKS_BRK;
	      break;
	    case STOP:
	      tch.t_stopc = GKS_BRK;
	      break;
	    case TOGGLE:
	      tch.t_startc = GKS_BRK;
	      tch.t_stopc = GKS_BRK;
	      break;
	    case SUSP:
	      ltch.t_suspc = GKS_BRK;
	      break;
	    case DSUSP:
	      ltch.t_dsuspc = GKS_BRK;
	      break;
	    case RPRNT:
	      ltch.t_rprntc = GKS_BRK;
	      break;
	    case FLUSH:
	      ltch.t_flushc = GKS_BRK;
	      break;
	    case WERAS:
	      ltch.t_werasc = GKS_BRK;
	      break;
	    case LNEXT:
	      ltch.t_lnextc = GKS_BRK;
	      break;
	    }
	}
      else
	{
	  gkyerr_.kerror = -2006;
	}

      /* After either a modify or restore, write back changes as required */

      switch(modified[index])
	{
	case NONE:
	  break;
	case V6:
	  (void)ioctl(fd, TIOCSETN, &tty);
	  break;
	case V7:
	  (void)ioctl(fd, TIOCSETC, &tch);
	  break;
	case BSD:
	  (void)ioctl(fd, TIOCSLTC, &ltch);
	  break;
	default:
	  gkyerr_.kerror = -2006;
	  break;
	}

      /* If a restore,  reset change flag  and character log */

      if(*action == KOFF)
	{
	  modified[index] = NONE;
	  was_break[index] = NONE;
	}
#endif
    } else {
      gkyerr_.kerror = -2004;
    }
  return((f77_integer)0);
}
