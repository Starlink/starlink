#include <config.h>
************************************************************************
*				BEGIN				       *
*                                                                      *
*      NUM_ common block definitions for numerical error trapping      *
*      ----------------------------------------------------------      *
*                                                                      *
*				BEGIN                                  *
************************************************************************
*
*    Version:
*     sun4_Solaris.
*
*    Authors :
*     R.F. Warren-Smith (DUVAD::RFWS)
*     BLY: M.J. Bly  (Starlink, RAL)
*     NXG: Norman Gray (Starlink, Glasgow)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*
*    History :
*     16-AUG-1988:  Original version (DUVAD::RFWS)
*     22-JAN-1997:  System specific versions for Unix (BLY)
*     21-MAR-2004:  Autoconf (NXG) 
*     15-AUG-2005:  Should not mark a COMMON as VOLATILE
*    endhistory
*

*   Exception status variable to indicate a numerical error has been
*   detected. 
      INTEGER NUM_ERROR


*   Common block.
      COMMON /NUM_CMN/ NUM_ERROR


*   Save the common block contents.
      SAVE /NUM_CMN/


*   Prevent compiler optimisation of common variables accessed by a
*   condition/signal handler.  The VOLATILE keyword is present in
*   Fortran 2000, and as a vendor extension in some earlier compilers.
*   Its purpose is to indicate that a given variable may change other
*   than under the control of the program, or as a result of external
*   events -- it's for device handling.  I (Norman Gray/nxg, autoconfing
*   this) do not see that this is relevant for a signal handler, and
*   very strongly suspect that this is voodoo.  It has been added in the
*   past, however (by AJC in 24-NOV-2002) so presumably there was some
*   reason for it.
#if HAVE_KEYWORD_VOLATILE
*   It is non portable marking the COMMON block as VOLATILE as well as
*   the contents. Now simply mark the content as VOLATILE.
      VOLATILE NUM_ERROR
#else

* This platform doesn't have VOLATILE
#endif



************************************************************************
*				END				       *
*                                                                      *
*      NUM_ common block definitions for numerical error trapping      *
*                                                                      *
*				END                                    *
************************************************************************
