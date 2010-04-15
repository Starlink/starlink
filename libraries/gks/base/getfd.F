#include "config.h"

      SUBROUTINE GKS_GETFD( LUN, FD )
* Name:
*    gks_getfd

* Purpose:
*    Get the unix file descriptor associated with a Fortran unit

* Language:
*    Fortran 77 (with extensions)

* Invocation:
*    CALL GKS_GETFD( LUN, FD )

* Description:
*    Given a Fortran unit number, obtain the underlying unix file descriptor.

* Arguments:
*    LUN =  INTEGER (Given)
*       The Fortran unit number
*    FD = INTEGER (Returned)
*       Unix file descriptor. -1 if unknown.

* Notes:
*     If present, can use the GETFD or FNUM builtins. If these are not
*     available attempts to recognize standard Fortran mappings. Will
*     return -1 for the FD if the lookup can not be associated with a
*     standard unit (usually this means STDIN, STDOUT or STDERR can
*     currently only be obtained). If attempting the fallback approach
*     this routine will fail for logical units associated with an actual
*     file.

*  Copyright:
*     Copyright (C) 2004 Particle Physics and Astronomy Research Council

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     19-JUN-2004 (TIMJ):
*        Original version.
*     28-JUL-2005 (PWD):
*        Added FNUM calls. This is available for gfortran and g95.

*  Bugs:
*     {not_any_bugs_here}

*.

*  Arguments Given:
      INTEGER LUN

*  Arguments Returned:
      INTEGER FD

*  Local variables:

*-
      FD = -1

#if HAVE_INTRINSIC_GETFD || HAVE_GETFD

*     Need to get the unix file descriptor using builtin
      FD = GETFD( LUN )
#elif HAVE_INTRINSIC_FNUM || HAVE_FNUM

*     Need to get the unix file descriptor using builtin
      FD = FNUM( LUN )

#else

*     On Cray, guarantee that units 100,101 and 102 correspond to
*     stdin, stdout and stderr but also allows 0, 5 and 6 (which
*     can be opened and closed by the programmer).

*     Could use INQUIRE to determine whether a filename is attached
*     to the unit but then we need to work out the FD. The GKS
*     method is to call out to C and reopen the file using "ab"
*     mode. This would require that the FD is closed after it is
*     used to prevent file descriptor leak.

*     g77 can also use the fio.h approach but there is no point
*     since the getfd builtin currently exists for g77.

*     Need to assume that anything except the standard Fortran
*     unit numbers are false
      IF (LUN .EQ. 5) THEN
*     STDIN
         FD = 0
      ELSE IF (LUN .EQ. 6) THEN
*     STDOUT
         FD = 1
      ELSE IF (LUN .EQ. 0) THEN
*     STDERR
         FD = 2
      ELSE
         FD = -1
         GO TO 999
      END IF

#endif

 999  CONTINUE
      END



