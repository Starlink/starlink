*+
*  Name:
*     TIO_EOF, TIO_ERR, TIO_GETMSG, TIO_MARK,
*        TIO_READ, TIO_SKIP, TIO_WRITE

*  Purpose:
*     Mock routines for absent TIO library.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     See calling routines.

*  Description:
*     The TIO library is not present in this version of Figaro.  However
*     there may be calls in Figaro to support disk-FITS.  To some extent
*     these calls are to FIG_FITIN and FIT_* routines, which would
*     handle both disk- and tape-FITS.  As a result they might call
*     certain TIO routines for the tape-FITS case.  If all is well these
*     calls are never actually made, but still some TIO routines must be
*     linked with.  This source file contains these dummy routines.

*  Arguments:
*     See calling routines.

*  Notes:
*     These routines are purely to satisfy the linker.  If called they
*     will report an error and stop (they don't even return).

*  Timing:
*     Reasonably fast.

*  Authors:
*     Horst Meyerdierks: hme (UoE, Starlink)
*     A C Davenhall: acd (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     20 Jul 1993 (hme):
*        Original version.
*     23 Feb 2001 (acd):
*        Added return values for the functions TIO_EOF and TIO_ERR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      LOGICAL FUNCTION TIO_EOF( STATUS )
      IMPLICIT NONE
      INTEGER STATUS
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      TIO_EOF = .TRUE.
      STOP
      END

      LOGICAL FUNCTION TIO_ERR( STATUS )
      IMPLICIT NONE
      INTEGER STATUS
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      TIO_ERR = .TRUE.
      STOP
      END

      SUBROUTINE TIO_GETMSG( STATUS, MSGBUF, MSGLEN )
      IMPLICIT NONE
      INTEGER STATUS
      CHARACTER * ( * ) MSGBUF
      INTEGER MSGLEN
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      STOP
      END

      SUBROUTINE TIO_MARK( IOCHAN, STATUS )
      IMPLICIT NONE
      INTEGER IOCHAN
      INTEGER STATUS
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      STOP
      END

      SUBROUTINE TIO_READ( IOCHAN, MAXLEN, BUFFER, ACTLEN, STATUS )
      IMPLICIT NONE
      INTEGER IOCHAN
      INTEGER MAXLEN
      BYTE BUFFER( * )
      INTEGER ACTLEN
      INTEGER STATUS
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      STOP
      END

      SUBROUTINE TIO_SKIP( IOCHAN, NTM, STATUS )
      IMPLICIT NONE
      INTEGER IOCHAN
      INTEGER NTM
      INTEGER STATUS
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      STOP
      END

      SUBROUTINE TIO_WRITE( IOCHAN, BUFFER, LENGTH, STATUS )
      IMPLICIT NONE
      INTEGER IOCHAN
      BYTE BUFFER( * )
      INTEGER LENGTH
      INTEGER STATUS
      INTEGER IGNORE
      IGNORE = 0
      CALL MSG_OUT( 'TIO_NOXST',
     :   'Programming error: TIO is not installed.', IGNORE )
      STOP
      END
