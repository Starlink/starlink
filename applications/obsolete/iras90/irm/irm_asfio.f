      SUBROUTINE IRM_ASFIO ( PNFILE, ACMODE, FORM, RECSZ, FD, OPEN,
     :                       STATUS )
*+
*  Name:
*     IRM_ASFIO

*  Purpose:
*     Opens a sequential file via a parameter.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL IRM_ASFIO ( PNFILE, ACMODE, FORM, RECSZ, FD, OPEN, STATUS )

*  Description:
*     This routine opens a sequential file via FIO_ASSOC.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.

*  Arguments:
*     PNFILE = CHARACTER*(*)
*        Parameter name by which file is to be opened
*     ACMODE = CHARACTER*(*)
*        Expression giving the required access mode.
*          Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*          For details, see FIO_OPEN.
*     FORM = CHARACTER*(*)( READ )
*        Expression giving the required formatting of the file.
*          Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*          'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ = INTEGER( READ )
*        Expression giving the maximum record size in bytes.
*          Set it to zero if the Fortran default is required.
*     FD = INTEGER( WRITE )
*        Variable to contain the file descriptor.
*     OPEN = LOGICAL( WRITE )
*        If true the file has been opened.
*     STATUS = INTEGER( READ, WRITE )
*        Global status value

*  Authors:
*     David S. Berry (MAVAD::DSB)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1992 (DSB):
*        Original version copied from AIF_ASFIO (written by Malcolm
*        Currie).
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard SSE constants.
      INCLUDE 'PAR_ERR'       ! Parameter-system errors

*  Arguments Given:
      CHARACTER PNFILE*(*)
      CHARACTER ACMODE*(*)
      CHARACTER FORM*(*)
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD

*  Status:
      INTEGER  STATUS

*  Local Constants:
      INTEGER
     :    MXLOOP               ! Maximum number of attempts at
                               ! opening a data file
      PARAMETER ( MXLOOP = 4 )

      INTEGER
     :    LOOP                 ! Number of attempts to open the file

      LOGICAL                  ! true if:
     :    LOOPAG,              ! Loop again to open output file
     :    OPEN                 ! File opened successfully

*.

*  Check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP = 0
      LOOPAG = .TRUE.
      OPEN = .FALSE.
      DO WHILE ( LOOPAG )

*  Attempt to obtain and open a file to output listing
         CALL FIO_ASSOC( PNFILE, ACMODE, FORM, RECSZ, FD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            OPEN = .FALSE.
            LOOPAG = .FALSE.
            CALL ERR_ANNUL( STATUS )

         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*  Here if filename is not allowed or file is not opened - try again
*  Need to flush error here, as not quitting routine
            LOOP = LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', PNFILE )
               CALL ERR_REP( 'IRM_ASFIO_ERR1',
     :           'IRM_ASFIO: Could not open file %^FILNAM - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*  End looping as user is having serious problems
               LOOPAG = .FALSE.
            END IF

            CALL PAR_CANCL( PNFILE, STATUS )

         ELSE

*  No problem, so exit loop
            LOOPAG = .FALSE.
            OPEN = .TRUE.

*  End of file-opened-successfully check
         END IF
      END DO

*  Abort for repeated error
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_ASFIO_ERR2',
     :     'IRM_ASFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END
