      SUBROUTINE RIO1_OPEN( UNIT, FILE, ACMODU, FORMU, RECSZ, RECLEN,
     :   STATUS )
*+
*  Name:
*     RIO1_OPEN

*  Purpose:
*     Do the actual Fortran OPEN statement

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO1_OPEN( UNIT, FILE, ACMODU, FORMU, RECSZ, RECLEN, STATUS )

*  Description:
*     Execute the actual Fortran OPEN statement. This is isolated in a
*     subroutine as it contains machine dependent code.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The FORTRAN unit number to be opened
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the file to be opened.
*     ACMODU = CHARACTER * ( * ) (Given)
*        The file access mode (in upper case).
*     FORMU = CHARACTER * ( * ) (Given)
*        The format of the file (in upper case).
*     RECSZ = INTEGER (Given)
*        The record length used when opening the file.
*     RECLEN = INTEGER (Returned)
*        The record length (in bytes) found by inquiry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Sun-specific features used:
*     -  None

*  Copyright:
*     Copyright (C) 1996 Council for the Central Laboratory of the
*                        Research Councils

*  Authors:
*     BKM: Brian McIlwrath (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-MAR-1996 (BKM):
*        Original version - based on sun4 variant.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO symbolic error constants
      INCLUDE 'FIO_PAR'          ! FIO symbolic constants

*  Arguments Given:
      CHARACTER * ( * ) ACMODU
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) FORMU
      INTEGER RECSZ
      INTEGER UNIT

*  Arguments Returned:
      INTEGER RECLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IOERR              ! The Fortran I/O status value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the record length is greater than zero, use it.
      IF ( RECSZ .GT. 0 ) THEN
         RECLEN = RECSZ
      ELSE IF( RECSZ .EQ. 0 ) THEN
*  If the record size has been given as zero, report an error if the
*  access mode is WRITE, otherwise inquire what the record length 
*  actually is.
         IF( ACMODU .EQ. 'WRITE' ) THEN
            STATUS = FIO__INVRL
            CALL EMS_REP( 'RIO1_OPEN_BADSZ',
     :         'You cannot specify a record length of zero when '
     :         // 'creating a new file', STATUS )
         ELSE
            INQUIRE( UNIT=UNIT, RECL=RECLEN )
         END IF
      ELSE
*  The record length is negative, report an error.
         STATUS = FIO__INVRL
         CALL EMS_REP( 'RIO1_OPEN_NEGSZ',
     :      'Programming error: RIO1_OPEN has been called with a '
     :      // 'negative record length.', STATUS )
      END IF

*  Open the file.
      IF ( ACMODU .EQ. 'READ' ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :      ERR=10, IOSTAT=IOERR,
     :      RECL=RECLEN, STATUS='OLD' )
      ELSE IF ( ACMODU .EQ. 'WRITE' ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :      ERR=10, IOSTAT=IOERR,
     :      RECL=RECLEN, STATUS='NEW' )
      ELSE IF ( ACMODU .EQ. 'UPDATE' ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :      ERR=10, IOSTAT=IOERR,
     :      RECL=RECLEN, STATUS='OLD' )
      ELSE IF ( ACMODU .EQ. 'APPEND') THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :      ERR=10, IOSTAT=IOERR,
     :      RECL=RECLEN, STATUS='UNKNOWN' )
      END IF

*  Inquire the record length of the file.
      INQUIRE( UNIT=UNIT, RECL=RECLEN )
      GOTO 999

*  Handle any error condition.
   10 CALL FIO_SERR( IOERR, STATUS )
      CALL FIO_PUNIT( UNIT, STATUS )

  999 CONTINUE
      END
