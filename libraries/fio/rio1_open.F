#include "config.h"
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

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     NXG: Norman Gray (Starlink, Glasgow)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1992 (PMA):
*        Original version.
*     17-MAR-1992 (PMA):
*        Fix an apparent bug whereby the result of the INQUIRE statement
*        was always multiplied by 4, rather then just in the case of
*        unformatted files.
*     31-MAR-1992 (PMA):
*        First Sun specific version.
*        Remove the use of BLOCKSIZE, ORGANIZATION and READONLY.
*        Inquire the record size if it is given as zero.
*        The record length is always given in bytes, not longwords.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     19-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     24-SEP-1993 (PMA):
*        Added code to check the record length. Previously it only
*        inquired the record length, which was a bug for an access
*        mode of WRITE.
*     20-OCT-1993 (PMA):
*        Remove code to multiply the record length by 4. This is only
*        needed on DEC machines.
*     23-DEC-1993 (PMA):
*        INCLUDE FIO_ERR
*     23-APR-2003 (NXG):
*        Now uses configure to disable/enable features
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

*   If the record length is greater than zero, use it.
      IF ( RECSZ .GT. 0 ) THEN
         IF ( FORMU .EQ. 'UNFORMATTED' ) THEN
*         Convert the number of bytes to the wordlength unit used by OPEN(recl)
#if FC_RECL_UNIT == 1
            RECLEN = RECSZ
#elif FC_RECL_UNIT == 2
            RECLEN = ( RECSZ + 1 ) / 2
#elif FC_RECL_UNIT == 4
            RECLEN = ( RECSZ + 3 ) / 4
#else
 error "Unrecognized FC_RECL_UNIT"
#endif
         ELSE
            RECLEN = RECSZ
         END IF

      ELSE IF( RECSZ .EQ. 0 ) THEN
*  If the record size has been given as zero, report an error if the
*  access mode is WRITE, otherwise inquire what the record length
*  actually is.
         IF( ACMODU .EQ. 'WRITE' ) THEN
            STATUS = FIO__INVRL
            CALL EMS_REP( 'RIO1_OPEN_BADSZ',
     :         'You cannot specify a record length of zero when '/
     :         / 'creating a new file', STATUS )
         ELSE
            INQUIRE( UNIT=UNIT, RECL=RECLEN )
*         RECLEN is now, obviously, in the correct wordlength units
         END IF

      ELSE
*  The record length is negative, report an error.
         STATUS = FIO__INVRL
         CALL EMS_REP( 'RIO1_OPEN_NEGSZ',
     :      'Programming error: RIO1_OPEN has been called with a '/
     :      / 'negative record length.', STATUS )
      END IF

*  Open the file.
      IF ( ACMODU .EQ. 'READ' ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :        ERR=10, IOSTAT=IOERR,
#if HAVE_FC_OPEN_ORGANISATIONRELATIVE
     :        ORGANISATION='RELATIVE',
#endif
#if HAVE_FC_OPEN_ACTIONREAD
     :        ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :        READONLY,
#endif
     :        RECL=RECLEN,
     :        STATUS='OLD' )
      ELSE IF ( ACMODU .EQ. 'WRITE' ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :        ERR=10, IOSTAT=IOERR,
#if HAVE_FC_OPEN_ORGANISATIONRELATIVE
     :        ORGANISATION='RELATIVE',
#endif
     :        RECL=RECLEN, STATUS='NEW' )
      ELSE IF ( ACMODU .EQ. 'UPDATE' ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :        ERR=10, IOSTAT=IOERR,
#if HAVE_FC_OPEN_ORGANISATIONRELATIVE
     :        ORGANISATION='RELATIVE',
#endif
     :        RECL=RECLEN, STATUS='OLD' )
      ELSE IF ( ACMODU .EQ. 'APPEND') THEN
         OPEN( UNIT=UNIT, FILE=FILE, ACCESS='DIRECT', FORM=FORMU,
     :        ERR=10, IOSTAT=IOERR,
#if HAVE_FC_OPEN_ORGANISATIONRELATIVE
     :        ORGANISATION='RELATIVE',
#endif
     :        RECL=RECLEN, STATUS='UNKNOWN' )
      END IF

*  Inquire the record length of the file.
      INQUIRE( UNIT=UNIT, RECL=RECLEN )

      IF ( FORMU .EQ. 'UNFORMATTED' ) THEN
*      Convert (returned) RECLEN back to bytes for unformatted files.
#if FC_RECL_UNIT == 1
         CONTINUE
#elif FC_RECL_UNIT == 2
         RECLEN = RECLEN * 2
#elif FC_RECL_UNIT == 4
         RECLEN = RECLEN * 4
*      We know it's not more, else we would have failed above.
#endif
      END IF

      GOTO 999

*  Handle any error condition.
   10 CALL FIO_SERR( IOERR, STATUS )
      CALL FIO_PUNIT( UNIT, STATUS )

  999 CONTINUE
      END
