      SUBROUTINE DAT_MSG( TOKEN, LOC )
*+
*  Name:
*     DAT_MSG

*  Purpose:
*     Assign the name of an HDS object to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DAT_MSG( TOKEN, LOC )

*  Description:
*     The routine assigns the full name (including the file name) of an
*     HDS object to a message token for use with the ERR_ and MSG_
*     routines (SUN/104) or with the EMS_ routines (SSN/4). Appropriate
*     syntax is used to represent file names which do not have the
*     standard (.sdf) file extension.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the HDS object.

*  Notes:
*     -  This routine has no STATUS argument and does not perform
*     normal error checking. If it should fail, then no value will be
*     assigned to the message token and this will be apparent in the
*     final message.

*  Machine-specific features used:
*     -  This routine makes assumptions about the form of VMS and Unix
*     file names.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1990 (RFWS):
*        Original version.
*     26-NOV-1990 (RFWS):
*        Moved the call to EMS_SETC outside the local error context to
*        prevent the message token value from being lost.
*     2-JAN-1991 (RFWS):
*        Fixed problem with undefined path name length.
*     7-AUG-1991 (RFWS):
*        Renamed from the original NDF routine to become an HDS (DAT)
*        routine. Also added handling of Unix file names.
*     8-AUG-1991 (RFWS):
*        Allow subscripts on top-level objects.
*     9-AUG-1991 (RFWS):
*        Modified the handling of the error flag.
*     9-AUG-1991 (RFWS):
*        Modified to call EMS_ instead of MSG_.
*     16-AUG-1991 (RFWS):
*        Changed to make the tests for file extensions case sensitive.
*     20-AUG-1991 (RFWS):
*        Removed dependence on PSX_ routines - use global constant
*        instead.
*     24-SEP-1991 (RFWS):
*        Changed to call EMS_ routines instead of ERR_ routines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT1_SYS'         ! Internal system-specific constants
      INCLUDE 'EMS_PAR'          ! EMS_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) LOC

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( EMS__SZMSG ) BUFF ! Buffer
      CHARACTER * ( EMS__SZMSG ) FILE ! Container file name
      CHARACTER * ( EMS__SZMSG ) PATH ! Object path name
      INTEGER BRA                ! Position of '(' character
      INTEGER DOT                ! Position of '.'
      INTEGER NC                 ! No. characters in buffer
      INTEGER NCF                ! No. characters in filename
      INTEGER NCP                ! No. characters in path name
      INTEGER NLEV               ! Object level in HDS
      INTEGER SEMI               ! Position of ';'
      INTEGER START              ! Position to start in path name
      INTEGER STATUS             ! Local status variable
      LOGICAL ODD                ! Is the filename odd?
      LOGICAL OK                 ! No error has occurred?

*.

*  Mark the error stack and initialise status.
      CALL EMS_MARK
      STATUS = SAI__OK

*  Obtain the data object path and container file name.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If necessary, handle VAX/VMS file names.  Locate the semicolon which
*  delimits the version number in the file name.
         IF ( DAT__VMS ) THEN
            SEMI = INDEX( FILE, ';' )

*  If found, then select the file name prior to it.
            IF ( SEMI .NE. 0 ) THEN
               NCF = MAX( 1, SEMI - 1 )

*  Otherwise, use the whole file name, excluding trailing blanks.
            ELSE
               NCF = MAX( 1, CHR_LEN( FILE ) )
            END IF

*  See if the file is "odd". Check to see if it has the default file
*  extension of '.SDF' with at least one character preceding it.
            ODD = .TRUE.
            IF ( NCF .GE. 5 ) THEN
               ODD = ( FILE( NCF - 3 : NCF ) .NE. '.SDF' )
            END IF

*  Assume Unix file names if not using VAX/VMS.  Find the file name
*  length.
         ELSE
            NCF = MAX( 1, CHR_LEN( FILE ) )

*  See if the file is "odd". Check to see if it has the default file
*  extension of '.sdf' with at least one character preceding it.
            ODD = .TRUE.
            IF ( NCF .GE. 5 ) THEN
               ODD = ( FILE( NCF - 3 : NCF ) .NE. '.sdf' )
            END IF
         END IF

*  If the file name is not odd, then omit the file extension.
         IF ( .NOT. ODD ) NCF = NCF - 4

*  Enter the file name into the buffer, surrounding it in quotes if it
*  is odd.
         NC = 0
         IF ( ODD ) CALL CHR_PUTC( '"', BUFF, NC )
         CALL CHR_PUTC( FILE( : NCF ), BUFF, NC )
         IF ( ODD ) CALL CHR_PUTC( '"', BUFF, NC )

*  If the object is not a top-level object, then find the position of
*  the first '.' in its pathname, which marks the start of the first
*  component name.
         NCP = MAX( 1, CHR_LEN( PATH ) )
         IF ( NLEV .GT. 1 ) THEN
            DOT = INDEX( PATH( : NCP ), '.' )

*  If successful, see if the '.' is preceded by a '(' indicating that
*  the top-level object is subscripted. Derive the starting position in
*  the path name so that the subscript is used if present.
            IF ( DOT .NE. 0 ) THEN
               BRA = INDEX( PATH( : DOT ), '(' )
               IF ( BRA .NE. 0 ) THEN
                  START = BRA
               ELSE
                  START = DOT
               END IF

*  Add the required part of the path name to the buffer.
               CALL CHR_PUTC( PATH( START : NCP ), BUFF, NC )
            END IF

*  If the object is a top-level object, then see if it is subscripted.
*  If so, then add the subscript to the buffer.
         ELSE
            START = INDEX( PATH( : NCP ), '(' )
            IF ( START .NE. 0 ) THEN
               CALL CHR_PUTC( PATH( START : NCP ), BUFF, NC )
            END IF
         END IF
      END IF

*  Note if any error has occurred.
      OK = STATUS .EQ. SAI__OK

*  If an error occurred, then annul it. Release the error stack.
      IF ( STATUS .NE. SAI__OK ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE

*  If no error occurred, then assign the resulting buffer contents to
*  the message token.
      IF ( OK ) CALL EMS_SETC( TOKEN, BUFF( : NC ) )

      END
