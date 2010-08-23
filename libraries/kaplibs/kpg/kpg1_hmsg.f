      SUBROUTINE KPG1_HMSG( TOKEN, LOC )
*+
*  Name:
*     KPG1_HMSG

*  Purpose:
*     Assigns the name of an HDS object to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_HMSG( TOKEN, LOC )

*  Description:
*     The routine assigns the full name (including the file name) of an
*     HDS object to a message token for use with the ERR_ and MSG_
*     routines (SUN/104). Appropriate syntax is used to represent file
*     names which do not have the standard (.SDF) file type.

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

*  Algorithm:
*     -  Mark the error stack and initialise status.
*     -  Initiallise the error flag.
*     -  Obtain the data object path and container file name.
*     -  If successful, then locate the semicolon which delimits the
*     version number in the file name.
*     -  If found, then select the file name prior to it.
*     -  Otherwise, use the whole file name, excluding trailing blanks.
*     -  See if the file is "odd" (i.e. does not have a file type of
*     .SDF.  If it does, then note it is not odd and remove the file
*     type field (otherwise this field is kept and used).
*     -  Enter the file name into the buffer, surrounding it in quotes
*     if it is odd.
*     -  If the file name is odd, then append the full path name to it.
*     -  Otherwise, if the object is not a top-level object, then find
*     the length of its path name and the position of the first '.',
*     which marks the start of the first component name.
*     -  If successful, add the remainder of the path name to the
*     buffer.
*     -  Note if any error has occurred.
*     -  If an error occurred, then annul it. Release the error stack.
*     -  If no error occurred, then assign the resulting buffer
*     contents to the message token.

*  Implementation Deficiencies:
*     -  The format of HDS object names produced by this routine may
*     need revision once the style to be used by ADAM for referring to
*     objects in files which do not have the default type (or version
*     number) has been clarified.

*  VAX-specific features used:
*     -  This routine makes assumptions about the form of a VMS file
*     name.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1990 (RFWS):
*        Original version.
*     26-NOV-1990 (RFWS):
*        Moved the call to MSG_SETC outside the local error context to
*        prevent the message token value from being lost.
*     2-JAN-1991 (RFWS):
*        Fixed problem with undefined path name length.
*     1991 March 27 (MJC):
*        Renamed from NDF1_MSG and removed NDF_CONST include file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) LOC

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER NDF__SZFIL         ! Size of HDS file specification
      PARAMETER ( NDF__SZFIL = 256 )
      INTEGER NDF__SZPTH         ! Size of HDS file specification
      PARAMETER ( NDF__SZPTH = 256 )

*  Local Variables:
      CHARACTER * ( NDF__SZFIL ) FILE ! Container file name
      CHARACTER * ( NDF__SZFIL + NDF__SZPTH ) BUFF ! Buffer
      CHARACTER * ( NDF__SZPTH ) PATH ! Object path name
      INTEGER DOT                ! Position of '.'
      INTEGER NC                 ! No. characters in buffer
      INTEGER NCF                ! No. characters in filename
      INTEGER NCP                ! No. characters in path name
      INTEGER NLEV               ! Object level in HDS
      INTEGER SEMI               ! Position of ';'
      INTEGER STATUS             ! Local status variable
      LOGICAL ODD                ! Is the filename odd?
      LOGICAL OK                 ! No error has occurred?

*.

*  Mark the error stack and initialise status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Initialise the error flag.
      OK = .FALSE.

*  Obtain the data object path and container file name.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  If successful, then locate the semicolon which delimits the version
*  number in the file name.
      IF ( STATUS .EQ. SAI__OK ) THEN
         SEMI = INDEX( FILE, ';' )

*  If found, then select the file name prior to it.
         IF ( SEMI .NE. 0 ) THEN
            NCF = MAX( 1, SEMI - 1 )

*  Otherwise, use the whole file name, excluding trailing blanks.
         ELSE
            NCF = MAX( 1, CHR_LEN( FILE ) )
         END IF

*  See if the file is "odd" (i.e. does not have a file type of .SDF).
         ODD = .TRUE.
         IF ( NCF .GE. 5 ) THEN

*  If it does, then note it is not odd and remove the file type field
*  (otherwise this field is kept and used).
            IF ( FILE( NCF - 3 : NCF ) .EQ. '.SDF' ) THEN
               ODD = .FALSE.
               NCF = NCF - 4
            END IF
         END IF

*  Enter the file name into the buffer, surrounding it in quotes if it
*  is odd.
         NC = 0
         IF ( ODD ) CALL CHR_PUTC( '"', BUFF, NC )
         CALL CHR_PUTC( FILE( : NCF ), BUFF, NC )
         IF ( ODD ) CALL CHR_PUTC( '"', BUFF, NC )

*  If the file name is odd, then append the full path name to it.
         NCP = MAX( 1, CHR_LEN( PATH ) )
         IF ( ODD ) THEN
            CALL CHR_PUTC( PATH( : NCP ), BUFF, NC )

*  Otherwise, if the object is not a top-level object, then find the
*  position of the first '.', which marks the start of the first
*  component name.
         ELSE IF ( NLEV .GT. 1 ) THEN
            DOT = INDEX( PATH, '.' )

*  If successful, add the remainder of the path name to the buffer.
            IF ( DOT .NE. 0 ) THEN
               CALL CHR_PUTC( PATH( DOT : NCP ), BUFF, NC )
            END IF
         END IF

*  Note if any error has occurred.
         OK = STATUS .EQ. SAI__OK
      END IF

*  If an error occurred, then annul it. Release the error stack.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  If no error occurred, then assign the resulting buffer contents to
*  the message token.
      IF ( OK ) CALL MSG_SETC( TOKEN, BUFF( : NC ) )

      END
