      SUBROUTINE ERR1_PRERR( TEXT, STATUS )
*+
*  Name:
*     ERR1_PRERR

*  Purpose:
*     Deliver the text of an error message to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR1_PRERR( TEXT, STATUS )

*  Description:
*     This uses SUBPAR_WRERR to send a message to the user.
*     Trailing blanks are removed. 
*     If SUBPAR_WRERR fails, an attempt is made to write the message
*     on STDERR. If that fails, or if STDERR was not a TTY, attempt to
*     output on STDOUT.
*     That is, we make every effort to ensure that the user sees the message.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This is the UNIX ADAM version of ERR1_PRERR.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1991 (PCTR):
*        Original version.
*     8-AUG-1991 (PCTR):
*        EMS Vn. 1.3 changes.
*    19-SEP-1994 (AJC)
*        Change to using SUBPAR_WRERR.
*        Avoid outputting to STDOUT if both it and STDERR are TTY.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'ERR_ERR'                 ! ERR_ error codes
      INCLUDE 'ERR_PAR'                 ! ERR_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! String length

      INTEGER STDEFD                    ! Standard error Unix File Descriptor
      PARAMETER( STDEFD = 2 )           !  - used by PSX

      INTEGER STDELU                    ! Standard error Fortran logical unit
      PARAMETER( STDELU = 0 )           !  - used by Fortran WRITE

*  Local Variables:
      INTEGER IOSTAT                    ! Fortran IOSTAT status
      INTEGER LENG                      ! String length
      INTEGER LSTAT                     ! Local status
      LOGICAL ERRTTY                    ! Whether STDERR is a TTY
*.

*  Initialise the local status.
      LSTAT = SAI__OK

*  Get the length of the error message.
      LENG = MIN( CHR_LEN( TEXT ), ERR__SZMSG )
      LENG = MAX( LENG, 1 )

*  Attempt to deliver the message via the user interface.
      CALL SUBPAR_WRERR( TEXT( 1 : LENG ), LSTAT )

*  Check the returned status and attempt to report the message on 
*  STDERR. If that fails or if STDERR was not a terminal try STDOUT.
      IF ( LSTAT .NE. SAI__OK ) THEN

*     Set the returned status.
         STATUS = ERR__OPTER

*     Write the message to STDERR
         WRITE( STDELU, '( A )', IOSTAT = IOSTAT ) TEXT( 1 : LENG )

*     If that failed or STDERR was not a TTY, try STDOUT
         CALL PSX_ISATTY( STDEFD, ERRTTY, LSTAT )
         IF ( ( IOSTAT .NE. 0 )
     :   .OR. ( ( LSTAT .EQ. SAI__OK ) .AND. .NOT. ERRTTY ) ) THEN           

*        Write the message to STDOUT
            WRITE( *, '( A )', IOSTAT = IOSTAT ) TEXT( 1 : LENG )
         END IF
      END IF

      END
