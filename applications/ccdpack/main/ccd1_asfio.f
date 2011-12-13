      SUBROUTINE CCD1_ASFIO( PNFILE, ACMODE, FORM, RECSZ, FD, OPEN,
     :                       STATUS )
*+
*  Name:
*     CCD1_ASFIO

*  Purpose:
*     Opens a sequential file via an ADAM parameter

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ASFIO( PNFILE, ACMODE, FORM, RECSZ, FD, OPEN, STATUS )

*  Description:
*     This routine opens a sequential file via FIO_OPEN, using the
*     parameter PNFILE (which should declared a literal).  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact. If the file exists and is opened for write access
*     and the open fails then an attempt to delete the previous file
*     is made which if successful is followed by another attempt to
*     open the file for write. If this fails then a error is reported
*     etc. The file opened should be closed using FIO_CLOSE and the
*     parameter annuled using PAR_CANCL.

*  Arguments:
*     PNFILE = CHARACTER *( * ) (Given)
*        Parameter name by which file is to be opened
*     ACMODE = CHARACTER *( * ) (Given)
*        Expression giving the required access mode.
*        Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*        For details, see FIO_OPEN.
*     FORM = CHARACTER *( * )(Given)
*         Expression giving the required formatting of the file.
*         Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*         'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ = INTEGER (Given)
*         Expression giving the maximum record size in bytes.
*         Set it to zero if the Fortran default is required.
*     FD = INTEGER (Returned)
*         Variable to contain the file descriptor.
*     OPEN = LOGICAL (Returned)
*         If true the file has been opened.
*     STATUS = INTEGER (Given and Returned)
*         Global status value

*  Copyright:
*     Copyright (C) 1989, 1992-1993 Science & Engineering Research
*     Copyright (C) 2008 Science and Technology Facilities Council
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1989 (MJC):
*        Original version.
*     11-MAY-1992 (PDRAPER):
*        Extracted from AIF and renamed for PISA use. Updated
*        prologue.
*     18-MAR-1993 (PDRAPER):
*        Added checks for file opened for write access which fails
*        (on UNIX system).
*     8-FEB-2008 (PDRAPER):
*        Initialise FD to protect against exit without checking OPEN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants
      INCLUDE 'FIO_PAR'          ! FIO system constants

*  Arguments Given:
      CHARACTER * ( * ) PNFILE
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) FORM
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD
      LOGICAL OPEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIO_TEST
      LOGICAL FIO_TEST           ! Tests FIO generic errors

*  Local Constants:
      INTEGER MXLOOP             ! Maximum number of attempts to open a
                                 ! file
      PARAMETER ( MXLOOP = 4 )

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! Name of file to be opened
      INTEGER LOOP               ! Number of attempts to open file
      LOGICAL LOOPAG             ! True if another attempt to open file
                                 ! is to be made

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise variables.
      LOOP = 0
      LOOPAG = .TRUE.
      OPEN = .FALSE.
      FD = 0

*  Loop until exceed number of tries or file is sucessfully opened.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( LOOPAG ) THEN

*  Open file via the given parameter name with the given
*  characteristics.
         CALL PAR_GET0C( PNFILE, FNAME, STATUS )
         CALL FIO_OPEN( FNAME, ACMODE, FORM, RECSZ, FD, STATUS )

*  Check status for PAR_ABORT or PAR_NULL.
         IF ( STATUS .EQ. PAR__ABORT .OR. STATUS .EQ. PAR__NULL ) THEN
            OPEN = .FALSE.
            LOOPAG = .FALSE.

*  If PAR__NULL then annul status and any error messages.
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Other non-OK status, must have problems opening the file.
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  If the file is opened for write access and FIO has issued an OPEN
*  error then try to delete the current file (assumes a UNIX system)
*  and if this succeeds try to open the file again).
            IF ( ACMODE .EQ. 'WRITE' .AND. FIO_TEST( 'OPEN error',
     :           STATUS ) ) THEN

*  Annul the error.
               CALL ERR_ANNUL( STATUS )

*  Delete the file!
               CALL FIO_ERASE( FNAME, STATUS )

*  And try to open it again.
               CALL FIO_OPEN( FNAME, ACMODE, FORM, RECSZ, FD, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Succeed stop looping and set flag.
                  LOOPAG = .FALSE.
                  OPEN = .TRUE.
               END IF
            END IF

*  Increment loop count, flush error and issue informative message
*  if still failing.
            IF ( LOOPAG ) THEN
               LOOP = LOOP + 1
               IF ( LOOP .LE. MXLOOP ) THEN
                  CALL ERR_REP( 'CCD1_ASFIO1',
     :              '  Failed to open file - try again',
     :              STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE

*  Exceeded number of tries - abort.
                  LOOPAG = .FALSE.
               END IF
            END IF

*  Cancel parameter association
            CALL PAR_CANCL( PNFILE, STATUS )
         ELSE

*  Opened file sucessfully, stop looping and set open flag.
            LOOPAG = .FALSE.
            OPEN = .TRUE.
         END IF

*  Return for another go if required.
         GO TO 1
      END IF

*  If status set on exit then affirm action.
      IF ( STATUS .NE. SAI__OK .AND. LOOP .EQ. MXLOOP ) THEN
         CALL ERR_REP( 'CCD1_ASFIO2',
     :     '  Repeatedly unable to open file - aborting.', STATUS )
      END IF

      END
* $Id$
