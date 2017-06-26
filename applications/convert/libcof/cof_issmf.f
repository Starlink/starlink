      SUBROUTINE COF_ISSMF( FUNIT, ISSMF, STATUS )
*+
*  Name:
*     COF_ISSMF

*  Purpose:
*     See if a FITS file was created from an NDF with a SMURF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_ISSMF( FUNIT, ISSMF, STATUS )

*  Description:

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical-unit number of the FITS file.
*     ISSMF = LOGICAL  (Returned)
*        Returned .TRUE. if the FITS file was created from an NDF with a
*        SMURF extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     26-JUN-2017 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT

*  Arguments Returned:
      LOGICAL ISSMF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER  FITSOK             ! Good status for FITSIO library
      PARAMETER ( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      CHARACTER * ( 70 ) EXTNAM  ! Value of EXTNAME keyword
      INTEGER CHDU               ! Number of the original current HDU
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type
      INTEGER IHDU               ! Number of the current HDU
      INTEGER NHDU               ! Number of HDUs
      LOGICAL THERE              ! Does the keyword exist?

*.

*  Initialise returned values.
      ISSMF = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the index of the current HDU, so that we can re-istate it before
*  leaving.
      CALL FTGHDN( FUNIT, CHDU )

*  Get the total number of HDUs.
      CALL FTTHDU( FUNIT, NHDU, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL CVG_FIOER( FSTAT, 'COF_ISSMF_NHDU', 'FTTHDU',
     :                   'Unable to get the number of HDUs in the '//
     :                   'FITS file ', STATUS )
         GO TO 999
      END IF

*  Loop round all HDSs.
      DO IHDU = 1, NHDU

*  Move to the next HDU.
         CALL FTMAHD( FUNIT, IHDU, HDUTYP, FSTAT )
         IF ( FSTAT .NE. FITSOK ) THEN
            CALL MSG_SETI( 'I', IHDU )
            CALL CVG_FIOER( FSTAT, 'COF_ISSMF_IHDU', 'FTMAHD',
     :                      'Unable to move to the HDU number ^I',
     :                      STATUS )
            GO TO 999
         END IF

*  Get the name of the extension stored in the HDU from the EXTNAME keyword.
         CALL COF_GKEYC( FUNIT, 'EXTNAME', THERE, EXTNAM, COMENT,
     :                   STATUS )

*  If it contains ".MORE.SMURF." then we have found an item from a SMURF
*  extension item, so break out of the loop and return .TRUE.
         IF( THERE .AND. INDEX( EXTNAM, 'SMURF' ) .NE. 0 ) THEN
            ISSMF = .TRUE.
            GO TO 1
         END IF

      END DO

*  Reset the original HDU.
1     CALL FTMAHD( FUNIT, CHDU, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL MSG_SETI( 'I', CHDU )
         CALL CVG_FIOER( FSTAT, 'COF_ISSMF_IHDU', 'FTMAHD',
     :                   'Unable to move to the HDU number ^I',
     :                   STATUS )
      END IF

  999 CONTINUE

      END
