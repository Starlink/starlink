      SUBROUTINE CCD1_CKFLA( IDS, NNDF, FILTER, STATUS )
*+
*  Name:
*     CCD1_CKFLA

*  Purpose:
*     Checks that the NDFs are suitable for combination into a flatfield
*     master.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKFLA( IDS, NNDF, STATUS )

*  Description:
*     This routine looks for frame types and filters in the input
*     NDFs and checks that each type is indeed a recognised flatfield
*     type. Frames with different types are reported as are frames with
*     no type if at least one frame has a proper type. The flatfields
*     are also checked to see that the filter types are the same.
*     If a single filter type is found then it is returned in FILTER
*     otherwise FILTER is returned as ' '.

*  Arguments:
*     IDS( NNDF ) = INTEGER (Given)
*        The NDF identifiers of the "flatfields".
*     NNDF = INTEGER (Given)
*        The number of NDFs given.
*     FILTER = CHARACTER * ( * ) (Returned)
*        The filter type of the NDFs. Blank if no consensus if found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine is used as part of the "automated" extensions
*     added to CCDPACK as of version 2.0.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1993 (PDRAPER):
*        Original version.
*     26-JAN-1994 (PDRAPER):
*        Added filter argument.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      INTEGER NNDF
      INTEGER IDS( NNDF )

*  Arguments Returned:
      CHARACTER * ( * ) FILTER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXTYP             ! Maximum flatfield types
      PARAMETER ( MAXTYP = 4 )

*  External References:
      LOGICAL CCD1_MATCH
      EXTERNAL CCD1_MATCH        ! String matches one in list
      LOGICAL CHR_SIMLR
      EXTERNAL CHR_SIMLR         ! Strings are similar (case
                                 ! independent)

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FLIST( MAXTYP ) ! List of possible frame types
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! NDF frame type
      CHARACTER * ( CCD1__NMLEN ) STNFIL ! Name of standard filter
      CHARACTER * ( 30 ) DATE    ! Time and date of debiassing
      INTEGER I                  ! Loop variable
      LOGICAL OK                 ! Found value
      LOGICAL MATCH              ! Filters all match.

*  Local Data:
      DATA FLIST / 'FLAT',
     :             'TWILIGHT_SKY',
     :             'NIGHT_SKY',
     :             'DOME' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over all NDFs. Extract the frame type and the filter type.
      MATCH = .TRUE.
      STNFIL = ' '
      DO 1 I = 1, NNDF
         CALL CCG1_FCH0C( IDS( I ), 'FTYPE', FTYPE, OK, STATUS )
         IF ( OK ) THEN

*  Located an NDF type, check it out. If none is located then do
*  nothing.
            IF ( CCD1_MATCH( FTYPE, FLIST, MAXTYP, STATUS ) ) THEN

*  Get the filter type of the NDF
               CALL CCG1_FCH0C( IDS( I ), 'FILTER', FILTER, OK, STATUS )
               IF ( I .NE. 1 ) THEN

*  Check the filter type against that of the first NDF.
                  IF ( .NOT. CHR_SIMLR( STNFIL, FILTER ) ) THEN

*  Filters do not match.
                     MATCH = .FALSE.
                     CALL NDF_MSG( 'NDF', IDS( I ) )
                     CALL CCD1_MSG( ' ',
     :' Warning - filter does not match (^NDF)', STATUS )
                  END IF
               ELSE

*  First time. Record this as the standard filter specification.
                  STNFIL = FILTER
               END IF
            ELSE

*  Isn't recognised as a flatfield. Issue a warning.
               CALL MSG_SETC( 'FTYPE', FTYPE )
               CALL NDF_MSG( 'NDF', IDS( I ) )
               CALL CCD1_MSG( ' ', ' Warning - NDF: ^NDF does not '//
     :'have a recognisable flatfield frame type (^FTYPE)', STATUS )
            END IF
         ELSE

*  No NDF type information so cannot match these.
            MATCH = .FALSE.
         END IF

*  Other checks. See if the NDF has been debiassed.
         CALL CCG1_FCH0C( IDS( I ), 'DEBIAS', DATE, OK, STATUS )
         IF ( .NOT. OK ) THEN
            CALL NDF_MSG( 'NDF', IDS( I ) )
            CALL CCD1_MSG( ' ', ' Warning - NDF: ^NDF does not appear'//
     :' to have been debiassed', STATUS )
         END IF
 1    CONTINUE

*  See if the filter type has been found.
      IF ( MATCH ) THEN
         FILTER = STNFIL
      ELSE
         FILTER = ' '
      END IF

      END
* $Id$
