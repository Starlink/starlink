      SUBROUTINE KPG1_AXANO( NDF, IAXIS, DEFAUL, AXSLAB, STATUS )
*+
*  Name:
*     KPG1_AXANO

*  Purpose:
*     Generates an axis annotation from the NDF's axis label and units.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXANO( NDF, IAXIS, DEFAUL, AXSLAB, STATUS )

*  Description:
*     This routine examines a nominated axis for a label and units.
*     It creates a string of the form "label (units)" or "label" if the
*     label but not the units are present.  If neither are present a
*     supplied default is returned instead.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     IAXIS = INTEGER (Given)
*        The number of the axis whose character components are to be
*        used.
*     DEFAUL = CHARACTER * ( * ) (Given)
*        A default to return in AXSLAB should there be no axis label.
*     AXSLAB = CHARACTER * ( * ) (Returned)
*        The axis annotation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The identifier should be associated with an NDF.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 12 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  NDF,
     :  IAXIS

      CHARACTER * ( * )
     :  DEFAUL

*  Arguments Returned:
      CHARACTER * ( * )
     :  AXSLAB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                ! Length of a character string less
                               ! trailing blanks

*  Local Variables:
      LOGICAL                    ! True if:
     :  AXIS,                    ! There is a defined axis system
     :  THERE                    ! The axis units are present

      CHARACTER * 256
     :  LABEL,                   ! Axis label
     :  UNITS                    ! Axis units

      INTEGER
     :  NCLAB,                   ! Number of characters in the label
     :  NCUNIT                   ! Number of characters in the units

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Is there an axis system in the NDF?

      CALL NDF_STATE( NDF, 'Axis', AXIS, STATUS )

      IF ( AXIS ) THEN

*       Obtain the axis annotation.
*       ===========================

*       Set the axis annotation to the default in case there is no axis
*       label, whereupon the default is used.

         LABEL = DEFAUL

*       Get the value and length in characters of the axis label.

         CALL NDF_ACGET( NDF, 'Label', IAXIS, LABEL, STATUS )
         CALL NDF_ACLEN( NDF, 'Label', IAXIS, NCLAB, STATUS )
         NCLAB = CHR_LEN( LABEL( :NCLAB ) )

*       Are the units present?

         CALL NDF_ASTAT( NDF, 'Units', IAXIS, THERE, STATUS )
         IF ( THERE ) THEN

*          Get the value and length in characters of the axis label.

            CALL NDF_ACGET( NDF, 'Units', IAXIS, UNITS, STATUS )
            CALL NDF_ACLEN( NDF, 'Units', IAXIS, NCUNIT, STATUS )
            NCUNIT = CHR_LEN( UNITS( :NCUNIT ) )

*          Form the axis annotation, comprising the label and the units.

            AXSLAB = LABEL( :NCLAB )//' ('//UNITS( :NCUNIT )//')'
         ELSE

*          The axis annotation is just the label.

            AXSLAB = LABEL( :NCLAB )
         END IF
      ELSE

*       Set the axis label to the default as there is no defined axis
*       system.

         AXSLAB = DEFAUL

      END IF

      END
