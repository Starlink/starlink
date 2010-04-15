      LOGICAL FUNCTION ARY1_DEFR( IDCB, STATUS )
*+
*  Name:
*     ARY1_DEFR

*  Purpose:
*     See if the creation of the arrays has been deferred.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARY1_DEFR( IDCB, STATUS )

*  Description:
*     Arrays created via ARY_DUPE initially have no HDS data objects to
*     contain the real and imaginary array values. The creation of these
*     arrays is deferred until they are mapped. This is done so that
*     any changes made to the properties (e.g. type, bounds, etc) of
*     the deferred array (before it is mapped) are reflected in the size
*     of the corresponding HDS container file. If the array creation is
*     not deferred, then any changes which should produce a reduction in
*     the container file size do not in fact do so because HDS never
*     shrinks the size of a container file (it is just padded with unused
*     space).

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Function value:
*     ARY1_DEFR = LOGICAL
*        If .TRUE. then the HDS arrays holding the real and imaginary
*        array values have not yet been created.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     17-JUL-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to non-imaginary data component.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS               ! Global status

*  Local variables:
      CHARACTER LOC*( DAT__SZLOC ) ! Locator to first component
      CHARACTER NAME*( DAT__SZNAM )! Name of first component
      INTEGER I                    ! Component index
      INTEGER NCOMP                ! Number of components
      LOGICAL PRIM                 ! Is the locator primitive?
*.

*  Initialise
      ARY1_DEFR = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The array is deferred if the DCB_DLOC locator (for the real
*  component) is null and the data object is either empty or contains a
*  single component called VARIANT.
      IF( DCB_DLOC( IDCB ) .EQ. ARY__NOLOC ) THEN
         CALL DAT_PRIM( DCB_LOC( IDCB ), PRIM, STATUS )
         IF( .NOT. PRIM ) THEN
            ARY1_DEFR = .TRUE.
            CALL DAT_NCOMP( DCB_LOC( IDCB ), NCOMP, STATUS )
            DO I = 1, NCOMP
               CALL DAT_INDEX( DCB_LOC( IDCB ), I, LOC, STATUS )
               CALL DAT_NAME( LOC, NAME, STATUS )
               IF( NAME .EQ. 'DATA' ) ARY1_DEFR = .FALSE.
               CALL DAT_ANNUL( LOC, STATUS )
            END DO
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DEFR', STATUS )

      END
