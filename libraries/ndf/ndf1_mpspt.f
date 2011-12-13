      SUBROUTINE NDF1_MPSPT( MAP, MAP0, MAP1, MAP2, INPRM, OUTPRM,
     :                       STATUS )
*+
*  Name:
*     NDF1_MPSPT

*  Purpose:
*     Split a Mapping up into two parallel Mappings with and without
*     inverse transformations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MPSPT( MAP, MAP0, MAP1, MAP2, INPRM, OUTPRM, STATUS )

*  Description:
*     This routine attempts to partition the inputs of the supplied
*     Mapping into two groups; a group that can be re-constructed from
*     a corresponding group of Mapping outputs, and a group that cannot
*     be re-constructed because the required inverse transformation is
*     not defined. A Mapping is returned for each group, together with
*     the permutation arrays needed to permute the inputs and outputs of
*     these Mappings back into their original order.

*  Arguments:
*     MAP = INTEGER (Given)
*        AST pointer to the Ampping to be analysed.
*     MAP0 = INTEGER (Returned)
*        A copy of the supplied Mapping that has been modified, if
*        necessary, to ensure that an inverse transformation is defined.
*        If the supplied Mapping did not itself have a defined inverse
*        transformation, then an inverse transformation will be supplied
*        that simply returns bad values for the axes that did not
*        originally have an inverse.
*     MAP1 = INTEGER (Returned)
*        A Mapping that transforms a subset of the inputs of MAP into a
*        subset of the outputs of MAP. The subset contains all inputs for
*        which the Mapping has an inverse transformation (i.e. all inputs
*        that can be re-generated from the corresponding output values).
*        AST__NULL will be returned if no unique subset of inputs can be
*        found that has a defined inverse.
*     MAP2 = INTEGER (Returned)
*        A Mapping that transforms a subset of the inputs of MAP into a
*        subset of the outputs of MAP. The subset contains all inputs for
*        which the Mapping does not have an inverse transformation (i.e. all
*        inputs that cannot be re-generated from the corresponding output
*        values). AST__NULL will be returned if all the inputs have defined
*        inverse transformations.
*     INPRM( NDF__MXDIM ) = INTEGER (Returned)
*        The index into this array is the index of an input to MAP. The
*        values returned in the array are the indices of the corresponding
*        inputs within a compound Mapping formed by combining MAP1 and MAP2
*        in parallel.
*     OUTPRM( NDF__MXDIM ) = INTEGER (Returned)
*        The index into this array is the index of an output within a
*        compound Mapping formed by combining MAP1 and MAP2. The values
*        returned in the array are the indices of the corresponding
*        outputs within MAP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     9-OCT-2007 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      INTEGER MAP

*  Arguments Returned:
      INTEGER MAP0
      INTEGER MAP1
      INTEGER MAP2
      INTEGER INPRM( NDF__MXDIM )
      INTEGER OUTPRM( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER IIN1
      INTEGER IIN2
      INTEGER ININD( NDF__MXDIM )
      INTEGER INMAP( NDF__MXDIM )
      INTEGER IOUT1
      INTEGER IOUT2
      INTEGER J
      INTEGER MAPS( NDF__MXDIM )
      INTEGER NIN
      INTEGER NMAP
      INTEGER NOUT
      INTEGER OUTIND( NDF__MXDIM )
      INTEGER OUTMAP( NDF__MXDIM )
      INTEGER TMAP
      LOGICAL HASINV( NDF__MXDIM )
*.

*  Initialise the returned values.
      MAP0 = AST__NULL
      MAP1 = AST__NULL
      MAP2 = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of inputs and outputs for the supplied Mapping.
      NIN = AST_GETI( MAP, 'Nin', STATUS )
      NOUT = AST_GETI( MAP, 'Nout', STATUS )

*  Split the supplied Mapping up into the largest possible number
*  of parallel Mappings.
      CALL NDF1_MPANL( MAP, NMAP, MAPS, HASINV, INMAP, ININD,
     :                 OUTMAP, OUTIND, MAP0, STATUS )

*  Loop round all the component Mappings found above, accumulating all
*  the component Mappings that have, and do not have, defined inverse
*  transformations within the supplied Mapping.
      DO I = 1, NMAP

*  If this component Mapping has an inverse transformation in the
*  supplied Mapping, add it into the current MAP1 Mapping.
         IF( HASINV( I ) ) THEN
            IF( MAP1 .EQ. AST__NULL ) THEN
               MAP1 = AST_CLONE( MAPS( I ), STATUS )
            ELSE
               TMAP = AST_CMPMAP( MAP1, MAPS( I ), .FALSE., ' ',
     :                            STATUS )
               CALL AST_ANNUL( MAP1, STATUS )
               MAP1 = TMAP
            END IF

*  If this component Mapping has no inverse transformation in the
*  supplied Mapping, add it into the current MAP2 Mapping.
         ELSE
            IF( MAP2 .EQ. AST__NULL ) THEN
               MAP2 = AST_CLONE( MAPS( I ), STATUS )
            ELSE
               TMAP = AST_CMPMAP( MAP2, MAPS( I ), .FALSE., ' ',
     :                            STATUS )
               CALL AST_ANNUL( MAP2, STATUS )
               MAP2 = TMAP
            END IF
         END IF

      END DO

*  Get the number of inputs and outputs for MAP1 and MAP2
      IF( MAP1 .NE. AST__NULL ) THEN
         IIN2 = AST_GETI( MAP1, 'Nin', STATUS )
         IOUT2 = AST_GETI( MAP1, 'Nout', STATUS )
      ELSE
         IIN2 = 0
         IOUT2 = 0
      END IF
      IIN1 = 0
      IOUT1 = 0

*  Now loop round all the component Mappings again.
      DO I = 1, NMAP

*  Find all inputs of the supplied Mapping that feed the current component
*  Mapping. For each, store the index of the corresponding input within a
*  parallel CmpMap holding MAP1 and MAP2.
         DO J = 1, NIN
            IF( INMAP( J ) .EQ. I ) THEN

               IF( HASINV( I ) ) THEN
                  INPRM( J ) = IIN1 + ININD( J )
               ELSE
                  INPRM( J ) = IIN2 + ININD( J )
               END IF

            END IF
         END DO

*  Find all outputs of the supplied Mapping that are fed by the current
*  component Mapping. For each, store the index of the corresponding
*  output within a parallel CmpMap holding MAP1 and MAP2.
         DO J = 1, NOUT
            IF( OUTMAP( J ) .EQ. I ) THEN

               IF( HASINV( I ) ) THEN
                  OUTPRM( IOUT1 + OUTIND( J ) ) = J
               ELSE
                  OUTPRM( IOUT2 + OUTIND( J ) ) = J
               END IF

            END IF
         END DO

*  Update the number of inputs and outputs used for each of MAP1 and MAP2.
         IF( HASINV( I ) ) THEN
            IIN1 = IIN1 + AST_GETI( MAPS( I ), 'Nin', STATUS )
            IOUT1 = IOUT1 + AST_GETI( MAPS( I ), 'Nout', STATUS )
         ELSE
            IIN2 = IIN2 + AST_GETI( MAPS( I ), 'Nin', STATUS )
            IOUT2 = IOUT2 + AST_GETI( MAPS( I ), 'Nout', STATUS )
         END IF

*  Free the Mapping pointer since it is no longer needed.
         CALL AST_ANNUL( MAPS( I ), STATUS )

      END DO

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MPSPT', STATUS )

      END
