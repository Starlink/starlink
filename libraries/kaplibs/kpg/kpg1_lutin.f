      SUBROUTINE KPG1_LUTIN( INEL, INARR, OUTEL, NN, OUTARR, STATUS )
*+
*  Name:
*     KPG1_LUTIN

*  Purpose:
*     Transfers a lookup table between arrays that have different
*     numbers of colour indices.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LUTIN( INEL, INARR, OUTEL, NN, OUTARR, STATUS )

*  Description:
*     This routine transfers a lookup table between arrays that may
*     have different numbers of colour indices.  When they are unequal
*     the lookup table is expanded or contracted to fit the destination
*     array.  This may be achieved either by linear interpolation or by
*     the nearest-neighbour method.  (There are different sizes
*     presumably because the lookup table was created on a device with
*     a differently sized colour table.)  If the arrays are of equal
*     size the lookup table is merely copied between them.

*  Arguments:
*     INEL = INTEGER (Given)
*        The number of colour indices in the lookup table.
*     INARR( 3, 0:INEL-1 ) = REAL (Given)
*        The source lookup table.  The first dimension is RGB.  Values
*        should lie in the range 0.0--1.0.
*     OUTEL = INTEGER (Given)
*        The number of colour indices available in the destination
*        array.  This is usually the number of colour indices available
*        on the chosen graphics device.
*     NN = LOGICAL (Given)
*        If true, and the number of input and output colour indices are
*        different, the nearest-neighbour method is used for assigning
*        values in the output array.  Otherwise linear interpolation
*        is used.  Nearest-neighbour preserves sharp edges in the
*        lookup; linear interpolation is recommended for smoothly
*        varying lookup tables.
*     OUTARR( 3, 0:OUTEL-1 ) = REAL (Returned)
*        This is the array into which the table is put. The values will
*        all lie in the range 0.0--1.0, even though the input may beyond
*        this range. (Input values below 0.0 become 0.0 in OUTARR, and
*        those above 1.0 become 1.0)
*     STATUS = INTEGER (Given)
*        Global status.

*  Algorithm:
*     -  If the data can be copied, i.e. the number of colour indices
*     are the same then transfer lookup table data from the input array
*     to the lookup-table array, checking the bounds lie between 0 and
*     1.
*     -  For unequal arrays a scale factor is computed and loops for
*     all output-array colour indices finding the equivalent input
*     values either by linear interpolation or nearest neighbour.  The
*     output values are constrained to the range 0.0--1.0.

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
*     1991 May 2 (MJC):
*        Original version based on LUTIN.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No assumed typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  INEL,
     :  OUTEL

      REAL
     :  INARR( 3, 0:INEL-1 )

      LOGICAL
     :  NN

*  Arguments Returned:
      REAL
     :  OUTARR( 3, 0:OUTEL-1 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL
     :  C,                      ! Work variable
     :  FRAC,                   ! Position in input table of output
                                ! colour index as a fraction of a colour
                                ! index between elements
     :  POSIN,                  ! Position in input table of output
                                ! colour index
     :  SCALE                   ! Scale factor between elements of the
                                ! lookup tables

      INTEGER
     :  IPOSIN,                 ! Integer truncated position in input
                                ! table of output colour index
     :  I, J                    ! General variables

*.

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    See whether the data can just be copied.
*    ========================================

      IF ( OUTEL .EQ. INEL ) THEN

*       Transfer table data from the input array to the lookup-table
*       array, checking the bounds.

         DO  I = 0, INEL-1, 1
            DO  J = 1, 3, 1
               OUTARR( J, I ) = MAX( 0., MIN( 1.0, INARR( J, I ) ) )
            END DO
         END DO

*    The arrays have different sizes.
*    ================================

      ELSE

*       Find the resampling scale factor.

         SCALE = REAL( INEL - 1 ) / REAL( OUTEL - 1 )

*       Nearest-neighbour method.
*       =========================

         IF ( NN ) THEN

*          For all entries in the output table.

            DO  I = 0, OUTEL-1, 1

*             Calculate the position in the input table.

               IPOSIN = NINT( SCALE * REAL( I ) )

*             Transfer table data from the input array to the
*             lookup-table array using the nearest neighbour, checking
*             the bounds.

               DO  J = 1, 3, 1
                  OUTARR( J, I ) = MAX( 0., MIN( 1.0,
     :                             INARR( J, IPOSIN ) ) )
               END DO
            END DO

*       Linear interpolation method.
*       ============================

         ELSE

*          For all entries in the output table.

            DO  I = 0, OUTEL-1, 1

*             Calculate the position in the input table and the
*             interpolation fraction.

               POSIN = SCALE * REAL( I )
               IPOSIN = INT( POSIN )
               FRAC = POSIN - REAL( IPOSIN )

*             Transfer table data from the input array to the
*             lookup-table array via linear interpolation, checking the
*             bounds.

               DO  J = 1, 3, 1
                  C = ( 1. - FRAC ) * INARR( J, IPOSIN ) +
     :                FRAC * INARR( J, MIN( INEL-1, IPOSIN + 1 )  )
                  OUTARR( J, I ) = MAX( 0., MIN( 1.0, C ) )
               END DO
            END DO

         END IF

      END IF

      END

