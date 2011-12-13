      SUBROUTINE TRN_TR2D( BAD, NXY, XIN, YIN, IDT, XOUT, YOUT,
     :                       STATUS )







*+
*  Name:
*     TRN_TR2D

*  Purpose:
*     transform 2-dimensional DOUBLE PRECISION data.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_TR2D( BAD, NXY, XIN, YIN, IDT, XOUT, YOUT, STATUS )

*  Description:
*     The routine applies a compiled transformation to a set of
*     2-dimensional data points supplied as two separate (X and Y)
*     arrays of DOUBLE PRECISION coordinate values.

*  Arguments:
*     BAD = LOGICAL (given)
*        Whether the input coordinates may be "bad".
*     NXY = INTEGER (given)
*        The number of data points to transform.
*     XIN( NXY ) = DOUBLE PRECISION (given)
*        Input X coordinate values for the data points.
*     YIN( NXY ) = DOUBLE PRECISION (given)
*        Input Y coordinate values for the data points.
*     IDT = INTEGER (given)
*        ID for the compiled transformation to be applied.
*     XOUT( NXY ) = DOUBLE PRECISION (returned)
*        Array to receive the transformed X coordinate values.
*     YOUT( NXY ) = DOUBLE PRECISION (returned)
*        Array to receive the transformed Y coordinate values.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Loop to transform the coordinates in batches, whose size is
*       determined by the amount of locally declared workspace.
*     - Find the range of data points in each batch and copy the
*       coordinate values to the input workspace.
*     - Call TRN_TRND to perform the transformation.
*     - Copy the transformed coordinates to the output arrays.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      LOGICAL BAD               ! Whether input coordinates may be "bad"

      INTEGER NXY               ! Number of data points to transform

      DOUBLE PRECISION XIN( * )           ! Input X coordinate values

      DOUBLE PRECISION YIN( * )           ! Input Y coordinate values

      INTEGER IDT               ! ID for the compiled transformation to
                                ! be applied

*  Arguments Returned:
      DOUBLE PRECISION XOUT( * )          ! Array for transformed X coordinate
                                ! values

      DOUBLE PRECISION YOUT( * )          ! Array for transformed Y coordinate
                                ! values


*  Status:
      INTEGER STATUS            ! Error status


*  Local Constants:
      INTEGER NWRK              ! Maximum number of data points to
                                ! transform in one batch

      PARAMETER ( NWRK = 256 )


*  Local Variables:
      INTEGER ISTART            ! First data point in batch

      INTEGER IEND              ! Last data point in batch

      INTEGER IBATCH            ! Number of data points in batch

      DOUBLE PRECISION WRK1( NWRK, 2 )    ! Workspace for input coordinate values

      DOUBLE PRECISION WRK2( NWRK, 2 )    ! Workspace for output coordinate values

      INTEGER IERRL             ! Local numerical error pointer

      INTEGER NERRL             ! Local numerical error count

      INTEGER NSTATL            ! Local numerical error status variable


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise the local numerical error status variable (note this
*   will never be set because only data copying operations are used
*   and these cannot generate numerical errors).
      NSTATL = SAI__OK


*   Loop to process the input data in batches.
      ISTART = 1
      DO WHILE( ( ISTART .LE. NXY ) .AND. ( STATUS .EQ. SAI__OK ) )


*   Find the number of data points in the batch.
        IEND = MIN( ISTART + NWRK - 1, NXY )
        IBATCH = IEND - ISTART + 1


*   Copy the input coordinate values for the batch into the input
*   workspace.
        CALL VEC_DTOD( BAD, IBATCH, XIN( ISTART ), WRK1( 1, 1 ),
     :                     IERRL, NERRL, NSTATL )
        CALL VEC_DTOD( BAD, IBATCH, YIN( ISTART ), WRK1( 1, 2 ),
     :                     IERRL, NERRL, NSTATL )


*   Transform the data points.
        CALL TRN_TRND( BAD, NWRK, 2, IBATCH, WRK1, IDT, NWRK, 2, WRK2,
     :                   STATUS )


*   Copy the transformed coordinate values into the output arrays.
        CALL VEC_DTOD( BAD, IBATCH, WRK2( 1, 1 ), XOUT( ISTART ),
     :                     IERRL, NERRL, NSTATL )
        CALL VEC_DTOD( BAD, IBATCH, WRK2( 1, 2 ), YOUT( ISTART ),
     :                     IERRL, NERRL, NSTATL )


*   Increment the pointer to the start of the next batch.
        ISTART = IEND + 1


*   End of "loop to process the input data in batches" loop.
      ENDDO


*   Exit routine.
      END


