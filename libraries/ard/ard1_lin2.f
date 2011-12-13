      SUBROUTINE ARD1_LIN2( RINDEX, NDIM, NWCS, LBND, UBND, MSKSIZ,
     :                      NPAR, PAR, NLP, IWCS, B, LBEXTB, UBEXTB,
     :                      LBINTB, UBINTB, WORK, STATUS )
*+
*  Name:
*     ARD1_LIN2

*  Purpose:
*     Initialise an array to hold a LINE region, with non-linear user
*     coords.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LIN2( RINDEX, NDIM, NWCS, LBND, UBND, MSKSIZ, NPAR, PAR,
*                     NLP, IWCS, B, LBEXTB, UBEXTB, LBINTB, UBINTB, WORK,
*                     STATUS )

*  Description:
*     Interior values are assigned to the points specified by the
*     supplied parameters. The supplied parameters are the pixel
*     co-ordinates of the two end points of the line.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of pixel axes
*     NWCS = INTEGER (Given)
*        The number of user axes
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        A list of pixel co-ordinates, in groups of NDIM.
*     NLP = INTEGER (Given)
*        The number of points along the line to be transformed.
*     IWCS = INTEGER (Given)
*        An identifer for an AST FrameSet. The Base Frame should be
*        PIXEL coordinates within the B array. The Current Frame should
*        be user coordinates.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     WORK( NLP, * ) = DOUBLE PRECISION (Given and Returned)
*        Work array. The size of the second axis should be the maximum of
*        NDIM and NWCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1994 (DSB):
*        Original version.
*     15-JUN-2001 (DSB):
*        Changed to be called from ARD1_KDRAW and use AST.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER NWCS
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      INTEGER NLP
      INTEGER IWCS

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )
      DOUBLE PRECISION WORK( NLP, * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I,                 ! Position index
     :        J,                 ! Axis index
     :        MAP,               ! AST identifier for user->pixel Mapping
     :        NPREQ              ! No of axis value to define a line

      DOUBLE PRECISION
     :        DELTA( ARD__MXDIM ),! Steps on all axes between points
     :        PC( 2*ARD__MXDIM )  ! Pixel coords at line ends

      LOGICAL
     :        GOOD,              ! Is this position good?
     :        LGOOD              ! Was the previous position good?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of parameters required.
      NPREQ = 2*NDIM

*  Report an error if the number of supplied parameters is wrong.
      IF( NPAR .NE. NPREQ ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_LIN2_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ^ND D region in ARD1_LIN2 '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Get the user -> pixel Mapping from the FrameSet, and the number of
*  user axes.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__CURRENT,
     :                                    AST__BASE, STATUS ),
     :                    STATUS )

*  Fill the work array with NLP positions evenly spaced along the line in
*  user coords.
      DO J = 1, NWCS
         DELTA( J ) = ( PAR( J + NWCS ) - PAR( J ) )/DBLE( NLP - 1 )
      END DO

      DO I = 1, NLP
         DO J = 1, NWCS
            WORK( I, J ) = PAR( J ) + DELTA( J )*( I - 1 )
         END DO
      END DO

*  Transform these users coords into pixel coords.
      CALL AST_TRANN( MAP, NLP, NWCS, NLP, WORK, .TRUE., NDIM, NLP,
     :                WORK, STATUS )

*  Initialise the interior bounding box. At the same time, store the first
*  position at the upper end of the PC array, and see if it is good.
      GOOD = .TRUE.
      DO J = 1, NDIM
         LBINTB( J ) = VAL__MAXI
         UBINTB( J ) = VAL__MINI
         IF( WORK( 1, J ) .EQ. AST__BAD ) THEN
            GOOD = .FALSE.
         ELSE
            PC( NDIM + J ) = WORK( 1, J )
         END IF
      END DO

*  Loop round each line segment in the transformed curve.
      DO I = 2, NLP

*  Save the good/bad flag for the previous position.
         LGOOD = GOOD

*  Move the previous position down from the upper end of the PC array to
*  the lower end. At the same time check if this position is good.
         GOOD = .TRUE.
         DO J = 1, NDIM
            PC( J ) = PC( NDIM + J )

            IF( WORK( I, J ) .EQ. AST__BAD ) THEN
               GOOD = .FALSE.
            ELSE
               PC( NDIM + J ) = WORK( I, J )
            END IF

         END DO

*  If both this position and the previous position were good, draw the
*  line.
         IF( GOOD .AND. LGOOD ) THEN
            CALL ARD1_LINFL( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPREQ,
     :                       PC, B, LBINTB, UBINTB, STATUS )
         END IF

      END DO

 999  CONTINUE

      END
