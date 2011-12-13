      SUBROUTINE ARD1_LKR( INDEX1, NDIM, LBND, UBND, MSKSIZ, MASK,
     :                     OPNSIZ, IOPND, LEX0, UEX0, LIN0, UIN0,
     :                     OPRNDS, IPB, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     LOADED, RINDEX, STATUS )
*+
*  Name:
*     ARD1_LKR

*  Purpose:
*     Set an array to hold a keyword region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LKR( INDEX1, NDIM, LBND, UBND, MSKSIZ, MASK, OPNSIZ,
*                    IOPND, LEX0, UEX0, LIN0, UIN0, OPRNDS, IPB, LBEXTB,
*                    UBEXTB, LBINTB, UBINTB, LOADED, RINDEX, STATUS )

*  Description:
*     The supplied value of IOPND is a pointer into the OPRNDS array.
*     The identified element gives the integer code corresponding to
*     the type of region (CIRCLE, BOX, etc) required. The next element
*     contains an integer which gives the position of the keyword
*     within the ARD description (1 for the first keyword, 2 for the
*     second, etc, excluding INPUT keywords).
*
*     The next (third) element contains an integer which gives the number of
*     parameters following. These parameters described the size, position,
*     etc, of the region. These are exactly as supplied in the ARD
*     expression (just converted from text to floating poitn).
*
*     The next element after the parameters (index 4+NPAR) indicates the
*     nature of the Mapping from pixel coords (in the B array) to user coords.
*     If it is zero, then the Mapping is non-linear, and the following element
*     contains the DOUBLE PRECISION equivalence of an INTEGER value
*     representing an AST FrameSet pointer. The Base Frame of this FrameSet
*     should be pixel coords and the Current Frame should be user coords.
*     The next element contains a lower limit on the distance in
*     user coords per pixel. This may vary across the array, and may
*     depend on direction, but a lower limit is all that is needed.
*
*     If the element with index 4+NPAR is non-zero, then the pixel -> user
*     Mapping is linear. The following element contains the DOUBLE PRECISION
*     equivalence of an INTEGER value representing an AST Frame pointer.
*     This is the user coordinate Frame. The following NDIM*(NDIM+1) elements
*     give the coefficients of the pixel->user linear mapping:
*
*        U1 = C0 + C1*P1 + C2*P2 + ...  + Cn*Pn
*        U2 = Cn+1 + Cn+2*P1 + Cn+3*P2 + ...  + C2n+1*Pn
*        ...
*        UN = ...
*
*
*     The B array is returned holding the value zero at all pixels outside
*     the region, and the value RINDEX at all other points. The bounds
*     of two boxes which enclose all interior and exterior points in
*     the region are returned.

*  Arguments:
*     INDEX1 = INTEGER (Given)
*        The value to use for the first keyword in the ARD description
*        (ignoring INPUT keywords). The value used to represent the
*        current region within the pixel mask is formed by adding the
*        value of INDEX1 onto an integer representing the position of
*        the keyword within the original algebraic ARD expression (i.e.
*        1 for the first keyword, 2 for the second keyword, etc), and
*        then subtracting one.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     MASK( MSKSIZ ) = INTEGER (Given)
*        The mask supplied to ARD_WORK by the calling application.
*     OPNSIZ = INTEGER (Given)
*        The size of the OPRNDS array.
*     IOPND = INTEGER (Given)
*        The index within OPRNDS of the start of the block of values
*        which describe the required region.
*     LEX0( NDIM ) = INTEGER (Given and Returned)
*        Lower bounds of exterior bounding box for MASK.
*     UEX0( NDIM ) = INTEGER (Given and Returned)
*        Upper bounds of exterior bounding box for MASK.
*     LIN0( NDIM ) = INTEGER (Given and Returned)
*        Lower bounds of interior bounding box for MASK.
*     UIN0( NDIM ) = INTEGER (Given and Returned)
*        Upper bounds of interior bounding box for MASK.
*     OPRNDS( OPNSIZ ) = DOUBLE PRECISION (Given and Returned)
*        A list of values describing each keyword field specified
*        within the ARD description. These may be changed on exit (e.g.
*        pixel co-ordinate values changed to pixel indices).
*     IPB = INTEGER (Given)
*        A point to the array to be filled.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B.
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     LOADED = LOGICAL (Given and Returned)
*        Have the contents of the supplied mask already been loaded onto
*        the stack? Always returned .TRUE. if the keyword is an INPUT
*        keyword.
*     RINDEX = INTEGER (Returned)
*        The region index used for the keyword.
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
*     26-JUN-2001 (DSB):
*        Modified to support AST.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDEX1
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER MASK( MSKSIZ )
      INTEGER OPNSIZ
      INTEGER IOPND

*  Arguments Given and Returned:
      INTEGER LEX0( NDIM )
      INTEGER UEX0( NDIM )
      INTEGER LIN0( NDIM )
      INTEGER UIN0( NDIM )
      DOUBLE PRECISION OPRNDS( OPNSIZ )
      INTEGER IPB
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )
      LOGICAL LOADED

*  Arguments Returned:
      INTEGER RINDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I,                 ! Dimension counter
     :        IAST(2),           ! AST Frame or FrameSet pointer
     :        ICOEFF,            ! Index of first linear coefficient
     :        IPAR,              ! Index of first region parameters
     :        LASTOP,            ! Index of last used operand value
     :        NPAR,              ! No. of region parameters
     :        TYPE               ! Integer code for region type

      LOGICAL
     :        LINEAR,            ! Is the pixel->user Mapping linear?
     :        INF                ! Is int. bounding box infinite?

      DOUBLE PRECISION
     :        DPP,               ! Distance per pixel
     :        DAST,              ! DOUBLE equivalence to variable IAST.
     :        PAR( 16 )          ! Array to use if fewer than 16 parameters

*  Make DAST and IAST share the same memory so that we can interpret
*  the real operand value as an integer identifier.
      EQUIVALENCE ( IAST, DAST )

*.

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the integer code which identifies the type of region required.
      TYPE = NINT( OPRNDS( IOPND ) )

*  Get the position of the keyword within the algebraic ARD expression.
*  Add on the base region index and subtract one to get the region
*  index to use for this keyword region.
      RINDEX = NINT( OPRNDS( IOPND + 1 ) ) + INDEX1 - 1

*  Save the number of parameters describing the region
      NPAR = NINT( OPRNDS( IOPND + 2 ) )

*  Save the index of the first region parameter.
      IPAR = IOPND + 3

*  See if the pixel->user mapping is linear. If not, extract the AST
*  FrameSet pointer. At the same time, note the number of region
*  parameters and the index of the first region parameter.
      IF( OPRNDS( IOPND + 3 + NPAR ) .EQ. 0.0 ) THEN
         LINEAR = .FALSE.
         DAST = OPRNDS( IOPND + 4 + NPAR )
         DPP = OPRNDS( IOPND + 5 + NPAR )
         LASTOP = IOPND + 5 + NPAR

*  If the mapping is linear, extract the AST Frame pointer (the user
*  coord Frame), and note the starting index of the coefficients of the
*  mapping.
      ELSE
         LINEAR = .TRUE.
         DAST = OPRNDS( IOPND + 4 + NPAR )
         ICOEFF = IOPND + 5 + NPAR
         LASTOP = IOPND + 4 + NPAR +  NDIM*( NDIM + 1 )
      END IF

*  Report an error if there are insufficient values in OPRNDS.
      IF( LASTOP .GT. OPNSIZ ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_LKR_ERR1', 'Operand stack exhausted '//
     :                 'while processing region type ^TYPE in '//
     :                 'ARD1_LKR (programming error).', STATUS )

         CALL MSG_SETI( 'NPAR', NPAR )
         CALL ERR_REP( 'ARD1_LKR_ERR2', '^NPAR parameters required.',
     :                  STATUS )

         CALL MSG_SETI( 'IOP', IOPND )
         CALL ERR_REP( 'ARD1_LKR_ERR3', 'Starting at index ^IOP',
     :                  STATUS )

         CALL MSG_SETI( 'OPSIZ', OPNSIZ )
         CALL ERR_REP( 'ARD1_LKR_ERR4', 'Stack size: ^OPSIZ', STATUS )

         GO TO 999

      END IF

*  WHOLE keywords...
      IF( TYPE .EQ. ARD__WHO ) THEN
         CALL ARD1_WHO( RINDEX, NDIM, MSKSIZ, %VAL( CNF_PVAL( IPB ) ),
     :                  LBEXTB,
     :                  UBEXTB, LBINTB, UBINTB, STATUS )

*  INPUT keywords...
      ELSE IF( TYPE .EQ. ARD__INP ) THEN

*  Copy the mask values and find the mask bounding boxes if they have
*  not already been found.
         CALL ARD1_LSM( NDIM, LBND, UBND, MSKSIZ, MASK, LOADED,
     :                  %VAL( CNF_PVAL( IPB ) ),
     :                  LEX0, UEX0, LIN0, UIN0, STATUS )

*  Copy the mask bounding boxes.
         DO I = 1, NDIM
            LBEXTB( I ) = LEX0( I )
            UBEXTB( I ) = UEX0( I )
            LBINTB( I ) = LIN0( I )
            UBINTB( I ) = UIN0( I )
         END DO

*  Return zero for the region index.
         RINDEX = 0

*  To speed things up, the processing of other keywords depends on whether
*  the mapping is linear or not. First deal with linear Mappings.
      ELSE IF( LINEAR ) THEN
         CALL ARD1_LNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                  OPRNDS( IPAR ), OPRNDS( ICOEFF ), IAST( 1 ),
     :                  %VAL( CNF_PVAL( IPB ) ),
     :                  LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                  STATUS )

*  Now deal with non-linear Mappings. Ensure we have room for at least
*  16 parameters.
      ELSE IF( NPAR .GE. 16 ) THEN
         CALL ARD1_NLNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                   OPRNDS( IPAR ), IAST(1), DPP, IPB,
     :                   LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

      ELSE
         DO I = 1, NPAR
            PAR( I ) = OPRNDS( IPAR + I - 1 )
         END DO
         CALL ARD1_NLNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                   PAR, IAST(1), DPP, IPB, LBEXTB, UBEXTB,
     :                   LBINTB, UBINTB, STATUS )

      END IF

*  See if the internal bounding box covers the entire mask.
      IF( LBINTB( 1 ) .EQ. VAL__MINI ) THEN
         INF = .FALSE.

      ELSE IF( LBINTB( 1 ) .EQ. VAL__MAXI ) THEN
         INF = .TRUE.

      ELSE
         INF = .TRUE.
         I = 1

         DO WHILE( INF .AND. I .LE. NDIM )
            IF( LBINTB( I ) .GT. LBND( I )  .OR.
     :          UBINTB( I ) .LT. UBND( I ) ) INF = .FALSE.
            I = I + 1
         END DO

      END IF

*  If it does, return VAL__MAXI for the lower bound on the first
*  axis.
      IF( INF ) LBINTB( 1 ) = VAL__MAXI

*  Jump to here if an error occurs.
 999  CONTINUE

      END
