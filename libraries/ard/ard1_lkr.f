      SUBROUTINE ARD1_LKR( INDEX1, NDIM, LBND, UBND, MSKSIZ, MASK,
     :                     OPNSIZ, IOPND, LEX0, UEX0, LIN0, UIN0, 
     :                     OPRNDS, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
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
*                    IOPND,LEX0, UEX0, LIN0, UIN0, OPRNDS, B, LBEXTB,
*                    UBEXTB, LBINTB, UBINTB, LOADED, RINDEX, STATUS )

*  Description:
*     The supplied value of IOPND is a pointer into the OPRNDS array.
*     The identified element gives the integer code corresponding to
*     the type of region (CIRCLE, BOX, etc) required. The next element
*     contains an integer which gives the position of the keyword
*     within the ARD description (1 for the first keyword, 2 for the
*     second, etc, excluding INPUT keywords). The next element contains
*     an integer which gives the number of parameters following. These
*     parameters described the size, position, etc, of the region. The
*     B array is returned holding the value zero at all pixels outside
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
*     OPRNDS( OPNSIZ ) = REAL (Given and Returned)
*        A list of values describing each keyword field specified
*        within the ARD description. These may be changed on exit (e.g.
*        pixel co-ordinate values changed to pixel indices).
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The returned array.
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

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1994 (DSB):
*        Original version.
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
      REAL OPRNDS( OPNSIZ )
      INTEGER B( MSKSIZ )
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
     :        NPAR,              ! No. of parameters describing region
     :        TYPE               ! Integer code for region type

      LOGICAL
     :        BADPAR,            ! Are the parameters missing?
     :        INF                ! Is int. bounding box infinite?

*.

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the integer code which identifies the type of region required.
      TYPE = NINT( OPRNDS( IOPND ) )

*  Get the position of the keyword within the algebraic ARD expression.
*  Add on the base region index and subtract one to get the region
*  index to use for this keyword region.
      RINDEX = NINT( OPRNDS( IOPND + 1 ) ) + INDEX1 - 1

*  Get the number of parameters associated with the region.
      NPAR = NINT( OPRNDS( IOPND + 2 ) )

*  Report an error if there are insufficient values in OPRNDS.
      IF( IOPND + 2 + NPAR .GT. OPNSIZ ) THEN
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

*  Initialise a flag to indicate that the correct number of parameters
*  are available.
      BADPAR = .FALSE.

*  Call a separate suboutine to handle each type of region.
*  POINT and PIXEL keywords...
      IF( TYPE .EQ. ARD__POI .OR. TYPE .EQ. ARD__PIX ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_POI( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     OPRNDS( IOPND + 3 ), B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  LINE keywords...
      ELSE IF( TYPE .EQ. ARD__LIN ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_LIN( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     OPRNDS( IOPND + 3 ), B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  ROW and COLUMN keywords...
      ELSE IF( TYPE .EQ. ARD__ROW .OR. TYPE .EQ. ARD__COL ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_RWCL( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                      OPRNDS( IOPND + 3 ), B, LBEXTB, UBEXTB,
     :                      LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  BOX keywords...
      ELSE IF( TYPE .EQ. ARD__BOX ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_BOX( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     OPRNDS( IOPND + 3 ), B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  RECT keywords...
      ELSE IF( TYPE .EQ. ARD__REC ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_REC( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     OPRNDS( IOPND + 3 ), B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  POLYGON and ROTBOX keywords.
      ELSE IF( TYPE .EQ. ARD__POL .OR. TYPE .EQ. ARD__ROT ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_POL( RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), NPAR, OPRNDS( IOPND + 3 ), B,
     :                     LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  CIRCLE keywords...
      ELSE IF( TYPE .EQ. ARD__CIR ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_CIR( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     OPRNDS( IOPND + 3 ), B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  ELLIPSE keywords...
      ELSE IF( TYPE .EQ. ARD__ELL ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_ELL( RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), NPAR, OPRNDS( IOPND + 3 ), B,
     :                     LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  FRAME keywords...
      ELSE IF( TYPE .EQ. ARD__FRA ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_FRA( RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), OPRNDS( IOPND + 3 ), B, LBEXTB,
     :                     UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  WHOLE keywords...
      ELSE IF( TYPE .EQ. ARD__WHO ) THEN
         CALL ARD1_WHO( RINDEX, NDIM, MSKSIZ, B, LBEXTB, UBEXTB, LBINTB,
     :                  UBINTB, STATUS )

*  INPUT keywords...
      ELSE IF( TYPE .EQ. ARD__INP ) THEN

*  Copy the mask values and find the mask bounding boxes if they have
*  not already been found.
         CALL ARD1_LSM( NDIM, LBND, UBND, MSKSIZ, MASK, LOADED, B,
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

*  Report an error and abort for any other keyword.
      ELSE
         STATUS = ARD__INTER
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_LKR_ERR5', 'Illegal keyword identifier '//
     :                 ' (^TYPE) encountered in routine ARD1_LKR '//
     :                 '(programming error).', STATUS )
      END IF

*  Report an error if no parameters have been supplied.
      IF( BADPAR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'TY', TYPE )
         CALL ERR_REP( 'ARD1_LKR_ERR6', 'Zero parameters found for '//
     :                 'region type ^TY in ARD1_LKR (programming '//
     :                 'error).', STATUS )
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
