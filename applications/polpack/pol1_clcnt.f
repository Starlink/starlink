      SUBROUTINE POL1_CLCNT( NIN, XIN, YIN, TR, NXBIN, NYBIN, WORK, 
     :                       MXCNT, STATUS )
*+
*  Name:
*     POL1_CLCNT

*  Purpose:
*     Square the supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CLCNT( NIN, XIN, YIN, TR, NXBIN, NYBIN, WORK, MXCNT, STATUS )

*  Description:
*     This routine counts the number of input positions contained in each 
*     output grid cell. The largest number in any one cell is returned.
*     An error is reported if no good input positions are supplied.

*  Arguments:
*     NIN = INTEGER (Given)
*        The number of input positions.
*     XIN( NIN ) = REAL (Given)
*        The X value at each input position.
*     YIN( NIN ) = REAL (Given)
*        The Y value at each input position.
*     TR( 4 ) = REAL (Given)
*        The  coefficients of the transformation from (X,Y) to cell indices.
*        The X cell index for a position (X,Y) is given by 
*        INT( TR( 1 ) + TR( 2 )*X ), the Y cell index is given by 
*        INT( TR( 3 ) + TR( 4 )*Y ).
*     NXBIN = INTEGER (Given)
*        The number of cells along the X axis.
*     NYBIN = INTEGER (Given)
*        The number of cells along the Y axis.
*     WORK( NXBIN, NYBIN ) = INTEGER (Returned)
*        Workspace. Returned holding the number of input positions in
*        each cell.
*     MXCNT = INTEGER (Returned)
*        The largest number of input positions in any one cell.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NIN
      REAL XIN( NIN )
      REAL YIN( NIN )
      REAL TR( 4 )
      INTEGER NXBIN
      INTEGER NYBIN

*  Arguments Returned:
      INTEGER WORK( NXBIN, NYBIN )
      INTEGER MXCNT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Input position index
      INTEGER IX                 ! Output cell X index
      INTEGER IY                 ! Output cell Y index
      REAL X                     ! Input X value
      REAL Y                     ! Input Y value
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the work array to hold zero for every cell.
      DO IY = 1, NYBIN
         DO IX = 1, NXBIN
            WORK( IX, IY ) = 0  
         END DO
      END DO

*  Go through each good input position.
      DO I = 1, NIN
         X = XIN( I )    
         Y = YIN( I )
         IF( X .NE. VAL__BADR .AND. Y .NE. VAL__BADR ) THEN

*  Get the indices of the output cell containing the input position.
            IX = INT( TR( 1 ) + TR( 2 )*X )
            IY = INT( TR( 3 ) + TR( 4 )*Y )

*  Increment the count of input positions in this cell if it is within
*  bounds.
            IF( IX .GE. 1 .AND. IX .LE. NXBIN .AND.
     :          IY .GE. 1 .AND. IY .LE. NYBIN ) THEN
               WORK( IX, IY ) = WORK( IX, IY ) + 1
            END IF

         END IF

      END DO

*  Find the largest number of input positions in any one output cell.
      MXCNT = 0
      DO IY = 1, NYBIN
         DO IX = 1, NXBIN
            MXCNT = MAX( MXCNT, WORK( IX, IY ) )
         END DO
      END DO

*  Report an error if no good input positions were supplied.
      IF( MXCNT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POL1_CLCNT_1', 'The input catalogue contains '//
     :                 'no good vector positions.', STATUS )
      END IF

      END
