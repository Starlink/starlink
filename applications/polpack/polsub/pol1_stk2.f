      SUBROUTINE POL1_STK2( NIN, SPEC, IN, XIN, YIN, ZIN, NXBIN, NYBIN,
     :                      NZBIN, MXCNT, TR, OUT, WORK, STATUS )
*+
*  Name:
*     POL1_STK2

*  Purpose:
*     Create a stack of pixel values suitable for binning.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STK2( NIN, SPEC, IN, XIN, YIN, ZIN, NXBIN, NYBIN, NZBIN,
*                     MXCNT, TR, OUT, WORK, STATUS )

*  Description:
*     This routine copies the values in the supplied array (IN) to
*     the output arrays, re-arranging them so that they can be binned
*     using POL1_CM1RR or POL1_CM3RR. The input values in each bin
*     are stored as a single column in the output arrays. The number
*     of columns in the output arrays is equal to the number of bins.

*  Arguments:
*     NIN = INTEGER (Given)
*        The number of input positions.
*     SPEC = LOGICAL (Given)
*        Is the data 3D ?
*     IN( NIN ) = REAL (Given)
*        The data value at each input position.
*     XIN( NIN ) = REAL (Given)
*        The X value at each input position.
*     YIN( NIN ) = REAL (Given)
*        The Y value at each input position.
*     ZIN( NIN ) = REAL (Given)
*        The Z value at each input position. Only accessed if SPEC is
*        .TRUE.
*     NXBIN = INTEGER (Given)
*        The number of output bins along the X axis.
*     NYBIN = INTEGER (Given)
*        The number of output bins along the Y axis.
*     NZBIN = INTEGER (Given)
*        The number of output bins along the Z axis. Should be 1 if SPEC
*        is .FALSE.
*     MXCNT = INTEGER (Given)
*        The largest number of input positions in any one cell.
*     TR( 6 ) = REAL (Given)
*        The  coefficients of the transformation from (X,Y,Z) to cell indices.
*        The X cell index for a position (X,Y) is given by
*        INT( TR( 1 ) + TR( 2 )*X ), the Y cell index is given by
*        INT( TR( 3 ) + TR( 4 )*Y ), the Z  cell index is given by
*        INT( TR( 5 ) + TR( 6 )*Z ).
*     OUT( NXBIN, NYBIN, NZBIN, MXCNT ) = INTEGER (Returned)
*        The output data values. The first 3 axes span the cell
*        indices. The input values for each output cell are stored in a
*        column parallel to the fourth axis. These columns are padded with
*        bad values.
*     WORK( NXBIN, NYBIN, NZBIN ) = INTEGER (Returned)
*        Workspace. Returned holding the number of input positions in
*        each cell.
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
*     5-FEB-2001 (DSB):
*        Modified to support 3D data.
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
      LOGICAL SPEC
      REAL IN( NIN )
      REAL XIN( NIN )
      REAL YIN( NIN )
      REAL ZIN( NIN )
      INTEGER NXBIN
      INTEGER NYBIN
      INTEGER NZBIN
      INTEGER MXCNT
      REAL TR( 6 )

*  Arguments Returned:
      REAL OUT( NXBIN, NYBIN, NZBIN, MXCNT )
      INTEGER WORK( NXBIN, NYBIN, NZBIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Input position index
      INTEGER IX                 ! Output cell X index
      INTEGER IY                 ! Output cell Y index
      INTEGER IZ                 ! Output cell Z index
      INTEGER J                  ! The index of the next free column element
      REAL X                     ! Input X value
      REAL Y                     ! Input Y value
      REAL Z                     ! Input Z value
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the work array holding the number of input values stored in
*  each output cell, to indicate that no input values have yet been stored
*  for any output cell.
      DO IZ = 1, NZBIN
         DO IY = 1, NYBIN
            DO IX = 1, NXBIN
               WORK( IX, IY, IZ ) = 0
            END DO
         END DO
      END DO

*  First do 3D data.
      IF( SPEC ) THEN

*  Go through each good input position.
         DO I = 1, NIN
            X = XIN( I )
            Y = YIN( I )
            Z = ZIN( I )
            IF( X .NE. VAL__BADR .AND.
     :          Y .NE. VAL__BADR .AND.
     :          Z .NE. VAL__BADR ) THEN

*  Get the indices of the output cell containing the input position.
               IX = INT( TR( 1 ) + TR( 2 )*X )
               IY = INT( TR( 3 ) + TR( 4 )*Y )
               IZ = INT( TR( 5 ) + TR( 6 )*Z )

*  Increment the number of input positions in this cell if it is within
*  bounds.
               IF( IX .GE. 1 .AND. IX .LE. NXBIN .AND.
     :             IY .GE. 1 .AND. IY .LE. NYBIN .AND.
     :             IZ .GE. 1 .AND. IZ .LE. NZBIN ) THEN
                  J = WORK( IX, IY, IZ ) + 1
                  WORK( IX, IY, IZ ) = J

*  Store the input data value on top of the column of values for the
*  output cell.
                  IF( J .LE. MXCNT ) OUT( IX, IY, IZ, J ) = IN( I )

               END IF

            END IF

         END DO

*  Now do 2D data.
      ELSE

*  Go through each good input position.
         DO I = 1, NIN
            X = XIN( I )
            Y = YIN( I )
            IF( X .NE. VAL__BADR .AND. Y .NE. VAL__BADR ) THEN

*  Get the indices of the output cell containing the input position.
               IX = INT( TR( 1 ) + TR( 2 )*X )
               IY = INT( TR( 3 ) + TR( 4 )*Y )

*  Increment the number of input positions in this cell if it is within
*  bounds.
               IF( IX .GE. 1 .AND. IX .LE. NXBIN .AND.
     :             IY .GE. 1 .AND. IY .LE. NYBIN ) THEN
                  J = WORK( IX, IY, 1 ) + 1
                  WORK( IX, IY, 1 ) = J

*  Store the input data value on top of the column of values for the
*  output cell.
                  IF( J .LE. MXCNT ) OUT( IX, IY, 1, J ) = IN( I )

               END IF

            END IF

         END DO

      END IF

*  Fill any unused values with BAD values.
      DO IZ = 1, NZBIN
         DO IY = 1, NYBIN
            DO IX = 1, NXBIN
               DO J = WORK( IX, IY, IZ ) + 1, MXCNT
                  OUT( IX, IY, IZ, J ) = VAL__BADR
               END DO
            END DO
         END DO
      END DO

      END
