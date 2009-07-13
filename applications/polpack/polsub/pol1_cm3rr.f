      SUBROUTINE POL1_CM3RR( STACK, NPIX, NLINES, VARS, METH, MINPIX,
     :                       NSIGMA, RESULT, WRK1, WRK2, NCON, POINT, 
     :                       USED, STATUS )
*+
*  Name:
*     POL1_CM3RR

*  Purpose:
*     To combine a stack of array lines into one line, using a variety
*     of methods. REAL version.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CM3RR( STACK, NPIX, NLINES, VARS, METH, MINPIX,
*                      NSIGMA, RESULT, WRK1, WRK2, NCON, POINT, 
*                      USED, STATUS )

*  Description:
*     The routine works along each line of the input stack of lines,
*     combining the data. This variant uses a single variance for each
*     line and and does NOT propagate it.  All work is done in single
*     precision when possible and double precision when not.  Note that
*     the output array is in the processing precision.  The array NCON
*     holds the actual numbers of pixels which were used in deriving
*     the output value plus any values already present in the array -
*     thus a cumilative sum of contributing pixel numbers may be kept.

*  Arguments:
*     STACK( NPIX, NLINES ) = <COMM> (Given)
*        The array of lines which are to be combined into a single line.
*     NPIX = INTEGER (Given)
*        The number of pixels in a line of data.
*     NLINES = INTEGER (Given)
*        The number of lines of data in the stack.
*     VARS( NLINES ) = REAL (Given)
*        The variance to to used for each line of data.
*     METH = CHARACTER * ( * ) (Given)
*        The method to use in combining the lines. One of 'MEAN', 'MEDIAN',
*        or 'SIGMA'.
*     MINPIX = INTEGER (Given)
*        The minimum number of pixels required to contribute to an
*        output pixel.
*     NSIGMA = REAL (Given)
*        The number of sigmas to clip the data at (METH = SIGMA ).
*     RESULT( NPIX ) = REAL (Returned)
*        The output line of data.
*     WRK1( NLINES ) = REAL (Given and Returned)
*        Workspace for calculations.
*     WRK2( NLINES ) = REAL (Given and Returned)
*        Workspace for calculations.
*     NCON( NLINES ) = DOUBLE PRECISION (Returned)
*        The actual number of contributing pixels from each input line
*        to the output line.
*     POINT( NLINES ) = INTEGER (Given and Returned)
*        Workspace to hold pointers to the original positions of the
*        data before extraction and conversion in to the WRK1 array.
*     USED( NLINES ) =LOGICAL (Given and Returned)
*        Workspace used to indicate which values have been used in
*        estimating a resultant value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - No propagation of variances is performed using this routine.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.
 
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1992 (PDRAPER):
*        Original version with no variance propagation. Revamping of
*        routine structure to increase efficiency.
*     21-JAN-1998 (DSB):
*        Copied from CCDPACK and modified to provide fewer stacking methods.
*     13-JUL-2009 (DSB):
*        Argument VARS changed from DOUBLE PRECISION to REAL. Use KAPLIBS
*        CCG_ routines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLINES
      CHARACTER * ( * ) METH
      INTEGER MINPIX
      REAL STACK( NPIX, NLINES )
      REAL VARS( NLINES )
      REAL NSIGMA

*  Arguments Given and Returned:
      INTEGER POINT( NLINES )
      LOGICAL USED( NLINES )
      REAL WRK1( NLINES )
      REAL WRK2( NLINES )

*  Arguments Returned:
      DOUBLE PRECISION NCON( NLINES )
      REAL RESULT( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables: 
      INTEGER NBAD
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Branch for each method.

*  Weighted mean...
      IF ( METH .EQ. 'MEAN' ) THEN
         CALL CCG_ME3R( NPIX, NLINES, STACK, VARS, MINPIX,
     :                  RESULT, NCON, NBAD, STATUS )

*  Weighted median...
      ELSE IF ( METH .EQ. 'MEDIAN' ) THEN
         CALL CCG_MD3R( NPIX, NLINES, STACK, VARS, MINPIX,
     :                  RESULT, WRK1, WRK2, POINT, USED,
     :                  NCON, NBAD, STATUS )

*  Sigma clipped mean...
      ELSE IF ( METH .EQ. 'SIGMA' ) THEN
         CALL CCG_SC3R( NSIGMA, NPIX, NLINES, STACK, VARS, MINPIX,
     :                  RESULT, WRK1, WRK2, POINT, USED, NCON, NBAD, 
     :                  STATUS )

*  Report an error if the method is not recognised.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'M', METH )
         CALL ERR_REP( 'BAD_METH', 'Bad method ''^M'' specified '//
     :                 'for image combination ( invalid or not '//
     :                 'implemented )', STATUS )
      END IF

      END
