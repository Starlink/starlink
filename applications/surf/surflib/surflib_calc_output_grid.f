      SUBROUTINE SURFLIB_CALC_OUTPUT_GRID (N_FILES, N_PTS, PIXEL_SZ,
     :     X_PTR, Y_PTR, NX, NY, I_CENTRE, J_CENTRE, 
     :     STATUS )

*+
*  Name:
*     SURFLIB_CALC_OUTPUT_GRID

*  Purpose:
*     Calculate the grid required to contain data given the X,Y positions

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*      SUBROUTINE SURFLIB_CALC_OUTPUT_GRID (N_FILES, N_PTS, PIXEL_SZ,
*     :     X_PTR, Y_PTR, NX, NY, I_CENTRE, J_CENTRE, 
*     :     STATUS )

*  Description:
*     Finds the extent of the data (given X and Y) and then
*     reports back the size of the grid that is necessary to contain
*     the data. Also returns the reference pixel position.

*  Arguments:
*     N_FILES = INTEGER (Given)
*       Number of data sets (ie files)
*     N_PTS = INTEGER (Given)
*       Number of X,Y pairs supplied
*     PIXEL_SZ = REAL (Given)
*       Size of each pixel (radians)
*     X_PTR( N_FILES ) = INTEGER (Given)
*       Pointers to arrays containing DOUBLE PRECISION X positions
*     Y_PTR( N_FILES ) = INTEGER (Given)
*       Pointers to arrays containing DOUBLE PRECISION Y positions
*     NX = INTEGER (Given)
*       Required number of points in X in output grid
*     NY = INTEGER (Given)
*       Required number of points in Y in output grid
*     I_CENTRE = INTEGER (Given)
*       Location of reference pixel in X
*     J_CENTRE = INTEGER (Given)
*       Location of reference pixel in Y
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)
*     John Lightfoot (jfl@roe.ac.uk)

*  History:
*     Original version: Timj, 1997 Oct 20 - taken from SURF_REBIN.F
*     $Log$
*     Revision 1.3  1999/07/29 21:53:30  timj
*     Add status checking for sculib_ranged.
*     Parametrize the max size of grid.
*
*     Revision 1.2  1998/02/03 00:11:39  timj
*     Change upper limit of map size to 4096 pixels.
*
*     Revision 1.1  1997/10/21 03:12:32  timj
*     Initial revision
*

*  Notes:
*     - This routine requires double precision arguments for the X and
*       Y coordinates.

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      INTEGER N_FILES
      INTEGER N_PTS (N_FILES)
      INTEGER X_PTR (N_FILES)
      INTEGER Y_PTR (N_FILES)
      REAL    PIXEL_SZ

*  Arguments Returned:
      INTEGER NX
      INTEGER NY
      INTEGER I_CENTRE
      INTEGER J_CENTRE

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_PIX             ! Maximum number of pixels allowed for grid
      PARAMETER ( MAX_PIX = 4096 )

*  Local Variables:
      DOUBLE PRECISION DTEMP      ! Scratch double
      DOUBLE PRECISION DTEMP1     ! Scratch double
      INTEGER          I          ! Loop variable
      DOUBLE PRECISION XMAX       ! Maximum X value
      DOUBLE PRECISION XMIN       ! Minimum X value
      DOUBLE PRECISION YMAX       ! Maximum Y value
      DOUBLE PRECISION YMIN       ! Minumum Y value


*.
      IF (STATUS .NE. SAI__OK) RETURN

      XMAX = VAL__MIND
      XMIN = VAL__MAXD
      YMIN = VAL__MAXD
      YMAX = VAL__MIND

*     Find range of data
      
      CALL SCULIB_RANGED (%val(X_PTR(1)), 1,
     :     N_PTS(1), XMAX, XMIN, STATUS)
      CALL SCULIB_RANGED (%val(Y_PTR(1)), 1,
     :     N_PTS(1), YMAX, YMIN, STATUS)
         
      IF (N_FILES .GT. 1) THEN
         DO I = 1, N_FILES
            CALL SCULIB_RANGED (%val(X_PTR(I)), 1,
     :           N_PTS(I), DTEMP, DTEMP1, STATUS)
            XMAX = MAX (XMAX,DTEMP)
            XMIN = MIN (XMIN,DTEMP1)
            CALL SCULIB_RANGED (%val(Y_PTR(I)), 1,
     :           N_PTS(I), DTEMP, DTEMP1, STATUS)
            YMAX = MAX (YMAX,DTEMP)
            YMIN = MIN (YMIN,DTEMP1)
         END DO
      END IF

*     calculate the size of the output array and the position of the centre
*     pixel. X increases to the left and lower pixel x index. The array is
*     slightly oversized to allow edge effects to be countered during the
*     convolution.

      IF (PIXEL_SZ .GT. 0.0 .AND. STATUS .EQ. SAI__OK) THEN
         NX = NINT (REAL(XMAX - XMIN) / PIXEL_SZ) + 10
         NY = NINT (REAL(YMAX - YMIN) / PIXEL_SZ) + 10
         I_CENTRE = NINT (REAL(XMAX) / PIXEL_SZ) + 5
         J_CENTRE = NINT (REAL(-YMIN) / PIXEL_SZ) + 5
      END IF

      IF ((NX .GT. MAX_PIX) .OR. (NY .GT. MAX_PIX)) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI('MX', MAX_PIX)
            CALL MSG_SETI('NX', NX)
            CALL MSG_SETI('NY', NY)
            CALL ERR_REP (' ', 'CALC_OUTPUT_GRID: output map is too '//
     :           'big, having one or both dimensions greater '//
     :           'than ^MX pixels (size is ^NX x ^NY)', STATUS)
         END IF
      END IF

      END
