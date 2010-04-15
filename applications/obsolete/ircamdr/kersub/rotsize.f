
*+  ROTSIZE - works out size of output array for ROTATE

      SUBROUTINE ROTSIZE ( IDIMS1, IDIMS2, ANGLE, ODIMS1, ODIMS2,
     :                     STATUS )

*    Description :
*
*     This routine works out the dimensions of the output array required
*     by ROTATE when a non-right angle rotation has been requested. Input
*     are the input image dimensions and the rotation angle in clockwise
*     degrees , and returned are the dimensions of the output array that
*     is to be used to hold the rotated image
*
*    Invocation :
*
*     CALL ROTSIZE ( IDIMS, ANGLE, ODIMS, STATUS )
*
*    Method :
*
*     Just calls NEWCOORDS for the each of the array corners, which
*     returns the x and y distances of the transformed extremities
*     from the array centre. Taking maxima and minima of these values
*     and adding them finds the size of output array required to hold
*     the rotated input array dimensions.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Konrad Flamm UoE (REVA::MJM)
*
*    History :
*
*     14-11-1985 :  First implementation
*                :  (REVA::MJM)
*     17-July-1994  Converted angles to radians to avoid VAX-specific
*                   trig functions, changed arguments to input DIMS
*                   separately so that routine will still compile (SKL@JACH)
*
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIMS1,             ! dimensions of image to be rotated
     :    IDIMS2             ! dimensions of image to be rotated

      REAL
     :    ANGLE                  ! rotation angle in degrees

*    Export :

      INTEGER
     :    ODIMS1,             ! dimensions of output array to hold
     :                           ! rotated imag
     :    ODIMS2              ! dimensions of output array to hold
     :                            ! rotated image

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      REAL
     :    CENTREX,                ! x coordinate of array centre
     :    CENTREY,                ! y      "      "   "     "
     :    CX( 4 ),                ! x distances of input array corners
                                  ! from array centre
     :    CY( 4 ),                ! y distances of input array corners
                                  ! from array centre
     :    CXP( 4 ),               ! x distances of transformed corners
                                  ! from array centre
     :    CYP( 4 ),               ! y distances of transformed corners
                                  ! from array centres
     :    MAXX,                   ! maximum transformed x coordinate
     :    MAXY,                   !    "         "      y     "
     :    MINX,                   ! minimum      "      x     "
     :    MINY,                   !    "         "      y     "
     :    CANGLE,                 ! cosine of the rotation angle
     :    SANGLE,                 ! sine of the rotation angle
     :    DTOR                    ! factor for converting degrees to radians

      PARAMETER ( DTOR = 3.141592 / 180.0 )

      INTEGER
     :    IMAXX,                  ! rounded up integer version of MAXX
     :    IMAXY,                  !    "     "    "       "     " MAXY
     :    IMINX,                  !    "     "    "       "     " MINX
     :    IMINY,                  !    "     "    "       "     " MINY
     :    I                       ! counter

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    set up the sine and cosine of the rotation angle
*    first convert degrees to radians
      ANGLE = ANGLE * DTOR
*
      CANGLE  =  COS( ANGLE )
      SANGLE  =  SIN( ANGLE )

*    initialise the arrays which contain the the coordinates of the
*    corners of the input image - we work in terms of distance from
*    the array centre throughout

      CENTREX  =  REAL( IDIMS1 ) / 2.0
      CENTREY  =  REAL( IDIMS2 ) / 2.0

      CX( 1 )  =  - CENTREX         ! bottom left corner
      CY( 1 )  =  - CENTREY
      CX( 2 )  =  - CENTREX         ! top left corner
      CY( 2 )  =  + CENTREY
      CX( 3 )  =  + CENTREX         ! top right corner
      CY( 3 )  =  + CENTREY
      CX( 4 )  =  + CENTREX         ! bottom right corner
      CY( 4 )  =  - CENTREY

*    now transform each of these corners into the rotated frame

      DO  I  =  1, 4
         CALL NEWCOORDS( CX( I ), CY( I ), CANGLE, SANGLE, CXP( I ),
     :                   CYP( I ), STATUS )
      END DO

*    get the maximum and minimum distances from the array centre

      MAXX  =  MAX( CXP( 1 ), CXP( 2 ), CXP( 3 ), CXP( 4 ) )
      MAXY  =  MAX( CYP( 1 ), CYP( 2 ), CYP( 3 ), CYP( 4 ) )
      MINX  =  MIN( CXP( 1 ), CXP( 2 ), CXP( 3 ), CXP( 4 ) )
      MINY  =  MIN( CYP( 1 ), CYP( 2 ), CYP( 3 ), CYP( 4 ) )

*    round up the maxima and minima to the nearest integer such
*    that the output array is both big enough and symmetric about
*    the array centre

      IMAXX  =  NINT( MAXX + 0.5 )
      IMAXY  =  NINT( MAXY + 0.5 )
      IMINX  =  NINT( MINX - 0.5 )
      IMINY  =  NINT( MINY - 0.5 )

*    calculate the output array dimensions from these values

      ODIMS1   =  IMAXX -  IMINX
      ODIMS2   =  IMAXY -  IMINY

*    that's it

      END
