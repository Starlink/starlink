
*+  TRANDATSUB - converts an x,y,i array of data into an image array

      SUBROUTINE TRANDATSUB ( OUTARRAY, ODIMS1, ODIMS2, INDATA,
     :         MAXREC, NPTS, XPOS, YPOS, INTENSPOS, PSCALE, XMIN,
     :         YMIN, STATUS )

*    Description :
*
*     This routine is called from TRANDAT, and maps the data points
*     read from a free-format file into INDATA onto pixels in the
*     2-d output array OUTARRAY. The data are stored in INDATA as
*     x and y coordinates and intensity values.
*
*    Invocation :
*
*     CALL TRANDATSUB( OUTARRAY, ODIMS, INDATA, MAXREC, NPTS, XPOS,
*                      YPOS, INTENSPOS, PSCALE, XMIN, YMIN, STATUS )
*
*    Method :
*
*     For all rows in output image
*        For all pixels in current row
*           Set output pixel to zero
*        Endfor
*     Endfor
*     For all input data points
*        Work out position of pixel in output image to hold current data
*         point from x,y coordinates and pixel scale
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     21-10-1985 :  First SSE/Adam implementation from the original
*                :  free-format -> .BDF file program written by
*                :  REVAD::CAA,MJM for the interim environment.
*                :  (REVA::MJM)
*     30-05-1987 :  Bug fix in argument passing from TRANDAT (REVS::MJM)
*     15-AUG-1994   Changed DIM arguments so routine will compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :     ODIMS1, ODIMS2,        ! output array dimensions
     :     NPTS,                  ! number of input data points
     :     XPOS,                  ! position of x coords in INDATA
     :     YPOS,                  !     "     " y    "    "    "
     :     INTENSPOS,             !     "       intensity values
     :     MAXREC                 ! maximum possible position index in INDATA

      REAL
     :     INDATA( MAXREC, NPTS ),  ! input data array
     :     PSCALE,                ! pixel-to-pixel distance in x,y units
     :     XMIN,                  ! minimum x coordinate value
     :     YMIN                   !    "    y     "        "

*    Export :

      REAL
     :     OUTARRAY( ODIMS1, ODIMS2 )   ! output image

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :     XCOORD,                ! x coordinate of output array pixel
     :     YCOORD,                ! y      "      "    "     "     "
     :     I, J, K                ! counters

      REAL
     :     CURRX,                 ! current x value
     :     CURRY,                 !    "    y   "
     :     CURRI                  !    "    intensity value

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    start by zeroing the output array elements
      DO  J  =  1, ODIMS2
         DO  I  =  1, ODIMS1
            OUTARRAY( I, J )  =  0.0
         END DO
      END DO

*    loop round all the points in INDATA, getting out the x,y,i
*    values for each one
      DO  K  =  1, NPTS

         CURRX  =  INDATA( XPOS, K )
         CURRY  =  INDATA( YPOS, K )
         CURRI  =  INDATA( INTENSPOS, K )

*       calculate the x,y coordinate of the current data point
         XCOORD  =  NINT( ( ( CURRX - XMIN ) / PSCALE ) + 1.0 )
         YCOORD  =  NINT( ( ( CURRY - YMIN ) / PSCALE ) + 1.0 )

*       now set requisite output array pixel to the correct value
         OUTARRAY( XCOORD, YCOORD )  =  CURRI

*    end of loop round all input data points
      END DO


*    return and end
      END
