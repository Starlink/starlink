

*+  MAXMIN - returns the max and min values found in a defined sub-array

      SUBROUTINE MAXMIN ( DIMS1, DIMS2, ARRAY, XSTART, YSTART, XFINISH,
     :                    YFINISH, NUMPIX, MAXIMUM, MINIMUM,
     :                    MAXPOS, MINPOS, STATUS )

*    Description :
*
*     This routine returns the maximum and minimum values found in
*     a specified subsection of an input 2-d array, where it found
*     the maxima and minima, and the number of pixels in the sub-array.
*
*    Invocation :
*
*     CALL MAXMIN( DIMS, ARRAY, XSTART, YSTART, XFINISH, YFINISH,
*                  NUMPIX, MAXIMUM, MINIMUM, MAXPOS, MINPOS, STATUS )
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     If sub-array coordinates are invalid then
*        Set all returned values to zero
*        Return
*     Endif
*     If sub-array coordinates are transposed then
*        Swap them
*     Endif
*     Work out number of pixels in sub-array
*     Initialise max and min to be equal to value of the
*       bottom left pixel of the specified subarray
*     For all rows of the sub-array
*        For all pixels in the current row
*           Compare current max and min with current pixel value,
*             swap if necessary, and keep positions updated
*        Endfor
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
*     14-04-1986 : First implementation (REVA::MJM)
*     24-11-1986 : Bug fix in max,min position updating (HILO::MJM)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2,              ! input array dimensions
     :    XSTART,                 ! x start coord of subarray
     :    YSTART,                 ! y   "     "    "     "
     :    XFINISH,                ! x finish  "    "     "
     :    YFINISH                 ! y   "     "    "     "

      REAL
     :    ARRAY( DIMS1, DIMS2 )   ! input array

*    Export :

      INTEGER
     :    NUMPIX,                 ! number of pixels in sub-array
     :    MAXPOS( 2 ),            ! where maximum is found
     :    MINPOS( 2 )             !   "   minimum  "   "

      REAL
     :    MAXIMUM,                ! maximum value found in subarray
     :    MINIMUM                 ! minimum   "     "    "     "

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    DUMMY,                  ! used in transposing sub-array coords
     :    I, J                    ! counters

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    check the sub-array coordinates for validity
      IF ( XSTART  .LT. 1 .OR. XSTART  .GT. DIMS1 .OR.
     :     YSTART  .LT. 1 .OR. YSTART  .GT. DIMS2 .OR.
     :     XFINISH .LT. 1 .OR. XFINISH .GT. DIMS1 .OR.
     :     YFINISH .LT. 1 .OR. YFINISH .GT. DIMS2 ) THEN

*       set returned values to sensible numbers and return
         NUMPIX       =  0
         MAXIMUM      =  0.0
         MINIMUM      =  0.0
         MAXPOS( 1 )  =  0
         MAXPOS( 2 )  =  0
         MINPOS( 1 )  =  0
         MINPOS( 2 )  =  0

         RETURN

      END IF

*    check sub-array x coordinates for order
      IF ( XSTART .GT. XFINISH ) THEN
         DUMMY    =  XSTART
         XSTART   =  XFINISH
         XFINISH  =  DUMMY
      END IF

*    check sub-array y coordinates for order
      IF ( YSTART .GT. YFINISH ) THEN
         DUMMY    =  YSTART
         YSTART   =  YFINISH
         YFINISH  =  DUMMY
      END IF

*    work out number of pixels in sub-array
      NUMPIX  =  ( XFINISH - XSTART + 1 ) * ( YFINISH - YSTART + 1 )

*    initialise max and min variables to be equal to the value of
*    the lower left corner of the specified subarray, and the
*    positions of each to be there
      MAXIMUM      =  ARRAY( XSTART, YSTART )
      MINIMUM      =  MAXIMUM
      MAXPOS( 1 )  =  XSTART
      MAXPOS( 2 )  =  YSTART
      MINPOS( 1 )  =  XSTART
      MINPOS( 2 )  =  YSTART

*    loop round all the rows of the specified subarray
      DO  J  =  YSTART, YFINISH

*       loop round all the pixels in the current row
         DO  I  =  XSTART, XFINISH

*          check current maximum against current pixel value
            IF ( ARRAY( I, J ) .GT. MAXIMUM ) THEN
               MAXIMUM      =  ARRAY( I, J )
               MAXPOS( 1 )  =  I
               MAXPOS( 2 )  =  J
            END IF

*          check current minimum against current pixel value
            IF ( ARRAY( I, J ) .LT. MINIMUM ) THEN
               MINIMUM      =  ARRAY( I, J )
               MINPOS( 1 )  =  I
               MINPOS( 2 )  =  J
            END IF

*        end of loop round pixels in current row
          END DO

*    end of loop round specified rows
      END DO


*    return and end
      END
