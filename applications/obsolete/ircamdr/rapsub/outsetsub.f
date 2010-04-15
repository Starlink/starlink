
*+  OUTSETSUB - replaces all pixels outside circle with specified value

      SUBROUTINE OUTSETSUB ( INARRAY, OUTARRAY, DIMS1, DIMS2, XCENTRE,
     :                       YCENTRE, DIAMETER, NEWVAL, STATUS )

*    Description :
*
*     This routine copies the data from one array to another except
*     outside the bounds of a specified circle, where a given fixed
*     value is inserted as a replacement.
*
*    Invocation :
*
*     CALL OUTSETSUB( INARRAY, OUTARRAY, DIMS, XCENTRE, YCENTRE,
*                     DIAMETER, NEWVAL, STATUS )
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Work out radius of circle from given diameter
*     For all pixels of output array
*        Work out distance of current pixel from centre of circle
*        If distance is greater than radius of circle then
*           Outarray pixel value  =  Replacement value
*        Else
*           Outarray pixel value  =  Inarray pixel value
*        Endif
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     17-09-1985 : First implementation (REVA::MJM)
*     03-07-1986 : Revised implementation (REVA::MJM)
*     15-Aug-1994  Changed DIM arguments so routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE        ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'    ! global SSE parameters

*    Import :

      INTEGER
     :    DIMS1, DIMS2,      ! dimensions of input array
     :    XCENTRE,        ! x coord of circle centre
     :    YCENTRE         ! y coord of circle centre

      REAL
     :    INARRAY( DIMS1, DIMS2 ),   ! input data
     :    DIAMETER,       ! diameter of circle to be used
     :    NEWVAL          ! replacement value

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )   ! output data

*    Status :

      INTEGER  STATUS      ! global status parameter

*    Local variables :

      REAL
     :    RADIUS,          ! radius of circle to be used
     :    DISTANCE         ! distance from circle centre

      INTEGER
     :    I, J             ! counters

*-
*    check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    set up radius
      RADIUS  =  DIAMETER / 2.0

*    loop round all rows in array
      DO  J  =  1, DIMS2

*       loop round all pixels in current row
         DO  I  =  1, DIMS1

*          see where current pixel is in relation to centre of circle
            DISTANCE  =  SQRT(REAL((I-XCENTRE)**2+(J-YCENTRE)**2))

*          check this against given radius value
            IF( DISTANCE .GT. RADIUS ) THEN

*             pixel lies outside circle - output array pixel value is
*             equal to the given replacement value
               OUTARRAY( I, J )  =  NEWVAL

            ELSE

*             pixel lies within circle - output array pixel value is
*             equal to input array pixel value
               OUTARRAY( I, J )  =  INARRAY( I, J )

*          end of if-pixel-lies-outside-of-circle check
            ENDIF

*       end of loop round all pixels of current row
         END DO

*    end of loop round all rows of output array
      END DO


*    end and return
      END
