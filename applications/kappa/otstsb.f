*+  OTSTSB - replaces all pixels outside circle with specified value

      SUBROUTINE OTSTSB ( INARR, DIM1, DIM2, XCENTR, YCENTR, DIAMTR,
     :                    NEWVAL, OUTARR, STATUS )
*
*    Description :
*
*     This routine copies the data from one array to another except
*     outside the bounds of a specified circle, where a given fixed
*     value is inserted as a replacement.
*
*    Invocation :
*
*     CALL OTSTSB( INARR, DIM1, DIM2, XCENTR, YCENTR, DIAMTR, NEWVAL,
*                  OUTARR, STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 ) = REAL( READ )
*         The input data array
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
*     XCENTR = REAL( READ )
*         The x co-ordinate of the circle centre
*     YCENTR = REAL( READ )
*         The y co-ordinate of the circle centre
*     DIAMTR = REAL( READ )
*         Diameter of the circle to be used, in pixels 
*     NEWVAL = REAL( READ )
*         Replacement value
*     ARRAY( DIM1, DIM2 ) = REAL( WRITE )
*         The output processed 2-d array
*     STATUS  =  INTEGER( READ )
*         Global status value
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
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     17-09-1985 : First implementation (REVA::MJM)
*     03-07-1986 : Revised implementation (REVA::MJM)
*     1986 Aug 15: Renamed from OUTSETSUB, reordered arguments (2nd to
*                  7th), completed prologue and nearly conformed to
*                  Starlink programming standards (RL.STAR::CUR).
*     1986 Sep  4: Renamed parameters section in prologue to arguments
*                  (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1990 Oct  9: Made the XCENTR and YCENTR truly co-ordinates rather
*                  than pixel indices (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT  NONE        ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'    ! global SSE parameters

*    Import :

      INTEGER
     :    DIM1, DIM2

      REAL
     :    INARR( DIM1, DIM2 ),
     :    DIAMTR,
     :    NEWVAL,
     :    XCENTR,
     :    YCENTR

*    Export :

      REAL 
     :    OUTARR( DIM1, DIM2 )

*    Status :

      INTEGER  STATUS

*    Local variables :

      REAL
     :    RADIUS,              ! Radius of circle to be used
     :    DISTNC               ! Distance from circle centre

      INTEGER
     :    I, J                 ! Counters

*-
*    check status on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    set up radius

      RADIUS  =  0.5 * DIAMTR

*    loop round all lines in array

      DO  J  =  1, DIM2

*       loop round all pixels in current line

         DO  I  =  1, DIM1

*          see where current pixel is in relation to centre of circle

            DISTNC  =  SQRT( ( REAL( I ) - 0.5 - XCENTR )**2 +
     :                       ( REAL( J ) - 0.5 - YCENTR )**2 )

*          check this against given radius value 

            IF ( DISTNC .GT. RADIUS ) THEN

*             pixel lies outside circle - output array pixel value is
*             equal to the given replacement value

               OUTARR( I, J )  =  NEWVAL
            ELSE

*             pixel lies within circle - output array pixel value is
*             equal to input array pixel value

               OUTARR( I, J )  =  INARR( I, J )

*          end of if-pixel-lies-outside-of-circle check

            END IF

*       end of loop round all pixels of current line

         END DO

*    end of loop round all lines of output array

      END DO

 999  CONTINUE

*    end and return

      END

