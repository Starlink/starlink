*+  SLC2T3 - puts a 2-d sub-array into a defined slice of a 3-d array

      SUBROUTINE SLC2T3 ( INARR, IDIM1, IDIM2, XSTART, YSTART, XSIZE,
     :                    YSIZE, WKDIM1, WKDIM2, WKDIM3, ZINDEX, WKARR,
     :                    STATUS )
*
*    Description :
*
*     This routine takes a defined sub-array of an input 2-d array and
*     copies it into a defined slice of a 3-d array.
*
*    Invocation :
*
*     CALL SLC2T3( INARR, IDIM1, IDIM2, XSTART, YSTART, XSIZE, YSIZE,
*    :             WKDIM1, WKDIM2, WKDIM3, ZINDEX, WKARR, STATUS )
*
*    Arguments :
*
*     INARR( IDIM1, IDIM2 )  =  REAL( READ )
*         Input data array
*     IDIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XSTART  =  INTEGER( READ )
*         x start co-ord of sub-array to be taken
*     YSTART  =  INTEGER( READ )
*         y start co-ord of sub-array to be taken
*     XSIZE  =  INTEGER( READ )
*         x size of sub-array to be taken
*     YSIZE  =  INTEGER( READ )
*         y size of sub-array to be taken
*     WKDIM1 = INTEGER( READ )
*         The first dimension of the 3-d array.
*     WKDIM2 = INTEGER( READ )
*         The second dimension of the 3-d array.
*     WKDIM3  =  INTEGER( READ )
*         The third dimension of the 3-d array.
*     ZINDEX  =  INTEGER( READ )
*         z index for position of x,y 2-d slice in 3-d array
*     WKARR( WKDIM1, WKDIM2, WKDIM3 )  =  REAL( UPDATE )
*         3-d array to be copied into
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry 
*     If o.k. then
*        For all chosen lines in 2-d input array
*           For all chosen pixels in current line
*              Copy current pixel value at x,y in 2-d array into pixel
*               x,y,ZINDEX in 3-d array
*           Endfor
*        Endfor
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     18-06-1986 : First implementation (REVA::MJM)
*     1986 Aug 16: Renamed from SLICE2TO3, reordered arguments (7th to
*                  9th), nearly conformed to Starlink programming
*                  standards (RL.STAR::CUR).
*     1986 Sep  5: Renamed parameters section in prologue to arguments
*                  (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIM1, IDIM2,
     :    WKDIM1, WKDIM2, WKDIM3,
     :    XSTART,
     :    YSTART,
     :    XSIZE,
     :    YSIZE,
     :    ZINDEX

      REAL
     :    INARR( IDIM1, IDIM2 )

*    Import-Export :

      REAL
     :    WKARR( WKDIM1, WKDIM2, WKDIM3 )

*    Status :

      INTEGER  STATUS

*    Local variables :

      INTEGER
     :    I, J, II, JJ            ! counters

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       loop round the lines of the 2-d sub-array first

         DO  J  =  1, YSIZE

*       now loop round the chosen pixels in the current line

            DO  I  =  1, XSIZE

*          work out where we are in 2-d array from position in 3-d array

               II  =  I + XSTART - 1
               JJ  =  J + YSTART - 1

*          copy the current pixel into the right place in the 3-d array

               WKARR( I, J, ZINDEX )  =  INARR( II, JJ )

*       end of loop round pixels in current line

            END DO

*    end of loop round chosen lines

         END DO

      END IF

*    return and end

      END
