*+  STAT3D - outputs the z axis statistical parameters for all pixels
*            in the x,y plane

      SUBROUTINE STAT3D ( WKARR, WKDIM1, WKDIM2, WKDIM3, GDDATA, ZNUMBR,
     :                    MEANAR, STDVAR, GODNUM, STATUS )
*
*    Description :
*
*     This routine takes as input a 3-d array with dimensions x,y,z.
*     Returned are two 2-d arrays, dimensions x,y, one of which holds
*     the mean value for each pixel in the x,y plane, averaged along
*     the z axis. The other returned array contains the standard
*     deviation from that mean for each pixel in the x,y plane. Each
*     x,y plane in the z axis direction of the 3-d array may or may
*     not have data in it, and this is indicated by the values found
*     in an input logical array. The final number of x,y planes
*     involved in the statistics is also returned.
*
*    Invocation :
*
*     CALL STAT3D( WKARR, WKDIM1, WKDIM2, WKDIM3, GDDATA, ZNUMBR,
*    :             MEANAR, STDVAR, GODNUM, STATUS )
*
*    Arguments :
*
*     WKARR( WKDIM1, WKDIM2, WKDIM3 )  =  REAL( READ )
*         3-d array containing data to be statistically analysed
*     WKDIM1 = INTEGER( READ )
*         The first dimension of the 2-d and 3-d arrays.
*     WKDIM2 = INTEGER( READ )
*         The second dimension of the 2-d and 3-d arrays.
*     WKDIM3 = INTEGER( READ )
*         The third dimension of the 3-d array and the mask.
*     GDDATA( WKDIM3 )  =  LOGICAL( READ )
*         Logical mask to indicate if z slice contains valid data or not
*     ZNUMBR  =  INTEGER( READ )
*         Number of x,y planes along z axis to be looked at
*     MEANAR( WKDIM1, WKDIM2 )  =  REAL( WRITE )
*         Array to hold means along z axis for each pixel in x,y plane
*     STDVAR( WKDIM1, WKDIM2 )  =  REAL( WRITE )
*         Same for standard deviations
*     GODNUM  =  INTEGER( WRITE )
*         Number of z slices finally used in statistics
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Work out how many of the used z planes contain valid data
*     Set up variables for dummy array passed to stats subroutine
*     Do for all lines (y direction)
*        Do for all pixels in current line (x direction)
*           Do for all included planes (z direction)
*              If current plane has valid data then
*                 Increment dummy array position index by one
*                 Copy (x,y,z) value from 3-d array into dummy array
*              Endif
*           Endfor
*           Call stats subroutine to do return mean and standard deviation
*            for current (x,y) plane pixel along z axis
*           Set mean and standard deviation output array pixels (x,y) to
*            hold the returned values
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
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     18-06-1986 : First implementation (REVA::MJM)
*     1986 Aug 16: Renamed from STATS3D, renamed  STATSSUB, nearly
*                  conformed to Starlink programming standards
*                  (RAL::CUR)
*     1986 Sep 5 : Renamed parameters section in prologue to arguments
*                  (RAL::CUR).
*     1988 Feb 19: Revised statistics subroutine call (RAL::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RAL::CUR).
*     1990 Mar  8: Revised arguments for STATSB and STATDS (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions

*    Import :

      INTEGER
     :    WKDIM1, WKDIM2, WKDIM3,
     :    ZNUMBR

      REAL
     :    WKARR( WKDIM1, WKDIM2, WKDIM3 )
     
      LOGICAL
     :    GDDATA( ZNUMBR )

*    Export :

      INTEGER
     :    GODNUM

      REAL
     :    MEANAR( WKDIM1, WKDIM2 ),
     :    STDVAR( WKDIM1, WKDIM2 )

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER
     :    MAXNUM               ! Maximum number of valid x,y planes
      PARAMETER( MAXNUM = 1000 )! allowed - same default as in calling
                               ! routine

*    Local variables :

      INTEGER
     :    DUMDIM( 2 ),         ! Dimensions of dummy array passed to
                               ! the statistics subroutine
     :    MAXPOS( 2 ),         ! Position of the maximum pixel
     :    MINPOS( 2 ),         ! Position of the minimum pixel
     :    XSTART,              ! x co-ord start in dummy stats array
     :    YSTART,              ! y   "     "    "   "     "     "
     :    XSIZE,               ! x size of subarray in dummy stats
                               ! array
     :    YSIZE,               ! y size of subarray in dummy stats
                               ! array
     :    NUMPIX,              ! Number of pixels included in stats s/r
     :    NINVAL,              !  " of invalid pixels  "    "   "    "
     :    I, J, K, L, M        ! Counter variables

      REAL
     :    DUMARR( MAXNUM, 1 ), ! Dummy array for passing data to stats 
                               ! s/r
     :    MAXMUM,              ! Maximum returned from stats s/r
     :    MINMUM,              ! Minimum     "      "    "    "
     :    TOTAL,               ! Total       "      "    "    "
     :    MEAN,                ! Mean        "      "    "    "
     :    STDDEV               ! Standard deviation "    "    "

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    initialise the good data counter

      GODNUM  =  0

*    calculate the number of good x,y planes - do it once here, even
*    though we could calculate in the main loop before - this way saves
*    cpu - loop round all the elements of the logical mask to check

      DO  L  =  1, ZNUMBR

*       check to see if current entry is set true, i.e. that the 
*       corresponding x,y plane of the 3-d array contains good data

         IF ( GDDATA( L ) ) THEN

*          increment counter by one

            GODNUM  =  GODNUM + 1

         END IF
      END DO


*    set up variables for the dummy statistics array

      DUMDIM( 1 ) =  GODNUM
      DUMDIM( 2 ) =  1
      XSTART      =  1
      YSTART      =  1
      XSIZE       =  GODNUM
      YSIZE       =  1

*    now the main business - start by looping round each line (in y)

      DO  J  =  1, WKDIM2

*       loop round each pixel in current line (in x)

         DO  I  =  1, WKDIM1

*          reset the dummy array index

            M  =  0

*          loop along z axis for the current x,y plane pixel

            DO  K  =  1, ZNUMBR

*             check to see if the current x,y plane contains good data

               IF ( GDDATA( K ) ) THEN

*                increment the dummy array counter

                  M  =  M + 1

*                stick current pixel (x,y,z) value into dummy
*                array to be sent to the statistics subroutine

                  DUMARR( M, 1 )  =  WKARR( I, J, K )

*             end of if-current-(x,y,z)-value-is-valid check

               END IF

*          end of loop along z axis for current (x,y) pixel

            END DO

*          send the dummy array off to get analysed statistically

            CALL STATSB( DUMARR, DUMDIM( 1 ), DUMDIM( 2 ), XSTART,
     :                   YSTART, XSIZE, YSIZE, NUMPIX, MAXMUM, MINMUM,
     :                   TOTAL, MEAN, STDDEV, NINVAL, MAXPOS, MINPOS,
     :                   STATUS )

*          on return, put the required values into the correct output
*          array positions

            MEANAR( I, J )  =  MEAN
            STDVAR( I, J )  =  STDDEV

*       end of loop round pixels in current line

         END DO

*    end of loop round lines

      END DO

 999  CONTINUE

*    return and end

      END
