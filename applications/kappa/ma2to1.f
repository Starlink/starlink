*+  MA2TO1 - Subroutine to extract a 1-D array from a 2-D array

      SUBROUTINE MA2TO1( MODE, IDIM1, IDIM2, ARRIN, ODIM, WDIM, SUM,
     :                   NUMPIX, ARROUT, STATUS )
*
*    Description :
*
*     The 1-dimensional output array, ARROUT, is extracted from the
*     2-dimensional input array, ARRIN, according to the value of MODE.
*     If MODE = 1 the n'th point of ARROUT is formed from the normalised
*     sum (allowing for bad pixels) of the points in the n'th column of
*     ARRIN. If MODE = 2 the n'th point of ARROUT is formed from the
*     normalised sum of the points in the n'th line of ARRIN. An
*     immediate return will occur if STATUS has an error value on entry.
*
*    Invocation :
*
*      CALL MA2TO1( MODE, IDIM1, IDIM2, ARRIN, ODIM, WDIM, SUM, NUMPIX,
*     :             ARROUT, STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 then the output 1-D array is formed from the
*           sum of the points in the corresponding columns of the
*           input array. If MODE is 2 then the output 1-D array is
*           formed from the sum of the points in the corresponding
*           lines in the input array.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Data to be summed to create the output array.
*     ODIM = INTEGER( READ )
*         The first dimension of the output 1-d array.
*     WDIM = INTEGER( READ )
*         Dimension of workspace arrays (should be maximum of IDIM1,
*           IDIM2)
*     SUM( WDIM ) = REAL( WRITE )
*         Work array for storing sums of pixel values
*     NUMPIX( WDIM ) = INTEGER( WRITE )
*         Work array for storing the number of valid pixels in the
*           summations.
*     ARROUT( ODIM ) = REAL( WRITE )
*         Data generated from input array according to the value of
*           MODE.
*     STATUS = INTEGER( UPDATE )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE 
*           has a value other than 1 or 2 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If no error on entry then
*        Set all output array elements to zero
*        If MODE = 1 then
*           Initialise counter arrays
*           For all points in output array
*              Count and form sum of valid pixels in corresponding
*                input array column
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points in
*                corresponding input array column
*              Endif
*           Endfor
*        Elseif MODE = 2 then
*           Initialise counter arrays
*           For all points in output array
*              Count and form sum of valid pixels in corresponding
*                input array line
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points in
*                corresponding input array line
*              Endif
*           Endfor
*        Else
*           Value of MODE not allowed in this routine, set STATUS to
*             SAI__ERROR and report an error
*        Endif
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     01/11/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 8 : Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR).
*     1986 Oct 29: Allow for bad pixels, which necessitated three new
*                  arguments (WDIM, SUM and NUMPIX) (RL.STAR::CUR).
*     1988 Jun 22: Added identification to error reporting
*                  (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global constants
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*    Import :

      INTEGER
     :  IDIM1, IDIM2,          ! Dimensions of input array
     :  ODIM,                  ! Dimension of output array
     :  WDIM,
     :  MODE
            
      REAL
     :  ARRIN( IDIM1, IDIM2 )

*    Export :

      REAL
     :  ARROUT( ODIM ),
     :  SUM( WDIM )

      INTEGER
     :  NUMPIX( WDIM )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,              ! index to input array elements, first dimension
     :  Y               !   "    "   "     "       "     second    "

      REAL
     :  XFAC, YFAC      ! work variables
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       set all elements of the output array to zero

         CALL ZERO1D( ODIM, ARROUT, STATUS )

*       create output array according to value of mode

         IF ( MODE .EQ. 1 ) THEN

*          initialise the summation arrays

            DO  X = 1, IDIM1
               SUM( X ) = 0.0
               NUMPIX( X ) = 0
            END DO

            YFAC = REAL( IDIM2 )

            DO  Y = 1, IDIM2

               DO  X = 1, IDIM1

*                sum number and value of the valid pixels

                  IF ( ARRIN( X, Y ) .NE. VAL__BADR ) THEN
                     SUM( X ) = SUM( X ) + ARRIN( X, Y )
                     NUMPIX( X ) = NUMPIX( X ) + 1
                  END IF
               END DO
            END DO

*          output array point is normalised sum of points in input
*          array column

            DO  X = 1, IDIM1
               IF ( NUMPIX( X ) .EQ. 0 ) THEN
                  ARROUT( X ) = VAL__BADR
               ELSE
                  ARROUT( X ) = SUM( X ) / REAL( NUMPIX( X ) ) * YFAC
               END IF
            END DO

         ELSE IF ( MODE .EQ. 2 ) THEN

*          initialise the summation arrays

            DO  Y = 1, IDIM2
               SUM( Y ) = 0.0
               NUMPIX( Y ) = 0
            END DO

            XFAC = REAL( IDIM1 )

            DO  Y = 1, IDIM2

               DO  X = 1, IDIM1

*                sum number and value of the valid pixels

                  IF ( ARRIN( X, Y ) .NE. VAL__BADR ) THEN
                     SUM( Y ) = SUM( Y ) + ARRIN( X, Y )
                     NUMPIX( Y ) = NUMPIX( Y ) + 1
                  END IF
               END DO
            END DO

*          output array point is normalised sum of points in input
*          array line

            DO  Y = 1, IDIM2
               IF ( NUMPIX( Y ) .EQ. 0 ) THEN
                  ARROUT( Y ) = VAL__BADR
               ELSE
                  ARROUT( Y ) = SUM( Y ) / REAL( NUMPIX( Y ) ) * XFAC
               END IF
            END DO

         ELSE

*          value of MODE is not allowed so set STATUS and report error

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA2TO1_WMODE',
     :        'MA2TO1: MODE = ^MODE not allowed.', STATUS )

         END IF
      END IF

      END
