*+  MA3TO1 - Subroutine to extract a 1-D array from a 3-D array

      SUBROUTINE MA3TO1( MODE, IDIM1, IDIM2, IDIM3, ARRIN, ODIM, WDIM,
     :                   SUM, NUMPIX, ARROUT, STATUS )
*
*    Description :
*
*     The 1-dimensional output array, ARROUT, is extracted from the
*     3-dimensional input array, ARRIN, according to the value of MODE.
*     For values of MODE of 1,2 and 3  each point in the output array
*     will be formed from the normalised (for invalid pixels) sum of all
*     the points in the input array with the first, second and
*     third-dimension index respectively given by the index to the
*     output array point. An immediate return will occur if STATUS has
*     an error value on entry.
*
*    Invocation :
*
*      CALL MA3TO1( MODE, IDIM1, IDIM2, IDIM3, ARRIN, ODIM, WDIM, SUM,
*     :             NUMPIX, ARROUT, STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 each point in the output 1-D array is formed
*           from the sum over the Y and Z axes of the corresponding
*           points of the input 3-D arrays X-axis.
*           If MODE is 2 each point in the output 1-D array is formed
*           from the sum over the X and Z axes of the corresponding
*           points of the input 3-D arrays Y-axis.
*           If MODE is 3 each point in the output 1-D array is formed
*           from sum over the X and Y axes of the corresponding points
*           of the input 3-D arrays Z-axis.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 3-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 3-d array.
*     IDIM3 = INTEGER( READ )
*         The third dimension of the input 3-d array.
*     ARRIN( IDIM1, IDIM2, IDIM3 ) = REAL( READ )
*         Data to be summed to create output array.
*     ODIM = INTEGER( READ )
*         The dimension of the output array.
*     WDIM = INTEGER( READ )
*         Dimension of workspace arrays (should be maximum of IDIM1, 
*           IDIM2, IDIM3)
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
*           has a value other than 1,2 or 3 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If no error on entry then
*        Set all elements of output array to zero
*        If MODE is 1 then
*           Initialise counter arrays
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                    Count and form sum of valid input array pixels
*                 Endfor
*              Endfor
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points indexed
*                  by the input array points' first dimension index.
*              Endif
*           Endfor
*        Else if MODE is 2 then
*           Initialise counter arrays
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                    Count and form sum of valid input array pixels
*                 Endfor
*              Endfor
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points indexed
*                  by the input array points' second dimension index.
*              Endif
*           Endfor
*        Else if MODE is 3 then
*           Initialise counter arrays
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                    Count and form sum of valid input array pixels
*                 Endfor
*              Endfor
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points indexed
*                  by the input array points' third dimension index.
*              Endif
*           Endfor
*        Else
*           Value of MODE not allowed so set STATUS to SAI__ERROR and
*             report an error
*        End if
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     28/10/1983 : Original version                     (ROE::ASOC5)
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
     :  IDIM1, IDIM2, IDIM3,   ! Dimensions of input array
     :  ODIM,                  ! Dimension of output array
     :  WDIM,
     :  MODE

      REAL
     :  ARRIN( IDIM1, IDIM2, IDIM3 )

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
     :  X,                     ! index to input array elements, 1st dim
     :  Y,                     !   "    "   "     "       "     2nd  "
     :  Z                      !   "    "   "     "       "     3rd  "

      REAL
     :  XYFAC,                 ! work variable for product of x,y dimns.
     :  XZFAC,                 !  "      "      "     "    "  x,z   "
     :  YZFAC                  !  "      "      "     "    "  y,z   "
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       set all elements of the output array to zero

         CALL ZERO1D( ODIM, ARROUT, STATUS )

         IF ( MODE .EQ. 1 ) THEN

*          initialise the summation arrays

            DO  X = 1, IDIM1
               SUM( X ) = 0.0
               NUMPIX( X ) = 0
            END DO

            YZFAC = REAL( IDIM2 * IDIM3 )

*          Loop for all pixels in input array

            DO  Z = 1, IDIM3

               DO  Y = 1, IDIM2

                  DO  X = 1, IDIM1

*                   sum number and value of the valid pixels

                     IF ( ARRIN( X, Y, Z ) .NE. VAL__BADR ) THEN
                        SUM( X ) = SUM( X ) + ARRIN( X, Y, Z )
                        NUMPIX( X ) = NUMPIX( X ) + 1
                     END IF
                  END DO
               END DO
            END DO

*          create output array from normalised sum of points over the
*          other two dimensions

            DO  X = 1, IDIM1
               IF ( NUMPIX( X ) .EQ. 0 ) THEN
                  ARROUT( X ) = VAL__BADR
               ELSE
                  ARROUT( X ) = SUM( X ) / REAL( NUMPIX( X ) ) * YZFAC
               END IF
            END DO


         ELSE IF ( MODE .EQ. 2 ) THEN

*          initialise the summation arrays

            DO  Y = 1, IDIM2
               SUM( Y ) = 0.0
               NUMPIX( Y ) = 0
            END DO

            XZFAC = REAL( IDIM1 * IDIM3 )

*          Loop for all pixels in input array

            DO  Z = 1, IDIM3

               DO  Y = 1, IDIM2

                  DO  X = 1, IDIM1

*                   sum number and value of the valid pixels

                     IF ( ARRIN( X, Y, Z ) .NE. VAL__BADR ) THEN
                        SUM( Y ) = SUM( Y ) + ARRIN( X, Y, Z )
                        NUMPIX( Y ) = NUMPIX( Y ) + 1
                     END IF
                  END DO
               END DO
            END DO

*          create output array from normalised sum of points over the
*          other two dimensions

            DO  Y = 1, IDIM2
               IF ( NUMPIX( Y ) .EQ. 0 ) THEN
                  ARROUT( Y ) = VAL__BADR
               ELSE
                  ARROUT( Y ) = SUM( Y ) / REAL( NUMPIX( Y ) ) * XZFAC
               END IF
            END DO

         ELSE IF ( MODE .EQ. 3 ) THEN

*          initialise the summation arrays

            DO  Z = 1, IDIM3
               SUM( Z ) = 0.0
               NUMPIX( Z ) = 0
            END DO

            XYFAC = REAL( IDIM1 * IDIM2 )

*          Loop for all pixels in input array

            DO  Z = 1, IDIM3

               DO  Y = 1, IDIM2

                  DO  X = 1, IDIM1

*                   sum number and value of the valid pixels

                     IF ( ARRIN( X, Y, Z ) .NE. VAL__BADR ) THEN
                        SUM( Z ) = SUM( Z ) + ARRIN( X, Y, Z )
                        NUMPIX( Z ) = NUMPIX( Z ) + 1
                     END IF
                  END DO
               END DO
            END DO

*          create output array from normalised sum of points over the
*          other two dimensions

            DO  Z = 1, IDIM3
               IF ( NUMPIX( Z ) .EQ. 0 ) THEN
                  ARROUT( Z ) = VAL__BADR
               ELSE
                  ARROUT( Z ) = SUM( Z ) / REAL( NUMPIX( Z ) ) * XYFAC
               END IF
            END DO

         ELSE

*          value of MODE not allowed so set STATUS and report error

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA3TO1_WMODE',
     :        'MA3TO1: MODE = ^MODE not allowed.', STATUS )

         END IF
      END IF

      END
