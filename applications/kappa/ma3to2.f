*+  MA3TO2 - Subroutine to extract a 2-D image from a 3-D image

      SUBROUTINE MA3TO2( MODE, IDIM1, IDIM2, IDIM3, ARRIN, ODIM1, ODIM2,
     :                   WDIM, SUM, NUMPIX, ARROUT, STATUS )
*
*    Description :
*
*     The 2-dimensional output array, ARROUT, is created from the
*     3-dimensional input array, ARRIN, according to the value of MODE.
*     If MODE is 1 the output array is created from the normalised
*     (allowing for bad pixels) sum of all the X,Y planes of the input
*     array, if MODE is 2 then the output array is the normalised sum of
*     all the X,Z planes of the input array and if MODE is 3 then the
*     output array is the normalised sum of all the Y,Z planes of the
*     input array. An immediate return will occur if STATUS has an error
*     value on entry.
*
*    Invocation :
*
*      CALL MA3TO2( MODE, IDIM1, IDIM2, IDIM3, ARRIN, ODIM1, ODIM2,
*     :             WDIM, SUM, NUMPIX, ARROUT, STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 then each point in the output array is formed
*           from the sum over the Z-axis of the corresponding point in
*           the XY plane of the input 3-D array.
*           If MODE is 2 then each point in the output array is formed
*           from the sum over the Y-axis of the corresponding point in
*           the XZ plane of the input 3-D array.
*           If MODE is 3 then each point in the output array is formed
*           from the sum over the X-axis of the corresponding point in
*           the YZ plane of the input 3-D array.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 3-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 3-d array.
*     IDIM3 = INTEGER( READ )
*         The third dimension of the input 3-d array.
*     ARRIN( IDIM1, IDIM2, IDIM3 ) = REAL( READ )
*         Data to be summed to create output array.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array.
*     WDIM = INTEGER( READ )
*         Dimension of workspace arrays (should be at least the
*           product of the largest two of IDIM1, IDIM2, IDIM3)
*     SUM( WDIM ) = REAL( WRITE )
*         Work array for storing sums of pixel values
*     NUMPIX( WDIM ) = INTEGER( WRITE )
*         Work array for storing the number of valid pixels in the
*           summations.
*     ARROUT( ODIM1, ODIM2 ) = REAL( WRITE )
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
*        Set all elements of the output array to zero
*        Initialise counter arrays
*        If MODE is 1 then
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                    Count and form sum of valid input array pixels
*                      along third dimension
*                 Endfor
*              Endfor
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points indexed
*                  by the first and second dimension indices to the
*                  input array point
*              Endif
*           Endfor
*        Else if MODE is 2 then
*           Initialise counter arrays
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                    Count and form sum of valid input array pixels
*                      along second dimension
*                 Endfor
*              Endfor
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points indexed
*                  by the first and third dimension indices to the
*                  input array point
*              Endif
*           Endfor
*        Else if MODE is 3 then
*           Initialise counter arrays
*           For all planes of the input array
*              For all lines of a plane
*                 For all points of a line
*                    Count and form sum of valid input array pixels
*                      along first dimension
*                 Endfor
*              Endfor
*           Endfor
*           For all points in output array
*              If number of valid pixels in sum is zero then
*                 Output pixel is invalid
*              Else
*                Output array point is normalised sum of points indexed
*                  by the second and third dimension indices to the
*                  input array point
*              Endif
*           Endfor
*        Else
*           Value of MODE not allowed, set STATUS to SAI__ERROR and
*             report an error
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
*     30/10/1983 : Original version                     (ROE::ASOC5)
*     20/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 9 : Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR).
*     1986 Oct 29: Allow for bad pixels, which necessitated three new
*                  arguments (WDIM, SUM and NUMPIX) (RL.STAR::CUR).
*     1987 Jun 30: Bug fixes (RL.STAR::CUR).
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
     :  ODIM1, ODIM2,          ! Dimensions of output array
     :  WDIM,
     :  MODE
            
      REAL
     :  ARRIN( IDIM1, IDIM2, IDIM3 ),
     :  SUM( WDIM )

      INTEGER
     :  NUMPIX( WDIM )

*    Export :

      REAL
     :  ARROUT( ODIM1, ODIM2 )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  I,                     ! Index to summation arrays
     :  X,                     ! Index to input array elements, 1st dim
     :  Y,                     !   "    "   "     "       "     2nd  "
     :  Z                      !   "    "   "     "       "     3rd  "

      REAL
     :  XFAC,                  ! Work variable for x dimension
     :  YFAC,                  !  "      "      "  y    "
     :  ZFAC                   !  "      "      "  z    "
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       set all elements of output image to zero

         CALL ZERO2D( ODIM1, ODIM2, ARROUT, STATUS )

*       initialise the summation arrays

         DO  I = 1, WDIM
            SUM( I ) = 0.0
            NUMPIX( I ) = 0
         END DO

*       create output array from input array according to MODE

         IF ( MODE .EQ. 1 )  THEN

            ZFAC = REAL( IDIM3 )

*          Loop for all pixels in input array

            DO  Z = 1, IDIM3

               DO  Y = 1, IDIM2

                  DO  X = 1, IDIM1

*                   sum number and value of the valid pixels and
*                   compute index to summation arrays, I

                     IF ( ARRIN( X, Y, Z ) .NE. VAL__BADR ) THEN
                        I = X + ( Y-1 ) * IDIM1
                        SUM( I ) = SUM( I ) + ARRIN( X, Y, Z )
                        NUMPIX( I ) = NUMPIX( I ) + 1
                     END IF
                  END DO
               END DO
            END DO

            DO  Y = 1, IDIM2
               DO  X = 1, IDIM1

*                compute index to summation arrays

                  I = X + ( Y-1 ) * IDIM1

*                must have used at least one valid pixel otherwise
*                output pixel is invalid

                  IF ( NUMPIX( I ) .EQ. 0 ) THEN
                     ARROUT( X, Y ) = VAL__BADR
                  ELSE

*                   create output array from normalised sum of all the
*                   X,Y planes of the input array

                     ARROUT( X, Y ) = SUM(I) / REAL( NUMPIX(I) ) * ZFAC
                  END IF
               END DO
            END DO

         ELSE IF ( MODE .EQ. 2 ) THEN

            YFAC = REAL( IDIM2 )

*          Loop for all pixels in input array

            DO  Z = 1, IDIM3

               DO  Y = 1, IDIM2

                  DO  X = 1, IDIM1

*                   sum number and value of the valid pixels and
*                   compute index to summation arrays, I

                     IF ( ARRIN( X, Y, Z ) .NE. VAL__BADR ) THEN
                        I = X + ( Z-1 ) * IDIM1
                        SUM( I ) = SUM( I ) + ARRIN( X, Y, Z )
                        NUMPIX( I ) = NUMPIX( I ) + 1
                     END IF
                  END DO
               END DO
            END DO

            DO  Z = 1, IDIM3
               DO  X = 1, IDIM1

*                compute index to summation arrays

                  I = X + ( Z-1 ) * IDIM1

*                must have used at least one valid pixel otherwise
*                output pixel is invalid

                  IF ( NUMPIX( I ) .EQ. 0 ) THEN
                     ARROUT( X, Z ) = VAL__BADR
                  ELSE

*                   create output array from normalised sum of all the
*                   X,Z planes of the input array

                     ARROUT( X, Z ) = SUM(I) / REAL( NUMPIX(I) ) * YFAC
                  END IF
               END DO
            END DO

         ELSE IF ( MODE .EQ. 3 ) THEN

            XFAC = REAL( IDIM1 )

*          Loop for all pixels in input array

            DO  Z = 1, IDIM3

               DO  Y = 1, IDIM2

*                compute index to summation arrays, I

                  I = Y + ( Z-1 ) * IDIM2

                  DO  X = 1, IDIM1

*                   sum number and value of the valid pixels and

                     IF ( ARRIN( X, Y, Z ) .NE. VAL__BADR ) THEN
                        SUM( I ) = SUM( I ) + ARRIN( X, Y, Z )
                        NUMPIX( I ) = NUMPIX( I ) + 1
                     END IF
                  END DO
               END DO
            END DO

            DO  Z = 1, IDIM3
               DO  Y = 1, IDIM2

*                compute index to summation arrays

                  I = Y + ( Z - 1 ) * IDIM2

*                must have used at least one valid pixel otherwise
*                output pixel is invalid

                  IF ( NUMPIX( I ) .EQ. 0 ) THEN
                     ARROUT( Y, Z ) = VAL__BADR
                  ELSE

*                   create output array from normalised sum of all the
*                   Y,Z planes of the input array

                     ARROUT( Y, Z ) = SUM(I) / REAL( NUMPIX(I) ) * XFAC
                  END IF
               END DO
            END DO

         ELSE

*          value of MODE not allowed, set STATUS and report as error

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MA3TO2_WMODE',
     :        'MA3TO2: MODE = ^MODE not allowed.', STATUS )

         END IF
      END IF

      END
