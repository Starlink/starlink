*+  MEDSET - sets up the weighting function for the weighted median
*            filter

      SUBROUTINE MEDSET( MODE, STEP, SAMSIZ, CENTRE, CORNER, SIDE,
     :                   MEDIAN, NUMSAM, SAMINF, STATUS )
*
*    Description :
*
*     The weighting function for the weighted median filter is set up
*     according to the value of MODE. If MODE is -1 then the input
*     values of CENTRE, CORNER and SIDE are used to set up the
*     weighting function otherwise one of the pre-defined weighting
*     functions will be used. If an unexpected value of MODE is
*     encountered then STATUS will be set to an error value and an error
*     reported. The SAMINF array is filled with the offsets (calculated
*     using the value of STEP ) for the elements to be sampled and their
*     corresponding weights. MEDIAN will contain the position of the
*     median in the sorted weighted list and NUMSAM will contain the
*     number of samples to be sorted. If STATUS had an error value on
*     entry then an immediate return occurs.
*
*     The predefined modes have the following weighting functions:
*
*     0:  1 1 1   1:  0 1 0   2:  1 0 1   3:  1 1 1   4:  0 1 0
*         1 1 1       1 1 1       0 1 0       1 3 1       1 3 1
*         1 1 1       0 1 0       1 0 1       1 1 1       0 1 0
*
*     5:  1 0 1   6:  1 2 1   7:  1 3 1
*         0 3 0       2 3 2       3 3 3
*         1 0 1       1 2 1       1 3 1
*
*    Invocation :
*
*      CALL MEDSET( MODE, STEP, SAMSIZ, CENTRE, CORNER, SIDE, MEDIAN,
*     :             NUMSAM, SAMINF, STATUS )
*
*    Arguments :
*
*     MODE   = INTEGER( READ )
*           Determines which of the predefined weighting functions is to
*           be used if it is the range 0 to 7. If it has the value of
*           -1 then the weighting function is user defined and the
*           information concerning the weighting function will be passed
*           to this routine.
*     STEP   = INTEGER( READ )
*           The separation in pixels between the elements of the
*           weighting function. This is used in calculating the offsets.
*     SAMSIZ = INTEGER( READ )
*           Number of elements in the sample.
*     CENTRE = INTEGER( UPDATE )
*           Centre value for the weighting function, passed to the
*           routine if MODE = -1 or set if MODE is in the range 0 to 7.
*     CORNER = INTEGER( UPDATE )
*           Corner value for the weighting function, passed to the
*           routine if MODE = -1 or set if MODE is in the range 0 to 7.
*     SIDE   = INTEGER( UPDATE )
*           Side value for the weighting function, passed to the
*           routine if MODE = -1 or set if MODE is in the range 0 to 7.
*     MEDIAN = INTEGER( WRITE )
*           Gives the position of the median in the sorted weighted
*           sample.
*     NUMSAM = INTEGER( WRITE )
*           Number of elements in the sample with non-zero weights.
*     SAMINF( SAMSIZ, 3 ) = INTEGER( WRITE )
*           Will contain the offsets to the elements to form the
*           sample and the weights corresponding to each element.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE
*           is outside the range -1 to 7 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If no error on entry then
*        If MODE = -1 then
*           User defined weighting function, calculate median position
*        Elseif 0 <= MODE <= 7 then
*           One of the pre-defined weighting functions
*           Set up CORNER, SIDE and CENTRE and position of median
*        Else
*           Value of MODE not allowed, set STATUS and report error
*        Endif
*        If no error then
*           Insert CORNER into corner elements of WEIGHT array
*           Insert SIDE into side elements of WEIGHT array
*           Insert CENTRE into centre element of WEIGHT array
*           Initialise number of elements in SAMINF array to zero
*           For all elements of the weight array
*              Calculate X, Y offsets from position in WEIGHT array and
*                STEP
*              If weight is non zero then
*                 Increment number of elements
*                 Insert X, Y offsets and weight into SAMINF
*              Endif
*           Endfor
*        Endif
*     Endif
*
*    Deficiencies :
*
*     Uses exclamation marks to show weighting functions.
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     20/10/1983  : Original version                     (ROE::ASOC5)
*     17/02/1984  : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 10 : Renamed parameters section to arguments and tidied
*                   (RL.STAR::CUR)
*     1988 Jun 22 : Added identification to error reporting
*                   (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  SAMSIZ,
     :  MODE,
     :  STEP

*    Import-Export :

      INTEGER
     :  CORNER,
     :  SIDE,
     :  CENTRE

*    Export :

      INTEGER
     :  SAMINF( SAMSIZ, 3 ),
     :  MEDIAN,
     :  NUMSAM

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  WEIGHT( 3, 3 ), ! array containing weighting function
     :  OFSETX,         ! 1st dimension offset to sample elements
     :  OFSETY,         ! 2nd     "        "    "    "       "
     :  INDEX,          ! index to WEIGHT array elements
     :  X,              !   "    "   "       "      "
     :  Y               !   "    "   "       "      "
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       set up weighting values according to value of MODE

         IF( MODE .EQ. -1 ) THEN

*          weighting function specified by user, calculate median
*          position

            MEDIAN = ( ( CENTRE + (4 * SIDE) + (4 * CORNER) ) / 2 ) + 1

         ELSEIF( MODE .EQ. 0 ) THEN

            CORNER = 1              !                      1 1 1
            SIDE   = 1              ! weighting function = 1 1 1
            CENTRE = 1              !                      1 1 1
            MEDIAN = 5

         ELSEIF( MODE .EQ. 1 ) THEN

            CORNER = 0              !                      0 1 0
            SIDE   = 1              ! weighting function = 1 1 1
            CENTRE = 1              !                      0 1 0
            MEDIAN = 3

         ELSEIF( MODE .EQ. 2 ) THEN

            CORNER = 1              !                      1 0 1
            SIDE   = 0              ! weighting function = 0 1 0
            CENTRE = 1              !                      1 0 1
            MEDIAN = 3

         ELSEIF( MODE .EQ. 3 ) THEN

            CORNER = 1              !                      1 1 1
            SIDE   = 1              ! weighting function = 1 3 1
            CENTRE = 3              !                      1 1 1
            MEDIAN = 6

         ELSEIF( MODE .EQ. 4 ) THEN

            CORNER = 0              !                      0 1 0
            SIDE   = 1              ! weighting function = 1 3 1
            CENTRE = 3              !                      0 1 0
            MEDIAN = 4

         ELSEIF( MODE .EQ. 5 ) THEN

            CORNER = 1              !                      1 0 1
            SIDE   = 0              ! weighting function = 0 3 0
            CENTRE = 3              !                      1 0 1
            MEDIAN = 4

         ELSEIF( MODE .EQ. 6 ) THEN

            CORNER = 1              !                      1 2 1
            SIDE   = 2              ! weighting function = 2 3 2
            CENTRE = 3              !                      1 2 1
            MEDIAN = 8

         ELSEIF( MODE .EQ. 7 ) THEN

            CORNER = 1              !                      1 3 1
            SIDE   = 3              ! weighting function = 3 3 3
            CENTRE = 3              !                      1 3 1
            MEDIAN = 10

         ELSE

*          value of MODE not allowed

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'ERR_MEDSET_WMODE',
     :        'MEDSET: MODE = ^MODE not allowed in routine MEDSET',
     :        STATUS )
         END IF

         IF( STATUS .EQ. SAI__OK ) THEN

*          set up the corner and side weights

            DO  INDEX = 1, 3, 2
               WEIGHT( INDEX, 1 ) = CORNER
               WEIGHT( INDEX, 3 ) = CORNER
               WEIGHT( INDEX, 2 ) = SIDE
               WEIGHT( 2, INDEX ) = SIDE
            END DO

*          set up the centre weight

            WEIGHT( 2, 2 ) = CENTRE
         END IF

*       now set up the array of offsets and weights defining the
*       sample array initialise the number of elements in the sample
*       to zero

         NUMSAM = 0

*       for all rows of the weighting function

         DO  Y = 1, 3

*          set up 2nd dim. offset to sample elements

            OFSETY = STEP * ( Y - 2 )

*          for all points in weighting function row

            DO  X = 1, 3

*             set up 1st dim. offset to sample elements

               OFSETX = STEP * ( X - 2 )

*             insert information into SAMINF if element has non-zero
*             weight

               IF( WEIGHT( X, Y ) .NE. 0 ) THEN

*                increment number of elements in sample

                  NUMSAM = NUMSAM + 1

*                insert offsets and weight

                  SAMINF( NUMSAM, 1 ) = OFSETX
                  SAMINF( NUMSAM, 2 ) = OFSETY
                  SAMINF( NUMSAM, 3 ) = WEIGHT( X, Y )
               END IF
            END DO
         END DO
      END IF

      END
