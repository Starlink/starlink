*+  P4_GET_STATS - Obtain stats on a sub-set of a 2-D array.
      SUBROUTINE P4_GET_STATS( DIM1, DIM2, DATA, QUALITY,
     :  QUAL, WHOLE, IST, IEN, IINCR, JST, JEN, JINCR,
     :  AUTOSCALE, HIGH, LOW, MEAN, SIGMA, STATUS )
*    Description :
*     This routine returns the mean and sigma for a sub-array. If autoscaling
*     is enabled, it will also return the HIGH and LOW value otherwise it
*     will take these values as input and only operate on data in that range.
*    Invocation :
*     CALL P4_GET_STATS( DIM1, DIM2, DATA, QUALITY, QUAL, WHOLE, IST, IEN,
*     :  IINCR, JST, JEN, JINCR, AUTOSCALE, HIGH, LOW, MEAN, SIGMA, STATUS )
*    Parameters :
*     DIM1               = INTEGER( READ )
*           First dimension of data array.
*     DIM2               = INTEGER( READ )
*           Second dimension of data array.
*     DATA( DIM1, DIM2 ) = REAL( READ )
*           Data array to be examined
*     QUALITY            = LOGICAL( READ )
*           Flag, which is TRUE if the QUAL array is to be used.
*     QUAL( DIM1, DIM2 ) = BYTE( READ )
*           Quality array to be used.
*     WHOLE              = LOGICAL( READ )
*           Should the whole array be examined ?
*     IST                = INTEGER( READ )
*           If WHOLE=FALSE, the first column to be examined.
*     IEN                = INTEGER( READ )
*           If WHOLE=FALSE, the last column to be examined.
*     IINCR              = INTEGER( READ )
*           The increment in the X/I direction.
*     JST                = INTEGER( READ )
*           If WHOLE=FALSE, the first row to be examined.
*     JEN                = INTEGER( READ )
*           If WHOLE=FALSE, the last row to be examined.
*     JINCR              = INTEGER( READ )
*           The increment in the Y/J direction.
*     AUTOSCALE          = LOGICAL( READ )
*           If AUTOSCALE=FALSE,  read HIGH/LOW
*     HIGH               = REAL( UPDATE )
*           The maximum value found.
*     LOW                = REAL( UPDATE )
*           The minimum value found.
*     MEAN               = REAL( WRITE )
*           The mean value found.
*     SIGMA              = REAL( WRITE )
*           The standard deviation found.
*     STATUS             = INTEGER( UPDATE )
*           Global ADAM status.
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*      4-Oct-1993: Original version.                                    (PND)
*      8-NOV-1993: Alter autoscale initial values                       (PND)
*     27-Dec-1993: Set min / max from primdat                           (PND)
*      4-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER
     :  DIM1,                             ! First dimension of data array
     :  DIM2,                             ! Second dimension of data array
     :  IST,                              ! First column to be examined
     :  IEN,                              ! Last column to be examined
     :  IINCR,                            ! Columns increment
     :  JST,                              ! First row to be examined
     :  JEN,                              ! Last row to be examined
     :  JINCR                             ! Rows increment
      LOGICAL
     :  QUALITY,                          ! T if the QUAL array is to be used.
     :  WHOLE,                            ! T if the whole data set to be used
     :  AUTOSCALE                         ! T if autoscaling required
      REAL
     :  DATA( DIM1, DIM2 )                ! Data array
      BYTE
     :  QUAL( DIM1, DIM2 )                ! Quality array
*    Export :
      REAL
     :  HIGH,                             ! Highest value found
     :  LOW,                              ! Lowest value found
     :  MEAN,                             ! Mean value found
     :  SIGMA                             ! Standard deviation value found
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE
     :  GOOD                              ! Quality value for "good"
      PARAMETER ( GOOD = 0 )
*    Local variables :
      INTEGER
     :  I, J,                             ! Loop counters
     :  ISTART, IEND,                     ! Counter limits
     :  JSTART, JEND                      ! Counter limits
      DOUBLE PRECISION
     : TOTALDP,                           ! Total values (ie Sum(X) )
     : TOTSQ                              ! Total square values ( ie Sum(X**2) )
      REAL
     : SIZE                               ! Total number of valis points
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialize counters
      TOTALDP = 0D0
      TOTSQ = 0D0
      SIZE = 0.0
      MEAN = 0.0
      SIGMA = 0.0

*   Determine if the whole array is to be searched.
      IF ( WHOLE ) THEN

         JSTART = 1
         JEND   = DIM2
         ISTART = 1
         IEND   = DIM1
      ELSE

         JSTART = JST
         JEND   = JEN
         ISTART = IST
         IEND   = IEN
      ENDIF

*   If autoscaling set the initial values
      IF ( AUTOSCALE ) THEN

         HIGH = VAL__MINR
         LOW  = VAL__MAXR
      ENDIF

*   Scan the whole array looking for "good" points.
*   Initialise HIGH and LOW to the first "good" point found
*   and thereafter take the minimum and maximum.
      DO J = JSTART, JEND, JINCR
         DO I = ISTART, IEND, IINCR

*         If this is a "good" point, update LOW and HIGH.
            IF ( QUALITY ) THEN

               IF ( QUAL(I,J) .EQ. GOOD ) THEN

                  IF ( AUTOSCALE ) THEN

                     SIZE = SIZE + 1.0
                     TOTALDP = TOTALDP + DATA(I,J)
                     TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                     LOW  = MIN( LOW,  DATA(I,J) )
                     HIGH = MAX( HIGH, DATA(I,J) )
                  ELSE

                     IF ( DATA(I,J).GE.LOW .AND.
     :                    DATA(I,J).LE.HIGH )  THEN
                        SIZE = SIZE + 1.0
                        TOTALDP = TOTALDP + DATA(I,J)
                        TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                     ENDIF
                  ENDIF
               ENDIF
            ELSE

               IF ( AUTOSCALE ) THEN

                  SIZE = SIZE + 1.0
                  TOTALDP = TOTALDP + DATA(I,J)
                  TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                  LOW  = MIN( LOW,  DATA(I,J) )
                  HIGH = MAX( HIGH, DATA(I,J) )
               ELSE

                  IF ( DATA(I,J).GE.LOW .AND.
     :                 DATA(I,J).LE.HIGH )  THEN
                     SIZE = SIZE + 1.0
                     TOTALDP = TOTALDP + DATA(I,J)
                     TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                  ENDIF
               ENDIF
            END IF
         END DO
      END DO

*   If no good points have been found, zero outputs
      IF ( SIZE .LE. 0.0 ) THEN

         SIGMA = 0.0
         MEAN  = 0.0

         IF ( AUTOSCALE )  THEN
            HIGH = 0.0
            LOW  = 0.0
         ENDIF

         CALL MSG_OUT( ' ',
     :     'NB: Array contains no good data points', STATUS )
      ELSE

         MEAN = REAL(TOTALDP/DBLE(SIZE))
         IF (SIZE.GT.1.0)
     :     SIGMA = SQRT(ABS((TOTSQ-(TOTALDP*TOTALDP)/SIZE)/(SIZE-1.0)))
      ENDIF

      END
