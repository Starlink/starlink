*+  RED4_GET_STATS - Obtain stats on a sub-set of a 2-D array.
      SUBROUTINE RED4_GET_STATS( DIM1, DIM2, DATA, WORK, QUALITY,
     :  QUAL, WHOLE, IST, IEN, IINCR, JST, JEN, JINCR, AUTOSCALE,
     :  HIGH, LOW, MEAN, SIGMA, MEDIAN, MODE, STATUS )
*   Invocation :
*     CALL RED4_GET_STATS( DIM1, DIM2, DATA, WORK, QUALITY, QUAL, WHOLE,
*        IST, IEN, IINCR, JST, JEN, JINCR, AUTOSCALE, HIGH, LOW, MEAN,
*        SIGMA, MEDIAN, MODE, STATUS )
*    Parameters :
*     DIM1               = INTEGER( READ )
*           First dimension of data array.
*     DIM2               = INTEGER( READ )
*           Second dimension of data array.
*     DATA( DIM1, DIM2 ) = REAL( READ )
*           Data array to be examined
*     WORK( DIM1, DIM2 ) = REAL( READ )
*           Work array
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
*           The derived standard deviation
*     MEDIAN             = REAL( WRITE )
*           The median value found.
*     MODE               = REAL( WRITE )
*           The (estimated) modal value
*     STATUS             = INTEGER( UPDATE )
*           Global ADAM status.
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     28-Apr-1995: Original version (PND)
*     09-Jan-1996: Remove NAG dependency M01CAF->PDA_DSORT (PND)
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
      DOUBLE PRECISION
     :  WORK( DIM1*DIM2 )                 ! Work array
      BYTE
     :  QUAL( DIM1, DIM2 )                ! Quality array
*    Export :
      REAL
     :  HIGH,                             ! Highest value found
     :  LOW,                              ! Lowest value found
     :  MEAN,                             ! Mean value found
     :  SIGMA,                            ! Standard deviation value found
     :  MEDIAN,                           ! Median value found
     :  MODE                              ! Modal value found
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE GOOD                           ! Quality value for "good"
      PARAMETER ( GOOD = 0 )
*    Local variables :
      INTEGER
     :  I, J,                             ! Loop counters
     :  ISTART, IEND,                     ! Counter limits
     :  JSTART, JEND,                     ! Counter limits
     :  HIX, HIY,                         ! X,Y co-ord of high point
     :  LOX, LOY,                         ! X,Y co-ord of low point
     :  NGOOD,                            ! Number of good points
     :  NPOINTS                           ! Nunber of  points in array
      REAL RATIO                          ! %age number of goof points
      DOUBLE PRECISION
     : TOTALDP,                           ! Total values (ie Sum(X) )
     : TOTSQ,                             ! Total square values ( ie Sum(X**2) )
     : RSIZE,
     : RSIZE2
      INTEGER
     : ISIZE,                             ! Number of valid points
     : ISIZE2                             ! Number of valid points
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialize counters
      HIX = 0
      HIY = 0
      LOX = 0
      LOY = 0
      NGOOD = 0
      NPOINTS = 0
      ISIZE = 0
      RATIO  = 0.0
      MEAN = 0.0
      SIGMA = 0.0
      MEDIAN = 0.0
      MODE = 0.0
      RSIZE = 0D0
      TOTALDP = 0D0
      TOTSQ = 0D0

*    Determine if the whole array is to be searched.
      IF ( WHOLE ) THEN
        ISTART = 1
        IEND   = DIM1
        JSTART = 1
        JEND   = DIM2
        NPOINTS = DIM1 * DIM2
      ELSE
        ISTART = IST
        IEND   = IEN
        JSTART = JST
        JEND   = JEN
        NPOINTS = ( IEN - IST ) * ( JEN - JST )
      ENDIF

*    If autoscaling set the initial values
      IF ( AUTOSCALE ) THEN
        HIGH = VAL__MINR
        LOW  = VAL__MAXR
      ENDIF

*    Scan the whole array looking for "good" points.
*    Initialise HIGH and LOW to the first "good" point found
*    and thereafter take the minimum and maximum.
      DO J = JSTART, JEND, JINCR
        DO I = ISTART, IEND, IINCR

*        Use quality for good points
          IF ( QUALITY ) THEN
            IF ( QUAL(I,J) .EQ. GOOD ) THEN
              IF ( AUTOSCALE ) THEN
                ISIZE = ISIZE + 1
                NGOOD = NGOOD + 1
                WORK(ISIZE) = DBLE( DATA(I,J) )
                TOTALDP = TOTALDP + DATA(I,J)
                TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                LOW  = MIN( LOW,  DATA(I,J) )
                HIGH = MAX( HIGH, DATA(I,J) )
                IF ( DATA(I,J) .EQ. LOW ) THEN
                  LOX = I
                  LOY = J
                ELSE IF ( DATA(I,J) .EQ. HIGH ) THEN
                  HIX = I
                  HIY = J
                ENDIF
              ELSE
                IF ( DATA(I,J).GE.LOW .AND. DATA(I,J).LE.HIGH )  THEN
                  ISIZE = ISIZE + 1
                  NGOOD = NGOOD + 1
                  WORK(ISIZE) = DBLE( DATA(I,J) )
                  TOTALDP = TOTALDP + DATA(I,J)
                  TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                  IF ( DATA(I,J) .EQ. LOW ) THEN
                    LOX = I
                    LOY = J
                  ELSE IF ( DATA(I,J) .EQ. HIGH ) THEN
                    HIX = I
                    HIY = J
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE

*        No quality array (assume all points good)
            NGOOD = NPOINTS
            IF ( AUTOSCALE ) THEN
              ISIZE = ISIZE + 1
              WORK(ISIZE) = DBLE( DATA(I,J) )
              TOTALDP = TOTALDP + DATA(I,J)
              TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
              LOW  = MIN( LOW,  DATA(I,J) )
              HIGH = MAX( HIGH, DATA(I,J) )
              IF ( DATA(I,J) .EQ. LOW ) THEN
                LOX = I
                LOY = J
              ELSE IF ( DATA(I,J) .EQ. HIGH ) THEN
                HIX = I
                HIY = J
              ENDIF
            ELSE
              IF ( DATA(I,J).GE.LOW .AND. DATA(I,J).LE.HIGH )  THEN
                ISIZE = ISIZE + 1
                WORK(ISIZE) = DBLE( DATA(I,J) )
                TOTALDP = TOTALDP + DATA(I,J)
                TOTSQ = TOTSQ + DATA(I,J)*DATA(I,J)
                IF ( DATA(I,J) .EQ. LOW ) THEN
                  LOX = I
                  LOY = J
                ELSE IF ( DATA(I,J) .EQ. HIGH ) THEN
                  HIX = I
                  HIY = J
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO

*    If no good points, zero the outputs
      IF ( ISIZE .LE. 0 ) THEN
         MEAN   = 0.0
         SIGMA  = 0.0
         MODE   = 0.0
         MEDIAN = 0.0
         IF ( AUTOSCALE )  THEN
            HIGH = 0.0
            LOW  = 0.0
         ENDIF
         CALL MSG_OUT( ' ', 'Array contains no good data points for statistics', STATUS )
      ELSE

         RSIZE  = DBLE( ISIZE )
         RSIZE2 = DBLE( ISIZE - 1 )

*       Calculate the mean
         MEAN = REAL(TOTALDP/RSIZE)

*       Calculate the standard deviation
         IF ( ISIZE .GT. 1 ) THEN
           SIGMA = SQRT(ABS((TOTSQ-(TOTALDP*TOTALDP)/RSIZE)/RSIZE2))
         ELSE
           SIGMA = 0.0
         ENDIF

*       Sort the array (isize elements only)
         CALL PDA_DSORT( WORK, WORK, ISIZE, 1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP( ' ', 'RED4_GET_STATS: Failed to sort array!', STATUS )
         ENDIF

*       Calculate the median
         ISIZE2 = ISIZE / 2
         IF ( 2*ISIZE2 .EQ. ISIZE ) THEN
           MEDIAN = 0.5 * REAL( WORK(ISIZE2) + WORK(ISIZE2+1) )
         ELSE
           MEDIAN = REAL( WORK(ISIZE2+1) )
         ENDIF

*       Calculate (approximate) the mode
         MODE = 3*MEDIAN - 2*MEAN


*       Report on number of used pixels etc
         CALL MSG_SETR( 'HI', HIGH )
         CALL MSG_SETI( 'X', HIX )
         CALL MSG_SETI( 'Y', HIY )
         CALL MSG_OUT( ' ', 'High = ^HI at (^X,^Y)', STATUS )
         CALL MSG_SETR( 'LO', LOW )
         CALL MSG_SETI( 'X', LOX )
         CALL MSG_SETI( 'Y', LOY )
         CALL MSG_OUT( ' ', 'Low  = ^LO at (^X,^Y)', STATUS )
         CALL MSG_SETI( 'NP', NPOINTS )
         CALL MSG_SETI( 'NG', NGOOD )
         CALL MSG_SETR( 'RA', REAL(NGOOD*100)/REAL(NPOINTS) )
         CALL MSG_OUT( ' ',
     :     'Number of data values = ^NP, Used ^NG (^RA %)', STATUS )
      ENDIF
      END
