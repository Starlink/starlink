*+  P4_BIN - Determine frequency distribution withing 2-D array subset
      SUBROUTINE P4_BIN( XDIM, YDIM, IN, QUAL, QUALITY, I1, I2, IINC,
     :  J1, J2, JINC, LOW, HIGH, SMOOTH, NBINS, XBIN, FREQ, WORK,
     :  TOOSMALL, TOOLARGE, FMIN, FMAX, MODE, STATUS )
*    Description :
*     This routine takes a subset of a 2-D array, delimited by
*     I1, I2, J1, and J2 in steps of IINC and JINC, and determines the
*     frequencies of the data values in NBINS bins between the values
*     LOW and HIGH. If a data value is found exactly on the boundary
*     between two bins it is assigned to the higher bin. The frequency
*     array may optionally be smoothed with a moving box filter, to
*     remove noise spikes which may affect the mode.
*     This routine is used when preparing a histogram of the data.
*    Invocation :
*      CALL P4_BIN( XDIM, YDIM, IN, QUAL, QUALITY, I1, I2, IINC, J1, J2,
*     :  JINC, LOW, HIGH, SMOOTH, NBINS, XBIN, FREQ, WORK, TOOSMALL,
*     :  TOOLARGE, FMIN, FMAX, MODE, STATUS )
*    Parameters :
*     XDIM                 = INTEGER( READ )
*        First dimension of input array
*     YDIM                 = INTEGER( READ )
*        Second dimension of data array
*     IN( XDIM, YDIM )     = REAL( READ )
*        The array of data to be binned.
*     QUAL( XDIM, YDIM )   = BYTE( READ )
*        The data quality associated with the array (if required)
*     QUALITY              = LOGICAL( READ )
*        Flag which is .TRUE. if data quality is to be checked
*     I1                   = INTEGER( READ )
*        The first X element of the array to be binned
*     I2                   = INTEGER( READ )
*        The last X element of the array to be binned
*     IINC                 = INTEGER( READ )
*        The increment in I to be used.
*     J1                   = INTEGER( READ )
*        The first Y element of the array to be binned
*     J2                   = INTEGER( READ )
*        The last Y element of the array to be binned
*     JINC                 = INTEGER( READ )
*        The increment in J to be used.
*     LOW                  = REAL( READ )
*        Data value corresponding to the lower edge of the first bin
*     HIGH                 = REAL( READ )
*        Data value corresponding to the upper edge of the last bin
*     SMOOTH               = INTEGER( READ )
*        Box size required for optional smooth.
*        If 0 or 1, no smooth will be carried out.
*        Other values will be rounded up to the nearest odd number.
*     NBINS                = INTEGER( READ )
*        The number of bins between LOW and HIGH
*     XBIN( NBINS )        = REAL( WRITE )
*        The data value of the lower edge of each bin (i.e. the
*        co-ordinate of the left edge of each bin on a histogram).
*     FREQ( NBINS )        = REAL( WRITE )
*        The frequency of data found within each bin. (Note that if
*        a data value lands exactly between two bins it will be
*        counted as being in the higher bin).
*     WORK( NBINS )            = REAL( WRITE )
*        A work array, which is only used if smoothing of the array
*        is required.
*     TOOSMALL             = INTEGER ( WRITE )
*        The number of data values too small to be counted (i.e. to the
*        left of the left-most bin on a histogram).
*     TOOLARGE             = INTEGER ( WRITE )
*        The number of data values too large to be counted (i.e. to the
*        right of the right-most bin on a histogram).
*     FMIN                 = REAL( WRITE )
*        The minimum frequency found.
*     FMAX                 = REAL( WRITE )
*        The maximum frequency found.
*     MODE                 = REAL( WRITE )
*        The co-ordinate of the centre of the bin in the which
*        maximum frequency count occurs.
*     STATUS               = INTEGER( UPDATE )
*        Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*      5-Feb-1990: Original version.                           (SMB)
*      6-Feb-1990: Data quality checking added.                (SMB)
*      4-Apr-1990: Modified to return the MODE.                (SMB)
*     20-Jul-1990: IINC and JINC parameters added.             (SMB)
*     25-Sep-1990: Bug, in which a rounding error could cause
*                  BIN to exceed NBINS for data values close to
*                  HIGH, fixed.                                (SMB)
*      2-Aug-1991: Modified to use GEN_BSMOTHQ instead of
*                  P4_BOX_SMOOTH.                              (SMB)
*     18-Feb-1993: Tidy up code, error reporting               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER
     :  XDIM,               ! Dimensions of the data array
     :  YDIM,
     :  I1, I2,             ! First element range required
     :  IINC,               ! First element increment
     :  J1, J2,             ! Second element range required
     :  JINC,               ! Second element increment
     :  SMOOTH,             ! Smooth box size required
     :  NBINS               ! Number of bins
      REAL
     :  IN( XDIM, YDIM ),   ! 2-D input data array
     :  LOW, HIGH           ! Data value range required
      BYTE
     :  QUAL( XDIM, YDIM )  ! Data quality array
      LOGICAL
     :  QUALITY             ! .TRUE. if data quality is to be checked
*    Export :
      REAL
     :  XBIN( NBINS ),      ! Co-ordinates of lower edge of each bins
     :  FREQ( NBINS ),      ! The frequencies found. (REAL because this
     :                      ! is what PGBIN expects).
     :  WORK( NBINS ),      ! Work array at least NBINS in size
     :  FMIN,               ! Minimum frequency
     :  FMAX,               ! Maximum frequency
     :  MODE                ! Centre of bin in which FMAX occurs
      INTEGER
     :  TOOSMALL,           ! Number of data items < LOW
     :  TOOLARGE            ! Number of data items >= HIGH
*    Status :
      INTEGER
     :  STATUS              ! Global status
*    External references :
*    Global variables :
*    Local Constants :
      BYTE GOOD             ! Value for good quality
      PARAMETER ( GOOD = 0 )
*    Local variables :
      INTEGER
     :  I,                  ! Input array X index
     :  J,                  ! Input array Y index
     :  NPTS,               ! Number of good points
     :  BIN,                ! Bin number
     :  BINMODE             ! Bin in which maximum frequency occurs
      REAL
     :  WIDTH               ! Bin width
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise counters (toolarge  = -1 because HIGH must always be toolarge!)
      NPTS     = 0
      TOOSMALL = 0
      TOOLARGE = -1

*   Determine the bin co-ordinates and initialise the frequency counters
      IF ( ( HIGH .EQ. VAL__SMLR ) .AND. ( LOW .EQ. 0.0 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4_BIN: '/
     :     /'Bin width would exceed machine precision', STATUS )
         RETURN
      ELSE
         WIDTH = (HIGH - LOW) / REAL( NBINS )
      END IF

      XBIN ( 1 ) = LOW
      FREQ( 1 ) = 0.0
      DO BIN = 2, NBINS

         XBIN( BIN ) = XBIN( BIN-1 ) + WIDTH
         FREQ( BIN ) = 0.0
      END DO

*   Decide if data quality is to be checked.
      IF ( QUALITY ) THEN

*      Data quality checking required.
*      Scan the required rows of the input array
         DO J = J1, J2, JINC

*          Scan the required columns of this row
            DO I = I1, I2, IINC

*            Check the quality of this data element.
*            If it is bad ignore it.
               IF ( QUAL( I, J ) .EQ. GOOD ) THEN

*               Check the data is within the required range
                  IF ( ( IN( I, J ) .GE. LOW ) .AND.
     :                 ( IN( I, J ) .LT. HIGH ) ) THEN

*                  Determine the bin in which this data value lies
*                  and increment it. (N.B. Ensure that a rounding error
*                  will not make BIN exceed NBINS).
                     BIN = 1 + INT( (IN( I, J ) - LOW) / WIDTH )
                     BIN = MIN( BIN, NBINS )
                     FREQ( BIN ) = FREQ( BIN ) + 1.0
                     NPTS = NPTS + 1

                  ELSE IF ( IN( I, J ) .LT. LOW ) THEN

*                  Data too small. Increment counter.
                     TOOSMALL = TOOSMALL + 1

                  ELSE IF ( IN( I, J ) .GE. HIGH ) THEN

*                  Data too large. Increment counter.
                     TOOLARGE = TOOLARGE + 1
                  END IF
               END IF
            END DO
         END DO
      ELSE

*      No data quality checking.
*      Scan the required rows of the input array
         DO J = J1, J2, JINC

*         Scan the required columns of this row
            DO I = I1, I2, IINC

*            Check the data is within the required range
               IF ( ( IN( I, J ) .GE. LOW ) .AND.
     :              ( IN( I, J ) .LT. HIGH ) ) THEN

*               Determine the bin in which this data value lies
*               and increment it. (N.B. Ensure that a rounding error
*               will not make BIN exceed NBINS).
                  BIN = 1 + INT( (IN( I, J ) - LOW) / WIDTH )
                  BIN = MIN( BIN, NBINS )
                  FREQ( BIN ) = FREQ( BIN ) + 1.0
                  NPTS = NPTS + 1

               ELSE IF ( IN( I, J ) .LT. LOW ) THEN

*               Data too small. Increment counter.
                  TOOSMALL = TOOSMALL + 1

               ELSE IF ( IN( I, J ) .GE. HIGH ) THEN

*               Data too large. Increment counter.
                  TOOLARGE = TOOLARGE + 1
               END IF
            END DO
         END DO
      END IF

*   Before determining the mode, smooth the frequency array if
*   required. Smooth it to the work array and then copy the work
*   array back to the original. (Note that the frequency array does
*   not have an associated quality array. The quality array is passed
*   as a dummy zero argument).
      IF ( SMOOTH .GT. 1 ) THEN

         CALL P4_BSMOTHQ( NBINS, FREQ, 0, SMOOTH, WORK, 0,
     :     .FALSE., .FALSE., 0.0, STATUS )

         DO BIN = 1, NBINS

            FREQ( BIN ) = WORK( BIN )
         END DO
      END IF

*   Determine the minimum and maximum frequency and the
*   location of the maximum frequency.
      FMIN = FREQ( 1 )
      FMAX = FREQ( 1 )
      BINMODE = 1
      DO BIN = 2, NBINS

         FMIN = MIN( FMIN, FREQ( BIN ) )

         IF ( FREQ( BIN ) .GT. FMAX ) THEN

            FMAX = FREQ( BIN )
            BINMODE = BIN
         END IF
      END DO

*   Calculate the co-ordinate of the centre of the bin containing
*   the maximum frequency. Half the bin width needs to be added because
*   XBIN holds the co-ordinates of the left edges of each bin.
      IF ( TOOLARGE .LT. 0 ) TOOLARGE = 0
      IF ( NPTS .LE. 0 ) THEN
         MODE = 0.0
      ELSE
         MODE = XBIN( BINMODE ) + ( WIDTH / 2.0 )
      ENDIF

      END
