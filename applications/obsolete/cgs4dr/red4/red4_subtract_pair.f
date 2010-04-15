*+  RED4_SUBTRACT_PAIR - Subtract OBJECT/SKY pair of observations from current group
      SUBROUTINE RED4_SUBTRACT_PAIR( DIM1, DIM2, SKYWT,
     :  OBJDATA, OBJQUAL, SKYDATA, SKYQUAL, DATA, VARIANCE, QUALITY,
     :  COADDS, STATUS )
*    Description :
*     This subroutine subtracts a pair of OBJECT and SKY observations from
*     the current reduced group. Variances are calculated from the
*     spread of values co-added into the group. SKY observations are
*     weighted by the factor supplied in SKYWT.
*    Invocation :
*      CALL RED4_SUBTRACT_PAIR( DIM1, DIM2, SKYWT,
*     :  OBJDATA, OBJQUAL, SKYDATA, SKYQUAL, DATA, VARIANCE, QUALITY,
*     :  COADDS, STATUS )
*    Parameters :
*     DIM1                             = INTEGER( READ )
*        First dimension of observation array
*     DIM2                             = INTEGER( READ )
*        Second dimension of observation array
*     SKYWT                            = REAL( READ )
*        Weighting to multiply SKY data by before subtracting.
*     OBJDATA( DIM1, DIM2 )            = REAL( READ )
*        OBJECT observation data array to be co-added.
*     OBJQUAL( DIM1, DIM2 )            = BYTE( READ )
*        OBJECT observation data quality array.
*     SKYDATA( DIM1, DIM2 )            = REAL( READ )
*        SKY observation data array to be co-added.
*     SKYQUAL( DIM1, DIM2 )            = BYTE( READ )
*        SKY observation data quality array.
*     DATA( DIM1, DIM2 )               = REAL( UPDATE )
*        The reduced group data array from which observations are to be
*        subtracted.
*     VARIANCE( DIM1, DIM2 )           = REAL( UPDATE )
*        The current variance of the reduced group array.
*     QUALITY( DIM1, DIM2 )            = BYTE( UPDATE )
*        The current data quality of the reduced group array.
*     COADDS( DATADIM1, DATADIM2 )     = WORD( UPDATE )
*        An array containing the number of observations which
*        have been co-added into each element of the reduced
*        group array. (NOTE: There are two observations per
*        pair, so the numbers within this array need to be
*        divided by 2 to make the number of pairs, which is
*        used in the calculations. This is done to be compatible
*        with RED4_COADD_OBS, where observations are co-added
*        one at a time).
*     STATUS                           = INTEGER( UPDATE )
*        Global status. It can be returned with the following values:
*        ADAM__OK    - Success.
*        SAI__WARN   - Variance weighting required, and the
*                      variance array in the group file contains
*                      one or more zeros.
*    Method :
*     The mean and variance are calculated by the method of moments,
*     as described in J.F.Kerry and E.S.Keeping, 1939. "Mathematics
*     of Statistics", Van Nostrand (see p.78). The population variance
*     is used and converted to "standard error squared".
*    Deficiencies :
*     Zero variance values are written when the variance cannot be
*     calculated. The preferred way of doing this (according to
*     Rodney Warren-Smith, Starlink) is to flag these points
*     with special "bad" (possibly negative) values in the
*     variance array. At the moment, negative values will cause DSA
*     to crash so zero is used. The value is defined in a parameter
*     which can be changed later when "bad" variance values are
*     supported.
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     (with some help from Bob Carswell, CAVAD::RFC)
*     P.N.Daly (JACH::PND)
*    History :
*     18-Feb-1991: Original version, based on RED4_COADD_PAIR and
*                  RED4_DO_REMOVE.                                 (SMB)
*     23-Feb-1993: Conform to error strategy                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'         ! Contains SAI__ERROR
*    Import :
      INTEGER
     :  DIM1,                   ! First dimension of observation array
     :  DIM2                    ! Second dimension of observation array
      REAL
     :  SKYWT,                  ! Weighting factor to be applied to SKY.
     :  OBJDATA( DIM1, DIM2 )   ! OBJECT observation data to be co-added
      BYTE
     :  OBJQUAL( DIM1, DIM2 )   ! Quality of OBJECT observation data
      REAL
     :  SKYDATA( DIM1, DIM2 )   ! SKY observation data to be co-added
      BYTE
     :  SKYQUAL( DIM1, DIM2 )   ! Quality of SKY observation data
*    Import-Export:
      REAL
     :  DATA( DIM1, DIM2 ),     ! Main coadded reduced group array
     :                          ! containing running mean
     :  VARIANCE( DIM1, DIM2 )  ! Main variance array
      BYTE
     :  QUALITY( DIM1, DIM2 )   ! Main quality array
      INTEGER*2
     :  COADDS( DIM1, DIM2 )    ! Number of observations contributing to data
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE
     :  GOOD                    ! Good quality value
      PARAMETER ( GOOD = 0 )
      BYTE
     :  BAD                     ! Bad quality value
      PARAMETER ( BAD = 1 )
      DOUBLE PRECISION
     :  BAD_VARIANCE            ! Bad variance value
      PARAMETER ( BAD_VARIANCE = 0.0D0 )
*    Local variables :
      INTEGER
     :  I, J,                   ! Loop index variables
     :  NUM                     ! Number of data values making up co-add
      DOUBLE PRECISION
     :  MEAN,                   ! Current running mean
     :  VAR,                    ! Current running variance
     :  OBS,                    ! Sky-subtracted data value from observations
     :  SUM,                    ! Current sum of data values
     :  SUM_SQ                  ! Current sum of square of data values
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Scan through each of the elements in the observation arrays
      DO J = 1, DIM2
         DO I = 1, DIM1

*         Check the quality of both the OBJECT and SKY observations
*         at this point and ignore them both if either are bad.
            IF ( ( OBJQUAL( I, J ) .EQ. GOOD ) .AND.
     :           ( SKYQUAL( I, J ) .EQ. GOOD ) ) THEN

*            Subtract the SKY value from the OBJECT value, to make
*            the value to be subtracted.
               OBS = DBLE( OBJDATA(I,J) - SKYWT * SKYDATA(I,J) )

*            Check the quality of this corresponding datum in the
*            reduced group array.
               IF ( QUALITY( I, J ) .EQ. GOOD ) THEN

*               The quality is good. Obtain the number of pairs
*               co-added so far, the current mean and current variance.
*               (N.B. For "variance" read "standard error squared").
                  NUM  = COADDS( I, J )
                  MEAN = DBLE( DATA( I, J ) )
                  VAR  = DBLE( NUM ) * DBLE ( VARIANCE( I, J ) )
               ELSE

*               The quality is bad, so no values have contributed so far.
                  NUM = 0
               ENDIF

*            Subsequent processing depends on the number of integrations
*            making up the observation.
               IF ( NUM .LE. 1 ) THEN

*               There are one or fewer integrations. Assuming that
*               the current pair is the same as the one which
*               has been previously co-added, remove the pair
*               leaving no contributing pairs.
                  NUM = 0
                  MEAN = 0.0D0
                  VAR = 0.0D0
               ELSE

*               Obtain the current sum of the data values from the mean.
                  SUM = MEAN * DBLE( NUM )

*               Use this value to obtain the current "sum of squares"
*               (using the Kerry and Keeping formula rearranged).
                  SUM_SQ = VAR * DBLE( NUM - 1 ) +
     :              SUM * SUM / DBLE( NUM )

*               Decrement the number of pairs in the co-add
*               and subtract the current data value from the
*               current sum and "sum of squares".
                  NUM = NUM - 1
                  SUM = SUM - OBS
                  SUM_SQ = SUM_SQ - OBS * OBS

*               Use the new sum and "sum of squares" to calculate
*               a new mean and variance.
                  MEAN = SUM / DBLE( NUM )

*               Check there are sufficient points left to calculate
*               a variance. If not, the variance is meaningless and
*               will be set to zero (which does not really mean zero
*               variance!).
                  IF ( NUM .GT. 1 ) THEN

                     VAR = ( SUM_SQ  -  ( SUM * SUM ) / DBLE( NUM ) )
     :                     / DBLE( NUM - 1 )

*                  Convert the true variance to "standard error squared".
                     VAR = VAR / DBLE( NUM )
                  ELSE

                     VAR = 0.0D0
                  END IF

*               Make sure rounding errors can never make the variance
*               less than zero.
                  IF (VAR .LT. 0.0D0) THEN

                     VAR = 0.0D0
                  ENDIF
               ENDIF

*            Write the new number of values, mean and variance into
*            the reduced group array.
               COADDS( I, J )   = NUM
               DATA( I, J )     = REAL( MEAN )
               VARIANCE( I, J ) = REAL( VAR )

*            The quality will only be good if all the points
*            have not been removed.
               IF ( NUM .GT. 0 ) THEN

                  QUALITY( I, J ) = GOOD
               ELSE

                  QUALITY( I, J ) = BAD
               END IF
            ENDIF
         END DO
      END DO

      END
