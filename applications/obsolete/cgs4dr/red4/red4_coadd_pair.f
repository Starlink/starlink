*+  RED4_COADD_PAIR - Coadd OBJECT/SKY pair of observations into current group
      SUBROUTINE RED4_COADD_PAIR( DIM1, DIM2, SKYWT,
     :  OBJDATA, OBJQUAL, SKYDATA, SKYQUAL, DATA, VARIANCE, QUALITY,
     :  COADDS, STATUS )
*    Description :
*     This subroutine coadds a pair of OBJECT and SKY observations into
*     the current reduced group. Variances are calculated from the
*     spread of values co-added into the group. SKY observations are
*     weighted by the factor supplied in SKYWT.
*    Invocation :
*      CALL RED4_COADD_PAIR( DIM1, DIM2, SKYWT,
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
*        The reduced group data array into which observations are to be
*        co-added.
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
*    P.N.Daly (JACH::PND)
*    History :
*     26-Jan-1991: Original version, made by combining together
*                  bits of RED4_DO_COADD and RED4_COADD_OBS. This
*                  was necessary because of a last minute change
*                  in the data reduction system specification!    (SMB)
*      1-Feb-1991: Modified so that the number of PAIRS is now
*                  written to the COADDS array.                   (SMB)
*     18-Feb-1991: IPOS removed.                                  (SMB)
*     12-Sep-1991: RED4_COMMON.INC common blocks added and
*                  POLYFIT option added                           (PND)
*     18-Feb-1993: Conform to error strategy                      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'         ! Contains SAI__ERROR
*    Global variables :
      INCLUDE 'RED4_COMMON.INC' ! RED4 common block required for PF_POLYFIT
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
*            the value to be co-added.
               OBS = DBLE( OBJDATA(I,J) - SKYWT * SKYDATA(I,J) )

*            Check the quality of this corresponding datum in the
*            reduced group array.
               IF ( QUALITY( I, J ) .EQ. GOOD ) THEN

*               The quality is good. Obtain the number of pairs
*               co-added so far, the current mean and current variance.
*               (N.B. For "variance" read "standard error squared").
                  NUM  = COADDS( I, J )
                  MEAN = DBLE( DATA( I, J ) )
                  VAR  = DBLE( NUM ) * DBLE( VARIANCE( I, J ) )
               ELSE

*               The quality is bad. It will become a good point
*               when the current (good) data value is co-added.
*               Record the fact that no values have contributed so far.
                  NUM = 0
               ENDIF

*            Check if this is the first observation pair
*            to be applied to this element of the observation array.
               IF ( NUM .LE. 0 ) THEN

*               This is the first observation pair to be applied.
*               The number of co-adds will become 1, and the running
*               mean will simply be the data value itself.
                  NUM = 1
                  MEAN = OBS

*               At this stage the variance is meaningless.
*               Initialise it to a bad value.
                  VAR = BAD_VARIANCE
               ELSE

*               This is not the first observation pair to be applied.
*               It will need to be co-added with the previous
*               observation pairs.
*               The method for obtaining the current sum and "sum of
*               squares" depends on whether this is the second or
*               a subsequent observation pair.
                  IF ( NUM .EQ. 1 ) THEN

*                  This is the second observation pair to be applied.
*                  The current sum of the data values will simply be the
*                  value already stored in the reduced group array.
*                  (The same as the mean, since NUM=1).
*                  Likewise, the current "sum of squares" will be the
*                  square of this value. (Note that it is not possible
*                  to obtain this from the current variance, as the
*                  variance for one point is meaningless).
                     SUM = MEAN
                     SUM_SQ = MEAN * MEAN
                  ELSE

*                  This is not the second observation pair to be applied.
*                  Obtain the current sum of the data values from the mean.
                     SUM = MEAN * DBLE( NUM )

*                  Use this value to obtain the current "sum of squares"
*                  (using the Kerry and Keeping formula rearranged).
                     SUM_SQ = VAR * DBLE( NUM - 1 )  +
     :                        SUM * SUM / DBLE( NUM )
                  END IF

*               Increment the number of pairs in the co-add
*               and add the current data value to the current sum
*               and "sum of squares".
                  NUM = NUM + 1
                  SUM = SUM + OBS
                  SUM_SQ = SUM_SQ + OBS * OBS

*               Use the new sum and "sum of squares" to calculate
*               a new mean and variance.
                  MEAN = SUM / DBLE( NUM )
                  VAR = ( SUM_SQ  -  ( SUM * SUM ) / DBLE( NUM ) )
     :                  / DBLE( NUM - 1 )

*               Convert this true variance into "standard error squared".
                  VAR = VAR / DBLE( NUM )

*               Make sure rounding errors can never make the variance
*               less than zero.
                  IF (VAR .LT. 0.0D0) THEN

                     VAR = 0.0D0
                  ENDIF
               ENDIF

*            Write the new number of values, mean and variance into
*            the reduced group array. (Note that the COADDS array
*            will contain the number of PAIRS of observations).
               COADDS( I, J )   = NUM
               DATA( I, J )     = REAL( MEAN )
               VARIANCE( I, J ) = REAL( VAR )

*            As at least one good observation pair has been
*            used, this element in the reduced group array is now good.
               QUALITY(I,J) = GOOD
            ELSE
               QUALITY(I,J) = BAD
            ENDIF
         END DO
      END DO

*  We now have data variance and quality array for OBJ-SKY so clean them
*   up before finally putting them in the reduced group.
      IF ( PF_POLYFIT .EQ. 'OBJ-SKY' ) THEN

         CALL RED4_RPOLYFIT( DIM1, DIM2,
     :     DATA, VARIANCE, QUALITY, STATUS )

      END IF

      END
