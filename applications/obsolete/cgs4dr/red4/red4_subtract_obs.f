*+  RED4_SUBTRACT_OBS - Subtract an observation from current group
      SUBROUTINE RED4_SUBTRACT_OBS( DIM1, DIM2, WEIGHT,
     :  VARIANCE_WT,
     :  OBSDATA, OBSVAR, OBSQUAL,
     :  DATA, VARIANCE, QUALITY,
     :  COADDS, STATUS )
*    Description :
*     This subroutine subtracts an observation from the current reduced group.
*     The data values and variances are subtracted from those of the group.
*     The resultant values are then divided by the number of coadds, to
*     produce a mean and "standard error squared".
*     Optionally, the contribution from each observation may be weighted
*     according to its variance.
*    Invocation :
*      CALL RED4_SUBTRACT_OBS( DIM1, DIM2, WEIGHT, VARIANCE_WT,
*     :  OBSDATA, OBSVAR, OBSQUAL, DATA, VARIANCE, QUALITY,
*     :  COADDS, STATUS )
*    Parameters :
*     DIM1                             = INTEGER( READ )
*        First dimension of observation array
*     DIM2                             = INTEGER( READ )
*        Second dimension of observation array
*     WEIGHT                           = REAL( READ )
*        Weighting to multiply observation data by before being subtracted.
*     VARIANCE_WT                      = LOGICAL( READ )
*        Flag which is .TRUE. if variance weighting is to be used.
*     OBSDATA( DIM1, DIM2 )            = REAL( READ )
*        Observation data array to be subtracted.
*     OBSVAR( DIM1, DIM2 )             = REAL( READ )
*        Observation variance array.
*     OBSQUAL( DIM1, DIM2 )            = BYTE( READ )
*        Observation data quality array.
*     DATA( DIM1, DIM2 )               = REAL( UPDATE )
*        The reduced group data array into which observations are to be
*        subtracted.
*     VARIANCE( DIM1, DIM2 )           = REAL( UPDATE )
*        The current variance of the reduced group array.
*     QUALITY( DIM1, DIM2 )            = BYTE( UPDATE )
*        The current data quality of the reduced group array.
*     COADDS( DATADIM1, DATADIM2 )     = WORD( UPDATE )
*        An array containing the number of data points which
*        have been co-added into each element of the reduced
*        group array.
*     STATUS                           = INTEGER( UPDATE )
*        Global status.
*        ADAM__OK    - Success.
*        SAI__WARN   - Variance weighting required, and the
*                      variance array in the group file contains
*                      one or more zeros.
*    Method :
*     Variance weighting is carried out using the method described in
*     P.R.Bevington, 1969. "Data Reduction and Error Analysis for the
*     Physical Sciences", p.69. The data values are weighted by the
*     inverse of their variance. The variance of the result is estimated
*     by taking the inverse of the sum of the inverse variances.
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     (with some help from Bob Carswell, CAVAD::RFC)
*     P.N.Daly (JACH::PND)
*    History :
*     31-Oct-1990: Original version, based on RED4_COADD_OBS.     (SMB)
*      7-Nov-1990: VARIANCE_WT and SKY_WT parameters included.    (SMB)
*      8-Nov-1990: Silly typing mistakes fixed.                   (SMB)
*      8-Nov-1990: Mistake discovered. The standard error was not
*                  being calculated properly. The variance should
*                  be divided by N squared, not N.                (SMB)
*     28-Nov-1990: The variance weighting was not being done properly.
*                  A normalising factor of (1/SIGMA**2) was missed
*                  out. Bevington consulted and code corrected. I
*                  hope it is now working correctly.                 (SMB)
*     23-Feb-1993: Conform to error strategy                         (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'         ! Includes SAI__WARN
*    Import :
      INTEGER
     :  DIM1,                   ! First dimension of observation array
     :  DIM2                    ! Second dimension of observation array
      LOGICAL
     :  VARIANCE_WT      ! TRUE if variance weighting is to be used.
      REAL
     :  WEIGHT,                 ! Weighting factor to be applied
     :  OBSDATA( DIM1, DIM2 ),  ! Observation data to be subtracted.
     :  OBSVAR( DIM1, DIM2 )    ! Variance of observation data
      BYTE
     :  OBSQUAL( DIM1, DIM2 )   ! Quality of observation data
*    Import-Export:
      REAL
     :  DATA( DIM1, DIM2 ),     ! Main coadded reduced group array
     :                          ! containing running mean
     :  VARIANCE( DIM1, DIM2 )  ! Main variance array
      BYTE
     :  QUALITY( DIM1, DIM2 )   ! Main quality array
      INTEGER*2
     :  COADDS( DIM1, DIM2 )    ! Number of coadds contributing to data
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE GOOD                 ! Good quality value
      PARAMETER ( GOOD = 0 )
      BYTE BAD                  ! Bad quality value
      PARAMETER ( BAD = 1 )
      REAL LARGE_NUMBER         ! A large number used instead of infinity
      PARAMETER ( LARGE_NUMBER = 1.0E30 )
      DOUBLE PRECISION
     :  DLARGE_NUMBER           ! A large number used instead of infinity
      PARAMETER ( DLARGE_NUMBER = 1.0D30 )
*    Local variables :
      INTEGER
     :  I, J,                   ! Loop index variables
     :  NUM                     ! Number of data values making up co-add
      DOUBLE PRECISION
     :  MEAN,                   ! Current running mean
     :  VAR,                    ! Current running variance
     :  OBS,                    ! Data value from observation array
     :  OVAR,                   ! Variance value from observation array
     :  SUM,                    ! Current sum of data values
     :  SUMINVAR,               ! Current sum of inverse variances
     :  SESQ,                   ! Current standard error squared
     :  INSESQ                  ! Current inverse standard error squared
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Check if variance weighting is required.
      IF ( VARIANCE_WT ) THEN

*      Variance weighting has been used. Use the algorithm described
*      in Bevington.
*      Scan through each of the elements in the observation array
         DO J = 1, DIM2
            DO I = 1, DIM1

*            Check the quality of this observation datum and ignore
*            it if it is bad, as it will not have been co-added in
*            the first place.
               IF ( OBSQUAL( I, J ) .EQ. GOOD ) THEN

*               Extract the data and variance values from the
*               observation array. (Note that the calling routine
*               will have already checked that OVAR is sensible.
*               Note also that the factor WEIGHT will be cancelled
*               out by the variance weighting. Only its sign will
*               be significant).
                  OBS = DBLE( WEIGHT ) * DBLE( OBSDATA( I, J ) )
                  OVAR = DBLE( ABS( WEIGHT ) ) * DBLE( OBSVAR( I, J ) )

*               Check the quality of the corresponding datum in the
*               reduced group data array.
                  IF ( QUALITY( I, J ) .EQ. GOOD ) THEN

*                  The quality is good. Obtain the number of data values
*                  co-added so far, the current total and current sum
*                  of the inverse variances (avoiding a divide by zero if
*                  the variance array is zero).
                     NUM = COADDS( I, J )

                     IF ( VARIANCE( I, J ) .GT. 0.0 ) THEN

                        SUMINVAR = 1.0D0 / DBLE( VARIANCE( I, J ) )
                        SUM = DBLE( DATA( I, J ) ) * SUMINVAR
                     ELSE

*                     The variance is zero. Try and cope sensibly by
*                     assigning a large number to the inverse and
*                     weighting the data by 1. But set a bad status,
*                     as this shouldn't really happen.
                        SUMINVAR = DLARGE_NUMBER
                        SUM = DBLE( DATA( I, J ) )
                        STATUS = SAI__WARN
                     END IF

*                  Check if the last co-add is about to be removed.
                     IF ( NUM .LE. 1 ) THEN

*                     The last co-add is to be removed. Assume that
*                     the current observation is the same as the one
*                     which has previously been coadded,. Remove
*                     the observation, leaving no contributing
*                     observations.
                        NUM = 0
                        MEAN = 0.0D0
                        INSESQ = DLARGE_NUMBER
                     ELSE

*                     This is not the last co-add to be removed.
*                     Subtract the observation value and inverse
*                     variance, weighting the datum by its inverse
*                     variance.
                        SUM = SUM - OBS / OVAR
                        SUMINVAR = SUMINVAR - 1.0D0 / OVAR

*                     Decrement the number of coadds and obtain a mean
*                     and inverse "standard error squared".
                        NUM = NUM - 1
                        MEAN = SUM / SUMINVAR
                        INSESQ = SUMINVAR
                     END IF
                  ELSE

*                  The quality is bad. No values will have contributed.
                     NUM = 0
                     MEAN = 0.0D0
                     INSESQ = DLARGE_NUMBER
                  END IF

*               Write the current mean and standard error squared
*               back to the reduced group array (avoiding problems
*               if the inverse standard error is zero or negative).
                  COADDS( I, J )   = NUM
                  DATA( I, J )     = REAL( MEAN )

                  IF ( INSESQ .GT. 0.0D0 ) THEN

                     VARIANCE( I, J ) = REAL( 1.0D0 / INSESQ )

*                  There must be at least one good observation remaining
*                  for the point to be assigned a good quality. Otherwise
*                  set a bad quality.
                     IF ( NUM .GT. 0 ) THEN

                        QUALITY(I,J) = GOOD
                     ELSE

                        QUALITY(I,J) = BAD
                     END IF
                  ELSE

                     VARIANCE( I, J ) = LARGE_NUMBER

*                  The variance array is not sensible. This becomes
*                  a bad point.
                     QUALITY(I,J) = BAD
                  END IF
               ENDIF
            END DO
         END DO
      ELSE

*      Variance weighting is not required. Remove the data in the
*      usual way, assuming a running mean of the data values and
*      the standard errors.
*      Scan through each of the elements in the observation array
         DO J = 1, DIM2
            DO I = 1, DIM1

*            Check the quality of this observation datum and ignore
*            it if it is bad, as it will not have been co-added in
*            the first place.
               IF ( OBSQUAL( I, J ) .EQ. GOOD ) THEN

*               Extract the data and variance values from the
*               observation array.
                  OBS = DBLE( WEIGHT ) * DBLE( OBSDATA( I, J ) )
                  OVAR = DBLE( ABS( WEIGHT ) ) * DBLE( OBSVAR( I, J ) )

*               Check the quality of the corresponding datum in the
*               reduced group data array.
                  IF ( QUALITY( I, J ) .EQ. GOOD ) THEN

*                  The quality is good. Obtain the number of data values
*                  co-added so far, the current total and current variance.
*                  (Note that the variance needs to be multipled by N
*                  squared. One N converts the standard error squared to
*                  variance. The other N converts the mean variance to
*                  total variance).
                     NUM = COADDS( I, J )
                     SUM = DBLE( NUM ) * DBLE( DATA( I, J ) )
                     VAR = DBLE( NUM * NUM ) * DBLE( VARIANCE( I, J ) )

*                  Check if the last co-add is about to be removed.
                     IF ( NUM .LE. 1 ) THEN

*                     The last co-add is to be removed. Assume that
*                     the current observation is the same as the one
*                     which has previously been coadded,. Remove
*                     the observation, leaving no contributing
*                     observations.
                        NUM = 0
                        MEAN = 0.0D0
                        SESQ = 0.0D0
                     ELSE

*                     This is not the last co-add to be removed.
*                     Subtract the observation value and variance.
                        SUM = SUM - OBS
                        VAR = VAR - OVAR

*                     Decrement the number of coadds and divide by this
*                     number to obtain a mean and "standard error squared".
*                     (Note that the variance is divided by N squared. One
*                     N converts the total variance into mean variance.
*                     The other converts mean variance to mean standard
*                     error squared).
                        NUM = NUM - 1
                        MEAN = SUM / DBLE( NUM )

                        SESQ = VAR / DBLE( NUM * NUM )
                     END IF
                  ELSE

*                  The quality is bad. No values will have contributed.
                     NUM = 0
                     MEAN = 0.0D0
                     SESQ = 0.0D0
                  END IF

*               Ensure that rounding errors can never make the
*               standard error squared less than zero.
                  IF ( SESQ .LT. 0.0D0 ) THEN

                     SESQ = 0.0D0
                  END IF

*               Write the current mean and standard error squared
*               back to the reduced group array.
                  COADDS( I, J )   = NUM
                  DATA( I, J )     = REAL( MEAN )
                  VARIANCE( I, J ) = REAL( SESQ )

*               There must be at least one good observation remaining
*               for the point to be assigned a good quality. Otherwise
*               set a bad quality.
                  IF ( NUM .GT. 0 ) THEN

                     QUALITY(I,J) = GOOD
                  ELSE

                     QUALITY(I,J) = BAD
                  END IF
               ENDIF
            END DO
         END DO
      END IF

      END
