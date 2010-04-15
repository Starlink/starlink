*+  RED4_DO_REMOVE - Subtract integration from current co-added observation
      SUBROUTINE RED4_DO_REMOVE( INTEGRATION, INTQUALITY, INTDIM1,
     :   INTDIM2, INDEX, INDDIM1, INDDIM2, DET_INDEX, DATA, VARIANCE,
     :   QUALITY, COADDS, DATADIM1, DATADIM2, STATUS )
*    Description :
*     This subroutine removes an integration which has previously been
*     coadded into the current observation.
*     The elements of the observation corresponding to those in the
*     integration are pointed to by the index array. The observation array
*     contains the running mean of all the integrations co-added into it.
*     The variances are re-calculated from the spread of values of the
*     data remaining in the coadd
*    Invocation :
*      CALL RED4_DO_REMOVE( INTEGRATION, INTQUALITY, INTDIM1,
*     :   INTDIM2, INDEX, INDDIM1, INDDIM2, DET_INDEX, DATA, VARIANCE,
*     :   QUALITY, COADDS, DATADIM1, DATADIM2, STATUS )
*    Parameters :
*     INTEGRATION( INTDIM1, INTDIM2 )  = REAL( READ )
*        Array containing integration data to be co-added
*     INTQUALITY( INTDIM1, INTDIM2 )   = BYTE( READ )
*        Array describing the quality of the integration data.
*        It is assumed that INTQUALITY=0 means good data.
*     INTDIM1                          = INTEGER( READ )
*        First dimension of integration array
*     INTDIM2                          = INTEGER( READ )
*        Second dimension of integration array
*     INDEX( INDDIM1, INDDIM2 )        = WORD( READ )
*        Array containing a lookup table for converting an
*        index position in the integration array into a
*        corresponding index position in the observation array.
*     INDDIM1                          = INTEGER( READ )
*        First dimension of the index array
*     INDDIM2                          = INTEGER( READ )
*        Second dimension of the index array
*     DET_INDEX                        = INTEGER( READ )
*        The current detector index, pointing to the values
*        in the index array relevant for the current detector
*        position.
*     DATA( DATADIM1, DATADIM2 )       = REAL( UPDATE )
*        The current observation array into which the integration
*        is to be co-added. This contains a running mean of the
*        data values added so far.
*     VARIANCE( DATADIM1, DATADIM2 )   = REAL( UPDATE )
*        The current variance associated with the observation array.
*        The variance value will only be meaningful if the number
*        of co-oadded points is two or greater.
*     QUALITY( DATADIM1, DATADIM2 )    = BYTE( UPDATE )
*        The current quality associated with the observation array.
*        It is assumed that QUALITY=0 means good data.
*     COADDS( DATADIM1, DATADIM2 )     = WORD( UPDATE )
*        An array containing the number of data points which
*        have been co-added into each element of the observation
*        array.
*     DATADIM1                         = INTEGER( READ )
*        First dimension of the observation array.
*     DATADIM2                         = INTEGER( READ )
*        Second dimension of the observation array.
*     STATUS                           = INTEGER( UPDATE )
*        Global status.
*    Method :
*     The mean and variance are calculated by the method of moments,
*     as described in J.F.Kerry and E.S.Keeping, 1939. "Mathematics
*     of Statistics", Van Nostrand (see p.78). The population variance
*     is used and converted to "standard error squared".
*    Deficiencies :
*     It is not possible for this routine to check that the integration
*     being removed has actually been added previously. It is assumed
*     the calling routine has already done this.
*     The invokation is in a rather strange order in order to be
*     compatible with calling routines.
*    Bugs :
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     1989:  Original version                                        (JFL)
*     22-Jan-1990: History added. This routine contained code which
*                  was rejected by the V5 compiler. Modified.        (SMB)
*     21-Feb-1990: Code spaced out. Bug in trap for divide by zero
*                  errors fixed. Good quality value parameterised.   (SMB)
*      6-Apr-1990: During tests, the variance values calculated by
*                  this routine were found to be erroneous. Because
*                  this routine is vitally important and the code
*                  was uncommented and difficult to understand, the
*                  whole thing was recoded again from scratch,
*                  retaining the original invokation and variable
*                  names. Gaps in the prologue were also filled in.  (SMB)
*     24-Jul-1990: Critical variables changed to double precision
*                  because of rounding errors.                       (SMB)
*     28-Sep-1990: Code modified to use "standard error squared"
*                  instead of variance.                              (SMB)
*     18-Feb-1993: Conform to error strategy                         (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Import :
*    Import :
      INTEGER
     :  INTDIM1,                ! First dimension of integration array
     :  INTDIM2,                ! Second dimension of integration array
     :  INDDIM1,                ! First dimension of index array
     :  INDDIM2,                ! Second dimension of index array
     :  DATADIM1,               ! First dimension of main observation data array
     :  DATADIM2                ! Second dimension of main observation data array
      REAL
     :  INTEGRATION( INTDIM1, INTDIM2 )  ! Integration data to be removed
      BYTE
     :  INTQUALITY( INTDIM1, INTDIM2 )   ! Quality of the integration data
      INTEGER*2
     :  INDEX( INDDIM1, INDDIM2 )        ! Array linking integration array
     :                                   ! index to observation array index
      INTEGER
     :  DET_INDEX                        ! The index of the detector position
*    Import-Export:
      REAL
     :  DATA( DATADIM1, DATADIM2 ),      ! Main coadded observation array
     :                                   ! containing running mean
     :  VARIANCE( DATADIM1, DATADIM2 )   ! Main variance array
      BYTE
     :  QUALITY( DATADIM1, DATADIM2 )    ! Main quality array
      INTEGER*2
     :  COADDS( DATADIM1, DATADIM2 )     ! Number of coadds contributing to data
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE GOOD                 ! Good quality value
      PARAMETER ( GOOD = 0 )
      BYTE BAD                  ! Bad quality value
      PARAMETER ( BAD = 1 )
*    Local variables :
      INTEGER
     :  I, J,                   ! Loop index variables
     :  IPOS,                   ! Index of data value in observation array
     :  NUM                     ! Number of data values making up co-add
      DOUBLE PRECISION
     :  MEAN,                   ! Current running mean
     :  VAR,                    ! Current running variance
     :  INT,                    ! Data value from integration array
     :  SUM,                    ! Current sum of data values
     :  SUM_SQ                  ! Current sum of square of data values
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Scan through each of the elements in the integration array
      DO J = 1, INTDIM2
         DO I = 1, INTDIM1

*         Check the quality of this integration datum and ignore
*         it if it is bad (as it will not have been co-added in
*         the first place).
            IF ( INTQUALITY( I, J ) .EQ. GOOD ) THEN

*            Extract the data value from the integration array
               INT = DBLE( INTEGRATION( I, J ) )

*            Determine the position in the observation array corresponding
*            to this element of the integration array.
               IPOS = INDEX( I, DET_INDEX )

*            Check the quality of this corresponding position in the
*            observation array.
               IF ( QUALITY( IPOS, J ) .EQ. GOOD ) THEN

*               The quality is good. Obtain the number of data values
*               co-added so far, the current mean and current variance.
*               (N.B. For "variance" read "standard error squared").
                  NUM  = COADDS( IPOS, J )
                  MEAN = DBLE( DATA( IPOS, J ) )
                  VAR  = DBLE( NUM ) * DBLE ( VARIANCE( IPOS, J ) )
               ELSE

*               The quality is bad, so no values have contributed so far.
                  NUM = 0
               ENDIF

*            Subsequent processing depends on the number of integrations
*            making up the observation.
               IF ( NUM .LE. 1 ) THEN

*               There are one or fewer integrations. Assuming that
*               the current integration is the same as the one which
*               has been previously co-added, remove the integration
*               leaving no contributing integrations.
                  NUM = 0
                  MEAN = 0.0D0
                  VAR = 0.0D0
               ELSE

*               Obtain the current sum of the data values from the mean.
                  SUM = MEAN * DBLE( NUM )

*               Use this value to obtain the current "sum of squares"
*               (using the Kerry and Keeping formula rearranged).
                  SUM_SQ = VAR * DBLE( NUM - 1 )  +
     :                     SUM * SUM / DBLE( NUM )

*               Decrement the number of points in the co-add
*               and subtract the current data value from the
*               current sum and "sum of squares".
                  NUM = NUM - 1
                  SUM = SUM - INT
                  SUM_SQ = SUM_SQ - INT * INT

*               Use the new sum and "sum of squares" to calculate
*               a new mean and variance.
                  MEAN = SUM / DBLE( NUM )

*               Check there are sufficient points left to calculate
*               a variance. If not, the variance is meaningless and
*               will be set to zero (which does not really mean zero variance!).
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
*            the observation array.
               COADDS( IPOS, J )   = NUM
               DATA( IPOS, J )     = REAL( MEAN )
               VARIANCE( IPOS, J ) = REAL( VAR )

*            The quality will only be good if all the points
*            have not been removed.
                IF ( NUM .GT. 0 ) THEN

                  QUALITY( IPOS, J ) = GOOD
               ELSE

                  QUALITY( IPOS, J ) = BAD
               END IF
            ENDIF
         END DO
      END DO

      END
