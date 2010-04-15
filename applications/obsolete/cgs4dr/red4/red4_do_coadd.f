*+  RED4_DO_COADD - Coadd an integration into current observation
      SUBROUTINE RED4_DO_COADD( INTEGRATION, INTQUALITY, INTDIM1,
     :   INTDIM2, IDX_ARRAY, INDDIM1, INDDIM2, DET_INDEX, DATA, VARIANCE,
     :   QUALITY, COADDS, DATADIM1, DATADIM2, STATUS )
*    Description :
*     This subroutine coadds an integration into the current observation.
*     The elements of the observation corresponding to those in the
*     integration are pointed to by the index array. The observation array
*     contains the running mean of all the integrations co-added into it.
*     Variances are calculated from the spread of values co-added into
*     the observation.
*    Invocation :
*      CALL RED4_DO_COADD (INTEGRATION, INTQUALITY, INTDIM1,
*     :   INTDIM2, IDX_ARRAY, INDDIM1, INDDIM2, DET_INDEX, DATA, VARIANCE,
*     :   QUALITY, COADDS, DATADIM1, DATADIM2, STATUS)
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
*     IDX_ARRAY( INDDIM1, INDDIM2 )        = WORD( READ )
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
*     The invokation is in a rather strange order in order to be
*     compatible with calling routines.
*
*     Zero variance values are written when the variance cannot be
*     calculated. The preferred way of doing this (according to
*     Rodney Warren-Smith, Starlink) is to flag these points
*     with special "bad" (possibly negative) values in the
*     variance array. At the moment, negative values will cause DSA
*     to crash so zero is used. The value is defined in a parameter
*     which can be changed later when "bad" variance values are
*     supported.
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     1989:  Original version                                        (JFL)
*     22-Jan-1990: History added. This routine contained code which
*                  was rejected by the V5 compiler. Modified.        (SMB)
*     21-Feb-1990: Code spaced out. Good quality value
*                  parametersised.                                   (SMB)
*      5-Apr-1990: During tests, the variance values calculated by
*                  this routine were found to be erroneous. Because
*                  this routine is vitally important and the code
*                  was uncommented and difficult to understand, the
*                  whole thing was recoded again from scratch,
*                  retaining the original invokation and variable
*                  names. Gaps in the prologue were also filled in.  (SMB)
*      6-Apr-1990: Variances were still wrong. Debugger used to
*                  locate and correct mistakes in formulae. Results
*                  are now ok.                                       (SMB)
*     24-Jul-1990: Another bug fix. The routine was producing
*                  unreliable variance values when the data contained
*                  large numbers. The fault was caused by rounding
*                  errors when the squares of two very large numbers
*                  were subtracted. Critical variables changed to
*                  double precision.                                 (SMB)
*     28-Sep-1990: Code modified to produce standard error squared
*                  instead of variance.                              (SMB)
*     26-Jan-1991: Bad variance value added, and deficiency section
*                  filled in.                                        (SMB)
*     18-Feb-1993: Conform to error strategy                         (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  INTDIM1,                ! First dimension of integration array
     :  INTDIM2,                ! Second dimension of integration array
     :  INDDIM1,                ! First dimension of index array
     :  INDDIM2,                ! Second dimension of index array
     :  DATADIM1,               ! First dimension of main observation data array
     :  DATADIM2                ! Second dimension of main observation data array
      REAL
     :  INTEGRATION( INTDIM1, INTDIM2 )  ! Integration data to be co-added
      BYTE
     :  INTQUALITY( INTDIM1, INTDIM2 )   ! Quality of the integration data
      INTEGER*2
     :  IDX_ARRAY( INDDIM1, INDDIM2 )    ! Array linking integration array
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
      BYTE GOOD                          ! Good quality value
      PARAMETER ( GOOD = 0 )
      BYTE BAD                           ! Bad quality value
      PARAMETER ( BAD = 1 )
      DOUBLE PRECISION BAD_VARIANCE      ! Bad variance value
      PARAMETER ( BAD_VARIANCE = 0.0D0 )
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
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Scan through each of the elements in the integration array
      DO J = 1, INTDIM2, 1
         DO I = 1, INTDIM1, 1

*         Determine the position in the observation array corresponding
*         to this element of the integration array.
            IPOS = IDX_ARRAY( I, DET_INDEX )

*         Check the quality of this integration datum and ignore it if it is bad.
            IF ( INTQUALITY(I,J) .EQ. GOOD ) THEN

*            Extract the data value from the integration array
               INT = DBLE( INTEGRATION( I, J ) )

*            Check the quality of this corresponding position in the observation array.
               IF ( QUALITY( IPOS, J ) .EQ. GOOD ) THEN

*               The quality is good. Obtain the number of data values
*               co-added so far, the current mean and current variance.
*               (N.B. For "variance" read "standard error squared").
                  NUM  = COADDS( IPOS, J )
                  MEAN = DBLE( DATA( IPOS, J ) )
                  VAR  = DBLE( NUM ) * DBLE( VARIANCE( IPOS, J ) )
               ELSE

*               The quality is bad. It will become a good point
*               when the current (good) data value is co-added.
*               Record the fact that no values have contributed so far.
                  NUM = 0
               ENDIF

*            Check if this is the first integration data value
*            to be applied to this element of the observation array.
               IF ( NUM .LE. 0 ) THEN

*               This is the first integration to be applied.
*               The number of co-adds will become 1, and the running
*               mean will simply be the data value itself.
                  NUM = 1
                  MEAN = INT

*               At this stage the variance is meaningless.
*               Initialise it to a bad value.
                  VAR = BAD_VARIANCE
               ELSE

*               This is not the first integration to be applied.
*               It will need to be co-added with the previous
*               integrations.
*               The method for obtaining the current sum and "sum of
*               squares" depends on whether this is the second or
*               a subsequent integration.
                  IF ( NUM .EQ. 1 ) THEN

*                  This is the second integration to be applied.
*                  The current sum of the data values will simply
*                  be the value already stored in the observation array.
*                  (The same as the mean, since NUM=1).
*                  Likewise, the current "sum of squares" will be the
*                  square of this value. (Note that it is not possible
*                  to obtain this from the current variance, as the
*                  variance for one point is meaningless).
                     SUM = MEAN
                     SUM_SQ = MEAN * MEAN
                  ELSE

*                  This is not the second integration to be applied.
*                  Obtain the current sum of the data values from the mean.
                     SUM = MEAN * DBLE( NUM )

*                  Use this value to obtain the current "sum of squares"
*                  (using the Kerry and Keeping formula rearranged).
                     SUM_SQ = VAR * DBLE( NUM - 1 )  +
     :                        SUM * SUM / DBLE( NUM )
                  END IF

*               Increment the number of points in the co-add
*               and add the current data value to the current sum
*               and "sum of squares".
                  NUM = NUM + 1
                  SUM = SUM + INT
                  SUM_SQ = SUM_SQ + INT * INT

*               Use the new sum and "sum of squares" to calculate
*               a new mean and variance.
                  MEAN = SUM / DBLE( NUM )
                  VAR = ( SUM_SQ  -  ( SUM * SUM ) / DBLE( NUM ) )
     :                  / DBLE( NUM - 1 )

*               Convert this true variance into "standard error squared".
                  VAR = VAR / DBLE( NUM )

*               Make sure rounding errors can never make the variance
*               less than zero.
                  IF ( VAR .LT. 0.0D0 ) VAR = 0.0D0
               ENDIF

*            Write the new number of values, mean and variance into
*            the observation array.
               COADDS( IPOS, J )   = NUM
               DATA( IPOS, J )     = REAL( MEAN )
               VARIANCE( IPOS, J ) = REAL( VAR )
               QUALITY(IPOS,J)     = GOOD
            ELSE
               QUALITY(IPOS,J)     = BAD
            ENDIF
         ENDDO
      ENDDO
      END
