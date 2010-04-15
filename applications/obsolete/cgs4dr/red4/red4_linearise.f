*+  RED4_LINEARISE - Linearise data with a polymonial
      SUBROUTINE RED4_LINEARISE( NCOEFF, COEFF, NELM, DATA, VARIANCE,
     :  QUALITY, STATUS )
*    Description :
*     This routine applies a polynomial of given coefficients to a
*     data array in order to linearise the values within it. The
*     variance array associated with the data is scaled accordingly.
*     The routine will only process those parts of the data array
*     which have been assigned a good quality value and which have
*     non-zero data values.
*    Invocation :
*     CALL RED4_LINEARISE( NCOEFF, COEFF, NELM, DATA, VARIANCE,
*     :  QUALITY, STATUS )
*    Parameters :
*     NCOEFF               = INTEGER( READ )
*           The number of coefficients to be applied.
*     COEFF( NCOEFF )      = DOUBLE PRECISION( READ )
*           The polynomial coefficients to be applied.
*     NELM                 = INTEGER( READ )
*           The size of the data array.
*     DATA( NELM )         = REAL( UPDATE )
*           Data array to be linearised.
*     VARIANCE( NELM )     = REAL( UPDATE )
*           Variance array to be scaled with the data.
*     QUALITY( NELM )      = BYTE( UPDATE )
*           Data quality array associated with the data.
*     STATUS               = INTEGER( UPDATE )
*           Global status.
*    Method :
*     The array is linearised using the polynomial formulae given in
*     CGS4/GEN/008, "CGS4 acceptance tests - part 1", section 5.5.
*     For destructive reads the polynomial is;
*        POLY(X) = A*(X-bias) + B*(X-bias)**2 + C*(X-bias)**3 + D*(X-bias)**4
*     and for non-destructive reads;
*        POLY(X) = A*(X) + B*(X)**2 + C*(X)**3 + D*(X)**4
*     where X is the A/D reading and A,B,C,D are tunable coefficients.
*     In this routine A=COEFF(1), B=COEFF(2), C=COEFF(3), D=COEFF(4)
*     and the number of coefficients is not limited to 4.
*     The routine linearises the data by applying the polynomial to
*     each point
*        NEWDATA = POLY( OLDDATA ).
*     The errors associated with each data point are re-scaled
*        NEWERROR = OLDERROR * NEWDATA / OLDDATA
*    Deficiencies :
*     Using this routine can slow down data reduction considerably.
*     Despite the checks, floating point overflows can still occur
*     if the coefficients have silly values.
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*          1989 ?: Original version.                         (JFL)
*     24-Apr-1990: "History" and "Deficiencies" added.       (SMB)
*      3-Sep-1990: "Description" added! Code spaced out and
*                  commented. Bad practise of testing real
*                  variables for equality removed. COEFF
*                  array made variable length (and
*                  corresponding "deficiencies" comment
*                  removed).                                 (SMB)
*     29-Nov-1990: "Parameters" and "Method" sections filled
*                  in. COEFF array changed to double precision
*                  and calculations changed to double precision.
*                  Modified so the error array is multiplied
*                  by the linearisation factor, rather than
*                  the variance array. Data quality taken
*                  into account. Order of arguments altered.
*                  More comments added.                      (SMB)
*      3-Dec-1990: Traps for floating point overflows added. (SMB)
*     22-Feb-1993: Conform to error strategy                 (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER
     :  NCOEFF,                   ! Number of polynomial coefficients
     :  NELM                      ! Number of elements in data arrays
      DOUBLE PRECISION
     :  COEFF( NCOEFF )           ! Linearisation polynomial coefficients
*    Import-Export:
      REAL
     :  DATA( NELM ),             ! Data array to be linearised
     :  VARIANCE( NELM )          ! Variance array associated with the data
      BYTE
     :  QUALITY( NELM )           ! Data quality array
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE GOOD                   ! Good quality value
      PARAMETER ( GOOD = 0 )
      BYTE BAD                    ! Bad quality value
      PARAMETER ( BAD = 1 )
      INTEGER MAXTENPOWER            ! Maximum allowable power of 10
      PARAMETER ( MAXTENPOWER = 38 ) !   in a real number
*    Local variables :
      INTEGER
     :  TENPOWER,                 ! Power of ten
     :  NOFLOW,                   ! Number of floating overflows trapped
     :  I, J                      ! loop variables
      REAL
     :  ADATA,                    ! Used to store ABS( DATA )
     :  MAXDATA                   ! Maximum allowed value for DATA
      DOUBLE PRECISION
     :  DOLDDATA,                 ! Double precision old data value
     :  DERROR,                   ! Double precision error value
     :  MAXDERROR,                ! Maximum allowed value for DERROR
     :  DNEWDATA                  ! Double precision new data value
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialise the number of floating point overflows.
      NOFLOW = 0

*   Set up the maximum possible data value which, when raised to the
*   highest power needed by the polynomial only just stays within the
*   allowed range for a real number. (Note that using integer
*   arithmetic to calculate TENPOWER rounds this figure down).
      TENPOWER = MAXTENPOWER / NCOEFF
      MAXDATA = 10.0 ** TENPOWER

*   Set up the maximum possible error value which, when squared only
*   just stays within the allowed range for a real number.
      TENPOWER = MAXTENPOWER / 2
      MAXDERROR = 10.0D0 ** TENPOWER

*   Loop through each element of the data array.
      DO I = 1, NELM

*      Check this element has a good quality, otherwise ignore it.
         IF ( QUALITY(I) .EQ. GOOD ) THEN

*         Check this element of the data array is non-zero and smaller
*         than the largest allowable value.
*         If the element is zero, leave it alone. There is no constant
*         term in the polynomial, so it will transform zero into zero.
            ADATA = ABS( DATA(I) )
            IF ( (ADATA .GT. VAL__MINR) .AND. (ADATA .LT. MAXDATA) ) THEN

*            Build up the new data value by applying the polynomial,
*            adding each coefficient in turn.
               DOLDDATA = DBLE( DATA(I) )
               DNEWDATA = COEFF(NCOEFF)*DOLDDATA

               DO J = NCOEFF-1, 1, -1

                  DNEWDATA = (DNEWDATA + COEFF(J)) * DOLDDATA
               END DO

*            Obtain the error associated with this element
*            (ensuring the variance is greater than zero).
               IF ( VARIANCE(I) .GT. 0.0 ) THEN

                  DERROR = SQRT( DBLE( VARIANCE(I) ) )
               ELSE

                  DERROR = 0.0D0
               END IF

*            Scale this error accordingly.
               DERROR = DERROR * DNEWDATA / DOLDDATA

*            Check the error value is smaller than the maximum allowed.
               IF ( DERROR .LT. MAXDERROR ) THEN

*               Write the new data and variance back to the original arrays.
                  DATA(I) = REAL( DNEWDATA )
                  VARIANCE(I) = REAL( DERROR * DERROR )
               ELSE

*               The error value is too large. Write the new data
*               value back to the original array, but leave the
*               variance alone and flag the pixel as bad. Also
*               increment the overflow count.
                  DATA(I) = REAL( DNEWDATA )
                  QUALITY(I) = BAD
                  NOFLOW = NOFLOW + 1
               END IF
            ELSE IF ( ADATA .GE. MAXDATA ) THEN

*            The data value is too large. Leave it alone but convert
*            it to a bad point. Also, increment the overflow count.
               QUALITY(I) = BAD
               NOFLOW = NOFLOW + 1
            END IF
         END IF
      END DO

*   Issue a warning if any overflows have been trapped.
      IF ( NOFLOW .GT. 0 ) THEN

         CALL MSG_SETI( 'NOFLOW', NOFLOW )
         CALL MSG_OUT( ' ', 'WARNING - ^NOFLOW floating overflows '/
     :     /'trapped during linearisation.', STATUS )
      END IF

      END
