*+  SFIT_OPSTAT - Output statistic to terminal/file
      SUBROUTINE SFIT_OPSTAT( FSTAT, STAT, NDOF, NGOOD, OCI, STATUS )
*
*    Description :
*
*     Writes line describing minimum in the fit statistic to the AIO stream
*     specified by OCI.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Dec 92 : Original. Extracted from SFIT (DJA)
*     12 Jul 94 : Increased number of figures output to 15.8 (DJA)
*     25 Jul 94 : Converted to use AIO system (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                   FSTAT         		! Statistic code
      DOUBLE PRECISION          STAT          		! Value of statistic
      INTEGER                   NDOF	      		! No. degrees of freedom
      INTEGER                   NGOOD	      		! No of good data elements
      INTEGER			OCI			! AIO stream id
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initial blank
      CALL AIO_BLNK( OCI, STATUS )

*    Fit statistic at minimum
      CALL MSG_FMTD( 'SVAL', '1PG15.8', STAT )
      CALL FIT_STATTOK( FSTAT, 'STAT', STATUS )
      IF ( FSTAT .EQ. FIT__CHISQ ) THEN

*      Chi-squared
        CALL MSG_SETI( 'NDOF', NDOF )
        CALL AIO_WRITE( OCI, 'Minimum ^STAT = ^SVAL, with ^NDOF '/
     :                             /'degrees of freedom', STATUS )

*      And unit normal deviate
        IF ( NDOF .GT. 0 ) THEN
          CALL AIO_BLNK( OCI, STATUS )
          CALL MSG_SETR( 'Z', SQRT(2*STAT)-SQRT(2*NDOF-1.0) )
          CALL AIO_WRITE( OCI, 'Equivalent unit normal deviation = ^Z',
     :                    STATUS )
        END IF

      ELSE

*      Cash statistic
        CALL MSG_SETI( 'NGOOD', NGOOD )
        CALL AIO_WRITE( OCI, 'Minimum ^STAT = ^SVAL, using ^NGOOD'/
     :                  /' valid  data points', STATUS )
        CALL AIO_BLNK( OCI, STATUS )

        CALL AIO_WRITE( OCI, 'Likelihood ratio for two models is ',
     :                      ' P1/P2=exp((Cash2-Cash1)/2)', STATUS )

      END IF
      CALL AIO_BLNK( OCI, STATUS )

      END
