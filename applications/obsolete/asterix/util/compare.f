*+  COMPARE - Chi-squared comparison of model array and data array
      SUBROUTINE COMPARE(STATUS)
*
*    Description :
*
*     Calculates the chi-squared fit of a data array to a model array. The
*     resulting chi-squared value is listed, together with the normalization
*     factor which brings the model into closest agreement with the data.
*
*    Parameters :
*
*     DATA=UNIV(W)
*      	    Dataset containing data values
*     MODEL=UNIV(R)
*           Dataset containing model values
*     POISS_ERR=LOGICAL(R)
*           Poisson data errors to be assumed
*
*    Method :
*
*     Chi-squared is calculated as    Sum[(data-model)**2/variance] ,
*     where variance is the variance of the data, unless the model array also
*     has a VARIANCE component, in which case it is the sum of the model and
*     data variances. i.e the program can be used to compare two datasets for
*     compatibility.
*     If the data have no error component then either Poisson or unit errors
*     may be assumed.
*     The number of degrees of freedom is taken to be the number of data points
*     (i.e. model is assumed to be independent of data).
*     Bad quality data points are excluded from the statistic, but the model
*     array is assumed to be 100% good.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     28 Apr 86 :         Original (BHVAD::TJP)
*     28 May 86 :         Option of unit errors (TJP)
*     14 Dec 88 : V1.0-1  Asterix88 upgrade (DJA)
*     18 Nov 90 : V1.3-0  Minor bug fix (DJA)
*     20 Mar 92 : V1.6-0  Uses quality properly. Calculations done using
*                         variance rather than error. Checkjs zero
*                         variance explicitly. (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) DLOC            ! Data dataset locator
      CHARACTER*(DAT__SZLOC) MLOC            ! Model dataset locator
      CHARACTER*(DAT__SZTYP) TYPE1           ! data type of 1st dataset
      CHARACTER*(DAT__SZTYP) TYPE2           ! data type of 2nd dataset

      REAL                   FACTOR          ! Renormalization factor for model
      REAL                   S               ! Chi-squared fit
      REAL                   SNEW            ! Chi-squared for scaled model
      REAL                   ZEQUIV          ! Equivalent normal z

      INTEGER                DIMS(DAT__MXDIM)! Dimensions of a dataset
      INTEGER                DPTR            ! Pointer to data array
      INTEGER                DQPTR           ! Pointer to data quality
      INTEGER                DVPTR           ! Pointer to data variance
      INTEGER                MPTR            ! Pointer to model data
      INTEGER                MVPTR           ! Pointer to model variance
      INTEGER                NDIM            ! # dimensions
      INTEGER                NDOF            ! # degrees of freedom for S
      INTEGER                NELD            ! # data elements
      INTEGER                NELM            ! # model elements
      INTEGER                NZV             ! # points with zero variance

      LOGICAL                MOD_ERR         ! Model errors present
      LOGICAL                OK              ! Data item acceptable
      LOGICAL                QUAL            ! Data quality present
      LOGICAL                POISS           ! Poisson errors assumed
      LOGICAL                PRIM            ! Primitive input ?
      LOGICAL                SOMEBAD         ! Any bad quality points ?
*
*    Version :
*
      CHARACTER*30           VERSION
         PARAMETER           (VERSION = 'COMPARE Version 1.8-0')
*-

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Start up Asterix
      CALL AST_INIT()

*    Get the data
      CALL USI_ASSOCI( 'DATA', 'READ', DLOC, PRIM, STATUS )
      CALL DAT_TYPE(DLOC,TYPE2,STATUS)
      CALL BDA_CHKDATA( DLOC, OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

      IF ( OK ) THEN
        CALL ARR_SUMDIM( NDIM, DIMS, NELD )
      ELSE
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'Numeric data required.', STATUS )
	GOTO 99
      END IF

*    Get the model
      CALL USI_ASSOCI( 'MODEL', 'READ', MLOC, PRIM, STATUS )
      CALL DAT_TYPE(MLOC,TYPE1,STATUS)

      CALL BDA_CHKDATA( MLOC, OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

      IF ( OK ) THEN
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      ELSE
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'Numeric data required.', STATUS )
	GO TO 99
      END IF

*    Check sizes compatible
      IF ( NELD .NE. NELM ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ','Data and model arrays are of'
     :                     //' different size', STATUS )
	GOTO 99

      ELSE
	CALL MSG_SETC( 'MTYPE', TYPE1 )
	IF ( TYPE1 .EQ. TYPE2 ) THEN
	  CALL MSG_PRNT( 'Datasets are both of type ^MTYPE' )
	ELSE
	  CALL MSG_SETC( 'DTYPE', TYPE2 )
	  CALL MSG_PRNT( 'Datasets are of types ^MTYPE and ^DTYPE' )
	END IF

      END IF

*    Model
      CALL BDA_MAPDATA( MLOC, 'READ', MPTR, STATUS )
      CALL BDA_CHKVAR( MLOC, MOD_ERR, NDIM, DIMS, STATUS )
      IF ( MOD_ERR ) THEN
        CALL BDA_MAPVAR( MLOC, 'READ', MVPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*    Data
      CALL BDA_MAPDATA( DLOC, 'READ', DPTR, STATUS )
      CALL BDA_CHKVAR( DLOC, OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( OK ) THEN
        CALL BDA_MAPVAR( DLOC, 'READ', DVPTR, STATUS )
      ELSE

*      No existing errors - options are Poisson errors or unit errors
	CALL USI_GET0L( 'POISS_ERR', POISS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL DYN_MAPR( 1, NELM, DVPTR, STATUS )

	IF ( POISS ) THEN
          CALL ARR_COP1R( NELM, %VAL(DPTR), %VAL(DVPTR), STATUS )
	ELSE
          CALL ARR_INIT1R( 1.0, NELM, %VAL(DVPTR), STATUS )
          CALL MSG_PRNT( 'Assuming unit data errors' )
	END IF

      END IF

*    Data quality present?
      CALL BDA_CHKQUAL( DLOC, QUAL, NDIM, DIMS, STATUS )
      IF ( QUAL ) THEN
        CALL BDA_MAPLQUAL( DLOC, 'READ', SOMEBAD, DQPTR, STATUS )
        IF ( .NOT. SOMEBAD ) THEN
          CALL BDA_UNMAPLQUAL( DLOC, STATUS )
          QUAL = .FALSE.
        END IF
      END IF

*    Calculate chi-squared fit, renormalization factor, and improved fit
      CALL COMPARE_CHIFIT( NELM, %VAL(DPTR), %VAL(DVPTR),
     :                     QUAL, %VAL(DQPTR), %VAL(MPTR),
     :                     MOD_ERR, %VAL(MVPTR),
     :                     S, NDOF, FACTOR, SNEW, NZV, STATUS )

*    Find equivalent normal z
      ZEQUIV=SQRT(2*S)-SQRT(2.0*NDOF-1)

*    Output results to terminal
      CALL MSG_PRNT( ' ' )
      CALL MSG_SETI( 'NDOF', NDOF )
      CALL MSG_PRNT( '^NDOF good data values ( = d.o.f )' )
      IF ( NZV .GT. 0 ) THEN
        CALL MSG_SETI( 'N', NZV )
        CALL MSG_PRNT( '^N points ignored due to zero data variance' )
      END IF
      CALL MSG_SETR( 'S', S )
      CALL MSG_PRNT( 'Chi-squared fit to model : ^S' )
      CALL MSG_SETR( 'ZEQUIV', ZEQUIV )
      CALL MSG_PRNT( 'Equivalent normal z : ^ZEQUIV' )
      CALL MSG_SETR( 'FAC', FACTOR )
      CALL MSG_PRNT( 'Model is optimised by scaling factor ^FAC' )
      CALL MSG_SETR( 'SNEW', SNEW )
      CALL MSG_PRNT( 'New chi-squared : ^SNEW' )

      ZEQUIV=SQRT(2*SNEW)-SQRT(2.0*NDOF-3)	! NDOF is reduced by 1
      CALL MSG_SETR( 'NEQZ', ZEQUIV )

      CALL MSG_PRNT( 'New equiv. normal z : ^NEQZ' )

*    Release dataset
      CALL BDA_RELEASE( DLOC, STATUS )
      CALL BDA_RELEASE( MLOC, STATUS )

*    Tidy up and exit
 99   CALL AST_CLOSE( STATUS )
      CALL AST_ERR( STATUS )

      END



*+  COMPARE_CHIFIT - Compare and model and data
      SUBROUTINE COMPARE_CHIFIT( N, DATA, DVAR, QFLAG, QUAL,
     :                           MODEL, ERRFLAG, MVAR, CHISQ,
     :                           NDOF, FACTOR, NCHISQ, NZV, STATUS )
*    Description :
*
*     Calculates chi-squared fit of MODEL and DATA arrays, also the
*     renormalization factor required to optimize the model fit, and
*     the reduced chi-squared obtained with this scaling of the model.
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
*     29 Apr 86 : Original (?)
*
*     20 Mar 92 : Tidied (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER           N                      ! Number of data points
      REAL              DATA(N)                ! Data values
      REAL              DVAR(N)                ! Data variance
      LOGICAL           QFLAG                  ! Data quality present?
      LOGICAL           QUAL(N)                ! Data quality
      REAL              MODEL(N)               ! Model values
      LOGICAL           ERRFLAG                ! Model errors present?
      REAL              MVAR(N)                ! Model variances
*
*    Export :
*
      INTEGER           NDOF                   ! Degrees of freedom
      REAL              CHISQ                  ! Chi-squared
      REAL              FACTOR                 ! Normalisation factor
      REAL              NCHISQ                 ! Chi-squared after norm'n
      INTEGER           NZV                    ! # points with 0 variance
*
*    Status :
*
      INTEGER           STATUS
*
*    Local variables :
*
      DOUBLE PRECISION  SSUM                   ! Chi-squared accumlator
      DOUBLE PRECISION  MDSUM,MMSUM            !

      REAL              VAR                    ! Combined data & model variance

      INTEGER           I                      ! Loop over data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise sums
      SSUM = 0.0D0
      MDSUM = 0.0D0
      MMSUM = 0.0D0
      NDOF = N
      NZV = 0

*    Summation - Model errors and data quality present
      IF ( ERRFLAG .AND. QFLAG ) THEN
	DO I=1,N
	  IF ( QUAL(I) ) THEN
	    VAR = DVAR(I) + MVAR(I)
            IF ( VAR .GT. 0.0 ) THEN
	      MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/VAR)
	      MMSUM=MMSUM+DBLE(MODEL(I)**2/VAR)
	      SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/VAR)
            ELSE
              NDOF = NDOF - 1
              NZV = NZV + 1
            END IF
          ELSE
            NDOF = NDOF - 1
          END IF
	END DO

*    Quality but no model errors
      ELSE IF ( QFLAG ) THEN
	DO I = 1 , N
	  IF ( QUAL(I) ) THEN
            IF ( DVAR(I) .GT. 0.0 ) THEN
	      MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/DVAR(I))
	      MMSUM=MMSUM+DBLE(MODEL(I)**2/DVAR(I))
	      SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/DVAR(I))
            ELSE
              NDOF = NDOF - 1
              NZV = NZV + 1
            END IF
          ELSE
            NDOF = NDOF - 1
          END IF
	END DO

*    Model errors but no quality
      ELSE IF ( ERRFLAG ) THEN
	DO I=1,N
	  VAR = DVAR(I)+MVAR(I)
          IF ( VAR .GT. 0.0 ) THEN
	    MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/VAR)
	    MMSUM=MMSUM+DBLE(MODEL(I)**2/VAR)
	    SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/VAR)
          ELSE
            NDOF = NDOF - 1
            NZV = NZV + 1
          END IF
	END DO

* No quality or model errors
      ELSE
	DO I=1,N
          IF ( DVAR(I) .GT. 0.0 ) THEN
	    MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/DVAR(I))
	    MMSUM=MMSUM+DBLE(MODEL(I)**2/DVAR(I))
	    SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/DVAR(I))
          ELSE
            NDOF = NDOF - 1
            NZV = NZV + 1
          END IF
	END DO

      END IF

*    Calculate statistics from the sums
      CHISQ = REAL(SSUM)
      FACTOR = MDSUM/MMSUM
      NCHISQ = CHISQ - MMSUM*(FACTOR-1)**2

      END
