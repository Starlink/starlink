*+  FIT_CHISQ_ACCUM
	SUBROUTINE FIT_CHISQ_ACCUM(NDAT,OBS,WT,PRED,DCHISQ)
*    Description :
*     Accumulates chi-squared fit between observed and predicted data.
*    History :
*      4 Feb 87: Original (BHVAD::TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
	INTEGER NDAT			! No of values in dataset
	REAL OBS(NDAT)			! Observed data
	REAL WT(NDAT)			! Data weights (inverse variances)
	REAL PRED(NDAT)			! Array of predicted data
*    Import-Export :
	DOUBLE PRECISION DCHISQ		! Double prec chisq accumulator
*    Export :
*    Status :
*    Local constants :
*    Local variables :
	INTEGER I			! Data index
*-----------------------------------------------------------------------------
C	print *,'fit_chisq_accum;ndat,chisq(init): ',ndat,dchisq
	DO I=1,NDAT
C	  print *,'fit_chisq_accum;i,obs: ',i,obs(i)
C	  print *,'wt: ',wt(i)
C	  print *,'pred,chisq: ',pred(i),dchisq
	  DCHISQ=DCHISQ+DBLE(WT(I)*(OBS(I)-PRED(I))**2)
	ENDDO

	END
*+  FIT_DEFIRREGRID - Define values in an irregular grid axis block
      SUBROUTINE FIT_DEFIRREGRID( PAR, NVAL, LOGARITHMIC, VALUES,
     :                                              GAX, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 92 : Original (DJA)
*     30 Jul 93 : Make dynamic copy of data rather than static (DJA)
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
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER                  PAR                     ! Parameter number
      INTEGER                  NVAL                    ! # values in axis
      LOGICAL                  LOGARITHMIC             ! Log axis?
      REAL                     VALUES(*)               ! Values for axis
*
*    Export :
*
      RECORD /GRID_AXIS/       GAX                     ! Grid axis block
*
*    Status :
*
      INTEGER                  STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Fill in fields
      GAX.PAR = PAR
      GAX.REGULAR = .FALSE.
      GAX.NVAL = NVAL
      GAX.LOGARITHMIC = LOGARITHMIC

*    Make space for copy
      CALL DYN_MAPR( 1, NVAL, GAX.VPTR, STATUS )

*    Copy values
      CALL ARR_COP1R( NVAL, VALUES, %VAL(GAX.VPTR), STATUS )

      END
*+  FIT_LOGL_ACCUM - Accumulate max-likilihood over dataset
      SUBROUTINE FIT_LOGL_ACCUM( NDAT, OBS, QOK, OBSQ, PRED,
     :                                       DSTAT, STATUS )
*
*    Description :
*
*     Accumulates maximum likelihood fit between observed and predicted data.
*     Invalid model values (<= 0) are replaced with BADREP. This value is
*     chosen to be sufficiently large to discourage the fitting from venturing
*     into the area of parameter space generating this model, but not so large
*     that overflow problems occur.
*
*    History :
*
*      27 May 92 : Original (BHVAD::DJA)
*      12 Jun 92 : Traps zero or negative models and replace their value
*                  with BADREP. Use quality (DJA)
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
      INTEGER             NDAT			! No of values in dataset
      REAL                OBS(NDAT)		! Observed data
      LOGICAL             QOK                   ! Use quality array?
      LOGICAL             OBSQ(NDAT)            ! Observed data quality
      REAL                PRED(NDAT)		! Array of predicted data
*
*    Import-Export :
*
      DOUBLE PRECISION    DSTAT			! Double prec accumulator
*
*    Status :
*
      INTEGER             STATUS
*
*    Local constants :
*
      REAL                BADREP
        PARAMETER         ( BADREP = 1.0E-6 )
*
*    Local variables :
*
      DOUBLE PRECISION    LSTAT			! Local accumulator
      INTEGER             I			! Data index
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      LSTAT = 0.0D0

*    Quality present?
      IF ( QOK ) THEN

*      Accumulate statistic for good points only
        DO I = 1, NDAT
          IF ( OBSQ(I) ) THEN
            IF ( PRED(I) .GT. 0.0 ) THEN
	      LSTAT = LSTAT + DBLE(PRED(I)-OBS(I)*LOG(PRED(I)))
            ELSE
	      LSTAT = LSTAT + DBLE(BADREP - OBS(I)*LOG(BADREP))
            END IF
          END IF
        END DO

      ELSE

*      Accumulate statistic all points
        DO I = 1, NDAT
          IF ( PRED(I) .GT. 0.0 ) THEN
	    LSTAT = LSTAT + DBLE(PRED(I)-OBS(I)*LOG(PRED(I)))
          ELSE
	    LSTAT = LSTAT + DBLE(BADREP - OBS(I)*LOG(BADREP))
          END IF
        END DO

      END IF

*    Add to external accumulator
      DSTAT = DSTAT + 2.0D0*LSTAT

      END
