*+  IPSF - puts PSF profile on 1D plot
      SUBROUTINE IPSF(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*     16 Aug 90 : V1.2-1 Outputs fit statistics (DJA)
*      3 Oct 91 : V1.5-0 PMAX parameter overrides fitting (DJA)
*     21 Feb 92 : V1.6-0 USes IRADIAL_DOIT to profile 2D psf array (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL    ADX, ADY                   ! Psf bin size in radians
      REAL    AMP                        ! Best fit source amplitude
      REAL    BGND                       ! Background value
      REAL    BGNDERR                    ! Background error value
      REAL    CHI                        ! Reduced chi-squared of fit
      REAL    CMAX                       ! Psf profile centre value
      REAL    EQNZ                       ! Equivalent normal z
      REAL    PMAX                       ! Requested psf central value

      INTEGER NDOF                       ! Degrees of freedom in fit
      INTEGER PDIMS(2)                   ! Psf dimensions
      INTEGER PSF_PTR                    ! Ptr to psf data
      INTEGER WPNTR                      ! Workspace for profiling

      LOGICAL FIT                        ! Fit psf profile to data?
      LOGICAL NEW
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IPSF Version 1.6-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP_1D) THEN
        CALL MSG_PRNT('AST_ERR: no 1D plot displayed')
      ELSE

*  get PSF if none yet loaded
        IF (I_PSF.EQ.0) THEN
          CALL PSF_ASSOCI(I_LOC,I_PSF,STATUS)
        ELSE
*  or see if different PSF to be loaded
          CALL USI_GET0L('NEW',NEW,STATUS)
          IF (NEW) THEN
            CALL PSF_RELEASE(I_PSF,STATUS)
            CALL PSF_ASSOCI(I_LOC,I_PSF,STATUS)
          ENDIF
        ENDIF

*  get dynamic space for line and 2D array
        I_N_AUX=I_N_1D
        CALL IMG_GETAUX(I_N_AUX,STATUS)

        PDIMS(1) = I_N_AUX*2+1
        PDIMS(2) = PDIMS(1)
        CALL DYN_MAPR( 2, PDIMS, PSF_PTR, STATUS )

*  get 2D psf data
        ADX = SIGN(I_XSCALE_1D,I_XSCALE)*I_WTORAD
        ADY = SIGN(I_XSCALE_1D,I_YSCALE)*I_WTORAD
        CALL PSF_2D_DATA(I_PSF,I_X*I_WTORAD,I_Y*I_WTORAD,
     :           0.0, 0.0, ADX, ADY,
     :           .TRUE., PDIMS(1), PDIMS(2), %VAL(PSF_PTR), STATUS )


*  get profiling workspace
        CALL DYN_MAPR( 1, I_N_AUX, WPNTR, STATUS )

*  profile the psf data
        CALL IRADIAL_DOIT( 1, PDIMS(1), PDIMS(2), %VAL(PSF_PTR),
     :              0, .FALSE., 0, .FALSE., REAL(PDIMS(1))/2.0,
     :              REAL(PDIMS(2))/2.0, %VAL(WPNTR), %VAL(I_DPTR_AUX),
     :              0, 0, STATUS )

*  release workspace
        CALL DYN_UNMAP( WPNTR, STATUS )
        CALL DYN_UNMAP( PSF_PTR, STATUS )

*  get background per pixel and correct for oversampling
        CALL USI_GET0R( 'BGND', BGND, STATUS )
        CALL USI_GET0R( 'BGNDERR', BGNDERR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        BGND = BGND / REAL(I_OSAMPLE**2)
        BGNDERR = BGNDERR / REAL(I_OSAMPLE**2)

*  get scale factor
        CALL USI_GET0R( 'PMAX', PMAX, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          FIT = .FALSE.
        ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
          FIT = .TRUE.
          CALL ERR_ANNUL(STATUS)
        ELSE
          GOTO 99
        END IF

*  fit profile to data
        IF ( FIT ) THEN
          CALL IPSF_FIT(I_N_AUX,%VAL(I_DPTR_1D),%VAL(I_VPTR_1D),
     :                  %VAL(I_QPTR_1D) , BGND, BGNDERR**2,
     :                          %VAL(I_DPTR_AUX),
     :                                        AMP,CHI,STATUS)

*    display statistics
          IF ( STATUS .EQ. SAI__OK ) THEN

*           find goodness of fit
             NDOF = I_N_AUX - 1
             EQNZ = SQRT(2.0*CHI*REAL(NDOF)) - SQRT(REAL(2*NDOF)-1.0)

             CALL MSG_BLNK()
             CALL MSG_PRNT( '********** Fit of psf model to data '/
     :                                              /'********** ')
             CALL MSG_BLNK()
             CALL MSG_SETR( 'AMP', AMP )
             CALL MSG_PRNT( '      Fitted source flux  : ^AMP' )
             CALL MSG_SETR( 'CHI', CHI )
             CALL MSG_PRNT( '      Reduced chi-squared : ^CHI' )
             CALL MSG_SETR( 'EQNZ', EQNZ )
             CALL MSG_PRNT( '      Equivalent normal z : ^EQNZ' )
             CALL MSG_BLNK()
           END IF

*      multiply to get central value = PMAX
        ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*        get current centre value
          CALL ARR_ELEM1R( I_DPTR_AUX, I_N_AUX, 1, CMAX, STATUS )
          CALL ARR_MULTR( PMAX/CMAX, I_N_AUX, %VAL(I_DPTR_AUX) )

        END IF

*      Add background value
        CALL ARR_ADD1R( BGND, I_N_AUX, %VAL(I_DPTR_AUX) )

*      Plot line
        CALL IMG_AUX(STATUS)

      ENDIF

 99   CONTINUE

      CALL USI_CLOSE()

      END


*+  IPSF_FIT
      SUBROUTINE IPSF_FIT(N,D,V,Q,B,BV,P,AMP,CHI,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER N
      REAL B,D(*),V(*),BV
      BYTE Q(*)
*    Import/Export :
      REAL P(*)
*    Export :
      REAL AMP,CHI
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Local constants :
*    Local variables :
      INTEGER I,NGOOD
      REAL PXD,PSQ,WT,DV
*-
      IF (STATUS.EQ.SAI__OK) THEN

*      Good radial bin counter
        NGOOD = N

*      Find normalisation
        PXD=0.0
        PSQ=0.0
        DO I=1,N
          IF (BIT_ANDUB( Q(I),I_MASK) .EQ. QUAL__GOOD ) THEN
            WT=V(I)+BV
            DV = MAX(0.0,D(I)-B)
            PXD=PXD+P(I)*DV/WT
            PSQ=PSQ+P(I)*P(I)/WT
          ELSE
            NGOOD = NGOOD - 1
          END IF
        END DO
        AMP = PXD/PSQ

*      Find quality of fit
        CHI=0.0
        DO I=1,N
          IF ( BIT_ANDUB(Q(I),I_MASK) .EQ. QUAL__GOOD ) THEN
            WT=V(I)+BV
            DV = MAX(0.0,D(I)-B)
            P(I)=P(I)*AMP
            CHI=CHI+(DV-P(I))**2.0/WT
          END IF
        END DO
        CHI = CHI / REAL(NGOOD-1)

      END IF

      END



      subroutine ipsf_print(n,d)

      integer n
      real d(n)

      integer i

      do i=1,n
        write(*,'(1x,f10.5)') d(i)
      enddo

      end
