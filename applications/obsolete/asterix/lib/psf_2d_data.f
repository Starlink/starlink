*+  PSF_2D_DATA - Grab 2D PSF data using routine specified by SLOT.
      SUBROUTINE PSF_2D_DATA( SLOT, X0, Y0, QX, QY, ADX, ADY, INTEG, NX,
     :                                               NY, ARRAY, STATUS )
*
*    Deficiencies :
*
*     Returns 2D array of psf probability using the psf defined by SLOT.
*     See PROG_011 for a complete definition of the arguments. The two
*     arguments ADX and ADY may be defaulted to the defined pixel size by
*     settting their value to VAL__BADR.
*
*    Method :
*
*     The spatial part of the psf pointed to by SLOT may be either a
*     simple primitive psf or a POLAR or RECT model. The latter two
*     options are only
*     invoked if certain criteria are met,
*
*      1) The value of NY must be > 1. Profiling routines use NY=1 and
*         should not access model psfs
*      2) The psf to array centre vector (QX,QY) must be zero
*
*     No such restrictions apply to energy modelling.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Oct 89 : Original (DJA)
*     29 Oct 92 : Spectral model functionality (DJA)
*      5 Jan 94 : Added defaulting of DX and DY (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      INTEGER                  SLOT                    ! PSF to use
      REAL                     X0, Y0                  ! Evaluate psf here
      REAL                     QX,QY                   ! Offset to ARRAY centre
      REAL                     ADX, ADY                ! Pixel size in radians
      INTEGER                  NX,NY                   ! Size of array
      LOGICAL                  INTEG                   ! Integrate probability?
*
*    Export :
*
      REAL                     ARRAY(*)                ! The probability array
*
*    Status :
*
      INTEGER                  STATUS
*
*    Functions :
*
      REAL                     PSF1_GETAXDR
*
*    Local variables :
*
      REAL                     DX, DY                  ! Pixel size in radians
      REAL                     MA, MR                  ! Centre of polar bin
      REAL                     MX0, MY0                ! Centre of model psf
      REAL                     R2                      ! Radius squared

      INTEGER                  IA, IR, IX, IY          ! Model bin indices
      INTEGER                  LID, MID                ! Library/psf numbers
      INTEGER                  MODI                    ! Model index
      INTEGER                  PDATA                   ! Model psf data ptr
      INTEGER                  X_AX, Y_AX, E_AX, T_AX  ! Axis identifiers

      LOGICAL*1                PSF_STATE               ! Model accessed yet?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get library and module identifiers
      LID = P_LIBID(SLOT)
      MID = P_MODID(SLOT)

*    Copy ADX and ADY to locals. Check for defaults
      DX = ADX
      DY = ADY
      IF ( (DX .EQ. VAL__BADR) .OR. (DY.EQ.VAL__BADR) ) THEN
        CALL PSF_QAXES( SLOT, X_AX, Y_AX, E_AX, T_AX, STATUS )
        IF ( DX .EQ. VAL__BADR ) THEN
          DX = PSF1_GETAXDR( P_INST(SLOT), X_AX, STATUS )
        END IF
        IF ( DY .EQ. VAL__BADR ) THEN
          DY = PSF1_GETAXDR( P_INST(SLOT), Y_AX, STATUS )
        END IF
      END IF

*    Is this model with satisfactory arguments
      IF ( P_MODEL(SLOT) .AND.
     :     (QX.EQ.0.0).AND.(QY.EQ.0.0).AND.
     :     (NY.GT.1) ) THEN

*      Is the size of ARRAY in-consistent with with the model
*      definition? This will be either because a different size
*      psf is being requested, or because its the first access
        IF ( NX*NY .GT. SM_NELM(SLOT) ) THEN

*        Free the current model data if present. If none present, it
*        must be the first allocation so grab the flags array too.
          IF ( SM_GOTDATA(SLOT) ) THEN
            CALL DYN_UNMAP( SM_DATA(SLOT), STATUS )
            SM_GOTDATA(SLOT) = .FALSE.
          ELSE
            CALL DYN_MAPB( 1, SM_NMOD(SLOT), SM_FLAG(SLOT), STATUS )
          END IF

*        Define new model size
          SM_NX(SLOT) = NX
          SM_NY(SLOT) = NY
          SM_NELM(SLOT) = NX*NY
          SM_SIZEDEF(SLOT) = .TRUE.

*        Allocate new memory
          IF ( .NOT. SM_GOTDATA(SLOT) ) THEN
            CALL DYN_MAPR( 1, SM_NELM(SLOT)*SM_NMOD(SLOT),
     :                             SM_DATA(SLOT), STATUS )
            SM_GOTDATA(SLOT) = .TRUE.
          END IF

*        Clear the flags
          CALL ARR_INIT1B( 0, SM_NMOD(SLOT),
     :                     %VAL(SM_FLAG(SLOT)), STATUS )

        END IF

*      Decide on the model index for this image position. Polar grid
        IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN

*        Find radial bin
          R2 = X0*X0+Y0*Y0
          IF ( SM_P_REG(SLOT) ) THEN
            IR = (SQRT(R2)/SM_P_DR(SLOT)) + 1
          ELSE
            IR = 1
            DO WHILE ( R2 .GT. SM_P_RUP(IR,SLOT) )
              IR = IR + 1
            END DO
          END IF

*        Find azimuthal bin. Only handle one azimuthal bin for central
*        radial bin.
          IF ( (IR.EQ.1) .OR. (SM_P_NA(SLOT).EQ.1) ) THEN
            IA = 1
          ELSE
            MA = ATAN2D(Y0,X0)+180.0
            IA = INT( MA*SM_P_NA(SLOT)/360.0) + 1
          END IF

*        Compute model index from azimuthal radial bins
          MODI = (IA-1)*SM_P_NR(SLOT) + IR

*      Rectangular grid
        ELSE IF ( SM_TYPE(SLOT) .EQ. PSF_RGRID ) THEN

*        Compute model index from azimuthal and altitudinal bins
          IX = INT(ABS(X0/SM_R_DX(SLOT))) + 1
          IY = INT(ABS(Y0/SM_R_DY(SLOT))) + 1
          MODI = (IY-1)*SM_R_NY(SLOT) + IX

        ELSE
          STATUS = SAI__ERROR
        END IF

*      Is the flag set for this model index
        CALL ARR_COP1B( 1, %VAL(SM_FLAG(SLOT)+MODI-1),
     :                  PSF_STATE, STATUS )

*      If the psf hasn't been access yet, do so now
        PDATA = SM_DATA(SLOT) + (MODI-1)*SM_NELM(SLOT)*VAL__NBR
        IF ( .NOT. PSF_STATE ) THEN

*        Decide centre of model psf bin
          IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN
            IF ( IR .EQ. 1 ) THEN
              MX0 = 0.0
              MY0 = 0.0
            ELSE
              IF ( IA .EQ. 1 ) THEN
                MA = 0.0
              ELSE
                MA = (REAL(IA-1)+0.5)*360.0/REAL(SM_P_NA(SLOT))
              END IF

              IF ( SM_P_REG(SLOT) ) THEN
                MR = (REAL(IR-1)+0.5)*SM_P_DR(SLOT)
              ELSE

*              Trap case of infinite irregular radius, and peg to last
*              specified radius.
                IF ( IR .EQ. SM_P_NR(SLOT) ) THEN
                  MR = SQRT( SM_P_RUP(IR-1,SLOT) )
                ELSE
                  MR = (SQRT(SM_P_RUP(IR,SLOT))-
     :                  SQRT(SM_P_RUP(IR-1,SLOT)))/2.0
                END IF
              END IF
              MX0 = MR * COSD(MA)
              MY0 = MR * SIND(MA)
            END IF
          ELSE
            MX0 = SIGN( (REAL(IX-1)+0.5)*SM_R_DX(SLOT), X0 )
            MY0 = SIGN( (REAL(IY-1)+0.5)*SM_R_DY(SLOT), Y0 )
          END IF

*        Get psf data
          CALL PSF_2D_DATA_INT( %VAL(L_MOD_D(MID,LID)), SLOT,
     :                          MX0, MY0, 0.0, 0.0, DX, DY, INTEG,
     :                          SM_NX(SLOT), SM_NY(SLOT),
     :                          %VAL(PDATA), STATUS )
          PSF_STATE = .TRUE.
          CALL ARR_COP1B( 1, PSF_STATE, %VAL(SM_FLAG(SLOT)
     :                                         +MODI-1), STATUS )

        END IF

*      Copy data to user array
        CALL PSF_2D_DATA_COPY( SM_NX(SLOT), SM_NY(SLOT),
     :                         %VAL(PDATA), NX, NY, ARRAY, STATUS )

*    Get simple psf data
      ELSE

        CALL PSF_2D_DATA_INT( %VAL(L_MOD_D(MID,LID)), SLOT, X0, Y0, QX,
     :                       QY, DX, DY, INTEG, NX, NY, ARRAY, STATUS )

      END IF

      END




*+  PSF_2D_DATA_INT - Grab 2D PSF data using routine specified by PSF_ROUTINE
      SUBROUTINE PSF_2D_DATA_INT( PSF_ROUTINE, SLOT, X0, Y0, QX, QY, DX,
     :                                 DY, INTEG, NX,NY, ARRAY, STATUS )
*
*    Deficiencies :
*
*     Internal routine for PSF_2D_DATA_INT. Calls psf routine in shareable
*     image. If the psf is energy modelled then each energy slice is called
*     in turn, the psf data being coadded.
*
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Oct 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      EXTERNAL                 PSF_ROUTINE             ! Action routine
      INTEGER                  SLOT                    ! PSF to use
      REAL                     X0, Y0                  ! Evaluate psf here
      REAL                     QX,QY                   ! Offset to ARRAY centre
      REAL                     DX, DY                  ! Pixel size in radians
      INTEGER                  NX,NY                   ! Size of array
      LOGICAL                  INTEG                   ! Integrate probability?
*
*    Export :
*
      REAL                     ARRAY(*)                ! The probability array
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  A                       ! Loop over ARRAY values
      INTEGER                  BPTR                    ! Channel bin centres
      INTEGER                  I                       ! Loop over energy bins
      INTEGER                  MAPPTR                  ! Workspace ptr
      INTEGER                  MAPSIZ                  ! Size of workspace

      LOGICAL                  FIRST                   ! First time through?
*
*    Local data :
*
      SAVE                     FIRST,MAPSIZ
      DATA                     FIRST/.TRUE./
*-

*    Energy modelling?
      IF ( EM_OK(SLOT) ) THEN

*      Pointer to channel bin centres
        BPTR = EM_CBPTR(SLOT)

*      More than one energy bin?
        IF ( EM_NBIN(SLOT) .GT. 1 ) THEN

*        First psf to be modelled?
          IF ( FIRST ) THEN
            FIRST = .FALSE.
            MAPSIZ = 0
          END IF

*        Is memory requirement greater than buffer?
          IF ( NX*NY .GT. MAPSIZ ) THEN

*          Unmap existing buffer
            IF ( MAPSIZ .GT. 0 ) THEN
              CALL DYN_UNMAP( MAPPTR, STATUS )
            END IF

*          Map a new buffer
            MAPSIZ = NX*NY
            CALL DYN_MAPR( 1, MAPSIZ, MAPPTR, STATUS )

*          Loop over energy bins. First one goes into the output ARRAY,
*          subsequent ones into workspace which is then added to ARRAY.
            DO I = 1, EM_NBIN(SLOT)

*            Define the energy band
              CALL PSF_DEF( SLOT, 0.0D0, 0.0D0, %VAL(BPTR), %VAL(BPTR),
     :                                                   0, 0, STATUS )

*            Get data
              IF ( I .EQ. 1 ) THEN
                CALL PSF_ROUTINE( SLOT, X0, Y0, QX, QY, DX, DY, INTEG,
     :                                         NX, NY, ARRAY, STATUS )
              ELSE
                CALL PSF_ROUTINE( SLOT, X0, Y0, QX, QY, DX, DY, INTEG,
     :                                  NX, NY, %VAL(MAPPTR), STATUS )
                CALL PSF_2D_DATA_EADD( NX*NY, %VAL(MAPPTR), ARRAY,
     :                                                    STATUS )
              END IF

*            Next energy band
              BPTR = BPTR + VAL__NBI

            END DO

*          Normalise by number of energy bins
            DO A = 1, NX*NY
              ARRAY(A) = ARRAY(A) / EM_NBIN(SLOT)
            END DO

          END IF

*      For single energy bin simply call the definition routine and then
*      the data routine
        ELSE

*        Define E band
          CALL PSF_DEF( SLOT, 0.0D0, 0.0D0, %VAL(BPTR), %VAL(BPTR),
     :                                               0, 0, STATUS )

*        Get data
          CALL PSF_ROUTINE( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
     :                                                   ARRAY, STATUS )

        END IF

      ELSE
        CALL PSF_ROUTINE( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
     :                                                 ARRAY, STATUS )

      END IF

      END



*+  PSF_2D_DATA_COPY - Copy psf data from model store to user array
      SUBROUTINE PSF_2D_DATA_COPY( INX, INY, IN, ONX, ONY, OUT, STATUS )
*
*    Deficiencies :
*
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Oct 89 : Original (DJA)
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
      INTEGER                  INX, INY                ! Model dimensions
      REAL                     IN(INX,INY)             ! Model psf
      INTEGER                  ONX, ONY                ! User psf dimensions
*
*    Export :
*
      REAL                     OUT(ONX,ONY)            ! User psf
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  I,J                     ! Copying loops
      INTEGER                  XB,YB                   ! Border in pixels
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Find border in each dimension
        XB = (INX-ONX)/2
        YB = (INY-ONY)/2

*      Copy data
        DO J = 1, ONY
          DO I = 1, ONX
            OUT(I,J) = IN(I+XB,J+YB)
          END DO
        END DO

      END IF

      END



*+  PSF_2D_DATA_EADD - Add an energy band psf to the accumulator
      SUBROUTINE PSF_2D_DATA_EADD( N, BAND, ACC, STATUS )
*
*    Deficiencies :
*
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     29 Oct 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                  N                       ! Psf length
      REAL                     BAND(N)                 ! Energy band
*
*    Import / export :
*
      REAL                     ACC(N)                  ! Accumulator
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  I                       ! Copying loops
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

        DO I = 1, N
          ACC(I) = ACC(I) + BAND(I)
        END DO

      END IF

      END
