*+  PSF_2D_DATA - Grab 2D PSF data using routine specified by PSID.
      SUBROUTINE PSF_2D_DATA( PSID, X0, Y0, QX, QY, ADX, ADY, INTEG, NX,
     :                                               NY, ARRAY, STATUS )
*
*    Deficiencies :
*
*     Returns 2D array of psf probability using the psf defined by PSID.
*     See PROG_011 for a complete definition of the arguments. The two
*     arguments ADX and ADY may be defaulted to the defined pixel size by
*     settting their value to VAL__BADR.
*
*    Method :
*
*     The spatial part of the psf pointed to by PSID may be either a
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
*     30 Oct 1989 (DJA):
*        Original version
*     29 Oct 1992 (DJA):
*        Spectral model functionality
*      5 Jan 1994 (DJA):
*        Added defaulting of DX and DY
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER                  PSID                    ! PSF to use
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
      REAL                      PSF0_GETAXDR
*
*    Local variables :
*
      REAL			GDX, GDR, GDY		! Model grid spacing
      REAL                      DX, DY                  ! Pixel size in radians
      REAL                      MA, MR                  ! Centre of polar bin
      REAL                      MX0, MY0                ! Centre of model psf
      REAL                      R2                      ! Radius squared
      REAL			RUP(PGRID_MAXR)

      INTEGER                   IA, IR, IX, IY          ! Model bin indices
      INTEGER			MDATA, MFLAG		! Model data
      INTEGER                   MODI                    ! Model index
      INTEGER			GNA, GNR, GNX, GNY      ! Grid dimensions
      INTEGER			NELM			! Model # elements
      INTEGER			NTOT			! Model # elements
      INTEGER                   PDATA                   ! Model psf data ptr
      INTEGER		        RTNPTR			! Data routine
      INTEGER			SMTYPE			! Model type
      INTEGER                  	X_AX, Y_AX, E_AX, T_AX  ! Axis identifiers

      LOGICAL			GOTDATA			! Got data space?
      LOGICAL			ISMODEL			! Model psf?
      LOGICAL                  	PSF_STATE               ! Model accessed yet?
      LOGICAL			SMREG			! Regular model?
      LOGICAL			THERE			! Routine exists?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get data routine
      CALL PSF0_FNDRTN( PSID, 'Data', THERE, RTNPTR, STATUS )
      IF ( .NOT. THERE ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Psf has no data routine!', STATUS )
        GOTO 99
      END IF

*  Copy ADX and ADY to locals. Check for defaults
      DX = ADX
      DY = ADY
      IF ( (DX .EQ. VAL__BADR) .OR. (DY.EQ.VAL__BADR) ) THEN
        CALL PSF_QAXES( PSID, X_AX, Y_AX, E_AX, T_AX, STATUS )
        IF ( DX .EQ. VAL__BADR ) THEN
          DX = PSF0_GETAXDR( PSID, X_AX, STATUS )
        END IF
        IF ( DY .EQ. VAL__BADR ) THEN
          DY = PSF0_GETAXDR( PSID, Y_AX, STATUS )
        END IF
      END IF

*  Is this model with satisfactory arguments
      CALL ADI_CGET0L( PSID, 'IsModel', ISMODEL, STATUS )
      IF ( ISMODEL .AND.
     :     (QX.EQ.0.0).AND.(QY.EQ.0.0).AND.
     :     (NY.GT.1) ) THEN

*    Is the size of ARRAY in-consistent with with the model
*    definition? This will be either because a different size
*    psf is being requested, or because its the first access
        CALL ADI_CGET0I( PSID, 'ModelNelm', NELM, STATUS )
        CALL ADI_CGET0I( PSID, 'ModelNtot', NTOT, STATUS )
        IF ( NX*NY .GT. NELM ) THEN

*      Free the current model data if present. If none present, it
*      must be the first allocation so grab the flags array too.
          CALL ADI_CGET0L( PSID, 'ModelDataOk', GOTDATA, STATUS )
          IF ( GOTDATA ) THEN
            CALL ADI_CGET0I( PSID, 'ModelData', MDATA, STATUS )
            CALL DYN_UNMAP( MDATA, STATUS )
            CALL ADI_CGET0I( PSID, 'ModelFlag', MFLAG, STATUS )
            GOTDATA = .FALSE.
          ELSE
            CALL DYN_MAPL( 1, NTOT, MFLAG, STATUS )
            CALL ADI_CPUT0I( PSID, 'ModelFlag', MFLAG, STATUS )
          END IF

*      Define new model size
          CALL ADI_CPUT0I( PSID, 'ModelNx', NX, STATUS )
          CALL ADI_CPUT0I( PSID, 'ModelNy', NY, STATUS )
          NELM = NX*NY
          CALL ADI_CPUT0I( PSID, 'ModelNelm', NELM, STATUS )

*      Allocate new memory
          IF ( .NOT. GOTDATA ) THEN
            CALL DYN_MAPR( 1, NELM*NTOT, MDATA, STATUS )
            CALL ADI_CPUT0I( PSID, 'ModelData', MDATA, STATUS )
            CALL ADI_CPUT0L( PSID, 'ModelDataOk', .TRUE., STATUS )
          END IF

*      Clear the flags
          CALL ARR_INIT1L( .FALSE., NTOT, %VAL(MFLAG), STATUS )

        ELSE
          CALL ADI_CGET0I( PSID, 'ModelData', MDATA, STATUS )
          CALL ADI_CGET0I( PSID, 'ModelFlag', MFLAG, STATUS )

        END IF

*    Decide on the model index for this image position. Polar grid
        CALL ADI_CGET0I( PSID, 'ModelType', SMTYPE, STATUS )
        IF ( SMTYPE .EQ. PSF_PGRID ) THEN

*      Regular bins?
          CALL ADI_CGET0L( PSID, 'ModelReg', SMREG, STATUS )

*      Find radial bin
          R2 = X0*X0+Y0*Y0
          IF ( SMREG ) THEN
            CALL ADI_CGET0R( PSID, 'ModelDr', GDR, STATUS )
            CALL ADI_CGET0I( PSID, 'ModelNr', GNR, STATUS )
            IR = (SQRT(R2)/GDR) + 1
          ELSE
            CALL ADI_CGET1R( PSID, 'ModelRup', PGRID_MAXR, RUP, GNR,
     :                       STATUS )
            IR = 1
            DO WHILE ( R2 .GT. RUP(IR) )
              IR = IR + 1
            END DO
          END IF
          CALL ADI_CGET0I( PSID, 'ModelNaz', GNA, STATUS )

*      Find azimuthal bin. Only handle one azimuthal bin for central
*      radial bin.
          IF ( (IR.EQ.1) .OR. (GNA.EQ.1) ) THEN
            IA = 1
          ELSE
            MA = ATAN2(Y0,X0)*MATH__RTOD+180.0
            IA = INT( MA*GNA/360.0) + 1
          END IF

*      Compute model index from azimuthal radial bins
          MODI = (IA-1)*GNR + IR

*    Rectangular grid
        ELSE IF ( SMTYPE .EQ. PSF_RGRID ) THEN

*      Compute model index from azimuthal and altitudinal bins
          CALL ADI_CGET0R( PSID, 'ModelDx', GDX, STATUS )
          CALL ADI_CGET0R( PSID, 'ModelDy', GDY, STATUS )
          CALL ADI_CGET0I( PSID, 'ModelNx', GNX, STATUS )
          CALL ADI_CGET0I( PSID, 'ModelNy', GNY, STATUS )
          IX = INT(ABS(X0/GDX)) + 1
          IY = INT(ABS(Y0/GDY)) + 1
          MODI = (IY-1)*GNY + IX

        ELSE
          STATUS = SAI__ERROR
        END IF

*    Is the flag set for this model index
        CALL ARR_ELEM1L( MFLAG, NTOT, MODI, PSF_STATE, STATUS )

*    If the psf hasn't been access yet, do so now
        PDATA = MDATA + (MODI-1)*NELM*VAL__NBR
        IF ( .NOT. PSF_STATE ) THEN

*      Decide centre of model psf bin
          IF ( SMTYPE .EQ. PSF_PGRID ) THEN
            IF ( IR .EQ. 1 ) THEN
              MX0 = 0.0
              MY0 = 0.0
            ELSE
              IF ( IA .EQ. 1 ) THEN
                MA = 0.0
              ELSE
                MA = (REAL(IA-1)+0.5)*360.0/REAL(GNA)
              END IF

              IF ( SMREG ) THEN
                MR = (REAL(IR-1)+0.5)*GDR
              ELSE

*            Trap case of infinite irregular radius, and peg to last
*            specified radius.
                IF ( IR .EQ. GNR ) THEN
                  MR = SQRT( RUP(IR-1) )
                ELSE
                  MR = (SQRT(RUP(IR))-SQRT(RUP(IR-1)))/2.0
                END IF
              END IF
              MX0 = MR * COS(MA*MATH__DTOR)
              MY0 = MR * SIN(MA*MATH__DTOR)
            END IF
          ELSE
            MX0 = SIGN( (REAL(IX-1)+0.5)*GDX, X0 )
            MY0 = SIGN( (REAL(IY-1)+0.5)*GDY, Y0 )
          END IF

*      Get psf data
          CALL PSF_2D_DATA_INT( %VAL(RTNPTR), PSID, MX0, MY0,
     :                          0.0, 0.0, DX, DY,
     :                          INTEG, GNX, GNY,
     :                          %VAL(PDATA), STATUS )

*      Mark model slot as filled
          CALL ARR_SELEM1L( MFLAG, NTOT, MODI, .TRUE., STATUS )

        END IF

*    Copy data to user array
        CALL PSF_2D_DATA_COPY( GNX, GNY,
     :                         %VAL(PDATA), NX, NY, ARRAY, STATUS )

*  Get simple psf data
      ELSE

        CALL PSF_2D_DATA_INT( %VAL(RTNPTR), PSID, X0, Y0, QX, QY,
     :                        DX, DY, INTEG, NX, NY, ARRAY, STATUS )

      END IF

*  Abort point
 99   CONTINUE

      END




*+  PSF_2D_DATA_INT - Grab 2D PSF data using routine specified by PSF_ROUTINE
      SUBROUTINE PSF_2D_DATA_INT( PSF_ROUTINE, PSID, X0, Y0,
     :                            QX, QY, DX,
     :                            DY, INTEG, NX,NY, ARRAY, STATUS )
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
      INTEGER                  PSID                    ! PSF to use
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
      INTEGER                   A                       ! Loop over ARRAY values
      INTEGER                   BPTR                    ! Channel bin centres
      INTEGER                   I                       ! Loop over energy bins
      INTEGER                   MAPPTR                  ! Workspace ptr
      INTEGER                   MAPSIZ                  ! Size of workspace
      INTEGER			NBIN			! # energy bins

      LOGICAL			EMOK			! Energy modelling on?
      LOGICAL                   FIRST                   ! First time through?
*
*    Local data :
*
      SAVE                     FIRST,MAPSIZ
      DATA                     FIRST/.TRUE./
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Energy modelling?
      CALL ADI_CGET0L( PSID, 'IsEnergyModel', EMOK, STATUS )
      IF ( EMOK ) THEN

*    Pointer to channel bin centres
        CALL ADI_CGET0I( PSID, 'NemBnds', BPTR, STATUS )
        CALL ADI_CGET0I( PSID, 'NemBin', NBIN, STATUS )

*    More than one energy bin?
        IF ( NBIN .GT. 1 ) THEN

*      First psf to be modelled?
          IF ( FIRST ) THEN
            FIRST = .FALSE.
            MAPSIZ = 0
          END IF

*      Is memory requirement greater than buffer?
          IF ( NX*NY .GT. MAPSIZ ) THEN

*        Unmap existing buffer
            IF ( MAPSIZ .GT. 0 ) THEN
              CALL DYN_UNMAP( MAPPTR, STATUS )
            END IF

*        Map a new buffer
            MAPSIZ = NX*NY
            CALL DYN_MAPR( 1, MAPSIZ, MAPPTR, STATUS )

*        Loop over energy bins. First one goes into the output ARRAY,
*        subsequent ones into workspace which is then added to ARRAY.
            DO I = 1, NBIN

*          Define the energy band
              CALL PSF_DEF( PSID, 0.0D0, 0.0D0, %VAL(BPTR), %VAL(BPTR),
     :                                                   0, 0, STATUS )

*          Get data
              IF ( I .EQ. 1 ) THEN
                CALL PSF_ROUTINE( PSID, X0, Y0, QX, QY, DX, DY, INTEG,
     :                                         NX, NY, ARRAY, STATUS )
              ELSE
                CALL PSF_ROUTINE( PSID, X0, Y0, QX, QY, DX, DY, INTEG,
     :                                  NX, NY, %VAL(MAPPTR), STATUS )
                CALL PSF_2D_DATA_EADD( NX*NY, %VAL(MAPPTR), ARRAY,
     :                                                    STATUS )
              END IF

*          Next energy band
              BPTR = BPTR + VAL__NBI

            END DO

*        Normalise by number of energy bins
            DO A = 1, NX*NY
              ARRAY(A) = ARRAY(A) / REAL(NBIN)
            END DO

          END IF

*    For single energy bin simply call the definition routine and then
*    the data routine
        ELSE

*      Define E band
          CALL PSF_DEF( PSID, 0.0D0, 0.0D0, %VAL(BPTR), %VAL(BPTR),
     :                                               0, 0, STATUS )

*      Get data
          CALL PSF_ROUTINE( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
     :                                                   ARRAY, STATUS )

        END IF

*  No energy modelling
      ELSE
        CALL PSF_ROUTINE( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
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
