*+  PSF_SLOTINIT - Initialise slot pointed to by SLOT
      SUBROUTINE PSF_SLOTINIT( LOC, SLOT, STATUS )
*
*    Author :
*
*     David J. Allan (ROSAT,University of Birmingham)
*
*    History :
*
*      1 Nov 89 : Original (DJA)
*     28 Oct 92 : Changes to accomodate spectral model options (DJA)
*     28 Jun 93 : Generalise axis access (DJA)
*     15 Dec 93 : Use internal PSF1_ routines to access axis data. Finds
*                 number of radial model bins properly (DJA)
*     23 Dec 93 : Added search for hint routine (DJA)
*
*    Type declarations :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER	       STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      CHARACTER        LOC*(DAT__SZLOC)       ! Dataset locator
      INTEGER          SLOT                   ! Psf slot
*
*    Function :
*
      INTEGER          CHR_LEN
*
*    Local variables :
*
      CHARACTER*80     ROUT

      REAL             MAXR                   ! Max radius of image corner
      REAL             X_BR, Y_BR	      ! Axis bases
      REAL             X_DR, Y_DR	      ! Axis bin widths
      REAL             X_TOR, Y_TOR	      ! Axis conversion factors
      REAL             XLO, XHI, YLO, YHI     ! Axis extrema

      INTEGER          I                      ! Loop over radial model bins
      INTEGER          LID, MID               ! Library and slot
      INTEGER          RLEN                   ! Length of ROUT
      INTEGER          X_AX,Y_AX,E_AX,T_AX    ! Axis identifiers
      INTEGER          X_DIM, Y_DIM           ! Axis dimensions
      INTEGER          X_PTR, Y_PTR           ! Axis data pointers

      LOGICAL          VALID                  ! Good locator?
      LOGICAL          X_REG, Y_REG	      ! Axes regular
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get identifiers
      LID = P_LIBID(SLOT)
      MID = P_MODID(SLOT)

*    Allocate internal storage
      CALL PSF1_ALLOC( P_INST(SLOT), STATUS )

*    Look for axis units if good locator
      CALL DAT_VALID( LOC, VALID, STATUS )
      IF ( VALID ) THEN

*      Get axis data from dataset
        CALL PSF_CHKAXES( SLOT, STATUS )

*      Identify axes
        CALL PSF_QAXES( SLOT, X_AX, Y_AX, E_AX, T_AX, STATUS )

*      Update model arguments if needed
        IF ( P_MODEL(SLOT) ) THEN

*        Extract axis data from internal storage
          CALL PSF1_GETAXVAL( P_INST(SLOT), X_AX, X_DIM, X_REG, X_PTR,
     :                        X_BR, X_DR, X_TOR, STATUS )
          CALL PSF1_GETAXVAL( P_INST(SLOT), Y_AX, Y_DIM, Y_REG, Y_PTR,
     :                        Y_BR, Y_DR, Y_TOR, STATUS )

*        Polar model?
          IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN

*          Regular radial bins?
            IF ( SM_P_REG(SLOT) ) THEN

*            Convert model args to radians
              SM_P_DR(SLOT) = ABS(SM_P_DR(SLOT) * X_TOR)

*            Find maximum radius of image corners (with a half-pixel margin),
*            in radians from the field centre
              XLO = X_BR - X_DR
              XHI = XLO + (X_DIM+1)*X_DR
              YLO = Y_BR - Y_DR
              YHI = YLO + (Y_DIM+1)*Y_DR
              MAXR = SQRT(MAX( XLO*XLO + YLO*YLO, XLO*XLO + YHI*YHI,
     :                    XHI*XHI + YLO*YLO, XHI*XHI + YHI*YHI ) )

*            Find number of radial bins
              SM_P_NR(SLOT) = INT( MAXR/SM_P_DR(SLOT) ) + 1

            ELSE

*            Convert to radian**2
              DO I = 1, SM_P_NR(SLOT)
                SM_P_RUP(I,SLOT) = SM_P_RUP(I,SLOT) * (X_TOR**2)
              END DO

            END IF

*          Hence number of polar bins
            SM_NMOD(SLOT) = SM_P_NR(SLOT) * SM_P_NA(SLOT)

*        Assume rectangular
          ELSE

*          Convert bin widths to radians
            SM_R_DX(SLOT) = SM_R_DX(SLOT) * X_TOR
            SM_R_DY(SLOT) = SM_R_DY(SLOT) * Y_TOR

*          Numbers of bins in X and Y axes
            SM_R_NX(SLOT) = INT( X_DIM * X_DR/ SM_R_DX(SLOT) )
            SM_R_NY(SLOT) = INT( Y_DIM * Y_DR/ SM_R_DY(SLOT) )

*          Hence number of rectangular bins
            SM_NMOD(SLOT)  = MAX(1,SM_R_NX(SLOT)*SM_R_NY(SLOT) )

          END IF

        END IF

      END IF

*    Try to locate the data routine
      ROUT = 'PSF_'//L_MODN(MID,LID)
      RLEN = CHR_LEN(ROUT)
      CALL PSF_FINDRTN( LID, ROUT(:RLEN), L_MOD_D(MID,LID), STATUS )
      IF ( L_MOD_D(MID,LID) .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Couldn''t find PSF data routine'/
     :                               /' in library', STATUS )
        GOTO 99
      END IF

*    Energy profiling routine
      CALL PSF_FINDRTN( LID, ROUT(:RLEN)//'_PFL',
     :                  L_MOD_PFL(MID,LID), STATUS )

*    Time/energy definition routine
      CALL PSF_FINDRTN( LID, ROUT(:RLEN)//'_DEF',
     :                  L_MOD_DEF(MID,LID), STATUS )

*    Hint supplier routine
      CALL PSF_FINDRTN( LID, ROUT(:RLEN)//'_HINT',
     :                  L_MOD_H(MID,LID), STATUS )

*    Must have a definition routine for energy modelling. Turn off modelling
*    if not present.
      IF ( (L_MOD_DEF(MID,LID).EQ.0) .AND. EM_OK(SLOT) ) THEN
        CALL MSG_PRNT( '! Modelled energy dependence is not '/
     :                 /'supported by this psf. The' )
        IF ( EM_MODE(SLOT) .EQ. PSF_E_CHANNEL ) THEN
          CALL MSG_SETC( 'MM', 'channel spectrum' )
        ELSE
          CALL MSG_SETC( 'MM', 'spectral model' )
        END IF
        CALL MSG_PRNT( '  ^MM supplied will be ignored.' )

        EM_OK(SLOT) = .FALSE.
      END IF

*    Initialisation routine
      CALL PSF_FINDRTN( LID, ROUT(:RLEN)//'_INIT',
     :                  L_MOD_I(MID,LID), STATUS )

*    Closure routine
      CALL PSF_FINDRTN( LID, ROUT(:RLEN)//'_CLOSE',
     :                  L_MOD_C(MID,LID), STATUS )

*    Call T/E definition if energy modelling OR energy data available. This
*    acts as flag to library routines that energy information will be passed
*    from above, and so prevents prompts like "mean photon energy" from
*    appearing
      IF ( EM_OK(SLOT) .OR. (E_AX .GT. 0) ) THEN
        CALL PSF_DEF( SLOT, 0.0D0, 0.0D0, 1, 1, 0, 0, STATUS )
      END IF

*    Call the INIT routine if defined
      IF ( L_MOD_I(MID,LID) .NE. 0 ) THEN
        CALL PSF_PSF_INIT_EXEC( %VAL(L_MOD_I(MID,LID)),
     :                          LOC, SLOT, STATUS )
      END IF

 99   CONTINUE

      END



*+  PSF_FINDRTN - Call initialisation routine for a given slot
      SUBROUTINE PSF_FINDRTN( LIBRARY, ROUTINE, PTR, STATUS )
*
*    Authors :
*
*     David J. Allan (ROSAT,University of Birmingham)
*
*    History :
*
*     01 Nov 89 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                     LIBRARY           ! Library to look up
      CHARACTER*(*)               ROUTINE           ! Routine to find
*
*    Import / Export :
*
      INTEGER                     PTR               ! Ptr to routine
*
*    Functions :
*
      INTEGER                     PSF_FINDR
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Already found?
      IF ( PTR .EQ. 0 ) THEN

*      Look it up
        STATUS = PSF_FINDR( ROUTINE, LIBRARY, PTR )

*      If not found, set ptr bad - note that PSF_FINDR doesn't do any error
*      reporting
        IF ( STATUS .NE. SAI__OK ) THEN
          PTR = 0
          STATUS = SAI__OK
        END IF

      END IF

      END



*+  PSF_PSF_INIT_EXEC - Call initialisation routine for a given slot
      SUBROUTINE PSF_PSF_INIT_EXEC( ROUTINE, LOC, SLOT, STATUS )
*
*    Authors :
*
*     David J. Allan (ROSAT,University of Birmingham)
*
*    History :
*
*     01 Nov 89 : Original (DJA)
*     15 Dec 93 : Pass instance data rather than Fortran structure (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      CHARACTER        LOC*(DAT__SZLOC)       ! Dataset locator
      EXTERNAL         ROUTINE                ! Psf initialiser
      INTEGER          SLOT                   ! Psf system slot
*
*    Local variables :
*
      INTEGER          PASS(2)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up arguments
      PASS(1) = SLOT
      PASS(2) = P_INST(SLOT)

*    Invoke initialisation routine
      CALL ROUTINE( PASS, LOC, STATUS )

      END
