*+  PSF_SLOTINIT - Initialise slot pointed to by SLOT
      SUBROUTINE PSF_SLOTINIT( SLOT, STATUS )
*
*    Author :
*
*     David J. Allan (ROSAT,University of Birmingham)
*
*    History :
*
*      1 Nov 1989 (DJA):
*        Original version
*     28 Oct 1992 (DJA):
*        Changes to accomodate spectral model options
*     28 Jun 1993 (DJA):
*        Generalise axis access
*     15 Dec 1993 (DJA):
*        Use internal PSF1_ routines to access axis data. Finds
*        number of radial model bins properly
*     23 Dec 1993 (DJA):
*        Added search for hint routine
*     25 Apr 1995 (DJA):
*        Use new data interfaces
*      1 May 1996 (DJA):
*        New method interface
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
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      INTEGER          		SLOT                   	! Psf slot
*
*    Local variables :
*
      CHARACTER*15		TAG			! Psf tag name

      REAL             		MAXR                   	! Max radius of image corner
      REAL             		X_BR, Y_BR	      	! Axis bases
      REAL             		X_DR, Y_DR	      	! Axis bin widths
      REAL             		X_TOR, Y_TOR	      	! Axis conversion factors
      REAL             		XLO, XHI, YLO, YHI     	! Axis extrema

      INTEGER          		I                      	! Loop over radial model bins
      INTEGER			RTNPTR			! Init routine ptr
      INTEGER          		X_AX,Y_AX,E_AX,T_AX    	! Axis identifiers
      INTEGER          		X_DIM, Y_DIM           	! Axis dimensions
      INTEGER          		X_PTR, Y_PTR           	! Axis data pointers

      LOGICAL          		X_REG, Y_REG	      	! Axes regular
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Allocate internal storage
      CALL PSF1_ALLOC( P_INST(SLOT), STATUS )

*  Get axis data from dataset
      CALL PSF_CHKAXES( SLOT, STATUS )

*  Identify axes
      CALL PSF_QAXES( SLOT, X_AX, Y_AX, E_AX, T_AX, STATUS )

*  Update model arguments if needed
      IF ( P_MODEL(SLOT) ) THEN

*    Extract axis data from internal storage
        CALL PSF1_GETAXVAL( P_INST(SLOT), X_AX, X_DIM, X_REG, X_PTR,
     :                      X_BR, X_DR, X_TOR, STATUS )
        CALL PSF1_GETAXVAL( P_INST(SLOT), Y_AX, Y_DIM, Y_REG, Y_PTR,
     :                      Y_BR, Y_DR, Y_TOR, STATUS )

*    Polar model?
        IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN

*      Regular radial bins?
          IF ( SM_P_REG(SLOT) ) THEN

*        Convert model args to radians
            SM_P_DR(SLOT) = ABS(SM_P_DR(SLOT) * X_TOR)

*        Find maximum radius of image corners (with a half-pixel margin),
*        in radians from the field centre
            XLO = X_BR - X_DR
            XHI = XLO + (X_DIM+1)*X_DR
            YLO = Y_BR - Y_DR
            YHI = YLO + (Y_DIM+1)*Y_DR
            MAXR = SQRT(MAX( XLO*XLO + YLO*YLO, XLO*XLO + YHI*YHI,
     :                    XHI*XHI + YLO*YLO, XHI*XHI + YHI*YHI ) )

*        Find number of radial bins
            SM_P_NR(SLOT) = INT( MAXR/SM_P_DR(SLOT) ) + 1

          ELSE

*        Convert to radian**2
            DO I = 1, SM_P_NR(SLOT)
              SM_P_RUP(I,SLOT) = SM_P_RUP(I,SLOT) * (X_TOR**2)
            END DO

          END IF

*      Hence number of polar bins
          SM_NMOD(SLOT) = SM_P_NR(SLOT) * SM_P_NA(SLOT)

*    Assume rectangular
        ELSE

*      Convert bin widths to radians
          SM_R_DX(SLOT) = SM_R_DX(SLOT) * X_TOR
          SM_R_DY(SLOT) = SM_R_DY(SLOT) * Y_TOR

*      Numbers of bins in X and Y axes
          SM_R_NX(SLOT) = INT( X_DIM * X_DR/ SM_R_DX(SLOT) )
          SM_R_NY(SLOT) = INT( Y_DIM * Y_DR/ SM_R_DY(SLOT) )

*      Hence number of rectangular bins
          SM_NMOD(SLOT)  = MAX(1,SM_R_NX(SLOT)*SM_R_NY(SLOT) )

        END IF

      END IF

*  Call T/E definition if energy modelling OR energy data available. This
*  acts as flag to library routines that energy information will be passed
*  from above, and so prevents prompts like "mean photon energy" from
*  appearing
      IF ( EM_OK(SLOT) .OR. (E_AX .GT. 0) ) THEN
        CALL PSF_DEF( SLOT, 0.0D0, 0.0D0, 1, 1, 0, 0, STATUS )
      END IF

*  Call the initialisation routine depending on the tag
      CALL ADI_CGET0C( P_PSID(SLOT), 'Tag', TAG, STATUS )
      CALL ADI_CGET0I( P_PLIST, TAG, RTNPTR, STATUS )
      CALL PSF_INIT_EXEC( %VAL(RTNPTR), P_PSID(SLOT), P_FID(SLOT),
     :                    P_INST(SLOT), STATUS )

*  Abort point
 99   CONTINUE

      END



*+  PSF_INIT_EXEC - Call initialisation routine for a given slot
      SUBROUTINE PSF_INIT_EXEC( ROUTINE, PSID, FID, INST, STATUS )
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
*
*    Import :
*
      EXTERNAL			ROUTINE			! Psf initialiser
      INTEGER			PSID
      INTEGER			FID			! Dataset id
      INTEGER          		INST                   	! Internal data
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke initialisation routine
      CALL ROUTINE( PSID, FID, INST, STATUS )

      END
