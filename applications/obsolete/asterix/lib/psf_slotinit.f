*+  PSF_SLOTINIT - Initialise slot pointed to by PSID
      SUBROUTINE PSF_SLOTINIT( PSID, STATUS )
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
*  Global Variables:
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      INTEGER          		PSID                   	! Psf slot
*
*    Local variables :
*
      CHARACTER*15		TAG			! Psf tag name

      REAL			DR, DX, DY		! Bin sizes
      REAL             		MAXR                   	! Max radius of image corner
      REAL             		X_BR, Y_BR	      	! Axis bases
      REAL             		X_DR, Y_DR	      	! Axis bin widths
      REAL             		X_TOR, Y_TOR	      	! Axis conversion factors
      REAL             		XLO, XHI, YLO, YHI     	! Axis extrema

      INTEGER			BPTR			! Bin pointers
      INTEGER			NA, NR, NX, NY		! Number of model bins
      INTEGER			NTOT			! Total # model bins
      INTEGER			RTNPTR			! Init routine ptr
      INTEGER			SMTYPE			! Spatial model type
      INTEGER          		X_AX,Y_AX,E_AX,T_AX    	! Axis identifiers
      INTEGER          		X_DIM, Y_DIM           	! Axis dimensions
      INTEGER          		X_PTR, Y_PTR           	! Axis data pointers

      LOGICAL			ISMODEL			! Model psf?
      LOGICAL			SMREG			! Spatial mode regular?
      LOGICAL          		X_REG, Y_REG	      	! Axes regular
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get axis data from dataset
      CALL PSF_CHKAXES( PSID, STATUS )

*  Identify axes
      CALL PSF_QAXES( PSID, X_AX, Y_AX, E_AX, T_AX, STATUS )

*  Update model arguments if needed
      CALL ADI_CGET0L( PSID, 'IsModel', ISMODEL, STATUS )
      IF ( ISMODEL ) THEN

*    Extract axis data from internal storage
        CALL PSF0_GETAXVAL( PSID, X_AX, X_DIM, X_REG, X_PTR,
     :                      X_BR, X_DR, X_TOR, STATUS )
        CALL PSF0_GETAXVAL( PSID, Y_AX, Y_DIM, Y_REG, Y_PTR,
     :                      Y_BR, Y_DR, Y_TOR, STATUS )

*    Extract model info
        CALL ADI_CGET0I( PSID, 'ModelType', SMTYPE, STATUS )

*    Polar model?
        IF ( SMTYPE .EQ. PSF_PGRID ) THEN

*      Regular radial bins?
          CALL ADI_CGET0L( PSID, 'ModelReg', SMREG, STATUS )
          IF ( SMREG ) THEN

*        Convert model args to radians
            CALL ADI_CGET0R( PSID, 'ModelDr', DR, STATUS )
            DR = ABS( DR * X_TOR )
            CALL ADI_CPUT0R( PSID, 'ModelDr', DR, STATUS )

*        Find maximum radius of image corners (with a half-pixel margin),
*        in radians from the field centre
            XLO = X_BR - X_DR
            XHI = XLO + (X_DIM+1)*X_DR
            YLO = Y_BR - Y_DR
            YHI = YLO + (Y_DIM+1)*Y_DR
            MAXR = SQRT(MAX( XLO*XLO + YLO*YLO, XLO*XLO + YHI*YHI,
     :                    XHI*XHI + YLO*YLO, XHI*XHI + YHI*YHI ) )

*        Find number of radial bins
            NR = INT( MAXR/DR ) + 1
            CALL ADI_CPUT0I( PSID, 'ModelNr', NR, STATUS )

          ELSE

*        Convert to radian**2
            CALL ADI_CGET0I( PSID, 'ModelNr', NR, STATUS )
            CALL ADI_CMAPR( PSID, 'ModelRup', 'UPDATE', BPTR, STATUS )
            CALL ARR_MULTR( X_TOR**2, NR, %VAL(BPTR), STATUS )
            CALL ADI_CUNMAP( PSID, 'ModelRup', BPTR, STATUS )

          END IF

*      Find total number of bins
          CALL ADI_CGET0I( PSID, 'ModelNaz', NA, STATUS )
          NTOT = NR * NA

*    Assume rectangular
        ELSE

*      Convert bin widths to radians
          CALL ADI_CGET0R( PSID, 'ModelDx', DX, STATUS )
          CALL ADI_CGET0R( PSID, 'ModelDy', DY, STATUS )
          DX = DX * X_TOR
          DY = DY * Y_TOR
          CALL ADI_CPUT0R( PSID, 'ModelDx', DX, STATUS )
          CALL ADI_CPUT0R( PSID, 'ModelDy', DY, STATUS )

*      Numbers of bins in X and Y axes
          NX = INT( X_DIM * X_DR/ DX )
          NY = INT( Y_DIM * Y_DR/ DY )
          CALL ADI_CPUT0I( PSID, 'ModelNx', NX, STATUS )
          CALL ADI_CPUT0I( PSID, 'ModelNy', NY, STATUS )

*      Hence number of rectangular bins
          NTOT = MAX(1,NX*NY)

        END IF

*    Write total number of bins
        CALL ADI_CPUT0I( PSID, 'ModelNtot', NTOT, STATUS )

      END IF

*  Call T/E definition if energy modelling OR energy data available. This
*  acts as flag to library routines that energy information will be passed
*  from above, and so prevents prompts like "mean photon energy" from
*  appearing
      CALL ADI_CGET0L( PSID, 'IsEnergyModel', ISMODEL, STATUS )
      IF ( ISMODEL .OR. (E_AX .GT. 0) ) THEN
        CALL PSF_DEF( PSID, 0.0D0, 0.0D0, 1, 1, 0, 0, STATUS )
      END IF

*  Call the initialisation routine depending on the tag
      CALL ADI_CGET0C( PSID, 'Tag', TAG, STATUS )
      CALL ADI_CGET0I( P_PLIST, TAG, RTNPTR, STATUS )
      CALL PSF_INIT_EXEC( %VAL(RTNPTR), PSID, STATUS )

      END



*+  PSF_INIT_EXEC - Call initialisation routine for a given slot
      SUBROUTINE PSF_INIT_EXEC( ROUTINE, PSID, STATUS )
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
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke initialisation routine
      CALL ROUTINE( PSID, STATUS )

      END
