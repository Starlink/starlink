*+  CREPSF - Creates dataset containing psf values
      SUBROUTINE CREPSF( STATUS )
*
*    Description :
*
*     Create dataset containing psf out to specified radius
*
*    Environment parameters :
*
*     OUT                   = UNIV(W)
*           Output event dataset
*     RADIUS                = INTEGER(R)
*           Radius in pixels
*     PSF                   = CHAR(R)
*           User's selected psf
*     PIXSIZE               = REAL(R)
*           Pixel size in arcmin
*     DX,DY                 = REAL(R)
*           Psf to grid offset
*     X0,Y0                 = REAL(R)
*           Image position for evaluation
*
*    Method :
*
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     25 Jun 90 : V1.2-0  Original (DJA)
*      2 Aug 90 : V1.2-1  Writes psf energy radii to psf structure (DJA)
*     10 Aug 90 : V1.2-2  Allows user to specify a MJD for the dataset (DJA)
*     15 Feb 92 : V1.6-0  Don't bother with energy table. (DJA)
*     10 Mar 92 : V1.6-1  Added X0,Y0 parameters (DJA)
*     15 Dec 92 : V1.7-0  Made units switchable, and added DX,DY parameter (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MATH_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER         OLOC*(DAT__SZLOC)      ! Locator to output dataset
      CHARACTER         PLOC*(DAT__SZLOC)      ! Locator to output psf box
      CHARACTER*20      UNITS                  ! Units to work in

      DOUBLE PRECISION  MJD                    ! MJD for observation

      REAL              DX, DY                 ! Table to psf offset
      REAL              SCALE                  ! Image binsize in arcmin
      REAL              TOR                    ! Units to radian conversion
      REAL              X0, Y0                 ! Image position

      INTEGER           DIMS(2)                ! Image dimensions
      INTEGER           DPTR                   ! Pointer to data
      INTEGER           PSLOT                  ! PSF system slot
      INTEGER           RADIUS                 ! User selected image half width

      LOGICAL           GOT_MJD                ! Got MJD from user
*
*    Version id :
*
      CHARACTER*30      VERSION
         PARAMETER      ( VERSION = 'CREPSF Version 1.7-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT( STATUS )
      CALL PSF_INIT( STATUS )

*    Get output dataset
      CALL USI_ASSOCO( 'OUT', 'PSF_DATA', OLOC, STATUS )

*    Get units to work with
      CALL PAR_GET0C( 'UNITS', UNITS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL CONV_UNIT2R( UNITS, TOR, STATUS )

*    Decide on image size etc.
      CALL PAR_PROMT( 'PIXSIZE', 'Pixel size in '//UNITS, STATUS )
      CALL PAR_GET0R( 'PIXSIZE', SCALE, STATUS )

*    Create axes
      CALL BDA_CREAXES( OLOC, 2, STATUS )
      CALL BDA_PUTAXUNITS( OLOC, 1, UNITS, STATUS )
      CALL BDA_PUTAXUNITS( OLOC, 2, UNITS, STATUS )
      CALL BDA_PUTAXLABEL( OLOC, 1, 'X position', STATUS )
      CALL BDA_PUTAXLABEL( OLOC, 2, 'Y position', STATUS )
      CALL BDA_PUTUNITS( OLOC, 'Probability/pixel', STATUS )

*    Image position
      CALL PAR_GET0R( 'X0', X0, STATUS )
      CALL PAR_GET0R( 'Y0', Y0, STATUS )

*    Convert to radians
      X0 = X0 * TOR
      Y0 = Y0 * TOR

*    Try for an MJD
      CALL PAR_GET0D( 'MJD', MJD, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        GOT_MJD = .FALSE.
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        GOT_MJD = .TRUE.
      ELSE
        GOTO 99
      END IF

*    Create header structure and write various parameters
      CALL BDA_CREHEAD( OLOC, STATUS )
      IF ( GOT_MJD ) THEN
        CALL DAT_NEW0I( OLOC, 'BASE_MJD', STATUS )
        CALL DAT_NEW0D( OLOC, 'BASE_UTC', STATUS )
        CALL CMP_PUT0I( OLOC, 'BASE_MJD', INT(MJD), STATUS )
        CALL CMP_PUT0D( OLOC, 'BASE_UTC', MJD-DBLE(INT(MJD)), STATUS )
      END IF

*    Get size of output psf dataset
      CALL PAR_GET0I( 'RADIUS', RADIUS, STATUS )
      DIMS(1) = RADIUS*2+1
      DIMS(2) = RADIUS*2+1

*    Create axis values
      CALL BDA_PUTAXVAL( OLOC, 1, RADIUS*SCALE, -SCALE, DIMS(1),
     :                                                  STATUS )
      CALL BDA_PUTAXVAL( OLOC, 2, -RADIUS*SCALE, SCALE, DIMS(2),
     :                                                  STATUS )

*    Create PSF structure and associate with PSF system
      CALL PSF_ASSOCO( OLOC, PSLOT, STATUS )

*    Map data
      CALL BDA_CREDATA( OLOC, 2, DIMS, STATUS )
      CALL BDA_MAPDATA( OLOC, 'WRITE', DPTR, STATUS )

*    Get psf <-> grid offsets
      CALL PAR_GET0R( 'DX', DX, STATUS )
      CALL PAR_GET0R( 'DY', DY, STATUS )
      DX = DX * TOR
      DY = DY * TOR

*    And get psf data
      CALL PSF_2D_DATA( PSLOT, X0, Y0, DX, DY, -SCALE*TOR, SCALE*TOR,
     :                 .TRUE., DIMS(1), DIMS(2), %VAL(DPTR), STATUS )

*    Tidy up
 99   CALL PSF_CLOSE( STATUS )
      CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END
