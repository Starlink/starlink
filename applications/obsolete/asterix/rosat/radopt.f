*+  RADOPT - Finds radius of source box which maximises S/N ratio
      SUBROUTINE RADOPT( STATUS )
*    Description :
*       Finds radius of source box which maximises S/N ratio. This
*       works for either the XRT or WFC
*    Environment parameters :
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     28-May-1991   original
*      8-Dec-1994 Updated for UNIX (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*
*    Local variables :
      REAL RADIUS,MAXFRAC,AZ,EL,ENERGY,NS,NB
      DOUBLE PRECISION DMJD                    ! Modified Julian date
      INTEGER IFILT                             ! Filter no. (WFC only)
      CHARACTER*40 INSTR
      LOGICAL DISP
*
*    Version :
*
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'RADOPT  version 1.8-0')
*-

*    Version
      CALL MSG_PRNT(VERSION)

*    Initialise ASTERIX
      CALL AST_INIT()

*    Get name of instrument
      CALL USI_GET0C( 'INSTR', INSTR, STATUS )

*    Get number of source counts
      CALL USI_GET0R('SCOUNT', NS, STATUS)

*    Get background counts per square arcmin
      CALL USI_GET0R('BCOUNT', NB, STATUS)

*    Get X offset from field centre in arcmin
      CALL USI_GET0R('AZIMUTH', AZ, STATUS)

*    Get Y offset from field centre in arcmin
      CALL USI_GET0R('ELEV', EL, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Get energy if XRT
      IF (INDEX(INSTR, 'XRT') .NE. 0) THEN

        CALL USI_GET0R('ENERGY', ENERGY, STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 99

*      Calculate the S/N ratio for these input values
        CALL RADOPT_XRTSNMAX(NS, NB, ENERGY, AZ, EL, RADIUS, MAXFRAC)

      ELSE
*
         CALL USI_GET0D('DMJD', DMJD, STATUS)
         CALL USI_GET0I('FILTER', IFILT, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 99
*
         CALL RADOPT_WFCSNMAX(NS, NB, AZ, EL, DMJD, IFILT,
     &                          RADIUS, MAXFRAC, STATUS)
*
      ENDIF

*    Display the result ?
      CALL USI_GET0L('DISP', DISP, STATUS)
      IF ( DISP ) THEN
        CALL MSG_SETR( 'R', RADIUS )
        CALL MSG_PRNT( 'Optimum radius : ^R arcmin' )
        CALL MSG_SETR( 'F', MAXFRAC )
        CALL MSG_PRNT( 'Enclosed fraction : ^F' )

      END IF

*    Write the result to output params.
      CALL USI_PUT0R('RADIUS', RADIUS, STATUS)
      CALL USI_PUT0R('MAXFRAC', MAXFRAC, STATUS)

*    Exit point
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
**************************************************************
*+RADOPT_XRTSNMAX   -  Finds radius of source box which maximises S/N ratio
      SUBROUTINE RADOPT_XRTSNMAX(NS, NB, ENERGY,
     &                              AZ, EL, RADIUS, MAXFRAC)
*    Description :
*      Calculates the source box radius which optimises the signal to
*      noise ratio given a background count rate of NB c/pixel,
*      and a source count rate of NS. This version works on the XRT.
*    History :
*      Author:  Richard Saxton  15-Dec-1990     (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      REAL NS              ! Number of counts from source
      REAL NB              ! Counts/arcmin from background
      REAL ENERGY          ! Mean photon energy
      REAL AZ,EL           ! Source position (arcmin)
*
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      REAL RADIUS          ! Optimum radius (arcmin)
      REAL MAXFRAC         ! Fraction of counts wihtin max SNR box.
*    Local constants :
      REAL PI
        PARAMETER (PI=3.1415926535)
*    Local variables :
      REAL TRAD            ! Test radius
      INTEGER RLP          ! Loop counter
      INTEGER MAXLP        ! Loop value at maximum
      REAL SNR             ! S/N at a given radius
      REAL SNRMAX          ! Max S/N
      REAL FRAC            ! Fraction of counts within given radius
      REAL RAZ,REL
      REAL DAZL,DAZH
      REAL DELL,DELH
*-
* Initialise variable
      SNRMAX=0.0
*
* Loop over test radii between 0.0 and 20.0 arcminutes in hundredth arcmin
* bins
      DO RLP=1,2000
*
         TRAD = RLP / 100.0
*
* Calculate the fraction of source counts within a circle of radius TRAD
         CALL XRT_PFRAC(ENERGY, AZ, EL, TRAD, FRAC)
*
* Calculate the signal to noise ratio
         SNR = NS * FRAC /
     &                SQRT( NS * FRAC + PI * 2.0 * NB * TRAD * TRAD )
*
* Update the maximum
         IF (SNR .GT. SNRMAX) THEN
            SNRMAX = SNR
            MAXLP = RLP
            MAXFRAC = FRAC
         ENDIF
*
      ENDDO
*
      RADIUS = REAL(MAXLP) / 100.0
*
      END


*+  RADOPT_WFCSNMAX - Finds radius of source box which maximises S/N ratio
      SUBROUTINE RADOPT_WFCSNMAX(NS, NB, AZ, EL,
     &                          DMJD, IFILT, RADIUS, MAXFRAC, STATUS)
*    Description :
*      Calculates the source box radius which optimises the signal to
*      noise ratio given a background count rate of NB c/pixel,
*      and a source count rate of NS. This version works for WFC data.
*    History :
*      Author:  Richard Saxton  15-Dec-1990     (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      REAL NS               ! Number of counts from source
      REAL NB               ! Counts/pixel from background
      REAL AZ,EL            ! Source position (arcmin)
      DOUBLE PRECISION DMJD ! MJD of observation
      INTEGER IFILT         ! Filter no (WFC only)
*
*    Export :
*
      REAL RADIUS          ! Optimum radius (arcmin)
      REAL MAXFRAC         ! Fraction of counts wihtin max SNR box.
*    Functions :
      REAL CAL_PSFT
      EXTERNAL CAL_PSFT
*    Local constants :
      DOUBLE PRECISION PI
        PARAMETER (PI=3.1415926535)
      DOUBLE PRECISION AM2RAD
        PARAMETER ( AM2RAD=PI/(180.0*60.0) )
*    Local variables :
      REAL TRAD            ! Test radius
      INTEGER RLP          ! Loop counter
      INTEGER ISTAT        ! Status variable
      INTEGER MAXLP        ! Loop value when maximum found
      REAL SNRMAX          ! Max S/N
      REAL SNR             ! S/N at a given angle
      REAL FRAC            ! Fraction of counts within given radius
      REAL ENERGY          ! Mean photon energy
      REAL RAZ,REL
      REAL DAZL,DAZH
      REAL DELL,DELH
*-
* Initialise variable
      SNRMAX=0.0
*
* Convert units to radians
      RAZ = AZ * AM2RAD
      REL = EL * AM2RAD
*
* Generate the mean energy from the filter no.
*  UVCAL
      IF (IFILT .EQ. 1) THEN
         ENERGY=0.1
*  S1
      ELSEIF (IFILT .EQ. 4 .OR. IFILT .EQ. 8) THEN
         ENERGY=0.15
*  S2
      ELSEIF (IFILT .EQ. 2 .OR. IFILT .EQ. 6) THEN
         ENERGY=0.09
*  P1
      ELSEIF (IFILT .EQ. 3) THEN
         ENERGY=0.065
*  P2
      ELSEIF (IFILT .EQ. 7) THEN
         ENERGY=0.02
      ENDIF

*    Get energy value
      CALL USI_DEF0R('ENERGY', ENERGY, STATUS)
      CALL USI_GET0R('ENERGY', ENERGY, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99
*
* Convert energy to ev
      ENERGY = ENERGY * 1000.
*
* Loop over test radii between 0.0 and 20.0 arcminutes in tenth arcmin
* bins
      DO RLP=1,200
*
         TRAD = RLP / 10.0
*
* Calculate the fraction of source counts within a circle of radius TRAD
         DAZL = - TRAD * AM2RAD
         DAZH = TRAD * AM2RAD
         DELL = DAZL
         DELH = DAZH
*
         FRAC = CAL_PSFT(DMJD, IFILT, ENERGY, RAZ, REL, DAZL, DAZH,
     &                                            DELL, DELH, ISTAT)
*
         IF (ISTAT .NE. 0) THEN
            CALL MSG_PRNT('Error calculating point spread fraction')
            GOTO 99
         ENDIF
*
* Calculate the signal to noise ratio
         SNR = NS * FRAC /
     &                SQRT( NS * FRAC + PI * 2.0 * NB * TRAD * TRAD )
*
* Update the maximum
         IF (SNR .GT. SNRMAX) THEN
            SNRMAX = SNR
            MAXLP = RLP
            MAXFRAC = FRAC
*
* Assume that there are no local minima
         ELSE
            GOTO 100
         ENDIF
*
      ENDDO
*
100   CONTINUE
      RADIUS = MAXLP / 10.0

 99   CONTINUE

      END
