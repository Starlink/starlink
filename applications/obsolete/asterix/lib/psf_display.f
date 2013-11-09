*+  PSF_DISPLAY - Display list of PSFs available
      SUBROUTINE PSF_DISPLAY( STATUS )
*
*    Description :
*
*     Prints out list of psfs known by system plus the syntax of model
*     specifications.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 90 : Original (DJA)
*     26 Oct 92 : Added CSPEC option (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'AST_PKG'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Status :
*
      INTEGER STATUS

*  External References:
      EXTERNAL                  AST_QPKGI
        LOGICAL                 AST_QPKGI

*  Local Variables:
      CHARACTER*15		NAME			! A psf name
      CHARACTER*80             	TEXT               	! Output buffer

      INTEGER                  	ICMP			! Loop over list
      INTEGER                  	IPSF               	! Psf column
      INTEGER                  	NPSF               	! # psfs
      INTEGER			PID			! A particular psf
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( PSF__PKG ) ) CALL PSF_INIT( STATUS )

*  Display header for table
      CALL MSG_PRNT( 'PSF system options : ' )
      CALL MSG_PRNT( ' ' )

*  Find number of psfs
      CALL ADI_NCMP( P_PLIST, NPSF, STATUS )

*  List them
      IPSF = 1
      DO ICMP = 1, NPSF

*    Index it
        CALL ADI_INDCMP( P_PLIST, ICMP, PID, STATUS )
        CALL ADI_NAME( PID, NAME, STATUS )
        CALL ADI_ERASE( PID, STATUS )

*    Clear buffer if first column
        IF ( IPSF .EQ. 1 ) CALL CHR_FILL( ' ', TEXT )

*    Write psf to buffer
        TEXT(3+(IPSF-1)*15:) = NAME

*    Next column
        IF ( IPSF .EQ. 5 ) THEN
          IPSF = 1
          CALL MSG_PRNT( TEXT(1:79) )
        ELSE
          IPSF = IPSF + 1
        END IF

      END DO

*  Flush buffer
      IF ( IPSF .NE. 1 ) THEN
        CALL MSG_PRNT( TEXT(1:79) )
      END IF

*  Plus line for models
      CALL MSG_PRNT( ' ' )
      CALL MSG_PRNT( '  CSPEC(psf/model,spec[,nbin])' )
      CALL MSG_PRNT( '  POLAR(psf,rbin[,abin])        RECT(psf'/
     :                                        /',xbin[,ybin])' )
      CALL MSG_PRNT( ' ' )

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_DISPLAY', STATUS )
      END IF

      END
