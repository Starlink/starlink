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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*80             TEXT               ! Output buffer

      INTEGER                  ILIB               ! Loop over libraries
      INTEGER                  IMOD               ! Loop over modules
      INTEGER                  IPSF               ! Psf column
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Display header for table
      CALL MSG_PRNT( 'PSF system options : ' )
      CALL MSG_PRNT( ' ' )

*    List the psfs available
      IPSF = 1
      DO ILIB = 1, L_NLIB

*      Loop over psfs in library
        DO IMOD = 1, L_NMOD(ILIB)

*        Clear buffer if first column
          IF ( IPSF .EQ. 1 ) CALL CHR_FILL( ' ', TEXT )

*        Write psf to buffer
          TEXT(3+(IPSF-1)*15:) = L_MODN(IMOD,ILIB)(1:15)

*        Next column
          IF ( IPSF .EQ. 5 ) THEN
            IPSF = 1
            CALL MSG_PRNT( TEXT(1:79) )
          ELSE
            IPSF = IPSF + 1
          END IF

        END DO

      END DO

*    Flush buffer
      IF ( IPSF .NE. 1 ) THEN
        CALL MSG_PRNT( TEXT(1:79) )
      END IF

*    Plus line for models
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
