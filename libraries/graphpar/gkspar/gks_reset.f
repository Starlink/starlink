      SUBROUTINE GKS_RESET ( WKID, STATUS )
*  Name:
*     GKS_RESET

*  Purpose:
*     Reset GKS Workstation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_RESET ( WKID, STATUS )

*  Description:
*     Reset the graphics device associated with the Workstation.

*  Arguments:
*     WKID = INTEGER (Given and Returned)
*         A variable containing the graphics descriptor.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     Check that the descriptor is valid, and is active.
*     Close the Workstation.
*     Zero the given WKID.

*  Authors:
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1983 (SLW):
*        Starlink Version.
*     06-MAR-1985 (BDK):
*        ADAM version.
*     26-FEB-1986 (AJC):
*        Correct error reporting.
*        Shorten prologue lines.
*     16-JAN-1992 (DLT):
*        Reformat Comments
*        Change names of internal routines from GKS_ to GKS1_.
*        Include par_par.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'     ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'     ! PAR Symbolic Constants
      INCLUDE 'GKS_PAR'     ! GKS Symbolic Constants
      INCLUDE 'gksenv_par'                ! GKS Environment Symbolic Constants

*  Import-Export:
      INTEGER WKID                      ! physical graphics device descriptor
*  Status:
      INTEGER STATUS                    ! status return

*  Global variables:
      INCLUDE 'gkspa_cmn'               ! GKS Parameter Table

*    Local variables :
      INTEGER RGD                       ! relative graphics descriptor
      INTEGER ISTAT                     ! local status value
      INTEGER ERRIND                    ! Inquiry error indicator
      INTEGER IFACT                     ! If workstation active
*-

      ISTAT = STATUS
      STATUS = SAI__OK

*   Look-up the relative graphics descriptor
      CALL GKS1_CHKID ( WKID, RGD, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( PDESC(RGD) .NE. 0 ) THEN

*         Clear Workstation Surface
            CALL GCLRWK( WKID )

*         See if Workstation is active
            CALL GQWKS (WKID, ERRIND, IFACT)
            IF (IFACT.EQ.GACTIV) CALL GDAWK( WKID )

*         Close Workstation
            CALL GCLWK( WKID )
            CALL GKS_GSTAT(STATUS)
            WKID = 0

*          Set current state
            PTNAME(RGD) = ' '
            PDESC(RGD) = 0
            PFREE(RGD) = .TRUE.

         ENDIF

      ENDIF

      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = ISTAT
      ENDIF

      END
