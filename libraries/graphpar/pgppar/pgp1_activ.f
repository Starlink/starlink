      SUBROUTINE PGP1_ACTIV ( STATUS )
*+
*  Name:
*     PGP1_ACTIV

*  Purpose:
*     Initialise PGP library

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP1_ACTIV( STATUS )

*  Description:
*     The table of assigned graphics devices in the PGP_IO Common Block
*     is initialised.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Authors:
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*    History :
*     23-JAN-1992 (DLT):
*        Original.

*  Bugs:
*     {note_any_bugs_here}

*  Type Definitions:
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'   ! SAI Symbolic Constants

      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants

      INCLUDE 'pgpenv_par'              ! PGP Environment Symbolic Constants

*    Status return :
      INTEGER STATUS                    ! Status

*    Global variables :
      INCLUDE 'pgppa_cmn'               ! PGP Parameter Table

      INCLUDE 'pgpgo_cmn'               ! PGP Initialisation Switch

      EXTERNAL PGP1_BLK

*    Local variables :
      INTEGER I                         ! loop index
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise common blocks
      DO I = 1, PGP__MXPAR
         PFREE(I) = .TRUE.
         PDESC(I) = 0
         PTNAME(I) = ' '
      ENDDO

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Declare PGP awake
      PGPSLP = .FALSE.

      END
