      SUBROUTINE EXSPEC( APPLIC, NAXIS1, NAXIS2, DATA_VM, QUAL_VM,
     :                   STATUS )
*+
*  Name:
*     SUBROUTINE EXSPEC

*  Purpose:
*     Extract spectrum from general parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL EXSPEC( APPLIC, NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )

*  Method:
*     The spectrum is extracted from the image using the parameters in
*     CMEXTP as a guide. The TRAK method is used.
*     The pixel subset is stored in dynamic VM.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-SEP-94 (MJC):
*       IUEDR Vn. 3.1-4
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'

*  Arguments Given:
      INTEGER NAXIS1        ! size of axis 1
      INTEGER NAXIS2        ! size of axis 2
      INTEGER DATA_VM       ! VM address for DATA array
      INTEGER QUAL_VM       ! VM address for QUAL array

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      EXTERNAL APPLIC       ! routine to be called

      EXTERNAL RTOW         ! (S,L) to (R,W) coordinate transform
      EXTERNAL WTOR         ! (R,W) to (S,L) coordinate transform

*  Local Variables:
      INTEGER MSUB          ! size of VM arrays
      INTEGER NSUB          ! number of subset pixels
      INTEGER LMN           ! subset start L
      INTEGER LMX           ! subset end L
      INTEGER SMN(4096)     ! subset start S
      INTEGER SMX(4096)     ! subset end S
      INTEGER D_VM          ! address of DATA array
      INTEGER Q_VM          ! address of QUAL array
      INTEGER R_VM          ! address of R array
      INTEGER W_VM          ! address of W array

*  Local Variables initialisation:
      DATA D_VM, Q_VM, R_VM, W_VM /4*0/
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Define the image subset containing necessary pixels.
      CALL DESUBS( WTOR, 4096, LMN, LMX, SMN, SMX, MSUB, STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
         CALL ERROUT( 'Error: defining image subset\\', STATUS )
         RETURN
      END IF

*   Get VM for subset list.
      CALL MWSUBS( MSUB, D_VM, Q_VM, R_VM, W_VM, STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
         CALL ERROUT( 'Error: getting subset VM\\', STATUS )
         RETURN
      END IF

*   Extract subset from image.
      CALL EXSUBS( NS, NL, %VAL( DATA_VM ), %VAL( QUAL_VM ), LMN, LMX,
     :             SMN, SMX, RTOW, MSUB, %VAL( D_VM ), %VAL( Q_VM ),
     :             %VAL( R_VM ), %VAL( W_VM ), NSUB, DBLANK, STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
         CALL ERROUT( 'Error: extracting subset\\', STATUS )
         RETURN
      END IF

*   Perform any ITF corrections.
      CALL ITFCOR( NSUB, %VAL( D_VM ), %VAL( Q_VM ), %VAL( R_VM ),
     :             %VAL( W_VM ) )

*   Extract spectrum from subset list.
      CALL APPLIC( NSUB, %VAL( D_VM ), %VAL( Q_VM ), %VAL( R_VM ),
     :             %VAL( W_VM ) )

*   Free VM.
      CALL FRSUBS( D_VM, Q_VM, R_VM, W_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: freeing VM\\', STATUS )
         RETURN
      END IF

      END
