      SUBROUTINE GNS_TNG ( NAME, IWKTYP, ICONID, STATUS )

*+
*  Name:
*     GNS_TNG

*  Purpose:
*     Translate name to a GKS device specification

*  Invocation:
*     CALL GNS_TNG( NAME, IWKTYP, ICONID, STATUS )

*  Description:
*     The workstation name is translated to a GKS workstation type and
*     connection identifier and, if necessary, a logical name created to
*     map the connection identifier onto the device implied by the
*     workstation name.
*
*     This routine is the same as GNS_TNDG but without an explicit
*     physical device name argument.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        Workstation name
*     IWKTYP = INTEGER (Returned)
*        GKS workstation type
*     ICONID = INTEGER (Returned)
*        Connection identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Authors:
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*      3-JUN-1988 (DLT):
*        Original version.
*      1-SEP-1992 (NE):
*        Updated prologue.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER*(*) NAME

*  Arguments Returned:
      INTEGER IWKTYP
      INTEGER ICONID

*  Status:
      INTEGER STATUS
*.

      CALL gns_TNDG(NAME,' ',IWKTYP,ICONID,STATUS)

      END
       
