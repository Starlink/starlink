************************************************************************

      SUBROUTINE AGP_ASSOC ( PARAM, ACMODE, PNAME, BORDER, PICID,
     :                       STATUS )

*+
*  Name:
*     AGP_ASSOC
*
*  Purpose :
*     Associate a device with AGI and PGPLOT.
*
*  Invocation :
*     CALL AGP_ASSOC( PARAM, ACMODE, PNAME, BORDER, PICID, STATUS )
*
*  Description :
*     This is a wrap-up routine to associate a device with the AGI
*     database via the ADAM parameter system and open PGPLOT on it.
*     A PGPLOT viewport corresponding to the current picture in the
*     database is created. This routine calls AGI_ASSOC, AGI_BEGIN
*     AGP_ACTIV and AGP_NVIEW. Also if the name string is not blank
*     then AGI_RCL is called to recall the last picture of that name.
*     This routine should be matched by a closing call to AGP_DEASS.
*
*  Arguments :
*     PARAM = CHARACTER*(*) (Given)
*        The name of the ADAM parameter for accessing device names
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode for pictures. 'READ', 'WRITE' or 'UPDATE'.
*     PNAME = CHARACTER*(*) (Given)
*        Recall last picture of this name if not blank.
*     BORDER = LOGICAL (Given)
*        Flag to indicate if a border is to be left around the viewport.
*     PICID = INTEGER (Returned)
*        Picture identifier for current picture on given device.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     Check status on entry.
*     Open AGI through the parameter system.
*     Begin an AGI context.
*     If the given name is not blank then use it to recall the last picture.
*     Activate the PGPLOT interface.
*     Create a new viewport corresponding to the current database picture.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*      7-JUL-1992 (NE):
*        Original version.
*     29-JUL-1992 (NE):
*        Added PNAME argument
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given :
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) PNAME
      LOGICAL BORDER

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER LNAME * 64
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Associate the parameter with a workstation and current picture.
      CALL AGI_ASSOC( PARAM, ACMODE, PICID, STATUS )

*  Start a new AGI context.
      CALL AGI_BEGIN

*   Copy given picture name to a local variable and remove leading blanks.
      LNAME = PNAME
      CALL CHR_LDBLK( LNAME )

*   If the given name is not blank then use it to recall the last picture.
*   The overwritten picture identifier will be tidied up by AGP_DEASS.
      IF ( LNAME .NE. ' ' ) THEN
         CALL AGI_RCL( LNAME, PICID, STATUS )
      ENDIF

*  Activate PGPLOT and create a viewport.
      CALL AGP_ACTIV( STATUS )
      CALL AGP_NVIEW( BORDER, STATUS )

  99  CONTINUE

      END

