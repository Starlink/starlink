*+ RED4_CREATE_OBSREDFILE - Create the reduction file for a particular observation
      SUBROUTINE RED4_CREATE_OBSREDFILE (STATUS)
*    Description :
*     This subroutine create the reduction container file for a specified
*      observation
*    Invocation :
*     CALL RED4_CREATE_OBSREDFILE (STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     REVAD::JFL, John Lightfoot
*     JACH::PND, Phil Daly
*    History :
*     date:  changes (institution::username)
*      18-Feb-1993: Conform to error strategy                        (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER*80 OBSFILE                 ! name of the observation file
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF (STATUS .NE. ADAM__OK) RETURN

*    Get the name of the observation file
      CALL PAR_GET0C ('OBSFILE', OBSFILE, STATUS)

*    Create the reduction file
      CALL RED4_MAKE_OBSREDFILE (OBSFILE, STATUS)

      END
