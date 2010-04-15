*+  RED4_SET_FORMAT - Sets the format of the reduction files
      SUBROUTINE RED4_SET_FORMAT( STATUS )
*    Description :
*    Invocation :
*     CALL RED4_SET_FORMAT( STATUS )
*    Parameters :
*     STATUS   = INTEGER( UPDATE )
*           Global ADAM status. This must be ADAM__OK on entry, or the
*           routine will not execute. It will be returned ADAM__OK if
*           the routine is successful. Otherwise it will contain an
*           error status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      9-Nov-1993: Original version                        (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'           ! Contains ADAM__OK
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
*    Status :
      INTEGER
     :  STATUS                     ! Global ADAM status
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the format
      CALL PAR_GET0C( 'FORMAT', SPECIFIED_FORMAT, STATUS )
      CALL CHR_UCASE( SPECIFIED_FORMAT )
      CALL CHR_RMBLK( SPECIFIED_FORMAT )
      CALL PAR_PUT0C( 'FORMAT', SPECIFIED_FORMAT, STATUS )

*   Set the appropriate common block variable
      CALL RED4_SET_FORMAT_2( SPECIFIED_FORMAT, STATUS )

*   Exit subroutine
      END
