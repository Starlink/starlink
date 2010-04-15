*+  RED4_CREATE_GRPRED - Create a reduced group file.
      SUBROUTINE RED4_CREATE_GRPRED( STATUS )
*    Description :
*     This routine creates an empty reduced group file which may be
*     used for test purposes.
*    Invocation :
*     CALL RED4_CREATE_GRPRED( STATUS )
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
*     Steven Beard  (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     18-Sep-1990: Original version.                       (SMB)
*      1-Jan-1991: Typing mistake fixed.                   (SMB)
*     20-Feb-1991: Made to convert input to upper case.    (SMB)
*     18-Feb-1993: Conform to error strategy               (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'      ! Contains ADAM__OK.
      INCLUDE 'SAI_ERR'       ! Contains SAI__ERROR.
*    Status :
      INTEGER
     :  STATUS                ! Global ADAM status.
*    External :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER
     :  GRPNUM                ! The group number
      CHARACTER*80
     :  OBSRED,               ! Name of reduced observation file.
     :  GRPRED                ! Name of reduced group file.
      CHARACTER*20 LPREFIX    ! prefix to add to file
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of a reduced observation which is to be used as a
*   template to construct the reduced group file, and convert it to
*   upper case.
      CALL PAR_GET0C( 'OBSRED', OBSRED, STATUS )

*   Obtain a group number to be used. (Perhaps this could be obtained
*   from the reduced observation file eventually ?)
      CALL PAR_GET0I( 'GRPNUM', GRPNUM, STATUS )

*   Check these parameters have been obtained successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Convert the name of the reduced observation file into the name
*      of the reduced group file, using the group number obtained
*      above.
         CALL RED4_ROBSTOGRP( OBSRED, GRPNUM, GRPRED, STATUS )

*      Create a container file to hold the reduced group.
         CALL RED4_GET_PREFIX ('RO', LPREFIX, STATUS)
         CALL RED4_MAKE_GRPREDFILE( LPREFIX(:CHR_LEN(LPREFIX))//OBSRED,
     :    GRPRED, STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CREATE_GRPRED: '/
     :     /'Error obtaining %OBSRED and '/
     :     /'%GRPNUM parameters.', STATUS )
      END IF

      END




