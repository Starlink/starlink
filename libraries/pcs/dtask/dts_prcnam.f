*+  DTASK_PRCNAM - gives the name by which the task is to register with AMS
      SUBROUTINE DTASK_PRCNAM ( NAME, LENGTH, STATUS )
*    Description :
*     Obtains name by which the task is to register with the ADAM Message
*     System (AMS).
*     This is the Unix version - returns either the name set by ICL OR the 
*     name of the exe running.
*    Invocation :
*     CALL DTASK_PRCNAM ( NAME, LENGTH, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (returned)
*           name, with any path information removed
*     LENGTH=INTEGER (returned)
*           length of the name
*     STATUS=INTEGER
*    Method :
*     If the environment variable ICL_TASK_NAME exists then take the
*     its translation as the AMS task name to use. 
*     Otherwise get ARG 0, find the end of any path information and take the
*     the remainder as the name.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     AJC: A J Chipperfield (STARLINK)
*     BKM: B K McIlwrath (STARLINK)
*    History :
*     13-NOV-1992 (AJC):
*      Original Version - dummy
*     23-FEB-1993 (AJC):
*      Version to use exe name
*     26-APR-1993 (AJC):
*      Add the translation of ICL_PARENT_PID to the name where appropriate
*     02-AUG-1993 (BKM):
*      Replace ICL_PARENT_PID by ICL_TASK_NAME
*     15-JUN-2001 (AJC):
*      Changed name from ADAM_PRCNAM
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Export :
      CHARACTER NAME*(*)   !  process name returned
      INTEGER LENGTH       !  length of name (will be 0-15 chars)
*    Status :
      INTEGER STATUS
*    External References:
      INTEGER CHR_LEN      !  used length of string
      EXTERNAL CHR_LEN
      INTEGER STRING_IANYR !  find char from right
      EXTERNAL STRING_IANYR
*    Local variables :
      CHARACTER*256 ARGV0
      INTEGER STNM         ! Start of command name
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL EMS_MARK

      CALL PSX_GETENV( 'ICL_TASK_NAME', NAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         LENGTH = CHR_LEN( NAME )
      ELSE
         CALL EMS_ANNUL( STATUS )

         CALL GETARG( 0, ARGV0 )
         LENGTH = CHR_LEN( ARGV0 )
         STNM = STRING_IANYR( ARGV0(1:LENGTH), '/' ) + 1
         NAME = ARGV0(STNM:LENGTH)
         LENGTH = LENGTH - STNM + 1
      ENDIF         

      CALL EMS_RLSE

      END
