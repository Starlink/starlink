*+  SUBPAR_PARNAME - Get the name of a parameter
      SUBROUTINE SUBPAR_PARNAME( NAMECODE, NAME, NAMELEN, STATUS)
*    Description : 
*     Get the name of a parameter
*    Invocation :
*     CALL SUBPAR_PARNAME( NAMECODE, NAME, NAMELEN, STATUS)
*    Parameters : 
*     NAMECODE=INTEGER(INPUT)
*           Identifier of the parameter
*     NAME=CHARACTER*(*)(OUTPUT)
*           Name of the parameter
*     NAMELEN=INTEGER(OUTPUT)
*           Length of the parameter name
*     STATUS=INTEGER(UPDATE)
*           SSE status variable
*    Method : 
*     Get the name from the global variable
*    Authors : 
*     Jon Fairclough (UKTH::JHF)
*     A J Chipperfield (STARLINK)
*    History : 
*     16-May-1986 : Original
*      3-MAR-1993 (AJC):
*       Use SAE_PAR not ADAMERRS
*       Add DAT_PAR for SUBPAR_CMN
*      7-SEP-1993 (AJC):
*       Report if error
*     {insert_further_changes_here}
*    Type Definitions : 
      IMPLICIT NONE 
*    Global constants : 
      INCLUDE 'SAE_PAR'                 ! ADAM Symbolic Constants 
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'
*    Import :
      INTEGER NAMECODE
*    Export :
      CHARACTER*(*) NAME
      INTEGER NAMELEN
*    Status : 
      INTEGER STATUS 
*    Global variables : 
      INCLUDE 'SUBPAR_CMN'
*- 
      IF (STATUS .NE. SAI__OK) RETURN
*
*    Begin
*
      IF (NAMECODE .GE. 1 .AND. NAMECODE .LE. SUBPAR__MAXPAR) THEN
         NAME = PARNAMES(NAMECODE)
         NAMELEN = PARLEN(NAMECODE)
      ELSE
         STATUS = SUBPAR__NOPAR
         CALL EMS_REP('SUP_PARNAME1',
     :   'SUBPAR_PARNAME: NAMECODE out of range', STATUS )
      ENDIF
*
*    End
*
      END
