*+  CMP_ERDSN - Report error on structure component
      SUBROUTINE CMP_ERDSN(LOC,CMP,STATUS)
*    Description :
*     This routine reports an error of the form:
*
*        <structure name>.<component name> status
*
*     where <structure name> is the name of the object located by LOC.
*           <component name> is CMP
*           status is the message text associated with the status value.
*    Parameters :
*     LOC=CHARACTER*(*)
*           Variable containing a locator associated with a structured
*           data object.
*     CMP=CHARACTER*(*)
*           Expression specifying the component name within the
*           structure
*     STATUS=INTEGER
*           Variable holding the status value.
*    Method :
*     Calls the routine DAT_ERDSN which outputs appropriately for
*     the environment being used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     A Chipperfield  (RAL::AJC)
*    History :
*     27-Mar-1987:  re-write to call DAT_ERDSN  (RAL::AJC)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import
      CHARACTER*(*) LOC			! structure locator
      CHARACTER*(*) CMP			! Component name
      INTEGER STATUS			! Status value
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*    Internal References :
*    Local data :
*-

      CALL DAT_ERDSN( LOC, CMP, STATUS)

      END
