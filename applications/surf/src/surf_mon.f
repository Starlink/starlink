      SUBROUTINE REDS (STATUS)
*+
*  Name:
*     REDS

*  Purpose:
*     main routine for SCUBA offline data reduction package

*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This is the main routine for the SCUBA reduction A-task.

*  Notes:
*     This routine is not seen by the user

*    Authors :
*     JFL: J.Lightfoot (ROE)
*     TIMJ: Tim Jenness (JACH)

*    History :
*     $Id$
*     25-FEB-1993: Original version
*     12-JAN-1995: Ported to UNIX, changed to 'new style'
*     $Log$
*     Revision 1.10  1996/12/17 20:30:31  timj
*     Add final ELSE to see if task is not recognised
*
c Revision 1.9  1996/11/18  02:25:49  timj
c Add REMSKY
c
c Revision 1.8  1996/11/01  22:15:46  timj
c Change PHOTOM to SCUPHOT
c
c Revision 1.7  1996/11/01  21:18:13  timj
c Add SCUHELP
c Update header.
c
c Revision 1.6  1996/10/15  01:44:29  timj
c Add DRAWSIG
c
c Revision 1.5  1996/09/18  19:13:19  timj
c Add KSTEST, change CONCAT to SCUCAT
c
c Revision 1.4  1996/09/17  02:14:22  timj
c Add CONCAT
c
c Revision 1.3  1996/09/16  20:27:18  timj
c Change PHOTOM to SCUPHOT
c
c Revision 1.2  1996/07/31  18:53:16  timj
c Add skydip option
c
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}

*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER*(PAR__SZNAM) NAME        ! name of action
*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL TASK_GET_NAME (NAME, STATUS)

      IF (NAME .EQ. 'CROSSTALK') THEN

         CALL REDS_CROSSTALK (STATUS)

      ELSE IF (NAME .EQ. 'EXTINCTION') THEN

         CALL REDS_EXTINCTION (STATUS)

      ELSE IF (NAME .EQ. 'FLATFIELD') THEN

         CALL REDS_FLATFIELD (STATUS)

      ELSE IF (NAME .EQ. 'GET_DEMOD') THEN

         CALL REDS_GET_DEMOD (STATUS)

      ELSE IF (NAME .EQ. 'GET_FLAT') THEN

         CALL REDS_GET_FLAT (STATUS)

      ELSE IF (NAME .EQ. 'MODIFY') THEN

         CALL REDS_MODIFY (STATUS)

      ELSE IF (NAME .EQ. 'SCUPHOT') THEN

         CALL REDS_SCUPHOT (STATUS)

      ELSE IF (NAME .EQ. 'REBIN') THEN

         CALL REDS_REBIN (STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_SWITCH') THEN

         CALL REDS_REDUCE_SWITCH (STATUS)
      ELSE IF (NAME .EQ. 'REMSKY') THEN

         CALL REDS_REMSKY (STATUS)

      ELSE IF (NAME .EQ. 'RESTORE') THEN

         CALL REDS_RESTORE (STATUS)

      ELSE IF (NAME .EQ. 'SKYDIP') THEN

         CALL REDS_SKYDIP (STATUS)

      ELSE IF (NAME .EQ. 'SCUCAT') THEN

         CALL REDS_SCUCAT (STATUS)

      ELSE IF (NAME .EQ. 'SCUHELP') THEN

         CALL SCUHELP (STATUS)

      ELSE IF (NAME .EQ. 'KSTEST') THEN

         CALL REDS_KSTEST (STATUS)

      ELSE IF (NAME .EQ. 'DRAWSIG') THEN
         CALL DRAWSIG (STATUS) 

      ELSE
         CALL MSG_SETC('TAS', NAME)
         CALL MSG_OUT(' ','^TAS is not present in the monolith',
     :        STATUS)

      END IF

      END
