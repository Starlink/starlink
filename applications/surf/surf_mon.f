      SUBROUTINE SURF_MON (STATUS)
*+
*  Name:
*     SURF_MON

*  Purpose:
*     main routine for SCUBA offline data reduction package

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_MON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This is the main routine for the SCUBA reduction A-task.

*  Notes:
*     This routine is not seen by the user

*  Authors:
*     JFL: J.Lightfoot (ROE)
*     TIMJ: Tim Jenness (JACH)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     25-FEB-1993: Original version
*     12-JAN-1995: Ported to UNIX, changed to 'new style'
*     $Log$
*     Revision 1.1  2004/07/14 20:07:20  timj
*     first attempt at autoconf
*
*     Revision 1.34  2000/06/16 01:23:15  timj
*     Add scuclkerr
*
*     Revision 1.33  1999/08/03 20:01:35  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.32  1999/05/15 01:45:18  timj
*     Add call to surf_set_app_name
*
*     Revision 1.31  1999/02/27 04:37:25  timj
*     Add REMIP
*
*     Revision 1.30  1999/01/12 02:51:01  timj
*     Add ADD_DBM
*
*     Revision 1.29  1998/11/24 21:57:06  timj
*     Add REDUCE_NOISE
*
*     Revision 1.28  1998/05/13 21:49:00  timj
*     Add CALCSKY
*
*     Revision 1.27  1998/04/23 01:34:03  timj
*     Add bad status message to start and end.
*
*     Revision 1.26  1998/04/16 02:48:57  timj
*     Add SCUMAKEWT
*
*     Revision 1.25  1998/01/08 20:08:51  timj
*     Add SCUBA2MEM
*
*     Revision 1.24  1997/10/28 01:24:17  timj
*     Add EXTRACT_FLAT.
*
*     Revision 1.23  1997/10/22 02:41:18  timj
*     Add SCUCLIP and new DESPIKE. Move old despike to despike2.
*
*     Revision 1.22  1997/10/14 18:55:26  jfl
*     added despike
*
*     Revision 1.21  1997/06/13 00:41:44  timj
*     Change all subroutines to SURF_
*
*     Revision 1.20  1997/06/12 23:36:21  timj
*     Comment out CROSSTALK
*
*     Revision 1.19  1997/06/12 21:12:39  timj
*     Change name to SURF_MON (from REDS) and rename subroutine.
*
*     Revision 1.18  1997/06/12 21:07:45  timj
*     Remove KSTEST and DRAWSIG
*     Comment out GET_FLAT
*
*     Revision 1.17  1997/05/27 23:10:27  timj
*     Remove GET_DEMOD, MODIFY and KSTEST from monolith
*
*     Revision 1.16  1997/05/23 18:09:43  timj
*     Add SCAN_RLB
*
*     Revision 1.15  1997/05/10 02:28:57  timj
*     Add EXTRACT_DATA as a new call to REDS_REBIN.
*
*     Revision 1.14  1997/05/01 18:28:07  timj
*     Add CHANGE_DATA.
*
*     Revision 1.13  1997/04/09 02:20:01  timj
*     Add CHANGE_* tasks
*     Add INTREBIN
*     Change REBIN argument to be the name of the task.
*
*     Revision 1.12  1997/01/11 01:30:54  timj
*     Add BOLREBIN (as option to REBIN)
*
c Revision 1.11  1996/12/18  00:17:27  timj
c Add SCUOVER
c
c Revision 1.10  1996/12/17  20:30:31  timj
c Add final ELSE to see if task is not recognised
c
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

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'

*  Status:
      INTEGER STATUS

*  Local variables:
      CHARACTER*(PAR__SZNAM) NAME        ! name of action
*.

      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC('TSK', NAME)
         CALL ERR_REP(' ', 'SURF: bad status on entry to ^NAME',
     :        STATUS)
      END IF

      IF (STATUS .NE. SAI__OK) RETURN

      CALL TASK_GET_NAME (NAME, STATUS)

*     Set application name in history recording
      CALL SURF_SET_APP_NAME( NAME, STATUS )

*     Begin a provenance block. This causes event handlers to be registered
*     with the NDF library so that a handler routine in NDG is called every
*     time an NDF is opened. This handler routine keeps a record of all NDFs
*     that are opened for input or output, until the block is closed by
*     calling NDG_ENDPV.
      CALL NDG_BEGPV( STATUS )

*     Execute required action
      IF (NAME .EQ. 'ADD_DBM') THEN

         CALL SURF_ADD_DBM (STATUS)

      ELSE IF (NAME .EQ. 'BOLREBIN') THEN

         CALL SURF_REBIN(NAME, STATUS)

      ELSE IF (NAME .EQ. 'CALCSKY') THEN

         CALL SURF_REBIN(NAME, STATUS)

      ELSE IF (NAME .EQ. 'CHANGE_DATA') THEN

         CALL SURF_CHGDATA (STATUS)

      ELSE IF (NAME .EQ. 'CHANGE_FLAT') THEN

         CALL SURF_CHGFLAT (STATUS)

      ELSE IF (NAME .EQ. 'CHANGE_POINTING') THEN

         CALL SURF_CHGPNT (STATUS)

      ELSE IF (NAME .EQ. 'CHANGE_QUALITY') THEN

         CALL SURF_CHGQUAL (STATUS)

      ELSE IF (NAME .EQ. 'DESPIKE') THEN

         CALL SURF_REBIN(NAME, STATUS)

      ELSE IF (NAME .EQ. 'DESPIKE2') THEN

         CALL SURF_DESPIKE (STATUS)

      ELSE IF (NAME .EQ. 'EXTINCTION') THEN

         CALL SURF_EXTINCTION (STATUS)

      ELSE IF (NAME .EQ. 'EXTRACT_DATA') THEN

         CALL SURF_REBIN (NAME, STATUS)

      ELSE IF (NAME .EQ. 'EXTRACT_FLAT') THEN

         CALL SURF_EXTFLAT(STATUS)

      ELSE IF (NAME .EQ. 'FLATFIELD') THEN

         CALL SURF_FLATFIELD (STATUS)

*      ELSE IF (NAME .EQ. 'GET_DEMOD') THEN

*         CALL SURF_GET_DEMOD (STATUS)

*      ELSE IF (NAME .EQ. 'GET_FLAT') THEN

*         CALL SURF_GET_FLAT (STATUS)

      ELSE IF (NAME .EQ. 'INTREBIN') THEN

         CALL SURF_REBIN (NAME, STATUS)

      ELSE IF (NAME .EQ. 'SCUMAKEWT') THEN

         CALL SURF_MAKE_WEIGHT( STATUS )

      ELSE IF (NAME .EQ. 'SCUPHOT') THEN

         CALL SURF_SCUPHOT (STATUS)

      ELSE IF (NAME .EQ. 'REBIN') THEN

         CALL SURF_REBIN (NAME, STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_SWITCH') THEN

         CALL SURF_REDUCE_SWITCH (STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_NOISE') THEN

         CALL SURF_REDUCE_NOISE (STATUS)

      ELSE IF (NAME .EQ. 'REMIP') THEN

         CALL SURF_REMIP ( STATUS )

      ELSE IF (NAME .EQ. 'REMSKY') THEN

         CALL SURF_REMSKY (STATUS)

      ELSE IF (NAME .EQ. 'RESTORE') THEN

         CALL SURF_RESTORE (STATUS)

      ELSE IF (NAME .EQ. 'SKYDIP') THEN

         CALL SURF_SKYDIP (STATUS)

      ELSE IF (NAME .EQ. 'SKYDIP2') THEN

         CALL SURF_SKYDIP2 (STATUS)

      ELSE IF (NAME .EQ. 'SCAN_RLB') THEN

         CALL SURF_SCAN_RLB (STATUS)

      ELSE IF (NAME .EQ. 'SCUBA2MEM') THEN

         CALL SURF_SCUBA2MEM (STATUS)

      ELSE IF (NAME .EQ. 'SCUCAT') THEN

         CALL SURF_SCUCAT (STATUS)

      ELSE IF (NAME .EQ. 'SCUCLKERR') THEN

         CALL SURF_SCUCLKERR (STATUS)

      ELSE IF (NAME .EQ. 'SCUOVER') THEN

         CALL SURF_SCUOVER (STATUS)

      ELSE IF (NAME .EQ. 'SCUHELP') THEN

         CALL SURF_SCUHELP (STATUS)

      ELSE IF (NAME .EQ. 'SCUCLIP') THEN

         CALL SURF_SCUCLIP (STATUS)

      ELSE
         CALL MSG_SETC('TAS', NAME)
         CALL MSG_OUT(' ','^TAS is not present in the SURF monolith',
     :        STATUS)

      END IF

*  End the provenance block. This will result in every output NDF being
*  given a provenance extension containing a record of the input NDFs
*  that the application accessed in order to create the output NDF. Any
*  output NDF that already contains a provenance extension is left
*  unchanged (so individual application can over-ride this automatic
*  provenance handling by adding a provenance extension to the output NDF
*  itself).
      CALL NDG_ENDPV( 'SURF:'//NAME, STATUS )

      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC('TSK', NAME)
         CALL ERR_REP(' ', 'SURF: bad status on exit from ^TSK',
     :        STATUS)
      END IF


      END
