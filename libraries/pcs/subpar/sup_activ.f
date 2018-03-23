      SUBROUTINE SUBPAR_ACTIV ( TASKNAME, LENGTH, STATUS )
*+
*  Name:
*     SUBPAR_ACTIV

*  Purpose:
*     Activate ADAM parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ACTIV ( TASKNAME, LENGTH, STATUS )

*  Description:
*     Starts-up ADAM parameter system.

*  Arguments:
*     TASKNAME=CHARACTER*(*) (given)
*        name of task
*     LENGTH=INTEGER (given)
*        length of TASKNAME
*     STATUS=INTEGER

*  Algorithm:
*     Call SUBPAR_FINDIF to find the interface module and set up the
*     SUBPAR common blocks.
*     Start HDS and open/create storage for the program parameters.
*     Initialise path to controlling task.
*     Declare exit handler for parameter system.

*  Implementation Deficiencies:
*     The handling of locking is expected to be done by HDS at a future
*     release so could be removed from here

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1989, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     BDK: B D Kelly (ROE)
*     KS: K Shortridge (AAO)
*     BMC: B McNally (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     04-OCT-1984 (BDK):
*        Original
*     26-OCT-1984 (BDK):
*        Exit handler added
*     17-APR-1985 (BMC):
*        Change arguments to PUTPATH
*     23-MAY-1985 (BDK):
*        Use EXENAME for interface file
*     23-AUG-1985 (BDK):
*        handle monoliths
*     28-AUG-1985 (BDK):
*        set EXTLOC and PROGNUM for non-monoliths
*     06-SEP-1985 (BDK):
*        set DYNLOC for storing dynamic defaults
*     06-SEP-1985 (BDK):
*        check all components for monoliths
*     08-OCT-1985 (BDK):
*        change parsing of EXENAME
*     11-NOV-1985 (BDK):
*        add call to TERMFACE
*     06-MAR-1986 (BDK):
*        add call to SETCHECK
*     10-APR-1986 (BDK):
*        use EXENAME for parameter storage SDF
*     11-OCT-1986 (BDK):
*        use LIB$GETJPI
*     14-MAY-1987 (BDK):
*        initialize LEX parser
*     14-MAY-1987 (BDK):
*        switch-off HDS error reporting
*     18-JUN-1987 (BDK):
*        on bad IFC, give up
*     01-MAR-1989 (KS):
*        move HDS search of monolith .SDF file to SUBPAR_FINDACT
*        to speed up monolith loading.
*     18-JUL-1989 (AJC):
*        tidy up.
*     01-AUG-1989 (AJC):
*        Call SUBPAR_FINDIF to find the interface module according
*        to new rules using search path.
*     13-JUN-1991 (AJC):
*        Call TERMFACE at beginning
*     14-JUN-1991 (AJC):
*        Improve error reporting - use EMS
*        Replace STR$TRIM by CHR_LEN
*     18-SEP-1991 (AJC):
*        Add bit to create new parameter file if old one locked
*        and to lock file as SUBPAR__ACTIV
*        Remove remaining VMS depedencies to lower level
*        Properly mark and release error levels and annul
*        messages from HDS_OPEN.
*        Remove call to EXC_LEVEL
*     27-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*     15-OCT-1991 (AJC):
*        Correctly note error in _RDIF
*     18-NOV-1991 (AJC):
*        Change "ADAMSTART" message
*     12-FEB-1992 (AJC):
*        Call SUBPAR_PFER to output system dependent message
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-MAY-1995 (AJC):
*        Allow longer PFNAM
*      7-JUN-1996 (AJC):
*        Remove HDS_LOCK (it prevents SETPAR being seen sometimes)
*        Use CLONE instead of assignment of EXTTOP to EXTLOC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'


*  Arguments Given:
      CHARACTER*(*) TASKNAME             ! name of task

      INTEGER LENGTH                     ! length of TASKNAME


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External References:

*  Local Variables:
      CHARACTER*1024 PFNAM                  ! name task parameter file
      CHARACTER*1024 IFNAM                  ! name of interface module
      CHARACTER*(SUBPAR__NAMELEN) TSKNAM   ! name of task
      INTEGER ISTAT                        ! internal status
      LOGICAL IFC                          ! TRUE if .IFC found
      LOGICAL THERE                        ! check for existence of HDS
                                           ! components

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context. This ensures error deferral even for
*  existing old-style tasks.
      CALL EMS_MARK

*      Initialise the path to the controlling task - this only ever gets
*      set to non-zero for A-tasks
         CALL SUBPAR_PUTPATH ( 0, 0, STATUS )

*      Initialise flag saying this task is controlled by another task,
*      rather than directly from the terminal
         CALL SUBPAR_TERMFACE ( .FALSE., STATUS )


*  Find the names associated with the executable image
      CALL SUBPAR_TSKNM( TSKNAM, PFNAM, IFNAM, IFC, STATUS )

*  Initialize the COMMON blocks from the interface module
      CALL SUBPAR_RDIF( IFNAM, IFC, STATUS )

*  If names found OK and COMMON initialized
      IF ( STATUS .EQ. SAI__OK ) THEN

*     Start HDS
         CALL HDS_START ( STATUS )

*     Get locator to program's private storage
         CALL HDS_OPEN ( PFNAM, 'UPDATE', EXTTOP, STATUS )

*     If failed, try to create the file
*     If failure was due to file locked then close the existing file
*     and create a new one.
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .EQ. DAT__FILCK ) THEN
               ISTAT = SAI__OK
               CALL HDS_CLOSE ( EXTTOP, ISTAT )
            ENDIF
            CALL EMS_ANNUL ( STATUS )
            CALL HDS_NEW ( PFNAM, TSKNAM, 'STRUC',
     :        0, 0, EXTTOP, STATUS )

*        If failed report and give up
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL EMS_REP( 'SUP_ACTIV1',
     :          'SUBPAR: Failed to open task''s parameter file',
     :          STATUS )
               CALL SUBPAR_PFER( STATUS )
            ENDIF
         ENDIF

         IF ( STATUS .EQ. SAI__OK ) THEN
*        For a monolith, the components are now set up, as required,
*        by SUBPAR_FINDACT. Therefore here do nothing

            IF ( MONOLITH ) THEN
*           Do nothing

            ELSE
*           Not a monolith. Ensure that the component for dynamic
*           defaults exists and get its locator.
               CALL DAT_THERE ( EXTTOP, 'ADAM_DYNDEF', THERE, STATUS )
               IF ( .NOT. THERE ) THEN
                  CALL DAT_NEW ( EXTTOP, 'ADAM_DYNDEF', 'DEFAULTS',
     :             0, 0, STATUS )
               ENDIF
               CALL DAT_FIND ( EXTTOP, 'ADAM_DYNDEF', DYNLOC, STATUS )

*           Set the program pointer to 1 and copy the file locator into
*           the 'top of parameters' locator
               PROGNUM = 1
               CALL DAT_CLONE( EXTTOP, EXTLOC, STATUS )

            ENDIF

*        Initialise flag saying that NEEDS lists must be checked before
*        application is called.
            CALL SUBPAR_SETCHECK ( .TRUE., STATUS )

*        Declare parameter system exit handler
            CALL SUBPAR_DEXIT ( STATUS )

*        Initialize command-line parser
            CALL LEX_CMDSET

         ENDIF

      ENDIF

*  Flush any error messages as task may continue.
*  We can use PARSECON_ERFL as there will be no path set up at this
*  stage, this will just PRINT the messages.
      CALL PARSECON_ERFL( ISTAT )
      CALL EMS_RLSE

      END
