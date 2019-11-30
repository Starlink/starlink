      SUBROUTINE SUBPAR_DEACT ( TTYPE, STATUS )
*+
*  Name:
*     SUBPAR_DEACT

*  Purpose:
*     Deactivate ADAM parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DEACT ( TTYPE, STATUS )

*  Description:
*     Closes-down the ADAM parameter system for a program.

*  Arguments:
*     TTYPE=CHARACTER*(*) (Given)
*        The type of reset required ( 'A', 'I' or 'R')
*     STATUS=INTEGER
*        Global status

*  Algorithm:
*     For action specified by PROGNUM, or for all actions if PROGNUM = 0,
*     de-activate the action in a manner determined by the TTYPE argument.
*     If TTYPE is not I or R: Update GLOBAL associations.
*                Cancel the parameters and set them into the GROUND state.
*     If TTYPE is I: Update GLOBAL associations.
*                Do not reset active or null parameters, but reset all
*                others. Free any files associated with active parameters.
*     If TTYPE is R: Do not update GLOBAL associations (assume it was done
*                previously)
*                Cancel the parameters and set them into the GROUND state.
*     Reset the MIN, MAX and Dynamic default values in all cases and re-use
*     the space. Annul the EXTLOC locator
*
*     To update GLOBAL associations, if the application terminated
*     successfully, copy the values of any parameters in the ACTIVE state
*     into the global area if they have a global association.
*     The global HDS structure is only opened if necessary, and is
*     closed again subsequently.

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1988, 1989, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996, 1997, 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     DSB: David S. Bery (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1984 (BDK):
*        Original
*     25-OCT-1984 (BDK):
*        remove closedown of HDS
*     09-NOV-1984 (BDK):
*        reset all parameter states
*     24-NOV-1984 (BDK):
*        reset states even on bad STATUS
*     03-JUN-1985 (BDK):
*        change annuling of locators
*     23-AUG-1985 (BDK):
*        handle monoliths
*     06-SEP-1985 (BDK):
*        annul DYNLOC for monoliths
*     13-MAY-1987 (BDK):
*        copy HDS names into the SDF
*     13-MAY-1987 (BDK):
*        free the SDF so it is updated on disk
*     13-DEC-1988 (AJC):
*        remove copy HDS names into the SDF - Now
*        done in SUBPAR_PUTNAME consistent with non-names
*     24-JUL-1989 (AJC):
*        DAT_ERASE existing global component whatever its
*        type now DAT_ERASE is recursive
*     14-AUG-1989 (AJC):
*        Use DAT_COPY to save global associations if private
*        storage already exists
*     09-JUL-1990 (AJC):
*        Un-define dynamic defaults
*     07-JUN-1991 (AJC):
*        Improve comments
*        Correct use of local status
*     02-AUG-1991 (AJC):
*        Translate global file name first
*        Remove .SDF from name
*     15-JAN-1992 (AJC):
*        Use SUBPAR_ADMUS to define ADAM_USER directory
*        Create GLOBAL if it doesn't exist
*     23-MAR-1992 (AJC):
*        Negate PARDYN(3,-) if set, to retain type information
*     20-JUL-1992 (AJC):
*        Move DATA statement
*     31-JUL-1992 (AJC):
*        Reset list pointers rather than negating dynamic type
*        but re-initialise PARDYN pointer also
*     16-NOV-1992 (AJC):
*        Un-define min and max values
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     30-JAN-1996 (AJC):
*        Add TTYPE argument to select bits of closedown
*      7-JUN-1996 (AJC):
*        Remove HDS_LOCK (it prevents SETPAR value being seen sometimes)
*        and correctly align code
*     11-OCT-1996 (AJC):
*        Only free files if locator valid
*        Allow invalid EXTLOC,DYNLOC
*        Don't update globals if TTYPE R (reset)
*        Use DAT_ANNUL not HDS_CLOSE for GLOBLOC
*        Work through actions to allow for altered extloc
*     21-FEB-1997 (AJC):
*        Fix bug which cancelled all parameter values before GLOBAL update
*     19-MAY-1997 (AJC):
*        Correct description for TTYPE argument
*     10-DEC-1997 (AJC):
*        Add some error reports
*      4-FEB-2000 (AJC):
*        De-activate only action PROGNUM unless PROGNUM=0 (set by CONTROL
*        PAR_RESET) in which case de-activate all actions.
*        Assume all active non-internal parameters have locator set in
*          PAR tables.
*     11-AUG-2000 (AJC):
*        Correct action for object names (type GE 20). (Shouldn't assume
*        locator in PAR tables was the correct thing to copy to GLOBAL.)
*     20-MAR-2008 (DSB):
*        When copying a parameter value from the application parameter
*        file to the global parameter file, check that the application
*        parameter exists before accesing it. If a parameter (such as an
*        output NDF) is specified on the command line, it becomes active.
*        But if the application never accesses the parameter (because it
*        decided not to create the output NDF), no value for it will
*        exist in the application parameter file. The old system was to
*        report an error in this situation. The new system just ignores
*        the parameter.
*     22-MAY-2017 (DSB):
*        Use EMS error reporting environments rather than just using a
*        local status variable. This guards against accidentally anulling
*        any pre-existing error messages.
*     {enter_further_changes_here}

*  Deficiencies:
*     None known

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'DAT_ERR'

*  Given Arguments:
      INTEGER ACTION
      CHARACTER*(*) TTYPE

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      CHARACTER*(DAT__SZLOC) BOTLOC    ! locator to global store
      CHARACTER*(DAT__SZLOC) ACTLOC    ! locator to action parameter store
      CHARACTER*(DAT__SZLOC) LOC       ! locator to parameter store
      CHARACTER*(DAT__SZLOC) FILOC     ! locator to associated file
      INTEGER J                        ! parameter counter
      INTEGER K                        ! action counter
      INTEGER KSTART, KEND             ! action counter limits
      CHARACTER*1000 ADMUSR            ! definition of ADAM_USER directory
      INTEGER AULEN                    ! used length of ADMUSR
      CHARACTER*80 GLOBNAME            ! name of global association
      INTEGER NUMLEVS                  ! number of levels in globname
      CHARACTER*15 COMPONENT(16)       ! GLOBNAME split into components
      CHARACTER*1000 FILENAME          ! container file for GLOBNAME
      INTEGER NAMLEN                   ! used length of FILENAME
      LOGICAL THERE                    ! return from DAT_THERE
      LOGICAL STARTED                  ! .FALSE. => global store not yet
                                       ! opened
      LOGICAL VALID                    ! .TRUE. => internal parameter
                                       ! locator exists
      LOGICAL ACTGOT                   ! .TRUE. if action parameter store
                                       ! locator already obtained
      CHARACTER*15 STRINGTYPE(0:5)     ! possible HDS type strings

*  Local Data:
      DATA STRINGTYPE / 'STRUC', '_CHAR*132', '_REAL', '_DOUBLE',
     :  '_INTEGER', '_LOGICAL' /
*.

*   Flag GLOBAL file not open
      STARTED = .FALSE.

*   Find which actions are to be de-activated
      IF ( PROGNUM .EQ. 0 ) THEN
         KSTART = 1
         KEND = ACTPTR
      ELSE
         KSTART = PROGNUM
         KEND = PROGNUM
      ENDIF

*   For each required action
      DO K = KSTART, KEND

*     Flag that we haven't yet got locator for action's parameter storage
         ACTGOT = .FALSE.

*     For each action parameter
         DO J = PROGADD(1,K), PROGADD(2,K)
            IF ( ( STATUS .EQ. SAI__OK )
     :         .AND. ( TTYPE .NE. 'R' ) ) THEN

*         Copy global associations if STATUS OK and not running as 'Realtime'
               IF ( ( PARSTATE(J) .EQ. SUBPAR__ACTIVE ) .AND.
     :            ( ( PARASSOC(2,J) .EQ. SUBPAR__WRITE ) .OR.
     :            ( PARASSOC(2,J) .EQ. SUBPAR__UPDATE ) ) ) THEN

*               The parameter has a value to be copied to the global store
*                If GLOBAL file not open, open it
                  IF ( .NOT. STARTED ) THEN

*                  Global store not yet open
*                  Get definition of ADAM_USER
                     CALL SUBPAR_ADMUS ( ADMUSR, AULEN, STATUS )

*                  Now translate the filename ( may be redundant now )
                     CALL SUBPAR_FNAME
     :                        ( ADMUSR(1:AULEN)//'GLOBAL',FILENAME,
     :                       NAMLEN, STATUS )

*                  and attempt to open the file
                     CALL HDS_OPEN ( FILENAME(1:NAMLEN), 'UPDATE',
     :                       GLOBLOC, STATUS )

*                  If the file was not found, create it.
                     IF ( STATUS .EQ. DAT__FILNF ) THEN
                        CALL EMS_ANNUL ( STATUS )
                        CALL HDS_NEW
     :                          ( FILENAME(1:NAMLEN), 'GLOBAL',
     :                          'STRUC', 0, 0, GLOBLOC, STATUS )
                     ENDIF

*                  If successful, set STARTED
                     IF ( STATUS .EQ. SAI__OK ) STARTED = .TRUE.

                  ENDIF

*               Get the global association and split it into its components
                  GLOBNAME = CHARLIST(PARASSOC(1,J))
                  CALL SUBPAR_SPLIT ( GLOBNAME, 16, NUMLEVS,
     :                    COMPONENT, FILENAME, STATUS )

                  IF ( ( COMPONENT(1) .EQ. 'GLOBAL' ) .AND.
     :                     ( NUMLEVS .EQ. 2 ) ) THEN

*                  If a global exists, delete it
                     CALL DAT_THERE ( GLOBLOC, COMPONENT(2),
     :                       THERE, STATUS )

                     IF ( THERE ) THEN
                        CALL DAT_ERASE ( GLOBLOC, COMPONENT(2),
     :                          STATUS )
                     ENDIF


                     IF ( PARTYPE(J) .GE. 20 ) THEN
*                     The stored 'value' is actually the name of a data
*                     structure
*                     If not already found, find locator to component in
*                     parameter file.
                        IF ( .NOT. ACTGOT ) THEN
                           IF (MONOLITH ) THEN
                              CALL DAT_FIND (
     :                          EXTTOP, ACTNAMES(K), ACTLOC, STATUS )
                           ELSE
                              CALL DAT_CLONE( EXTTOP, ACTLOC,
     :                          STATUS )
                           ENDIF
                           IF ( STATUS .EQ. SAI__OK ) THEN
                              ACTGOT = .TRUE.
                           ELSE
                              CALL EMS_SETC( 'ACT', ACTNAMES(K) )
                              CALL EMS_REP( 'SUP_DEACT1', 'SUBPAR: '//
     :                        'Failed to find locator for action ^ACT',
     :                         STATUS )
                           ENDIF
                        ENDIF
*                     If a parameter value is available, copy it to
*                     global.
                        CALL DAT_THERE( ACTLOC, PARNAMES(J), THERE,
     :                    STATUS )
                        IF( THERE ) THEN
                           CALL DAT_FIND ( ACTLOC, PARNAMES(J), LOC,
     :                       STATUS )
                           CALL DAT_COPY (
     :                       LOC, GLOBLOC, COMPONENT(2), STATUS )
                           CALL DAT_ANNUL ( LOC, STATUS )
                        ENDIF

                     ELSE IF ( PARTYPE(J) .GE. 10 ) THEN
*                     Get locator to parameter store
                        CALL SUBPAR_GETLOC( J, VALID, LOC, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( VALID ) THEN

*                        copy component to GLOBAL
                              CALL DAT_COPY (
     :                          LOC, GLOBLOC, COMPONENT(2), STATUS )

                           ELSE
                              CALL EMS_SETC
     :                                ( 'PARAM', PARNAMES(J) )
                              CALL EMS_REP
     :                                ( 'SUP_DEACT3', 'SUBPAR: '//
     :                                'Invalid locator for parameter '//
     :                                '^PARAM', STATUS )
                           ENDIF

                        ELSE
                           CALL EMS_SETC( 'PARAM', PARNAMES(J) )
                           CALL EMS_REP( 'SUP_DEACT2', 'SUBPAR: '//
     :                          'Failed to find locator for parameter '
     :                          // '^PARAM', STATUS )
                        ENDIF

                     ELSE IF ( PARTYPE(J) .GT. 0 ) THEN

*                     The parameter value is an 'INTERNAL' scalar - only
*                     relevant for d-tasks.
                        CALL DAT_NEW ( GLOBLOC, COMPONENT(2),
     :                    STRINGTYPE(PARTYPE(J)), 0, 0, STATUS )
                        CALL DAT_FIND ( GLOBLOC, COMPONENT(2),
     :                    BOTLOC, STATUS )
                        IF ( PARTYPE(J) .EQ. SUBPAR__REAL ) THEN
                           CALL DAT_PUTR ( BOTLOC, 0, 0, PARREAL(J),
     :                       STATUS )
                        ELSE IF ( PARTYPE(J) .EQ. SUBPAR__INTEGER )
     :                  THEN
                           CALL DAT_PUTI ( BOTLOC, 0, 0, PARINT(J),
     :                       STATUS )
                        ELSE IF ( PARTYPE(J) .EQ. SUBPAR__INT64 )
     :                  THEN
                           CALL DAT_PUTK ( BOTLOC, 0, 0, PARINT64(J),
     :                       STATUS )
                        ELSE IF ( PARTYPE(J) .EQ. SUBPAR__DOUBLE )
     :                  THEN
                           CALL DAT_PUTD ( BOTLOC, 0, 0,
     :                       PARDOUBLE(J), STATUS )
                        ELSE IF ( PARTYPE(J) .EQ. SUBPAR__CHAR )
     :                  THEN
                           CALL DAT_PUTC ( BOTLOC, 0, 0, PARVALS(J),
     :                       STATUS )
                        ELSE IF ( PARTYPE(J) .EQ. SUBPAR__LOGICAL )
     :                  THEN
                           CALL DAT_PUTL ( BOTLOC, 0, 0, PARLOG(J),
     :                       STATUS )
                        ENDIF
                        CALL DAT_ANNUL ( BOTLOC, STATUS )

                     ENDIF

                  ENDIF

               ENDIF ! End of updating Globals

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL EMS_SETC( 'PARAM', PARNAMES(J) )
                  CALL EMS_REP( 'SUP_DEACT5', 'SUBPAR: '//
     :            'Failed to update GLOBAL file for parameter ^PARAM',
     :             STATUS )


*           If the file is corrupt, recommend its removal.
                  IF ( STATUS .EQ. DAT__INCHK ) THEN
                     CALL EMS_MARK
                     STATUS = SAI__OK
                     CALL SUBPAR_ADMUS( ADMUSR, AULEN, STATUS )
                     CALL SUBPAR_FNAME( ADMUSR(1:AULEN)//'GLOBAL.sdf',
     :                                  FILENAME, NAMLEN, STATUS )
                     CALL EMS_RLSE
                     STATUS = DAT__INCHK
                     CALL EMS_SETC( 'F', FILENAME )
                     CALL EMS_REP( 'SUP_CRINT2',
     :                           'SUBPAR: The global parameter file: '//
     :                           '^F is corrupt and should be deleted',
     :                           STATUS )
                  END IF
               ENDIF

            ENDIF

*         Even if the STATUS is not OK,
            CALL EMS_BEGIN( STATUS )

*         If the task type is not 'I', or the parameter is not active
*         or null, reset it back to the ground state;
            IF ( (TTYPE .NE. 'I') .OR.
     :         ( ( PARSTATE(J) .NE. SUBPAR__ACTIVE ) .AND.
     :         ( PARSTATE(J) .NE. SUBPAR__NULL ) ) ) THEN
               CALL SUBPAR_CANCL ( J, STATUS )
               PARSTATE(J) = SUBPAR__GROUND

*         otherwise just ensure that associated HDS files are freed for
*         use by others.
            ELSE IF ( PARTYPE(J) .GE. 20 ) THEN
               CALL SUBPAR_GETFLOC ( J, VALID, FILOC, STATUS )
               IF ( VALID ) THEN
                  CALL HDS_FREE ( FILOC, STATUS )
               ENDIF
            ENDIF

*         and re-initialise the dynamic defaults and min and max pointers
            PARDYN(1,J) = 0
            PARMIN(1,J) = 0
            PARMAX(1,J) = 0

*         Restore the original error reporting environment.
            CALL EMS_END( STATUS )

*      End of parameter loop
         ENDDO

*   Clean up Action
         IF( ACTGOT ) CALL DAT_ANNUL( ACTLOC, STATUS )

*   End of action loop
      ENDDO

*   Close container file for globals if it's open
      IF ( STARTED ) CALL DAT_ANNUL ( GLOBLOC, STATUS )

*   Reset list pointers to re-use space if required
      INTPTR = INTPSV
      REALPTR = REALPSV
      DOUBLEPTR = DOUBLEPSV
      CHARPTR = CHARPSV
      LOGPTR = LOGPSV

*   If this is a monolith, release the program locator and the locator to
*   dynamic defaults.
*   If SUBPAR_DEACT is called via a RESET message, these locators will
*   not be valid, having been annulled previously.
      IF ( MONOLITH ) THEN
         CALL EMS_BEGIN( STATUS )
         CALL HDS_FLUSH( 'PROGRAM', STATUS )
         CALL EMS_END( STATUS )
      ENDIF

*   Force the parameter SDF file to be updated on disk
      CALL EMS_BEGIN( STATUS )
      CALL HDS_FREE ( EXTTOP, STATUS )
      CALL EMS_END( STATUS )

      END
