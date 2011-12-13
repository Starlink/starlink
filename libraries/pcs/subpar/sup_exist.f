      SUBROUTINE SUBPAR_EXIST ( NAMECODE, ACCESS, LOC, STATUS )
*+
*  Name:
*     SUBPAR_EXIST

*  Purpose:
*     return a locator associated with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_EXIST ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     Given the index of a program parameter, an HDS name is associated
*     with it. An attempt is made to get an HDS locator corresponding to
*     the associated data structure. Under error conditions the relevant
*     status is returned.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     ACCESS=CHARACTER*(*) (given)
*        Access mode, 'READ', 'WRITE' or 'UPDATE'
*     LOC=CHARACTER*DAT__SZLOC (returned)
*        Locator to data structure
*     STATUS=INTEGER
*        Status return
*        PAR__ERROR => name got ok, but failed to get locator.

*  Algorithm:
*     A valid HDS name is obtained and associated with the parameter. An
*     attempt is then made to get a locator to the named object. If this
*     succeeds, the locator is returned. If it fails, status PAR__ERROR
*     is returned, BUT the parameter is left in the ACTIVE state and the
*     HDS name remains associated with it.

*  Deficiencies:
*     Sets a PAR error code PAR__ERROR. Assume can't change this now as
*     users of DAT_EXIST may be testing for it. Also doesn't report an
*     error when PAR__ERROR is set. HDS may already have done so but it
*     is not a true error and reporting may cause problems if user does
*     not annul errors. This probaly needs further sorting out.

*  Copyright:
*     Copyright (C) 1984, 1986, 1987, 1988, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     26-SEP-1984 (BDK):
*        Original
*     13-MAR-1986 (BDK):
*        total rewrite with different behaviour
*     17-NOV-1987 (BDK):
*        return PAR__ERROR only if file/object not found
*     16-DEC-1988 (AJC):
*        avoid calling SUBPAR_GETHDS as this now saves current
*        value as does SUBPAR_GETNAME
*     16-JUL-1991 (AJC):
*         use CHR not STR$ for portability
*     10-NOV-1992 (AJC):
*         Use SUBPAR__ICACM and report a message
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*      3-FEB-2000 (AJC):
*        Use SUBPAR_PARGP to get an HDS group name for the parameter
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'DAT_ERR'


*  Arguments Given:
      INTEGER NAMECODE             ! Number of program parameter

      CHARACTER*(*) ACCESS         ! Access mode, 'READ', 'WRITE'
                                   ! or 'UPDATE'


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC   ! Locator to data structure


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*(DAT__SZLOC) FILOC         ! HDS locator (temporary)

      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)

      LOGICAL VALID                        ! .TRUE. => parameter has an
                                           ! HDS locator associated with
                                           ! it

      CHARACTER*6 ACMODE                   ! upper-case version of
                                           ! access mode

      CHARACTER*132 STRUCTNAME             ! name of HDS structure

*.


      IF ( STATUS .NE. SAI__OK ) RETURN


*
*   Check access mode
*
      ACMODE = ACCESS
      CALL CHR_UCASE( ACMODE )

      IF ( ( ACMODE .NE. 'READ' ) .AND. ( .NOT. PARWRITE(NAMECODE) ) )
     :  THEN

         STATUS = SUBPAR__ICACM
         CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
         CALL EMS_REP( 'SUP_EXIST1',
     :   'SUBPAR: Parameter ^PARAM - Cannot open for ''WRITE''',
     :    STATUS )
         CALL EMS_REP( 'SUP_ASSOC1B',
     :   'Parameter defined ''ACCESS READ'' in the interface file',
     :    STATUS )

      ELSE
*
*      If the parameter already has a locator associated with it, clone it
*      and associate the new locator with the parameter name inside HDS, so
*      that DAT_CANCL can be used to annul the locator without knowing
*      its value.
*      NB. The locator returned MAY be pointing to the temporary HDS
*      storage for parameter values - see PAR_PUT0x and PAR_GET0x
*
         CALL SUBPAR_GETLOC ( NAMECODE, VALID, BOTLOC, STATUS )

         IF ( VALID ) THEN

            CALL DAT_CLONE ( BOTLOC, LOC, STATUS )
            CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )

         ELSE
*
*         The association has not been made previously.
*         Get a file/structure name for the named parameter and obtain a
*         corresponding locator.
*         NOTE - GETNAME will search for a valid HDS structure following
*         VPATH.
*
            CALL SUBPAR_GETNAME ( NAMECODE, STRUCTNAME, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*
*            Get the HDS locators to the top and bottom level objects for the
*            (named data structure.
*
               CALL SUBPAR_HDSLOCS ( STRUCTNAME, ACMODE,
     :                               FILOC, BOTLOC, STATUS )
*
*            Store internally the topmost-level locator ( so that the file
*            can be closed when necessary ), and the bottom-level locator.
*            Take a copy of the bottom-level locator, which is to be
*            returned to the calling routine.
*
               CALL SUBPAR_PUTFLOC ( NAMECODE, FILOC, STATUS )
               CALL SUBPAR_PUTLOC ( NAMECODE, BOTLOC, STATUS )
               CALL DAT_CLONE ( BOTLOC, LOC, STATUS )

*
*            Associate the parameter name with both copies of the
*            bottom-level locator within HDS so that DAT_CANCL can be used
*            to annul the locator without knowing its value.
*            -- NB this must not be done with FILOC - it must only be
*            annulled via a call to HDS_CLOSE.
*
               CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )
               CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )
*
               IF ( ( STATUS .EQ. DAT__FILNF ) .OR.
     :           ( STATUS .EQ. DAT__OBJNF ) ) THEN
                  STATUS = PAR__ERROR
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      END
