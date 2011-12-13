      SUBROUTINE SUBPAR_ASSOC ( NAMECODE, ACCESS, LOC, STATUS )
*+
*  Name:
*     SUBPAR_ASSOC

*  Purpose:
*     return a locator associated with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ASSOC ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     Given the index of a program parameter, an HDS locator is returned
*     corresponding to the associated data structure.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     ACCESS=CHARACTER*(*) (given)
*        Access mode, 'READ', 'WRITE' or 'UPDATE'
*     LOC=CHARACTER*DAT__SZLOC (returned)
*        Locator to data structure
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     The character string associated with the parameter is
*     obtained, and interpreted as a VMS filename (an HDS container
*     file), followed by the full name of the structure component
*     required. A locator is obtained to the named component.
*     If there is already a locator associated with the parameter, that is
*     used; otherwise SUBPAR_FINDHDS is called to search the VPATH.
*
*     Under error conditions other than PAR__NULL, PAR__ABORT or
*     PAR__NOUSR repeated attempts will be made by SUBPAR_FINDHDS to
*     get a valid locator.

*  Copyright:
*     Copyright (C) 1984, 1988, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     19-SEP-1984 (BDK):
*        Original
*     16-AUG-1988 (AJC):
*        Include SUBPAR_PAR - How did it work before
*     02-FEB-1990 (AJC):
*        Guard against hanging locators
*     16-JUL-1991 (AJC):
*        Use CHR not STR$UCASE
*     10-NOV-1992 (AJC):
*        Report on inconsistent acces mode
*        Remove loop on FINDHDS - relies on FINDHDS to loop
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
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PARERR'


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
      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)

      LOGICAL VALID                        ! .TRUE. => parameter has an
                                           ! HDS locator associated with
                                           ! it

      CHARACTER*6 ACMODE                   ! upper-case version of
                                           ! access mode

*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Initialise locator
*
      LOC = ' '
*
*   Check access mode
*
      ACMODE = ACCESS
      CALL CHR_UCASE ( ACMODE )

      IF ( ( ACMODE .NE. 'READ' ) .AND. ( .NOT. PARWRITE(NAMECODE) ) )
     :  THEN

         STATUS = SUBPAR__ICACM
         CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
         CALL EMS_REP( 'SUP_ASSOC1A',
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
*         NOTE - FINDHDS will search for a valid HDS structure following
*         VPATH and will trap all errors other than PAR__NULL, PAR__ABORT
*         and PAR__NOUSR.
*
            CALL SUBPAR_FINDHDS ( NAMECODE, ACMODE, LOC, STATUS )

         ENDIF
*
*      If successful, mark the parameter as active
*
         IF ( STATUS .EQ. SAI__OK ) THEN

            PARSTATE(NAMECODE) = SUBPAR__ACTIVE

         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN

            PARSTATE(NAMECODE) = SUBPAR__NULL

         ELSE

            PARSTATE(NAMECODE) = SUBPAR__CANCEL

         ENDIF


      ENDIF

      END
