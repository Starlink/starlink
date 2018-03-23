      SUBROUTINE SUBPAR_HDSLOCS ( STRUCTNAME, ACCESS, FILOC, BOTLOC,
     :  STATUS )
*+
*  Name:
*     SUBPAR_HDSLOCS

*  Purpose:
*     Get HDS locators given full structure-name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HDSLOCS ( STRUCTNAME, ACCESS, FILOC, BOTLOC,

*  Description:
*     Given the full name of an HDS structure, open the container file
*     and return the file locator and the locator to the bottom-level
*     object specified.
*     In the event of a failure, annul and nullify locators.

*  Arguments:
*     STRUCTNAME=CHARACTER*(*) (given)
*        full structure name - eg "dba1::[junk.sub]jn.sdf"a.b.c
*     ACCESS=CHARACTER*(*) (given)
*        access mode - 'READ', 'WRITE', or 'UPDATE'
*     FILOC=CHARACTER*(DAT__SZLOC) (returned)
*        HDS locator to container file
*     BOTLOC=CHARACTER*(DAT__SZLOC) (returned)
*        HDS locator to lowest-level object specified
*     STATUS=INTEGER

*  Algorithm:
*     The given structure-name is split into the container name plus
*     HDS components. The container is opened, and the structure
*     followed down to the component specified.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1989, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1984 (BDK):
*        Original
*     10-MAY-1985 (BDK):
*        Handle arrays of structures
*     13-MAR-1986 (BDK):
*        on error, ensure file is closed
*     02-SEP-1986 (BDK):
*        increase size of component names
*     01-FEB-1989 (AJC):
*        nullify locators if it fails
*     31-JUL-1991 (AJC):
*        EMS error reporting
*     24-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     25-MAR-2005 (TIMJ):
*        Increase the maximum filename size from 80 to 256.
*        There should be a single include file for this definition.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      CHARACTER*(*) STRUCTNAME           ! full structure name

      CHARACTER*(*) ACCESS               ! access mode


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) FILOC       ! locator to container

      CHARACTER*(DAT__SZLOC) BOTLOC      ! locator to object


*  Status:
      INTEGER STATUS


*  Local Constants:
      INTEGER MAXLEVS               ! maximum number of levels in
                                    ! structure-name
      PARAMETER ( MAXLEVS = 16 )


*  Local Variables:
      CHARACTER*32 COMPONENT(MAXLEVS) ! components of structure-name

      INTEGER NUMLEVS               ! number of levels in structure-name

      CHARACTER*1024 FILENAME        ! name of container-file

      CHARACTER*(DAT__SZLOC) TOPLOC ! temporary locator

      INTEGER LEVEL                 ! counter for levels in structure

      INTEGER ISTAT                 ! internal status

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Split the name up into its components
*
      CALL SUBPAR_SPLIT ( STRUCTNAME, MAXLEVS, NUMLEVS, COMPONENT,
     :  FILENAME, STATUS )

*
*   Open the HDS container file. Normally BOTLOC will be returned as a
*   clone of FILOC. They differ if the top object is an array.
*   HDS_OPEN will nullify locators if it fails
*
      CALL SUBPAR_HDSOPEN ( FILENAME, COMPONENT(1), ACCESS, FILOC,
     :  BOTLOC, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*     See if a search is required
         IF ( NUMLEVS .GT. 1 ) THEN

*        Move down to the required component. Any individual component may
*        be an element of a structure array.
            TOPLOC = BOTLOC

            LEVEL = 2

*        Set new error context
            CALL EMS_MARK

*        and search
            DOWHILE ( ( LEVEL .LE. NUMLEVS )
     :          .AND. ( STATUS .EQ. SAI__OK ) )

*           DATFIND will nullify BOTLOC if it fails
               CALL SUBPAR_DATFIND ( TOPLOC, COMPONENT(LEVEL), BOTLOC,
     :           STATUS )
               CALL DAT_ANNUL ( TOPLOC, STATUS )
               TOPLOC = BOTLOC

               LEVEL = LEVEL + 1

            ENDDO

            IF ( STATUS .NE. SAI__OK ) THEN
               ISTAT = SAI__OK
               CALL HDS_CLOSE ( FILOC, ISTAT )
*           Nullify locator
               FILOC = ' '
*           and ignore HDS error reports
               CALL EMS_ANNUL ( ISTAT )

*           Add report for SUBPAR
               CALL EMS_SETC ( 'STRUC', STRUCTNAME )



               CALL EMS_SETC ( 'COMP', COMPONENT(LEVEL-1) )
               CALL EMS_REP ( 'SUP_HDSLOCS',
     :         'SUBPAR: Error finding component ''^COMP'' in ^STRUC',
     :          STATUS )
            ENDIF

*        Release error context
            CALL EMS_RLSE

         ENDIF

      ENDIF

      END
