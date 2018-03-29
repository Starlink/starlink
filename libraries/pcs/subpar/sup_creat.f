      SUBROUTINE SUBPAR_CREAT ( NAMECODE, TYPE, NDIMS, DIMS, STATUS )
*+
*  Name:
*     SUBPAR_CREAT

*  Purpose:
*     create a data structure component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CREAT ( NAMECODE, TYPE, NDIMS, DIMS, STATUS )

*  Description:
*     Given the number of a program parameter, an HDS data structure
*     component is created, as specified by the associated character
*     string and the given type and dimensionality.
*     This is a simulation of the SSE routine, implemented for ADAM.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        number of program parameter
*     TYPE=CHARACTER*(*) (given)
*        Type of HDS component. This may be a primitive type or a
*        structure
*     NDIMS=INTEGER (given)
*        Number of dimensions of the component
*     DIMS(*)=INTEGER (given)
*        Dimensions of the component
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     The character string associated with the given parameter is
*     obtained, and interpreted as a VMS filename (an HDS container
*     file), followed by the full name of the structure component
*     required. The component is created if possible. The data structure
*     down to the level immediately above the required new component
*     must exist already.

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     17-AUG-1984 (BDK):
*        Original
*     05-FEB-1985 (BDK):
*        Remove explicit setting of PARSTATE as this is done
*        elsewhere
*     10-MAY-1985 (BDK):
*        handle array components
*     05-SEP-1985 (BDK):
*        correct arguments to HDSOPEN
*     08-MAY-1987 (BDK):
*        check write access for parameter
*     02-FEB-1990 (AJC):
*        comment guard against hanging locators
*     15-NOV-1990 (AJC):
*        use COMPONENT(1) not FILENAME in HDS_OPEN
*        ensure file is closed after error
*     03-AUG-1991 (AJC):
*        first translate the filename
*     10-NOV-1992 (AJC):
*        Use SUBPAR errors for PAR_ICACM and DAT_COMEX
*        Add error reports
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     19-MAR-2018 (DSB):
*        Cater for long file names (<= 1000).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      INTEGER NAMECODE             ! pointer to program parameter

      CHARACTER*(*) TYPE           ! Type of the required component.
                                   ! primitives such as '_REAL' etc.
                                   ! or some freely-defined structure
                                   ! type

      INTEGER NDIMS                ! Number of dimensions of the component

      INTEGER DIMS(*)              ! Dimensions of the component


*  Status:
      INTEGER STATUS

*    Global variable :
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*1000 FILENAME              ! name of VMS container file

      CHARACTER*1000 FULNAM                ! expanded filename

      INTEGER NAMLEN                       ! used length of FULNAM

      CHARACTER*1000 STRUCTNAME            ! character string associated
                                           ! with named parameter

      CHARACTER*15 COMPONENT(30)           ! names of the levels in the
                                           ! data structure

      INTEGER NUMLEVS                      ! number of levels in the
                                           ! named structure

      CHARACTER*(DAT__SZLOC) FILOC         ! HDS locator of container
                                           ! file

      CHARACTER*(DAT__SZLOC) TOPLOC        ! HDS locator (temporary)

      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)

      LOGICAL VALID                        ! .TRUE. => parameter is
                                           ! marked as having valid HDS
                                           ! locators associated

      INTEGER LEVEL                        ! structure-level loop count

      INTEGER ISTAT                        ! local status


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the parameter has a locator associated with it, then this is an
*  error.
      CALL SUBPAR_GETLOC ( NAMECODE, VALID, BOTLOC, STATUS )

      IF ( VALID ) THEN

         STATUS = SUBPAR__INVST
         CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
         CALL EMS_REP ( 'SUP_CREAT1A',
     :   'SUBPAR: Parameter ^PARAM - Cannot create object', STATUS )
         CALL EMS_REP ( 'SUP_CREAT1B',
     :   '        Parameter already has an object associated',
     :    STATUS )

      ELSE IF ( .NOT. PARWRITE(NAMECODE) ) THEN

         STATUS = SUBPAR__ICACM
         CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
         CALL EMS_REP ( 'SUP_CREAT2A',
     :   'SUBPAR: Parameter ^PARAM - Cannot create object', STATUS )
         CALL EMS_REP ( 'SUP_CREAT2B',
     :   '        ''ACCESS READ'' in the interface file', STATUS )
      ELSE

*     The association has not been made previously.
*     Get the file/structure name associated with the named
*     parameter.
*     NOTE - if there isn't a current name, GETNAME will go looking for
*     one following VPATH.
         CALL SUBPAR_GETNAME ( NAMECODE, STRUCTNAME, STATUS )

*     Split the name up into a VMS filename, followed by a set of
*     component names leading down the hierarchy to the part of the
*     structure required.
         CALL SUBPAR_SPLIT ( STRUCTNAME, 30, NUMLEVS, COMPONENT,
     :     FILENAME, STATUS )

*     If the top-level of the file has been specified, then create the
*     container file and top-level object.
         IF ( NUMLEVS .EQ. 1 ) THEN

*        first translate the filename
            CALL SUBPAR_FNAME ( FILENAME, FULNAM, NAMLEN, STATUS )
            CALL HDS_NEW ( FULNAM(1:NAMLEN), COMPONENT(1), TYPE, NDIMS,
     :                     DIMS, FILOC, STATUS )
            CALL HDS_CLOSE ( FILOC, STATUS )

         ELSE

*        Open the container file. TOPLOC will be a clone of FILOC
*        unless the top-level is an array, a component of which has
*        been specified.
*        _HDSOPEN will nullify the locators if it fails.
            CALL SUBPAR_HDSOPEN ( FILENAME, COMPONENT(1), 'UPDATE',
     :         FILOC, TOPLOC, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*           Move down to the lower levels of the structure
               BOTLOC = TOPLOC

               DO LEVEL = 2, NUMLEVS - 1

                  CALL SUBPAR_DATFIND ( TOPLOC, COMPONENT(LEVEL),
     :                                  BOTLOC, STATUS )

*              _DATFIND will nullify BOTLOC if it fails
                  CALL DAT_ANNUL ( TOPLOC, STATUS )
                  TOPLOC = BOTLOC

               ENDDO

               IF ( STATUS .EQ. SAI__OK ) THEN

*              Create the bottom-level object
                  CALL DAT_NEW ( BOTLOC, COMPONENT(NUMLEVS), TYPE,
     :                           NDIMS, DIMS, STATUS )

*              Annul the locator.
                  CALL DAT_ANNUL ( BOTLOC, STATUS )

               ENDIF

*           Close the container file
*           Ensure HDS_CLOSE works even if STATUS bad
               ISTAT = SAI__OK
               CALL HDS_CLOSE ( FILOC, ISTAT )

            ENDIF

         ENDIF

      ENDIF

      END
