      SUBROUTINE SUBPAR_LOADIFC ( LUCON, STATUS )
*+
*  Name:
*     SUBPAR_LOADIFC

*  Purpose:
*     Read compiled interface file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_LOADIFC ( LUCON, STATUS )

*  Description:
*     The compiled version of the interface file is read into the common
*     block variables.

*  Arguments:
*     LUCON=INTEGER (given)
*        unit number for FORTRAN read
*     STATUS=INTEGER

*  Algorithm:
*     The first two records contain the program name and the search-path
*     for the .EXE file.
*     The third record in the file contains the pointers indicating how
*     many values were stored in each array. This record is read into
*     the BYTE array equivalenced to the pointers.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     09-OCT-1984 (BDK):
*        Original
*     27-FEB-1985 (BDK):
*        read PROGNAME and EXEPATH from .IFC
*     23-AUG-1985 (BDK):
*        read PROGADD - for monoliths
*     20-SEP-1985 (BDK):
*        trap i/o error
*     11-NOV-1985 (BDK):
*        read PARLIT - flag for LITERAL parameters
*     14-MAR-1986 (BDK):
*        read PARMENU, PARCOORDS, ACTHELP, AVTKEY, ACTMENU,
*        ACTCOORDS
*     05-MAY-1987 (BDK):
*        read PARPPATH
*     21-MAY-1990 (AJC):
*        read PARHKEY and allow for its absence
*     03-JUL-1991 (AJC):
*        split to handle new (packed) or old-style modules
*        also modify error handling
*     19-JUL-1991 (AJC):
*        expand out PTRIFL for portability
*     29-JAN-1992 (AJC):
*        remove unused declarations
*     14-JUL-1992 (AJC):
*        initialize PARDYN and MIN/MAX arrays
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
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
      INTEGER LUCON                ! FORTRAN unit for input


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      INTEGER IOSTAT              ! I/O status
      INTEGER I                   ! Loop control

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Read the program name and the .EXE search-path
      READ ( LUCON, IOSTAT=IOSTAT ) PROGNAME
      IF ( IOSTAT .EQ. 0 ) THEN
         READ ( LUCON, IOSTAT=IOSTAT ) EXEPATH
         IF( IOSTAT .EQ. 0 ) THEN
*        Read the set of pointers indicating how many values were stored by
*        the interface parser in the arrays.
            READ ( LUCON, IOSTAT=IOSTAT ) PARPTR, ACTPTR, NEEDPTR,
     :      INTPTR, REALPTR, DOUBLEPTR, CHARPTR, LOGPTR,
     :      FACENAME, PROGNAME, EXEPATH, MONOLITH, INT64PTR
         ENDIF
      ENDIF

      IF ( IOSTAT .NE. 0 ) THEN
*     Failed to read the start of the file
         STATUS = SUBPAR__BADIFC
         CALL EMS_REP( 'SUP_LOADIFC1',
     :   'SUBPAR: Error reading interface module', STATUS )
         CALL EMS_FIOER( 'IOSTAT', IOSTAT )
         CALL EMS_REP( 'SUP_LOADIFC2',
     :   '^IOSTAT', STATUS )

      ELSE
*     Determine the type of .IFC
         IF ( EXEPATH(132:132) .EQ. '1' ) THEN
*        New-style with run length encoding
            CALL SUBPAR_LDIFC1( LUCON, STATUS )

         ELSE
*        Old-style
            CALL SUBPAR_LDIFC0( LUCON, STATUS )

         ENDIF

         IF ( PARPTR .GE. 1 ) THEN
            DO I = 1, PARPTR
*           Initialize PARDYN pointers
               PARDYN(1,I) = 0
*           Initialize PARMIN/MAX
               PARMIN(2,I) = -1
               PARMAX(2,I) = -1
            ENDDO

         ENDIF

      ENDIF

      END
