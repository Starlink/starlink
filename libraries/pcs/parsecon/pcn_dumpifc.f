      SUBROUTINE PARSECON_DUMPIFC ( LUCON, STATUS )
*+
*  Name:
*     PARSECON_DUMPIFC

*  Purpose:
*     Write compiled interface file.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_DUMPIFC ( LUCON, STATUS )

*  Description:
*     The compiled version of the interface file is written to disk.

*  Arguments:
*     LUCON=INTEGER (given)
*        unit number for FORTRAN write
*     STATUS=INTEGER

*  Algorithm:
*     The first two records contain the program name and the .EXE
*     search-path.
*     The third record in the file contains the pointers indicating how
*     many values were stored in each array. This record is written
*     using the BYTE array equivalenced to the pointers.

*  Implementation Deficiencies:
*     The parameter full help specifiers are written on the end of the
*     file so that the reader can just fail on that read if an old-style
*     .ifc is presented. Thus the need to re-compile all interface files
*     is avoided.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1990, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     09.10.1984:  Original (REVAD::BDK)
*     27.02.1985:  output PROGNAME and EXEPATH (REVAD::BDK)
*     23.08.1985:  include PROGADD (REVAD::BDK)
*     11.11.1985:  output PARLIT (REVAD::BDK)
*     13.05.1986:  output PARMENU, PARCOORDS, ACTHELP, ACTKEY, ACTMENU,
*        ACTCOORDS (REVAD::BDK)
*     05.05.1987:  output PARPPATH (REVAD::BDK)
*     04.07.1990:  add PARHKEY (RLVAD::AJC)
*     04.07.1991:  produce run-length encoded version of .IFC to decrease
*        size of file (RLVAD::AJC)
*     02.10.1991:  expand out PTRIFL for portability (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     28.06.1995:  Use PACKL for PARWRITE, not PACKI (RLVAD::AJC)
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
      INTEGER LUCON                ! FORTRAN unit for output


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      INTEGER I
      INTEGER J

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Write the program name
*
      WRITE ( LUCON ) PROGNAME
*
*   Write search path
*   This is not used for its original purpose and has been hijacked
*   to flag a new-style .IFC
*   If last character is '1' run length encoded .IFC
      EXEPATH(132:132) = '1'
      WRITE ( LUCON ) EXEPATH
*
*   Write the set of pointers indicating how many values were stored by
*   the interface parser in the arrays.
*
      WRITE ( LUCON ) PARPTR, ACTPTR, NEEDPTR,
     :      INTPTR, REALPTR, DOUBLEPTR, CHARPTR, LOGPTR,
     :      FACENAME, PROGNAME, EXEPATH, MONOLITH, INT64PTR

*
*   Write the data in the arrays.
*
      IF ( PARPTR .GT. 0 ) THEN

         WRITE ( LUCON ) ( PARLEN(J), J=1,PARPTR )

         WRITE ( LUCON ) ( PARTYPE(J), J=1,PARPTR )

         CALL PARSECON_PACKL( LUCON, PARWRITE, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 3, PARLIMS, 1, PARPTR, STATUS )

         CALL PARSECON_PACKL( LUCON, PARCONT, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 3, PARDEF, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, PARASSOC, 1, PARPTR, STATUS )

         CALL PARSECON_PACKI( LUCON, PARPOS, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, PARRPATH, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNB( LUCON, 5, PARVPATH, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARHELP, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARNAMES, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARPROM, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARKEY, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARPTY, 1, PARPTR, STATUS )

         CALL PARSECON_PACKL( LUCON, PARLIT, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARMENU, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, PARCOORDS, 1, PARPTR, STATUS )

         CALL PARSECON_PAKNB( LUCON, 5, PARPPATH, 1, PARPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, PARHKEY, 1, PARPTR, STATUS )

      ENDIF

      IF ( ACTPTR .GT. 0 ) THEN

         CALL PARSECON_PACKC( LUCON, ACTNAMES, 1, ACTPTR, STATUS )

         WRITE ( LUCON ) ( ACTLEN(J), J=1,ACTPTR )

         CALL PARSECON_PACKL( LUCON, MAYOB, 1, ACTPTR, STATUS )

         CALL PARSECON_PACKL( LUCON, MAYCAN, 1, ACTPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, NEEDOB, 1, ACTPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, NEEDCAN, 1, ACTPTR, STATUS )

         WRITE ( LUCON ) ( ( PROGADD(I,J), I=1,2 ), J=1,ACTPTR )

         CALL PARSECON_PACKC( LUCON, ACTHELP, 1, ACTPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, ACTKEY, 1, ACTPTR, STATUS )

         CALL PARSECON_PACKC( LUCON, ACTMENU, 1, ACTPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, ACTCOORDS, 1, ACTPTR, STATUS )

      ENDIF

      IF ( NEEDPTR .GT. 0 ) THEN

         CALL PARSECON_PACKI( LUCON, NEEDPAR, 1, NEEDPTR, STATUS )

         CALL PARSECON_PAKNI( LUCON, 2, NEEDLIMS, 1, NEEDPTR, STATUS )

         CALL PARSECON_PACKL( LUCON, NEEDCONT, 1, NEEDPTR, STATUS )

      ENDIF

      IF ( DOUBLEPTR .GT. 0 ) THEN
         WRITE ( LUCON ) ( DOUBLELIST(J), J=1,DOUBLEPTR )

      ENDIF

      IF ( INTPTR .GT. 0 ) THEN
         WRITE ( LUCON ) ( INTLIST(J), J=1,INTPTR )
      ENDIF

      IF ( INT64PTR .GT. 0 ) THEN
         WRITE ( LUCON ) ( INT64LIST(J), J=1,INT64PTR )
      ENDIF

      IF ( REALPTR .GT. 0 ) THEN
         WRITE ( LUCON ) ( REALLIST(J), J=1,REALPTR )
      ENDIF

      IF ( CHARPTR .GT. 0 ) THEN
         CALL PARSECON_PACKC( LUCON, CHARLIST, 1, CHARPTR, STATUS )
      ENDIF

      IF ( LOGPTR .GT. 0 ) THEN
         WRITE ( LUCON ) ( LOGLIST(J), J=1,LOGPTR )
      ENDIF

      END

