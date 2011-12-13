      SUBROUTINE PARSECON_SETVP ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETVP

*  Purpose:
*     Sets-up parameter vpath.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETVP ( ENTRY, STATUS )

*  Description:
*     Interprets the provided string as a VPATH specification,
*     and adds it into the VPATH store for the most recently declared
*     program parameter.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        VPATH specifier
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, and the
*     result is interpreted as a set of path specifiers which are encoded
*     into the array holding VPATH.

*  Copyright:
*     Copyright (C) 1984, 1985, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     19.09.1984:  Original (REVAD::BDK)
*     01.03.1985:  make 'INTERNAL' generate full path (REVAD::BDK)
*     15.05.1990:  correct heading comment (RLVAD::AJC)
*     16.10.1990:  use CHR for upper case (RLVAD::AJC)
*     25.06.1991:  STRING_ARRCHAR changed to PARSECON_ARRCHAR (RLVAD::AJC)
*     25.02.1991:  Report errors (RLVAD::AJC)
*     26.02.1992:  _ARRCHAR no longer capitalizes (RLVAD::AJC)
*        7.09.1992:  remove superfluous CHR_UCASE
*                    improve error report (RLVAD::AJC)
*        9.09.1992:  make precedence dynamic, default to match FETCH (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     01.02.2004:  Added to CVS repository cvs.starlink.ac.uk.  See there
*        for further changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'


*  Arguments Given:
      CHARACTER*(*) ENTRY             ! the VPATH string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External References:
*     None


*  Local Variables:
      CHARACTER*80 VALUE              ! VPATH string with quotes removed

      INTEGER POS                     ! loop counter for VPATH step

      CHARACTER*15 STEPS(5)           ! split VPATH string

      INTEGER LENSTEPS(5)             ! length of STEPS strings

      INTEGER COUNT                   ! number of steps

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( PARPTR .GT. SUBPAR__MAXPAR ) THEN
*      We've bust the array bounds
         STATUS = PARSE__NOMEM
         CALL EMS_SETI ( 'MAXVP', SUBPAR__MAXPAR )
         CALL EMS_REP ( 'PCN_SETVP2',
     :        'PARSECON: Too many vpaths: maximum ^MAXVP',
     :        STATUS )
         RETURN                 ! JUMP OUT
      ENDIF

*   Remove the quotes from ENTRY and force to uppercase.
      CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
      CALL CHR_UCASE( VALUE )

*   Split the string up into the set of path specifiers
      CALL PARSECON_ARRCHAR ( VALUE, 5, COUNT, STEPS, LENSTEPS,
     : STATUS )

*   Blank the search path for the latest parameter
      DO POS = 1, 5

         PARVPATH(POS,PARPTR) = SUBPAR__NOPATH

      ENDDO

*   Load encoded version of the VPATH
      IF ( STEPS(1) .EQ. 'INTERNAL' ) THEN

*      Load the search-path INTERNAL, DYNAMIC, DEFAULT, NOPROMPT
         PARVPATH(1,PARPTR) = SUBPAR__INTERNAL
         PARVPATH(2,PARPTR) = SUBPAR__DYNAMIC
         PARVPATH(3,PARPTR) = SUBPAR__DEFAULT
         PARVPATH(4,PARPTR) = SUBPAR__NOPROMPT

      ELSE

*      Load the given search-path
         DO POS = 1, COUNT
            IF ( STEPS(POS) .EQ. 'CURRENT' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__CURRENT
            ELSE IF ( STEPS(POS) .EQ. 'DEFAULT' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__DEFAULT
            ELSE IF ( STEPS(POS) .EQ. 'DYNAMIC' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__DYNAMIC
            ELSE IF ( STEPS(POS) .EQ. 'GLOBAL' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__GLOBAL
            ELSE IF ( STEPS(POS) .EQ. 'NOPROMPT' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__NOPROMPT
            ELSE IF ( STEPS(POS) .EQ. 'PROMPT' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__PROMPT
            ELSE IF ( STEPS(POS) .EQ. 'INTERNAL' ) THEN
               PARVPATH(POS,PARPTR) = SUBPAR__INTERNAL
            ELSE
               STATUS = PARSE__IVVPATH
               CALL EMS_SETC( 'ITEM', ENTRY )
               CALL EMS_REP ( 'PCN_SETVP1',
     :         'PARSECON: Illegal item in VPATH ^ITEM', STATUS )
            ENDIF

         ENDDO

      ENDIF

      END
