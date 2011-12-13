      SUBROUTINE CAT_APUT (CI, IAST, STATUS)
*+
*  Name:
*     CAT_APUT
*  Purpose:
*     Put an AST Object to a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_APUT (CI, IAST; STATUS)
*  Description:
*     Put an AST Object to a catalogue.
*
*     The object is stored as textual information.
*  Arguments:
*     CI = INTEGER (Given)
*        Catalogue identifier.
*     IAST = INTEGER (Given)
*        An AST pointer to the given object.  If AST__NULL is supplied
*        the routine returns without doing anything and without
*        reporting an error.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the given AST object is not null then
*       Get the catalogue name (for use in error messages).
*       Get the maximum length of a line of textaul information for this
*       catalogue.
*       Set the effective line length after allowing for the AST
*       signature at the start of the line.
*       Set the catalogue identifier.
*       Create an AST channel to write the AST object to the catalogue.
*       Write the AST object to the channel.
*       Annul the AST channel.
*       Report any error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     DSB: David S. Berry (Starlink)
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/10/99 (ACD): Original version (from KPG1_WCATW).
*     3/11/99  (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'AST_PAR'          ! AST constants and function declarations.
*  Global Variables:
      INCLUDE 'CAT1_AST_CMN'     ! CAT - AST common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  IAST
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      EXTERNAL CAT1_SNKTA
      INTEGER  CHR_LEN
*  Local Variables:
      CHARACTER
     :  CNAME*(CAT__SZCNM)   ! Catalogue name.
      INTEGER
     :  TLNSZE,  ! Size of a line of text.
     :  LASTSG,  ! Length of CAT__ASTSG (AST signature).
     :  ACHAN,   ! AST channel.
     :  DUMMY    ! Dummy returned function value.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check whether the given AST object is null.  If so then the
*       routine simply exits without further action.

         IF (IAST .NE. AST__NULL) THEN

*
*          Get the catalogue name (for use in error messages).

            CALL CAT_TIQAC (CI, 'NAME', CNAME, STATUS)

            IF (STATUS .NE. SAI__OK) THEN
               CNAME = '<unknown>'
            END IF

*
*          Get the maximum length of a line of textual information for
*          this catalogue.  Then compute and set the effective line
*          length, allowing for: the AST signature at the start of the
*          line, the character used to disambiguate new lines from
*          continuation lines and a safety margin of one character.

            CALL CAT_SZTXT (CI, 'WRITE', TLNSZE, STATUS)

            LASTSG = CHR_LEN(CAT__ASTSG)

            TLNSZ__AST = TLNSZE - (LASTSG + 2)

*
*          Set the catalogue identifier.

            CI__AST = CI

*
*          Create an AST Channel to write the AST frame-set to the
*          catalogue.  CAT1_SNKTA is provided as the 'sink' routine for
*          writing the data, and the inclusion of only essential
*          information is specified.

            ACHAN = AST_CHANNEL (AST_NULL, CAT1_SNKTA,
     :        'Full=-1,Comment=0', STATUS)

*
*          Write the supplied AST object to the Channel, thus transferring
*          it to the catalogue.

            DUMMY = AST_WRITE (ACHAN, IAST, STATUS)

*
*          Annul the Channel pointer, thus deleting the Channel.

            CALL AST_ANNUL (ACHAN, STATUS)

*
*          Report any error.

            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC ('CNAME', CNAME)

               CALL ERR_REP ('CAT_APUT_ERR', 'Failed to write AST '/
     :           /'object to catalogue ^CNAME.', STATUS)
            END IF

         END IF

      END IF

      END
