      SUBROUTINE CAT_AGET (CI, IAST, STATUS)
*+
*  Name:
*     CAT_AGET
*  Purpose:
*     Get an AST Object from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_AGET (CI; IAST; STATUS)
*  Description:
*     Get an AST Object from a catalogue.
*
*     If there is no AST object stored with the catalogue then a
*     default one is created.
*
*     This routine accesses the textual information of the catalogue
*     (which is used to store the AST object).  If the routine exits
*     successfully then access to the textual information is reset to
*     the start.  If it aborts with a bad status then the current
*     position in the textual information is indeterminate.
*  Arguments:
*     CI = INTEGER (Given)
*        Catalogue identifier.
*     IAST = INTEGER (Returned)
*        An AST pointer to the returned object.  AST__NULL is returned if
*        an error occurs.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the returned AST pointer.
*     Get the catalogue name (for use in error messages).
*     Reset access to the textual information.
*     Initialise the 'AST object found' flag.
*     Create an AST channel through which text from the catalogue can
*     be read.
*     Initialise prior to reading from the AST channel.
*     Attempt to read the AST object from the channel.
*     Annul the AST channel.
*     If an object was found then
*       Copy the identifier to the return variable.
*     else
*       Attempt to create an AST object.
*     end if
*     Reset access to the textual information.
*     Report any error.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/10/99 (ACD): Original version.
*     4/11/99  (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'AST_PAR'          ! AST constants and function declarations.
*  Global Variables:
      INCLUDE 'CAT1_AST_CMN'     ! CAT - AST common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Arguments Returned:
      INTEGER
     :  IAST
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      EXTERNAL CAT1_SRCTA
*  Local Variables:
      CHARACTER
     :  CNAME*(CAT__SZCNM)       ! Catalogue name.
      INTEGER
     :  ACHAN,   ! AST channel.
     :  WRKAST   ! Working copy of AST identifier.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the returned AST pointer.

         IAST = AST__NULL

*
*       Get the catalogue name (for use in error messages).

         CALL CAT_TIQAC (CI, 'NAME', CNAME, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CNAME = '<unknown>'
         END IF

*
*       Reset access to the textual information.

         CALL CAT_RSTXT (CI, STATUS)

*
*       Initialise the 'AST object found' flag to indicate that an
*       AST object has not yet been found.

         FND__AST = .FALSE.

*
*       Create an AST Channel through which text stored in the catalogue
*       can be read, and an AST Object extracted.  The subroutine
*       CAT1_SRCTA extracts the text from the catalogue, concatenates
*       continuation lines, and supplies the assembled line to the AST
*       library.  Textual information not related to AST is skipped over.

         ACHAN = AST_CHANNEL(CAT1_SRCTA, AST_NULL, 'SKIP=1', STATUS)

*
*       Initialise prior to reading from the AST channel.
*       Set the catalogue to be used and initialise the current AST line.
*       A fake initial current line is created, which AST will
*       subsequently ignore.

         CI__AST = CI

         LINE__AST = 'Fake intial line'
         LLINE__AST = 16

*
*       Read the AST object from the channel.

         WRKAST = AST_READ(ACHAN, STATUS)

*
*       Annul the AST channel.

         CALL AST_ANNUL (ACHAN, STATUS)

*
*       If an object was found then copy the identifier to the return
*       variable, otherwise attempt to create an AST object.

         IF (FND__AST) THEN
            IAST = WRKAST
         ELSE
            CALL CAT1_ANEW (CI, IAST, STATUS)
         END IF

*
*       Reset access to the textual information.

         CALL CAT_RSTXT (CI, STATUS)

*
*       Report any error.  If an error occurred then the returned AST
*       pointer is explicitly set to null.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('CNAME', CNAME)

            CALL ERR_REP ('CAT_AGET_ERR', 'Failed to return an AST '/
     :        /'object for catalogue ^CNAME.', STATUS)


            IAST = AST__NULL
         END IF

      END IF

      END
