      SUBROUTINE CAT5_PUTXT (CI, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT5_PUTXT
*  Purpose:
*     Put a line of textual information to a small text list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_PUTXT (CI, CLASS, TEXT; STATUS)
*  Description:
*     Put a line of textual information to a small text list.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  The classes permitted for
*         putting text are:
*         COMMENT  -  intended for general comments,
*         HISTORY  -  intended for history information.
*     TEXT   =  CHARACTER*(*) (Given)
*        A single line of textual information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the class is 'HISTORY' or 'COMMENT' then
*       Get the array element for the catalogue.
*       Get the Fortran unit number for the table.
*       Determine whether the catalogue is a standard STL or a
*       KAPPA-format one.
*       Attempt to write the line to the description file.
*       Report any error.
*     else
*       Set the status.
*       Report error: attempt to put an line of text with an illegal
*       class.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     26/7/96  (ACD): Original version.
*     10/12/96 (ACD): Added writing 'KAPPA format' STLs.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT5_STL_CMN'      ! STL back-end common block.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  CLASS*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS    ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  DFUNIT,  ! Fortran unit number for the description file.
     :  LSTAT,   ! Fortran I/O status.
     :  LCLASS,  ! Length of CLASS  (excl. trail. blanks).
     :  ERRLEN   !   "    "  ERRTXT ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75     ! Error message text.
      LOGICAL
     :  KFLAG    ! Flag; is STL in 'KAPPA format' or standard.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the class for the line of text is one of the
*       permitted classes.

         IF (CLASS .EQ. 'COMMENT'  .OR.  CLASS .EQ. 'HISTORY') THEN

*
*          Get the array element for the catalogue and the Fortran
*          unit number for the description file.

            CALL CAT1_CIELM (CI, CIELM, STATUS)

            DFUNIT = STUNT__CAT5(CIELM)

*
*          Determine whether the catalogue is a standard STL or a
*          KAPPA-format one.

            KFLAG = KFLAG__CAT5(CIELM)

*
*          Attempt to write the line of text and report any error.

            IF (.NOT. KFLAG) THEN
               WRITE(DFUNIT, 2000, IOSTAT=LSTAT) TEXT
 2000          FORMAT('T ', A)
            ELSE
               WRITE(DFUNIT, 2001, IOSTAT=LSTAT) TEXT
 2001          FORMAT('#T ', A)
            END IF

            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT5_PUTXT_ERR', 'Error writing '/
     :           /'line of text to description file.', STATUS)
            END IF

         ELSE

*
*          The given class does not correspond to one of the permitted
*          classes.  Set the status and report an error.

            STATUS = CAT__ERROR

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Text class supplied, ', ERRTXT, ERRLEN)

            IF (CLASS .NE. ' ') THEN
               LCLASS = CHR_LEN(CLASS)
               CALL CHR_PUTC (CLASS(1 : LCLASS), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC (', is illegal.  Line of text not written.',
     :        ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT5_PUTXT_ICL', ERRTXT(1 : ERRLEN),
     :        STATUS)

         END IF

      END IF

      END
