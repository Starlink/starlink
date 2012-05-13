      SUBROUTINE KPS1_PUTVL( OBJLOC, TYPE, NDIM, DIMS, SIZE, NLINES,
     :                        INDENT, ONEPLN, LOGEXM, FD, LINE, LENG,
     :                        STATUS )
*+
*  Name:
*     KPS1_PUTVL

*  Purpose:
*     Appends the values of a primitive object to a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PUTVL( OBJLOC, TYPE, NDIM, DIMS, SIZE, NLINES, INDENT,
*    :                  ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )

*  Description:
*     The value or values of the primitive object with specified locator
*     are appended to the given string at the defined position.  The
*     routine calls a subroutine to do the actual formatting (see
*     KPS1_PUTx for details) depending on the type of the object.  If
*     the type is not a recognised primitive type, the string
*     '{special}' is appended to the line.

*  Arguments:
*     OBJLOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the primitive object.
*     TYPE = CHARACTER * ( * ) (Given)
*        Type of the primitive object.
*     NDIM = INTEGER (Given)
*        Dimensionality of the primitive object.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Array of dimensions of the primitive object.
*     SIZE = INTEGER (Given)
*        Number of elements for the primitive object if it is treated
*        as a vector.
*     NLINES = INTEGER (Given)
*        Number of lines to store values.
*     INDENT = INTEGER (Given)
*        Indentation level for any continuation lines (otherwise
*        ignored).
*     ONEPLN = LOGICAL (Given)
*        If true the elements of a character array each appear on a
*        separate line
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an text
*        file.
*     FD = INTEGER (Given)
*        The text file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The string to which the primitive values are to be appended.
*     LENG = INTEGER (Given and Returned)
*        The position in the string where the values will be
*        appended, returned as the length of the string ignoring
*        KPS1_iling blanks.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Depending on the type of the primitive object one of the KPS1_PUT
*     routines is called to do the work.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 May 9 (MJC):
*        Original version based on TRA_PUTVL in HDSTRACE.
*     2012 May 12 (MJC):
*        Add support for _INT64.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Environment constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( DAT__SZLOC ) OBJLOC ! Locator to the primitive object
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of the primitive object

      INTEGER INDENT             ! Indentation level for any
                                 ! continuation lines
      INTEGER NDIM               ! Dimensionality of the object
      INTEGER NLINES             ! Number of lines to present values
      INTEGER DIMS( DAT__MXDIM ) ! Array of object dimensions
      INTEGER SIZE               ! Size of object if vectorized
      INTEGER FD                 ! Log file's description

      LOGICAL LOGEXM             ! Output to go to the log file?
      LOGICAL ONEPLN             ! Elements of a character array each
                                 ! appear on a new line

*  Arguments Given and Returned:
      CHARACTER * ( * ) LINE     ! String to which the values are to be
                                 ! appended
      INTEGER LENG               ! Current position in the string

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! String equality test

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Append values for all the supported types.

*  Double precision.
      IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         CALL KPS1_PUTD( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                    LOGEXM, FD, LINE, LENG, STATUS )

*  Real.
      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         CALL KPS1_PUTR( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                    LOGEXM, FD, LINE, LENG, STATUS )

*  Integer.
      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         CALL KPS1_PUTI( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                    LOGEXM, FD, LINE, LENG, STATUS )

*  64-bit integer.
      ELSE IF ( CHR_SIMLR( TYPE, '_INT64' ) ) THEN
         CALL KPS1_PUTK( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                   LOGEXM, FD, LINE, LENG, STATUS )

*  Word.
      ELSE IF ( CHR_SIMLR( TYPE,  '_WORD' ) ) THEN
         CALL KPS1_PUTW( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                    LOGEXM, FD, LINE, LENG, STATUS )

*  Unsigned word.
      ELSE IF ( CHR_SIMLR( TYPE,  '_UWORD' ) ) THEN
         CALL KPS1_PUTUW( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                     LOGEXM, FD, LINE, LENG, STATUS )

*  Byte.
      ELSE IF ( CHR_SIMLR( TYPE,  '_BYTE' ) ) THEN
         CALL KPS1_PUTB( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                    LOGEXM, FD, LINE, LENG, STATUS )


*  Unsigned byte.
      ELSE IF ( CHR_SIMLR( TYPE,  '_UBYTE' ) ) THEN
         CALL KPS1_PUTUB( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                     LOGEXM, FD, LINE, LENG, STATUS )

*  Logical.
      ELSE IF ( CHR_SIMLR( TYPE, '_LOGICAL' ) ) THEN
         CALL KPS1_PUTL( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

*  Character.
      ELSE IF ( CHR_SIMLR( TYPE( 1:5 ), '_CHAR') ) THEN
         CALL KPS1_PUTC( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )

*       Not recognised !
      ELSE
         CALL CHR_PUTC( '{special}', LINE, LENG )

      END IF

      END
