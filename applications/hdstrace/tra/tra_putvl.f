      SUBROUTINE TRA_PUTVL( OBJLOC, TYPE, NDIM, DIMS, SIZE, NLINES,
     :                      INDENT, ONEPLN, LOGEXM, FD, LINE, LENG,
     :                      STATUS )
*+
*  Name:
*     TRA_PUTVL

*  Purpose:
*     Appends the values of a primitive object to a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA_PUTVL( OBJLOC, TYPE, NDIM, DIMS, SIZE, NLINES, INDENT,
*    :                ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )

*  Description:
*     The value or values of the primitive object with specified locator
*     are appended to the given string at the defined position.  The
*     routine calls a subroutine to do the actual formatting (see
*     TRA_PUTx for details) depending on the type of the object.  If
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
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The string to which the primitive values are to be appended.
*     LENG = INTEGER (Given and Returned)
*        The position in the string where the values will be
*        appended, returned as the length of the string ignoring
*        trailing blanks.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Depending on the type of the primitive object one of the TRA_PUT
*     routines is called to do the work.

*  Copyright:
*     Copyright (C) 1984, 1989, 1991-1992 Science & Engineering
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     06/04/1984 (DB):
*        Original version.
*     1989 May 10 (MJC):
*        Used {} to delimit commentary instead of <>, and placed equals
*        sign at the start.
*     1989 May 15 (MJC):
*        Tidy up and added NLINES, INDENT, LINE to arguments for
*        LSPUT<T> subroutines.
*     1989 Jun 15 (MJC):
*        Renamed from PUTVAL to avoid confusion with the TRACE version;
*        added ONEPLN, LOGEXM, FD and STATUS arguments.
*     1991 January 30 (MJC):
*        Converted to SST prologue.
*     1992 January 13 (MJC):
*        Enabled the bad-value flagging for the byte and word, signed
*        and unsigned data types via separate calls to TRA_PUTx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*  Arguments Given:
      CHARACTER * ( DAT__SZLOC )
     :  OBJLOC                 ! Locator to the primitive object
      CHARACTER * ( DAT__SZTYP )
     :  TYPE                   ! Type of the primitive object

      INTEGER
     :  INDENT,                ! Indentation level for any continuation
                               ! lines
     :  NDIM,                  ! Dimensionality of the object
     :  NLINES,                ! Number of lines to present values
     :  DIMS( DAT__MXDIM ),    ! Array of object dimensions
     :  SIZE,                  ! Size of object if vectorized
     :  FD                     ! Log file's description

      LOGICAL                  ! True if:
     :  LOGEXM,                ! Output to go to the log file
     :  ONEPLN                 ! Elements of a character array each
                               ! appear on a new line

*  Arguments Given and Returned:
      CHARACTER * ( * )
     :  LINE                   ! String to which the values are to be
                               ! appended
      INTEGER
     :  LENG                   ! Current position in the string

*  Status:
      INTEGER
     :  STATUS                 ! Global status

*  External References:
      LOGICAL CHR_SIMLR        ! String equality test

*.

*    Check the global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Append values for all the supported types.

      IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN

*       Double precision.

         CALL TRA_PUTD( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN

*       Real.

         CALL TRA_PUTR( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN

*      Integer.

         CALL TRA_PUTI( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_INT64' ) ) THEN

*      Integer*8.

         CALL TRA_PUTK( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE,  '_WORD' ) ) THEN

*      Word.

         CALL TRA_PUTW( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE,  '_UWORD' ) ) THEN

*      Unsigned word.

         CALL TRA_PUTUW( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                   LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE,  '_BYTE' ) ) THEN

*      Byte.

         CALL TRA_PUTB( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE,  '_UBYTE' ) ) THEN

*      Unsigned byte.

         CALL TRA_PUTUB( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                   LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_LOGICAL' ) ) THEN

*       Logical.

         CALL TRA_PUTL( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  LOGEXM, FD, LINE, LENG, STATUS )

      ELSE IF ( CHR_SIMLR( TYPE(1:5), '_CHAR') ) THEN

*       Character.

         CALL TRA_PUTC( OBJLOC, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                  ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )

      ELSE

*       Not recognised !

         CALL CHR_PUTC( '{special}', LINE, LENG )
      END IF

      END
