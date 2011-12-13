
*+
*  Name:
*    cat1_errtr

*  Copyright:
*    Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*    All Rights Reserved.

*  Licence:
*    This program is free software; you can redistribute it and/or
*    modify it under the terms of the GNU General Public License as
*    published by the Free Software Foundation; either version 2 of
*    the License, or (at your option) any later version.
*
*    This program is distributed in the hope that it will be
*    useful,but WITHOUT ANY WARRANTY; without even the implied
*    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*    PURPOSE. See the GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*    02110-1301, USA

*-
      SUBROUTINE CAT1_ERRTR (TEXT, STATUS)
      IMPLICIT NONE
      INTEGER STATUS
      CHARACTER TEXT*(*)
      INCLUDE 'CAT_ERR'
      INTEGER TPOS

      IF (STATUS .EQ. CAT__ERROR) THEN
         TEXT = 'General CAT error.'
      ELSE IF (STATUS .EQ. CAT__INVOP) THEN
         TEXT = 'Invalid STATE and MODE combination opening cat.'
      ELSE IF (STATUS .EQ. CAT__ILLOP) THEN
         TEXT = 'Invalid parameters when opening catalogue.'
      ELSE IF (STATUS .EQ. CAT__INVBK) THEN
         TEXT = 'Invalid back-end type.'
      ELSE IF (STATUS .EQ. CAT__MAXOP) THEN
         TEXT = 'Maximum permitted number of open cats. exceeded.'
      ELSE IF (STATUS .EQ. CAT__MAXID) THEN
         TEXT = 'Maximum permitted number of identifiers exceeded.'
      ELSE IF (STATUS .EQ. CAT__MAXAT) THEN
         TEXT = 'Maximum permitted number of attributes exceeded.'
      ELSE IF (STATUS .EQ. CAT__EOF  ) THEN
         TEXT = 'End-of-file encountered.'
      ELSE IF (STATUS .EQ. CAT__TYPCV) THEN
         TEXT = 'Type conversion error.'
      ELSE IF (STATUS .EQ. CAT__IMATT) THEN
         TEXT = 'Attempt to modify an immutable attribute.'
      ELSE IF (STATUS .EQ. CAT__NOATT) THEN
         TEXT = 'Attribute not found.'
      ELSE IF (STATUS .EQ. CAT__INVCN) THEN
         TEXT = 'Invalid catalogue name.'
      ELSE IF (STATUS .EQ. CAT__NOLUN) THEN
         TEXT = 'Failed to get a free Fortran logical unit number.'
      ELSE IF (STATUS .EQ. CAT__NOCMP) THEN
         TEXT = 'Component not found.'
      ELSE IF (STATUS .EQ. CAT__INVDT) THEN
         TEXT = 'Invalid data type.'
      ELSE IF (STATUS .EQ. CAT__INVRW) THEN
         TEXT = 'Invalid row number.'
      ELSE IF (STATUS .EQ. CAT__INVGN) THEN
         TEXT = 'Invalid genus for a column.'
      ELSE IF (STATUS .EQ. CAT__INVGT) THEN
         TEXT = 'Invalid get (not column, parameter or expr.).'
      ELSE IF (STATUS .EQ. CAT__INVEX) THEN
         TEXT = 'Invalid expression (unable to parse it).'
      ELSE IF (STATUS .EQ. CAT__INVWT) THEN
         TEXT = 'Attempt to write to a read-only catalogue.'
      ELSE IF (STATUS .EQ. CAT__INVPT) THEN
         TEXT = 'Invalid put (not column, parameter or expr.).'
      ELSE IF (STATUS .EQ. CAT__IDNFD) THEN
         TEXT = 'Identifier not found.'
      ELSE IF (STATUS .EQ. CAT__INVNL) THEN
         TEXT = 'Invalid code to specify type of null for column.'
      ELSE IF (STATUS .EQ. CAT__INVEC) THEN
         TEXT = 'Invalid specification of vector element.'
      ELSE IF (STATUS .EQ. CAT__INVCL) THEN
         TEXT = 'Invalid column specification.'
      ELSE IF (STATUS .EQ. CAT__INVPR) THEN
         TEXT = 'Invalid parameter specification.'
      ELSE IF (STATUS .EQ. CAT__INVID) THEN
         TEXT = 'Invalid identifier.'
      ELSE IF (STATUS .EQ. CAT__INVSR) THEN
         TEXT = 'Invalid operation on unsorted column.'
      ELSE IF (STATUS .EQ. CAT__INVAC) THEN
         TEXT = 'Invalid access mode.'
      ELSE IF (STATUS .EQ. CAT__INVDS) THEN
         TEXT = 'Invalid description file.'
      ELSE IF (STATUS .EQ. CAT__NOCAT) THEN
         TEXT = 'Unable to open catalogue.'
      ELSE IF (STATUS .EQ. CAT__IOERR) THEN
         TEXT = 'Catalogue I/O error.'
      ELSE IF (STATUS .EQ. CAT__INVCD) THEN
         TEXT = 'Invalid catalogue description.'
      ELSE IF (STATUS .EQ. CAT__INVAS) THEN
         TEXT = 'Invalid AST frame-set.'
      ELSE IF (STATUS .EQ. CAT__INVAP) THEN
         TEXT = 'Invalid AST frame-set specification.'
      ELSE IF (STATUS .EQ. CAT__DUPNM) THEN
         TEXT = 'Duplicate column, parameter or expression name.'
      ELSE
         TPOS = 0
         TEXT = ' '
         CALL CHR_PUTC ('Unknown status: ', TEXT, TPOS)
         CALL CHR_PUTI (STATUS, TEXT, TPOS)
      END IF

      END
