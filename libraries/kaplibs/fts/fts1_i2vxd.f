      SUBROUTINE FTS1_I2VXD( BSWAP, EL, BUF, STATUS )
*+
*  Name:
*     FTS1_I2VXD

*  Purpose:
*     Converts a vector of 64-bit IEEE floating-point numbers to Vax-D
*     format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_I2VXD( BSWAP, EL, BUF, STATUS )

*  Description:
*     This is a dummy routine used to build the KAPPA FITS readers on
*     UNIX.

*  Arguments:
*     BSWAP = LOGICAL (Given)
*        Whether or not adjacent bytes are to be swapped.  If and only
*        if BSWAP is 1 will the bytes be swapped.  Swapping must occur
*        either prior or within this routine to obtain the correct
*        Vax-D values.  An expression must not be given for this
*        argument.  Bytes in the order 1 2 3 4 5 6 7 8 become
*        2 1 4 3 6 5 8 7 after swapping.
*     EL = INTEGER (Given)
*        The number of IEEE numbers to be converted.
*     BUF( EL ) = DOUBLE PRECISION (Given and Returned)
*        On input the IEEE numbers to be converted.  On return these
*        are converted to Vax-D format.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Two masks...
*
*     Vax-D floating-point representation.
*     ====================================
*
*     Vax D floating point is stored as the following arrangement.
*
*         15  14                7 6                 0
*        ---------------------------------------------
*        | s |     exponent      |      mantissa     | :A (byte address)
*        |-------------------------------------------|
*        |                mantissa                   | :A+2
*        |-------------------------------------------|
*        |                mantissa                   | :A+4
*        |-------------------------------------------|
*        |                mantissa                   | :A+6
*        ---------------------------------------------
*         63                                       48
*
*     where s is the sign bit (0 indicates +), and the numbers shows the
*     bits used for the various components.

*  References:
*     -  "IEEE Standard for Binary Floating-Point Arithmetic",
*     ANSI/IEEE 754, 1985.
*     -  "Floating Point Agreement for FITS", D.C. Wells & P. Grosbol,
*     1990.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 11 (MJC):
*        Original version.
*     1993 January 7 (MJC):
*        Dummy version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing

*  Arguments Given:
      INTEGER
     :  EL

      LOGICAL
     :  BSWAP

*  Arguments Given and Returned:
      DOUBLE PRECISION BUF( EL )

*  Status:
      INTEGER STATUS           ! Global status

*.

*  Do nothing.

      END
