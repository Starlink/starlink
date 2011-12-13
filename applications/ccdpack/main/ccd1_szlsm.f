      SUBROUTINE CCD1_SZLSM( M, N, XC, FVECC, FJACC, LJC, S, IGRADE,
     :                       NITER, NF, IW, LIW, W, LW )
*+
*  Name:
*     CCD1_SZLSM

*  Purpose:
*     Monitoring routine for NAG routine E04GBF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SZLSM( M, N, XC, FVECC, FJACC, LJC, S, IGRADE, NITER,
*                      NF, IW, LIW, W, LW )

*  Description:
*     This is an instance of the LSQMON service routine required by the
*     NAG library routine E04GBF. It does nothing.

*  Arguments:
*     These satisfy the requirements of the calling routine (see NAG
*     library documentation), but are not used.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     30-APR-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      DOUBLE PRECISION FVECC( M )
      INTEGER LJC
      DOUBLE PRECISION FJACC( LJC, N )
      DOUBLE PRECISION S( N )
      INTEGER IGRADE
      INTEGER NITER
      INTEGER NF
      INTEGER LIW
      INTEGER IW( LIW )
      INTEGER LW
      DOUBLE PRECISION W( LW )

*.

*  Do nothing.
      END
* $Id$
