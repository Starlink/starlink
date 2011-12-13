      SUBROUTINE UGET( J, K, N )
*+
*  Name:
*     UGET

*  Purpose:
*     Obtains a block of an external area for MEMSYS3.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL UGET( J, K, N )

*  Description:
*     This subroutine is called directly by the MEMSYS3 library.  It
*     transfers N elements from external storage to the internal work
*     arrays held in common block /MECOMS/.  The external storage is
*     assumed to be in the form of a continuous 1-d vector.  The given
*     argument J is the index within this external storage vector of
*     the first element to be transferred.  This element is copied to
*     element K of the internal work array ME_ST.

*  Arguments:
*     J = INTEGER (Given)
*        The index within the external storage vector of the
*        first element to be transfered.
*     K = INTEGER (Given)
*        The index within the internal work array at which the first
*        element is to be stored.
*     N = INTEGER (Given)
*        The number of elements to be transferred.

*  Copyright:
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1995 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'ME_COM'           ! MEMSYS3 common blocks
      INCLUDE 'C1_COM'           ! Common blocks used to communicate
                                 ! with MEM2D.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER J
      INTEGER K
      INTEGER N

*.

      CALL KPS1_MEMTR( N, J, %VAL( CNF_PVAL( C1_IP0 ) ), K, ME_ST )


      END
