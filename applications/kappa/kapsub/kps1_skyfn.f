      SUBROUTINE KPS1_SKYFN( M, N, XC, FVECC, IFLAG )
*+
*  Name:
*     KPS1_SKYFN

*  Purpose:
*     Evaluates function values for minimisation routine PDA_LMDIF1

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SKYFN( M, N, XC, FVECC, IFLAG )

*  Description:
*     This function is related to application SETSKY.  It services the
*     calls to PDA_LMDIF1 made in subroutine KPS1_SKYFT.  The data
*     needed by this routine is passed from KPS1_SKYFT in common blocks
*     defined in include file SFT_COM.
*
*     The argument XC defines a projection (together with values
*     supplied in common).  This projection is used to transform the sky
*     co-ordinates supplied in common.  The residuals between the
*     resulting image co-ordinates and the image co-ordinates supplied in
*     common are returned in argument FVECC.  Separate residuals are
*     returned for the X and Y axes.
*
*     If an error occurs in this routine, an error is reported and the
*     status value is returned in common variable ISTAT.

*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals to be calculated.  M should be two
*        times the number of positions stored in common.
*     N = INTEGER (Given)
*        The number of projection parameters which are allowed to vary.
*     XC( N ) = DOUBLE PRECISION (Given)
*        The projection parameters for which the residuals are required.
*        These projection parameters are stored in the same order as the
*        "P" argument for subroutine IRA_CREAT, except that for each
*        projection parameter value which has been fixed by the user,
*        the remaining elements in XC are shuffled down to occupy the
*        location which otherwise would have been used by the fixed
*        projection parameter.  Fixed parameter values are supplied in
*        common.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        The residuals.
*     IFLAG = INTEGER (Returned)
*        Some error flag?  Manual does not say.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
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
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1994 (DSB):
*        Original version.
*     1996 January 31 (MJC):
*        Renamed from LSFUN1 and IFLAG argument added for PDA.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'IRA_PAR'          ! IRA public constants

*  Global Variables:
      INCLUDE 'SFT_COM'          ! Used for communicating with KPS1_SKYFT
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        IPWA = INTEGER (Read)
*           Pointer to work space containing sky longitude values.
*        IPWB = INTEGER (Read)
*           Pointer to work space containing sky latitude values.
*        IPWX = INTEGER (Read)
*           Pointer to work space containing supplied image X values.
*        IPWY = INTEGER (Read)
*           Pointer to work space containing supplied image Y values.
*        IPWXO = INTEGER (Read)
*           Pointer to work space for temporary image X values.
*        IPWYO = INTEGER (Read)
*           Pointer to work space for temporary image Y values.
*        ISTAT = INTEGER (Read and Write)
*           Local status value.
*        NPOSC = INTEGER (Read)
*           No. of supplied sky positions.

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      INTEGER IFLAG

*  Arguments Returned:
      DOUBLE PRECISION FVECC( M )

*.

*  Call a lower level routine to do the work, passing the work arrays
*  using %VAL so that their contents can be accessed.
      CALL KPS1_SKYF3( M, N, NPOSC, XC, %VAL( CNF_PVAL( IPWA ) ),
     :                 %VAL( CNF_PVAL( IPWB ) ),
     :                 %VAL( CNF_PVAL( IPWX ) ),
     :                 %VAL( CNF_PVAL( IPWY ) ),
     :                 %VAL( CNF_PVAL( IPWXO ) ),
     :                 %VAL( CNF_PVAL( IPWYO ) ), FVECC, ISTAT )

      END
