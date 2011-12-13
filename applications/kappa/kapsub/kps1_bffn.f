      SUBROUTINE KPS1_BFFN( M, N, XC, FVEC, IFLAG )
*+
*  Name:
*     KPS1_BFFN

*  Purpose:
*     Evaluates function values for minimisation routine PDA_LMDIF1

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFFN( M, N, XC, FVEC, IFLAG )

*  Description:
*     This function is related to application BEAMFIT.  It services the
*     calls to PDA_LMDIF1 made in subroutine KPS1_BFFT.  The data
*     needed by this routine is passed from KPS1_BFFT in common blocks
*     defined in include file BF_COM.
*
*     The argument XC (together with values supplied in common) defines
*     a generalised Gaussian fit to imaging data.  The residuals between
*     the resulting fitted image and the original image supplied in a
*     COMMON block are returned in argument FVEC.
*
*     If an error occurs in this routine, an error is reported and the
*     status value is returned in common variable ISTAT.

*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals to be calculated.
*     N = INTEGER (Given)
*        The number of fit parameters which are allowed to vary.
*     XC( N ) = DOUBLE PRECISION (Given)
*        The parameters of the two-dimensional Gaussian fit for which
*        the residuals are required.  The full list of elements for each
*        Gaussian is as follows.
*           1  --  X pixel centre
*           2  --  Y pixel centre
*           3  --  Major axis standard deviation in pixels
*           4  --  Minor axis standard deviation in pixels
*           5  --  Position angle of the major axis in radians
*           6  --  Amplitude
*           7  --  Background (assumed to be a constant level)
*           8  --  Shape exponent
*
*        Where each fit parameter has been fixed by the user, the
*        remaining elements in XC are shuffled down to occupy the
*        location which otherwise would have been used by the fixed
*        projection parameter.  Fixed parameter values are supplied in
*        common.
*     FVEC( M ) = DOUBLE PRECISION (Returned)
*        The residuals.
*     IFLAG = INTEGER (Returned)
*        Success flag.  0 is success.  Negative is failure.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     2007 February 14 (MJC):
*        Original version.
*     2010 July 5 (MJC):
*        Switch to generalised Gaussian fit in documentation.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'BF_PAR'           ! BEAMFIT constants

*  Global Variables:
      INCLUDE 'BF_COM'           ! Used for communicating with KPS1_BFFT
*        IPWD = INTEGER (Read)
*           Pointer to work space containing data values.
*        IPWV = INTEGER (Read)
*           Pointer to work space containing data errors
*        ISTAT = INTEGER (Read & Write)
*           Local status.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      INTEGER IFLAG

*  Arguments Returned:
      DOUBLE PRECISION FVEC( M )

*.

*  Call a lower-level routine to do the work, passing the work arrays
*  using %VAL so that their contents can be accessed.
      CALL KPS1_BFFTG( M, N, XC, %VAL( CNF_PVAL( IPWD ) ),
     :                 %VAL( CNF_PVAL( IPWV ) ), FVEC, ISTAT )

      END
