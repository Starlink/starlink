      SUBROUTINE CCD1_FFCL( PTYPE, BAD, IPIN, ID, NX, NY, NITER, GAMMA,
     :                      IX, IY, SIGMA, IPOUT, NGOOD, IPWRK,
     :                      IPILIN, IPNLIN, STATUS )
*+
*  Name:
*     CCD1_FFCL

*  Purpose:
*     To pass information to the appropriate version of CCG1_FFRJ,
*     which removes defects from a substantially smooth array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FFCL( PTYPE, BAD, IPIN, ID, NX, NY, NITER, GAMMA,
*                     IX, IY, SIGMA, IPOUT, NGOOD, IPWRK,
*                     IPILIN, IPNLIN, STATUS )

*  Description:
*     The routine calls the version of CCG1_FFRJ which is appropriate
*     to the data type in use. The only types allowed are _REAL and
*     _DOUBLE (this is imposed by the blocking routine which handles
*     only these data types). This routine also writes out a message
*     indicating the name of the NDF in use. This is then followed by
*     the iteration mesages from CCG1_FFRJ

*  Arguments:
*     PTYPE = CHARACTER * ( * ) (Given)
*        The data type at which the input arrays are to be processed.
*        Must be one of _REAL or _DOUBLE others are not supported.
*     BAD = LOGICAL (Given and Returned)
*        Whether BAD pixels are initially present or not, on exit
*        signfies if pixels have been rejected.
*     IPIN = INTEGER (Given)
*        Pointer to the h2D array which requires cleaning.
*     ID = INTEGER (Given)
*        The NDF identifier of the origin of the data array.
*     NX = INTEGER (Given)
*        First dimension of input array.
*     NY = INTEGER (Given)
*        Second dimension of input array.
*     NITER = INTEGER (Given)
*        Number of cleaning interations.
*     GAMMA = REAL (Given)
*        The number of standard deviations at which rejection occurs.
*     IX = INTEGER (Given)
*        X size of the smoothing box.
*     IY = INTEGER (Given)
*        Y size of the smoothing box.
*     SIGMA = REAL (Returned)
*        Estimate of the RMS noise in an output pixel.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to the cleaned output array.
*     NGOOD = INTEGER (Returned)
*        The number of good pixels left in image after cleaning.
*     IPWRK = INTEGER (Given and Returned)
*        Pointer to Workspace of size and type of input data.
*     IPILIN = INTEGER (Given and Returned)
*        Pointer to integer workspace of size NX.
*     IPNLIN = INTEGER (Given and Returned)
*        Pointer to workspace of size NX of type PTYPE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses pointers to arrays.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) PTYPE
      INTEGER IPIN
      INTEGER ID
      INTEGER NX
      INTEGER NY
      INTEGER NITER
      REAL GAMMA
      INTEGER IX
      INTEGER IY

*  Arguments Given and Returned:
      LOGICAL BAD
      INTEGER IPOUT
      INTEGER IPWRK
      INTEGER IPILIN
      INTEGER IPNLIN

*  Arguments Returned:
      REAL SIGMA
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write out the NDF name message.
      CALL CCD1_MSG( ' ',  ' ', STATUS )
      CALL NDF_MSG( 'FFCL_NDFNAM', ID )
      CALL CCD1_MSG( ' ', '  Cleaning NDF: ^FFCL_NDFNAM', STATUS )

*  Call the appropriate version of CCG1_FFREJ
      IF ( PTYPE .EQ. '_REAL' ) THEN
         CALL CCG1_FFRJR( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                    NX, NY, NITER, GAMMA,
     :                    IX, IY, SIGMA, %VAL( CNF_PVAL( IPOUT ) ),
     :                    NGOOD,
     :                    %VAL( CNF_PVAL( IPWRK )),
     :                    %VAL( CNF_PVAL( IPILIN ) ),
     :                    %VAL( CNF_PVAL( IPNLIN ) ),
     :                    STATUS )

      ELSE IF( PTYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_FFRJD( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                    NX, NY, NITER, GAMMA,
     :                    IX, IY, SIGMA, %VAL( CNF_PVAL( IPOUT ) ),
     :                    NGOOD,
     :                    %VAL( CNF_PVAL( IPWRK )),
     :                    %VAL( CNF_PVAL( IPILIN ) ),
     :                    %VAL( CNF_PVAL( IPNLIN ) ),
     :                    STATUS )
      END IF
      END
* $Id$
