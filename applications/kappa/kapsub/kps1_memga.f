      SUBROUTINE KPS1_MEMGA( INDF, RATE, ALPHA, BETA, DEF, SIGMA, STDEV,
     :                       NITER, ITER, ILEVEL, METHOD, IMAGE, ISTAT,
     :                       STATUS )
*+
*  Name:
*     KPS1_MEMGA

*  Purpose:
*     Gets analysis/continuation information from the MEM2D extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMGA( INDF, RATE, ALPHA, BETA, DEF, SIGMA, STDEV,
*                      NITER, ITER, ILEVEL, METHOD, IMAGE, ISTAT,
*                      STATUS )

*  Description:
*     An attempt is made to access an extension named MEM2D.  The
*     information is then copied out of the extension to common blocks
*     and to the arguments of this routine.  The MEM2D extension is
*     created by routine NDFOUT.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier of the input.
*     RATE = REAL (Returned)
*        MEM3 argument RATE.
*     ALPHA = REAL (Returned)
*        MEM3 argument ALPHA.
*     BETA = REAL (Returned)
*        MEM3 argument BETA.
*     DEF = REAL (Returned)
*        MEM3 argument DEF.
*     SIGMA = REAL (Returned)
*        MEM3 argument SIGMA.
*     STDEV = REAL (Returned)
*        Initial guess at Gaussian noise level.
*     NITER = INTEGER (Returned)
*        Maximum number of iterations to perform.
*     ITER = INTEGER (Returned)
*        Number of iterations completed.
*     ILEVEL = INTEGER (Returned)
*        User-information level.
*     METHOD = INTEGER (Returned)
*        MEM3 argument METHOD.
*     IMAGE = CHARACTER (Returned)
*        The name of the NDF which is being deconvolved.
*     ISTAT = INTEGER (Returned)
*        MEM3 argument ISTAT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-OCT-1990 (DSB):
*        Original version.
*     27-FEB-1991 (DSB):
*        Name changed form GETANA to KPS1_GETAN
*     1991 July 18 (MJC):
*        Name changed form KPS1_GETAN to KPS1_MEMGA.
*     20-MAR-1995 (DSB):
*        Modified to allow use of external work arrays.
*     1995 April 7 (MJC):
*        Minor stylistic changes.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants

*  Global Variables:
      INCLUDE 'C1_COM'           ! Used to communicate with OPUS and
                                 ! TROPUS
      INCLUDE 'ME_COM'           ! Common blocks etc required by MEMSYS3
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER     INDF

*  Arguments Returned:
      REAL        RATE
      REAL        ALPHA
      REAL        BETA
      REAL        DEF
      REAL        SIGMA
      INTEGER     NITER
      INTEGER     ITER
      INTEGER     ILEVEL
      INTEGER     METHOD
      REAL        STDEV
      CHARACTER * ( * ) IMAGE
      INTEGER	  ISTAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! Locator to the MEM2D extension
      INTEGER     NEL            ! No. of array elements copied.
      INTEGER     SZAREA         ! No. of elements in a MEMSYS3 area

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an HDS locator to the NDF extension holding the analysis
*  information.
      CALL NDF_XLOC( INDF, 'MEM2D', 'READ', ALOC, STATUS )

*  Now copy the data from the extension.............

*  Arguments to this routine.
      CALL CMP_GET0R( ALOC, 'RATE', RATE, STATUS )
      CALL CMP_GET0R( ALOC, 'ALPHA', ALPHA, STATUS )
      CALL CMP_GET0R( ALOC, 'BETA', BETA, STATUS )
      CALL CMP_GET0R( ALOC, 'SIGMA', SIGMA, STATUS )
      CALL CMP_GET0I( ALOC, 'NITER', NITER, STATUS )
      CALL CMP_GET0I( ALOC, 'ITER', ITER, STATUS )
      CALL CMP_GET0I( ALOC, 'ILEVEL', ILEVEL, STATUS )
      CALL CMP_GET0R( ALOC, 'DEF', DEF, STATUS )
      CALL CMP_GET0R( ALOC, 'STDEV', STDEV, STATUS )
      CALL CMP_GET0I( ALOC, 'METHOD', METHOD, STATUS )
      CALL CMP_GET0C( ALOC, 'INPUT', IMAGE, STATUS )
      CALL CMP_GET0I( ALOC, 'ISTAT', ISTAT, STATUS )

*  Common block /C1_COM/
      CALL CMP_GET0I( ALOC, 'C1_NPX', C1_NPX, STATUS )
      CALL CMP_GET0I( ALOC, 'C1_NLN', C1_NLN, STATUS )
      CALL CMP_GET1I( ALOC, 'C1_DIM', 2, C1_DIM, NEL, STATUS )
      CALL CMP_GET0I( ALOC, 'C1_XMG', C1_XMG, STATUS )
      CALL CMP_GET0I( ALOC, 'C1_YMG', C1_YMG, STATUS )
      CALL CMP_GET0R( ALOC, 'C1_ICF', C1_ICF, STATUS )

*  Common block /MECOMP/.
      CALL CMP_GET1I( ALOC, 'KC', 40, ME_KC, NEL, STATUS )
      CALL CMP_GET1I( ALOC, 'KD', 40, ME_KD, NEL, STATUS )
      CALL CMP_GET0I( ALOC, 'NTRANS', ME_NTR, STATUS )
      CALL CMP_GET0I( ALOC, 'L0', ME_L0, STATUS )
      CALL CMP_GET0I( ALOC, 'L1', ME_L1, STATUS )
      CALL CMP_GET0I( ALOC, 'M0', ME_M0, STATUS )
      CALL CMP_GET0I( ALOC, 'M1', ME_M1, STATUS )
      CALL CMP_GET0I( ALOC, 'M2', ME_M2, STATUS )
      CALL CMP_GET0I( ALOC, 'M3', ME_M3, STATUS )

*  Now set up the other items in /MECOMP/ (KA, KB, MJ, MK, NJ, NK),
*  and pointers to any external work arrays.
      IF ( ME_M1 .EQ. 0 ) THEN
         CALL KPS1_MEMCP( ILEVEL, 'CONSTANT', STATUS )
      ELSE
         CALL KPS1_MEMCP( ILEVEL, 'NDF', STATUS )
      END IF

*  Store the size of a full MEMSYS area.
      SZAREA = ME_MK * ME_NK

*  MEMSYS3 areas.  First deal with cases where external storage is
*  being used.  Copy the data from the NDF extension to the work array
*  allocated within KPS1_MEMCP.
      IF ( C1_WEXT ) THEN
         CALL CMP_GET1R( ALOC, 'FILE1', SZAREA,
     :                   %VAL( CNF_PVAL( C1_IP( 1 ) ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE3', SZAREA,
     :                   %VAL( CNF_PVAL( C1_IP( 3 ) ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE21', SZAREA,
     :                   %VAL( CNF_PVAL( C1_IP( 21 ) ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE22', SZAREA,
     :                   %VAL( CNF_PVAL( C1_IP( 22 ) ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE23', SZAREA,
     :                   %VAL( CNF_PVAL( C1_IP( 23 ) ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE24', SZAREA,
     :                   %VAL( CNF_PVAL( C1_IP( 24 ) ) ),
     :                   NEL, STATUS )
         IF ( ME_M1 .EQ. 1 ) CALL CMP_GET1R( ALOC, 'FILE20', SZAREA,
     :
     :   %VAL( CNF_PVAL( C1_IP( 20 ) ) ), NEL,
     :                                       STATUS )

*  If internal storage is being used, copy the areas from the NDF
*  extension to the work array in common block /MECOMS/.
      ELSE
         CALL CMP_GET1R( ALOC, 'FILE1', SZAREA, ME_ST( ME_KB( 1 ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE3', SZAREA, ME_ST( ME_KB( 3 ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE21', SZAREA, ME_ST( ME_KB( 21 ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE22', SZAREA, ME_ST( ME_KB( 22 ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE23', SZAREA, ME_ST( ME_KB( 23 ) ),
     :                   NEL, STATUS )
         CALL CMP_GET1R( ALOC, 'FILE24', SZAREA, ME_ST( ME_KB( 24 ) ),
     :                   NEL, STATUS )
         IF ( ME_M1 .EQ. 1 ) CALL CMP_GET1R( ALOC, 'FILE20', SZAREA,
     :                                       ME_ST( ME_KB( 20 ) ), NEL,
     :                                       STATUS )
      END IF

*  Common block /MECOML/
      CALL CMP_GET1D( ALOC, 'XTABLE', ME_SIZ, ME_XTB, NEL, STATUS )
      CALL CMP_GET1D( ALOC, 'YTABLE', ME_SIZ, ME_YTB, NEL, STATUS )
      CALL CMP_GET1D( ALOC, 'VTABLE', ME_SIZ, ME_VTB, NEL, STATUS )
      CALL CMP_GET0I( ALOC, 'NTABLE', ME_NTB, STATUS )

*  Release the HDS locator to the extension.
      CALL DAT_ANNUL( ALOC, STATUS )

      END
