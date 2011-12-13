      SUBROUTINE KPS1_MEMSA( PARAM, MODE, EXTEND, INDF, RATE, ALPHA,
     :                       BETA, DEF, SIGMA, NITER, ITER, ILEVEL,
     :                       METHOD, STDEV, ISTAT, STATUS )
*+
*  Name:
*     KPS1_MEMSA

*  Purpose:
*     Produces an output NDF containing the current MEM reconstruction,
*     and optionally, analysis information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMSA( PARAM, MODE, EXTEND, INDF, RATE, ALPHA, BETA,
*                      DEF, SIGMA, NITER, ITER, ILEVEL, METHOD, STDEV,
*                      ISTAT, STATUS )

*  Description:
*     This routine saves a maximum-entropy reconstruction to an NDF.
*     If a new output NDF is to be created (MODE=WRITE), then a new
*     NDF is created based on the input NDF.  If the output NDF is to be
*     updated (MODE=UPDATE), then a call to LPG_ASSOC is made which
*     should return the same NDF that was used on the last call to
*     this routine (so long as the ADAM parameter has not been cancelled
*     between the two calls).
*
*     The image in area 1 is copied to area 5, blurred with the ICF and
*     copied to the data array, (removing the margin).
*
*     If required (EXTEND=.TRUE.), an extension is created called MEM2D,
*     and the values of all data required to continue, or analyse,
*     the MEM run is written to it.  If such an extension already exists
*     then it is updated rather than being deleted and re-created.

*  Arguments:
*     PARAM = CHARACTER (Given)
*        The ADAM parameter to obtain the name of the NDF.
*     MODE = CHARACTER (Given)
*        The mode of operation: WRITE or UPDATE.
*     EXTEND = LOGICAL (Given)
*        If .TRUE. then analysis information is appended to the output
*        NDF in an extension called MEM2D.
*     INDF = INTEGER (Given)
*        The NDF identifier of the input image.
*     RATE = REAL (Given)
*        MEM3 argument RATE.
*     ALPHA = REAL (Given)
*        MEM3 argument ALPHA.
*     BETA = REAL (Given)
*        MEM3 argument BETA.
*     DEF = REAL (Given)
*        MEM3 argument DEF.
*     SIGMA = REAL (Given)
*        MEM3 argument SIGMA.
*     NITER = INTEGER (Given)
*        Maximum number of iterations to perform.
*     ITER = INTEGER (Given)
*        Number of iterations completed.
*     ILEVEL = INTEGER (Given)
*        User-information level.
*     METHOD = INTEGER (Given)
*        MEM3 argument METHOD.
*     STDEV = REAL (Given)
*        Initial guess at the Gaussian noise level.
*     ISTAT = INTEGER (Given)
*        MEM3 argument ISTAT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If a new NDF is created, the WCS, AXIS, QUALITY and UNITS components
*     of the input NDF are propagated to it.
*     -  See SUN/117 for details of the MEM3 parameters.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
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
*     25-FEB-1991 (DSB):
*        1) Variable default model in file <20> allowed for.
*        2) Hidden image retained in file <1> in the MEM2D extension
*           rather than the image blurred with ICF.
*     28-FEB-1991 (DSB):
*        Name changed from NDFOUT to KPS1_NDFOU.
*     1991 July 4 (MJC):
*        Propagated AXIS and UNITS.  Name changed from KPS1_NDFOU to
*        KPS1_MEMSA.  Renamed calls to KPS1_SETOU to KPS1_MEMOU.
*     22-MAR-1995 (DSB):
*        Modified to allow use of external arrays.
*     1995 April 7 (MJC):
*        Minor stylistic changes.  Stripped trailing blanks.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
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
                                 ! TROPUS.
      INCLUDE 'ME_COM'           ! Common blocks etc required by MEMSYS3
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) MODE
      LOGICAL     EXTEND
      INTEGER     INDF
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
      INTEGER     ISTAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL      BAD           ! TRUE if o/p contains any bad pixels
      LOGICAL      EXISTS        ! True if o/p already contains an
				 ! extension for analysis information
      CHARACTER * ( 9 ) FORM     ! Storage form of o/p; SIMPLE or
                                 ! PRIMITIVE.
      INTEGER      IERR          ! Dummy argument
      INTEGER      IPOUT         ! Pointer to output data array
      CHARACTER * ( DAT__SZLOC ) LOC1 ! HDS locator to MEM2D extension
      INTEGER      NEL           ! No. of elements in the data array
      INTEGER      NERR          ! Dummy argument
      INTEGER      ONDF          ! NDF identifier for output
      INTEGER      STLEN         ! Length of input NDF name
      CHARACTER * ( 132 ) STRING ! Input NDF name
      INTEGER      SZAREA        ! No. of elements in a MEMSYS3 area

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the size of a full MEMSYS3 area.
      SZAREA = ME_MK * ME_NK

*  Start a new NDF context
      CALL NDF_BEGIN

*  Either create a new NDF or access an existing NDF for update.
      IF ( MODE .EQ. 'WRITE' ) THEN
         CALL LPG_PROP( INDF, 'WCS,AXIS,QUALITY,UNITS', PARAM, ONDF,
     :                  STATUS )

      ELSE
         CALL LPG_ASSOC( PARAM, 'UPDATE', ONDF, STATUS )

      END IF

*  If required, get an HDS locator to an extension called MEM2D which
*  holds the analysis information.
      IF ( EXTEND ) THEN

         CALL NDF_XSTAT( ONDF, 'MEM2D', EXISTS, STATUS )

*  If the extension already exists, access it for UPDATE mode.
         IF ( EXISTS ) THEN
            CALL NDF_XLOC( ONDF, 'MEM2D', 'UPDATE', LOC1, STATUS )

*  If no extension exists, create it.
         ELSE
            CALL NDF_XNEW( ONDF, 'MEM2D', 'MEM2D_EXTENSION', 0, 0, LOC1,
     :                     STATUS )

*  Create components within the new MEM2D extension to hold all the
*  required information............

*  MEMSYS3 areas:
            CALL DAT_NEW1R( LOC1, 'FILE1', SZAREA, STATUS )
            CALL DAT_NEW1R( LOC1, 'FILE3', SZAREA, STATUS )
            CALL DAT_NEW1R( LOC1, 'FILE21', SZAREA, STATUS )
            CALL DAT_NEW1R( LOC1, 'FILE22', SZAREA, STATUS )
            CALL DAT_NEW1R( LOC1, 'FILE23', SZAREA, STATUS )
            CALL DAT_NEW1R( LOC1, 'FILE24', SZAREA, STATUS )

            IF ( ME_KB(20) .GT. 0 ) THEN
               CALL DAT_NEW1R( LOC1, 'FILE20', SZAREA, STATUS )
            END IF

*  Common block /MECOMP/
            CALL DAT_NEW1I( LOC1, 'KC', 40, STATUS )
            CALL DAT_NEW1I( LOC1, 'KD', 40, STATUS )
            CALL DAT_NEW0I( LOC1, 'L0', STATUS )
            CALL DAT_NEW0I( LOC1, 'L1', STATUS )
            CALL DAT_NEW0I( LOC1, 'M0', STATUS )
            CALL DAT_NEW0I( LOC1, 'M1', STATUS )
            CALL DAT_NEW0I( LOC1, 'M2', STATUS )
            CALL DAT_NEW0I( LOC1, 'M3', STATUS )
            CALL DAT_NEW0I( LOC1, 'NTRANS', STATUS )

*  Common block /MECOML/
            CALL DAT_NEW1D( LOC1, 'XTABLE', ME_SIZ, STATUS )
            CALL DAT_NEW1D( LOC1, 'YTABLE', ME_SIZ, STATUS )
            CALL DAT_NEW1D( LOC1, 'VTABLE', ME_SIZ, STATUS )
            CALL DAT_NEW0I( LOC1, 'NTABLE', STATUS )

*  Common block /C1_COM/
            CALL DAT_NEW0I( LOC1, 'C1_NPX', STATUS )
            CALL DAT_NEW0I( LOC1, 'C1_NLN', STATUS )
            CALL DAT_NEW1I( LOC1, 'C1_DIM', 2, STATUS )
            CALL DAT_NEW0I( LOC1, 'C1_XMG', STATUS )
            CALL DAT_NEW0I( LOC1, 'C1_YMG', STATUS )
            CALL DAT_NEW0R( LOC1, 'C1_ICF', STATUS )

*  Arguments to this routine.
            CALL DAT_NEW0R( LOC1, 'RATE', STATUS )
            CALL DAT_NEW0R( LOC1, 'ALPHA', STATUS )
            CALL DAT_NEW0R( LOC1, 'BETA', STATUS )
            CALL DAT_NEW0R( LOC1, 'DEF', STATUS )
            CALL DAT_NEW0R( LOC1, 'SIGMA', STATUS )
            CALL DAT_NEW0R( LOC1, 'STDEV', STATUS )
            CALL DAT_NEW0I( LOC1, 'NITER', STATUS )
            CALL DAT_NEW0I( LOC1, 'ITER', STATUS )
            CALL DAT_NEW0I( LOC1, 'ILEVEL', STATUS )
            CALL DAT_NEW0I( LOC1, 'METHOD', STATUS )
            CALL DAT_NEW0I( LOC1, 'ISTAT', STATUS )

*  The name of the input NDF
            CALL DAT_NEW0C( LOC1, 'INPUT', 132, STATUS )

         END IF

*  Now copy the data to the extension.............

*  MEMSYS3 areas:
         IF ( C1_WEXT ) THEN
            CALL CMP_PUT1R( LOC1, 'FILE1', SZAREA,
     :                      %VAL( CNF_PVAL( C1_IP( 1 ) ) ),
     :                      STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE3', SZAREA,
     :                      %VAL( CNF_PVAL( C1_IP( 3 ) ) ),
     :                      STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE21', SZAREA,
     :                      %VAL( CNF_PVAL( C1_IP( 21 ) ) ),
     :                      STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE22', SZAREA,
     :                      %VAL( CNF_PVAL( C1_IP( 22 ) ) ),
     :                      STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE23', SZAREA,
     :                      %VAL( CNF_PVAL( C1_IP( 23 ) ) ),
     :                      STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE24', SZAREA,
     :                      %VAL( CNF_PVAL( C1_IP( 24 ) ) ),
     :                      STATUS )

            IF ( ME_KB(20) .GT. 0 ) CALL CMP_PUT1R( LOC1, 'FILE20',
     :                                              SZAREA,
     :
     :   %VAL( CNF_PVAL( C1_IP( 20 ) ) ),
     :                                              STATUS )

         ELSE
            CALL CMP_PUT1R( LOC1, 'FILE1', SZAREA,
     :                      ME_ST( ME_KB( 1 ) ), STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE3', SZAREA,
     :                      ME_ST( ME_KB( 3 ) ), STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE21', SZAREA,
     :                      ME_ST( ME_KB( 21 ) ), STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE22', SZAREA,
     :                      ME_ST( ME_KB( 22 ) ), STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE23', SZAREA,
     :                      ME_ST( ME_KB( 23 ) ), STATUS )
            CALL CMP_PUT1R( LOC1, 'FILE24', SZAREA,
     :                      ME_ST( ME_KB( 24 ) ), STATUS )

            IF ( ME_KB(20) .GT. 0 ) THEN
               CALL CMP_PUT1R( LOC1, 'FILE20', SZAREA,
     :                         ME_ST( ME_KB( 20 ) ), STATUS )
            END IF
         END IF

*  Common block /MECOMP/
         CALL CMP_PUT1I( LOC1, 'KC', 40, ME_KC, STATUS )
         CALL CMP_PUT1I( LOC1, 'KD', 40, ME_KD, STATUS )
         CALL CMP_PUT0I( LOC1, 'NTRANS', ME_NTR, STATUS )

         IF ( .NOT. EXISTS ) THEN
            CALL CMP_PUT0I( LOC1, 'L0', ME_L0, STATUS )
            CALL CMP_PUT0I( LOC1, 'L1', ME_L1, STATUS )
            CALL CMP_PUT0I( LOC1, 'M0', ME_M0, STATUS )
            CALL CMP_PUT0I( LOC1, 'M1', ME_M1, STATUS )
            CALL CMP_PUT0I( LOC1, 'M2', ME_M2, STATUS )
            CALL CMP_PUT0I( LOC1, 'M3', ME_M3, STATUS )
         END IF

*  Common block /MECOML/
         CALL CMP_PUT1D( LOC1, 'XTABLE', ME_SIZ, ME_XTB, STATUS )
         CALL CMP_PUT1D( LOC1, 'YTABLE', ME_SIZ, ME_YTB, STATUS )
         CALL CMP_PUT1D( LOC1, 'VTABLE', ME_SIZ, ME_VTB, STATUS )
         CALL CMP_PUT0I( LOC1, 'NTABLE', ME_NTB, STATUS )

*  Common block /C1_COM/
         IF ( .NOT. EXISTS ) THEN
            CALL CMP_PUT0I( LOC1, 'C1_NPX', C1_NPX, STATUS )
            CALL CMP_PUT0I( LOC1, 'C1_NLN', C1_NLN, STATUS )
            CALL CMP_PUT1I( LOC1, 'C1_DIM', 2, C1_DIM, STATUS )
            CALL CMP_PUT0I( LOC1, 'C1_XMG', C1_XMG, STATUS )
            CALL CMP_PUT0I( LOC1, 'C1_YMG', C1_YMG, STATUS )
            CALL CMP_PUT0R( LOC1, 'C1_ICF', C1_ICF, STATUS )
         END IF

*  Arguments to this routine.
         CALL CMP_PUT0R( LOC1, 'RATE', RATE, STATUS )
         CALL CMP_PUT0R( LOC1, 'ALPHA', ALPHA, STATUS )
         CALL CMP_PUT0R( LOC1, 'BETA', BETA, STATUS )
         CALL CMP_PUT0R( LOC1, 'SIGMA', SIGMA, STATUS )
         CALL CMP_PUT0I( LOC1, 'NITER', NITER, STATUS )
         CALL CMP_PUT0I( LOC1, 'ITER', ITER, STATUS )
         CALL CMP_PUT0I( LOC1, 'ILEVEL', ILEVEL, STATUS )
         CALL CMP_PUT0I( LOC1, 'ISTAT', ISTAT, STATUS )

         IF ( .NOT. EXISTS ) THEN
            CALL CMP_PUT0R( LOC1, 'DEF', DEF, STATUS )
            CALL CMP_PUT0R( LOC1, 'STDEV', STDEV, STATUS )
            CALL CMP_PUT0I( LOC1, 'METHOD', METHOD, STATUS )

*  The name of the input NDF
            CALL NDF_MSG( 'NAME', INDF )
            CALL MSG_LOAD( 'REPORT', '^NAME', STRING, STLEN, STATUS )
            CALL CMP_PUT0C( LOC1, 'INPUT', STRING(:STLEN), STATUS )
         END IF

*  Release the HDS locator to the extension.
         CALL DAT_ANNUL( LOC1, STATUS )

      END IF

*  Map the output data array.
      CALL KPG1_MAP( ONDF, 'DATA', '_REAL', MODE, IPOUT, NEL, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Now deal with cases where the MEMSYS3 areas are stored in
*  a dynamically allocated external work array.
      IF ( C1_WEXT ) THEN

*  Copy the final "hidden" image in file <1> to file <5> and blur with
*  the ICF, to get the required deconvolved image. Files <2> and <4>
*  are used as work space. (The "hidden" image must be left in file <1>
*  for analysis/continuation).
         CALL VEC_RTOR( .FALSE., SZAREA, %VAL( CNF_PVAL( C1_IP( 1 ) ) ),
     :                  %VAL( CNF_PVAL( C1_IP( 5 ) ) ),
     :                  IERR, NERR, STATUS )
         CALL KPS1_ICBLU( C1_ICF, .FALSE.,
     :                    %VAL( CNF_PVAL( C1_IP( 2 ) ) ),
     :                    %VAL( CNF_PVAL( C1_IP( 4 ) ) ),
     :                    %VAL( CNF_PVAL( C1_IP( 5 ) ) ),
     :                    STATUS )

*  Copy the deconvolved image from file <5> to the output NDF.
         IF ( ME_KB( 20 ) .GT. 0) THEN
            CALL KPS1_MEMOU( %VAL( CNF_PVAL( C1_IP( 5 ) ) ),
     :                       %VAL( CNF_PVAL( C1_IP( 22 ) ) ),
     :                       C1_DIM( 1 ), C1_DIM( 2 ),
     :                       %VAL( CNF_PVAL( C1_IP( 20 ) ) ),
     :                       C1_NPX, C1_NLN,
     :                       C1_XMG, C1_YMG, %VAL( CNF_PVAL( IPOUT ) ),
     :                       BAD,
     :                       STATUS )
         ELSE
            CALL KPS1_MEMOU( %VAL( CNF_PVAL( C1_IP( 5 ) ) ),
     :                       %VAL( CNF_PVAL( C1_IP( 22 ) ) ),
     :                       C1_DIM( 1 ), C1_DIM( 2 ), DEF, 1, 1,
     :                       C1_XMG, C1_YMG, %VAL( CNF_PVAL( IPOUT ) ),
     :                       BAD,
     :                       STATUS )
         END IF

*  Now deal with cases where the MEMSYS3 areas are stored internally in
*  the array ME_ST in /MECOMS/.
      ELSE
         CALL VEC_RTOR( .FALSE., SZAREA, ME_ST( ME_KB( 1 ) ),
     :                  ME_ST( ME_KB( 5 ) ), IERR, NERR, STATUS )
         CALL KPS1_ICBLU( C1_ICF, .FALSE., ME_ST( ME_KB( 2 ) ),
     :                    ME_ST( ME_KB( 4 ) ), ME_ST( ME_KB( 5 ) ),
     :                    STATUS )

*  Copy the deconvolved image from file <5> to the output NDF.
         IF ( ME_KB( 20 ) .GT. 0) THEN
            CALL KPS1_MEMOU( ME_ST( ME_KB( 5 ) ), ME_ST( ME_KB( 22 ) ),
     :                       C1_DIM( 1 ), C1_DIM( 2 ),
     :                       ME_ST( ME_KB( 20 ) ), C1_NPX, C1_NLN,
     :                       C1_XMG, C1_YMG, %VAL( CNF_PVAL( IPOUT ) ),
     :                       BAD,
     :                       STATUS )
         ELSE
            CALL KPS1_MEMOU( ME_ST( ME_KB( 5 ) ), ME_ST( ME_KB( 22 ) ),
     :                       C1_DIM( 1 ), C1_DIM( 2 ), DEF, 1, 1,
     :                       C1_XMG, C1_YMG, %VAL( CNF_PVAL( IPOUT ) ),
     :                       BAD,
     :                       STATUS )
         END IF

      END IF

*  Unless the output NDF is primitive, set the bad pixel flag
*  in the output NDF.
      CALL NDF_FORM( ONDF, 'DATA', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD, ONDF, 'DATA', STATUS )
      END IF

*  Update the output title.
      CALL NDF_CINP( 'TITLE', ONDF, 'TITLE', STATUS )

*  Force the HDS file associated with the given parameter to be
*  updated, so that its memory cache coincides with the data on disk.
      CALL DAT_UPDAT( PARAM, STATUS )

*  End the NDF context
 999  CONTINUE

      CALL NDF_END( STATUS )

      END
