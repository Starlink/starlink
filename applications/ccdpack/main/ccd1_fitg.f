      SUBROUTINE CCD1_FITG( MAP, X, Y, PARNM, NPAR, TOLER, IPID1,
     :                      IPX1, IPY1, NREC1, IPID2, IPX2, IPY2,
     :                      NREC2, PARVAL, STATUS )
*+
*  Name:
*     CCD1_FITG

*  Purpose:
*     Performs the fitting of a general transformation between two
*     list of positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_FITG( MAP, X, Y, PARNM, NPAR, TOLER, IPID1, IPX1, IPY1,
*                      NREC1, IPID2, IPX2, IPY2, NREC2, PARVAL, STATUS )

*  Description:
*     This routine determines a least squares minimisation solution to
*     the transformation between the points in the input lists which
*     have the same identifiers. The transformations are defined in MAP
*     which contains TRANSFORM (SUN/61) like statements. The parameters
*     present in the transformation strings should be listed in the
*     PARNM. These values are the `variables' for which a minimisation
*     is performed. The transformation strings must contain references
*     to the values given in the variables X and Y -- these are the
*     tokens which represent the input points.  The results of the
*     minimisation process are returned in the array PARVAL and the
*     corresponding parameterisations are PNAM. The actual number of
*     unique parameters are NPAR.
*
*     Before performing the fit this routine matches the identifiers.
*     It then calls a NAg routine to determine the transformation.

*  Arguments:
*     MAP( 2 ) = CHARACTER * ( * ) (Given)
*        Transformations defining how X and Y mapping to the new X and
*        Y. These should be complete TRANSFORM statements with
*        references to the varaibles X and Y and parameterised by the
*        XPARNM and YPARNM values.
*     X = CHARACTER * ( * ) (Given)
*       The character value of the token in the MAP strings which
*       represents the first of the variables.
*     Y = CHARACTER * ( * ) (Given)
*       The character value of the token in the MAP strings which
*       represents the second of the variables.
*     PARNM( NPAR ) = CHARACTER * ( * ) (Given and Returned)
*        The names of the parameters which are present in string
*        MAP. (Things like PA, PB etc.). These tokens should be unique
*        in so much that no duplication of the same name occurs. The
*        size of this array should actually be 2 elements greater than
*        the number of parameters. On exit this contains the values
*        X and Y in the last two positions, the other entrie remain
*        unchanged.
*     NPAR = INTEGER (Given)
*        The number of parameter names.
*     TOLER = DOUBLE PRECISION (Given)
*        The tolerance in the required in the fit.
*     IPID1 = INTEGER (Given and Returned)
*        Pointer to an INTEGER array of size NREC1, which contains the
*        identifiers for positions IPX1, IPY1. On exit the data pointed
*        to by this has been rearranged to match the identifiers to the
*        other input list. No values are removed and the correspondence
*        with IPX1, IPY1 is maintained.
*     IPX1 = INTEGER (Given and Returned)
*        Pointer to a DOUBLE PRECISION array of size (NREC1). This
*        contains the X positions related to identifiers ID1. On
*        exit the data pointed to by this has been rearranged to match
*        the identifiers to the other input list. No values are removed
*        and the correspondence to IPID1 is maintained.
*     IPY1 = INTEGER (Given and Returned)
*        Pointer to a DOUBLE PRECISION array of size (NREC1). This
*        contains the Y positions related to identifiers ID1. On
*        exit the data pointed to by this has been rearranged to match
*        the identifiers to the other input list. No values are removed
*        and the correspondence to IPID1 is maintained.
*     NREC1 = INTEGER (Given)
*        Number of entries in input lists IPID1 and IPX1, IPY1.
*     IPID2 = INTEGER (Given and Returned)
*        Pointer to an INTEGER array of size NREC2, which contains the
*        identifiers for positions IPX2, IPY2. On exit the data pointed
*        to by this has been rearranged to match the identifiers to the
*        other input list. No values are removed and the correspondence
*        with IPX2, IPY2 is maintained.
*     IPX2 = INTEGER (Given and Returned)
*        Pointer to a DOUBLE PRECISION array of size (NREC2). This
*        contains the X positions related to identifiers ID2. On
*        exit the data pointed to by this has been rearranged to match
*        the identifiers to the other input list.  No values are
*        removed and the correspondence to IPID2 is maintained.
*     IPY2 = INTEGER (Given and Returned)
*        Pointer to a DOUBLE PRECISION array of size (NREC2). This
*        contains the Y positions related to identifiers ID2. On
*        exit the data pointed to by this has been rearranged to match
*        the identifiers to the other input list.  No values are
*        removed and the correspondence to IPID2 is maintained.
*     NREC2 = INTEGER (Given)
*        Number of entries in input lists IPID2 and IPX2, IPY2.
*     PARVAL( * ) = DOUBLE PRECISION (Given and Returned)
*        The parameters which define the best fit. On entry these
*        values should be set at initial guesses for the minimisation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*      -  This routine accesses and releases its own workspace.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     10-JUL-1992 (PDRAPER):
*        Original version.
*     16-JUL-1993 (PDRAPER):
*        Added numeric error traps.
*     17-SEP-1996 (PDRAPER):
*        Converted to use PDA_LMDIF1 rather than E04FDF (denagged).
*     4-OCT-1996 (PDRAPER):
*        Changed LIB$ call to NUM_.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants

*  Global Variables:

                                 ! defines NUM_ERROR
      INCLUDE 'CCD1_FITCM'       ! Common block for passing fit
                                 ! information to LSFUN1 subroutine
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CCD1_IPPIN = INTEGER (Write)
*           Pointer to array to hold all the parameter values as
*           variables on input to the transformation.
*        CCD1_IPPO = INTEGER (Write)
*           Pointer to array to hold the output (transformed)
*           X and Y positions.
*        CCD1_IPX = INTEGER (Write)
*           Pointer to X reference positions. (IPX2)
*        CCD1_IPY = INTEGER (Write)
*           Pointer to Y reference positions. (IPY2)
*        CCD1_NDXY = INTEGER (Write)
*           Size of first dimension of CCD1_IPX and CCD1_IPY arrays
*           when declared.
*        CCD1_NREC = INTEGER (Write)
*           Number of values in X and Y lists.
*        CCD1_NPAR = INTEGER (Write)
*           Number of parameters in forward transformation.
*        CCD1_IDTR = INTEGER (Write)
*           TRANSFORM identifier

*  Arguments Given:
      CHARACTER * ( * ) MAP( 2 )
      CHARACTER * ( * ) X
      CHARACTER * ( * ) Y
      INTEGER NPAR
      INTEGER NREC1
      INTEGER NREC2
      DOUBLE PRECISION TOLER

*  Arguments Given and Returned:
      CHARACTER * ( * ) PARNM( * )
      INTEGER IPID1
      INTEGER IPX1
      INTEGER IPY1
      INTEGER IPID2
      INTEGER IPX2
      INTEGER IPY2

*  Arguments Returned:
      DOUBLE PRECISION PARVAL( * )

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP          ! Condition handler
      EXTERNAL CCD1_LSFUN1      ! Sum of squares function
      EXTERNAL NUM_WASOK
      LOGICAL NUM_WASOK          ! Was numeric operation ok?

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator for compiled transform
      INTEGER IPOK              ! Pointer to logical flags workspace
      INTEGER IPFVEC            ! Pointer to list of final residuals
      INTEGER IPWRK1            ! Workspace
      INTEGER IPWRK2            ! Workspace
      INTEGER NWORK             ! Amount of workspace required
      INTEGER IFAIL             ! Fit routine status flag
      INTEGER NRES              ! Number of residuals
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the condtion handler
      CALL NUM_HANDL( NUM_TRAP )

*  Initialise the error counter.
      CALL NUM_CLEARERR()

*  Match the identifiers of the lists and remove unmatched positions.
      CALL CCD1_MTCHL( %VAL( CNF_PVAL( IPID1 ) ),
     :                 %VAL( CNF_PVAL( IPX1 ) ),
     :                 %VAL( CNF_PVAL( IPY1 ) ), NREC1,
     :                 %VAL( CNF_PVAL( IPID2 ) ),
     :                 %VAL( CNF_PVAL( IPX2 ) ),
     :                 %VAL( CNF_PVAL( IPY2 ) ), NREC2,
     :                 CCD1_NREC, STATUS )

*  Only proceed if NMATCH is greater than zero, otherwise issue an
*  error.
      IF ( CCD1_NREC .GT. 0 ) THEN

*  Check that a determination of the transformation with this number of
*  free parameters is possible.
         IF ( NPAR .GT. CCD1_NREC * 2 ) THEN

*  Cannot solve the system of equations.
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NPAR )
            CALL MSG_SETI( 'M', CCD1_NREC * 2 )
            CALL ERR_REP( 'CCD1_FITG_ERR1',
     :      '  Cannot solve for ^N unknowns with ^M data points.'//
     :      'Reduce the number of variables or add more data points',
     :      STATUS )
            GO TO 99
         END IF

*  Extend the list of parameters to include the pseudo parameters
*  which represent the data points.

*  Add X and Y to this list.
         CCD1_NPAR = NPAR
         PARNM( CCD1_NPAR + 1 ) = X
         PARNM( CCD1_NPAR + 2 ) = Y

*  Compile the transformation. All the parameters as well as the
*  variables X and Y are treated as tokens. The point data is given in
*  an array, in which the current value of the parameters are also
*  stored all forming part of a general transformation.  Using this
*  method greatly increases the speed of execution of the transform
*  (which requires compilation just once, instead of for every
*  iteration), but does have the overhead of additional complexity.
         LOCTR = DAT__NOLOC
         CALL TRN_NEW( CCD1_NPAR + 2, 2, MAP, PARNM, '_DOUBLE',
     :                 'CCDREG_transformation', ' ', 'CCDREG_TRN',
     :                 LOCTR, STATUS )
         CALL TRN_COMP( LOCTR, .TRUE., CCD1_IDTR, STATUS )

*  Now get workspace for storing the pre-transformed variable data.
*  (Note that the parameters as well as X and Y variables are treat in
*  this fashion, hence the +2.)
         CALL CCD1_MALL( CCD1_NREC * ( CCD1_NPAR + 2 ), '_DOUBLE',
     :                   CCD1_IPPIN, STATUS )

*  Workspace for storing the output (transformed ) positions.
         CALL CCD1_MALL( CCD1_NREC * 2, '_DOUBLE', CCD1_IPPO, STATUS )

*  Transfer these to the input transformation array.
         CALL CCD1_ITRA( PARVAL, %VAL( CNF_PVAL( IPX1 ) ),
     :                   %VAL( CNF_PVAL( IPY1 ) ), NREC1,
     :                   CCD1_NREC, CCD1_NPAR + 2, .TRUE.,
     :                   %VAL( CNF_PVAL( CCD1_IPPIN ) ), STATUS )

*  Set pointer to access reference XY positions from LSFUN1.
         CCD1_IPX = IPX2
         CCD1_IPY = IPY2
         CCD1_NDXY = NREC2

*  Get workspace for minimisation routine.
         NRES = CCD1_NREC * 2
         CALL CCD1_MALL( NRES, '_DOUBLE', IPFVEC, STATUS )
         CALL CCD1_MALL( CCD1_NPAR, '_INTEGER', IPWRK1, STATUS )
         NWORK = NRES * CCD1_NPAR + 5 * CCD1_NPAR + NRES
         CALL CCD1_MALL( NWORK, '_DOUBLE', IPWRK2, STATUS )

*  Trap any STATUS .NE. SAI__OK value before calling NAg routine.
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now try to determine the fit parameters.
         CALL PDA_LMDIF1( CCD1_LSFUN1, NRES, CCD1_NPAR, PARVAL,
     :                    %VAL( CNF_PVAL( IPFVEC ) ), TOLER, IFAIL,
     :                    %VAL( CNF_PVAL( IPWRK1 ) ),
     :                    %VAL( CNF_PVAL( IPWRK2 ) ), NWORK )

*  Trap numeric errors.
         IF ( .NOT. NUM_WASOK() ) THEN
            CALL NUM_GETERR()
            CALL ERR_REP( ' ',
     :'  A numeric error occurred when performing fit', STATUS )
            GO TO 99
         END IF
         IF ( IFAIL .GT. 3 ) THEN

*  Issue a warning that minimum may not have been achieved.
            CALL CCD1_MSG( ' ', '  Warning - fit may not be good',
     :                     STATUS )
         END IF
      END IF

*  Exit label
 99   CONTINUE

*  Close down transform and release workspace.
      CALL TRN_ANNUL( CCD1_IDTR, STATUS )
      CALL DAT_ANNUL( LOCTR, STATUS )
      CALL TRN_CLOSE( STATUS )
      CALL CCD1_MFREE( CCD1_IPPIN, STATUS )
      CALL CCD1_MFREE( CCD1_IPPO, STATUS )
      CALL CCD1_MFREE( IPOK, STATUS )
      CALL CCD1_MFREE( IPWRK1, STATUS )
      CALL CCD1_MFREE( IPWRK2, STATUS )
      CALL CCD1_MFREE( IPFVEC, STATUS )

*  Remove the condtion handler.
      CALL NUM_REVRT

      END
* $Id$
