      SUBROUTINE LSFUN1( M, N, XC, FVECC )
*+
*  Name:
*     LSFUN1

*  Purpose:
*     Returns the least squares values to E04FDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LSFUN1( M, N, XC, FVECC )

*  Description:
*     This routine calculates the squared difference between the
*     current parameterised position and the reference positions.
*     It is used EXCLUSIVELY by CCD1_GFIT. No other routine in CCDPACK
*     should be called this name without extraction from the monolith.

*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals (=CCD1_NREC * 2).
*     N = INTEGER (Given)
*        The number of parameters (variables) =(CCD1_NPAR)
*     XC( N ) = DOUBLE PRECISION (Given)
*        The values of the parameters.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        The squared differeces.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'CCD1_FITCM'       ! Common block for passing fit
                                 ! information.
*        CCD1_IPPIN = INTEGER
*           Pointer to array to hold all the parameter values as
*           variables on input to the transformation.
*        CCD1_IPPO = INTEGER
*           Pointer to array to hold the output (transformed)
*           X and Y positions.
*        CCD1_IPX = INTEGER
*           Pointer to X reference positions.
*        CCD1_IPY = INTEGER
*           Pointer to Y reference positions.
*        CCD1_NDXY = INTEGER
*           Size of first dimension of CCD1_IPX and CCD1_IPY arrays
*           when declared.
*        CCD1_NREC = INTEGER
*           Number of X and Y values.
*        CCD1_NPAR = INTEGER
*           Number of parameters in forward transformation.
*        CCD1_IDTR = INTEGER
*           TRANSFORM identifier.

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )

*  Arguments Returned:
      DOUBLE PRECISION FVECC( M )

*  Local Variables:
      INTEGER STATUS             ! Local status

*.

*  Initialise STATUS
      STATUS = SAI__OK

*  Add the current parameter estimates to the input array prior to
*  transformation.
      CALL CCD1_ITRA( XC, %VAL( CCD1_IPX ), %VAL( CCD1_IPY ),
     :                CCD1_NDXY, CCD1_NREC, CCD1_NPAR + 2, .FALSE.,
     :                %VAL( CCD1_IPPIN ), STATUS )

*  Transform the data.
       CALL TRN_TRND( .FALSE., CCD1_NREC, CCD1_NPAR + 2, CCD1_NREC,
     :                %VAL( CCD1_IPPIN ), CCD1_IDTR, CCD1_NREC, 2,
     :                %VAL( CCD1_IPPO ), STATUS )

*  Now form the differences.
      CALL LSFUNS( %VAL( CCD1_IPX ), %VAL( CCD1_IPY ),
     :             %VAL( CCD1_IPPO ), CCD1_NREC, FVECC, STATUS )

      END

* $Id$
