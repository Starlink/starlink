      SUBROUTINE BIG1_ABIAT( DTYPE, IMAGE, DIM1, DIM2, INL, INR, SEED, 
     :                       STATUS )
*+
*  Name:
*     BIG1_ABIAT

*  Purpose:
*     To impose a reproducible pseudo-random bias frame to data.
*     This is done in two parts: over the bias strips noise only is
*     written, while between the bias strips noise is added to the
*     existing value of the image.
*     This routine is a harness for the BGG1_ABIA<T> routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BIG1_ABIAT( DTYPE, IMAGE, DIM1, DIM2, INL, INR, SEED, 
*                      STATUS )

*  Arguments:
*     DTYPE = CHARACTER * ( * )
*        Data type of the IMAGE array.
*     IMAGE( 1 ) = unknown array (Given and Returned)
*        The image to which noise is to be added.
*     DIM1 = INTEGER (Given)
*        First dimension of IMAGE.
*     DIM2 = INTEGER (Given)
*        Second dimension of IMAGE.
*     INL = INTEGER (Given)
*        Width of left bias strip.
*     INR = INTEGER (Given)
*        Width of right bias strip.
*     SEED = INTEGER (Given)
*        Integer seed, fed to PDA pseudo-random number routines
*        for generating a reproducible sequence of random numbers.
*        Should be of form 4 * INT + 1
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Assumes there are no bad pixels.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1998 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      INTEGER INL
      INTEGER INR
      INTEGER SEED

*  Arguments Given and Returned:
      INTEGER IMAGE( 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call appropriate routine according to data type.
      IF      ( DTYPE .EQ. '_WORD'    ) THEN
         CALL BGG1_ABIAW( IMAGE, DIM1, DIM2, INL, INR, SEED, STATUS )
      ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
         CALL BGG1_ABIAI( IMAGE, DIM1, DIM2, INL, INR, SEED, STATUS )
      ELSE IF ( DTYPE .EQ. '_REAL'    ) THEN
         CALL BGG1_ABIAR( IMAGE, DIM1, DIM2, INL, INR, SEED, STATUS )
      ELSE IF ( DTYPE .EQ. '_DOUBLE'  ) THEN
         CALL BGG1_ABIAD( IMAGE, DIM1, DIM2, INL, INR, SEED, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DTYPE', DTYPE )
         CALL ERR_REP( 'CCDBGEN_TYPE', 
        :              'CCDBGEN: Invalid data type (^DTYPE)', STATUS )
      END IF

      END
* $Id: ccdb1_abia.f,v 1.5 1998/06/17 16:53:26 mbt Exp $
