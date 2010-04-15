      SUBROUTINE SPD_FAAC( A_MNDF,
     :   A_DNELM, A_NCOMP, A_TNPAR, A_TYPE, STATUS )
*+
*  Name:
*     SPD_FAAC

*  Purpose:
*     Get general information on results structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FAAC( MNDF, DNELM, NCOMP, TNPAR, TYPE, STATUS )

*  Description:
*     This routine returns general information on the result structure
*     of the given main NDF. The result structure must have been
*     accessed beforehand with SPD_FAAA.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The identifier of the main NDF. This is used to identify the
*        result structure in question.
*     DNELM = INTEGER (Returned)
*        The size of the result structure NDF, i.e. the size of its data
*        and variance array.
*     NCOMP = INTEGER (Returned)
*        The number of components that the structure currently caters
*        for. This is the length of component-related vectors like
*        LINENAME and LABFREQ.
*     TNPAR = INTEGER (Returned)
*        The total number of components that the structure currently
*        caters for. This is the length of parameter related vectors
*        like PARATYPE.
*     TYPE( 3 ) = CHARACTER * ( * ) (Returned)
*        The data types used in the current access to the result
*        structure (not necessarily the data types used in the HDS
*        container file). The three types are for (1) data and variance
*        arrays, (2) the laboratory frequency vector, (3) the MASKL and
*        MASKR vectors. Each type can be '_REAL' or '_DOUBLE'
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants

*  Global Variables:
      INCLUDE 'SPD_FCOM'         ! Specdre FITRES common block

*  Arguments Given:
      INTEGER A_MNDF

*  Arguments Returned:
      INTEGER A_DNELM
      INTEGER A_NCOMP
      INTEGER A_TNPAR
      CHARACTER * ( * ) A_TYPE( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER SLOT               ! Slot number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the slot.
      SLOT = 0
      DO 1 I = 1, SPD__FMXR
         IF ( MNDF(I) .EQ. A_MNDF ) SLOT = I
 1    CONTINUE
      IF ( SLOT .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FAAC_E01', 'SPD_FAAC: Error: ' //
     :      'No result structure accessed for that main NDF.',
     :      STATUS )
         GO TO 500
      END IF

*  Simply copy the values from the slot in the common block into the
*  returned arguments.
      A_DNELM = DNELM(SLOT)
      A_NCOMP = NCOMP(SLOT)
      A_TNPAR = TNPAR(SLOT)
      DO 2 I = 1, 3
         A_TYPE(I) = TYPE(I,SLOT)
 2    CONTINUE

*  Return.
 500  CONTINUE
      END
