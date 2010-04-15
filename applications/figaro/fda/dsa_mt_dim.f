      SUBROUTINE DSA_MATCH_DIMENSION( DSARE1, AXIS1, DSARE2, AXIS2,
     :   STATUS )
*+
*  Name:
*     DSA_MATCH_DIMENSION

*  Purpose:
*     Check that two NDF dimensions match.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MATCH_DIMENSION( DSARE1, AXIS1, DSARE2, AXIS2, STATUS )

*  Description:
*     This routine checks that one specified dimension of one NDF
*     is the same as a specified dimension of another (or the same) NDF.
*     This is usually called prior to
*     an operation such as a copying of a one-dimensional array into a
*     single cross-section of another array, where it is important that
*     the dimensions match. If the specified dimensions do not match, an
*     error message is issued and a bad status value is returned.

*  Arguments:
*     DSARE1 = CHARACTER * ( * ) (Given)
*        The reference name associated with the first NDF.
*     AXIS1 = INTEGER (Given)
*        The dimension number in the first NDF.
*     DSARE2 = CHARACTER * ( * ) (Given)
*        The reference name associated with the second NDF.
*     AXIS1 = INTEGER (Given)
*        The dimension number in the first NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Jan 1989 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     21 Dec 2000 (acd):
*        Removed unused variables.
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

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSARE1
      INTEGER AXIS1
      CHARACTER * ( * ) DSARE2
      INTEGER AXIS2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLO1, SLO2         ! The reference slots
      INTEGER NDIM1, NDIM2       ! Centre array dimensionalities
      INTEGER DIM1( NDF__MXDIM ) ! Centre array dimensions
      INTEGER DIM2( NDF__MXDIM ) ! Centre array dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up both references.
      CALL DSA1_RFND( DSARE1, SLO1, STATUS )
      CALL DSA1_RFND( DSARE2, SLO2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Look up the dimensions of both NDFs (we're not interested in lower
*  bounds!)
      CALL NDF_DIM( DSA__REFID1(SLO1), NDF__MXDIM, DIM1, NDIM1, STATUS )
      CALL NDF_DIM( DSA__REFID1(SLO2), NDF__MXDIM, DIM2, NDIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Compare the two dimensions. First, check that each is valid.
      IF ( AXIS1 .GT. NDIM1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETI( 'FDA_T002', AXIS1 )
         CALL ERR_REP( 'FDA_E030', 'DSA_MATCH_DIMENSION: The axis ' //
     :      '^FDA_T002 is beyond the dimensionality in reference ' //
     :      '^FDA_T001.', STATUS )
         GO TO 500
      END IF
      IF ( AXIS2 .GT. NDIM2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL MSG_SETI( 'FDA_T008', AXIS2 )
         CALL ERR_REP( 'FDA_E031', 'DSA_MATCH_DIMENSION: The axis ' //
     :      '^FDA_T008 is beyond the dimensionality in reference ' //
     :      '^FDA_T007.', STATUS )
         GO TO 500
      END IF

*  Now the actual dimensions.
      IF ( DIM1(AXIS1) .NE. DIM2(AXIS2) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL MSG_SETI( 'FDA_T002', AXIS1 )
         CALL MSG_SETI( 'FDA_T008', AXIS2 )
         CALL ERR_REP( 'FDA_E032', 'DSA_MATCH_AXIS: The dimension ' //
     :      '^FDA_T002 in reference ^FDA_T001 and the dimension ' //
     :      '^FDA_T008 in reference ^FDA_T007 differ.', STATUS )
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
