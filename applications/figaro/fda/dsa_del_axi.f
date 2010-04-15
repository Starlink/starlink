      SUBROUTINE DSA_DELETE_AXIS( DSAREF, AXIS, STATUS )
*+
*  Name:
*     DSA_DELETE_AXIS

*  Purpose:
*     Delete a specified axis structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_DELETE_AXIS( DSAREF, AXIS, STATUS )

*  Description:
*     This routine removes the information for a specified axis from an
*     NDF. Only the compulsory centre array with its default values
*     remains. None of the arrays for this axis should be mapped when
*     this routine is called.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Status:
*     This routine ignores any width component.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14 Dec 1989 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     30 Jan 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Move main action to internal routine DSA1_DELAX.
*        Translate between application-side status and Starlink status.
*     20 Feb 1996 (hme):
*        Delete width component as well.
*     1996 July 9 (MJC):
*        Calls DSA2_AFILLF for DSA2_NFILLF to allow default axis
*        co-ordinates to be pixel indices.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
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
      CHARACTER * ( * ) DSAREF
      INTEGER AXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find reference slot and call the real routine.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_DELAX( SLOT, AXIS, STATUS )

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




      SUBROUTINE DSA1_DELAX( SLOT, AXIS, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT
      INTEGER AXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! Whether a structure exists
      INTEGER NDIM               ! NDF dimensionality
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER NELM               ! Array size
      INTEGER PNTR               ! Array pointer
      CHARACTER * ( DAT__SZLOC ) TLOC( 4 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the NDF has no axes at all, there is nothing to do.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'AXIS', EXIST, STATUS )
      IF ( .NOT. EXIST ) GO TO 500

*  Find the relevant NDF bounds.
      CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :   LBND, UBND, NDIM, STATUS )

*  Check the validity of the value of AXIS.
      IF ( AXIS .LT. 1 .OR. AXIS .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T002', AXIS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E011', 'DSA1_DELAX: Invalid axis ' //
     :      'number ^FDA_T002 for reference ^FDA_T001.', STATUS )
         GO TO 500
      END IF

*  Unmap any components in this axis.
*  Since this is wild-carded, no error will occur if nothing is mapped.
      CALL NDF_AUNMP( DSA__REFID1(SLOT), '*', AXIS, STATUS )

*  Reset each recognised component, bar CENTRE.
      CALL NDF_AREST( DSA__REFID1(SLOT), 'LABEL,UNITS,WIDTH',
     :   AXIS, STATUS )

*  Remove .MORE if it exists.
*  If something inside is mapped at the moment, that will cause an error
*  later. We don't bother here to check all map slots whether they refer
*  to this reference slot and this axis, and a buffer NDF for N-D centres.
      CALL NDF_LOC( DSA__REFID1(SLOT), 'UPDATE', TLOC(1), STATUS )
      CALL DAT_FIND( TLOC(1), 'AXIS', TLOC(2), STATUS )
      CALL DAT_CELL( TLOC(2), 1, AXIS, TLOC(3), STATUS )
      CALL DAT_THERE( TLOC(3), 'MORE', EXIST, STATUS )
      IF ( EXIST ) CALL DAT_ERASE( TLOC(3), 'MORE', STATUS )
      CALL DAT_ANNUL( TLOC(3), STATUS )
      CALL DAT_ANNUL( TLOC(2), STATUS )
      CALL DAT_ANNUL( TLOC(1), STATUS )

*  Initialise the compulsory 1-D centres.
      CALL NDF_ASTYP( '_REAL', DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :   STATUS )
      CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, '_REAL',
     :   'WRITE', PNTR, NELM, STATUS )
      CALL DSA2_AFILLF( NELM, LBND(AXIS), %VAL( CNF_PVAL(PNTR) ),
     :                  STATUS )
      CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )

*  Return.
 500  CONTINUE
      END
