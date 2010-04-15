      SUBROUTINE SPD_CAAD( NDF1, AXIS1, CLIST, NDF2, AXIS2, STATUS )
*+
*  Name:
*     SPD_CAAD

*  Purpose:
*     Copy an axis between NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CAAD( NDF1, AXIS1, CLIST, NDF2, AXIS2, STATUS )

*  Description:
*     This routine copies one specified axis from one NDF into another
*     specified axis of another NDF. The second NDF must be a base NDF
*     and must not be the base of the first NDF. The components copied
*     by this routine are LABEL, UNITS, CENTRE, WIDTH and VARIANCE. In
*     addition the normalisation flag is enquired and set if necessary.
*     Character components are copied using a length of 64 characters.
*     Arrays are copied using internal type _REAL if the source is
*     _REAL, _DOUBLE otherwise. The permanent type of the target will
*     always be the same as the source type.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        The identifier for the source NDF.
*     AXIS1 = INTEGER (Given)
*        The number of the source axis.
*     CLIST = CHARACER * ( * ) (Given)
*        Unused. Should be given as '*'.
*     NDF2 = INTEGER (Given)
*        The identifier for the target NDF.
*     AXIS2 = INTEGER (Given)
*        The number of the target axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     01 Jul 1992 (hme):
*        Original version.
*     23 Jan 1993 (hme):
*        Take care that we don't get defaults for label and units.
*        'Copy' the non-existence of optional components, too.
*     24 Nov 1994 (hme):
*        Renamed from SPACG.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF1
      INTEGER AXIS1
      CHARACTER * ( * ) CLIST
      INTEGER NDF2
      INTEGER AXIS2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST1             ! True if a structure exists
      LOGICAL EXIST2             ! True if NDF2 has AXIS
      INTEGER LB, UB             ! Temporary integers
      INTEGER PNTR1, PNTR2       ! Array pointers
      INTEGER NELM               ! Array size
      INTEGER NDIM               ! NDF dimensionality
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      CHARACTER * ( 64 ) STRING  ! String to copy
      CHARACTER * ( NDF__SZTYP ) TYPE ! Data type

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the bounds in question.
      CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      LB = LBND(AXIS1)
      UB = UBND(AXIS1)
      CALL NDF_BOUND( NDF2, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( LB .NE. LBND(AXIS2) .OR. UB .NE. UBND(AXIS2) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CAAD_E01', 'Error copying axis ' //
     :      'between NDFs. The NDF bounds are different.', STATUS )
         GO TO 500
      END IF

*  If both NDFs have no AXIS structure, there is nothing to do.
      CALL NDF_STATE( NDF1, 'AXIS', EXIST1, STATUS )
      CALL NDF_STATE( NDF2, 'AXIS', EXIST2, STATUS )
      IF ( .NOT. ( EXIST1 .OR. EXIST2 ) ) GO TO 500

*  Label.
      CALL NDF_ASTAT( NDF1, 'LABEL', AXIS1, EXIST1, STATUS )
      IF ( EXIST1 ) THEN
         CALL NDF_ACGET( NDF1, 'LABEL', AXIS1, STRING, STATUS )
         CALL NDF_ACPUT( STRING, NDF2, 'LABEL', AXIS2, STATUS )
      ELSE
         CALL NDF_ASTAT( NDF2, 'LABEL', AXIS2, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL NDF_AREST( NDF2, 'LABEL', AXIS2, STATUS )
         END IF
      END IF

*  Unit.
      CALL NDF_ASTAT( NDF1, 'UNITS', AXIS1, EXIST1, STATUS )
      IF ( EXIST1 ) THEN
         CALL NDF_ACGET( NDF1, 'UNITS', AXIS1, STRING, STATUS )
         CALL NDF_ACPUT( STRING, NDF2, 'UNITS', AXIS2, STATUS )
      ELSE
         CALL NDF_ASTAT( NDF2, 'UNITS', AXIS2, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL NDF_AREST( NDF2, 'UNITS', AXIS2, STATUS )
         END IF
      END IF

*  Centres. (These must exist, since the axis structures exist.)
      CALL NDF_ATYPE( NDF1, 'CENTRE', AXIS1, TYPE, STATUS )
      CALL NDF_ASTYP( TYPE, NDF2, 'CENTRE', AXIS2, STATUS )
      IF ( TYPE .NE. '_REAL' ) TYPE = '_DOUBLE'
      CALL NDF_AMAP( NDF1, 'CENTRE', AXIS1, TYPE, 'READ',
     :               PNTR1, NELM, STATUS )
      CALL NDF_AMAP( NDF2, 'CENTRE', AXIS2, TYPE, 'WRITE',
     :               PNTR2, NELM, STATUS )
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOD( .FALSE., NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                  %VAL( CNF_PVAL( PNTR2 ) ), LB, UB, STATUS )
      ELSE
         CALL VEC_RTOR( .FALSE., NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                  %VAL( CNF_PVAL( PNTR2 ) ), LB, UB, STATUS )
      END IF
      CALL NDF_AUNMP( NDF1, 'CENTRE', AXIS1, STATUS )
      CALL NDF_AUNMP( NDF2, 'CENTRE', AXIS2, STATUS )

*  Widths.
      CALL NDF_ASTAT( NDF1, 'WIDTH', AXIS1, EXIST1, STATUS )
      IF ( EXIST1 ) THEN
         CALL NDF_ATYPE( NDF1, 'WIDTH', AXIS1, TYPE, STATUS )
         CALL NDF_ASTYP( TYPE, NDF2, 'WIDTH', AXIS2, STATUS )
         IF ( TYPE .NE. '_REAL' ) TYPE = '_DOUBLE'
         CALL NDF_AMAP( NDF1, 'WIDTH', AXIS1, TYPE, 'READ',
     :                  PNTR1, NELM, STATUS )
         CALL NDF_AMAP( NDF2, 'WIDTH', AXIS2, TYPE, 'WRITE',
     :                  PNTR2, NELM, STATUS )
         IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .FALSE., NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), LB, UB, STATUS )
         ELSE
            CALL VEC_RTOR( .FALSE., NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), LB, UB, STATUS )
         END IF
         CALL NDF_AUNMP( NDF1, 'WIDTH', AXIS1, STATUS )
         CALL NDF_AUNMP( NDF2, 'WIDTH', AXIS2, STATUS )
      ELSE
         CALL NDF_ASTAT( NDF2, 'WIDTH', AXIS2, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL NDF_AREST( NDF2, 'WIDTH', AXIS2, STATUS )
         END IF
      END IF

*  Variances.
      CALL NDF_ASTAT( NDF1, 'VARIANCE', AXIS1, EXIST1, STATUS )
      IF ( EXIST1 ) THEN
         CALL NDF_ATYPE( NDF1, 'VARIANCE', AXIS1, TYPE, STATUS )
         CALL NDF_ASTYP( TYPE, NDF2, 'VARIANCE', AXIS2, STATUS )
         IF ( TYPE .NE. '_REAL' ) TYPE = '_DOUBLE'
         CALL NDF_AMAP( NDF1, 'VARIANCE', AXIS1, TYPE, 'READ',
     :                  PNTR1, NELM, STATUS )
         CALL NDF_AMAP( NDF2, 'VARIANCE', AXIS2, TYPE, 'WRITE',
     :                  PNTR2, NELM, STATUS )
         IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .FALSE., NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :         %VAL( CNF_PVAL( PNTR2 ) ), LB, UB, STATUS )
         ELSE
            CALL VEC_RTOR( .FALSE., NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :         %VAL( CNF_PVAL( PNTR2 ) ), LB, UB, STATUS )
         END IF
         CALL NDF_AUNMP( NDF1, 'VARIANCE', AXIS1, STATUS )
         CALL NDF_AUNMP( NDF2, 'VARIANCE', AXIS2, STATUS )
      ELSE
         CALL NDF_ASTAT( NDF2, 'VARIANCE', AXIS2, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL NDF_AREST( NDF2, 'VARIANCE', AXIS2, STATUS )
         END IF
      END IF

*  Normalisation flag.
      CALL NDF_ANORM( NDF1, AXIS1, EXIST1, STATUS )
      IF ( EXIST1 ) CALL NDF_ASNRM( EXIST1, NDF2, AXIS2, STATUS )

*  Return.
 500  CONTINUE
      END
