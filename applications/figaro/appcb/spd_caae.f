      SUBROUTINE SPD_CAAE( NDF1, AXIS1, CLIST, NDF2, AXIS2,
     :   MATCH, STATUS )
*+
*  Name:
*     SPD_CAAE

*  Purpose:
*     Compare two NDF axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CAAE( NDF1, AXIS1, CLIST, NDF2, AXIS2, MATCH, STATUS )

*  Description:
*     This routine compares one specified axis from one NDF to another
*     specified axis of another NDF. Both NDFs or axes may be the same
*     or not. Which components of the axis are compared depends on the
*     CLIST argument.
*
*     The normalisation flag is compared if and only if centres are
*     compared. Character components are compared using a length of 64
*     characters; case is ignored. Arrays are compared using internal
*     type _REAL; two values are considered equal when their relative
*     difference is less than or equal to 1E-5.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        The identifier for the first NDF.
*     AXIS1 = INTEGER (Given)
*        The number of the first axis.
*     CLIST = CHARACTER * ( * ) (Given)
*        The components to be compared:
*        ' '   (none): check only the appropriate NDF bounds,
*        'C' (centre): check bounds, centres, normalisation flag,
*        '*'    (all): check bounds, label, unit, centres, widths,
*                      variances, normalisation flag.
*     NDF2 = INTEGER (Given)
*        The identifier for the second NDF.
*     AXIS2 = INTEGER (Given)
*        The number of the second axis.
*     MATCH = LOGICAL (Returned)
*        This is returned false, if any difference between the two axes
*        is detected.
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
*        Make limited use of CLIST.
*     25 Nov 1994 (hme):
*        Renamed from SPACH.
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

*  Arguments Returned:
      LOGICAL MATCH

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if strings are similar

*  Local Variables:
      LOGICAL NORM1              ! Normalisation flag
      LOGICAL NORM2              ! Normalisation flag
      LOGICAL EXIST1             ! True if component exists
      LOGICAL EXIST2             ! True if component exists
      INTEGER LB, UB             ! Temporary integers
      INTEGER PNTR1              ! Array pointer
      INTEGER PNTR2              ! Array pointer
      INTEGER NELM               ! Array size
      INTEGER NDIM               ! NDF dimensionality
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      CHARACTER * ( 64 ) STRNG1  ! String to compare
      CHARACTER * ( 64 ) STRNG2  ! String to compare

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default value.
      MATCH = .TRUE.

*  Check the bounds in question.
      CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      LB = LBND(AXIS1)
      UB = UBND(AXIS1)
      CALL NDF_BOUND( NDF2, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( LB .NE. LBND(AXIS2) .OR. UB .NE. UBND(AXIS2) )
     :   MATCH = .FALSE.
      IF ( .NOT. MATCH ) GO TO 500

*  If the component list is blank, this is it.
      IF ( CLIST .EQ. ' ' ) GO TO 500

*  Label. (Match is assumed if either is absent.)
      IF ( CLIST .EQ. '*' ) THEN
         CALL NDF_ASTAT( NDF1, 'LABEL', AXIS1, EXIST1, STATUS )
         CALL NDF_ASTAT( NDF2, 'LABEL', AXIS2, EXIST2, STATUS )
         IF ( EXIST1 .AND. EXIST2 ) THEN
            CALL NDF_ACGET( NDF1, 'LABEL', AXIS1, STRNG1, STATUS )
            CALL NDF_ACGET( NDF2, 'LABEL', AXIS2, STRNG2, STATUS )
            IF ( .NOT. CHR_SIMLR( STRNG1, STRNG2 ) ) MATCH = .FALSE.
         END IF
      END IF
      IF ( .NOT. MATCH ) GO TO 500

*  Unit. (Match is assumed if either is absent.)
      IF ( CLIST .EQ. '*' ) THEN
         CALL NDF_ASTAT( NDF1, 'UNITS', AXIS1, EXIST1, STATUS )
         CALL NDF_ASTAT( NDF2, 'UNITS', AXIS2, EXIST2, STATUS )
         IF ( EXIST1 .AND. EXIST2 ) THEN
            CALL NDF_ACGET( NDF1, 'UNITS', AXIS1, STRNG1, STATUS )
            CALL NDF_ACGET( NDF2, 'UNITS', AXIS2, STRNG2, STATUS )
            IF ( .NOT. CHR_SIMLR( STRNG1, STRNG2 ) ) MATCH = .FALSE.
         END IF
      END IF
      IF ( .NOT. MATCH ) GO TO 500

*  Centres and normalisation flag.
      IF ( CLIST .EQ. '*' .OR.
     :     CLIST .EQ. 'C' .OR. CLIST .EQ. 'c' ) THEN
         CALL NDF_AMAP( NDF1, 'CENTRE', AXIS1, '_REAL',
     :                  'READ', PNTR1, NELM, STATUS )
         CALL NDF_AMAP( NDF2, 'CENTRE', AXIS2, '_REAL',
     :                  'READ', PNTR2, NELM, STATUS )
         CALL SPD_UAALR( NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR2 ) ), 1E-5,
     :                   MATCH, STATUS )
         CALL NDF_AUNMP( NDF1, 'CENTRE', AXIS1, STATUS )
         CALL NDF_AUNMP( NDF2, 'CENTRE', AXIS2, STATUS )
         IF ( .NOT. MATCH ) GO TO 500
         CALL NDF_ANORM( NDF1, AXIS1, NORM1, STATUS )
         CALL NDF_ANORM( NDF2, AXIS2, NORM2, STATUS )
         MATCH = (         NORM1 .AND. NORM2   ) .OR.
     :           ( .NOT. ( NORM1  .OR. NORM2 ) )
      END IF
      IF ( .NOT. MATCH ) GO TO 500

*  Widths.
      IF ( CLIST .EQ. '*' ) THEN
         CALL NDF_AMAP( NDF1, 'WIDTH', AXIS1, '_REAL',
     :                  'READ', PNTR1, NELM, STATUS )
         CALL NDF_AMAP( NDF2, 'WIDTH', AXIS2, '_REAL',
     :                  'READ', PNTR2, NELM, STATUS )
         CALL SPD_UAALR( NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR2 ) ), 1E-5,
     :                   MATCH, STATUS )
         CALL NDF_AUNMP( NDF1, 'WIDTH', AXIS1, STATUS )
         CALL NDF_AUNMP( NDF2, 'WIDTH', AXIS2, STATUS )
      END IF
      IF ( .NOT. MATCH ) GO TO 500

*  Variances.
      IF ( CLIST .EQ. '*' ) THEN
         CALL NDF_AMAP( NDF1, 'VARIANCE', AXIS1, '_REAL',
     :                  'READ', PNTR1, NELM, STATUS )
         CALL NDF_AMAP( NDF2, 'VARIANCE', AXIS2, '_REAL',
     :                  'READ', PNTR2, NELM, STATUS )
         CALL SPD_UAALR( NELM, %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR2 ) ), 1E-5,
     :                   MATCH, STATUS )
         CALL NDF_AUNMP( NDF1, 'VARIANCE', AXIS1, STATUS )
         CALL NDF_AUNMP( NDF2, 'VARIANCE', AXIS2, STATUS )
      END IF
      IF ( .NOT. MATCH ) GO TO 500

*  Return.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) MATCH = .FALSE.
      END
