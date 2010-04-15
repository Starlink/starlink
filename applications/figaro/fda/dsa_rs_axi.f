
      SUBROUTINE DSA_RESHAPE_AXIS( DSAREF, AXIS,
     :                             MODREF, MODAXS, NDIM, DIMS, STATUS )
*+
*  Name:
*     DSA_RESHAPE_AXIS

*  Purpose:
*     Create axis arrays with modified dimensions in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_RESHAPE_AXIS( DSAREF, AXIS,
*        MODREF, MODAXS, NDIM, DIMS, STATUS )

*  Description:
*     This routine combines the given shape for an axis centre (and
*     width) array with a model axis. The model axis determines which
*     axis components are created and what types and values they have.
*
*     Namely, the types and values of the label, unit, and logarithmic
*     binning flag are copied from the model. Any of these that does not
*     exist in the model will be removed from the target as well.
*
*     The target's centre array will have the type and values from the
*     model, but the shape from the given arguments. Note that the
*     values will most probably make no sense if the shape is different
*     to that of the model.
*
*     If the model has a width component, then a width array of the
*     given shape will be created in the target, replacing any width
*     that might have existed in the target. As for the centre array,
*     type and values are copied from the model. If the model has no
*     width then the target will have no width either.
*
*     If the model and target are the same axis in the same data
*     structure and if the shape also is to be kept the same, then the
*     target will be left unchanged.
*
*     If the model axis does not exist (i.e. if the model data
*     structure has no axis information at all for any of its axes),
*     then the target is left unchanged. It is not emptied. This is just
*     as in earlier implementations.
*
*     Contrary to earlier implementations, this routine returns with an
*     error if the given shape is inconsistent with the target's main
*     data shape. Therefore, the main data should be reshaped before the
*     axes are rehsaped.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis to be reshaped.
*     MODREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the structure whose axis
*        data is to serve as model for the reshaped structure.
*     MODAXS = INTEGRE (Given)
*        The number of the axis in the model structure that is to serve
*        as the model for the reshaped axis.
*     NDIM = INTEGER (Given)
*        The number of dimensions for the reshaped data.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the reshaped data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Status:
*     The code to copy the width array is yet missing.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     MJC: Malcolm J. Currie (STARLINK)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Jul 1988 (ks):
*        Original version.
*     18 Jul 1988 (ks):
*        Check that input axis exists added.
*     11 Dec 1989 (ks):
*        Sets axis-reshaped flag.
*     02 Mar 1990 (ks):
*        Modified to use DSA__ routines rather than assuming the
*        original Figaro format.
*     07 Mar 1990 (ks):
*        Now clears the common AXIS_EXIST flag.
*     24 Mar 1991 (ks):
*        Handle case where data file types are incompatible.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     15 Oct 1992 (hme):
*        Refuse creation of an N-D axis in an NDF.
*     13 Feb 1995 (ks):
*        Previous change reversed. N-D arrays are now allowed to be
*        created in an NDF, but will be moved to the extension structure
*        when the structure is closed. Code now checks that it has in
*        fact reshaped the data array.
*     31 Jan 1996 (hme):
*        FDA library.
*     07 Feb 1996 (hme):
*        When copying data, fill target with bad values prior to copy.
*     12 Feb 1996 (hme):
*        Mark reference/axis as axis-reshaped.
*     19 Feb 1996 (hme):
*        Use a cloned model identfier for map access, in order to avoid
*        access conflicts.
*        Translate between application-side status and Starlink status.
*     20 Feb 1996 (hme):
*        Handle width.
*     01 Jul 1996 (mjcl):
*        Removed CHR_LEN-based string truncation from NDF_CPUT calls.
*     1996 July 9 (MJC):
*        Calls DSA2_ACRE for NDF_ACRE and DSA2_AFILLF for DSA2_NFILLF
*        to allow default axis co-ordinates to be pixel indices.
*     1996 July 15 (MJC):
*        Set DSA__REFMDC to .TRUE. when an n-d axis comes from the
*        model axis centres.  Likewise set DSA__REFMDW to .TRUE. for
*        n-d widths from the model axis widths.
*     21 Dec 2000 (acd):
*        Removed unused declaration of CHR_LEN.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      INTEGER AXIS
      CHARACTER * ( * ) MODREF
      INTEGER MODAXS
      INTEGER NDIM
      INTEGER DIMS( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER * ( DAT__SZNAM ) TMPNAM ! Temporary name for old array
      PARAMETER ( TMPNAM = 'OLDDATA' )

*  Local Variables:
      LOGICAL LTEMP              ! Ignored
      LOGICAL THERE              ! Whether HDS structures exist
      LOGICAL MATCH              ! True if axis dimensions match data
      LOGICAL FLAG( NDF__MXDIM ) ! Flags dimensions in data
      LOGICAL SAME               ! Indicates data structures are same
      LOGICAL CHANGE             ! Indicates change in dimensions
      LOGICAL AXISND             ! True if N-D centre array
      LOGICAL WTHERE             ! Whether model has width
      INTEGER I                  ! Loop index through axis dimensions
      INTEGER J                  ! Loop index through data dimensions
      INTEGER SLOT               ! Table slot number for ref name
      INTEGER MODSLT             ! Table slot number for model name
      INTEGER MODNDF             ! Cloned model NDF identifier
      INTEGER ODIM               ! # Dimensions in structure element
      INTEGER MDIM               ! # Dimensions in model data
      INTEGER ODIMS( NDF__MXDIM ) ! Dimensions of structure element
      INTEGER MDIMS( NDF__MXDIM ) ! Dimensions of model data
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER NELM1, NELM2       ! Array sizes
      INTEGER PNTR1, PNTR2       ! Array pointers
      CHARACTER * ( 256 ) STRING ! Any string for copying
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! Data type
      CHARACTER * ( DAT__SZLOC ) MLOC ! Model axis cell
      CHARACTER * ( DAT__SZLOC ) RLOC ! Target axis cell
      CHARACTER * ( DAT__SZLOC ) WIDLOC ! Locator to width array
      CHARACTER * ( DAT__SZLOC ) ARYLOC ! Locator to old array
      CHARACTER * ( DAT__SZLOC ) TMPLOC ! Locator to new array
      CHARACTER * ( DAT__SZLOC ) DELLOC ! Locator for deletion
      CHARACTER * ( DAT__SZLOC ) PARLOC ! Parent locator
      CHARACTER * ( DAT__SZLOC ) TLOC( 5 ) ! HDS locators
      CHARACTER * ( DAT__SZNAM ) ARYNAM ! Array component name
      CHARACTER * ( DAT__SZNAM ) DELNAM ! Component name for deletion
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK


*  Preliminaria.
*  =============

*  Look up reference names.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_RFND( MODREF, MODSLT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check the validity of the values of AXIS and MODAXS.
      IF ( AXIS .LT. 1 .OR. AXIS .GT. NDF__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T008', AXIS )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E043', 'DSA_RESHAPE_AXIS: Invalid axis ' //
     :      'number ^FDA_T008 for reference ^FDA_T007.', STATUS )
         GO TO 500
      ELSE IF ( MODAXS .LT. 1 .OR. MODAXS .GT. NDF__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T002', MODAXS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(MODSLT) )
         CALL ERR_REP( 'FDA_E044', 'DSA_RESHAPE_AXIS: Invalid axis ' //
     :      'number ^FDA_T002 for model reference ^FDA_T001.',
     :      STATUS )
         GO TO 500
      END IF

*  Continue that check by comparing with the main data shapes.
      CALL NDF_DIM( DSA__REFID1(MODSLT), NDF__MXDIM,
     :   ODIMS, ODIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( MODAXS .GT. ODIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T002', MODAXS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(MODSLT) )
         CALL ERR_REP( 'FDA_E045', 'DSA_RESHAPE_AXIS: Invalid axis ' //
     :      'number ^FDA_T002 for model reference ^FDA_T001.',
     :      STATUS )
         GO TO 500
      END IF
      CALL NDF_DIM( DSA__REFID1(SLOT), NDF__MXDIM,
     :   ODIMS, ODIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( AXIS .GT. ODIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T008', AXIS )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E046', 'DSA_RESHAPE_AXIS: Invalid axis ' //
     :      'number ^FDA_T008 for reference ^FDA_T007.', STATUS )
         GO TO 500
      END IF

*  Detailed check of the given shape against the main data.
      MATCH = NDIM .LE. ODIM .AND. DIMS(1) .EQ. ODIMS(AXIS)
      IF ( MATCH ) THEN
         DO 1 J = 1, ODIM
            FLAG(J)=.FALSE.
 1       CONTINUE
         FLAG(AXIS)=.TRUE.
         DO 4 I = 2, NDIM
            MATCH = .FALSE.
            DO 2 J = 1, ODIM
               IF ( DIMS(I) .EQ. ODIMS(J) ) THEN
                  FLAG(J) = .TRUE.
                  MATCH = .TRUE.
                  GO TO 3
               END IF
 2          CONTINUE
 3          CONTINUE
            IF ( .NOT. MATCH ) GO TO 5
 4       CONTINUE
 5       CONTINUE
      END IF
      IF ( .NOT. MATCH ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T008', AXIS )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E047', 'DSA_RESHAPE_AXIS: The requested ' //
     :      'shape for axis number ^FDA_T008 in the reference ' //
     :      '^FDA_T007 is incompatible with the shape of the main ' //
     :      'data array.', STATUS )
         GO TO 500
      END IF

*  Establish whether the model and target are the same axis in the same
*  NDF. Before, there would be only a check whether the two are in the
*  same NDF (or DST structure), but not whether they were the same axis
*  within that NDF. That was a bug, I presume.
      CALL NDF_SAME( DSA__REFID1(SLOT), DSA__REFID1(MODSLT),
     :   SAME, LTEMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      SAME = SAME .AND. AXIS .EQ. MODAXS

*  Establish whether the requested target shape is different from the
*  model shape.
      CALL DSA1_AXSIZ( MODSLT, MODAXS, NDF__MXDIM,
     :   MDIM, MDIMS, NELM1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CHANGE = MDIM .NE. NDIM
      IF ( .NOT. CHANGE ) THEN
         DO 6 I = 1, NDIM
            IF ( MDIMS(I) .NE. DIMS(I) ) CHANGE = .TRUE.
 6       CONTINUE
      END IF


*  Case of model == target and shape unchanged.
*  ============================================

*  If the input and output axis arrays are the same, and the dimensions
*  are unchanged, then we don't want to do anything.
      IF ( SAME .AND. .NOT. CHANGE ) GO TO 500


*  Case of empty model axis.
*  =========================

*  If the model axis does not exist, then the target is left alone.
*  In NDF the model axis can be completely absent only if all axes are
*  absent, so this case is less likely than it used to be.
      CALL NDF_STATE( DSA__REFID1(MODSLT), 'AXIS', THERE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( .NOT. THERE ) GO TO 500


*  Locate the model and target axis cells.
*  =======================================

*  If the target NDF has no AXIS vector yet create it.
      CALL DSA2_ACRE( DSA__REFID1(SLOT), STATUS )

*  Locate the two AXIS(...) cells (we know both exist).
      CALL NDF_LOC( DSA__REFID1(MODSLT), 'READ', TLOC(1), STATUS )
      CALL DAT_FIND( TLOC(1), 'AXIS', TLOC(2), STATUS )
      CALL DAT_CELL( TLOC(2), 1, MODAXS, MLOC, STATUS )
      CALL DAT_ANNUL( TLOC(2), STATUS )
      CALL DAT_ANNUL( TLOC(1), STATUS )
      CALL NDF_LOC( DSA__REFID1(SLOT), 'UPDATE', TLOC(1), STATUS )
      CALL DAT_FIND( TLOC(1), 'AXIS', TLOC(2), STATUS )
      CALL DAT_CELL( TLOC(2), 1, AXIS, RLOC, STATUS )
      CALL DAT_ANNUL( TLOC(2), STATUS )
      CALL DAT_ANNUL( TLOC(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( MLOC, STATUS )
         CALL DAT_ANNUL( RLOC, STATUS )
         GO TO 500
      END IF


*  Copy scalars.
*  =============

      IF ( .NOT. SAME ) THEN


*     Copy label.
*     -----------

*     Reset target.
*     If model has label, get model value and put into target.
         CALL NDF_AREST( DSA__REFID1(SLOT), 'LABEL', AXIS, STATUS )
         CALL NDF_ASTAT( DSA__REFID1(MODSLT), 'LABEL', MODAXS,
     :      THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_ACGET( DSA__REFID1(MODSLT), 'LABEL',
     :         MODAXS, STRING, STATUS )
            CALL NDF_ACPUT( STRING, DSA__REFID1(SLOT),
     :         'LABEL', AXIS, STATUS )
         END IF


*  Copy unit.
*  ----------

*     Reset target.
*     If model has label, get model value and put into target.
         CALL NDF_AREST( DSA__REFID1(SLOT), 'UNITS', AXIS, STATUS )
         CALL NDF_ASTAT( DSA__REFID1(MODSLT), 'UNITS', MODAXS,
     :      THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_ACGET( DSA__REFID1(MODSLT), 'UNITS',
     :         MODAXS, STRING, STATUS )
            CALL NDF_ACPUT( STRING, DSA__REFID1(SLOT),
     :         'UNITS', AXIS, STATUS )
         END IF


*  Copy log-binning flag.
*  ----------------------

*     Remove target .MORE.
         CALL DAT_THERE( RLOC, 'MORE', THERE, STATUS )
         IF ( THERE ) CALL DAT_ERASE( RLOC, 'MORE', STATUS )

*     If model has .MORE.FIGARO.LOG.
         CALL DAT_THERE( MLOC, 'MORE', THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND(  MLOC, 'MORE', TLOC(1), STATUS )
            CALL DAT_THERE( TLOC(1), 'FIGARO', THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND(  TLOC(1), 'FIGARO', TLOC(2), STATUS )
               CALL DAT_THERE( TLOC(2), 'LOG', THERE, STATUS )
               IF ( THERE ) THEN

*              Create target .MORE.FIGARO and copy LOG.
                  CALL DAT_FIND(  TLOC(2), 'LOG', TLOC(3), STATUS )
                  CALL DAT_NEW(   RLOC, 'MORE', 'EXT', 0, 0, STATUS )
                  CALL DAT_FIND(  RLOC, 'MORE', TLOC(4), STATUS )
                  CALL DAT_NEW(   TLOC(4), 'FIGARO', 'FIGARO_EXT',
     :               0, 0, STATUS )
                  CALL DAT_FIND(  TLOC(4), 'FIGARO', TLOC(5), STATUS )
                  CALL DAT_COPY(  TLOC(3), TLOC(5), 'LOG', STATUS )
                  CALL DAT_ANNUL( TLOC(5), STATUS )
                  CALL DAT_ANNUL( TLOC(4), STATUS )
                  CALL DAT_ANNUL( TLOC(3), STATUS )

               END IF
               CALL DAT_ANNUL( TLOC(2), STATUS )
            END IF
            CALL DAT_ANNUL( TLOC(1), STATUS )
         END IF

      END IF


*  Array preliminaria.
*  ===================

*  Check for and locate N-D model centre array.
      CALL DSA1_AXISND( MODSLT, MODAXS,
     :   AXISND, ARYLOC, DELLOC, DELNAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check for and locate N-D model width array.
      IF ( AXISND ) THEN
         CALL DAT_NAME( ARYLOC, NAME, STATUS )
         IF ( NAME .EQ. 'DATA' ) THEN
            CALL DAT_PAREN( ARYLOC,  TLOC(2), STATUS )
            CALL DAT_PAREN( TLOC(2), TLOC(1), STATUS )
            CALL DAT_ANNUL( TLOC(2), STATUS )
         ELSE
            CALL DAT_PAREN( ARYLOC,  TLOC(1), STATUS )
         END IF
         CALL DAT_THERE( TLOC(1), 'WIDTH', WTHERE, STATUS )
         IF ( WTHERE ) THEN
            CALL CMP_STRUC( TLOC(1), 'WIDTH', THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND(  TLOC(1), 'WIDTH', TLOC(2), STATUS )
               CALL DAT_FIND(  TLOC(2), 'DATA', WIDLOC, STATUS )
               CALL DAT_ANNUL( TLOC(2), STATUS )
            ELSE
               CALL DAT_FIND( TLOC(1), 'WIDTH', WIDLOC, STATUS )
            END IF
         END IF
         CALL DAT_ANNUL( TLOC(1),  STATUS )
      ELSE
         CALL NDF_ASTAT( DSA__REFID1(MODSLT), 'WIDTH', MODAXS,
     :      WTHERE, STATUS )
         WIDLOC = DAT__NOLOC
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500


*  Copy centre array.
*  ==================

*  Find out what the target type is.
      IF ( AXISND ) THEN
         CALL DAT_TYPE( ARYLOC, NDFTYP, STATUS )
      ELSE
         CALL NDF_ATYPE( DSA__REFID1(MODSLT), 'CENTRE',
     :      MODAXS, NDFTYP, STATUS )
      END IF


*  If 1-D to 1-D.
*  --------------

      IF ( .NOT. AXISND .AND. NDIM .EQ. 1 ) THEN

*     Change target's type via NDF.
         CALL NDF_ASTYP( NDFTYP, DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      STATUS )

*     If model and target differ.
         IF ( .NOT. SAME ) THEN

*        Map both via NDF.
            CALL NDF_CLONE( DSA__REFID1(MODSLT), MODNDF, STATUS )
            CALL NDF_AMAP( MODNDF, 'CENTRE',
     :         MODAXS, NDFTYP, 'READ', PNTR1, NELM1, STATUS )
            CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE',
     :         AXIS, NDFTYP, 'WRITE', PNTR2, NELM2, STATUS )

*        Copy as many elements as the smaller array holds.
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )

*        Unmap both.
            CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )
            CALL NDF_AUNMP( MODNDF, 'CENTRE', MODAXS, STATUS )
            CALL NDF_ANNUL( MODNDF, STATUS )

         END IF


*  Else if N-D to N-D.
*  -------------------

      ELSE IF ( AXISND .AND. NDIM .GT. 1 ) THEN

*     If model and target are the same.
         IF ( SAME ) THEN

*        Get array name and parent locator. Rename the array.
            CALL DAT_NAME(  ARYLOC, ARYNAM, STATUS )
            CALL DAT_PAREN( ARYLOC, PARLOC, STATUS )
            CALL DAT_RENAM( ARYLOC, TMPNAM, STATUS )

*        Create an array with the desired type and shape.
            CALL DAT_NEW(  PARLOC, ARYNAM, NDFTYP, NDIM, DIMS, STATUS )
            CALL DAT_FIND( PARLOC, ARYNAM, TMPLOC, STATUS )

*        Map the old and new array via HDS.
            CALL DAT_MAPV( ARYLOC, NDFTYP, 'READ',
     :         PNTR1, NELM1, STATUS )
            CALL DAT_MAPV( TMPLOC, NDFTYP, 'WRITE',
     :         PNTR2, NELM2, STATUS )

*        Copy as many elements as the smaller array holds.
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )

*        Unmap both. Remove the old array.
            CALL DAT_UNMAP( TMPLOC, STATUS )
            CALL DAT_UNMAP( ARYLOC, STATUS )
            CALL DAT_ANNUL( TMPLOC, STATUS )
            CALL DAT_ANNUL( ARYLOC, STATUS )
            CALL DAT_ERASE( PARLOC, TMPNAM, STATUS )
            CALL DAT_ANNUL( PARLOC, STATUS )
            CALL DAT_ANNUL( DELLOC, STATUS )

*     Else (model and target differ).
         ELSE

*        Create target .MORE.FIGARO.DATA_ARRAY.DATA and locate it.
            CALL DAT_THERE(  RLOC, 'MORE', THERE, STATUS )
            IF ( .NOT. THERE )
     :         CALL DAT_NEW( RLOC, 'MORE', 'EXT', 0, 0, STATUS )
            CALL DAT_FIND(   RLOC, 'MORE', TLOC(1), STATUS )
            CALL DAT_THERE( TLOC(1), 'FIGARO', THERE, STATUS )
            IF ( .NOT. THERE ) CALL DAT_NEW( TLOC(1), 'FIGARO',
     :         'FIGARO_EXT', 0, 0, STATUS )
            CALL DAT_FIND(  TLOC(1), 'FIGARO', TLOC(2), STATUS )
            CALL DAT_NEW(   TLOC(2), 'DATA_ARRAY', 'ARRAY',
     :         0, 0, STATUS )
            CALL DAT_FIND(  TLOC(2), 'DATA_ARRAY', PARLOC, STATUS )
            CALL DAT_NEW(   PARLOC,  'DATA', NDFTYP,
     :         NDIM, DIMS, STATUS )
            CALL DAT_FIND(  PARLOC,  'DATA', TMPLOC, STATUS )
            CALL DAT_ANNUL( PARLOC,  STATUS )
            CALL DAT_ANNUL( TLOC(2), STATUS )
            CALL DAT_ANNUL( TLOC(1), STATUS )

*        Map model and target via HDS.
            CALL DAT_MAPV( ARYLOC, NDFTYP, 'READ',
     :         PNTR1, NELM1, STATUS )
            CALL DAT_MAPV( TMPLOC, NDFTYP, 'WRITE',
     :         PNTR2, NELM2, STATUS )

*        Copy as many elements as the smaller array holds.
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )

*        Unmap both.
            CALL DAT_UNMAP( TMPLOC, STATUS )
            CALL DAT_UNMAP( ARYLOC, STATUS )
            CALL DAT_ANNUL( TMPLOC, STATUS )
            CALL DAT_ANNUL( ARYLOC, STATUS )
            CALL DAT_ANNUL( DELLOC, STATUS )

*        Initialise compulsory target 1-D centres.
            CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :         LBND, UBND, ODIM, STATUS )
            CALL NDF_ASTYP( '_REAL', DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :         STATUS )
            CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, '_REAL',
     :         'WRITE', PNTR1, NELM1, STATUS )
            CALL DSA2_AFILLF( NELM1, LBND(AXIS),
     :                        %VAL( CNF_PVAL(PNTR1) ), STATUS )
            CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )

         END IF


*  Else if 1-D to N-D.
*  -------------------

      ELSE IF ( .NOT. AXISND .AND. NDIM .GT. 1 ) THEN

*     Create target .MORE.FIGARO.DATA_ARRAY.DATA and locate it.
         CALL DAT_THERE(  RLOC, 'MORE', THERE, STATUS )
         IF ( .NOT. THERE )
     :      CALL DAT_NEW( RLOC, 'MORE', 'EXT', 0, 0, STATUS )
         CALL DAT_FIND(   RLOC, 'MORE', TLOC(1), STATUS )
         CALL DAT_THERE(  TLOC(1), 'FIGARO', THERE, STATUS )
         IF ( .NOT. THERE )
     :      CALL DAT_NEW( TLOC(1), 'FIGARO', 'FIGARO_EXT', 0,0, STATUS )
         CALL DAT_FIND(   TLOC(1), 'FIGARO', TLOC(2), STATUS )
         CALL DAT_NEW(    TLOC(2), 'DATA_ARRAY', 'ARRAY', 0, 0, STATUS )
         CALL DAT_FIND(   TLOC(2), 'DATA_ARRAY', PARLOC, STATUS )
         CALL DAT_NEW(    PARLOC,  'DATA', NDFTYP, NDIM, DIMS, STATUS )
         CALL DAT_FIND(   PARLOC,  'DATA', ARYLOC, STATUS )
         CALL DAT_ANNUL(  PARLOC,  STATUS )
         CALL DAT_ANNUL(  TLOC(2), STATUS )
         CALL DAT_ANNUL(  TLOC(1), STATUS )

*     Map the old array via NDF, the new array via HDS.
         CALL NDF_CLONE( DSA__REFID1(MODSLT), MODNDF, STATUS )
         CALL NDF_AMAP( MODNDF, 'CENTRE', MODAXS,
     :      NDFTYP, 'READ', PNTR1, NELM1, STATUS )
         CALL DAT_MAPV( ARYLOC, NDFTYP, 'WRITE', PNTR2, NELM2, STATUS )

*     Copy as many elements as the smaller array holds.
         IF ( NELM2 .GT. NELM1 )
     :      CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                       STATUS )
         NELM1 = MIN( NELM1, NELM2 )
         CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                    %VAL( CNF_PVAL(PNTR1) ),
     :                    %VAL( CNF_PVAL(PNTR2) ), STATUS )

*     Unmap both.
         CALL DAT_UNMAP( ARYLOC, STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL NDF_AUNMP( MODNDF, 'CENTRE', MODAXS, STATUS )
         CALL NDF_ANNUL( MODNDF, STATUS )

*     Initialise compulsory target 1-D centres.
         CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :      LBND, UBND, ODIM, STATUS )
         CALL NDF_ASTYP( '_REAL', DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      STATUS )
         CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, '_REAL',
     :      'WRITE', PNTR1, NELM1, STATUS )
         CALL DSA2_AFILLF( NELM1, LBND(AXIS), %VAL( CNF_PVAL(PNTR1) ),
     :                     STATUS )
         CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )


*  Else if N-D to 1-D.
*  -------------------

      ELSE IF ( AXISND .AND. NDIM .EQ. 1 ) THEN

*     Change target type via NDF.
         CALL NDF_ASTYP( NDFTYP, DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      STATUS )

*     Map old array via HDS, target via NDF.
         CALL DAT_MAPV( ARYLOC, NDFTYP, 'READ', PNTR1, NELM1, STATUS )
         CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, NDFTYP,
     :      'WRITE', PNTR2, NELM2, STATUS )

*     Copy as many elements as the smaller array holds.
         IF ( NELM2 .GT. NELM1 )
     :      CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                       STATUS )
         NELM1 = MIN( NELM1, NELM2 )
         CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                    %VAL( CNF_PVAL(PNTR1) ),
     :                    %VAL( CNF_PVAL(PNTR2) ), STATUS )

*     Unmap both.
         CALL DAT_UNMAP( ARYLOC, STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )

*     If model and target are the same.
*        Remove model array's highest otherwise empty parent.
         IF ( SAME ) CALL DAT_ERASE( DELLOC, DELNAM, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )

      END IF

*  Indicate that the axis centres have been changed.
      IF ( .NOT. SAME ) DSA__REFMDC( AXIS, SLOT ) = .TRUE.

*  Copy width array.
*  =================

*  If model different from target, reset 1-D width, any N-D width would
*  have been removed with .MORE above.
      IF ( .NOT. SAME )
     :   CALL NDF_AREST( DSA__REFID1(SLOT), 'WIDTH', AXIS, STATUS )

*  If model has width component.
      IF ( WTHERE ) THEN

*     Find out what the target type is.
         IF ( AXISND ) THEN
            CALL DAT_TYPE( WIDLOC, NDFTYP, STATUS )
         ELSE
            CALL NDF_ATYPE( DSA__REFID1(MODSLT), 'WIDTH',
     :         MODAXS, NDFTYP, STATUS )
         END IF


*     If 1-D to 1-D.
*     --------------

         IF ( .NOT. AXISND .AND. NDIM .EQ. 1 ) THEN

*        Change target's type via NDF.
            CALL NDF_ASTYP( NDFTYP, DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         STATUS )

*        If model and target differ.
            IF ( .NOT. SAME ) THEN

*           Map both via NDF.
               CALL NDF_CLONE( DSA__REFID1(MODSLT), MODNDF, STATUS )
               CALL NDF_AMAP( MODNDF, 'WIDTH',
     :            MODAXS, NDFTYP, 'READ', PNTR1, NELM1, STATUS )
               CALL NDF_AMAP( DSA__REFID1(SLOT), 'WIDTH',
     :            AXIS, NDFTYP, 'WRITE', PNTR2, NELM2, STATUS )

*           Copy as many elements as the smaller array holds.
               IF ( NELM2 .GT. NELM1 )
     :            CALL DSA1_BFILL( NDFTYP, NELM2,
     :                             %VAL( CNF_PVAL(PNTR2) ), STATUS )
               NELM1 = MIN( NELM1, NELM2 )
               CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                          %VAL( CNF_PVAL(PNTR1) ),
     :                          %VAL( CNF_PVAL(PNTR2) ), STATUS )

*           Unmap both.
               CALL NDF_AUNMP( DSA__REFID1(SLOT),'WIDTH', AXIS, STATUS )
               CALL NDF_AUNMP( MODNDF, 'WIDTH', MODAXS, STATUS )
               CALL NDF_ANNUL( MODNDF, STATUS )

            END IF


*     Else if N-D to N-D.
*     -------------------

         ELSE IF ( AXISND .AND. NDIM .GT. 1 ) THEN

*        If model and target are the same.
            IF ( SAME ) THEN

*           Get array name and parent locator. Rename the array.
               CALL DAT_NAME(  WIDLOC, ARYNAM, STATUS )
               CALL DAT_PAREN( WIDLOC, PARLOC, STATUS )
               CALL DAT_RENAM( WIDLOC, TMPNAM, STATUS )

*           Create an array with the desired type and shape.
               CALL DAT_NEW(  PARLOC, ARYNAM, NDFTYP, NDIM, DIMS,
     :            STATUS )
               CALL DAT_FIND( PARLOC, ARYNAM, TMPLOC, STATUS )

*           Map the old and new array via HDS.
               CALL DAT_MAPV( WIDLOC, NDFTYP, 'READ',
     :            PNTR1, NELM1, STATUS )
               CALL DAT_MAPV( TMPLOC, NDFTYP, 'WRITE',
     :            PNTR2, NELM2, STATUS )

*           Copy as many elements as the smaller array holds.
               IF ( NELM2 .GT. NELM1 )
     :            CALL DSA1_BFILL( NDFTYP, NELM2,
     :                             %VAL( CNF_PVAL(PNTR2) ), STATUS )
               NELM1 = MIN( NELM1, NELM2 )
               CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                          %VAL( CNF_PVAL(PNTR1) ),
     :                          %VAL( CNF_PVAL(PNTR2) ), STATUS )

*           Unmap both. Remove the old array.
               CALL DAT_UNMAP( TMPLOC, STATUS )
               CALL DAT_UNMAP( WIDLOC, STATUS )
               CALL DAT_ANNUL( TMPLOC, STATUS )
               CALL DAT_ANNUL( WIDLOC, STATUS )
               CALL DAT_ERASE( PARLOC, TMPNAM, STATUS )
               CALL DAT_ANNUL( PARLOC, STATUS )

*        Else (model and target differ).
            ELSE

*           Create target .MORE.FIGARO.WIDTH.DATA and locate it.
               CALL DAT_FIND(   RLOC, 'MORE', TLOC(1), STATUS )
               CALL DAT_FIND(  TLOC(1), 'FIGARO', TLOC(2), STATUS )
               CALL DAT_NEW(   TLOC(2), 'WIDTH', 'ARRAY', 0, 0, STATUS )
               CALL DAT_FIND(  TLOC(2), 'WIDTH', PARLOC, STATUS )
               CALL DAT_NEW(   PARLOC,  'DATA', NDFTYP,
     :            NDIM, DIMS, STATUS )
               CALL DAT_FIND(  PARLOC,  'DATA', TMPLOC, STATUS )
               CALL DAT_ANNUL( PARLOC,  STATUS )
               CALL DAT_ANNUL( TLOC(2), STATUS )
               CALL DAT_ANNUL( TLOC(1), STATUS )

*           Map model and target via HDS.
               CALL DAT_MAPV( WIDLOC, NDFTYP, 'READ',
     :            PNTR1, NELM1, STATUS )
               CALL DAT_MAPV( TMPLOC, NDFTYP, 'WRITE',
     :            PNTR2, NELM2, STATUS )

*           Copy as many elements as the smaller array holds.
               IF ( NELM2 .GT. NELM1 )
     :            CALL DSA1_BFILL( NDFTYP, NELM2,
     :                             %VAL( CNF_PVAL(PNTR2) ), STATUS )
               NELM1 = MIN( NELM1, NELM2 )
               CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                          %VAL( CNF_PVAL(PNTR1) ),
     :                          %VAL( CNF_PVAL(PNTR2) ), STATUS )

*           Unmap both.
               CALL DAT_UNMAP( TMPLOC, STATUS )
               CALL DAT_UNMAP( WIDLOC, STATUS )
               CALL DAT_ANNUL( TMPLOC, STATUS )
               CALL DAT_ANNUL( WIDLOC, STATUS )

            END IF


*     Else if 1-D to N-D.
*     -------------------

         ELSE IF ( .NOT. AXISND .AND. NDIM .GT. 1 ) THEN

*        Create target .MORE.FIGARO.WIDTH.DATA and locate it.
            CALL DAT_FIND(  RLOC, 'MORE', TLOC(1), STATUS )
            CALL DAT_FIND(  TLOC(1), 'FIGARO', TLOC(2), STATUS )
            CALL DAT_NEW(   TLOC(2), 'WIDTH', 'ARRAY', 0, 0, STATUS )
            CALL DAT_FIND(  TLOC(2), 'WIDTH', PARLOC, STATUS )
            CALL DAT_NEW(   PARLOC, 'DATA', NDFTYP, NDIM, DIMS, STATUS )
            CALL DAT_FIND(  PARLOC, 'DATA', WIDLOC, STATUS )
            CALL DAT_ANNUL( PARLOC,  STATUS )
            CALL DAT_ANNUL( TLOC(2), STATUS )
            CALL DAT_ANNUL( TLOC(1), STATUS )

*        Map the old array via NDF, the new array via HDS.
            CALL NDF_CLONE( DSA__REFID1(MODSLT), MODNDF, STATUS )
            CALL NDF_AMAP( MODNDF, 'WIDTH', MODAXS,
     :         NDFTYP, 'READ', PNTR1, NELM1, STATUS )
            CALL DAT_MAPV( WIDLOC, NDFTYP, 'WRITE',
     :         PNTR2, NELM2, STATUS )

*        Copy as many elements as the smaller array holds.
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )

*        Unmap both.
            CALL DAT_UNMAP( WIDLOC, STATUS )
            CALL DAT_ANNUL( WIDLOC, STATUS )
            CALL NDF_AUNMP( MODNDF, 'WIDTH', MODAXS, STATUS )
            CALL NDF_ANNUL( MODNDF, STATUS )

*        If model and target are the same, reset width component of
*        target NDF.
            IF ( SAME ) CALL NDF_AREST( DSA__REFID1(SLOT),
     :         'WIDTH', AXIS, STATUS )


*     Else if N-D to 1-D.
*     -------------------

         ELSE IF ( AXISND .AND. NDIM .EQ. 1 ) THEN

*        Change target type via NDF.
*        Map old array via HDS, target via NDF.
*        Copy as many elements as the smaller array holds.
*        Unmap both.

*        Change target type via NDF.
            CALL NDF_ASTYP( NDFTYP, DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         STATUS )

*        Map old array via HDS, target via NDF.
            CALL DAT_MAPV( WIDLOC, NDFTYP, 'READ',
     :         PNTR1, NELM1, STATUS )
            CALL NDF_AMAP( DSA__REFID1(SLOT), 'WIDTH', AXIS, NDFTYP,
     :         'WRITE', PNTR2, NELM2, STATUS )

*        Copy as many elements as the smaller array holds.
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTYP, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )

*        Unmap both.
            CALL DAT_UNMAP( WIDLOC, STATUS )
            CALL NDF_AUNMP( DSA__REFID1(SLOT), 'WIDTH', AXIS, STATUS )

*        If model and target are the same, remove model width.
            IF ( SAME ) THEN
               CALL DAT_NAME( WIDLOC, NAME, STATUS )
               IF ( NAME .EQ. 'DATA' ) THEN
                  CALL DAT_PAREN( WIDLOC,  TLOC(2), STATUS )
                  CALL DAT_PAREN( TLOC(2), TLOC(1), STATUS )
                  CALL DAT_ANNUL( TLOC(2), STATUS )
               ELSE
                  CALL DAT_PAREN( WIDLOC,  TLOC(1), STATUS )
               END IF
               CALL DAT_ANNUL( WIDLOC, STATUS )
               CALL DAT_ERASE( TLOC(1), 'WIDTH', STATUS )
            ELSE
               CALL DAT_ANNUL( WIDLOC, STATUS )
            END IF

         END IF

*  Indicate that the axis widths have been changed.
         IF ( .NOT. SAME ) DSA__REFMDW( AXIS, SLOT ) = .TRUE.

      END IF


*  Release model and target cells.
*  ===============================

      CALL DAT_ANNUL( MLOC, STATUS )
      CALL DAT_ANNUL( RLOC, STATUS )

*  Flag the slot/axis as re-shaped.
      DSA__REFRSA( AXIS, SLOT ) = .TRUE.

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
