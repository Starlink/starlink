      SUBROUTINE DSA_COERCE_AXIS_DATA( DSAREF, AXIS, TYPE,
     :   NDIM, DIMS, STATUS )
*+
*  Name:
*     DSA_COERCE_AXIS_DATA

*  Purpose:
*     Force the existence of an axis centre array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_COERCE_AXIS_DATA( DSAREF, AXIS, TYPE,
*        NDIM, DIMS, STATUS )

*  Description:
*     This routine ensures that a certain axis has a centre array of
*     the specified data type and shape. The values in the array are
*     kept, but may be meaningless due to the change in shape.
*
*     This routine changes the shape of any existing width component,
*     which in earlier implementations was omitted.
*
*     Contrary to earlier implementations, this routine refuses to
*     create an axis whose shape is incompatible with the main data
*     shape at the time. Therefore the main data have to be given a new
*     shape first, and the axes have to be given appropriate shapes
*     after that.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis to be reshaped.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type that the data array is to have. This must be one of
*        the primitive types recognised by the DSA_ routines. (Usually,
*        this will be 'FLOAT' or 'DOUBLE').
*     NDIM = INTEGER (Given)
*        The number of dimensions the array is to have.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions the array is to have.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     04 Aug 1987 (ks):
*        Original version.
*     04 Oct 1989 (ks):
*        Reworked to become little more than a call to DSA_COERCE_ARRAY.
*        Comments modified to indicate that data is only lost if type
*        changes.
*     08 Mar 1990 (ks):
*        Now uses DSA__ routines rather than assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     15 Oct 1992 (hme):
*        Refuse creation of an N-D axis in an NDF.
*     13 Feb 1995 (ks):
*        N-D NDF axis arrays allowed again. These are now handled at
*        file close time by DSA_CHECK_NDF_AXIS, which will move them to
*        the axis extension.
*     30 Jan 1996 (hme):
*        FDA library.
*     07 Feb 1996 (hme):
*        When copying data, fill target with bad values prior to copy.
*     12 Feb 1996 (hme):
*        Mark reference/axis as axis-reshaped.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     20 Feb 1996 (hme):
*        Handle width if present.
*     1996 July 9 (MJC):
*        Calls DSA2_ACRE for NDF_ACRE and DSA2_AFILLF for DSA2_NFILLF
*        to allow default axis co-ordinates to be pixel indices.
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
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIMS( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER * ( DAT__SZNAM ) TMPNAM ! Temporary name for old array
      PARAMETER ( TMPNAM = 'OLDDATA' )

*  Local Variables:
      LOGICAL MATCH              ! True if axis dimensions match data
      LOGICAL FLAG( NDF__MXDIM ) ! Flags dimensions in data
      LOGICAL AXISND             ! True if N-D centre array
      LOGICAL THERE              ! Whether HDS component exists
      LOGICAL WTHERE             ! Whether width array exists
      INTEGER I                  ! Loop index through axis dimensions
      INTEGER J                  ! Loop index through data dimensions
      INTEGER SLOT               ! The reference slot
      INTEGER MNDIM              ! Main data dimensionality
      INTEGER MDIM( NDF__MXDIM ) ! Main data dimensions
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER NELM1, NELM2       ! Array sizes
      INTEGER PNTR1, PNTR2       ! Array pointers
      CHARACTER * ( NDF__SZTYP ) NDFTY1 ! The type in NDF speak
      CHARACTER * ( NDF__SZTYP ) NDFTY2 ! The type in NDF speak
      CHARACTER * ( DAT__SZLOC ) WIDLOC ! Locator to old width
      CHARACTER * ( DAT__SZLOC ) ARYLOC ! Locator to old array
      CHARACTER * ( DAT__SZLOC ) TMPLOC ! Locator to new array
      CHARACTER * ( DAT__SZLOC ) DELLOC ! Locator for deletion
      CHARACTER * ( DAT__SZLOC ) PARLOC ! Parent locator
      CHARACTER * ( DAT__SZNAM ) ARYNAM ! Array component name
      CHARACTER * ( DAT__SZNAM ) DELNAM ! Component name for deletion
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      CHARACTER * ( DAT__SZLOC ) TLOC( 5 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK


*  Preliminaria.
*  =============

*  Look up reference slot and convert type specification.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_NDFTYP( TYPE, NDFTY2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get main NDF dimensions.
      CALL NDF_DIM( DSA__REFID1(SLOT), NDF__MXDIM,
     :   MDIM, MNDIM, STATUS )

*  Check axis number validity.
      IF ( AXIS .LT. 1 .OR. AXIS .GT. MNDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T002', AXIS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E009', 'DSA_COERCE_AXIS: Invalid axis ' //
     :      'number ^FDA_T002 for reference ^FDA_T001.', STATUS )
         GO TO 500
      END IF

*  Check shape validity (consistency with main data shape).
      MATCH = NDIM .LE. MNDIM .AND. DIMS(1) .EQ. MDIM(AXIS)
      IF ( MATCH ) THEN
         DO 1 J = 1, MNDIM
            FLAG(J)=.FALSE.
 1       CONTINUE
         FLAG(AXIS)=.TRUE.
         DO 4 I = 2, NDIM
            MATCH = .FALSE.
            DO 2 J = 1, MNDIM
               IF ( DIMS(I) .EQ. MDIM(J) ) THEN
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
         CALL MSG_SETI( 'FDA_T002', AXIS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E010', 'DSA_COERCE_AXIS: The requested ' //
     :      'shape for axis number ^FDA_T002 in the reference ' //
     :      '^FDA_T001 is incompatible with the shape of the main ' //
     :      'data array.', STATUS )
         GO TO 500
      END IF

*  Check for and locate N-D centre array.
      CALL DSA1_AXISND( SLOT, AXIS,
     :   AXISND, ARYLOC, DELLOC, DELNAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check for and locate N-D width array.
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
         CALL NDF_ASTAT( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :      WTHERE, STATUS )
         WIDLOC = DAT__NOLOC
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500


*  If 1-D -> 1-D.
*  ==============

      IF ( .NOT. AXISND .AND. NDIM .EQ. 1 ) THEN

*     Ensure the NDF has an axis component.
         CALL DSA2_ACRE( DSA__REFID1(SLOT), STATUS )

*     Change type via NDF. (Shape is fine at all times => NDF.)
*     Values are preserved in this operation.
         CALL NDF_ASTYP( NDFTY2, DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      STATUS )


*  Else if N-D -> N-D.
*  ===================

      ELSE IF ( AXISND .AND. NDIM .GT. 1 ) THEN

*     Get array name and parent locator. Rename the array.
         CALL DAT_NAME(  ARYLOC, ARYNAM, STATUS )
         CALL DAT_PAREN( ARYLOC, PARLOC, STATUS )
         CALL DAT_RENAM( ARYLOC, TMPNAM, STATUS )

*     Create an array with the desired type and shape.
         CALL DAT_NEW(  PARLOC, ARYNAM, NDFTY2, NDIM, DIMS, STATUS )
         CALL DAT_FIND( PARLOC, ARYNAM, TMPLOC, STATUS )

*     Map the old and new array.
         CALL DAT_TYPE( ARYLOC, NDFTY1, STATUS )
         CALL DAT_MAPV( ARYLOC, NDFTY1, 'READ',  PNTR1, NELM1, STATUS )
         CALL DAT_MAPV( TMPLOC, NDFTY2, 'WRITE', PNTR2, NELM2, STATUS )

*     Copy as many elements as the smaller array holds.
         IF ( NELM2 .GT. NELM1 )
     :      CALL DSA1_BFILL( NDFTY2, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                       STATUS )
         NELM1 = MIN( NELM1, NELM2 )
         CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM1,
     :      %VAL( CNF_PVAL(PNTR1) ), %VAL( CNF_PVAL(PNTR2) ), STATUS )

*     Unmap both arrays. Remove the old array.
         CALL DAT_UNMAP( TMPLOC, STATUS )
         CALL DAT_UNMAP( ARYLOC, STATUS )
         CALL DAT_ANNUL( TMPLOC, STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL DAT_ERASE( PARLOC, TMPNAM, STATUS )
         CALL DAT_ANNUL( PARLOC, STATUS )

*     Now the same for the width, if it exists.
*     Here, we use the old width type, not the given type.
         IF ( WTHERE ) THEN
            CALL DAT_NAME(  WIDLOC, ARYNAM, STATUS )
            CALL DAT_PAREN( WIDLOC, PARLOC, STATUS )
            CALL DAT_RENAM( WIDLOC, TMPNAM, STATUS )
            CALL DAT_TYPE(  WIDLOC, NDFTY1, STATUS )
            CALL DAT_NEW(  PARLOC, ARYNAM, NDFTY1, NDIM, DIMS, STATUS )
            CALL DAT_FIND( PARLOC, ARYNAM, TMPLOC, STATUS )
            CALL DAT_MAPV( WIDLOC, NDFTY1, 'READ',
     :         PNTR1, NELM1, STATUS )
            CALL DAT_MAPV( TMPLOC, NDFTY1, 'WRITE',
     :         PNTR2, NELM2, STATUS )
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTY1, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTY1, NDFTY1, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )
            CALL DAT_UNMAP( TMPLOC, STATUS )
            CALL DAT_UNMAP( WIDLOC, STATUS )
            CALL DAT_ANNUL( TMPLOC, STATUS )
            CALL DAT_ANNUL( WIDLOC, STATUS )
            CALL DAT_ERASE( PARLOC, TMPNAM, STATUS )
            CALL DAT_ANNUL( PARLOC, STATUS )
         END IF

*     Annull the remaining locator from DSA1_AXISND.
         CALL DAT_ANNUL( DELLOC, STATUS )


*  Else if 1-D -> N-D.
*  ===================

      ELSE IF ( .NOT. AXISND .AND. NDIM .GT. 1 ) THEN

*     Ensure the NDF has an axis component.
         CALL DSA2_ACRE( DSA__REFID1(SLOT), STATUS )

*     Create .MORE.FIGARO.DATA_ARRAY.DATA and locate it.
*     Create .MORE.FIGARO.WIDTH.DATA and locate it.
         CALL NDF_LOC( DSA__REFID1(SLOT), 'READ', TLOC(1), STATUS )
         CALL DAT_FIND(   TLOC(1), 'AXIS',   TLOC(2), STATUS )
         CALL DAT_CELL(   TLOC(2), 1, AXIS,  TLOC(3), STATUS )
         CALL DAT_THERE(  TLOC(3), 'MORE',   THERE,   STATUS )
         IF ( .NOT. THERE )
     :      CALL DAT_NEW( TLOC(3), 'MORE', 'EXT', 0, 0, STATUS )
         CALL DAT_FIND(   TLOC(3), 'MORE',   TLOC(4), STATUS )
         CALL DAT_THERE(  TLOC(4), 'FIGARO', THERE,   STATUS )
         IF ( .NOT. THERE )
     :      CALL DAT_NEW( TLOC(4), 'FIGARO', 'FIGARO_EXT', 0,0, STATUS )
         CALL DAT_FIND(   TLOC(4), 'FIGARO', TLOC(5), STATUS )
         CALL DAT_NEW(    TLOC(5), 'DATA_ARRAY', 'ARRAY', 0, 0, STATUS )
         CALL DAT_FIND(   TLOC(5), 'DATA_ARRAY', PARLOC, STATUS )
         CALL DAT_NEW(    PARLOC,  'DATA', NDFTY2, NDIM, DIMS, STATUS )
         CALL DAT_FIND(   PARLOC,  'DATA', ARYLOC, STATUS )
         CALL DAT_ANNUL(  PARLOC,  STATUS )
         IF ( WTHERE ) THEN
            CALL NDF_ATYPE( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         NDFTY1, STATUS )
            CALL DAT_NEW(  TLOC(5), 'WIDTH', 'ARRAY', 0, 0, STATUS )
            CALL DAT_FIND( TLOC(5), 'WIDTH', PARLOC, STATUS )
            CALL DAT_NEW(  PARLOC,  'DATA', NDFTY1, NDIM, DIMS, STATUS )
            CALL DAT_FIND( PARLOC,  'DATA', WIDLOC, STATUS )
            CALL DAT_ANNUL( PARLOC,  STATUS )
         END IF
         CALL DAT_ANNUL( TLOC(5), STATUS )
         CALL DAT_ANNUL( TLOC(4), STATUS )
         CALL DAT_ANNUL( TLOC(3), STATUS )
         CALL DAT_ANNUL( TLOC(2), STATUS )
         CALL DAT_ANNUL( TLOC(1), STATUS )

*     Map the old array via NDF. Map the new array.
         CALL NDF_ATYPE( DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      NDFTY1, STATUS )
         CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      NDFTY1, 'READ', PNTR1, NELM1, STATUS )
         CALL DAT_MAPV( ARYLOC, NDFTY2, 'WRITE',  PNTR2, NELM2, STATUS )

*     Copy as many elements as the smaller array holds.
         IF ( NELM2 .GT. NELM1 )
     :      CALL DSA1_BFILL( NDFTY2, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                       STATUS )
         NELM1 = MIN( NELM1, NELM2 )
         CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM1,
     :      %VAL( CNF_PVAL(PNTR1) ), %VAL( CNF_PVAL(PNTR2) ), STATUS )

*     Unmap both arrays.
         CALL DAT_UNMAP( ARYLOC, STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )

*     Initialise the compulsory 1-D centres.
         CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :      LBND, UBND, MNDIM, STATUS )
         CALL NDF_ASTYP( '_REAL', DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      STATUS )
         CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, '_REAL',
     :      'WRITE', PNTR1, NELM1, STATUS )
         CALL DSA2_AFILLF( NELM1, LBND(AXIS), %VAL( CNF_PVAL(PNTR1) ),
     :                     STATUS )
         CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )

*     Map, copy, unmap for width.
*     Reset width component.
         IF ( WTHERE ) THEN
            CALL NDF_ATYPE( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         NDFTY1, STATUS )
            CALL NDF_AMAP( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         NDFTY1, 'READ', PNTR1, NELM1, STATUS )
            CALL DAT_MAPV( WIDLOC, NDFTY1, 'WRITE',
     :         PNTR2, NELM2, STATUS )
            IF ( NELM2 .GT. NELM1 )
     :         CALL DSA1_BFILL( NDFTY1, NELM2, %VAL( CNF_PVAL(PNTR2) ),
     :                          STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTY1, NDFTY1, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )
            CALL DAT_UNMAP( WIDLOC, STATUS )
            CALL DAT_ANNUL( WIDLOC, STATUS )
            CALL NDF_AUNMP( DSA__REFID1(SLOT), 'WIDTH', AXIS, STATUS )
            CALL NDF_AREST( DSA__REFID1(SLOT), 'WIDTH', AXIS, STATUS )
         END IF


*  Else (N-D -> 1-D).
*  ==================

      ELSE IF ( AXISND .AND. NDIM .EQ. 1 ) THEN

*     Change type via NDF. (Shape if fine at all times => NDF.)
         CALL NDF_ASTYP( NDFTY2, DSA__REFID1(SLOT), 'CENTRE', AXIS,
     :      STATUS )

*     Map the old array. Map the NDF centre component.
         CALL DAT_TYPE( ARYLOC, NDFTY1, STATUS )
         CALL DAT_MAPV( ARYLOC, NDFTY1, 'READ',  PNTR1, NELM1, STATUS )
         CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, NDFTY2,
     :      'WRITE', PNTR2, NELM2, STATUS )

*     Copy as many elements as the smaller array holds.
         NELM1 = MIN( NELM1, NELM2 )
         CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM1,
     :      %VAL( CNF_PVAL(PNTR1) ), %VAL( CNF_PVAL(PNTR2) ), STATUS )

*     Unmap both arrays.
         CALL DAT_UNMAP( ARYLOC, STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL NDF_AUNMP( DSA__REFID1(SLOT), 'CENTRE', AXIS, STATUS )

*     Erase the old array's highest otherwise empty parent.
         CALL DAT_ERASE( DELLOC, DELNAM, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )

*     Map, copy, unmap, erase width.
*     We erase only .MORE.FIGARO.WIDTH and leave .MORE.FIGARO around
*     even if it is now empty.
         IF ( WTHERE ) THEN
            CALL DAT_TYPE( WIDLOC, NDFTY1, STATUS )
            CALL NDF_ASTYP( NDFTY1, DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :         STATUS )
            CALL DAT_MAPV( WIDLOC, NDFTY1, 'READ',
     :         PNTR1, NELM1, STATUS )
            CALL NDF_AMAP( DSA__REFID1(SLOT), 'WIDTH', AXIS, NDFTY1,
     :         'WRITE', PNTR2, NELM2, STATUS )
            NELM1 = MIN( NELM1, NELM2 )
            CALL DSA1_CPDAT( NDFTY1, NDFTY1, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )
            CALL DAT_UNMAP( WIDLOC, STATUS )
            CALL NDF_AUNMP( DSA__REFID1(SLOT), 'WIDTH', AXIS, STATUS )
            CALL DAT_NAME(  WIDLOC, NAME, STATUS )
            IF ( NAME .EQ. 'DATA' ) THEN
               CALL DAT_PAREN( WIDLOC,  TLOC(2), STATUS )
               CALL DAT_PAREN( TLOC(2), TLOC(1), STATUS )
               CALL DAT_ANNUL( TLOC(2), STATUS )
            ELSE
               CALL DAT_PAREN( WIDLOC,  TLOC(1), STATUS )
            END IF
            CALL DAT_ANNUL( WIDLOC, STATUS )
            CALL DAT_ERASE( TLOC(1), 'WIDTH', STATUS )
            CALL DAT_ANNUL( TLOC(1), STATUS )
         END IF

      END IF


*  Flag the slot/axis as re-shaped.
      DSA__REFRSA( AXIS, SLOT ) = .TRUE.

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL DAT_ANNUL( WIDLOC, STATUS )
         CALL DAT_ANNUL( TMPLOC, STATUS )
         CALL DAT_ANNUL( PARLOC, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )
      END IF

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
