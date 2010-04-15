      SUBROUTINE DSA_MAP_AXIS_DATA( DSAREF, AXIS, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_MAP_AXIS_DATA

*  Purpose:
*     Map one of the axis centre arrays in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MAP_AXIS_DATA( DSAREF, AXIS, MODE, TYPE,
*        ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps a specified axis centre array in an NDF.
*
*     Contrary to earlier implementations the default centre values
*     derive from the NDF bounds and are 0.5 less than the pixel index.
*     However, for non-existent 1-dimensional axis centres the old
*     pixel-index co-ordinates may be used if the user has requested it
*     (determined via DSA2_PIXIN).

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     MODE = CHARACTER * ( * ) (Given)
*        One of 'READ','WRITE', or 'UPDATE', indicating the way the data
*        is going to be accessed. Only the first character is
*        significant.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be used to map the array. E.g. 'FLOAT' or
*        'DOUBLE'.
*     ADDRESS = INTEGER (Returned)
*        The memory address of the mapped array.
*     MSLOT = INTEGER (Returned)
*        The map slot, a handle that can be used to unmap this array
*        later.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Status:
*     Access to an N-dimensional array is not possible if the NDF is
*     question is a section instead of a base NDF.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     ACD: A C Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     06 Jul 1987 (ks):
*        Original version.
*     25 Feb 1988 (ks):
*        Bug fix.  If data array is n-D and axis data had to be created,
*        it was being mapped with too many elements.  Also correct one
*        comment.
*     20 Jul 1988 (ks):
*        Modify call to DSA_MAP_ARRAY - quality processing flags set
*        false.
*     08 Sep 1989 (ks):
*        Modify call to DSA_MAP_ARRAY - flagged value propagation flag
*        set false.
*     11 Dec 1989 (ks):
*        Add setting of update flag for write or update mapping.
*     19 Jan 1990 (ks):
*        Use DSA__ routines to get details of data structure.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     26 Oct 1994 (ks):
*        Now uses new calling sequence for DSA_MAP_ARRAY.
*     05 Feb 1996 (hme):
*        FDA library.
*     12 Feb 1996 (hme):
*        For UW access mark reference/axis as centre-modified.
*     19 Feb 1996 (hme):
*        Move the work into DSA1_MAPCEN.
*        Translate between application-side status and Starlink status.
*     1996 July 9 (MJC):
*        Fill a non-existent centre array with pixel indices, if
*        requested.
*     1997 August 26 (ACD):
*        Force the label and units components of the axis structure to
*        exist.
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
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

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

*  Find reference slot and call the work routine.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_MAPCEN( SLOT, AXIS, MODE, TYPE, ADDRESS, MSLOT, STATUS )

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



      SUBROUTINE DSA1_MAPCEN( SLOT, AXIS, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )

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
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL AXISND             ! True if N-D centre array
      LOGICAL EXIST              ! Axis centres exist?
      LOGICAL ISBAS              ! Whether main NDF is base NDF
      LOGICAL PIXIND             ! Fill centres with pixel indices?
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NDIM               ! Number of dimensions in the NDF
      INTEGER NELM               ! Ignored
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      CHARACTER * ( NDF__SZTYP ) NDFTY1 ! The type in NDF speak
      CHARACTER * ( NDF__SZTYP ) NDFTY2 ! The type in NDF speak
      CHARACTER * ( DAT__SZNAM ) DELNAM ! Ignored
      CHARACTER * ( DAT__SZLOC ) DELLOC ! Ignored
      CHARACTER * 64             LABEL  ! Axis label.
      INTEGER LABLEN             ! Length of LABEL (excl. trail. blanks).

*  Internal References:
      LOGICAL CHR_SIMLR          ! Whether two strings are similar

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the data type.
      CALL DSA1_NDFTYP( TYPE, NDFTY2, STATUS )

*  Find map slot.
      CALL DSA1_MSNEW( MSLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check for and locate N-D centre array.
      CALL DSA1_AXISND( SLOT, AXIS,
     :   AXISND, DSA__MAPLO1(MSLOT), DELLOC, DELNAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If N-D centre array exists.
      IF ( AXISND ) THEN

*     If NDF is a section, return with error report.
         CALL NDF_ISBAS( DSA__REFID1(SLOT), ISBAS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         IF ( .NOT. ISBAS ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'FDA_T002', AXIS )
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL ERR_REP( 'FDA_E018', 'DSA_MAPCEN: Cannot access ' //
     :         'an multi-dimensional axis data array if the data ' //
     :         'structure is an NDF section. Failure for axis ' //
     :         'number ^FDA_T002 in reference ^FDA_T001.', STATUS )
            GO TO 500
         END IF

*     Find out stored type of the centre array.
         CALL DAT_TYPE( DSA__MAPLO1(MSLOT), NDFTY1, STATUS )

*     If stored and requested type are the same.
         IF ( CHR_SIMLR( NDFTY1, NDFTY2 ) ) THEN

*        Map the stored array on first locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        First pointer is the returned address.
            ADDRESS = DSA__MAPPT1(MSLOT)

*     Else if read access.
         ELSE IF ( CHR_SIMLR( MODE(:1), 'R' ) ) THEN

*        Map the stored array on first locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        Create temporary vector of requested type on second locator.
            CALL DAT_TEMP( NDFTY2, 1, NELM,
     :         DSA__MAPLO2(MSLOT), STATUS )

*        Map temporary vector on second pointer (=returned address).
            CALL DAT_MAPV( DSA__MAPLO2(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT2(MSLOT), NELM, STATUS )
            ADDRESS = DSA__MAPPT2(MSLOT)

*        Copy data from stored array to temporary vector.
            CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM,
     :                       %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ),
     :                       %VAL( CNF_PVAL(DSA__MAPPT2(MSLOT)) ),
     :                       STATUS )

*        Unmap and annull first locator, zero first pointer.
            CALL DAT_UNMAP( DSA__MAPLO1(MSLOT), STATUS )
            CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
            DSA__MAPPT1(MSLOT) = 0

*     Else if write access.
         ELSE IF ( CHR_SIMLR( MODE(:1), 'W' ) ) THEN

*        Map the stored array on first locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        Create temporary vector of requested type on second locator.
            CALL DAT_TEMP( NDFTY2, 1, NELM,
     :         DSA__MAPLO2(MSLOT), STATUS )

*        Map temporary vector on second pointer (=returned address).
            CALL DAT_MAPV( DSA__MAPLO2(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT2(MSLOT), NELM, STATUS )
            ADDRESS = DSA__MAPPT2(MSLOT)

*     Else (update access).
         ELSE

*        Map the stored array on first locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        Create temporary vector of requested type on second locator.
            CALL DAT_TEMP( NDFTY2, 1, NELM,
     :         DSA__MAPLO2(MSLOT), STATUS )

*        Map temporary vector on second pointer (=returned address).
            CALL DAT_MAPV( DSA__MAPLO2(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT2(MSLOT), NELM, STATUS )
            ADDRESS = DSA__MAPPT2(MSLOT)

*        Copy data from stored array to temporary vector.
            CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM,
     :                       %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ),
     :                       %VAL( CNF_PVAL(DSA__MAPPT2(MSLOT)) ),
     :         STATUS )

         END IF

*     DSA_UNMAP will have to check both locators. If both are valid then
*     it will have to copy back from the second pointer to the first
*     pointer. At any rate it will thereafter have to unmap and annull
*     both locators.

*     Fill in map slot.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DSA__MAPUSD(MSLOT) = .TRUE.
            DSA__MAPAXI(MSLOT) = AXIS
            DSA__MAPREF(MSLOT) = SLOT
            DSA__MAPNAM(MSLOT) = 'NCENTRE'
         END IF

*     Release delible locator.
         CALL DAT_ANNUL( DELLOC, STATUS )

*  Else (axis is 1-D).
      ELSE

*     Determine if the axis centres exist.
         CALL NDF_STATE( DSA__REFID1(SLOT), 'AXIS', EXIST, STATUS )

*     Find which type of default axis centres are required if the
*     axis-centre array does not exist.
         PIXIND = .FALSE.
         IF ( .NOT. EXIST ) CALL DSA2_PIXIN( PIXIND, STATUS )

*     Obtain the NDF bounds.
         CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM, LBND, UBND,
     :      NDIM, STATUS )

*     Use any existing axis-centre array as is.  Also use NDF_AMAP to
*     default to Starlink-standard pixel co-ordinates, but override
*     this for update and write access when pixel-index co-ordinates
*     are required.
         IF ( EXIST .OR. .NOT. PIXIND .OR.
     :      INDEX( 'UuWw', MODE(:1) ) .NE. 0 ) THEN

*        Map axis centre component.
            CALL NDF_AMAP( DSA__REFID1(SLOT), 'CENTRE', AXIS, NDFTY2,
     :         MODE, ADDRESS, NELM, STATUS )

*        Explicitly set the label and units axis components if the
*        MODE is 'WRITE'.
            IF (CHR_SIMLR( MODE(:1), 'W' ) ) THEN
               LABLEN = 0
               LABEL = ' '

               CALL CHR_PUTC( 'Axis ', LABEL, LABLEN)
               CALL CHR_PUTI( AXIS, LABEL, LABLEN)

               CALL NDF_ACPUT( LABEL(1 : LABLEN), DSA__REFID1(SLOT),
     :            'LABEL', AXIS, STATUS )
               CALL NDF_ACPUT( 'pixel', DSA__REFID1(SLOT), 'UNITS',
     :            AXIS, STATUS )
            END IF

*        If the arrays do not exist and pixel indices are required,
*        the centre arrays must be filled.
            IF ( .NOT. EXIST .AND.
     :           INDEX( 'UuWw', MODE(:1) ) .NE. 0 ) THEN

*        Fill the array, selecting the routine of the appropriate type.
               IF ( NDFTY2 .EQ. '_REAL' ) THEN
                  CALL DSA2_IFILLF( NELM, LBND(AXIS),
     :                              %VAL( CNF_PVAL(ADDRESS) ), STATUS )
               ELSE
                  CALL DSA2_IFILLD( NELM, LBND(AXIS),
     :                              %VAL( CNF_PVAL(ADDRESS) ), STATUS )
               END IF
            END IF

*        Fill in map slot.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__MAPUSD(MSLOT) = .TRUE.
               DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
               DSA__MAPAXI(MSLOT) = AXIS
               DSA__MAPREF(MSLOT) = SLOT
               DSA__MAPNAM(MSLOT) = 'CENTRE'
            END IF

         ELSE

*        Find the number of elements in the selected axis.
            NELM = UBND( AXIS ) - LBND( AXIS ) + 1

*        Create some work space to store the axis centres.  Map it using
*        the desired data type.
            CALL DAT_TEMP( NDFTY2, 1, NELM, DSA__MAPLO1(MSLOT), STATUS )

*        Map the work array.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        The pointer must be returned for access in the calling routine.
            ADDRESS = DSA__MAPPT1(MSLOT)

*        Fill the array, selecting the routine of the appropriate type.
            IF ( NDFTY2 .EQ. '_REAL' ) THEN
               CALL DSA2_IFILLF( NELM, LBND(AXIS),
     :            %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ), STATUS )
            ELSE
               CALL DSA2_IFILLD( NELM, LBND(AXIS),
     :            %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ), STATUS )
            END IF

*        Fill in map slot.  Specify the name as NCENTRE to get DSA_UNMAP
*        to release the workspace and not the NDF axis centre.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__MAPUSD(MSLOT) = .TRUE.
               DSA__MAPAXI(MSLOT) = AXIS
               DSA__MAPREF(MSLOT) = SLOT
               DSA__MAPNAM(MSLOT) = 'NCENTRE'
            END IF
         END IF
      END IF

*  Set the centre-modified flag.
      IF ( INDEX( 'UuWw', MODE(:1) ) .NE. 0 )
     :   DSA__REFMDC( AXIS, SLOT ) = .TRUE.

*  Return.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
         CALL DAT_ANNUL( DSA__MAPLO2(MSLOT), STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )
      END IF
      END
