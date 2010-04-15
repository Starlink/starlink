      SUBROUTINE DSA_MAP_WIDTH( DSAREF, AXIS, MODE, TYPE,
     :                          ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_MAP_WIDTH

*  Purpose:
*     Map one of the axis width arrays in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MAP_WIDTH( DSAREF, AXIS, MODE, TYPE,
*        ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps a specified axis width array in an NDF. If there
*     is in fact no axis width array, then an array is generated and its
*     address is returned. The contents of such an implicit width array
*     depend on the axis centre array, the width values are half the
*     difference between the left and right neighbour pixel's centre
*     values.

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
*
*     An N-dimensional width array cannot be defaulted, therefore read
*     or update access to an absent N-dimensional width will fail.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26 Aug 1988 (ks):
*        Original version.
*     08 Sep 1989 (ks):
*        Call to DSA_MAP_ARRAY now sets propagate flag false.
*     11 Dec 1989 (ks):
*        Now sets update flag for write or update mapping.
*     02 Mar 1990 (ks):
*        Now uses DSA__ routines to get structure details, rather than
*        assuming the original Figaro data structure.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     26 Oct 1994 (ks):
*        Now uses new calling sequence for DSA_MAP_ARRAY.
*     20 Feb 1996 (hme):
*        FDA library.
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

*  Find reference slot and call work routine.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_MAPWID( SLOT, AXIS, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )

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



      SUBROUTINE DSA1_MAPWID( SLOT, AXIS, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )

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
      LOGICAL ISBAS              ! Whether main NDF is base NDF
      LOGICAL EXIST              ! Whether a component exists
      INTEGER NELM               ! Ignored
      INTEGER NDIM               ! Array dimensionality
      INTEGER DIM( NDF__MXDIM )  ! Array dimensions
      CHARACTER * ( NDF__SZTYP ) NDFTY1 ! The type in NDF speak
      CHARACTER * ( NDF__SZTYP ) NDFTY2 ! The type in NDF speak
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      CHARACTER * ( DAT__SZLOC ) DELLOC ! Ignored
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! HDS locator
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! HDS locator

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
*  The width locator DSA__MAPLO1 is used here as centre locator. It will
*  be modified further below to locate the width.
      CALL DSA1_AXISND( SLOT, AXIS,
     :   AXISND, DSA__MAPLO1(MSLOT), DELLOC, NAME, STATUS )
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
            CALL ERR_REP( 'FDA_E091', 'DSA_MAP_WIDTH: Cannot ' //
     :         'access a multi-dimensional width array if the ' //
     :         'data structure is an NDF section. Failure for axis ' //
     :         'number ^FDA_T002 in reference ^FDA_T001.', STATUS )
            GO TO 500
         END IF

*     Starting from the centre array locator, look for the width
*     component.
         CALL DAT_NAME( DSA__MAPLO1(MSLOT), NAME, STATUS )
         IF ( NAME .EQ. 'DATA' ) THEN
            CALL DAT_PAREN( DSA__MAPLO1(MSLOT), TLOC2, STATUS )
            CALL DAT_PAREN( TLOC2,  TLOC1, STATUS )
            CALL DAT_ANNUL( TLOC2, STATUS )
         ELSE
            CALL DAT_PAREN( DSA__MAPLO1(MSLOT), TLOC1, STATUS )
         END IF

*     If there is no width.
         CALL DAT_THERE( TLOC1, 'WIDTH', EXIST, STATUS )
         IF ( .NOT. EXIST ) THEN

*        If write access, create WIDTH.DATA as undefined array with same
*        shape as centre array and with given data type.
            IF ( CHR_SIMLR( MODE(:1), 'W' ) ) THEN
               CALL DAT_SHAPE( DSA__MAPLO1(SLOT), NDF__MXDIM,
     :            DIM, NDIM, STATUS )
               CALL DAT_NEW(  TLOC1, 'WIDTH', 'ARRAY', 0, 0, STATUS )
               CALL DAT_FIND( TLOC1, 'WIDTH', TLOC2, STATUS )
               CALL DAT_NEW(  TLOC2, 'DATA', NDFTY2, NDIM, DIM, STATUS )
               CALL DAT_ANNUL( TLOC2, STATUS )

*        Else (read or update access), report an error.
*        (We should create a defined width array with values derived
*        from the centre array, and that width array should be temporary
*        if the access mode is read. ND axes are rare, widths are rare,
*        access to absent widths is rare, therefore this case is very
*        very very rare.)
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'FDA_T002', AXIS )
               CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
               CALL ERR_REP( 'FDA_E092', 'DSA_MAP_WIDTH: Cannot ' //
     :            'generate default values for a multi-dimensional ' //
     :            'width array. Failure for axis ' //
     :            'number ^FDA_T002 in reference ^FDA_T001.', STATUS )
               GO TO 500
            END IF

         END IF

*     There is now a width and TLOC1 is its parent. Replace the centre
*     locator with the locator of the width.
         CALL CMP_STRUC( TLOC1, 'WIDTH', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
            CALL DAT_FIND(  TLOC1, 'WIDTH', TLOC2, STATUS )
            CALL DAT_FIND(  TLOC2, 'DATA', DSA__MAPLO1(MSLOT), STATUS )
            CALL DAT_ANNUL( TLOC2, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         ELSE
            CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
            CALL DAT_FIND(  TLOC1, 'WIDTH', DSA__MAPLO1(MSLOT), STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF

*     Find out stored type of the width array.
         CALL DAT_TYPE( DSA__MAPLO1(MSLOT), NDFTY1, STATUS )

*     If stored and requested type are the same.
         IF ( CHR_SIMLR( NDFTY1, NDFTY2 ) ) THEN

*        Map the stored array on third locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        First pointer is the returned address.
            ADDRESS = DSA__MAPPT1(MSLOT)

*     Else if read access.
         ELSE IF ( CHR_SIMLR( MODE(:1), 'R' ) ) THEN

*        Map the stored array on third locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        Create temporary vector of requested type on fourth locator.
            CALL DAT_TEMP( NDFTY2, 1, NELM,
     :         DSA__MAPLO2(MSLOT), STATUS )

*        Map temporary vector on fourth pointer (=returned address).
            CALL DAT_MAPV( DSA__MAPLO2(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT2(MSLOT), NELM, STATUS )
            ADDRESS = DSA__MAPPT2(MSLOT)

*        Copy data from stored array to temporary vector.
            CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM,
     :                       %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ),
     :                       %VAL( CNF_PVAL(DSA__MAPPT2(MSLOT)) ),
     :                       STATUS )

*        Unmap and annull third locator, zero third pointer.
            CALL DAT_UNMAP( DSA__MAPLO1(MSLOT), STATUS )
            CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
            DSA__MAPPT1(MSLOT) = 0

*     Else if write access.
         ELSE IF ( CHR_SIMLR( MODE(:1), 'W' ) ) THEN

*        Map the stored array on third locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        Create temporary vector of requested type on fourth locator.
            CALL DAT_TEMP( NDFTY2, 1, NELM,
     :         DSA__MAPLO2(MSLOT), STATUS )

*        Map temporary vector on fourth pointer (=returned address).
            CALL DAT_MAPV( DSA__MAPLO2(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT2(MSLOT), NELM, STATUS )
            ADDRESS = DSA__MAPPT2(MSLOT)

*     Else (update access).
         ELSE

*        Map the stored array on third locator and pointer.
            CALL DAT_MAPV( DSA__MAPLO1(MSLOT), NDFTY1, MODE,
     :         DSA__MAPPT1(MSLOT), NELM, STATUS )

*        Create temporary vector of requested type on fourth locator.
            CALL DAT_TEMP( NDFTY2, 1, NELM,
     :         DSA__MAPLO2(MSLOT), STATUS )

*        Map temporary vector on fourth pointer (=returned address).
            CALL DAT_MAPV( DSA__MAPLO2(MSLOT), NDFTY2, 'WRITE',
     :         DSA__MAPPT2(MSLOT), NELM, STATUS )
            ADDRESS = DSA__MAPPT2(MSLOT)

*        Copy data from stored array to temporary vector.
            CALL DSA1_CPDAT( NDFTY1, NDFTY2, NELM,
     :                       %VAL( CNF_PVAL(DSA__MAPPT1(MSLOT)) ),
     :                       %VAL( CNF_PVAL(DSA__MAPPT2(MSLOT)) ),
     :                       STATUS )

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
            DSA__MAPNAM(MSLOT) = 'NWIDTH'
         END IF

*     Release delible locator.
         CALL DAT_ANNUL( DELLOC, STATUS )

*  Else (axis is 1-D).
      ELSE

*     If component is new and mode is write or update, set the type to
*     the given type.
         CALL NDF_ASTAT( DSA__REFID1(SLOT), 'WIDTH', AXIS,
     :      EXIST, STATUS )
         IF ( ( CHR_SIMLR(MODE(:1),'W') .OR. CHR_SIMLR(MODE(:1),'U') )
     :      .AND. .NOT. EXIST ) CALL NDF_ASTYP( NDFTY2,
     :      DSA__REFID1(SLOT), 'WIDTH', AXIS, STATUS )

*     Map axis width component.
         CALL NDF_AMAP( DSA__REFID1(SLOT), 'WIDTH', AXIS, NDFTY2, MODE,
     :      ADDRESS, NELM, STATUS )

*     Fill in map slot.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DSA__MAPUSD(MSLOT) = .TRUE.
            DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
            DSA__MAPAXI(MSLOT) = AXIS
            DSA__MAPREF(MSLOT) = SLOT
            DSA__MAPNAM(MSLOT) = 'WIDTH'
         END IF

      END IF

*  Set the width-modified flag.
      IF ( INDEX( 'UuWw', MODE(:1) ) .NE. 0 )
     :   DSA__REFMDW( AXIS, SLOT ) = .TRUE.

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( TLOC2, STATUS )
         CALL DAT_ANNUL( TLOC1, STATUS )
         CALL DAT_ANNUL( DSA__MAPLO1(MSLOT), STATUS )
         CALL DAT_ANNUL( DSA__MAPLO2(MSLOT), STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )
      END IF

*  Return.
      END
