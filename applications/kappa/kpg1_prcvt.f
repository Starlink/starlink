      SUBROUTINE KPG1_PRCVT( LOC, STATUS )
*+
*  Name:
*     KPG1_PRCVT

*  Purpose:
*     Convert an HDS primitive to a native data representation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PRCVT( LOC, STATUS )

*  Description:
*     The routine converts a primitive HDS object so that it is stored
*     using the appropriate native data representation provided by the
*     host machine.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given and Returned)
*        Locator for the object to be converted. This may be modified
*        on exit (as the original object may have to be erased and
*        re-created).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine returns without action if the object supplied is not
*     primitive, or if it does not need conversion (i.e. if it has
*     already been converted or was created on the current machine). In
*     this case the LOC argument will be returned unchanged.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given and Returned:
      CHARACTER * ( DAT__SZLOC ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZFILE             ! Max. size of file name
      PARAMETER ( SZFILE = 256 )
      INTEGER SZPATH             ! Max. size of path name
      PARAMETER ( SZPATH = 256 )

*  Local Variables:
      CHARACTER * ( 10 ) FORM1 ! Original data format
      CHARACTER * ( 10 ) FORM2 ! Required data format
      CHARACTER * ( 10 ) ORDER1 ! Original byte order
      CHARACTER * ( 10 ) ORDER2 ! Required byte order
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZLOC ) LOCSCR ! Scratch object locator
      CHARACTER * ( DAT__SZLOC ) LOCTMP ! Temporary structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Object name
      CHARACTER * ( DAT__SZTYP ) TYPE ! Object type
      CHARACTER * ( SZFILE ) FILE ! File name
      CHARACTER * ( SZPATH ) PATH ! Path name
      INTEGER DIM( DAT__MXDIM )  ! Object dimension sizes
      INTEGER LENGTH             ! Object length
      INTEGER NDIM               ! Number of object dimensions
      INTEGER NLEV               ! Nesting level
      INTEGER PNTR               ! Pointer to mapped scratch data
      INTEGER SIZE( 1 )          ! Object size
      LOGICAL FIRST              ! First invocation?
      LOGICAL PRIM               ! Object primitive?
      SAVE LOCTMP
      SAVE FIRST

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the locator supplied refers to a primitive object. There
*  is nothing else to do if it does not.
      PRIM = .FALSE.
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. PRIM ) THEN
      
*  If this is the first conversion to be performed, then create a
*  temporary HDS structure for use as a work area, saving its locator.
         IF ( FIRST ) THEN
            DIM( 1 ) = 0
            CALL DAT_TEMP( ' ', 0, DIM, LOCTMP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) FIRST = .FALSE.
         END IF

*  Obtain the type of the object to be converted. Create a scratch
*  scalar object of the same type and obtain a locator to it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_TYPE( LOC, TYPE, STATUS )
            DIM( 1 ) = 0
            CALL DAT_NEW( LOCTMP, 'SCRATCH', TYPE, 0, DIM, STATUS )
            CALL DAT_FIND( LOCTMP, 'SCRATCH', LOCSCR, STATUS )

*  Obtain data representation information for both objects. Annul the
*  scratch object locator and erase the associated object.
            CALL DAT_DREP( LOC, FORM1, ORDER1, STATUS )
            CALL DAT_DREP( LOCSCR, FORM2, ORDER2, STATUS )
            CALL DAT_ANNUL( LOCSCR, STATUS )
            CALL DAT_ERASE( LOCTMP, 'SCRATCH', STATUS )
         END IF

*  Check if the object needs to be converted. If so, obtain its name,
*  shape, size and length.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( ( FORM1 .NE. FORM2 ) .OR. ( ORDER1 .NE. ORDER2 ) ) THEN
               CALL DAT_NAME( LOC, NAME, STATUS )
               CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM, STATUS )
               CALL DAT_SIZE( LOC, SIZE( 1 ), STATUS )
               CALL DAT_LEN( LOC, LENGTH, STATUS )

*  If this is a character object, then create a scratch object with the
*  same type and size and obtain a locator to it.
               IF ( TYPE( : 5 ) .EQ. '_CHAR' ) THEN
                  CALL DAT_NEW( LOCTMP, 'SCRATCH', TYPE, 1, SIZE,
     :                          STATUS )
                  CALL DAT_FIND( LOCTMP, 'SCRATCH', LOCSCR, STATUS )

*  Map the scratch object for writing and read the values out of the
*  original object into it (thus performing the conversion).
                  CALL DAT_MAPV( LOCSCR, TYPE, 'WRITE', PNTR, SIZE( 1 ),
     :                           STATUS )
                  CALL KPG1_NAGTC( %VAL( PNTR ), LOC, NDIM, DIM, STATUS,
     :                             %VAL( LENGTH ) )

*  If it is not a character object, then allocate memory for the
*  converted values and read the original object valuies into it (thus
*  performing the conversion).
               ELSE
                  CALL PSX_MALLOC( LENGTH * SIZE( 1 ), PNTR, STATUS )
                  CALL DAT_GET( LOC, TYPE, NDIM, DIM, %VAL( PNTR ),
     :                          STATUS )
               END IF

*  Determine the original object's nesting level and file and path
*  names.
               CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  If it is a top-level object, then erase the container file and
*  create a new one with the same name (the resulting object will then
*  have the correct data representation).
               IF ( NLEV. LE. 1 ) THEN
                  CALL HDS_ERASE( LOC, STATUS )
                  CALL HDS_NEW( FILE, NAME, TYPE, NDIM, DIM, LOC,
     :                          STATUS )

*  If it is not a top-level object, then find its parent structure and
*  erase the original object within it.
               ELSE
                  CALL DAT_PAREN( LOC, LOCP, STATUS )
                  CALL DAT_ANNUL( LOC, STATUS )
                  CALL DAT_ERASE( LOCP, NAME, STATUS )

*  Create a new object with the same name and obtain a locator for it.
*  Annul the parent locator.
                  CALL DAT_NEW( LOCP, NAME, TYPE, NDIM, DIM, STATUS )
                  CALL DAT_FIND( LOCP, NAME, LOC, STATUS )
                  CALL DAT_ANNUL( LOCP, STATUS )
               END IF

*  If this is a character object, then write the converted values into
*  the new object, annul the scratch object locator and erase the
*  associated object.
               IF ( TYPE( : 5 ) .EQ. '_CHAR' ) THEN
                  CALL KPG1_NAPTC( %VAL( PNTR ), LOC, NDIM, DIM, STATUS,
     :                             %VAL( LENGTH ) )
                  CALL DAT_ANNUL( LOCSCR, STATUS )
                  CALL DAT_ERASE( LOCTMP, 'SCRATCH', STATUS )

*  If it is not a character object, then write the converted values and
*  free the allocated memory.
               ELSE
                  CALL DAT_PUT( LOC, TYPE, NDIM, DIM, %VAL( PNTR ),
     :                          STATUS )
                  CALL PSX_FREE( PNTR, STATUS )
               END IF
            END IF
         END IF
      END IF

      END
