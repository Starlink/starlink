      SUBROUTINE IRH1_GTIDH( TITLE, IDH, STATUS )
*+
*  Name:
*     IRH1_GTIDH

*  Purpose:
*     Create a new empty group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_GTIDH( TITLE, IDH, STATUS )

*  Description:
*     The IRH_ system stores information describing each group in
*     arrays in common.  The same element from different arrays holds
*     information for the same group. Different elements within each
*     array hold information for different groups. The identifiers used
*     by IRH to identify each group are just the index into these
*     common arrays at which the information describing the group is
*     stored. The arrays in common have a lower bound of 1 and an upper
*     bound given by symbolic constant IRH__MAXG.
*
*     The common arrays hold "global" properties of each group (such as
*     the group size for instance). The actual contents of each group
*     are stored in temporary HDS structures. These structures have an
*     HDS type of GROUP. An array of GROUP structures is created,
*     initially containing only a small number of groups. The size of
*     this array is extended as neccesary to make room for new groups.
*     Each cell of this array is a single GROUP structure and contains
*     the following components:
*
*     TITLE (_CHAR) - A title for the group.
*
*     NAMES(SIZE) (_CHAR) - The names which form the group. This is a
*     character array. The array is extended as necessary to make room
*     for new names. The current size is stored in common array
*     HCM_SIZE. Unused elements of the NAMES array are set to the value
*     of the symbolic constant IRH__BLANK.
*
*     MOD_GROUP(SIZE) (_INTEGER) - This is zero for all names NOT
*     created by a modification element. For names created by a
*     modification element, MOD_GROUP gives the IRH identifier of the
*     group containing the name upon which this name was based.
*
*     MOD_INDEX(SIZE) (_INTEGER) - This is zero for all names not
*     created by a modification element. For names created by a
*     modification element, MOD_INDEX gives the index of the name upon
*     which this name was based. The index refers to the group
*     identified by MOD_GROUP.
*
*     LEVEL(SIZE) (_INTEGER) - This gives the depth of indirection at
*     which the corresponding name was specified. Names given directly,
*     rather than by indirection have a value zero.
*
*     FILE(SIZE) (_CHAR) - This is blank for names with indirection
*     level of zero. For other names, it gives the specification of
*     the file in which the name was given.
*
*     HDS locators to all these components are stored in common. The
*     arrays are kept permanently mapped until the group is annulled.
*     The pointers are also stored in common.
*
*     This routine finds the lowest common array index not currently in
*     use, and returns it as the IDH identifier value. If there is no
*     free space in the common arrays an error is reported. The
*     identifier thus found is used as an index into the HDS array of
*     GROUP structures. If the IDH value is bigger than the size of the
*     array, the array is extended to accomodate the new group. The
*     components listed above are created within the GROUP structure
*     indexed by IDH. The locator to the GROUP structure is stored in
*     common.
*
*     The string supplied in the TITLE argument is stored in the TITLE
*     component. The NAMES, MOD_GROUP, MOD_INDEX, LEVEL, and FILE
*     arrays are mapped, and the pointers stored in common, as is the
*     size of these mapped arrays. The MOD_GROUP, MOD_INDEX and LEVEL
*     components are all initialised to hold zeros.  The FILE component
*     is initialised to hold blank strings.
*
*     If any error occurs, an invalid group identifier (IRH__NOID) is
*     returned.

*  Arguments:
*     TITLE = CHARACTER (Given)
*        A title to associate with the group.
*     IDH = INTEGER (Returned)
*        The group identifier for the new group. Set to IRH__NOID if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed references to I90_PAR and added trailing string
*        lengths and DAT_PAR.
*     27-FEB-1992 (PDRAPER):
*        Changed IRH1_SETC argument list.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_LOC = CHARACTER (Read)
*           An HDS locator to the array of GROUP structures.
*        HCM_LOCG( IRH__MAXG ) = CHARACTER (Write)
*           HDS locators to each individual GROUP structure. 
*        HCM_LOCNM( IRH__MAXG ) = CHARACTER (Write)
*           HDS locators to NAMES component of each group.
*        HCM_LOCMG( IRH__MAXG ) = CHARACTER (Write)
*           HDS locators to MOD_GROUP component of each group.
*        HCM_LOCMI( IRH__MAXG ) = CHARACTER (Write)
*           HDS locators to MOD_INDEX component of each group.
*        HCM_LOCLV( IRH__MAXG ) = CHARACTER (Write)
*           HDS locators to LEVEL component of each group.
*        HCM_LOCFL( IRH__MAXG ) = CHARACTER (Write)
*           HDS locators to FILE component of each group.
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped NAMES array of each group.
*        HCM_MGPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        HCM_MIPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        HCM_LVPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped LEVEL array of each group.
*        HCM_FLPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILE array of each group.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Write and Write)
*           The index of the last entry in each group.
*        HCM_SIZE( IRH__MAXG ) = INTEGER (Write)
*           The size of the array components in each GROUP structure.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read and Write)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      CHARACTER TITLE*(*)

*  Arguments Returned:
      INTEGER IDH

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER NEL                ! No. of elements in mapped array.
      INTEGER SIZE               ! Current size of the array of GROUP
                                 ! structures.
      INTEGER TLEN               ! Used length of the title.
*.

*  Set the output group identifier invalid before checking the status.
      IDH = IRH__NOID
      
*  Check inherited global status.If bad, return with an invalid
*  identifier.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the lowest IRH identifier not currently in use.
      DO I = 1, IRH__MAXG
         IF( .NOT.HCM_VALID( I ) .AND. IDH .EQ. IRH__NOID ) IDH = I
      END DO      

*  If no invalid identifier was found, give an error report.
      IF( IDH .EQ. IRH__NOID ) THEN
         STATUS = IRH__NOMORE
         CALL ERR_REP('IRH1_GTIDH_ERR1',
     :              'IRH1_GTIDH: Maximum number of groups exceeded',
     :               STATUS )
         GO TO 999
      END IF

*  If the HDS array of GROUP structures is not big enough to hold the
*  new group, extend it. Since extending an HDS file is an expensive 
*  operation, the array is extended by more than necessary to reduce the
*  number of times the array needs to be extended.
      CALL DAT_SIZE( HCM_LOC, SIZE, STATUS )
      IF( IDH .GT. SIZE ) CALL DAT_ALTER( HCM_LOC, 1, IDH + IRH__INCG,
     :                                     STATUS )

*  Get a locator to the cell with index equal to IDH.  This locates the
*  GROUP structure used to store the names contained in the new group.
*  The locator is stored in common.
      CALL DAT_CELL( HCM_LOC, 1, IDH, HCM_LOCG( IDH ), STATUS )

*  Create a character scalar component called TITLE, within the GROUP
*  structure and store the supplied title.
      TLEN = MAX( 1, CHR_LEN( TITLE ) )
      CALL DAT_NEW0C( HCM_LOCG( IDH ), 'TITLE', TLEN, STATUS )
      CALL CMP_PUT0C( HCM_LOCG( IDH ), 'TITLE', TITLE( : TLEN ),
     :                STATUS )

*  Create a character array component called NAMES, within the GROUP
*  structure. This is extended as necessary by other IRH routines. The
*  initial size of this array is given by the symbolic constant 
*  IRH_INITN.
      CALL DAT_NEW1C( HCM_LOCG( IDH ), 'NAMES', IRH__SZNAM, IRH__INITN,
     :                STATUS )
      HCM_SIZE( IDH ) = IRH__INITN

*  Get a locator to the NAMES array, and store it in common.
      CALL DAT_FIND( HCM_LOCG( IDH ), 'NAMES', HCM_LOCNM( IDH ),
     :               STATUS )

*  Map the NAMES array, storing the pointer and size in common.
      CALL DAT_MAPV( HCM_LOCNM( IDH ), '_CHAR', 'WRITE',
     :               HCM_NMPNT( IDH ), NEL, STATUS )

*  Set the contents of NAMES to "blank".  NB, the final argument
*  specifies the length of each character string in the mapped NAMES
*  array, and is required by UNIX. There is no corresponding dummy
*  argument in the code for IRH1_SETC.
      CALL IRH1_SETC( 1, NEL, NEL, %VAL( HCM_NMPNT( IDH ) ),
     :                IRH__BLANK, STATUS, %VAL( IRH__SZNAM ) )

*  Create the MOD_GROUP component.
      CALL DAT_NEW1I( HCM_LOCG( IDH ), 'MOD_GROUP', IRH__INITN, STATUS )

*  Get a locator to the MOD_GROUP component, and store it in common.
      CALL DAT_FIND( HCM_LOCG( IDH ), 'MOD_GROUP', HCM_LOCMG( IDH ),
     :               STATUS )

*  Map the MOD_GROUP array, storing the pointer in common (the size will
*  always be the same as NAMES).
      CALL DAT_MAPV( HCM_LOCMG( IDH ), '_INTEGER', 'WRITE',
     :               HCM_MGPNT( IDH ), NEL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of MOD_GROUP to zero.
      CALL IRH1_SETI( 1, NEL, 0, NEL, %VAL( HCM_MGPNT( IDH ) ), STATUS )

*  Create the MOD_INDEX component.
      CALL DAT_NEW1I( HCM_LOCG( IDH ), 'MOD_INDEX', IRH__INITN, STATUS )

*  Get a locator to the MOD_INDEX component, and store it in common.
      CALL DAT_FIND( HCM_LOCG( IDH ), 'MOD_INDEX', HCM_LOCMI( IDH ),
     :               STATUS )

*  Map the MOD_INDEX array, storing the pointer in common (the size will
*  always be the same as NAMES).
      CALL DAT_MAPV( HCM_LOCMI( IDH ), '_INTEGER', 'WRITE',
     :               HCM_MIPNT( IDH ), NEL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of MOD_INDEX to zero.
      CALL IRH1_SETI( 1, NEL, 0, NEL, %VAL( HCM_MIPNT( IDH ) ), STATUS )

*  Create the LEVEL component.
      CALL DAT_NEW1I( HCM_LOCG( IDH ), 'LEVEL', IRH__INITN, STATUS )

*  Get a locator to the LEVEL component, and store it in common.
      CALL DAT_FIND( HCM_LOCG( IDH ), 'LEVEL', HCM_LOCLV( IDH ),
     :               STATUS )

*  Map the LEVEL array, storing the pointer in common (the size will
*  always be the same as NAMES).
      CALL DAT_MAPV( HCM_LOCLV( IDH ), '_INTEGER', 'WRITE',
     :               HCM_LVPNT( IDH ), NEL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of LEVEL to zero.
      CALL IRH1_SETI( 1, NEL, 0, NEL, %VAL( HCM_LVPNT( IDH ) ), STATUS )

*  Create the FILE component.
      CALL DAT_NEW1C( HCM_LOCG( IDH ), 'FILE', IRH__SZNAM, IRH__INITN,
     :                STATUS )

*  Get a locator to the FILE component, and store it in common.
      CALL DAT_FIND( HCM_LOCG( IDH ), 'FILE', HCM_LOCFL( IDH ),
     :               STATUS )

*  Map the FILE array, storing the pointer in common (the size will    
*  always be the same as NAMES).
      CALL DAT_MAPV( HCM_LOCFL( IDH ), '_CHAR', 'WRITE',
     :               HCM_FLPNT( IDH ), NEL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999
     
*  Set the contents of LEVEL to blank.  NB, the final argument
*  specifies the length of each character string in the mapped FILE
*  array, and is required by UNIX. There is no corresponding dummy
*  argument in the code for IRH1_SETC.
      CALL IRH1_SETC( 1, NEL, NEL, %VAL( HCM_FLPNT( IDH ) ), 
     :                ' ', STATUS, %VAL( IRH__SZNAM ) )

*  The number of used entries in these array (called the "group size")
*  will not be the same as the total array size if there are any spare
*  entries. Initialise the group size to zero.
      HCM_GSIZE( IDH ) = 0

*  If all is OK, indicate that the group is in use.
      IF ( STATUS .EQ. SAI__OK ) HCM_VALID( IDH ) = .TRUE.

*  If an error has occured ensure the group identifier is invalid, and
*  give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         IDH = IRH__NOID
         CALL ERR_REP( 'IRH1_GTIDH_ERR2',
     :                 'IRH1_GTIDH: Unable to initialise a new group',
     :                 STATUS )
      END IF

      END
* $Id$
