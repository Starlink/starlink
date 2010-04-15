      SUBROUTINE DSA_CREATE_NAMED_STRUCTURE( DSAREF, NDFNAM, STRID,
     :                                       STATUS )
*+
*  Name:
*     DSA_CREATE_NAMED_STRUCTURE

*  Purpose:
*     Create an NDF, using a name and a structure definition.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_CREATE_NAMED_STRUCTURE( DSAREF, NDFNAM, STRID, STATUS )

*  Description:
*     This routine takes the name of an NDF, creates it as specified
*     by a structure definition file (as read by DSA_READ_STRUCT_DEF)
*     and opens it for output, associating it with a specified reference
*     name.
*     The NDF name can be a filename, or a filename combined with an HDS
*     structure name within the file. A full example might be
*
*        myfile.more.some.struct(5).data
*
*     Note that the following call sequence is frowned upon:
*
*        CALL PAR_RDCHAR()
*        CALL DSA_CREATE_NAMED_STRUCTURE()
*
*     The PAR_RDCHAR may cause at least inconsistencies in the
*     user interface. DSA_*_NAMED_* should be used if the application
*     genuinely knows the NDF name without consulting the parameter
*     system. Applications that use the above sequence should instead:
*
*        CALL DSA_CREATE_STRUCTURE()
*        CALL DSA_GET_ACTUAL_NAME()
*
*     In this sequence the NDF name is acquired from the parameter
*     system in a consistent way and then made available to the
*     application for output to the terminal, an ASCII file or a plot
*     label.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name to be associated with the opened NDF.
*        Only the first 16 characters are significant. The
*        name is case-insensitive.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The name of the NDF.
*     STRID = CHARACTER * ( * ) (Given)
*        The name of a structure type as already defined by a structure
*        definition file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks:  Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     ACD: A C Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31 Aug 1987 (ks):
*        Original version.
*     09 Sep 1988 (ks):
*        PARM_VALUE initialised, together with other common variables.
*     28 Nov 1988 (ks):
*        Now uses DSA_INIT_REF_SLOT for initialisation.
*     15 Jan 1990 (ks):
*        Now sets the file extension common variables, and passes
*        REF_SLOT to DSA_BUILD_STRUCTURE.
*     19 Jan 1990 (ks):
*        DSA_SET_FILE_TYPE is now a DSA__ routine.
*     15 Feb 1991 (ks):
*        Call sequence for DSA_FNAME modified.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Mar 1996 (hme):
*        FDA library.
*     12 Mar 1996 (hme):
*        Initialise NDF data and axis data to zero while they are still
*        just in an HDS structure tree. Once imported into NDF the axis
*        data can no longer be initialised. NDF_ACRE assumes they are
*        fine, NDF_AMAP(...'WRITE'...) assumes 'UPDATE' should be used.
*     27 Aug 1997 (ACD):
*        Explicitly set the character components of the NDF.  (Leaving
*        them undefined resulted in an invalid NDF.)
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
      INCLUDE 'DAT_ERR'          ! DAT status values
      INCLUDE 'NDF_ERR'          ! NDF status values

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) NDFNAM
      CHARACTER * ( * ) STRID

*  Status:
      INTEGER STATUS             ! Global status
      INTEGER DSA_STATUS         ! Internal status for DSA calls

*  Local Variables:
      INTEGER SLOT               ! Reference slot number
      INTEGER STRSLT             ! Structure slot number
      INTEGER INDF               ! Temporary NDF identifier
      CHARACTER * ( 16 ) REFUC   ! Given reference in upper case
      CHARACTER * ( 32 ) TYPE    ! Type of structure

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK
      DSA_STATUS = 0

*  Upper case reference name.
      REFUC = DSAREF
      CALL CHR_UCASE( REFUC )

*  Try to find the reference, in order to check that the same name is
*  not used twice.
      DO 1 SLOT = 1, DSA__MAXREF
         IF ( DSA__REFUSD(SLOT) .AND.
     :        DSA__REFNAM(SLOT) .EQ. REFUC ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL ERR_REP( 'FDA_E089', 'DSA_CREATE_NAMED_STRUCTURE: ' //
     :         'Reference name ^FDA_T001 already in use.', STATUS )
            GO TO 500
         END IF
 1    CONTINUE

*  Find a free slot for the NDF identifier, placeholder, or locator.
      DO 2 SLOT = 1, DSA__MAXREF
         IF ( .NOT. DSA__REFUSD(SLOT) ) GO TO 3
 2    CONTINUE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', REFUC )
         CALL ERR_REP( 'FDA_E090', 'DSA_CREATE_NAMED_STRUCTURE: ' //
     :      'No slot left for reference ^FDA_T001.', STATUS )
         GO TO 500
 3    CONTINUE

*  Attempt to open the named NDF for write access.
      CALL NDF_OPEN( DAT__ROOT, NDFNAM, 'WRITE',
     :   'NEW', DSA__REFID1(SLOT), DSA__REFPLC(SLOT), STATUS )

*  If failure due to the named NDF being open already.
*  Annul the error.
*  Open a new temporary NDF instead of the permanent one.
*  Store intended name in common block.
      IF ( STATUS .EQ. DAT__FILIN .OR. STATUS .EQ. NDF__FILIN ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDF_TEMP( DSA__REFPLC(SLOT), STATUS )
         IF ( STATUS .EQ. SAI__OK ) DSA__REFNDF(SLOT) = NDFNAM
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500


*  Although we got a placeholder for the new NDF now, we won't use it
*  immediately. DSA3_BUILD has to build the NDF as an HDS structure
*  tree, and it does that in a temporary HDS structure.

*  When the tree is finished it is imported into NDF. The new NDF is
*  moderately illegal, since it has undefined data and axis data. These
*  are defined.

*  Then comes the moment to copy the thing into the placeholder of the
*  ultimate permanent NDF.

*  Find type for NDF. Should be 'NDF', but usually is not.
*  Depends on the structure definition file.
      CALL DSA3_DEFTYP( STRID, .TRUE., TYPE, STRSLT, DSA_STATUS )

      IF ( DSA_STATUS .EQ. 0 ) THEN

*     Get the temporary HDS top level.
         CALL DAT_TEMP( TYPE, 0, 0, DSA__REFLOC(SLOT), STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*        Fill in the DSA reference slot, which is used during DSA3_BUILD.
            DSA__REFUSD(SLOT) = .TRUE.
            DSA__REFBAD(SLOT) = .FALSE.
            DSA__REFQUA(SLOT) = .FALSE.
            DSA__REFID2(SLOT) = NDF__NOID
            DSA__REFDPT(SLOT) = 0
            DSA__REFQPT(SLOT) = 0
            DSA__REFNAM(SLOT) = REFUC

*        Build the structure tree.
*        All structures are only created, primitives remain undefined.
            CALL DSA3_BUILD( REFUC, STRSLT, DSA_STATUS )

*        Initialise the data component and each axis component.
*        Axis initialisation cannot be done later with NDF calls,
*        since NDF assumes existence of axis data when AXIS itself exists.
            CALL DSA3_FIXNDF( DSA__REFLOC(SLOT), DSA_STATUS )

            IF ( DSA_STATUS .EQ. 0 ) THEN

*           Import the new structure tree as an NDF.
               CALL NDF_FIND( DSA__REFLOC(SLOT), ' ', INDF, STATUS )

*           Explicitly set the NDF character components.
               CALL NDF_CPUT( 'Unknown', INDF, 'TITLE', STATUS )
               CALL NDF_CPUT( 'Unknown', INDF, 'LABEL', STATUS )
               CALL NDF_CPUT( 'Unknown', INDF, 'UNITS', STATUS )

*           Copy it to the placeholder of the final NDF.
               CALL NDF_SCOPY( INDF, 'DATA,QUAL,VAR,AXIS,UNITS',
     :            DSA__REFPLC(SLOT), DSA__REFID1(SLOT), STATUS )

*           Release the imported NDF.
               CALL NDF_ANNUL( INDF, STATUS )

            END IF

*        Release the temporary structure tree.
            CALL DAT_ANNUL( DSA__REFLOC(SLOT), STATUS )

         END IF

      END IF

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = DSA_STATUS
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA3_FIXNDF( LOC, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) LOC

      INTEGER STATUS

      LOGICAL REPLY
      INTEGER PNTR, NELM
      INTEGER I, N, IGNORE
      CHARACTER * ( DAT__SZLOC ) LOCA, LOCC, LOCD, TLOC

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Locate the data array, primitive or simple.
      CALL CMP_STRUC( LOC, 'DATA_ARRAY', REPLY, STATUS )
      IF ( REPLY ) THEN
         CALL DAT_FIND(  LOC,  'DATA_ARRAY', TLOC, STATUS )
         CALL DAT_FIND(  TLOC, 'DATA',       LOCD, STATUS )
         CALL DAT_ANNUL( TLOC, STATUS )
      ELSE
         CALL DAT_FIND(  LOC,  'DATA_ARRAY', LOCD, STATUS )
      END IF

*  Map data, fill with zeros, unmap, release.
      CALL DAT_MAPV(  LOCD, '_DOUBLE', 'WRITE', PNTR, NELM, STATUS )
      IF ( STATUS .EQ. SAI__OK )
     :   CALL DSA2_CFILLD( NELM, %VAL( CNF_PVAL(PNTR) ), 0D0 )
      CALL DAT_UNMAP( LOCD, STATUS )
      CALL DAT_ANNUL( LOCD, STATUS )

*  If axis structure exists.
      CALL DAT_THERE( LOC, 'AXIS', REPLY, STATUS )
      IF ( REPLY ) THEN

*     Locate AXIS.
         CALL DAT_FIND(  LOC, 'AXIS', LOCA, STATUS )
         CALL DAT_SHAPE( LOCA, 1, N, IGNORE, STATUS )

*     For each axis.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 1 I = 1, N

*           Locate axis data, primitive or simple.
               CALL DAT_CELL(  LOCA, 1, I, LOCC, STATUS )
               CALL CMP_STRUC( LOCC, 'DATA_ARRAY', REPLY, STATUS )
               IF ( REPLY ) THEN
                  CALL DAT_FIND(  LOCC, 'DATA_ARRAY', TLOC, STATUS )
                  CALL DAT_FIND(  TLOC, 'DATA',       LOCD, STATUS )
                  CALL DAT_ANNUL( TLOC, STATUS )
               ELSE
                  CALL DAT_FIND(  LOCC, 'DATA_ARRAY', LOCD, STATUS )
               END IF

*           Map, fill with zeros, unmap, release.
               CALL DAT_MAPV(  LOCD, '_DOUBLE', 'WRITE',
     :            PNTR, NELM, STATUS )
               IF ( STATUS .EQ. SAI__OK )
     :            CALL DSA2_CFILLD( NELM, %VAL( CNF_PVAL(PNTR) ), 0D0 )
               CALL DAT_UNMAP( LOCD, STATUS )
               CALL DAT_ANNUL( LOCD, STATUS )
               CALL DAT_ANNUL( LOCC, STATUS )

 1          CONTINUE
         END IF

*     Release AXIS.
         CALL DAT_ANNUL( LOCA, STATUS )

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
