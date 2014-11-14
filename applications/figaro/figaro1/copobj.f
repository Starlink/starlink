      SUBROUTINE COPOBJ( STATUS )
*+
*  Name:
*     COPOBJ

*  Purpose:
*     Copy an HDS object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COPOBJ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates or modifies an object to be a copy of an
*     existing HDS object (in the same or a different container file).
*     The destination object can either be a newly created scalar object
*     or an existing cell of an array. If it is a cell of a structure
*     array, it must be an empty structure.

*  Usage:
*     copobj source object

*  ADAM Parameters:
*     SOURCE = HDSOBJECT (Read)
*        The existing HDS object to be copied. Specify beginning with
*        directory and file name in the syntax of the operating system,
*        followed by the dot-separated structure hierarchy. Elements of
*        structure arrays are specified in ordinary brackets ().
*     OBJECT = HDSOBJECT (Read)
*        The HDS object to be created or modified. Specify beginning
*        with directory and file name in the syntax of the operating
*        system, followed by the dot-separated structure hierarchy.
*        Elements of structure arrays are specified in ordinary brackets
*        (). An array element (cell) cannot be created, but an exisiting
*        cell can be modified.

*  Examples:
*     copobj source=@"file1.dst".Z.DATA object=file2.DATA_ARRAY
*        Copy the data array from a Figaro DST file into the data array
*        of an NDF. Note that file2.DATA_ARRAY must not exist
*        beforehand, and that file2 without DATA_ARRAY is not a legal
*        NDF. So probably this would be the first action after creation
*        of the empty HDS file "file2".

*  Algorithm:
*     This routine copies an HDS object into another one. This is a
*     formidable task, because an HDS object can be
*     -  primitive                 (0) or a structure        (1)
*     -  scalar                    (0) or an array           (2)
*     -  a component of its parent (0) or a cell in an array (4)
*     In addition, the destination object can exist or not exist.
*
*     Not all 8 combinations can acutally exist: cells must always be
*     scalars, never arrays. (Cells are array elements.)
*     Any of the 6 sources can be copied, if it exists, and if the
*     destination specification makes sense for the type of source.
*     So here are the 8 types of sources again, plus the given value:
*     0: not cell, scalar, primitive
*     1: not cell, scalar, structure
*     2: not cell, array,  primitive
*     3: not cell, array,  structure
*     4: cell,     scalar, primitive
*     5: cell,     scalar, structure
*     6: cell,     array,  primitive, this does not exist
*     7: cell,     array,  structure, this does not exist
*     8: given value
*     Cell or not cell can be decided with HDS_TRACE (PATH ends with")"
*     or not).
*     Scalar or array can be decided with DAT_SHAPE.
*     Primitive or structure can be decided with DAT_STRUC.
*
*     If the destination exists:
*     0: cannot overwrite primitive scalar
*     1: cannot overwrite structure scalar (DTA_CYVAR can)
*     2: cannot overwrite primitive array
*     3: cannot overwrite structure array
*     4: OK if source is 0 or 4            (DTA_CYVAR cannot)
*     5: OK if empty and source is 1 or 5
*     6: this does not exist
*     7: this does not exist
*
*     If the destination does not exist:
*     0: OK if source is 0 or 4
*     1: OK if source is 1 or 5
*     2: OK if source is 2
*     3: OK if source is 3
*     4: cannot create cell
*     5: cannot create cell
*     6: this does not exist
*     7: this does not exist
*
*     There are four copy algorithms:
*     A: DAT_PUT( DSTLOC, '_CHAR', 0, 0, CVALUE, STATUS )
*     B: DAT_COPY( SRCLOC, DPAREN, FDCOMP, STATUS )
*     C: DAT_NCOMP( SRCLOC, NCOMP, STATUS )
*           DAT_INDEX( SRCLOC, I=1...NCOMP, TLOC, STATUS )
*           DAT_NAME( TLOC, TNAME, STATUS )
*           DAT_COPY( TLOC, DSTLOC, TNAME, STATUS )
*           DAT_ANNUL( TLOC, STATUS )
*     D: DAT_GET0C( SRCLOC, CVALUE, STATUS )
*        DAT_PUT0C( DSTLOC, CVALUE, STATUS )
*
*     The possible copy operations and algorithms used are:
*     8 -> 0   A; destination must exist
*     8 -> 4   A; destination must exist
*     0 -> 0   B; destination must be new
*     2 -> 2   B; destination must be new
*     3 -> 3   B; destination must be new
*     0 -> 4   D; destination must exist
*     4 -> 4   D; destination must exist
*     4 -> 0   C; destination must be new
*     1 -> 1   C; destination must be new
*     5 -> 1   C; destination must be new
*     1 -> 5   C; destination must exist
*     5 -> 5   C; destination must exist
*     We can use A for 8 -> 0 or 4.
*     We can use B for 0, 2, 3 -> same.
*     We can use D for 0, 4 -> 4.
*     We must use C otherwise. When we use C and destination does not
*     exist, we have to create and locate it with
*     DAT_NEW( DPAREN, FDCOMP, HDSTYP, 0, 0, STATUS )
*     DAT_FIND( DPAREN, FDCOMP, DSTLOC, STATUS )

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     JFL: John Lightfoot (ROE)
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  History:
*     28-NOV-1983 (KS/CIT):
*        Original version.
*     19-APR-1985 (KS):
*        Bug fix. String now set to blanks before character object is
*        read.  Prevents extraneous characters appearing at end of
*        string.
*     01-MAY-1985 (KS):
*        Very minor bug fix. Format of file open error message changed
*        so parentheses match.
*     05-MAY-1986 (KS):
*        ICH_ENCODE replaced by the more accurate ICH_CF for the setting
*        of numeric DCL symbols.
*     25-MAR-1991 (KS):
*        Now allows for more than one default extension, through use of
*        FIGX_ASFNAM.
*     17-SEP-1991 (HME):
*        If data_object and value are in the same file, LET must not try
*        to open it a second time.
*     17-SEP-1991 (JFL):
*        Length of object name incresed from 32 to 80 characters to cope
*        with long names in JCMT structure.
*     01-OCT-1992 (HME):
*        Re-written in terms of HDS.
*     27-OCT-1992 (HME):
*        Reviewed again with the know-how from re-writing DTA_CYVAR.
*     12-MAR-1993 (HME):
*        Report errors immediately.
*     06-APR-1993 (HME):
*        Split away from LET.
*     14-NOV-2014 (TIMJ):
*        HDSv5 can not open a file for READ and then for UPDATE
*        so the logic is reopened to allow the source to be tested,
*        then closed, then reopened later on. The reason for DAT_REF/HDS_FIND
*        is because DAT_ASSOC retains an internal primary locator that will
*        not free the file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*  LOC(1): source
*  LOC(2): parent of destination
*  LOC(3): destination
      CHARACTER * ( 132 ) CVALUE
      LOGICAL SFOUND, DFOUND     ! True if source exists
      LOGICAL SSTRUC, DSTRUC     ! True if source is structure
      INTEGER I                  ! Loop variable
      INTEGER NCOMP              ! How many components to copy
      INTEGER STYPE, DTYPE       ! Type of source/destin.
      INTEGER TNDIM              ! Temporary array dimensionality
      INTEGER TDIMS( DAT__MXDIM ) ! Temporary array dimensions
      INTEGER LPATH              ! Length of SPATH
      CHARACTER * ( 64 ) MESSAG  ! Error message
      CHARACTER * ( DAT__SZLOC ) LOC( 3 ) ! HDS locators
      CHARACTER * ( DAT__SZNAM ) OBJECT ! HDS component name
      CHARACTER * ( DAT__SZTYP ) HDSTYP ! HDS type of the source
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary component locator
      CHARACTER * ( DAT__SZNAM ) TNAME ! Temporary component name
      CHARACTER * ( 132 ) FILE   ! HDS file names
      CHARACTER * ( 132 ) SNAME  ! Source HDS hierarchy path
      CHARACTER * ( 132 ) DNAME  ! Destin. HDS hierarchy path
      CHARACTER * ( 256 ) SPATH  ! Full reference to source object (FILE+hierarchy)

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  This code is mostly adapted from DTA_CYVAR.

*  Locate source by association with an ADAM parameter.
*  Also get the source object name via HDS_TRACE.
*  We use the parameter system to find the name but then
*  we close it and re-open the file later on.
*  This is because some HDS implementations do not like
*  a file to be opened for READ and then later opened for UPDATE
*  whilst still being opened for READ.
      CALL DAT_ASSOC( 'SOURCE', 'READ', LOC(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Source object not found.'
         CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
         GO TO 500
      END IF
      SFOUND = .TRUE.
      CALL HDS_TRACE( LOC(1), I, SNAME, FILE, STATUS )
      CALL DAT_REF( LOC(1), SPATH, LPATH, STATUS )
      CALL DAT_ANNUL( LOC(1), STATUS )
      LOC(1) = DAT__NOLOC
      CALL DAT_CANCL('SOURCE', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Locate destination by association with an ADAM parameter. This may
*  legitimately fail, since in many cases we expect this not to
*  exist.
*  We use DAT_EXIST here because it returns with a status. DAT_ASSOC
*  would repromt forever.
      CALL DAT_EXIST( 'OBJECT', 'UPDATE', LOC(3), STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         DFOUND = .TRUE.
      ELSE
         DFOUND = .FALSE.
         CALL ERR_ANNUL( STATUS )
      END IF

*  Now locate either the destination or its parent.

*  If the destination does not yet exist.
      IF ( .NOT. DFOUND ) THEN

*     Create it temporarily as an empty scalar structure.
*     If a cell was specified as destination, this will fail. That's
*     fine, because DAT_CREAT spots an error condition for us.
         CALL DAT_CREAT( 'OBJECT', 'STRUCT', 0, 0, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Cannot create destination object.'
            CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
            GO TO 500
         END IF

*     Now locate the dummy object's parent, find out the component
*     name within the parent and trace the parent to re-locate it
*     lateron.
         CALL DAT_ASSOC( 'OBJECT', 'UPDATE', LOC(3), STATUS )
         CALL DAT_NAME(  LOC(3), OBJECT, STATUS )
         CALL DAT_PAREN( LOC(3), LOC(2), STATUS )
         CALL HDS_TRACE( LOC(2), I, DNAME, FILE, STATUS )

*     Now delete the dummy.
         CALL DAT_ANNUL( LOC(2), STATUS )
         CALL DAT_ANNUL( LOC(3), STATUS )
         CALL DAT_DELET( 'OBJECT', STATUS )

*     For some obscure reason this would have invalidated the parent
*     locator anyway, so we re-locate it now.
         CALL FIG_HDSDIG( FILE, DNAME, 'UPDATE', LOC(2), STATUS )

*     Check status.
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Cannot locate parent of destination object.'
            CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
            GO TO 500
         END IF

*  Else (destination does exist).
      ELSE

*     Just trace it to later evaluate the path.
         CALL HDS_TRACE(  LOC(3), I, DNAME, FILE, STATUS )
      END IF

*  Reset the types of source and destination.
      STYPE  = 0
      DTYPE  = 0

*Now open the source file properly
      CALL HDS_FIND( DAT__ROOT, SPATH, 'READ', LOC(1), STATUS )

*  We can deduce whether the objects are cells or not.
*  (The destination can be a cell only if it existed. And in that
*  case DNAME is the full destination name.)
      I = CHR_LEN( SNAME )
      IF ( SNAME(I:I) .EQ. ')' ) STYPE = STYPE + 4
      I = CHR_LEN( DNAME )
      IF ( DFOUND .AND. DNAME(I:I) .EQ. ')' ) DTYPE = DTYPE + 4

*  We can deduce whether the objects are arrays or scalars.
*  For a new destination this follows from the attribute of the
*  source.
      CALL DAT_SHAPE( LOC(1), DAT__MXDIM, TDIMS, TNDIM, STATUS )
      IF ( TNDIM .GT. 0 ) THEN
         STYPE = STYPE + 2
         IF ( .NOT. DFOUND ) DTYPE = DTYPE + 2
      END IF
      IF ( DFOUND ) THEN
         CALL DAT_SHAPE( LOC(3), DAT__MXDIM, TDIMS, TNDIM, STATUS )
         IF ( TNDIM .GT. 0 ) DTYPE = DTYPE + 2
      END IF

*  We can deduce whether the objects are primitives or structures.
*  For a new destination this follows from the attribute of the
*  source.
      CALL DAT_STRUC( LOC(1), SSTRUC, STATUS )
      IF ( SSTRUC ) THEN
         STYPE = STYPE + 1
         IF ( .NOT. DFOUND ) DTYPE = DTYPE + 1
      END IF
      IF ( DFOUND ) THEN
         CALL DAT_STRUC( LOC(3), DSTRUC, STATUS )
         IF ( DSTRUC ) DTYPE = DTYPE + 1
      END IF

*  After all these HDS/DAT calls, check the status.
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error assessing the HDS objects involved.'
         CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Now use one of the three algorithms B,C,D to effect the copy.
*  -------------------------------------------------------------

*  If DAT_COPY can be used to copy from source into parent of
*  destination.
      IF (  (  .NOT. DFOUND                        ) .AND.
     :      (  STYPE .EQ. 0 .OR. STYPE .EQ. 2 .OR.
     :         STYPE .EQ. 3                        ) .AND.
     :      (  DTYPE .EQ. STYPE                    )       ) THEN

*     This works only if the destination is new. But DAT_COPY will
*     check that.
         CALL DAT_COPY( LOC(1), LOC(2), OBJECT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Failed to copy object.'
            CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if DAT_GET/PUT0C can be used to copy a primitive scalar into
*  an exiting cell.
      ELSE IF ( DFOUND .AND. DTYPE .EQ. 4          .AND.
     :          ( STYPE .EQ. 0 .OR. STYPE .EQ. 4 )       ) THEN
         CALL DAT_GET0C( LOC(1), CVALUE, STATUS )
         CALL DAT_PUT0C( LOC(3), CVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Failed to copy primitive scalar.'
            CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if DAT_GET/PUT0C can be used to copy a primitive scalar into
*  a new scalar.
      ELSE IF ( ( .NOT. DFOUND ) .AND.
     :          DTYPE .EQ. 0 .AND. STYPE .EQ. 4 ) THEN
         CALL DAT_TYPE( LOC(1), HDSTYP, STATUS )
         CALL DAT_NEW(  LOC(2), OBJECT, HDSTYP, 0, 0, STATUS )
         CALL DAT_FIND( LOC(2), OBJECT, LOC(3), STATUS )
         CALL DAT_GET0C( LOC(1), CVALUE, STATUS )
         CALL DAT_PUT0C( LOC(3), CVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Failed to copy primitive scalar.'
            CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if DAT_INDEX/DAT_COPY can be used to copy source component by
*  component into destination.
      ELSE IF
     :   ( ( STYPE .EQ. 1 .OR. STYPE .EQ. 5            ) .AND.
     :     ( (       DFOUND  .AND. DTYPE .EQ. 5 ) .OR.
     :       ( (.NOT.DFOUND) .AND. DTYPE .EQ. 1 )      )       ) THEN

*     If destination exists, it must be an empty structure.
         IF ( DFOUND ) THEN
            IF ( .NOT. DSTRUC ) THEN
               MESSAG = 'Destination object is not a structure.'
               CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: '
     :            // MESSAG, STATUS )
               GO TO 500
            END IF
            CALL DAT_NCOMP( LOC(3), NCOMP, STATUS )
            IF ( STATUS .EQ. SAI__OK .AND. NCOMP .GT. 0 ) THEN
               MESSAG = 'Destination structure is not empty.'
               CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: '
     :            // MESSAG, STATUS )
               GO TO 500
            END IF
         END IF

*     The algorithm itself requires the destination to exist and to
*     have been located. If necessary, create and locate it now.
*     DTYPE can be 0, 1 or 5, but when it is 5 we know that the
*     destination already exists. For 0 and 1 we need a scalar
*     structure.
         IF ( .NOT. DFOUND ) THEN
            CALL DAT_TYPE( LOC(1), HDSTYP, STATUS )
            CALL DAT_NEW(  LOC(2), OBJECT, HDSTYP, 0, 0, STATUS )
            CALL DAT_FIND( LOC(2), OBJECT, LOC(3), STATUS )
         END IF

*     Find out how many components there are in the source object,
*     then copy each one in turn.
         CALL DAT_NCOMP( LOC(1), NCOMP, STATUS )
         DO 1 I = 1, NCOMP
            CALL DAT_INDEX(  LOC(1), I,   TLOC,  STATUS )
            CALL DAT_NAME(  TLOC,         TNAME, STATUS )
            CALL DAT_COPY(  TLOC, LOC(3), TNAME, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )
 1       CONTINUE

*     If the destination locator was aquired by this routine from
*     DAT_NEW, we want to annul it again.
         IF ( .NOT. DFOUND ) THEN
            CALL DAT_ANNUL( LOC(3), STATUS )
         END IF

*  Else (no copy algorithm available).
      ELSE
         STATUS = SAI__ERROR
         MESSAG =
     :      'Cannot copy between specified source and destination.'
         CALL ERR_REP( 'COPOBJ_ERR', 'COPOBJ: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Annul remaining locators and cancel parameters.
      IF ( DFOUND ) THEN
         CALL DAT_ANNUL( LOC(3), STATUS )
      ELSE
         CALL DAT_ANNUL( LOC(2), STATUS )
      END IF
      CALL DAT_ANNUL( LOC(1), STATUS )
      CALL DAT_CANCL( 'SOURCE', STATUS )
      CALL DAT_CANCL( 'OBJECT', STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         DO 2 I = 3, 1, -1
            CALL DAT_ANNUL( LOC(I), STATUS )
 2       CONTINUE
         CALL DAT_CANCL( 'OBJECT', STATUS )
         CALL DAT_CANCL( 'SOURCE', STATUS )
      END IF

      END
