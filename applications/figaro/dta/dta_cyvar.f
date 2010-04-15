      SUBROUTINE DTA_CYVAR( SNAME, DNAME, STATUS )
*+
*  Name:
*     DTA_CYVAR

*  Purpose:
*     Copy a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_CYVAR( SNAME, DNAME, STATUS )

*  Description:
*     This routine copies an HDS object into another one. This is a
*     formidable task, because an HDS object can be
*     -  primitive                 (0) or a structure        (1)
*     -  scalar                    (0) or an array           (2)
*     -  a component of its parent (0) or a cell in an array (4)
*     In addition, the destination object can exist, not exist, or be a
*     new top level (in a new HDS file).
*
*     Not all 8 combinations can acutally exist: cells must always be
*     scalars, never arrays. (Cells are array elements.)
*     Any of the 6 sources can be copied, if it exists, and if the
*     destination specification makes sense for the type of source.
*     So here are the 8 types of sources again:
*     0: not cell, scalar, primitive
*     1: not cell, scalar, structure
*     2: not cell, array,  primitive
*     3: not cell, array,  structure
*     4: cell,     scalar, primitive
*     5: cell,     scalar, structure
*     6: cell,     array,  primitive, this does not exist
*     7: cell,     array,  structure, this does not exist
*     Cell or not cell can be decided from the dimensionality returned
*     by DTA_SPLITN: Cell <=> NDIM.GT.0.
*     Scalar or array can be decided with DAT_SHAPE.
*     Primitive or structure can be decided with DAT_STRUC.
*
*     If the destination exists, it must be an empty scalar structure:
*     0: cannot overwrite primitive
*     1: OK if empty and source is 1 or 5
*     2: cannot overwrite primitive
*     3: cannot overwrite array structure
*     4: cannot overwrite primitive
*     5: OK if empty and source is 1 or 5
*     6: this does not exist
*     7: this does not exist
*
*     If the destination does not exist, we can copy anything so long as
*     the destination is not a cell.
*     0: OK if source is 0 or 4
*     1: OK if source is 1 or 5
*     2: OK if source is 2
*     3: OK if source is 3
*     4: cannot create cell
*     5: cannot create cell
*     6: this does not exist
*     7: this does not exist
*     8: In addition, a new file (new top level) can be created if the
*        source is 1 or 5.
*
*     There are three copy algorithms:
*     A: HDS_COPY( SRCLOC, DFILE, STATUS )
*     B: DAT_COPY( SRCLOC, DPAREN, FDCOMP, STATUS )
*     C: DAT_NCOMP( SRCLOC, NCOMP, STATUS )
*           DAT_INDEX( SRCLOC, I=1...NCOMP, TLOC, STATUS )
*           DAT_NAME( TLOC, TNAME, STATUS )
*           DAT_COPY( TLOC, DSTLOC, TNAME, STATUS )
*           DAT_ANNUL( TLOC, STATUS )
*
*     The possible copy operations and algorithms used are:
*     1 -> 8   A
*     0 -> 0   B; destination must be new
*     1 -> 1   B if destination is new
*     2 -> 2   B; destination must be new
*     3 -> 3   B; destination must be new
*     4 -> 0   C; destination must be new
*     1 -> 1   C if destination exists
*     5 -> 1   C; destination may exist or be new
*     1 -> 5   C; destination must exist
*     5 -> 5   C; destination must exist
*     5 -> 8   C
*     We can use A for 1 -> 8.
*     We can use B for 0, 1, 2, 3 -> same.
*     We must use C otherwise. When we use C and destination does not
*     exist, we have to create and locate it with
*     DAT_NEW( DPAREN, FDCOMP, HDSTYP, 0, 0, STATUS )
*     DAT_FIND( DPAREN, FDCOMP, DSTLOC, STATUS )
*     or with
*     HDS_NEW( DFILE, FDNAME, HDSTYP, 0, 0, DSTLOC, STATUS )

*  Arguments:
*     SNAME = CHARACTER * ( * ) (Given)
*        The object to be copied. Must be terminated either by a space
*        or by the end of the string. Case is not significant. SNAME
*        must not include dimension information unless to specify array
*        elements.
*     DNAME = CHARACTER * ( * ) (Given)
*        The name of the destination object. Generally, this should not
*        already exist but its parent structure must exist. If DNAME is
*        a top level name then a new file will be created if it does not
*        exist. If it is a top level name and the file does exist then
*        its top level structure must be empty. SNAME must not include
*        dimension information unless to specify array elements.
*     STATUS = INTEGER (Returned)
*        The returned DTA status:
*        0:
*           OK.
*        DTA_NOTFND:
*           source object not found,
*           expected destination to exist, but didn't.
*        DTA_EXIST:
*           expected destination not to exist, but did,
*           expected existing destination to be structure, but wasn't,
*           expected existing destination to be empty structure, but
*              contained components.
*        DTA_INVCPY:
*           cannot copy that kind or source to that kind of destination.
*        other:
*           DTA status from a deeper level or translated from an HDS
*           status.

*  Authors:
*     KS: Keith Shortridge (CIT, AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1982 (KS):
*        Original version.
*     27-MAR-1986 (KS):
*        Re-written for use with the HDS based version of the DTA
*        package.
*     27-JUN-1986 (KS):
*        Optimisation of using direct file copy for a top-level ->
*        top-level copy added.
*     29-OCT-1986 (KS):
*        Calls to DTA_ASFNAM are now made using DNAME instead of FDNAME
*        to generate the filename. This is because FDNAME will have '_'
*        where DNAME has '-' (which became valid in VMS 4.4 filenames).
*     03-FEB-1987 (KS):
*        Modified to make use of the fact that the new version of the
*        HDS routine DAT_COPY, will do a recursive copy, and to use the
*        new routine HDS_COPY.  (This routine will now no longer work
*        with the original Bliss version of HDS).
*     20-AUG-1987 (KS):
*        Was not noticing if HDS_COPY failed, and was returning OK
*        status.  Now takes error exit.
*     09-JUN-1988 (KS):
*        Now supports copying structures which are elements of structure
*        arrays.  Subtle change is that it will now copy a structure
*        into an existing structure, so long as it is empty.
*     13-MAR-1990 (KS):
*        Was only copying a structure into an existing empty structure
*        if it was an array element. This restriction removed.
*     10-JAN-1992 (KS):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to  enable compilation on a SUN.
*        HDS_ERASE now used instead of LIB$DELETE_FILE.
*     24-JAN-1992 (KS):
*        Calls to EMS added to control error reporting.
*     06-FEB-1992 (KS):
*        Call to STR$DNCASE added to make sure that any new file created
*        has a lower case name ('.dst' and not '.DST'). Whether this
*        change should stay is a moot point.
*     12-FEB-1992 (KS):
*        STR$DNCASE replaced by DTAZ_FILEFOLD.
*     18-AUG-1992 (HME):
*        Changed back to DTA_*.
*     15-OCT-1992 (HME):
*        Re-written. Now can copy from and to structure array elements
*        (by copying component by component).
*     25-OCT-1992 (HME):
*        Changed STR$DNCASE to ICH_DFOLD.
*     12-MAR-1993 (HME):
*        Changed CHARACTER*15 to *(DAT__SZLOC).
*     16-JUN-1993 (hme):
*        Call EMS_BEGIN after status is set zero, otherwise EMS_END will
*        restore any bad status on entry, although the routine may have
*        worked.
*     19-JUL-1995 (KS):
*        Added use of EMS_STATUS variable to prevent 'cross-talk' between
*        the status passed to EMS calls and the overall STATUS value.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants and Variables:
      INCLUDE 'DTACODES'         ! DTA system error codes
      INCLUDE 'DTASDEF'          ! Data structure parameters
      INCLUDE 'DTAPROBE'         ! Internal error probe common block
      INCLUDE 'DAT_PAR'          ! DAT global constants
      INCLUDE 'SAE_PAR'          ! SAE global constants

*  Arguments Given:
      CHARACTER * ( * ) SNAME
      CHARACTER * ( * ) DNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Internal references:
      INTEGER ICH_DFOLD          ! Convert string to lower case

*  Local Variables:
      LOGICAL SFOUND, DFOUND     ! True if source exists
      LOGICAL SSTRUC, DSTRUC     ! True if source is structure
      INTEGER EMS_STATUS         ! Used for EMS calls.
      INTEGER I                  ! Temporary integer
      INTEGER NCOMP              ! How many components to copy
      INTEGER IGNORE             ! Temporary copy of STATUS
      INTEGER LEV                ! Level returned by DTA_LOCATE
      INTEGER STYPE, DTYPE       ! Type of source/destin.
      INTEGER SLEV, DLEV         ! Hierarchy level of source/destin.
      INTEGER SLASTC( DST_MAXLEV ) ! SNAME pointers before dots
      INTEGER DLASTC( DST_MAXLEV ) ! DNAME pointers before dots
      INTEGER SNDIM, DNDIM       ! Source/destin. array dimensionality
      INTEGER SELEM( DST_MAXDIM ) ! Source array element
      INTEGER DELEM( DST_MAXDIM ) ! Destination array element
      INTEGER TNDIM              ! Temporary array dimensionality
      INTEGER TDIMS( DST_MAXDIM ) ! Temporary array dimensions
      CHARACTER * ( 80 ) FSNAME  ! Upper case source name
      CHARACTER * ( 80 ) FDNAME  ! Upper case destination name
      CHARACTER * ( 80 ) DFILE   ! Lower case destination file name
      CHARACTER * ( 32 ) FDCOMP  ! Destination component name
      CHARACTER * ( DAT__SZLOC ) HDSTYP  ! HDS type of the source
      CHARACTER * ( DAT__SZLOC ) SRCLOC  ! Source locator
      CHARACTER * ( DAT__SZLOC ) DSTLOC  ! Destination locator
      CHARACTER * ( DAT__SZLOC ) SPAREN  ! Source parent locator
      CHARACTER * ( DAT__SZLOC ) DPAREN  ! Destination parent locator
      CHARACTER * ( DAT__SZLOC ) TLOC    ! Temporary component locator
      CHARACTER * ( 32 ) TNAME   ! Temporary component name

*.

*  Initialise.
      EMS_STATUS = 0
      CALL EMS_BEGIN( EMS_STATUS )
      STATUS = 0
      STYPE  = 0
      DTYPE  = 0

*  Convert the given names to upper case.
      CALL DTA_TRNAME( SNAME, FSNAME )
      CALL DTA_TRNAME( DNAME, FDNAME )

*  Derive the file name for any new HDS container file.
*  We use a lower case copy of DNAME here, which is appropriate for Unix
*  systems and does not matter on VMS.
      DFILE = DNAME
      IGNORE = ICH_DFOLD( DFILE )
      I = MAX( 1, INDEX(DFILE,' ') )
      DFILE(I:) = '.dst'

*  Analyse the names.
      CALL DTA_SPLITN( FSNAME, DST_MAXLEV, DST_MAXDIM, SLEV, SLASTC,
     :   SNDIM, SELEM, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500
      CALL DTA_SPLITN( FDNAME, DST_MAXLEV, DST_MAXDIM, DLEV, DLASTC,
     :   DNDIM, DELEM, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Extract the destination component name.
      IF ( DLEV .EQ. 1 ) THEN
         FDCOMP = FDNAME
      ELSE
         FDCOMP = FDNAME( DLASTC(DLEV-1)+2 : DLASTC(DLEV) )
      END IF

*  We can deduce whether the objects are cells or not.
      IF ( SNDIM .GT. 0 ) STYPE = STYPE + 4
      IF ( DNDIM .GT. 0 ) DTYPE = DTYPE + 4

*  Locate the objects. The destination may or may not exist. In either
*  case DTA_LOCATE's status will be 0. ?FOUND tells wether the object
*  exists, ???LOC will locate it. If it does not exist, ?PAREN will
*  locate its parent. There is one case where non-0 status can must be
*  tolerated, when the destination is a new HDS container file. On the
*  other hand, if the source does not exist, we abort at any rate.
      CALL DTA_LOCATE( FSNAME, SLEV, SLASTC, SRCLOC, SFOUND, SPAREN,
     :   LEV, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500
      IF ( .NOT. SFOUND ) THEN
         STATUS = DTA_NOTFND
         GO TO 500
      END IF
      CALL DTA_LOCATE( FDNAME, DLEV, DLASTC, DSTLOC, DFOUND, DPAREN,
     :   LEV, STATUS )
      IF ( STATUS .NE. 0 .AND. DLEV .NE. 1 ) THEN
         GO TO 500
      ELSE IF ( STATUS .NE. 0 ) THEN
         STATUS = 0
         DFOUND = .FALSE.
         DTYPE  = DTYPE + 8
      END IF

*  We can deduce whether the objects are arrays or scalars.
*  For a new destination this follows from the attribute of the source.
      CALL DAT_SHAPE( SRCLOC, DST_MAXDIM, TDIMS, TNDIM, STATUS )
      IF ( TNDIM .GT. 0 ) THEN
         STYPE = STYPE + 2
         IF ( .NOT. DFOUND ) DTYPE = DTYPE + 2
      END IF
      IF ( DFOUND ) THEN
         CALL DAT_SHAPE( DSTLOC, DST_MAXDIM, TDIMS, TNDIM, STATUS )
         IF ( TNDIM .GT. 0 ) DTYPE = DTYPE + 2
      END IF

*  We can deduce whether the objects are prmitives or structures.
*  For a new destination this follows from the attribute of the source.
      CALL DAT_STRUC( SRCLOC, SSTRUC, STATUS )
      IF ( SSTRUC ) THEN
         STYPE = STYPE + 1
         IF ( .NOT. DFOUND ) DTYPE = DTYPE + 1
      END IF
      IF ( DFOUND ) THEN
         CALL DAT_STRUC( DSTLOC, DSTRUC, STATUS )
         IF ( DSTRUC ) DTYPE = DTYPE + 1
      END IF

*  After all these HDS/DAT calls, check the status.
      IF ( STATUS .NE. 0 ) THEN
         IGNORE = STATUS
         CALL EMS_ANNUL( IGNORE )
         CALL DTA_HDSERC( STATUS )
         GO TO 500
      END IF

*  If the destination is a new HDS container file, we are not interested
*  in bits 0,1,2 of DTYPE.
      DTYPE = MIN( 8, DTYPE )

*  Now use one of the three algorithms to effect the copy.
*  -------------------------------------------------------

*  If HDS_COPY is appropriate.
      IF ( STYPE .EQ. 1 .AND. DTYPE .EQ. 8 ) THEN

*     We can copy without further checks.
         CALL HDS_COPY( SRCLOC, DFILE, FDNAME, STATUS )

*     Now we want to get hold of the file for DTA.
*     (DTA status bails out, but HDS/DAT status falls through.)
         IF ( STATUS .EQ. 0 ) THEN
            CALL DTA_ASFNAM( FDNAME, DFILE, 'OLD', 0, ' ', STATUS )
            IF ( STATUS .NE. 0 ) GO TO 500
         END IF

*  Else if DAT_COPY can be used to copy from source into parent of
*  destination.
      ELSE IF ( (.NOT.DFOUND) .AND.
     :   STYPE .LT. 4 .AND. DTYPE .EQ. STYPE ) THEN

*     This works only if the destination is new. But DAT_COPY will check
*     that.
         CALL DAT_COPY( SRCLOC, DPAREN, FDCOMP, STATUS )

*  Else if DAT_INDEX/DAT_COPY can be used to copy source component by
*  component into destination.
      ELSE IF
     :   (  (  ( STYPE .EQ. 1 .OR. STYPE .EQ. 5 ) .AND.
     :         ( DTYPE .EQ. 1 .OR. DTYPE .EQ. 5 )       ) .OR.
     :      (  STYPE .EQ. 4 .AND. DTYPE .EQ. 0          ) .OR.
     :      (  STYPE .EQ. 5 .AND. DTYPE .EQ. 8          )      ) THEN

*     Still not all operations are allowed:
*     5 -> 8 in any case: DTYPE=8 needs no further checks.
*     1 -> 1 only if destination exists: No check necessary, if
*     destination was new 1 -> 1 would have been handled in previous IF
*     clause.
*     5 -> 1 in any case: No check.
*     1 -> 5 only if destination exists: Must check.
*     5 -> 5 only if destination exists: Must check.
*     4 -> 0 only if destination is new: Must check.
         IF ( (.NOT.DFOUND) .AND. DTYPE .EQ. 5 ) THEN
            STATUS = DTA_NOTFND
            GO TO 500
         ELSE IF (  DFOUND  .AND. DTYPE .EQ. 0 ) THEN
            STATUS = DTA_EXIST
            GO TO 500
         END IF

*     If destination exists, it must be an empty structure.
*     (DTA status bails out, but HDS/DAT status falls through.)
         IF ( DFOUND ) THEN
            IF ( .NOT. DSTRUC ) THEN
               STATUS = DTA_EXIST
               GO TO 500
            END IF
            CALL DAT_NCOMP( DSTLOC, NCOMP, STATUS )
            IF ( STATUS .EQ. 0 .AND. NCOMP .GT. 0 ) THEN
               STATUS = DTA_EXIST
               GO TO 500
            END IF
         END IF

*     The algorithm itself requires the destination to exist and to have
*     been located. If necessary, create and locate it now. DTYPE can be
*     0, 1, 5 or 8, but when it is 5 we know that the destination
*     already exists.
         IF ( .NOT. DFOUND ) THEN

*        If we need a scalar.
            IF ( DTYPE .EQ. 0 .OR. DTYPE .EQ. 1 ) THEN
               CALL DAT_TYPE( SRCLOC, HDSTYP, STATUS )
               CALL DAT_NEW(  DPAREN, FDCOMP, HDSTYP, 0, 0, STATUS )
               CALL DAT_FIND( DPAREN, FDCOMP, DSTLOC, STATUS )

*        Else if we need a new HDS container file with a top level
*        structure.
            ELSE IF ( DTYPE .EQ. 8 ) THEN
               CALL DAT_TYPE( SRCLOC, HDSTYP, STATUS )
               CALL HDS_NEW( DFILE, FDNAME, HDSTYP, 0, 0,
     :            DSTLOC, STATUS )
            END IF
         END IF

*     Find out how many components there are in the source object, then
*     copy each one in turn.
         CALL DAT_NCOMP( SRCLOC, NCOMP, STATUS )
         DO 1 I = 1, NCOMP
            CALL DAT_INDEX( SRCLOC, I, TLOC,       STATUS )
            CALL DAT_NAME(    TLOC,         TNAME, STATUS )
            CALL DAT_COPY(    TLOC, DSTLOC, TNAME, STATUS )
            CALL DAT_ANNUL(   TLOC, STATUS )
 1       CONTINUE

*     If the destination locator was aquired by this routine from
*     DAT_NEW, we want to annul it again.
*     If the destination locator was aquired by this routine from
*     HDS_NEW, we want to close and re-open that file so that it enters
*     the DTA bookeeping.
*     (DTA status bails out, but HDS/DAT status falls through.)
         IF ( .NOT. DFOUND ) THEN
            IF ( DTYPE .EQ. 0 .OR. DTYPE .EQ. 1 ) THEN
               CALL DAT_ANNUL( DSTLOC, STATUS )
            ELSE IF ( DTYPE .EQ. 8 ) THEN
               CALL HDS_CLOSE( DSTLOC, STATUS )
               IF ( STATUS .EQ. 0 ) THEN
                  CALL DTA_ASFNAM( FDNAME, DFILE, 'OLD',
     :               0, ' ', STATUS )
                  IF ( STATUS .NE. 0 ) GO TO 500
               END IF
            END IF
         END IF

*  Else (no copy algorithm available).
      ELSE
         STATUS = DTA_INVCPY
         GO TO 500
      END IF

*  After all these HDS/DAT calls, check the status.
*  Any DTA status setting will have bailed out to statement 500, while
*  any HDS/DAT status will fall through to here.
      IF ( STATUS .NE. 0 ) THEN
         IGNORE = STATUS
         CALL EMS_ANNUL( IGNORE )
         CALL DTA_HDSERC( STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      IGNORE = 0
      CALL EMS_END( IGNORE )

      END
