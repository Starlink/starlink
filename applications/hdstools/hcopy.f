      SUBROUTINE HCOPY( STATUS )
*+
* Name:
*    HCOPY

* Purpose:
*    Copy HDS data objects.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hcopy in out

* ADAM Parameters:
*    INP=UNIV (Read)
*          Object being copied from, structured or primitive.
*    OUT=CHAR (Read)
*          Object being copied to - takes shape and type of input.

* Description:
*    Copies a data object from one place to another. The copy is
*    recursive, so if a structure is specified as input then
*    everything below that level will be copied to the output
*    destination.
*
*    A suitable output object will be created if necessary (replacing
*    any existing unsuitable one of the same name) but any structure
*    above the specified object must already exist. Any content of an
*    existing component is lost.

* Examples:
*    % hcopy file1 file2
*       Copy complete container file.
*
*    % hcopy file.more.asterix.grafix.coltab tab1
*       Extract an object into its own container file.
*
*    % hcopy file1.data_array file2.data_array
*       Copy the component DATA_ARRAY in file1
*       to DATA_ARRAY in file2. The DATA_ARRAY component of file2
*       will be created, but file2 must already exist.
*
*    % hcopy '[1 2 3 4 5]' file.array
*       Copy the explicit values given into a component ARRAY within file.
*
*    % hcopy '"Counts/sec"' 'file.axis(2).units'
*       A more practical example of the above.
*
*    % hcopy 'file.axis(1)' 'file.axis(2)'
*       Copy the first element of the AXIS array to the second - works for
*       structure or primitive arrays.

* Deficiencies:
*    Error reporting could be more helpful.

* Bugs:

* Authors:
*    JCMP: Jim Peden (Birmingham University)
*    RJV:  R J Vallance (Birmingham University)
*    DJA:  D J Allan (Birmingham University)
*    AJC:  A J Chipperfield (Starlink, RAL)
*    BC:   Brad Cavanagh (Joint Astronomy Centre, Hawaii)
*    TIMJ: Tim Jenness (JAC, Hawaii)

* History :
*    19-JAN-1984 (JCMP):
*       Original
*    27-JAN-1986 (JCMP):
*       V0.4-1 ADAM version
*    25-SEP-1986 (JCMP):
*       V0.5-1 Copies primitives too
*    04-MAY-1989 (RJV):
*       V1.0-1 Major rewrite
*    09-JUL-1991 (DJA):
*       V1.5-0 Copies elements of structure arrays ok now. Also
*              writes files to directories specs containing DOT
*    22-JUN-1992 (DJA):
*       V1.6-0 DAT_PAR included explicitly
*    14-APR-1993 (DJA):
*       V1.7-0 Incorrect DAT_ANNUL replaced with HDS_CLOSE
*    11-JUN-1993 (DJA):
*       V1.7-1 Fixed bug in copy to container file with length > 15
*              characters
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*    30-NOV1994 (DJA):
*       V1.8-1 Allow primitive to primitive slice copying
*    09-FEB-1995 (DJA):
*       V1.8-2 Changed definition of array same sizeness to allow
*              images to be copied into planes of cubes
*    16-AUG-1995 (DJA):
*       V1.8-3 Changed above definition again, this time to cope
*              with slices whose dims=1 precede the real dimensions
*    16-NOV-2001 (AJC):
*       V3.0-0 Complete re-write
*    02-FEB-2007 (BC):
*       V3.0-1 Close HDS locator when finished, initialise locators.
*     18-JUL-2007 (TIMJ):
*       Add CNF_PVAL for 64-bit
*     2014-11-21 (TIMJ):
*       Open input file, then close it, and then reopen it. Required
*       to stop HDS potentially complaining about the same file being
*       opened for READ and then UPDATE.
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MSG_PAR'
      INCLUDE 'CNF_PAR'

*    Status :
      INTEGER STATUS

*    External references :
      INTEGER CHR_LEN
      LOGICAL HDX_NUMERIC

*    Local Constants :
      CHARACTER*1 DOT
        PARAMETER (DOT='.')

*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC     ! input locator
      CHARACTER*(DAT__SZLOC) OLOC     ! output dataset locator
      CHARACTER*50 NAME               ! Object name (Length allows for section)
      CHARACTER*(DAT__SZLOC) FILOC    ! locator to level above output object
      CHARACTER*(DAT__SZLOC) CLOC     ! locator to level above output object
      CHARACTER*132 OUT               ! output specification string
      CHARACTER*132 REF	              ! input reference name
      CHARACTER*(DAT__SZTYP) TYPE     ! Type of the input object

      INTEGER I                       ! Loop over OUT
      INTEGER IDOT,JDOT               ! Dot positions
      INTEGER IKET                    ! ] position
      INTEGER LENOUT                  ! Length of output specifier
      INTEGER JPAR                    ! Position of ( in OUT
      INTEGER LREF                    ! Length of input reference name
      INTEGER NDIM                    ! Input dimensionality
      INTEGER DIMS(DAT__MXDIM)        ! Input dimensions
      INTEGER CLENI                   ! CHAR length
      INTEGER IPTR, OPTR              ! Pointers to mapped data
      INTEGER NVAL                    ! Number of mapped values

      LOGICAL INSLICE                 ! Input is slice or cell
      LOGICAL OUTSLICE                ! Output is slice or cell
      LOGICAL OUTTOP                  ! If output is top-level
      LOGICAL PRIM                    ! Object primitive
      LOGICAL DONE                    ! Copy done
      LOGICAL THERE                   ! Object exists?
      LOGICAL TRUNC                   ! Dummy arg for ARR_COP1C

*    Version id :
      CHARACTER*(20) VERSION
	PARAMETER(VERSION= 'HCOPY Version 3.0-1')
*.

*    Initialise Locators
      OLOC = DAT__NOLOC
      FILOC = DAT__NOLOC
      ILOC = DAT__NOLOC
      CLOC = DAT__NOLOC

*    Set MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Associate the input object file
      CALL DAT_ASSOC( 'INP', 'READ', ILOC, STATUS )

*    and get info about the input object
      CALL DAT_NAME( ILOC, NAME, STATUS )
      CALL DAT_TYPE( ILOC, TYPE, STATUS )
      CALL DAT_SHAPE( ILOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*    See if it is a slice or cell
      CALL DAT_REF( ILOC, REF, LREF, STATUS )
      INSLICE = .TRUE.
      IF( REF(LREF:LREF) .NE. ')' ) INSLICE = .FALSE.

*    Get specification of output object
      CALL PAR_GET0C( 'OUT', OUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Length of output object name
      LENOUT = CHR_LEN(OUT)

*    See if output string specifies a top-level object.
*    Find first DOT after first ">" or last "/".
*    Note that ADAM kindly converts ] to > in VMS.
      IKET = INDEX(OUT,'>')
      IF ( IKET .EQ. 0 ) THEN
         I = LENOUT
         DO WHILE ( (I.GT.0) .AND. (IKET.EQ.0) )
            IF ( OUT(I:I) .EQ. '/' ) THEN
               IKET = I
            ELSE
               I = I - 1
            END IF
         END DO
      END IF
      IDOT = INDEX(OUT(IKET+1:),DOT)+IKET

*    Is output to top-level object
      OUTTOP = .FALSE.
      IF ( IDOT .EQ. IKET ) OUTTOP = .TRUE.

*    Is output a slice or cell
      OUTSLICE = .FALSE.
      IF ( OUT(LENOUT:LENOUT) .EQ. ")" ) OUTSLICE = .TRUE.

*    HDS may not be able to open a file for write that
*    has already been opened for read, so to mitigate that
*    we close the input and will re-open it later. Only do this
*    if we are not going to be creating a new file
      IF ( (.NOT.INSLICE .AND. OUTTOP .AND. .NOT. OUTSLICE) .OR.
     :     (OUTTOP .AND. .NOT. OUTSLICE) ) THEN
*        This will be a HDS_NEW or HDS_COPY below which will
*        not require UPDATE access to the input file.
      ELSE
         CALL DAT_ANNUL( ILOC, STATUS )
         ILOC = DAT__NOLOC
         CALL DAT_CANCL('INP', STATUS )
      END IF

*    Initialise DONE
      DONE = .FALSE.

      IF ( .NOT.INSLICE .AND. OUTTOP .AND. .NOT. OUTSLICE ) THEN
*    Handle simple case of a whole object copied to a top-level object
         CALL HDS_COPY( ILOC, OUT, NAME, STATUS )
         DONE = .TRUE.

*    Otherwise get a locator to the required output object
      ELSE
         IF ( OUTTOP ) IDOT = LENOUT + 1
         JPAR = INDEX( OUT(1:IDOT-1), "(" )
         IF ( JPAR .GT. 0 ) IDOT = JPAR
         IF ( OUTTOP .AND. .NOT. OUTSLICE ) THEN
             CALL HDS_NEW( OUT, NAME, TYPE, NDIM, DIMS, FILOC, STATUS )
             CALL DAT_CLONE( FILOC, OLOC, STATUS )
         ELSE
            CALL HDS_OPEN( OUT(:IDOT-1), 'UPDATE', FILOC, STATUS )
            CALL HDS_FIND( DAT__ROOT, REF, 'READ', ILOC, STATUS )
            IF ( IDOT .EQ. JPAR ) IDOT = IDOT - 1

            IF ( OUTSLICE ) THEN
*         We don't need to delete any existing object.
*         Locate the slice or cell
               CALL HDX_FIND( FILOC, OUT(IDOT+1:), OLOC, STATUS )

            ELSE
*         Find the level above the required output object
*         Extract name of bottom-level object
                  IF ( .NOT. OUTTOP ) THEN
                     JDOT = LENOUT
                     DO WHILE ( OUT(JDOT:JDOT) .NE. DOT )
                        JDOT = JDOT - 1
                     END DO
                     NAME = OUT(JDOT+1:LENOUT)

                     CALL HDX_FIND(
     :                 FILOC, OUT(IDOT+1:JDOT-1), CLOC, STATUS )

*         We delete any existing object
*         rather than checking for correct type and size etc.
                     CALL DAT_THERE( CLOC, NAME, THERE, STATUS )
                     IF ( THERE ) THEN
                        CALL DAT_ERASE( CLOC, NAME, STATUS )
                     ENDIF

*             If input is a whole object we can use DAT_COPY
                     IF ( .NOT. INSLICE ) THEN
                        CALL DAT_COPY( ILOC, CLOC, NAME, STATUS )
                        DONE = .TRUE.

*             Otherwise we must create the object
                     ELSE
                        CALL DAT_NEW( CLOC, NAME, TYPE, NDIM, DIMS,
     :                    STATUS )
                        CALL DAT_FIND( CLOC, NAME, OLOC, STATUS )
                     ENDIF

*            Annul the intermediate locator
                     CALL DAT_ANNUL( CLOC, STATUS )

                  ENDIF
               ENDIF
            ENDIF

* We are now ether DONE or have a locator to the output object
            IF ( .NOT. DONE ) THEN
*            Is the input primitive?
               CALL DAT_PRIM( ILOC, PRIM, STATUS )
               IF ( .NOT. PRIM ) THEN
*               Copying the content of a structure
*               First ensure the destination is emptied in case it is an
*               element of a structure array.
                  CALL HDX_CLEAR( OLOC, STATUS )
                  CALL HDX_COPY( ILOC, OLOC, STATUS )

               ELSE
*                Is primitive slice or cell to be copied
*                so can't use HDS_COPY or DAT_COPY

*                For character types of any length
                  IF (TYPE(:5).EQ.'_CHAR') THEN
*                  Get character size of object
                     CALL DAT_CLEN(OLOC,CLENI,STATUS)

*                  Map object and values and make modification
                     CALL DAT_MAPV(ILOC,TYPE,'READ',IPTR,NVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OLOC,'_CHAR','WRITE',OPTR,NVAL,STATUS)
*                  Copy the data - Note truncation can't occur here
                     CALL ARR_COP1C(
     &                 NVAL,%VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(OPTR)),
     &                 TRUNC,STATUS,
     &                 %VAL(CNF_CVAL(CLENI)),%VAL(CNF_CVAL(CLENI)))

*               Treat all numeric types as double precision
                  ELSEIF (HDX_NUMERIC(ILOC)) THEN
                     CALL DAT_MAPV(
     &                 ILOC,'_DOUBLE','READ',IPTR,NVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OLOC,'_DOUBLE','WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_COP1D(NVAL,%VAL(CNF_PVAL(IPTR)),
     :                              %VAL(CNF_PVAL(OPTR)),STATUS)

*               Logicals
                  ELSEIF (TYPE.EQ.'_LOGICAL') THEN
                     CALL DAT_MAPV(
     &                 ILOC,'_LOGICAL','READ',IPTR,NVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OLOC,'_LOGICAL','WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_COP1L(NVAL,%VAL(CNF_PVAL(IPTR)),
     :                              %VAL(CNF_PVAL(OPTR)),STATUS)

                  ELSE
*               Unrecognised type
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'TYPE', TYPE )
                     CALL ERR_REP( ' ', 'Unrecognized data type ^TYPE',
     &                 STATUS )

                  ENDIF

*               Unmap the data
                  CALL DAT_UNMAP(ILOC,STATUS)
                  CALL DAT_UNMAP(OLOC,STATUS)

               ENDIF


            ENDIF

*            Close the output
            IF (OLOC .NE. DAT__NOLOC) CALL DAT_ANNUL( OLOC, STATUS )
            CALL HDS_CLOSE ( FILOC, STATUS )

      END IF

*    Tidy up
 99   CONTINUE
      IF (ILOC .NE. DAT__NOLOC) CALL DAT_ANNUL( ILOC, STATUS )
      CALL DAT_CANCL('INP', STATUS)
      END
