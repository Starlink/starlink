      SUBROUTINE DSA_CHECK_NDF_AXIS( REF_SLOT, NDIM, DIMS, STATUS )
*+
*  Name:
*     DSA_CHECK_NDF_AXIS

*  Purpose:
*     Check an NDF's AXIS structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_CHECK_NDF_AXIS( REF_SLOT, STATUS )

*  Description:
*     This routine is to be called by DSA_CHECK_STRUCTURE just before
*     completing the check and after it is satisfied that the axes are
*     OK. This routine imposes further restrictions on the AXIS
*     structure in an NDF. The AXIS structure may be either absent, or
*     it must adhere to the following:
*     -  There are as many elements in the structure array AXIS as there
*        are dimensions in the NDF (in its data array).
*     -  Each element AXIS(i) contains at least a component DATA_ARRAY.
*     There are certainly more restrictions, but none seem relevant in
*     the sense that DSA adheres to them anyway.

*  Arguments:
*     REF_SLOT = INTEGER (Given)
*        The common table slot used by the structure to be checked.
*     NDIM = INTEGER (Given)
*        The dimensionality of the NDF (or its data array).
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the NDF (or its data array).
*     STATUS = INTEGER (Given and Returned)
*        The global status. If bad status is passed to this routine it
*        returns immediately. This routine always returns good status if
*        it runs.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     KS:  Keith Shortridge (AAO)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15 Oct 1992 (hme):
*        Original version.
*     16 Jun 1993 (hme):
*        Do not use given NDIM for size declaration.
*     13 Feb 1995 (KS):
*        Changed to fix up n-d data arrays by moving them to the extension
*        to the axis structure.
*     19 Jul 1995 (hme):
*        Do not use given NDIM for size declaration.
*     20 Jul 1995 (hme):
*        Take care of case where the 1-D axis data are there, but
*        undefined. DSA and DTA do not have the concept of undefined HDS
*        objects, so we have to invent a new DTA routine for this.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA common block
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER REF_SLOT
      INTEGER NDIM
      INTEGER DIMS( MAX_AXES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL DFINED             ! Whether 1-D array defined
      INTEGER I                  ! Loop variable
      INTEGER IGNORE             ! Ignored status
      INTEGER PNTR               ! Array pointer
      INTEGER ANDIM              ! Dimensionality of AXIS structure
      INTEGER ADIMS              ! Dimensions of AXIS structure
      INTEGER TNDIM              ! Dimensionality of AXIS structure
      INTEGER TDIMS( MAX_AXES )  ! Dimensions of AXIS structure
      CHARACTER * ( 80 ) ERROR   ! Error description - ignored
      CHARACTER * ( 80 ) NAME    ! .AXIS[i] name
      CHARACTER * ( 80 ) NAME2   ! .AXIS[i].DATA_ARRAY name
      CHARACTER * ( 80 ) NAME3   ! .AXIS[i].DATA_ARRAY[dims] name
*.
*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  If the structure is not an NDF, there is nothing to do.
      IF ( .NOT. NDF_FORMAT(REF_SLOT) ) GO TO 500

*  If the structure does not have an AXIS structure, everything is fine.
*  Oddly, this is signalled by bad status here.
      CALL DTA_CRNAM( OBJ_NAMES(REF_SLOT), 'AXIS', 0, 0,
     :   NAME, STATUS )
      CALL DTA_SZVAR( NAME, 1, ANDIM, ADIMS, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         STATUS = 0
         GO TO 500
      END IF

*  If the highest axis AXIS(NDIM) does not exist, create it. That will
*  create all others that might be missing.
      IF ( ADIMS .LT. NDIM )
     :   CALL DSA__CREATE_AXIS( REF_SLOT, NDIM, STATUS )

*  For each axis there must be a DATA_ARRAY.
      DO 1 I = 1, NDIM

*     Construct a couple of names in order to look for the data array.
*     NAME2 is the name of the axis array in the normal NDF location,
*     while NAME3 is the name of the axis array in the extension used
*     for non-NDF standard items.
         CALL DTA_CRNAM( OBJ_NAMES(REF_SLOT), 'AXIS', 1, I,
     :      NAME, STATUS )
         CALL DTA_CRNAM( NAME, 'DATA_ARRAY', 0, 0, NAME2, STATUS )
         CALL DTA_CRNAM( NAME, 'MORE.FIGARO.DATA_ARRAY', 0, 0,
     :      NAME3, STATUS )

*     First we look at the array, if any, in the extension. We are only
*     interested in one possibility here, namely that - one way or
*     another - it has been reshaped to a 1D array (this can happen through
*     a call to DSA_RESHAPE_AXIS). If this is the case, it ought to be
*     moved to the standard location, so we do that.
         STATUS = 0
         CALL DSA_ARRAY_SIZE( NAME3, MAX_AXES, TNDIM, TDIMS, ERROR,
     :      STATUS )
         IF ( ( STATUS .EQ. 0 ) .AND. ( TNDIM .EQ. 1 ) ) THEN
            CALL DTA_DLVAR( NAME2, STATUS )
            CALL DTA_RNVAR( NAME3, NAME2, STATUS )
         END IF

*     Look at the standard location. Check the size of the data array.
*     If it is multi-dimensional (shock horror!) we can't leave it here,
*     since this violates the NDF format definition. Instead, we move it
*     to the axis extension structure. This will leave the normal axis
*     structure without an array, and the next part of the code will
*     therefore replace it with an innocuous 1D array filled with pixel
*     numbers. This leaves 2 axis arrays in the structure, an n-d array
*     that Figaro can use, and a 1D array for the rest of us.
         STATUS = 0
         CALL DSA_ARRAY_SIZE( NAME2, MAX_AXES, TNDIM, TDIMS, ERROR,
     :      STATUS )
         IF ( STATUS .EQ. 0 ) THEN
            IF ( TNDIM .GT. 1 ) THEN
               CALL DSA__CREATE_AXIS_EXTRA( REF_SLOT, I, STATUS )
               STATUS = 0
               CALL DTA_RNVAR( NAME2, NAME3, STATUS )
               IF ( STATUS .NE. 0 ) GO TO 500

*           Setting STATUS non-zero makes it look as if the array wasn't
*           found in the normal structure, so a dummy will be created.
               STATUS = 1
            END IF
         END IF

*     At this point STATUS .NE. 0 is equivalent to 'the compulsory 1-D
*     array does not exist'. However, it might be that the array exists
*     and is undefined. I don't know how DSA does this, but it happens.
*     And STATUS would be zero now if that is the case.
*     We use a routine similar to DSA_ARRAY_SIZE to see if the array is
*     defined. If it isn't, we delete it now and set STATUS to 1.
         IF ( STATUS .EQ. 0 ) THEN
            CALL DSA_ARRAY_DEFINED( NAME2, DFINED, STATUS )
            IF ( .NOT. DFINED ) THEN
               CALL DTA_DLVAR( NAME2, STATUS )
               STATUS = 1
            END IF
         END IF

*     If the data array is not there, we must create it and fill it with
*     pixel numbers.
         IF ( STATUS .NE. 0 ) THEN
            CALL DTA_CRNAM( NAME, 'DATA_ARRAY', 1, DIMS(I),
     :         NAME3, STATUS )
            CALL DTA_CRVAR( NAME3, 'FLOAT', STATUS )
            IF ( STATUS .NE. 0 ) GO TO 500
            CALL DTA_MUVARF( NAME2, DIMS(I), PNTR, STATUS )
            IF ( STATUS .NE. 0 ) THEN
               IGNORE = 0
               CALL DTA_FRVAR( NAME2, IGNORE )
               GO TO 500
            END IF
            CALL GEN_NFILLF( DIMS(I), %VAL( CNF_PVAL( PNTR ) ) )
            CALL DTA_FRVAR( NAME2, STATUS )
            IF ( STATUS .NE. 0 ) GO TO 500
         END IF
 1    CONTINUE

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. 0 ) THEN

*     If anything went wrong, try to delete the whole axis structure.
*     If that also fails, we probably have no write/update access.
         STATUS = 0
         CALL DTA_DLVAR(
     :      OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS', STATUS )

*     Depending on the outcome of the tried deletion, issue a message.
         IF ( STATUS .EQ. 0 ) THEN
            CALL DSA_WRUSER(
     :         'An error occured while rectifying the axis ' //
     :         'information in ')
            CALL DSA_WRNAME( OBJ_NAMES(REF_SLOT) )
            CALL DSA_WRUSER( ' which is an NDF-type structure. ' //
     :         'The axis information has been deleted. ' //
     :         'The structure is otherwise intact.' )
            CALL DSA_WRFLUSH
         ELSE
            CALL DSA_WRUSER(
     :         'An error occured while rectifying the axis ' //
     :         'information in ')
            CALL DSA_WRNAME( OBJ_NAMES(REF_SLOT) )
            CALL DSA_WRUSER( ' which is an NDF-type structure. ' //
     :         'The axis information also could not be deleted. ' //
     :         'The structure is basically intact. ' )
            CALL DSA_WRUSER( 'However the axis structure does not' //
     :         ' comply with the specification of an NDF.' )
            CALL DSA_WRFLUSH
         END IF
      END IF
      STATUS = 0
      END
