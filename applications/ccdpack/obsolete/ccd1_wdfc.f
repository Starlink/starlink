      SUBROUTINE CCD1_WDFC( LIST, PREFIX, USEPRO, PROTEC, CONTIN, TRAIL,
     :                      FD, FTYPES, GIDIN, NNDF, VALID, POINT1,
     :                      POINT2, NFRAME, STATUS )
*+
*  Name:
*     CCD1_WDFC

*  Purpose:
*     Writes a command to debias all "debiassable" frames.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_WDFC( LIST, PREFIX, USEPRO, PROTEC, CONTIN, TRAIL, FD,
*                      FTYPES, GIDIN, NNDF, VALID, POINT1, POINT2,
*                      NFRAME, STATUS )

*  Description:
*     This routine locates all the frame TYPES within FTYPES which
*     require debiassing (TARGET, FLAT, DARK and FLASH). The names of
*     these NDFs are then accessed and written into a file "debias{n}"
*     (where n is a unique number for this application run).
*     A command is then written to the file FD
*
*       debias IN=^debias{n} OUT=*//TRAIL
*
*     which is terminated with a continuation character. The command is
*     written in a style suitable for the type of script using the
*     arguments PREFIX, USEPRO, PROTEC and CONTIN.

*  Arguments:
*     LIST = LOGICAL (Given)
*        Whether or not the list of NDFs to be debiassed should be
*        reported via the logging system or not.
*     PREFIX = CHARACTER * ( * ) (Returned)
*        The prefix for commands.
*     USEPRO = LOGICAL (Returned)
*        Whether quoting protection should be used (in case special
*        characters are present).
*     PROTEC = CHARACTER * ( * ) (Returned)
*        The protection character (if used).
*     CONTIN = CHARACTER * ( * ) (Returned)
*        The continuation character for this script type.
*     TRAIL = CHARACTER * ( * ) (Given)
*        The trailing modification element to be used when forming the
*        output file names.
*     FD = INTEGER (Given)
*        The FIO file descriptor of the script.
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        The frame and filter types of the input NDFs. (1,*) are the
*        frame types, (2,*) are the filters.
*     GIDIN = INTEGER (Given)
*        IRG identifier of the input group of NDF names.
*     NNDF = INTEGER (Given)
*        The number of NDFs.
*     VALID( NNDF ) = LOGICAL (Given)
*        Mask of flags indicating which NDFs are valid (i.e. those which
*        should be processed, the others are ignored).
*     POINT1( NNDF ) = INTEGER (Given and Returned)
*        Workspace for pointer to frames of given type.
*     POINT2( NNDF ) = INTEGER (Given and Returned)
*        Workspace for pointer to frames of all types. On exit this will
*        contain the indices of the frames which are marked for
*        debiassing.
*     NFRAME = INTEGER (Returned)
*        Number of debiassable frames located.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1993 (PDRAPER):
*        Original version.
*     27-JAN-1994 (PDRAPER):
*        Added LIST argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message system parameters
      INCLUDE 'IRH_PAR'          ! IRH parameters
      INCLUDE 'FIO_PAR'          ! FIO parameters

*  Arguments Given:
      LOGICAL LIST
      CHARACTER * ( * ) PREFIX
      LOGICAL USEPRO
      CHARACTER * ( * ) PROTEC
      CHARACTER * ( * ) CONTIN
      CHARACTER * ( * ) TRAIL
      INTEGER FD
      INTEGER NNDF
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      INTEGER GIDIN
      LOGICAL VALID( NNDF )

*  Arguments Given and Returned:
      INTEGER POINT1( NNDF )
      INTEGER POINT2( NNDF )

*  Arguments Returned:
      INTEGER NFRAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing blanks

*  Local Variables:
      CHARACTER * ( 6 ) DTYPES( 4 ) ! The frames types which may be
                                    ! debiassed.
      CHARACTER * ( FIO__SZFNM ) TEMP ! Name of temporary file (indirection)
      CHARACTER * ( MSG__SZMSG ) MESS ! Output buffer
      CHARACTER * ( IRH__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( IRH__SZNAM ) OUT ! Output NDFs specification
      INTEGER IAT                ! Position in string
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NFOUND             ! Number of frames of a type found
      INTEGER FDTMP              ! Temporary file FIO descriptor
      LOGICAL OPEN               ! Temporary file is opened

*  Local Data:
      DATA DTYPES / 'TARGET', 'FLAT', 'FLASH', 'DARK'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First get a list of pointers to all the frames of "debiassable" type.
*  This includes "TARGET", "FLAT", "FLASH" and "DARK" (and may include
*  the extended flatfield types at some time).
      NFRAME = 0
      DO 1 I = 1, 4
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, DTYPES( I ),
     :                    POINT1, NFOUND, STATUS )

*  If any have been located then copy there pointers into the main
*  stack.
         IF ( NFOUND .GT. 0 ) THEN
            DO 2 J = 1, NFOUND
               NFRAME = NFRAME + 1
               POINT2( NFRAME ) = POINT1( J )
 2          CONTINUE
         END IF
 1    CONTINUE

*  If we havn't found any frames of a debiassable type, set status and
*  exit.
      IF ( NFRAME .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_WDFC1',
     :   '  CCD1_WDFC: Unable to locate any frames to debias.', STATUS )
         GO TO 99
      ELSE

*  Get a name for the indirection file.
         CALL CCD1_TMPNM( 'debias', TEMP( 2: ), STATUS )

*  Open the file.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         OPEN = .FALSE.
         CALL CCD1_OPFIO( TEMP( 2: ), 'WRITE', 'LIST', 0, FDTMP,
     :                    STATUS )
         IF ( STATUS .EQ. SAI__OK ) OPEN = .TRUE.
         MESS = IRH__COMC // ' List of names used by debias'
         CALL FIO_WRITE( FDTMP, MESS( :CHR_LEN( MESS ) ), STATUS )

*  Now extract the name of the NDFs and write these into the file.
         DO 3 I = 1, NFRAME
            NDFNAM = ' '
            CALL IRH_GET( GIDIN, POINT2( I ), 1, NDFNAM, STATUS )
            IAT = CHR_LEN( NDFNAM )
            CALL FIO_WRITE( FDTMP, NDFNAM( :IAT ), STATUS )

*  List these to the logging system if asked.
            IF ( LIST ) THEN
               CALL MSG_SETC( 'NDFNAM', NDFNAM( :IAT )  )
               CALL CCD1_MSG( ' ', '    ^NDFNAM', STATUS )
            END IF
 3       CONTINUE

*  Close the temporary file.
         CALL FIO_CLOSE( FDTMP, STATUS )

*  Now create the command for the script file. First add the routine
*  name. Note we're using the standard characterisations for the script
*  type.
         MESS = PREFIX//'debias'
         IAT = CHR_LEN( MESS ) + 2

*  Now the input and output file specifiers.
         TEMP( 1: 1 ) = IRH__INDC
         OUT = '*'//TRAIL
         CALL CCD1_ADKEY( 'IN', TEMP, USEPRO, PROTEC, MESS, IAT,
     :                    STATUS )
         CALL CCD1_ADKEY( 'OUT', OUT, USEPRO, PROTEC, MESS, IAT,
     :                    STATUS )

*  Finally add the continuation character.
         MESS( IAT: ) = CONTIN

*  Now write out the message.
         CALL FIO_WRITE( FD, MESS( :CHR_LEN( MESS ) ), STATUS )
      END IF

*  Exit label.
 99   CONTINUE
      END
* $Id$
