      SUBROUTINE KPG1_FLCOR( PNAME, NDIM, POSCOD, NPOINT, IPCO,
     :                         LBND, UBND, STATUS )
*+
*  Name:
*     KPG1_FLCOx
 
*  Purpose:
*     Obtains a list of co-ordinates from a text file.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_FLCOx( PNAME, NDIM, POSCOD, NPOINT, IPCO, LBND, UBND,
*                      STATUS )
 
*  Description:
*     This routine obtains a list of n-dimensional co-ordinates for a
*     series of positions.  The text file should contain on each line
*     a set of free-format co-ordinates that defines a position, except
*     for comment lines denoted by a hash or shriek in column 1.
*     The file is opened using the supplied parameter.  A pass is made
*     through the file to see how many records it contains.  Dynamic
*     workspace is reserved to contain the co-ordinates, and the file
*     is then rewound.  Another pass is made through the file to read
*     the co-ordinate values which are then stored in the workspace.
*     Pointers to the workspace are returned.  This workspace should be
*     released by calling PSX_FREE when it is no longer needed.
 
*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Name of the parameter with which to associate the text file.
*     NDIM = INTEGER (Given)
*        The number of co-ordinate dimensions to read from the text
*        file.
*     POSCOD( NDIM ) = INTEGER (Given)
*        The column numbers of the co-ordinate information in order
*        x, y, z, etc.  These must be positive.
*     NPOINT = INTEGER (Returned)
*        The number of co-ordinate sets specified in the text file.
*     IPCO = INTEGER (Returned)
*        A pointer to workspace of type _REAL and size NDIM by NPOINT
*        holding the NDIM co-ordinates of each point.
*     LBND( NDIM ) = ? (Returned)
*        The lower bounds of the input data, i.e. the minimum value for
*        each co-ordinate.
*     UBND( NDIM ) = ? (Returned)
*        The upper bounds of the input data, i.e. the maximum value for
*        each co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each of the real or double-precision
*     data types: replace "x" in the routine name by R or D
*     respectively, as appropriate.  The returned co-ordinate array
*     will have this type, and the supplied bounds arrays should also
*     have the specified data type.
*     -  The number of points read from the file is reported at the
*     normal reporting level.
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     11-NOV-1993 (DSB):
*        Original version based on MJC's code in SEGMENT.
*     1995 April 11 (MJC):
*        Made generic, n-dimensional, and renamed from KPS1_FLXYR.
*        Added POSCOD and bounds arguments, and message reporting.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'FIO_ERR'          ! FIO_ error constants
 
*  Arguments Given:
      CHARACTER * ( * ) PNAME
      INTEGER NDIM
      INTEGER POSCOD( NDIM )
 
*  Arguments Returned:
      INTEGER NPOINT
      INTEGER IPCO
      REAL LBND( NDIM )
      REAL UBND( NDIM )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! Text buffer mostly for reading file
      INTEGER COUNT              ! Start and end line in file
      LOGICAL CMPLET             ! The file has been completely read?
      INTEGER FD                 ! File descriptor
      INTEGER I                  ! Loop counter
      INTEGER NCHAR              ! Number of characters read from file
 
*.
 
*  Initialise the returned value of NPOINT to indicate that no polygon
*  has been obtained.
      NPOINT = 0
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Obtain the input text file containing the co-ordinates of the polygon
*  vertices.
      CALL MSG_BLANK( STATUS )
      CALL FIO_ASSOC( PNAME, 'READ', 'LIST', 0, FD, STATUS )
 
*  If an error occurred while obtaining the co-ordinate list...
      IF ( STATUS .NE. SAI__OK ) THEN
 
*  If a null value was obtained, annul the error.
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
 
*  Abort.
         GO TO 999
      END IF
 
*  Determine the number of points in the list.  Loop round until an
*  error encountered (this will happen when the end of file is reached,
*  if not before).
      DO WHILE ( STATUS .EQ. SAI__OK )
 
*  Read a record of the text file.
         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )
 
*  Remove leading blanks.
         CALL CHR_LDBLK( BUFFER )
 
*  Watch for blank lines or comment lines.  A hash or shriek in the
*  first column indicates a comment line.  In such cases the line can
*  be ignored.
         IF ( BUFFER( 1 : 1 ) .NE. '!' .AND.
     :        BUFFER( 1 : 1 ) .NE. '#' .AND.
     :        BUFFER .NE. ' ' .AND.
     :        STATUS .EQ. SAI__OK ) NPOINT = NPOINT + 1
 
      END DO
 
*  If an end-of-file error has been reported, annul it.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
 
*  Report an error if the file contains no co-ordinates.
      IF ( NPOINT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_FLCOx_NOVER', 'The co-ordinate file '/
     :     /'$PNAME contains no co-ordinates.', STATUS )
         GO TO 999
      END IF
 
*  Rewind the file.
      CALL FIO_RWIND( FD, STATUS )
 
*  Obtain dynamic memory space for the positions.
      CALL PSX_CALLOC( NDIM * NPOINT, '_REAL', IPCO, STATUS )
 
*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
 
*  Initialise the bounds.
      DO I = 1, NDIM
         LBND( I ) = VAL__MAXR
         UBND( I ) = VAL__MINR
      END DO
 
*  Read positions from the input list, starting at the beginning and
*  reading them all.
      COUNT = 1
      CALL KPG1_RFCOR( FD, NDIM, NPOINT, POSCOD, COUNT, %VAL( IPCO ),
     :                   LBND, UBND, CMPLET, STATUS )
 
*  Report the number of positions read and the file name.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_SETI( 'NXY', COUNT )
      CALL MSG_SETC( 'FN', PNAME )
      BUFFER = ' ^NXY pixel positions read in from $'//PNAME//'.'
      CALL MSG_OUTIF( MSG__NORM, 'NREAD', BUFFER, STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
 
*  Close the file.
      CALL FIO_CLOSE( FD, STATUS )
 
*  Arrive here if an error has occurred.
 999  CONTINUE
 
*  If an error has occurred, set the number of vertices in the polygon
*  to zero, and attempt to cancel the parameter and release the
*  workspace.  This is done within a new error reporting environment
*  because PSX_FREE does nothing if an error status exists on entry (in
*  contrast to most other `tidying up' routines).
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         NPOINT = 0
         CALL FIO_CANCL( PNAME, STATUS )
         CALL PSX_FREE( IPCO, STATUS )
         CALL ERR_END( STATUS )
      END IF
 
      END
