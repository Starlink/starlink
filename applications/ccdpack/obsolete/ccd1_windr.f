         SUBROUTINE CCD1_WINDR ( ID, MEMID, XSIZE, YSIZE, XDIM, YDIM,
     :                           DEPTH, XOFF, YOFF, ARRAY, WORK, ISTAT )
*+
*  Name:
*     CCD1_WINDR

*  Purpose:
*     Displays an array of INTEGER data in the current IDI window.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_WINDR( ID, MEMID, XSIZE, YSIZE, XDIM, YDIM, DEPTH,
*                      XOFF, YOFF, ARRAY, WORK, ISTAT )

*  Description:
*     This routine display an array of INTEGER data in an IDI display.
*     The position of the array is determined by it's size and offset
*     from the memory origin. This routine performs any clipping which
*     is required before displaying the data.

*  Arguments:
*     ID = INTEGER (Given)
*        The display identifier
*     MEMID = INTEGER (Given)
*        The memory identifier
*     XSIZE = INTEGER (Given)
*        The X size of the IDI display.
*     YSIZE = INTEGER (Given)
*        The Y size of the IDI display.
*     XDIM = INTEGER (Given)
*        The X size of the input data.
*     YDIM = INTEGER (Given)
*        The Y size of the input data.
*     DEPTH = INTEGER (Given)
*        The data depth.
*     XOFF = INTEGER (Given)
*        Offset of the lower corner of the array (window coordinates).
*     YOFF = INTEGER (Given)
*        Offset of the lower corner of the array (window coordinates).
*     ARRAY( XDIM, YDIM ) = INTEGER (Given)
*        The data to be written into the IDI memory.
*     WORK( XDIM, YDIM ) = INTEGER (Given and Returned)
*        Workspace to hold the data if it is clipped.
*     ISTAT = INTEGER (Given and Returned)
*        The IDI status.

*  Notes:
*     -  This routine does not use the STARLINK inherited status
*     stratedy, IDI rules are used instead
*     -  This routine uses a load direction of 0 (top to bottom).
*     and a packing factor of one (one item per integer).

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'IDI_ERR'          ! IDI error codes.

*  Arguments Given:
      INTEGER ID
      INTEGER MEMID
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER XDIM
      INTEGER YDIM
      INTEGER DEPTH
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ARRAY( XDIM, YDIM )

*  Arguments Given and Returned:
      INTEGER WORK( * )

*  Status:
      INTEGER ISTAT             ! IDI status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER II                 ! Index into workspace
      INTEGER J                  ! Loop variable
      INTEGER JJ                 ! Index into workspace
      INTEGER XBOT               ! Lower bounds of section
      INTEGER XDIMA              ! Dimension of section
      INTEGER XOFFA              ! Offset of transfer window
      INTEGER XSTART             ! Index in array of first value
      INTEGER XTOP               ! Upper bounds of section
      INTEGER YBOT               ! Lower bounds of section
      INTEGER YDIMA              ! Dimension of section
      INTEGER YOFFA              ! Offset of transfer window
      INTEGER YSTART             ! Index of array of first value
      INTEGER YTOP               ! Upper bounds of section

*.

*  Determine if the transfer window means that the data needs clipping
*  before writing to the memory.
      IF ( XOFF .LT. 0  .OR.  YOFF .LT. 0  .OR.
     :     XOFF + XDIM .GT. XSIZE  .OR.  YOFF + YDIM .GT. YSIZE ) THEN

*  The data needs clipping in X and possibly in Y. This means that we
*  need to extract a region into workspace to ensure data vectorisation.

*  Determine the bounds of the region to extract from the input ARRAY.
*  First the offset from the 1,1 position 
         IF ( XOFF .LT. 0 ) THEN
            XBOT = ABS( XOFF ) + 1
            XOFFA = 0
         ELSE
            XBOT = 1
            XOFFA = XOFF
         END IF
         IF ( YOFF .LT. 0 ) THEN
            YBOT = ABS( YOFF ) + 1
            YOFFA = 0
         ELSE
            YBOT = 1
            YOFFA = YOFF
         END IF

*  Now the upper bounds
         IF ( XOFF + XDIM .GT. XSIZE ) THEN
            XTOP = XSIZE - XOFF
         ELSE
            XTOP = XDIM
         END IF
         IF ( YOFF + YDIM .GT. YSIZE ) THEN
            YTOP = YSIZE - YOFF
         ELSE
            YTOP = YDIM
         END IF

*  Set the size of the workspace array bounds
         XDIMA = XTOP - XBOT + 1
         YDIMA = YTOP - YBOT + 1

*  Extract the data. Use spans of size the first dimension to vectorise
*  the input to the array correctly.
         JJ = 0            
         DO 1 J = YBOT, YTOP
            JJ = JJ + 1
            II = 0
            DO 2 I = XBOT, XTOP
               II = II + 1
               WORK( ( JJ - 1 ) * XDIMA + II ) = ARRAY( I, J )
 2          CONTINUE
 1       CONTINUE
                
*  Set up the transfer window.
         CALL IIMSTW( ID, MEMID, 0, XDIMA, YDIMA, DEPTH, XOFFA, YOFFA,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
                
*  And write in the data.
         XSTART = 0
         YSTART = 0
         CALL IIMWMY( ID, MEMID, WORK, XDIMA * YDIMA, DEPTH, 1, XSTART,
     :                YSTART, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
      ELSE      
                
*  Nothing to do just get on with it quickly.
*  Set the transfer window.
         CALL IIMSTW( ID, MEMID, 0, XDIM, YDIM, DEPTH, XOFF, YOFF,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
                
*  And write in the data.
         XSTART = 0
         YSTART = 0
         CALL IIMWMY( ID, MEMID, ARRAY, XDIM * YDIM, DEPTH, 1,
     :                XSTART, YSTART, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
      END IF    
                
*  Exit label.  
 99   CONTINUE  
                
      END
* $Id$
