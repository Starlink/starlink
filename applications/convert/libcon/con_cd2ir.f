      SUBROUTINE CON_CD2IR( NCOLS, NROWS, ARRAY, LINE, IMDES, ERR,
     :                      STATUS )
*+
*  Name:
*     CON_CD2IR

*  Purpose:
*     Copies an image array to an output IRAF image.
*     
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_CD2IR( NCOLS, NROWS, ARRAY, LINE, IMDES, ERR, STATUS )

*  Description:
*     This routine transfers a two-dimensional array to an output IRAF
*     image using the subroutine IMPL2R() from the IRAF IMFORT
*     subroutine library.
*
*     It copies a line at a time so that dynamic memory allocation
*     could be used in the calling routine, so that images of virtually
*     any size can be catered for without excessive resources.
*
*     If an error occurs when writing to the IRAF image, the routine
*     should set the STATUS to something useful. Instead, the IRAF
*     error indicator is passed back up to the calling routine which
*     then checks it.

*  Arguments:
*     NCOLS = INTEGER (Given)
*        The number of columns in the data array.
*     NROWS = INTEGER (Given)
*        The number of rows in the data array.
*     ARRAY( NCOLS, NROWS ) = REAL (Given)
*        The data array to be copied to the IRAF image file.
*     IMDES = INTEGER (Given)
*        The image descriptor obtained from the call to to IMFORT
*        imopen() routine in the calling routine.
*     ERR = INTEGER (Given and Returned)
*        The IRAF error reporting integer.
*     LINE( NCOLS ) = REAL (Returned)
*        Work array to enable a line of the input image to be copied
*        at a time.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The IRAF image file must be open for write access.

*  External Routines Used:
*     IRAF IMFORT subroutine library:
*        IMPL2R()

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*
*     {enter_new_authors_here}

*  History:
*     21-AUG-1992 (RAHM):
*        Original version.
*     1992 September 29 (MJC):
*        Tidied, made to conform to SGP/16.  Performed the copy for
*        the whole array rather than just a line, to improve the
*        efficiency. Renamed from LINE_COPY.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOLS              ! Number of columns
      INTEGER NROWS              ! Number of rows
      INTEGER IMDES              ! Image descriptor 

      REAL ARRAY( NCOLS, NROWS ) ! The entire image

*  Arguments Given and Returned:
      INTEGER ERR                ! The IRAF error indicator.

*  Arguments Returned:
      REAL LINE( NCOLS )         ! The array to contain each line of
                                 ! the image in turn.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for all lines.
      DO J = 1, NROWS

*  Copy the current line from the data array into the dynamic memory
*  allocated in the calling routine.
         DO I = 1, NCOLS
            LINE( I ) = ARRAY( I, J )
         END DO

*  Use the IRAF imfort routine IMPL2R() to transfer the line of pixels
*  from the dynamic memory to the IRAF image.
         CALL IMPL2R( IMDES, LINE, J, ERR )

*  Exit immediately if something has gone wrong.
         IF ( ERR .NE. 0 ) GOTO 999
      END DO

  999 CONTINUE

      END
