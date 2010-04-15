*+  LCCELL - link contourable cells in a 2-dimensional image
*
      SUBROUTINE LCCELL( DIM1, DIM2, ARRAY, DIML, DIMU, DIMO1, DIMO2,
     :                   NCONT, CONT, SLIST, CLIST, LINK, FLAG,
     :                   STATUS )
*
*    Description :
*
*     The routine searches a rectangular area of a 2-dimensional image
*     and identifies all the cells (groups of 4 adjacent pixels) which
*     are "contourable".
*
*     Cells are deemed to be contourable if none of the pixels in
*     the cell is "bad" and if at least one of the contour levels
*     supplied exceeds the lowest pixel value in the cell but does not
*     exceed the highest pixel value.
*
*     A set of pointers is returned, which form a series of linked
*     lists gathering together all the cells associated with each
*     single contour level and all the cells associated with more than
*     one contour level.  A set of flag values is also returned.
*
*     This is not a user-level routine and does not perform any
*     validation on the argument values supplied.  It is optimised for
*     efficient execution.
*
*    Notes :
*
*     (i)  Finding an image positon at which to start following a
*     contour is a time-critical part of contouring, especially if the
*     image is only sparsely covered by contour lines.  It is
*     inefficient to perform an individual search through the entire
*     image for every contour level, so this routine identifies all the
*     contourable cells in an image in a single pass.
*
*     (ii)  So that the contourable cells can be retrieved, they are
*     associated together via a series of linked lists (one for each
*     contour level).  Cells which are contourable at only a single
*     level ("simple" cells) are linked into one of these lists, from
*     which they may be retrieved without referring to any other cells
*     in the image.
*
*     (iii)  Some cells ("crowded" cells) will be contourable at more
*     than one level.  There are many possible combinations of contour
*     levels at which a crowded cell may be contourable, so it is not
*     feasible to provide a linked list for each possibility.  All
*     crowded cells are therefore linked into a single list and must be
*     considered potentially contourable at any level.  Crowded cells
*     must be individually tested when contouring is actually
*     performed, although the number of such cells is usually very
*     small.
*
*     (iv)  The majority of cells are usually blank (i.e. most of the
*     image does not contain contour lines).  The number of cells which
*     must be considered is therefore greatly reduced once this routine
*     has been run.
*
*     (v)  Note that the criterion of "contourability" does not mean
*     that a contour line will necessarily pass through each contourable
*     cell.  For instance, one or more pixel values in a cell may be
*     equal to the contour level, in which case the contour runs along
*     the edge of the cell (i.e. through a pixel centre) and might
*     later be associated with one of the neighbouring cells.  Such
*     cases are rare, however.
*
*     (vi)  This routine also initialises a set of flag values
*     associated with cells in the image.  One value (FLAG = 1) is used
*     to identify cells which contain "bad" pixels.  These cells do not
*     appear in the linked lists, but may be encountered when
*     contour-following is performed.  The flag value indicates that
*     these cells are not to be contoured and that contour-following
*     should cease at that point.
*
*     (vii)  A second value (FLAG = 0) is used to indicate that a
*     contourable cell has not yet been contoured.  For efficiency,
*     this value is only set for simple contourable cells.  The flag
*     value may be altered once the cell has been contoured to prevent
*     it being considered twice.  Crowded cells are not flagged in this
*     routine because they may be contoured repeatedly (at different
*     contour levels) and the flag value must therefore be
*     re-initialised each time.
*
*    Invocation :
*
*     CALL LCCELL( DIM1, DIM2, ARRAY, DIML, DIMU, DIMO1, DIMO2, NCONT,
*                  CONT, SLIST, CLIST, LINK, FLAG, STATUS )
*
*    Parameters :
*
*     DIM1 = INTEGER (given)
*         The first dimension of the 2-dimensional image ARRAY being
*           contoured.
*
*     DIM2 = INTEGER (given)
*         The second dimension of the 2-dimensional image ARRAY being
*           contoured.
*
*     ARRAY( DIMS1, DIMS2 ) = REAL (given)
*         The 2-d data array.
*
*     DIML( 2 ) = INTEGER (given)
*         The lower dimension bounds for the image ARRAY region to be
*           processed (the ARRAY indices of the bottom-left pixel in
*           the bottom-left image cell to be considered).
*
*     DIMU( 2 ) = INTEGER (given)
*         The upper dimension bounds for the image ARRAY region to be
*           processed (the ARRAY indices of the bottom-left pixel in
*           the top-right image cell to be considered).
*
*     DIMO1 = INTEGER (given)
*         The first dimension of the output arrays LIST and FLAG.
*           It must have a value at least equal to
*           DIMU( 1 ) - DIML( 1 ).
*
*     DIMO2 = INTEGER (given)
*         The second dimension of the output arrays LIST and FLAG.
*           It must have a value at least equal to
*           DIMU( 2 ) - DIML( 2 ).
*
*     NCONT = INTEGER (given)
*         The number of contour levels.
*
*     CONT( NCONT ) = REAL (given)
*         The list of contour levels.  These must be given in
*           non-decreasing order.
*
*     SLIST( NCONT ) = INTEGER (returned)
*         A list of pointers into the start of the linked list of
*           simple contourable image cells for each contour level.
*           SLIST( I ) returns the index of the (vectorised) LINK array
*           element for the first simple contourable cell associated
*           with contour level I (or zero if the list is empty).
*
*     CLIST = INTEGER (returned)
*         A pointer into the start of the linked list of crowded image
*           cells.  CLIST returns the index of the (vectorised) LINK
*           array element for the first crowded cell (or zero if the
*           list is empty).
*
*     LINK( DIMO1, DIMO2 ) = INTEGER (returned)
*         A set of pointers linking the contourable image cells
*           together into a series of separate linked lists.  The array
*           element LINK( X, Y ) is associated with the image cell
*           whose bottom-left corner pixel is:
*
*              ARRAY( DIML( 1 ) + X - 1, DIML( 2 ) + Y - 1)
*
*           Each LINK element which is associated with a contourable
*           cell contains the array index of another LINK element, which
*           is associated with the next cell in the same linked list
*           (or zero if it is the last element in the list).  The array
*           indices returned are single numbers which regard LINK as a
*           1-dimensional (vectorised) array.  Those LINK elements
*           which are not associated with contourable cells are not
*           altered by this routine.
*
*     FLAG( DIMO1, DIMO2 ) = INTEGER (returned)
*         A array of flag values which are associated with image cells
*           in the same manner as the LINK array.  Values are returned
*           in this array as follows:
*
*              1 => The cell contains a "bad" pixel and should not be
*                   contoured.
*              0 => The cell is contourable and simple.
*
*           FLAG elements corresponding to all other cells are not
*           altered by this routine.
*
*     STATUS = INTEGER (given)
*         Inherited error status.  If this has an error value when the
*           routine is invoked, then an immediate return will be made
*           without performing any processing.
*
*    Method :
*
*     - Check inherited error status.
*
*     - Initialise the start-of-list pointers and the current contour
*       level.
*
*     - Scan through the rectangular region of the image specified,
*       extracting the pixel values from the four corners of each cell.
*
*     - Find the lowest and highest pixel values in the cell.
*
*     - Test these values to determine if the cell contains any "bad"
*       pixels.  If so, then set the appropriate FLAG value and do not
*       consider the cell further.
*
*     - Search up or down through the contour levels, as appropriate
*       (starting at the current level), until a level is found which
*       exceeds the lowest pixel value but does not exceed the highest
*       pixel value.  If no such level exists, then the cell is not
*       contourable and is not considered further.
*
*     - Once a suitable contour level has been found, test the levels
*       immediately above and/or below (if they exist) to determine if
*       there is more than one suitable level.  If so, then the cell
*       is crowded, otherwise it is simple.
*
*     - Set an appropriate FLAG value for simple contourable cells.
*
*     - Link contourable cells into the appropriate linked list,
*       according to the current contour level and whether they are
*       simple or crowded.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     R.F. Warren-Smith (DUVAD::RFWS)
*
*    History :
*
*     26-SEP-1988:  Original version (DUVAD::RFWS)
*
*    Type Definitions :

      IMPLICIT NONE            ! No implicit typing

*    Global Constants :

      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'PRM_PAR'        ! PRIMDAT primitive data constants

*    Import :

      INTEGER DIM1, DIM2       ! ARRAY dimension sizes
      REAL ARRAY( DIM1 * DIM2 )! Image array (vectorised)
      INTEGER DIML( 2 )        ! Lower dimension bounds of ARRAY region
      INTEGER DIMU( 2 )        ! Upper dimension bounds of ARRAY region
      INTEGER DIMO1, DIMO2     ! Dimension sizes of the output arrays
      INTEGER NCONT            ! Number of contour levels
      REAL CONT( NCONT )       ! Array of contour levels

*    Export :

      INTEGER SLIST( NCONT )   ! Start-of-list pointers for simple cells
      INTEGER CLIST            ! Start-of-list pointer for crowded cells
      INTEGER LINK( DIMO1 * DIMO2 )
                               ! Array of cell-to-cell links
                               ! (vectorised)

      INTEGER FLAG( DIMO1 * DIMO2 )
                               ! Array of cell flag values (vectorised)

*    Status :

      INTEGER STATUS           ! Inherited error status

*    Local variables :

      LOGICAL SIMPLE           ! Whether a cell is simple
      INTEGER Y                ! Y (2-dimensional) ARRAY index of the
                               ! bottom-left pixel in a cell
      INTEGER IPIXBL           ! (Vectorised) ARRAY index of the
                               ! bottom-left pixel in a cell
      INTEGER IPIXTL           ! Ditto, top-left pixel
      INTEGER CELL             ! (Vectorised) LINK and FLAG array index
                               ! for a cell
      INTEGER ICELL            ! Value taken by CELL for the first
                               ! cell in an image row
      INTEGER ICONT            ! Counter to index contour levels
      REAL PIXBL               ! Data value of bottom-left cell pixel
      REAL PIXBR               ! Ditto, bottom-right pixel
      REAL PIXTR               ! Ditto, top-right pixel
      REAL PIXTL               ! Ditto, top-left pixel
      REAL LOPIX               ! Lowest pixel value in a cell
      REAL HIPIX               ! Highest pixel value in a cell

*-

*   Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*   Initialise the start-of-list pointers.
      DO ICONT = 1, NCONT
         SLIST( ICONT ) = 0
      END DO
      CLIST = 0


*   Initialise the "current" contour level.
      ICONT = 1


*   Scan through each row of cells in the rectangular region of the
*   image being considered.  This is done in reverse (i.e. moving in
*   the - Y direction) as it results in a more pleasing plotting order
*   for the contours.
      DO Y = DIMU( 2 ), DIML( 2 ), - 1


*   Identify the first cell in the current row of the panel (ICELL is
*   the index of the first cell's entry in the vectorised LINK and FLAG
*   arrays).
         ICELL = DIMO1 * ( Y - DIML( 2 ) ) + 1


*   Calculate the starting indices for identifying the bottom-left and
*   top-left pixels of the cell in the vectorised image ARRAY.
         IPIXBL = DIM1 * ( Y - 1 ) + ( DIML( 1 ) - 1 )
         IPIXTL = IPIXBL + DIM1


*   Scan through each cell in the current row of the rectangular image
*   region being considered (i.e. moving in the + X direction).
*   Increment the vectorised image ARRAY pixel indices accordingly.
         DO CELL = ICELL, ICELL + ( DIMU( 1 ) - DIML( 1 ) )
            IPIXBL = IPIXBL + 1
            IPIXTL = IPIXTL + 1


*   Extract the four corner pixel values for the cell from the image
*   ARRAY.
            PIXBL = ARRAY( IPIXBL )
            PIXBR = ARRAY( IPIXBL + 1 )
            PIXTL = ARRAY( IPIXTL )
            PIXTR = ARRAY( IPIXTL + 1 )


*   Identify the lowest and highest pixel values in the current cell
*   (LOPIX and HIPIX).  The following nested tests determine these with
*   the minimum number of intercomparisons and assignments by explicitly
*   considering all possible combinations of pixel values.
            IF ( PIXBL .GE. PIXBR ) THEN

               IF ( PIXTR .GE. PIXTL ) THEN

                  IF ( PIXBL .GE. PIXTR ) THEN
                     HIPIX = PIXBL
                  ELSE
                     HIPIX = PIXTR
                  END IF

                  IF ( PIXBR .GE. PIXTL ) THEN
                     LOPIX = PIXTL
                  ELSE
                     LOPIX = PIXBR
                  END IF

               ELSE

                  IF ( PIXBL .GE. PIXTL ) THEN
                     HIPIX = PIXBL
                  ELSE
                     HIPIX = PIXTL
                  END IF

                  IF ( PIXBR .GE. PIXTR ) THEN
                     LOPIX = PIXTR
                  ELSE
                     LOPIX = PIXBR
                  END IF

               END IF

            ELSE

               IF ( PIXTR .GE. PIXTL ) THEN

                  IF ( PIXBR .GE. PIXTR ) THEN
                     HIPIX = PIXBR
                  ELSE
                     HIPIX = PIXTR
                  END IF

                  IF ( PIXBL .GE. PIXTL ) THEN
                     LOPIX = PIXTL
                  ELSE
                     LOPIX = PIXBL
                  END IF

               ELSE

                  IF ( PIXBR .GE. PIXTL ) THEN
                     HIPIX = PIXBR
                  ELSE
                     HIPIX = PIXTL
                  END IF

                  IF ( PIXBL .GE. PIXTR ) THEN
                     LOPIX = PIXTR
                  ELSE
                     LOPIX = PIXBL
                  END IF

               END IF

            END IF


*   If any pixel in the current cell is "bad", then either LOPIX or
*   HIPIX will have the "bad" data value.  (This assumes that the "bad"
*   value lies at one extreme of the number range, but does not assume
*   which end this will be.)  The following test checks for a "bad"
*   cell - on the assumption that these are relatively rare, this is
*   quicker than testing each pixel individually.  Note that since
*   numerical constants are involved, only a single comparison is
*   needed at run time.

*   ...if the "bad" value is the highest number:
            IF ( ( ( VAL__BADR .EQ. NUM__MAXR ) .AND.
     :            ( HIPIX .EQ. VAL__BADR ) )
     :          .OR.

*   ...if the "bad" value is the lowest number:
     :          ( ( VAL__BADR .EQ. NUM__MINR ) .AND.
     :            ( LOPIX .EQ. VAL__BADR ) ) ) THEN


*   If the cell is "bad", then indicate this in the FLAG array.
               FLAG( CELL ) = 1
            ELSE


*   If the current contour level lies above the highest pixel value in
*   the current cell, then move down through the contour levels.  Note
*   that ICONT is not reset between cells.  This is because adjacent
*   cells will tend to have similar pixel values.  This arrangement
*   therefore ensures that the current contour level is optimised for
*   the next cell.
               IF ( CONT( ICONT ) .GT. HIPIX ) THEN

*   ...this loop decrements ICONT until CONT( ICONT ) does not exceed
*      the highest pixel value.  If the bottom contour is reached before
*      this happens, then the cell contains no contours and an exit is
*      made to statement 3.  A similar exit is also made (for the same
*      reason) if the next contour down does not exceed the lowest
*      pixel value.  Note that the loop termination test is put at the
*      end, since its outcome before the first iteration is already
*      known.
    1             CONTINUE
                     IF ( ICONT .EQ. 1 ) GO TO 3
                     IF ( CONT( ICONT - 1 ) .LE. LOPIX ) GO TO 3
                     ICONT = ICONT - 1
                  IF ( CONT( ICONT ) .GT. HIPIX ) GO TO 1


*   At this point, the current contour level is the highest one which
*   does not exceed the highest pixel value.  If there are no lower
*   contours, then the cell is simple.  The cell is also simple if the
*   next contour down does not exceed the lowest pixel value -
*   otherwise it is crowded.
                  IF ( ICONT .EQ. 1 ) THEN
                     SIMPLE = .TRUE.
                  ELSE
                     SIMPLE = CONT( ICONT - 1 ) .LE. LOPIX
                  END IF


*   If the current contour level does not exceed the lowest pixel value
*   in the current cell, then move up through the contour levels.
               ELSE IF ( CONT( ICONT ) .LE. LOPIX ) THEN

*   ...this loop mirrors the one above, incrementing ICONT until
*      CONT( ICONT ) exceeds the lowest pixel value.  A similar exit is
*      made to statement 3 if it is discovered that the cell cannot
*      contain a contour.  Note that ICONT is altered earlier in this
*      loop; this ensures that if the current cell is not contourable
*      (the most common case) and subsequent cells also have similar
*      pixel values, then the first loop (i.e. the one above) will
*      execute in future - this is slightly faster since it involves
*      one less test.
    2             CONTINUE
                     IF ( ICONT .EQ. NCONT ) GO TO 3
                     ICONT = ICONT + 1
                     IF ( CONT( ICONT ) .GT. HIPIX ) GO TO 3
                  IF ( CONT( ICONT ) .LE. LOPIX ) GO TO 2


*   At this point, the current contour level is the lowest one which
*   exceeds the lowest pixel value.  If there are no higher contours,
*   then the cell is simple.  The cell is also simple if the next
*   contour up exceeds the highest pixel value - otherwise it is
*   crowded.
                  IF ( ICONT .EQ. NCONT ) THEN
                     SIMPLE = .TRUE.
                  ELSE
                     SIMPLE = CONT( ICONT + 1 ) .GT. HIPIX
                  END IF


*   If the current contour level already lies within the range of
*   pixel values in the cell, then further tests must be made to see if
*   it is the only such level.  This is done by testing the levels
*   immediately above and below, having first ascertained that they
*   exist.  The following tests explicitly consider all possibilities.
               ELSE

*   ...if the current level is the bottom one, then test the level
*      above, if it exists:
                  IF ( ICONT .EQ. 1 ) THEN
                     IF ( ICONT .EQ. NCONT ) THEN
                        SIMPLE = .TRUE.
                     ELSE
                        SIMPLE = CONT( ICONT + 1 ) .GT. HIPIX
                     END IF

*   ...if the current level is not the bottom one, then test the level
*      below:
                  ELSE
                     IF ( CONT( ICONT - 1 ) .GT. LOPIX ) THEN
                        SIMPLE = .FALSE.
                     ELSE

*   ...if still simple after testing the level below, then test the
*      level above, if it exists:
                        IF ( ICONT .EQ. NCONT ) THEN
                           SIMPLE = .TRUE.
                        ELSE
                           SIMPLE = CONT( ICONT + 1 ) .GT. HIPIX
                        END IF
                     END IF
                  END IF
               END IF


*   If the current cell only contains a single contour level (i.e. it
*   is simple), then set an appropriate FLAG value and link it into the
*   appropriate list of simple cells.
               IF ( SIMPLE ) THEN
                  FLAG( CELL ) = 0
                  LINK( CELL ) = SLIST( ICONT )
                  SLIST( ICONT ) = CELL


*   If the cell contains more than one contour level, then link it into
*   the list of crowded cells.
               ELSE
                  LINK( CELL ) = CLIST
                  CLIST = CELL
               END IF


*   This is an exit point to which execution is directed as soon as it
*   is discovered that a cell contains no contours.
    3          CONTINUE


*   End of "the cell does not contain bad pixels" condition.
            END IF


*   End of "scan through each cell in the current row" loop.
         END DO


*   End of "scan through each row of cells" loop.
      END DO


*   Exit routine.
      END
