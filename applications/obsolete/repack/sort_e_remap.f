*+SORT_E_REMAP Adjust the dimension of a mapped vector(expand or shrink)
      SUBROUTINE SORT_E_REMAP( LOC, TYPE, IDIM, IPTR, EXPAND )
* Input:
      CHARACTER*(*)	LOC, TYPE	! Locator to input vector

      INTEGER           IDIM            ! Required length for vector
      LOGICAL           EXPAND          ! Expand request

* Export:
      INTEGER           IPTR       	! Pointer to remapped vector
*-

*   Check status - return if bad
      IF (STATUS .NE. 0) RETURN

      CALL DAT_UNMAP( LOC, STATUS )
      CALL DAT_ALTER( LOC, 1, IDIM, STATUS )

      IF (EXPAND) THEN
        CALL DAT_MAP( LOC, TYPE, 'UPDATE', 1, IDIM, IPTR, STATUS )

      END IF

      IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_E_REMAP'
      ENDIF

      END

