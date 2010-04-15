
      SUBROUTINE TSP_PHSGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,
     :   VPTR,STATUS)
*+
*
*   T S P _ P H S G E T I T E M
*
*   Get the data for the Stokes parameters present in the data
*   A pointer of zero is returned if the Stokes parameter does not exist
*
*   For each item the slice of the data array corresponding to the selected
*   channel (Y value) and the specified range of indices is mapped and
*   a pointer to it is returned.
*
*  (>) LOC  (Integer)   Top Level locator
*  (>) CHAN (Integer)   Channel to use
*  (>) STRT (Integer)   Start index for data
*  (>) FIN  (Integer)   End index for data
*  (<) IPTR (Integer)   Pointer to I data (real array)
*  (<) QPTR (Integer)   Pointer to Q data (real array)
*  (<) UPTR (Integer)   Pointer to U data (real array)
*  (<) VPTR (Integer)   Pointer to V data (real array)
*  (!) STATUS (Integer) status value
*
*   Jeremy Bailey    28/2/1988
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      INTEGER CHAN
      INTEGER STRT,FIN,IPTR,QPTR,UPTR,VPTR,STATUS

*  HDS locators (in common)
      CHARACTER*(DAT__SZLOC) LOC,ILOC2,QLOC,QLOC2,ULOC,ULOC2
      CHARACTER*(DAT__SZLOC) VLOC,VLOC2
      COMMON /GET_ITEM/ ILOC2,QLOC2,ULOC2,VLOC2

*  Local variables
      INTEGER UPPER(2), LOWER(2)
      INTEGER STAT
      IF (STATUS .EQ. SAI__OK) THEN

*  Intensity Data  -  Map the Intensity Array

*  Determine limits of slice

          UPPER(1) = CHAN
          UPPER(2) = FIN
          LOWER(1) = CHAN
          LOWER(2) = STRT
          CALL TSP_MAP_SLICE(LOC,2,LOWER,UPPER,'READ',IPTR,
     :             ILOC2,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              IPTR = 0
              CALL ERR_REP('MSG','Error Accessing Data ^STATUS',STATUS)
          ENDIF

*  Q Stokes parameter  -  Map the Q array

          CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
          CALL TSP_MAP_SLICE(QLOC,2,LOWER,UPPER,'READ',QPTR,
     :             QLOC2,STATUS)
          CALL DAT_ANNUL(QLOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              QPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF

*  U Stokes parameter  -  Map the U array

          CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)
          CALL TSP_MAP_SLICE(ULOC,2,LOWER,UPPER,'READ',UPTR,
     :             ULOC2,STATUS)
          CALL DAT_ANNUL(ULOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              UPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF

*  V Stokes parameter  -  Map the V array

          CALL TSP_GET_STOKES(LOC,'V',VLOC,STATUS)
          CALL TSP_MAP_SLICE(VLOC,2,LOWER,UPPER,'READ',VPTR,
     :             VLOC2,STATUS)
          CALL DAT_ANNUL(VLOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              VPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF
       ENDIF
       END





      SUBROUTINE TSP_PHSUNMAPITEM(LOC,STATUS)
*+
*
*   T S P _ P H S U N M A P I T E M
*
*   PHASEPLOT command  -   Unmap items
*
*   Unmap all the arrays mapped by TSP_PHSGETITEM. The array locators
*   allocated by TSP_PHSGETITEM are kept in a common block so that they
*   can be accessed by this routine as well.
*
*   Parameters
*
*   (>)  LOC    (Char)    HDS locator to top level object
*   (!)  STATUS (Integer) Status value
*
*   Jeremy Bailey   28/2/1988
*
*     Modified:
*         11/12/1991     Tidy up and improve commenting
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      INTEGER STATUS
      CHARACTER*(DAT__SZLOC) LOC,ILOC2,QLOC2,ULOC2,VLOC2
      COMMON /GET_ITEM/ ILOC2,QLOC2,ULOC2,VLOC2

*  Unmap each item
      CALL TSP_UNMAP(ILOC2,STATUS)
      CALL TSP_UNMAP(QLOC2,STATUS)
      CALL TSP_UNMAP(ULOC2,STATUS)
      CALL TSP_UNMAP(VLOC2,STATUS)
      STATUS = SAI__OK
      END

