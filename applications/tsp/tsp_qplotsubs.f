      SUBROUTINE TSP_QPLTXLIMITS(X,STRT,FIN,XN,STATUS)
*+
*
*  T S P _ Q P L T X L I M I T S
*
*  QPLOT command
*
*  Determine the range of X values (MJD times) in the data to plot
*
*  (>) X       (Double)  Original array of X data
*  (!) STRT    (Integer) Start index
*  (!) FIN     (Integer) Finish index
*  (<) XN      (Double)  New array of X data for restricted range
*  (!) STATUS  (Integer) status value
*
*  Jeremy Bailey  28/2/1988
*
*  Modified:
*     11/12/1991
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      INTEGER STRT,FIN,STATUS
      DOUBLE PRECISION X(*),XN(*)

*  Local variables
      DOUBLE PRECISION XS,XE
      INTEGER ISTRT,IFIN
      INTEGER I
      LOGICAL WHOLE

      IF (STATUS .EQ. SAI__OK) THEN

*  Get WHOLE parameter (plot all of data)
          CALL PAR_GET0L('WHOLE',WHOLE,STATUS)
          IF (.NOT. WHOLE) THEN

*  If not WHOLE prompt for X range using start and end of data as defaults

              CALL PAR_DEF0D('XSTART',X(STRT),STATUS)
              CALL PAR_GET0D('XSTART',XS,STATUS)
              CALL PAR_DEF0D('XEND',X(FIN),STATUS)
              CALL PAR_GET0D('XEND',XE,STATUS)
          ELSE
              XS=X(STRT)
              XE=X(FIN)
          ENDIF

*  Search through array for indices that correspond to these limits

          ISTRT = STRT
          IFIN = FIN
          DO I=STRT,FIN-1
              IF (X(I) .LE. XS .AND. X(I+1) .GE. XS) THEN
                  ISTRT=I
              ENDIF
              IF (X(I) .LE. XE .AND. X(I+1) .GE. XE) THEN
                  IFIN=I+1
              ENDIF
          ENDDO

*  Fill the output X array with the appropriate values

          STRT = ISTRT
          FIN = IFIN
          DO I=1,FIN-STRT+1
              XN(I)=X(I+STRT-1)
          ENDDO
      ENDIF
      END



      SUBROUTINE TSP_QPLTGETITEM(LOC,CHAN,STRT,FIN,IPTR,QPTR,UPTR,
     :   VPTR,IEPTR,QEPTR,UEPTR,VEPTR,STATUS)
*+
*
*   T S P _ Q P L T G E T I T E M
*
*   QPLOT command
*
*   Get the data for the Stokes parameters present in the data
*   A pointer of zero is returned if the Stokes parameter does not exist
*
*   Parameters
*
*  (>) LOC  (Integer)   Top Level locator
*  (>) CHAN (Integer)   Channel to use
*  (>) STRT (Integer)   Start index for data
*  (>) FIN  (Integer)   End index for data
*  (<) IPTR (Integer)   Pointer to I data (real array)
*  (<) QPTR (Integer)   Pointer to Q data (real array)
*  (<) UPTR (Integer)   Pointer to U data (real array)
*  (<) VPTR (Integer)   Pointer to V data (real array)
*  (<) IEPTR (Integer)  Pointer to I variance
*  (<) QEPTR (Integer)  Pointer to Q variance
*  (<) UEPTR (Integer)  Pointer to U variance
*  (<) VEPTR (Integer)  Pointer to V variance
*  (!) STATUS (Integer) status value
*
*  Jeremy Bailey    28/2/1988
*
*  Modified:
*     11/12/1991
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      INTEGER CHAN
      INTEGER STRT,FIN,IPTR,QPTR,UPTR,VPTR,STATUS
      INTEGER IEPTR,QEPTR,UEPTR,VEPTR

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,ILOC,ILOC2,QLOC,QLOC2,ULOC,ULOC2
      CHARACTER*(DAT__SZLOC) VLOC,VLOC2,IELOC,QELOC,UELOC,VELOC

*  These locators are in common so that they can be accessed by
*  TSP_QPLTUNMAPITEM

      COMMON /GET_ITEM/ILOC2,QLOC2,ULOC2,VLOC2,IELOC,QELOC,UELOC,VELOC

*  Limits of slice
      INTEGER UPPER(2), LOWER(2)
      INTEGER STAT
      IF (STATUS .EQ. SAI__OK) THEN

*  Intensity Data  -  Map the Intensity Array

*  Set limits of slice. The data is extracted from the selcted channel
*  and only the range in X between STRT and FIN is included

          UPPER(1) = CHAN
          UPPER(2) = FIN
          LOWER(1) = CHAN
          LOWER(2) = STRT

*  Map the slice - set the pointer to zero if unsuccesful

          CALL TSP_MAP_SLICE(LOC,2,LOWER,UPPER,'READ',IPTR,
     :        ILOC2,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              IPTR = 0
              CALL ERR_REP('MSG','Error Accessing Data ^STATUS',STATUS)
          ENDIF

*  Intensity Variance -

          CALL ERR_MARK
          CALL TSP_MAP_VSLICE(LOC,2,LOWER,UPPER,'READ',IEPTR,
     :        IELOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              IEPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF

*  Get the Q stokes parameter

          CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)

*  Q Stokes parameter  -  Map the Q array

          CALL TSP_MAP_SLICE(QLOC,2,LOWER,UPPER,'READ',QPTR,
     :        QLOC2,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              QPTR = 0
          ENDIF

*  Q Stokes variance

          CALL TSP_MAP_VSLICE(QLOC,2,LOWER,UPPER,'READ',QEPTR,
     :        QELOC,STATUS)
          CALL DAT_ANNUL(QLOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              QEPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF

*  Get the U stokes parameter

          CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)

*  U Stokes parameter  -  Map the U array

          CALL TSP_MAP_SLICE(ULOC,2,LOWER,UPPER,'READ',UPTR,
     :        ULOC2,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              UPTR = 0
          ENDIF

*  U Stokes variance

          CALL TSP_MAP_VSLICE(ULOC,2,LOWER,UPPER,'READ',UEPTR,
     :        UELOC,STATUS)
          CALL DAT_ANNUL(ULOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              UEPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF

*  Get the V stokes parameter

          CALL TSP_GET_STOKES(LOC,'V',VLOC,STATUS)

*  V Stokes parameter  -  Map the V array

          CALL TSP_MAP_SLICE(VLOC,2,LOWER,UPPER,'READ',VPTR,
     :        VLOC2,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              VPTR = 0
          ENDIF

*  V Stokes variance

          CALL TSP_MAP_VSLICE(VLOC,2,LOWER,UPPER,'READ',VEPTR,
     :        VELOC,STATUS)
          CALL DAT_ANNUL(VLOC,STATUS)
          IF (STATUS .NE. SAI__OK) THEN
              VEPTR = 0
              CALL ERR_ANNUL(STATUS)
          ENDIF
          CALL ERR_RLSE

       ENDIF
       END





      SUBROUTINE TSP_QPLTUNMAPITEM(LOC,STATUS)
*+
*
*   T S P _ Q P L T U N M A P I T E M
*
*   QPLOT command  -   Unmap items
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

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,ILOC2,QLOC2,ULOC2,VLOC2
      CHARACTER*(DAT__SZLOC) IELOC,QELOC,UELOC,VELOC
      COMMON /GET_ITEM/ILOC2,QLOC2,ULOC2,VLOC2,IELOC,QELOC,UELOC,VELOC

*  Unmap each item
      CALL TSP_UNMAP(ILOC2,STATUS)
      CALL TSP_UNMAP(QLOC2,STATUS)
      CALL TSP_UNMAP(ULOC2,STATUS)
      CALL TSP_UNMAP(VLOC2,STATUS)
      CALL TSP_UNMAP(IELOC,STATUS)
      CALL TSP_UNMAP(QELOC,STATUS)
      CALL TSP_UNMAP(UELOC,STATUS)
      CALL TSP_UNMAP(VELOC,STATUS)
      STATUS = SAI__OK
      END

