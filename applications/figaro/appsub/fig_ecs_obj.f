C+
C                           F I G _ E C S _ O B J
C
C  Routine name:
C     FIG_ECS_OBJ
C
C  Function:
C     Fills up the main data array for an output structure for ECHSELECT.
C
C  Description:
C     Given the order selection array produced during ECHSELECT, this
C     routine collapses the image orders into single cross-sections of
C     the output collapsed echellogram.  It can do this either for the
C     sky orders or the object orders.  It just sums the object x-sects
C     for each order, but when it collects sky x-sects, it scales each
C     to match the number of object x-sects for the corresponding order.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_ECS_OBJ (IMAGE,NX,NY,MINORD,NORD,MDELTA,OBJECT,
C                                                       ORDERS,CDATA)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) IMAGE       (Real array,ref) The original image data to
C                     be collapsed.
C     (>) NX          (Integer,ref) The first dimension of IMAGE.
C     (>) NY          (Integer,ref) The second dimension of IMAGE.
C     (>) MINORD      (Integer,ref) The lowest order number selected
C     (>) NORD        (Integer,ref) The number of orders selected
C     (>) MDELTA      (Integer,ref) The order for the order numbers -
C                     -1 => descending, +1 => ascending.
C     (>) OBJECT      (Logical,ref) True if the object data is to be
C                     collapsed, false if it is to be the sky data.
C     (>) ORDERS      (Integer array,ref) Indicates for each cross-section
C                     whether it is unselected (=0), object for order M
C                     (=M) or sky for order M (= -M).
C     (>) CDATA       (Real array,ref) The data array for the collapsed
C                     echellogram.  This has dimensions (NX,NORD).
C
C  External variables used:  None.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:
C     This is an internal routine of ECHSELECT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 21st Feb 1989.
C-
C  History:
C     29th Feb 1989.   Original version.  KS / AAO.
C+
      SUBROUTINE FIG_ECS_OBJ (IMAGE,NX,NY,MINORD,NORD,MDELTA,OBJECT,
     :                                                    ORDERS,CDATA)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL OBJECT
      INTEGER NX, NY, MINORD, NORD, MDELTA, ORDERS(NY)
      REAL    IMAGE(NX,NY), CDATA(NX,NORD)
C
C     Local variables
C
      REAL      FACTOR        ! Scaling factor for sky x-sects
      INTEGER   IORD          ! Index through x-sects of CDATA
      INTEGER   IX            ! Index through x-section elements
      INTEGER   IY            ! Index through x-sects of IMAGE
      INTEGER   ORDER         ! Order number for current x-sect
      INTEGER   TOTOBJ        ! Total number of object x-sects for order
      INTEGER   TOTSKY        ! Total number of sky x-sects for order
      LOGICAL   USE           ! Indicates this x-sect is to be used
C
C     Loop through all the x-sects in the collapsed echellogram
C
      DO IORD=1,NORD
C
C        For each x-sect, work out which order number applies,
C        clear the sky and object totals, and zero out the x-sect.
C
         TOTSKY=0
         TOTOBJ=0
         IF (MDELTA.LT.1) THEN
            ORDER=MINORD+NORD-IORD
         ELSE
            ORDER=MINORD+IORD-1
         END IF
         DO IX=1,NX
            CDATA(IX,IORD)=0.0
         END DO
C
C        Now look through each x-sect of the image, to see if it is
C        either sky or object for this order.
C
         DO IY=1,NY
            IF (ABS(ORDERS(IY)).EQ.ORDER) THEN
C
C              Correct order, now see if we want to use this.
C
               IF (OBJECT) THEN
C
C                 If we are collecting object data, we want it if
C                 this is an order x-sect, and that's all.
C
                  USE=(ORDERS(IY).GT.0)
               ELSE
C
C                 If we are collecting sky data, we want this if it is
C                 a sky cross-sect, but we also need to know the total
C                 sky and object x-sects for this order.
C
                  USE=(ORDERS(IY).LT.0)
                  IF (USE) THEN
                     TOTSKY=TOTSKY+1
                  ELSE
                     TOTOBJ=TOTOBJ+1
                  END IF
               END IF
C
C              If we want this cross-sect, add it in to the total.
C
               IF (USE) THEN
                  DO IX=1,NX
                     CDATA(IX,IORD)=CDATA(IX,IORD)+IMAGE(IX,IY)
                  END DO
               END IF
            END IF
         END DO
C
C        Finally, if we are collecting sky data, scale it to match
C        the number of x-sects in the corresponding object data.
C
         IF (.NOT.OBJECT) THEN
            FACTOR=FLOAT(TOTOBJ)/FLOAT(TOTSKY)
            DO IX=1,NX
               CDATA(IX,IORD)=CDATA(IX,IORD)*FACTOR
            END DO
         END IF
      END DO
C
      END
