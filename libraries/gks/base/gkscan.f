C# IL>=a, OL>=0
      SUBROUTINE GKSCAN(IFROMY,ITOY,IETI,IETMAX,RETX,RETY,RETDX,
     :                     RETEY,IETNXT,XMIN,YMIN,XMAX,YMAX,
     :                     DOSUB,OUTSUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     This is the Scan Conversion routine, called repeatedly
*     by Fill Area, and transformed Cell Array, to deliver
*     the interior spans on a scanline, as determined from the
*     Edge Table, previously constructed by the routine GKMET
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     08/02/83  NGB   Remove Include for MYWKD.CMN
*     06/06/83  AS    Remove diagnostics
*     04/07/83  PGLS  Change KERROR
*     29/11/83  NGB   Optimised to avoid looping FROM Y..TO Y
*     29/11/83  NGB   Redundant Arguments IX,IY,IA removed
*     26/01/84  MGC   Increment X values correctly
*     15/02/84  MGC   Omit edge start points instead of end points
*     27/02/84  MGC   Omit edge end points instead of start points
*     02/03/84  MGC   Omit span end points - fill style consistency
*     20/08/85  RMK   Split IF statement which tested whether a variable was
*                     non-zero and also used it as an array index (S125).
*                     Removed local variables which are not used.
*
*  ARGUMENTS
*  ---------
*     INP IFROMY  start scanline
*     INP ITOY    end scanline (.LE. FROMY)
*     INP IETI    Edge Table last active edge Index
*     INP IETMAX  Edge Table used size
*     INP RETX    Edge Table X Start array
*     INP RETY    Edge Table Y Start array
*     INP RETDX   Edge Table Slope array
*     INP RETEY   Edge Table Y Limit array
*     INP IETNXT  Edge Table Link array for chaining Waiting and
*                 Active lists
*     INP XMIN,YMIN,XMAX,YMAX Clipping Rectangle
*     INP DOSUB   Scan Line Processing Routine
*     INP OUTSUB  W/S PolyLine or RasterOut Routine
*
      INTEGER IFROMY, ITOY, IETI, IETMAX, IETNXT(0:IETMAX)
      REAL RETX(0:IETMAX), RETY(0:IETMAX), RETDX(0:IETMAX),
     :     RETEY(0:IETMAX), XMIN, YMIN, XMAX, YMAX
      EXTERNAL DOSUB, OUTSUB
*
* Note: The Edge Tables are passed as parameters
*         so that they may be re-indexed from zero.
*
*  LOCALS
*  ------
*
      INTEGER I,J,K
      REAL AX,AY,BX,BY
      LOGICAL SORTED
*
*
*  ALGORITHM
*  ---------
*     Scan converts down to ITOY and outputs spans using <DOSUB>
*     passing down the Workstation output primitive routine for
*     the <DOSUB> to use.
*
*     -updates IETI as it proceeds.
*
*---------------------------------------------------------------------






*************
* STAGE 1
* first introduce any new edges whose Y-Span includes Y
* check the current Y-value against the starting values of new edges,
* and put one or more edges from the Edge Table into the Active Edge List
* Waiting entries are hung off IETI, -Active ones off IETNXT(0)
* IETI was the last active
      I = IETNXT(IETI)

   20 CONTINUE
      IF (I .NE. 0) THEN
         IF (RETY(I) .GE. ITOY) THEN
            RETX(I) = RETX(I) - RETDX(I)*(IFROMY-RETY(I))
            IETI = I
            I = IETNXT(I)
            GOTO 20
         ENDIF
      ENDIF

* now update all active edges hung off IETNXT(0)

      I = 0
      J = IETNXT(I)

* while still within the active section ...
   30 CONTINUE
      IF (J .NE. IETNXT(IETI)) THEN
* ...Update active edge
* -increment X in proportion to DY
         RETX(J) = RETX(J)  +  RETDX(J)*(IFROMY-ITOY)
* -check Y for expiry
         IF (RETEY(J) .GE. ITOY) THEN
* snuff it, but don't lose the waiting list
            IF (J .EQ. IETI) IETI = I
* Kill J by bypassing it
            IETNXT(I) = IETNXT(J)
            J = I
* so retaining I as the next one's predecessor
         ENDIF

         I = J
         J= IETNXT(I)
         GOTO 30

      ENDIF
*        ...still doing active ones

*
****************
* STAGE 2
* now we need to identify new set of active spans



* re-sort on X, repeating until SORTED
   40 CONTINUE
         I = 0
         J = IETNXT(I)
* J is first active
         SORTED =  .TRUE.
*  ..until proved otherwise
   50    CONTINUE
         IF (J .NE. IETNXT(IETI)) THEN
            K = IETNXT(J)
* initial order: I -> J -> K

* if J > K then we need to swap them over, and trigger another pass
            IF (K .NE. IETNXT(IETI)) THEN
* as long as both J and K are active
               IF (RETX(J) .GT. RETX(K)) THEN

*  - reorder as: I -> K -> J

                  SORTED =  .FALSE.
* the waiting list hangs off the end, so may have to be transferred
                  IF (K .EQ. IETI) IETI = J
*  ..I -> K..
                  IETNXT(I) = K
*  ..J -> L..
                  IETNXT(J) = IETNXT(K)
*  ..K -> J..
                  IETNXT(K) = J

* we now have I -> K -> J -> L
* next time round, we want to use  K,J,L as our new I,J,K
* but the next I will be taken from this J, so ...
                  J = K
*  ...rename K as J
               ENDIF
            ENDIF

            I = J
* slide I,J,K one place down the list
            J = IETNXT(J)
            GOTO 50
         ENDIF

* repeat until SORTED...
      IF (.NOT. SORTED) GOTO 40

* all X-intersects now ordered


*
*************
* STAGE 3
* finally, output spans between successive pairs of active edges
* using the supplied output routine DOSUB, passing it the parameter DOPARA

      J=IETNXT(0)
   60 CONTINUE
      IF (J .NE. IETNXT(IETI)) THEN
         AX=RETX(J)
         AY=ITOY
         J=IETNXT(J)
         IF (J .NE. IETNXT(IETI)) THEN
            BX=RETX(J) - 1.0
            BY=ITOY

* process span (if appropriate)
            IF (BX .GE. AX)
     :         CALL DOSUB(AX,AY,BX,BY,OUTSUB,XMIN,YMIN,XMAX,YMAX)
            J = IETNXT(J)
         ENDIF
         GOTO 60
      ENDIF

      END
