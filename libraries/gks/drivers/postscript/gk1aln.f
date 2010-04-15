*------------------------------------------------------------------------
      SUBROUTINE GK1ALN(N,X,Y)
*-----------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to PostScript external file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character, required by the buffering routine.
*  FC      - Format of coordinates
*  I, J    - Temporary count variables.
*  ICUT    - Maximum number of points in a short polyline.
*  IREM    - Dummy integer, required by the buffering routine.
*  LC      - Length of formatted coordinates
*  LFC     - Length of coordinate format code
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Parameters
      INTEGER    ICUT
      PARAMETER (ICUT = 5)
*
      INTEGER IREM, I, J, LC, LFC, LC1, LC2, LFC1, LFC2
      CHARACTER S*25, DUMMY, FC*7, FC1*7, FC2*7

*  ALGORITHM
*  ---------
*     Depending on number of Polyline points call appropriate PS procedure.
*     For long polylines select coordinate format to save filespace.
* --------------------------------------------------------------------
*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Treat short polylines separately
*
      IF (N .LE. ICUT) THEN
*        Short - just send the coordinates in selected format
         CALL GK1ASF(N,X,LC1,LFC1,FC1)
         CALL GK1ASF(N,Y,LC2,LFC2,FC2)
         DO 300 I=N, 1, -1
            WRITE(S,FC1(1:LFC1)) X(I)
            CALL GKFOCO(KIOPB, S(1:LC1), IREM)
            WRITE(S,FC2(1:LFC2)) Y(I)
            CALL GKFOCO(KIOPB, S(1:LC2), IREM)
  300    CONTINUE
*        Number of points and the procedure to deal with them:
         WRITE(S,120) N
  120    FORMAT(I3, ' pls')
         CALL GKFOCO(KIOPB, S(1:7), IREM)

      ELSE

*        Long - send the coordinates in selected format as arrays:
         CALL GKFOCO(KIOPB, ' s[', IREM)

         CALL GK1ASF(N,X,LC,LFC,FC)
         DO 100 I=1, N
            WRITE(S,FC(1:LFC)) X(I)
            CALL GKFOCO(KIOPB, S(1:LC), IREM)
  100    CONTINUE
*
         CALL GKFOCO(KIOPB, '][', IREM)
*
         CALL GK1ASF(N,Y,LC,LFC,FC)
         DO 200 I=1, N
            WRITE(S,FC(1:LFC)) Y(I)
            CALL GKFOCO(KIOPB, S(1:LC), IREM)
  200    CONTINUE
*        Now call the plong procedure
         CALL GKFOCO(KIOPB, ']plong', IREM)

      END IF

      END
