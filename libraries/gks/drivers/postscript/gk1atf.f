*---------------------------------------------------------------------
      SUBROUTINE GK1ATF (NT, NAT, TZA)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send a series of trapezoids (parallel sides horizontal) into
*     the current path,(for Fill Areas that have been subjected to
*     trapezoid decomposition).
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP NT      - The Actual Number of Trapezoids
*     INP NAT     - Number of Trapezoids that can be put into the arrays
*     INP TZA     - Array trapedoid specifications
*
*     The array TZA contains the specification of NT trapezoids in
*     sequentially each of form:
*
*        (Upper-Left X, Lower-Left X, Upper-Right X, Lower-Right X,
*         Upper Y, Lower Y).
*
*
      INTEGER NT, NAT
      REAL TZA (6,NAT)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character, required by the buffering routine.
*  FCOORD  - Format String for each coordinate
*  FS      - Format String for the 6 trapezoid coordinates
*  I       - Temporary count variable.
*  IREM    - Dummy integer, required by the buffering routine.
*  ITZ     - Current Trapezoid
*  LCF     - Length of coordinate format
*  LCOORD  - Length of each coordinate
*  LS      - Length of string containing trapezoid coordinates
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*
*
      CHARACTER S*100, DUMMY, FCOORD*7, FS*8
      INTEGER ITZ, I, IREM, LCOORD, LCF, LS
*
*  ALGORITHM
*  ---------
*     A path is formed out trapezoids. Each trapezoid is a subpath
*     formed by the 'tz' procedure. The path is preceded by a
*     'gsave' and followed by the appropiate fill area
*     prodecure 'fasoldo' 'fapi' etc  and  'grestore'.
*
*     The coordinates specifying the each trapeziod are put into the
*     current path, using the 'tz' operater that allows each trapezoid
*     to be specified by 6 real numbers, rather than 8, since the
*     Y-coordinates are the same for both the left and right hand
*     sides of the trapezoid.
*     The format of the coordinates is chosen to save file space.
*
*  NOTE
*  ----
*     This routing does not call 'save' or 'restore' one of the
*     routines calling this one should do it.
*
*---------------------------------------------------------------------
*

      IF(NT .LE. 0) RETURN
*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)


*     Take a graphics memory snapshot, so as not to clutter it
*     with temporaries.
      CALL GKFOCO(KIOPB,'gsave',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Select the format of the coordinates to be sent
      CALL GK1ASF (6*NT,TZA,LCOORD,LCF,FCOORD)
      LS =  6*LCOORD
      FS= '(6'//FCOORD(2:LCF)//' '
*
*     Send Trapezoids to current path - but not any of 0 height
      DO 10 ITZ=1,NT
        IF(NINT(100.0*TZA(5,ITZ)) .GT. NINT(100.0*TZA(6,ITZ)))THEN
           WRITE(S,FS) (TZA(I,ITZ), I=1,6)
           CALL GKFOCO(KIOPB,S(1:LS)//' tz ',IREM)
        ENDIF
  10  CONTINUE
*
*
*     Call the fasoldo procedure to do solid fill.
*
      IF(KWFAIS(KWKIX).EQ.GSOLID)THEN
         CALL GKFOCO(KIOPB,' fasoldo', IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)
      ELSE

*
*     Deal with pattern here.
*
        CALL GK1APA
      ENDIF

*
*     End Fill Area - restore graphics state
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'grestore',IREM)

      END
