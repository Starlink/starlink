      SUBROUTINE GRGFIL(TYPE, NAME)
*+
*     - - - - - - - -
*       G R G F I L    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
* This routine encsapsulates the algorithm for finding the PGPLOT
* run-time data files.
*
* 1. The binary font file: try the following in order:
*     file specified by PGPLOT_FONT
*     file "grfont.dat" in directory specified by PGPLOT_DIR
*                       (with or without '/' appended)
*     file "grfont.dat" in directory ../etc/ relative to the
*                       directories on PATH.
*     file "grfont.dat" in directory /usr/local/pgplot/
*
* 2. The color-name database: try the following in order:
*     file specified by PGPLOT_RGB
*     file "rgb.txt" in directory specified by PGPLOT_DIR
*                       (with or without '/' appended)
*     file "grfont.dat" in directory ../etc/ relative to the
*                       directories on PATH.
*     file "rgb.txt" in directory /usr/local/pgplot/
*
*    Given
*        TYPE    c    either 'FONT' or 'RGB' to request the corresponding file.
*
*    Returned
*        NAME    c    receives the file name.
*
*   D.L.Terrett  Starlink  Feb 1995 (after Tim Pearson)
*+
      IMPLICIT NONE

      CHARACTER*(*) TYPE, NAME

      CHARACTER*(*) DEFDI1, DEFDI2, DEFFNT, DEFRGB
      PARAMETER  (DEFDI1='/../etc/')
      PARAMETER  (DEFDI2='/usr/local/pgplot/')
      PARAMETER  (DEFFNT='grfont.dat')
      PARAMETER  (DEFRGB='rgb.txt')
      CHARACTER*255 FF
      CHARACTER*2048 PATH
      CHARACTER*16 DEFLT
      INTEGER I, L, LD, I1, I2, LP
      LOGICAL TEST, DEBUG
      INTEGER CHR_LEN

* Is debug output requested?
      CALL GRGENV('DEBUG', FF, L)
      DEBUG = L.GT.0

* Which file?
      IF (TYPE.EQ.'FONT') THEN
         DEFLT = DEFFNT
         LD = LEN(DEFFNT)
      ELSE IF (TYPE.EQ.'RGB') THEN
         DEFLT = DEFRGB
         LD = LEN(DEFRGB)
      ELSE
         CALL GRWARN('Internal error in routine GRGFIL')
      END IF

* Translate PATH
      CALL GRGENV('PATH', PATH, LP)

* Try each possibility in turn.
      I = 1
      I1 = 1
      IF (LP.EQ.0) THEN
         I2 = 0
      ELSE
         I2 = -1
      ENDIF
   10 CONTINUE
         IF (I.EQ.1) THEN
            CALL GRGENV(TYPE, FF, L)
         ELSE IF (I.EQ.2) THEN
            CALL GRGENV('DIR', FF, L)
            IF (L.GT.0) THEN
               FF(L+1:) = DEFLT
               L = L+LD
            END IF
         ELSE IF (I.EQ.3) THEN
            CALL GRGENV('DIR', FF, L)
            IF (L.GT.0) THEN
               FF(L+1:L+1) = '/'
               FF(L+2:) = DEFLT
               L = L+1+LD
            END IF
         ELSE
            IF (I2.NE.0) THEN
               I2 = INDEX(PATH(I1:LP),':')
               IF (I2.EQ.0) THEN
                  FF = PATH(I1:LP) // DEFDI1 // DEFLT
               ELSE
                  FF = PATH(I1:I1+I2-2) // DEFDI1 // DEFLT
                  I1 = I1 + I2
               ENDIF
               L = CHR_LEN(FF)
            ELSE
               FF = DEFDI2//DEFLT
               L = LEN(DEFDI2)+LD
               I = -1
            ENDIF
         ENDIF
         IF (L.GT.0) THEN
         IF (DEBUG) CALL GRWARN('Looking for '//FF(:L))
            INQUIRE (FILE=FF(:L), EXIST=TEST)
            IF (TEST) THEN
               NAME = FF(:L)
               RETURN
            ELSE IF (DEBUG) THEN
               CALL GRWARN('WARNING: file not found')
            END IF
         END IF
         I = I + 1
      IF (I.NE.0) GO TO 10

* Failed to find the file.
      NAME = DEFLT
      END
