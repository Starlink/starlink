*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*   SUBROUTINE CDRAW
*
*   Extract from DIPSO MAIN, converted to subroutine.
*   Purpose:   to draw a continuum, using cursor or file
*   input, with linear or spline interpolation
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE CDRAW
     : (PARAMS,
     : WAVE, FLUX, ASIZE1, NPOINT,
     : WORK, NWORK,
     : POINTR,
     : BREAKS, MAXBRK, NBREAK,
     : TITLE, WORV,
     : STKSZE, XSTACK,
     : MAXSTK, STKNPT,
     : NPLOTS, NONSTK, DEVTYP, MARK, IPAL,
     : LBOX, POLTST, HSTTST, MRKTST, ROTST,
     : SUBCHK,COLOUR,CURSOR)

*

       IMPLICIT NONE
       INCLUDE 'SAE_PAR'
       INTEGER STATUS
*

       INTEGER ASIZE1, NPOINT, IHX
       INTEGER SLEN
       INTEGER STKSZE
       INTEGER NWORK, IERR
       INTEGER MAXSTK
       INTEGER MAXBRK, NBREAK
       INTEGER NPLOTS, NONSTK, DEVTYP, IPAL
       INTEGER BREAKS(MAXBRK)
       INTEGER STKNPT(MAXSTK)

       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL XSTACK(STKSZE)
       REAL WORK(NWORK)
       REAL WORV, MARK
       REAL DIVX,DIVY
       CHARACTER*(*) TITLE
       CHARACTER*(*) PARAMS

       LOGICAL LBOX, POLTST, HSTTST, MRKTST, ROTST
       LOGICAL FIRSTCURSOR
       LOGICAL COLOUR,CURSOR
*
*
*

       INTEGER MODE, ISYMB, I, NWORKK
       INTEGER POINTR(MAXSTK)

       REAL RMODE, X1, X2, GVAL, BVAL

       LOGICAL OK, SUBCHK
       LOGICAL FILEIN

      COMMON /DIVFACTOR/ DIVX, DIVY

*

       SUBCHK = .TRUE.

*

*  Remove leading blanks from supplied command parameter string.
       CALL SSTRIP (PARAMS)

*  If the first parameter is not a file name, we shall use the cursor
*  to get points.
       IF (PARAMS(1:).EQ.' '
     : .OR.PARAMS(1:).EQ.'0'
     : .OR.PARAMS(1:).EQ.'1') THEN

*  Abort if there is no cursor available or if there is no plot.
          IF (.NOT.CURSOR
     :    .OR.NPLOTS.EQ.0)   THEN
              WRITE (*,
     :        '(''   CDRAW:  no plot available'')')
              SUBCHK = .FALSE.
              RETURN
          ENDIF

*  Indicate that we are not reading data from a file.
          FILEIN = .FALSE.

*  Get the required mode (0 or 1).
          RMODE = 0.0
          CALL DECODE ('CDRAW',PARAMS,0,1,RMODE,'Mode ',OK)
          IF (.NOT.OK) THEN
             OK = .TRUE.
             SUBCHK = .FALSE.
             RETURN
          ENDIF
          MODE = NINT(RMODE)

*  If mode 1 was requested, there should be something on the stack to
*  define the X positions at which to sample the fit. Abort if there
*  isn't.
          IF (MODE.NE.0) THEN
             IF (NONSTK.LE.0) THEN
                WRITE (*,
     :          '(''   CDRAW 1: no stack data to establish grid;''/
     :            ''     join-the-dots retained'')')
                MODE = 0
             ENDIF
          ENDIF

*  If a anything other than "1" or "2" was supplied as the first
*  parameter on the command line, assume it is an NDF name.
       ELSE

*  Read the specified NDF into the current arrays.
          STATUS = SAI__OK
          CALL READ( 'CDRAW', PARAMS, WORV, TITLE, STATUS )

*  If an error has occurred, flush it and return.
          IF( STATUS .NE.0 ) THEN
             CALL ERR_FLUSH( STATUS )
             SUBCHK = .FALSE.
             RETURN
          ENDIF

*  Indicate that we are reading values from a file.
          FILEIN = .TRUE.

*  Use mode 1 unless there is nothin on the stack in which case use mode
*  0.
          MODE = 1
          IF (NONSTK.LE.0) THEN
             WRITE (*,
     :       '(''   CDRAW:  no stack data to establish grid;''/
     :         ''     no spline interpolation'')')
             MODE = 0
          ENDIF

       ENDIF

*  If using the cursor get the artificial data points.
       IF (.NOT.FILEIN) THEN
          NPOINT = 1
          FIRSTCURSOR = .TRUE.
          CALL SGSCURSE(I,WAVE(1),FLUX(1),FIRSTCURSOR)
          FIRSTCURSOR = .FALSE.
*         IF (MARK.LT.5) THEN
*            ISYMB = MARK + 1
*         ELSE
*            ISYMB = 1
*         ENDIF
          ISYMB = 3
          IF(COLOUR)CALL PPALET (IPAL)
          IF (MODE.EQ.0) THEN
             CALL POINTSYM(WAVE(1)/divx,FLUX(1)/divy,ISYMB)
          ELSE
             CALL NOBOX
             CALL POINTSYM(WAVE(1)/divx,FLUX(1)/divy,ISYMB)
          ENDIF
          CALL SGSCURSE(I,X1,X2,FIRSTCURSOR)
   90     IF ( X1.GT.WAVE(NPOINT) ) THEN
            NPOINT=NPOINT+1
            WAVE(NPOINT)=X1
            FLUX(NPOINT)=X2
** Draw a symbol
            IF (MODE.EQ.0) THEN
               CALL POINTSYM(X1/DIVX,X2/DIVY,ISYMB)
** Join the symbols

               CALL LINE(WAVE(NPOINT-1)/DIVX,FLUX(NPOINT-1)/DIVY,
     :                   WAVE(NPOINT)/DIVX,FLUX(NPOINT)/DIVY)
               CALL SGS_FLUSH
               CALL PLOTIT( 0, 0, 2 )
            ELSE
               CALL POINTSYM(X1/DIVX,X2/DIVY,ISYMB)
            ENDIF
            CALL SGSCURSE(I,X1,X2,FIRSTCURSOR)
            GO TO 90
          END IF
       ENDIF

*  We now have the artificial data in the current arrays, either from
*  the cursor or from a file.
          IF (NPOINT.GT.1) THEN
             BREAKS(1)=NPOINT
             NBREAK=1
             TITLE = 'CDRAWn join-the-dots'
*
             IF (MODE.NE.0) THEN
                NWORKK = NPOINT
                DO I = 1, NPOINT
                   WORK(I) = WAVE(I)
                   WORK(ASIZE1+I) = FLUX(I)
                ENDDO

                IF (WAVE(NPOINT).LT.XSTACK(POINTR(NONSTK))
     :          .OR.WAVE(1).GT.
     :          XSTACK(POINTR(NONSTK)+STKNPT(NONSTK)-1)) THEN
                   WRITE (*,'(''   CDRAW 1:  no overlap with''
     :             ,'' stack grid recognised''/
     :             ''             join-the-dots retained'')')
                   GO TO 91
                ENDIF

                NPOINT = ASIZE1
                NBREAK = MAXBRK
                CALL GETSTK
     :       (NONSTK,NPOINT,WAVE,FLUX,NBREAK,BREAKS,TITLE,WORV,OK)
                TITLE = 'CDRAWn spline fit'
                IF (.NOT.OK) THEN
                   WRITE (*,
     :          '(''   CDRAW 1:  failed to access stack grid;''/
     :           ''              join-the-dots retained'')')
   91           CONTINUE
                   TITLE = 'CDRAWn join-the-dots'
                   OK = .TRUE.
                   NPOINT = NWORKK
                   BREAKS(1) = NPOINT
                   NBREAK = 1
                   IF (POLTST) CALL SETPOL
                   IF (HSTTST) CALL SETHIS
                   IF (MRKTST) CALL SETMAR
                   DO I = 1, NPOINT
                      WAVE(I) = WORK(I)
                      FLUX(I) = WORK(ASIZE1+I)
                      IF (.NOT.FILEIN)THEN
                        CALL LINE(WORK(1)/divx,WORK(ASIZE1+1)/divy,
     :                            WAVE(I)/divx,FLUX(I)/divy)
                        CALL PLOTIT( 0, 0, 2 )
                        CALL SGS_FLUSH
                      ENDIF
                     ENDDO
                   IF (LBOX) CALL JOBOX
                   IF (ROTST) THEN
                      CALL IPSET (IPAL,12)
                   ENDIF
                   SUBCHK = .FALSE.
                   RETURN
                ENDIF

                GVAL = 0.0
                BVAL = -12.4816
                DO I = 1, NPOINT
                   IF (WAVE(I).LT.WORK(1)
     :          .OR.WAVE(I).GT.WORK(NWORKK)) THEN
                      FLUX(I) = BVAL
                   ELSE
                      FLUX(I) = GVAL
                   ENDIF
                ENDDO
                CALL SRTBRK
     :       (ASIZE1, WAVE, FLUX, NPOINT, MAXBRK, BREAKS, NBREAK,
     :       BVAL, OK)
                IF (.NOT.OK) THEN
                   WRITE (*,
     :          '(''   CDRAW 1:  error sorting break data;''/
     :            ''             join-the-dots retained'')')
                   OK = .TRUE.
                   GO TO 91
                ELSEIF (NPOINT.LE.1) THEN
                   WRITE (*,
     :          '(''   CDRAW 1:   no overlap with stack grid;''/
     :            ''              join-the-dots retained'')')
                   GO TO 91
                ENDIF

                CALL INTEP
     :   (WAVE(1),FLUX(1),WORK(1),WORK(ASIZE1+1),NWORKK,IERR,.FALSE.)
                DO I = 2, NPOINT
                   CALL INTEP
     :   (WAVE(I),FLUX(I),WORK(1),WORK(ASIZE1+1),NWORKK,IERR,.TRUE.)
                ENDDO
                CALL SETPOL
                DO I = 2, NPOINT
                   IF (.NOT.FILEIN)THEN
                      CALL LINE(WAVE(I-1)/divx, FLUX(I-1)/divy,
     :                          WAVE(I)/divx,FLUX(I)/divy)
                   ENDIF
                ENDDO
                CALL PLOTIT( 0, 0, 2 )
                CALL SGS_FLUSH
             ENDIF
          ELSE
             WRITE (*,
     :       '(''   CDRAW: no continuum stored'')')
             NPOINT = 0
          ENDIF
          IF (POLTST) CALL SETPOL
          IF (HSTTST) CALL SETHIS
          IF (MRKTST) CALL SETMAR
          IF (LBOX) CALL JOBOX
          IF (ROTST) CALL IPSET (IPAL,12)
*

       RETURN
       END
