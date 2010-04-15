*
       SUBROUTINE GETSTK
     : (INDEX,NPTS,XVALS,YVALS,NBRKS,IBRKS,TITLE,WORV,OK)
*
       INTEGER INDEX, NPTS, NBRKS, TLEN
       INTEGER IBRKS(NBRKS)
       REAL XVALS(NPTS), YVALS(NPTS)
       REAL WORV
       CHARACTER*(*) TITLE
       LOGICAL OK
*
       INCLUDE 'DECLARE_STKS'
*
*
       OK = .TRUE.
*
       IF (INDEX.LE.0 .OR. INDEX.GT.NONSTK) THEN
          WRITE (*,'(''   GETSTK:  stack index outside range'')')
          OK = .FALSE.
          GOTO 300
       ENDIF
       IF (NPTS.LT.STKNPT(INDEX)) THEN
          WRITE (*,
     :    '(''   GETSTK:  data array sizes ='',I6,
     :    ''; insufficient space provided'')') STKNPT(INDEX)
          OK = .FALSE.
          GOTO 300
       ENDIF
       IF (NBRKS.LT.BSTNPT(INDEX)) THEN
          WRITE (*,
     :    '(''   GETSTK:   break array size ='',I4,
     :    ''; insufficient space provided'')') BSTNPT(INDEX)
          OK = .FALSE.
          GOTO 300
       ENDIF
*
       NPTS = STKNPT(INDEX)
       NBRKS = BSTNPT(INDEX)

       I1 = POINTR(INDEX)
       DO 100 I = 1, NPTS
          XVALS(I) = XSTACK(I1)
          YVALS(I) = YSTACK(I1)
          I1 = I1 + 1
  100  CONTINUE

       I1 = BPOINT(INDEX)
       DO 200 I = 1, NBRKS
          IBRKS(I) = BSTACK(I1)
          I1 = I1 + 1
  200  CONTINUE

       TITLE = ' '
       TLEN = LEN( TITLE )
       TITLE = STITLE(INDEX)(1:MIN(80,TLEN))

       WORV = WORVST(INDEX)
       IF (WORV.EQ.0.0) WORV = 1.0
*
*
  300  CONTINUE
       END
