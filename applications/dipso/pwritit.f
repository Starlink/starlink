*
*    PWRITIT
*    Decode PWRITE string into 3 numerical values and the text string
*    Supply results to JM calls
*
       SUBROUTINE PWRITIT
     : (PARAMS, TMPSTR, CMD,
     : XS2NDC, YS2NDC,
     : DEFHEIGHT, HTFACTOR, NDEGREES, IFONT,
     : LNTYPE,MAXSTK, VARRAY, OK,STATUS)

       IMPLICIT NONE
       INCLUDE 'SAE_PAR'
       INTEGER STATUS

       CHARACTER*(*) PARAMS, TMPSTR, CMD
       INTEGER LNTYPE, MAXSTK, NDEGREES, IFONT
       REAL DEFHEIGHT, HTFACTOR, VARRAY(MAXSTK)
       REAL XS2NDC, YS2NDC
       LOGICAL  OK

       CHARACTER*1 BLEEP
       COMMON /BLEEP/ BLEEP
       REAL DIVX, DIVY
       COMMON /DIVFACTOR/ DIVX, DIVY

       INTEGER I, J, K, L1, L2, IHX, ISTA
       INTEGER IPWR, ICEN
       INTEGER SLEN
       REAL VX1, VX2, XUSER, YUSER, XUSER1, YUSER1, HEIGHT

       REAL ZZX, ZZX1, ZZX2, ZZY, ZZY1, ZZY2

       IF( STATUS .NE. SAI__OK ) RETURN

       TMPSTR = ' '
       DO I = 1, 3
          CALL SSTRIP (PARAMS)
          L1 = INDEX(PARAMS,' ')
          L2 = SLEN(TMPSTR) + 2
          TMPSTR(L2:) = PARAMS(1:L1)
          PARAMS(1:) = PARAMS(L1:)
       ENDDO
       IF (PARAMS(1:1).EQ.' ') PARAMS(1:) = PARAMS(2:)
       CALL DECODE
     : ('PWRITE',TMPSTR(1:SLEN(TMPSTR)),3,3,VARRAY,
     : 'X_coord Y_coord Location_index ',OK)
       IF (.NOT.OK) RETURN
       VX1 = VARRAY(1)
       VX2 = VARRAY(2)
       IPWR = NINT(VARRAY(3))
       IF (IPWR.LE.0 .OR. IPWR.GT.9) THEN
          WRITE (*,
     :    '(''   PWRITE:  location index must be 1-9'',A)') BLEEP
          OK = .FALSE.
          RETURN
       ENDIF
       IF (PARAMS.EQ.' ') THEN

          CALL RDSTR( 'PWRITE', 'String', ' ', PARAMS, STATUS )
          IF( STATUS .NE. SAI__OK ) THEN
             OK = .FALSE.
             RETURN
          END IF

          I = MAX(1,SLEN(PARAMS))
          IF (PARAMS(I:I).EQ.'"') THEN
             PARAMS(I:I) = ' '
             PARAMS(1:) = ' '//PARAMS(1:)
             J = MAX(1,INDEX(PARAMS,'"'))
             PARAMS(J:) = PARAMS(J+1:)
           ENDIF
       ENDIF


       IF (IFONT.LT.2) THEN
          ICEN = 0
       ELSE
          IF (IPWR.EQ.1 .OR. IPWR.EQ.4 .OR. IPWR.EQ.7) THEN
             ICEN = +2
          ELSEIF (IPWR.EQ.3.OR.IPWR.EQ.6.OR.IPWR.EQ.9) THEN
             ICEN = -2
          ELSE
             ICEN = 0
          ENDIF
       ENDIF

       J = SLEN(PARAMS) + 1
       IF (IFONT.EQ.2) THEN
          IF (IPWR.EQ.1) THEN
             PARAMS(J:) = '''H:-40V:+60'' '
             J = SLEN(PARAMS) + 1
          ELSEIF (IPWR.EQ.2) THEN
             PARAMS(1:) = ' ''H:-70V:-130'''//PARAMS(1:)
             J = SLEN(PARAMS)
          ELSEIF (IPWR.EQ.3) THEN
             PARAMS(1:) = ' ''H:-60V:-60'''//PARAMS(1:)
             J = SLEN(PARAMS)
          ELSEIF (IPWR.EQ.7) THEN
             PARAMS(J:) = '''H:-40V:-60'' '
             J = SLEN(PARAMS)+1
          ELSEIF (IPWR.EQ.8) THEN
             PARAMS(1:) = ' ''H:-70V:+100'''//PARAMS(1:)
             J = SLEN(PARAMS)
          ELSEIF (IPWR.EQ.9) THEN
             PARAMS(1:) = ' ''H:-60V:+58'''//PARAMS(1:)
             J = SLEN(PARAMS)
          ENDIF
       ENDIF

       J = SLEN(PARAMS)

**  Transform from user co-ords to graph window co-ords
      CALL SGS_IZONE(ZZX1,ZZX2,ZZY1,ZZY2,ZZX,ZZY)
*   WRITE(*,*)'WORLD',ZZX1,ZZX2,ZZY1,ZZY2
       XUSER=VX1/DIVX
       YUSER=VX2/DIVY
       XUSER1=VX1*1.0/XS2NDC
       YUSER1=VX2*YS2NDC
*       XPOS=GRID(1)+((VX1-XMIN)/(XMAX-XMIN))*(GRID(2)-GRID(1))
*       YPOS=GRID(3)+((VX2-YMIN)/(YMAX-YMIN))*(GRID(4)-GRID(3))
       HEIGHT=DEFHEIGHT*HTFACTOR
       CALL GSLN(1)
       CALL WTSTRUSER(XUSER,YUSER,PARAMS(1:J),
     :    HEIGHT,NDEGREES,ICEN)
       CALL SGS_FLUSH
       CALL PLOTIT( 0, 0, 2 )
       CALL GSLN(LNTYPE)

       END
