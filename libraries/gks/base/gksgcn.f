C# IL>=a, OL>=1
      SUBROUTINE GKSGCN(IXWKID,TRNC2,TRNC3,XSRSUB,XCLSUB,LHCLIP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To playback the contents of a segment to workstations as
*     directed by the input parameters.
*     Subroutine terminates when end of segment is reached or error occurs.
*     Note that if required BEGIN SEGMENT and END SEGMENT are sent
*     by the caller.
*
*  MAINTENANCE LOG
*  ---------------
*     23/11/83  JRG   Created
*     14/02/84  JRG   GDP reference points in the WCA
*     22/02/84  JRG   For now, do not check data record size; also
*                     set text precision (was missing)
*     01/05/84  CJW   Correct wkstn interface for CA (I194)
*     01/05/84  CJW   Support (SIC) sliced primatives (I191)
*     23/05/84  JRG   .. at least for polyline,polymarker,cell array (bug S62)
*     01/01/85  MGC   Revised interface for Level 2a segment functions.
*                     Also, now reads GKS state list.
*     01/03/86  MGC   Bugfix I299: Playback pick identifiers
*     10/12/86  JCS   COMMENTED PRINT STATEMENTS REMOVED
*     20/01/87  DCS   IS conversion. Set last source flags to be out of
*                     date after sending transformation or attribute
*                     data to workstations. Use clipping rectangle in
*                     GKS State List instead of viewport of current NT.
*     20/01/87  ARG   IS conversion. Error numbers changed.
*     23/04/87  KWB   IS conversion. Changes to cell array to reflect
*                     new workstation interface.
*
*  ARGUMENTS
*  ---------
*     INP  IXWKID  Workstation Identifier or nil (see comment)
*     INP  TRNC2   Transformation C2 - combined insert transformation
*     INP  TRNC3   Transformation C3 - combined segment transformation
*                  (segment itself is in KRPSG (in COMMON)).
*     INP  XSRSUB  Segment read routine
*     INP  XCLSUB  Call Layer interface routine
*     INP  LHCLIP  .TRUE. if clipping rectangles are to be honoured
*
      INTEGER IXWKID
      REAL TRNC2(6),TRNC3(6)
      LOGICAL LHCLIP
      EXTERNAL XSRSUB,XCLSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Arrays from CSS return on the stack. This routine
*                     deallocates it.
*     Read   /CCA/    Individual variables from CSS return in this
*                     CSS Communication Area.
*     Read   /SL/     Last source flags, clipping indicator and clipping
*                     rectangle.
*     Modify /WCA/    Individual variables from CSS are put here.
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ICASE  Derived from ITYPE; used for computed GOTO and so starts from 1
*     IHOLD  Local copy of KERROR. Needed because more than one error
*            condition may occur and we want the first.
*     ITYPE  Type of item returned from CSS
*     NR,NI,NC Number of REALs, INTEGERs, CHARACTER*80's returned from CSS
*     IPR    Stack pointers for array of reals (x & y)
*     IPI    Stack pointer for integers
*     IPC    Stack pointer for CHARACTER*80 information
*     J      Loop pointer
*     MINLEN Minimum length of array information for each of the
*            output primitives in turn (this array is not altered).
*     MORE   Indicates whether there is more of the current item not yet
*            returned by CSS (values GMORE and GNMORE)
*
      INTEGER ICASE,IHOLD,ITYPE, NR,NI,NC, IPR,IPI,IPC, J,
     :  MINLEN(6), MORE
      DATA MINLEN/2,1,1,3,1,1/
*
*  STACK USAGE
*  -----------
*     1*NR     REAL     (twice) used by CSS to return 2 arrays (x & y) of
*                       REAL info
*     1*NI     INTEGER  used by CSS to return 1 array of INTEGER info
*     1*NC     CHARACTER*80  Currently unused .... no CHARACTER stack yet
*
*  ERRORS
*  ------
*      301   Overflow of segment storage ... in this case because replay
*            cannot cope with big segment item
*    -2004   Bug in parameters of internal routine
*
*  COMMENTS
*  --------
*  The workstation identifier supplied to this routine is not used
*  other than to be passed down to the segment playback routine.
*  The identifier may be a valid workstation identifier, or it may
*  be nil.
*
*---------------------------------------------------------------------


*   -------------
*   Start of loop. Read CSS once for each iteration.
  100 CONTINUE

*   Read CSS
      CALL XSRSUB(ITYPE, NR,NI,NC, IPR,IPI,IPC, MORE)
      IF( KERROR.EQ.0 ) THEN

*       Currently we allow polyline and polymarker items that require
*       more than one call. Eventually the workstation interface should
*       be changed to allow all output primitives.
          IF( MORE.EQ.GMORE .AND.
     :       .NOT. ( ITYPE.EQ.KPL .OR. ITYPE.EQ.KPM .OR. ITYPE.EQ.KCA
     :                                                      )) THEN
            KERROR=301
            GOTO 999
          ENDIF

*       Branch in groups.
          IF( KPL.LE.ITYPE .AND. ITYPE.LE.KGDP ) THEN
*          First group is output primitives.
            ICASE=ITYPE-KPL+1
*                 PL  PM  TX  FA  CA  GDP
            GOTO(210,210,230,210,230,260) ICASE
            GOTO 970

*          Polyline, polymarker, fill area (they all just have arrays of pts)
  210       IF( NR.LT.MINLEN(ICASE) ) GOTO 970
            CALL XCLSUB(IXWKID,ITYPE,1,KDAT,
     :            NR,QSTACK(IPR),QSTACK(IPR+NR), 1,CH)
            GOTO 290

*          Text, cell array (both have integer array + individual variables)
  230       IF( NI.LT.MINLEN(ICASE) ) GOTO 970
            QWR1=QSS1
            QWR2=QSS2
*          (the following needed for cell array, doesn't matter for text)
            QWR3=QSS3
            QWR4=QSS4
            QWR5=QSS5
            QWR6=QSS6
            KWI1=KSS1
            KWI2=KSS2
            KWI3=1
            KWI4=1
            KWI5=KSS1
            KWI6=KSS2
            CALL XCLSUB(IXWKID,ITYPE,NI,KSTACK(IPI),
     :            1,QDAT,QDAT,1,CH)
            GOTO 290

*          G.D.P: will need changing to handle data record
*                 When that happens, NC will need to be checked
  260       IF( NR.LT.1 ) GOTO 970
            QWR1=QSS1
            QWR2=QSS2
            QWR3=QSS3
            QWR4=QSS4
            QWR5=QSS5
            QWR6=QSS6
            KWI1=KSS1
            CALL XCLSUB(IXWKID,ITYPE,1,KDAT,
     :            NR,QSTACK(IPR),QSTACK(IPR+NR),1,CH)
            GOTO 290

*
  290       CONTINUE
          ELSEIF( KSPLA.LE.ITYPE .AND. ITYPE.LE.KNT ) THEN
*          Output attribute group
            IF( ITYPE.LE.KSFAA ) THEN
*                  PL  PM  TX  FA
              GOTO(310,320,330,340) ITYPE-KSPLA+1
                GOTO 970
*              Polyline attributes
  310           KIPLI =KSPLI
                KILNTY=KSLNTY
                KIPLCI=KSPLCI
                QILNWD=QSLNWD
                DO 315 J=1,3
  315             KIPLAF(J)=KSPLAF(J)
                KSPLWK=KHANGE
                GOTO 390

*              Polymarker attributes
  320           KIPMI=KSPMI
                KIMKTY=KSMKTY
                KIPMCI=KSPMCI
                QIMKSZ=QSMKSZ
                DO 325 J=1,3
  325             KIPMAF(J)=KSPMAF(J)
                KSPMWK=KHANGE
                GOTO 390

*              Text attributes
  330           KITXI=KSTXI
                KITXFN=KSTXFN
                KITXPR=KSTXPR
                KITXCI=KSTXCI
                KITXP =KSTXP
                KIHTXA=KSHTXA
                KIVTXA=KSVTXA
                QICHXP=QSCHXP
                QICHSP=QSCHSP
                QICHHX=QSCHHX
                QICHHY=QSCHHY
                QICHWX=QSCHWX
                QICHWY=QSCHWY
                DO 335 J=1,4
  335             KITXAF(J)=KSTXAF(J)
                KSTXWK=KHANGE
                GOTO 390

*              Fill area attributes
  340           KIFAI =KSFAI
                KIFAIS=KSFAIS
                KIFASI=KSFASI
                KIFACI=KSFACI
                QIPAHX=QSPAHX
                QIPAHY=QSPAHY
                QIPAWX=QSPAWX
                QIPAWY=QSPAWY
                QIPAX =QSPAX
                QIPAY =QSPAY
                DO 345 J=1,3
  345             KIFAAF(J)=KSFAAF(J)
                KSFAWK=KHANGE
                GOTO 390
*
  390           CONTINUE
            ELSEIF( ITYPE.EQ.KNT ) THEN
*            Normalization (C2) and Segment (C3) transformations
              DO 410 J=1,6
                QWRA(J)=TRNC2(J)
                QWRA(10+J)=TRNC3(J)
  410         CONTINUE

*            Clipping rectangles honoured/ignored
              IF (LHCLIP) THEN
                QWR7=QSS7
                QWR8=QSS8
                QWR9=QSS9
                QWR10=QSS10
              ELSE
                IF( KCLIN.EQ.GCLIP ) THEN
                  QWR7=QCCLXL
                  QWR8=QCCLXR
                  QWR9=QCCLYB
                  QWR10=QCCLYT
                ELSE
                  QWR7=0.0
                  QWR8=1.0
                  QWR9=0.0
                  QWR10=1.0
                ENDIF
              ENDIF
              CALL GKTOLD
            ELSEIF( ITYPE.EQ.KSPKID ) THEN
*            Pick identifier
              KWI1=KSS1
            ELSE
              GOTO 970
            ENDIF

*          Here to send item to workstation .... no arrays
            CALL XCLSUB(IXWKID,ITYPE, 1,KDAT, 1,QDAT,QDAT, 1,CH)
          ELSEIF( ITYPE.NE.KENSG) THEN
            GOTO 970
          ENDIF
      ENDIF

*   Save KERROR
      IHOLD=KERROR

*   Here to deallocate stack. We pass through here whether there was
*   was error or not.
*   Deallocate stack in reverse order to the parameters returning
*   from GKCSRD.
      CALL GKSTDA(KCHARS,IPC)
      CALL GKSTDA(KINTGS,IPI)
      CALL GKSTDA(KREALS,IPR)
      IF( IHOLD.NE.0 ) KERROR=IHOLD

*   Do another item if error-free and not at the end
      IF( KERROR.EQ.0 .AND. ITYPE.NE.KENSG ) GOTO 100
*   End of loop
*   -----------

      GOTO 999

*   Bug in data returned by GKCSRD
  970 CALL GKBUG(-2004,'GKSGCN')

*
  999 CONTINUE
      END
