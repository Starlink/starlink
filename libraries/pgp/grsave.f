      SUBROUTINE GRSAVE(SAVE)
*+
*
*     - - - - - - - -
*       G R S A V E     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Save (or restore) all the global GKS attributes that may be changed
*   by GRPCKG.
*
*   Given
*      SAVE   l    Save operation (TRUE) or restore (FALSE)
*
*   D.L.Terrett  Starlink  Oct 1988
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      LOGICAL SAVE, SAVEOK
      INTEGER IERR
      SAVE SAVEOK

      INTEGER LASF(13)
      INTEGER ICLIP, ITRN, IFACI, IFAIS, ITYPE, MTYPE, MCI, IPLCI
      REAL WINDO(4), VIEWP(4), WIND2(4), VIEW2(4), WSC, CLRECT(4)

      SAVE LASF, ICLIP, ITRN, WINDO, VIEWP, WIND2, VIEW2, IFACI, IFAIS,
     1     WSC, ITYPE, MTYPE, MCI, IPLCI

      DATA SAVEOK/.FALSE./

      IF (SAVE) THEN
         SAVEOK = .TRUE.

*     Aspect source flags
         CALL GQASF(IERR, LASF)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQASF', IERR)
            GO TO 9999
         END IF

*     Clipping
         CALL GQCLIP(IERR,ICLIP,CLRECT)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQCLIP', IERR)
            GO TO 9999
         END IF

*     Current normalization transformation
         CALL GQCNTN(IERR,ITRN)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQCNTN', IERR)
            GO TO 9999
         END IF

*     Normalization transformations used by GRPCKG
         CALL GQNT(TRN,IERR,WINDO,VIEWP)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQNT', IERR)
            GO TO 9999
         END IF
         CALL GQNT(TRN2,IERR,WIND2,VIEW2)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQNT', IERR)
            GO TO 9999
         END IF

*     Fill area colour index and interior style
         CALL GQFACI(IERR, IFACI)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQFACI', IERR)
            GO TO 9999
         END IF
         CALL GQFAIS(IERR, IFAIS)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQFAIS', IERR)
            GO TO 9999
         END IF

*     Line type, width scale factor and colour index
         CALL GQLN(IERR,ITYPE)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQLN', IERR)
            GO TO 9999
         END IF
         CALL GQLWSC(IERR,WSC)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQLWSC', IERR)
            GO TO 9999
         END IF
         CALL GQPLCI(IERR,IPLCI)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQPLCI', IERR)
            GO TO 9999
         END IF

*     Marker type and colour index
         CALL GQMK(IERR,MTYPE)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQMK', IERR)
            GO TO 9999
         END IF
         CALL GQPMCI(IERR,MCI)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSAVE', 'GQPMCI', IERR)
            GO TO 9999
         END IF

      ELSE

         IF (.NOT.SAVEOK) THEN
            CALL ERR_REP('GRNOSA',
     :      'GRSAVE - restore not preceeded by save', GRNOSA)
            GO TO 9999
         END IF

*     Aspect source flags
         CALL GSASF(LASF)

*     Clipping
         CALL GSCLIP(ICLIP)

*     Current normalization transformation
         CALL GSELNT(ITRN)

*     Normalization transformations used by GRPCKG
         CALL GSWN(TRN,WINDO(1),WINDO(2),WINDO(3),WINDO(4))
         CALL GSVP(TRN,VIEWP(1),VIEWP(2),VIEWP(3),VIEWP(4))
         CALL GSWN(TRN2,WIND2(1),WIND2(2),WIND2(3),WIND2(4))
         CALL GSVP(TRN2,VIEW2(1),VIEW2(2),VIEW2(3),VIEW2(4))

*     Fill area colour index and interior style
         CALL GSFACI(IFACI)
         CALL GSFAIS(IFAIS)

*     Line type, width scale factor and colour index
         CALL GSLN(ITYPE)
         CALL GSLWSC(WSC)
         CALL GSPLCI(IPLCI)

*     Marker type and colour index
         CALL GSMK(MTYPE)
         CALL GSPMCI(MCI)

      END IF
 9999 CONTINUE
      END
