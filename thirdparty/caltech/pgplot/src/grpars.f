C*GRPARS -- parse device specification string
C+
      INTEGER FUNCTION GRPARS (SPEC,DEV,TYPE,APPEND)
      CHARACTER*(*) SPEC, DEV
      INTEGER  TYPE
      LOGICAL  APPEND
C
C GRPCKG: decode a device-specification; called by GROPEN.
C
C Returns:
C  GRPARS (output): 1 if the device-specification is
C       acceptable; any other value indicates an error.
C
C Arguments:
C  SPEC (input): the device specification.
C  DEV  (output):  device name or file spec.
C  TYPE (output): device type (integer code); 0 if no device
C       type is specified.
C  APPEND (output): .TRUE. if /APPEND specified, .FALSE. otherwise.
C--
C 23-Jul-1984 - [TJP].
C 19-Feb-1988 - allow device part to be quoted [TJP].
C 30-Mar-1989 - remove logical translation of device and type [TJP].
C 17-Jun-1991 - ignore comments after ' (' [TJP].
C 19-Dec-1994 - rewritten to scan backwards [TJP].
C  6-Jun-1995 - correct a zero-length string problem [TJP].
C-----------------------------------------------------------------------
      CHARACTER*32  CTYPE, UPPER
      CHARACTER*6   APPSTR
      CHARACTER*256 DESCR
      INTEGER       GRDTYP, GRTRIM
      INTEGER       L, LC, LS
      DATA          APPSTR/'APPEND'/
C
C Default results.
C
      DEV = ' '
      TYPE = 0
      APPEND = .FALSE.
      GRPARS = 1
      CTYPE = ' '
C
C Null string is acceptable.
C
      IF (LEN(SPEC).LT.1) RETURN
      IF (SPEC.EQ.' ') RETURN
C
C On systems where it is possible, perform a "logical name" translation.
C
      DESCR = SPEC
      CALL GRLGTR(DESCR)
C
C Discard trailing blanks: L is length of remainder.
C
      L = GRTRIM(DESCR)
C
C Find last slash in string (position LS or 0).
C
      LS = L
 20   IF (DESCR(LS:LS).NE.'/') THEN
         LS = LS-1
         IF (LS.GT.0) GOTO 20
      END IF
C
C Check for /APPEND qualifier; if present, look again for type.
C
      IF (LS.GT.0) THEN
         CTYPE = DESCR(LS+1:L)
         CALL GRTOUP(UPPER,CTYPE)
         CTYPE = UPPER
         IF (CTYPE.EQ.APPSTR) THEN
            APPEND = .TRUE.
            L = LS-1
            LS = L
 30         IF (DESCR(LS:LS).NE.'/') THEN
               LS = LS-1
               IF (LS.GT.0) GOTO 30
            END IF
         ELSE
            APPEND = .FALSE.
         END IF
      END IF
C
C If LS=0 there is no type field: use PGPLOT_TYPE.
C
      IF (LS.EQ.0) THEN
         CALL GRGENV('TYPE', CTYPE, LC)
      ELSE
         CTYPE = DESCR(LS+1:L)
         LC = L-LS
         L = LS-1
      END IF
C
C Check for allowed type.
C
      IF (LC.GT.0) THEN
         CALL GRTOUP(UPPER,CTYPE)
         CTYPE = UPPER
         TYPE = GRDTYP(CTYPE)
         IF (TYPE.EQ.0) CALL GRWARN('Unrecognized device type')
         IF (TYPE.EQ.-1) CALL GRWARN('Device type is ambiguous')
      ELSE
         TYPE = 0
         CALL GRWARN('Device type omitted')
      END IF
      IF (TYPE.EQ.0) GRPARS = GRPARS+2
C
C Remove quotes from device if necessary.
C
      IF (L.GE.1) THEN
         IF (DESCR(1:1).EQ.'"' .AND. DESCR(L:L).EQ.'"') THEN
            DEV = DESCR(2:L-1)
            L = L-2
         ELSE
            DEV = DESCR(1:L)
         END IF
      END IF
C
C      write (*,*) 'Device = [', DEV(1:L), ']'
C      write (*,*) 'Type   = [', CTYPE, ']', TYPE
C      write (*,*) 'APPEND = ', APPEND
C
      END
