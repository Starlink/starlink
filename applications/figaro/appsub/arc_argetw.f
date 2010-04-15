C+
      SUBROUTINE ARC_ARGETW(ARC1,ARC2,ARC3,ARCS,NLARCS,
     :                                VINT,VFIT,FORGET,WAVEL)
C
C     A R G E T W
C
C     Arc utility routine.  Lists the position of an arc line
C     and gets the wavelength for it.
C
C     Parameters -     (">" input, "<" output)
C
C     (>) ARC1     (Real array ARC1(NLARCS)) The list of lines
C                  for the first of the possible arcs.
C     (>) ARC2     (Real array ARC2(NLARCS)) The list of lines
C                  for the first of the possible arcs.
C     (>) ARC3     (Real array ARC3(NLARCS)) The list of lines
C                  for the first of the possible arcs.
C     (>) ARCS     (Character) The arc types used.  This is the
C                  ARCTYPE parameter for the main ARC routine.
C     (>) NLARCS   (Integer) The dimension of the ARCn arrays.
C     (>) VINT     (Real) A guess at the possible value obtained
C                  by interpolation. Ignored if zero.
C     (>) VFIT     (Real) A guess at the possible value obtained
C                  by fitting.  Ignored if 0.
C     (<) FORGET   (Logical) Returned true if the line
C                  is to be ignored.
C     (<) WAVEL    (Real) The wavelength of the line.
C
C                                             KS / CIT 1st July 1983
C     Modified:
C
C     4th Sept 1985.  KS / AAO.  Now assumes wavelengths are exact
C                     if no arc type was specified.
C     26th Jul 1993.  HME / UoE, Starlink.  Disuse PAR_RDUSER and
C                     PAR_Q*, use PAR_ABORT.  Use ICH_CF instead of
C                     ICH_ENCODE.
C     25th Jul 1996.  MJCL / Starlink, UCL.  Catenation changed
C                     in light of Linux port.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FORGET
      INTEGER NLARCS
      REAL ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS)
      REAL VFIT,VINT,WAVEL
      CHARACTER*(*) ARCS
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_FOLD,ICH_VERIF,ICH_NUMBR,ICH_KEY,ICH_LEN
      REAL ARC_ARFIND
      CHARACTER ICH_CF*16
C
C     Local variables
C
      LOGICAL REPLYOK
      INTEGER IND,LENGTH,LTYPE,NEXT,STATUS,TYPE
      REAL VALUE
      CHARACTER*64 REPLY,TYPES
C
C     Generate the possible type keywords - 'EXACT', plus
C     the arc types as passed in ARCS - in the form needed
C     by ICH_KEY.
C
      TYPES='EXACT,'//ARCS
      LTYPE=ICH_FOLD(TYPES)+1
      TYPES(LTYPE:LTYPE)=','
C
C     Now ask for wavelength
C
      FORGET=.FALSE.
      REPLYOK=.FALSE.
      DO WHILE (.NOT.REPLYOK)
         CALL PAR_CNPAR('WAVELEN')
         CALL PAR_RDCHAR('WAVELEN',' ',REPLY)
         IF (PAR_ABORT()) RETURN
C
C        First check for a null reply, indicating the line is
C        to be ignored.
C
         LENGTH=ICH_FOLD(REPLY)
         IF (LENGTH.EQ.0) THEN
            FORGET=.TRUE.
            REPLYOK=.TRUE.
         ELSE
C
C           Not a null, so first check for a '?'
C
            NEXT=ICH_VERIF(REPLY,1,' ')
            IF (REPLY(NEXT:NEXT).EQ.'?') THEN
C
C              Output some help.
C
               CALL PAR_WRUSER('Enter wavelength as a number followed',
     :                                                      STATUS)
               CALL PAR_WRUSER(
     :                       '(optionally) by the arc type it is from',
     :                                                      STATUS)
               CALL PAR_WRUSER('If arc type is not specified,',STATUS)
               IND=INDEX(ARCS,',')-1
               IF (IND.LE.0) IND=ICH_LEN(ARCS)
               REPLY=ARCS(:IND)//' is assumed.'
               CALL PAR_WRUSER(REPLY,STATUS)
               CALL PAR_WRUSER('If type is given as ''EXACT'', then the'
     :                                                      ,STATUS)
               CALL PAR_WRUSER(
     :                        'wavelength is used as entered, otherwise'
     :                                                      ,STATUS)
               CALL PAR_WRUSER(
     :                      'the nearest tabulated wavelength is used',
     :                                                        STATUS)
               CALL PAR_WRUSER('Can also be given as ''I'' or ''F''',
     :                                                        STATUS)
               CALL PAR_WRUSER('For the interpolated or fitted value',
     :                                                         STATUS)
            ELSE
C
C              Check for an 'I', an 'F' or a valid number
C
               STATUS=0
               IF ((REPLY(1:2).EQ.'I').AND.(VINT.GT.0.)) THEN
                  VALUE=VINT
                  NEXT=3
               ELSE IF ((REPLY(1:2).EQ.'F').AND.(VFIT.GT.0.)) THEN
                  VALUE=VFIT
                  NEXT=3
               ELSE
                  STATUS=ICH_NUMBR(REPLY,1,' ,;',VALUE,NEXT)
               END IF
               IF (STATUS.NE.0) THEN
                  CALL PAR_WRUSER('Not a valid wavelength',STATUS)
               ELSE
C
C                 Number decodes OK, so look for a type
C
                  IF (ICH_VERIF(REPLY,NEXT,' ').EQ.0) THEN
C
C                    No type given, so assume default.  If no arc
C                    type specified, assume exact.
C
                     IF (ARCS.EQ.'NONE') THEN
                        TYPE=1
                     ELSE
                        TYPE=2
                     END IF
                  ELSE
                     TYPE=ICH_KEY(REPLY,NEXT,' ,;',TYPES(:LTYPE),
     :                                                'Abbr.',NEXT)
                     IF (TYPE.EQ.0) THEN
                        CALL PAR_WRUSER('Invalid arc type',STATUS)
                        WAVEL=0.
                     END IF
                  END IF
C
C                 Use type spec to locate exact arc line
C
                  IF (TYPE.EQ.1) THEN
                     WAVEL=VALUE
                  ELSE IF (TYPE.EQ.2) THEN
                     WAVEL=ARC_ARFIND(ARC1,NLARCS,VALUE)
                  ELSE IF (TYPE.EQ.3) THEN
                     WAVEL=ARC_ARFIND(ARC2,NLARCS,VALUE)
                  ELSE IF (TYPE.EQ.4) THEN
                     WAVEL=ARC_ARFIND(ARC3,NLARCS,VALUE)
                  END IF
C
C                 Either output what appears to be the line's
C                 wavelength, or complain.
C
                  IF (WAVEL.EQ.0.) THEN
                     CALL PAR_WRUSER('Cannot obtain a wavelength value',
     :                                                       STATUS)
                  ELSE
                     REPLY='Wavelength is '//ICH_CF(WAVEL)
                     NEXT=ICH_LEN(REPLY)+1
                     REPLY(NEXT:)=' OK?'
                     CALL PAR_WRUSER(REPLY,STATUS)
                     CALL PAR_CNPAR('LINEOK')
                     CALL PAR_RDKEY('LINEOK',.TRUE.,REPLYOK)
                     IF (PAR_ABORT()) RETURN
                  END IF
               END IF
            END IF
         END IF
      END DO
C
      RETURN
      END
