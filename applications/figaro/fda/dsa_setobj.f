      SUBROUTINE DSA_SETOBJ (RECORD,IST,ENV,STATUS)
*+
*  Name:
*     DSA_SETOBJ

*  Purpose:
*     Create and/or set data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SETOBJ (RECORD,IST,ENV,STATUS)

*  Description:
*     Creates and/or sets data objects as described in a character
*     string. The string should have the format 'objectname = value'
*     with the value optionally enclosed in double quotes if it is a
*     character quantity. The object name should be one defined in a
*     structure definition file (i.e. one that will be recognised by
*     DSA_ELEMENT_NAME). This routine will not create structures, so all
*     the upper levels should already exist.
*
*     Note that this routine replaces FIG_SETOBJ, which handled SET
*     commands which explicitly specified the structured name of the
*     object in question, e.g. 'SET .Z.UNITS = "AB magnitudes"'. It is
*     possible to have EQUATE'd variables in a structure definition file
*     whose names are structured, and so it is possible to achieve
*     compatability with the old SET commands by using structure
*     definition files that EQUATE variables called, for example,
*     ".Z.UNITS" to their proper name (".UNITS" for an NDF file,
*     ".Z.UNITS" for a .DST file, in this case).
*
*     Contrary to earlier implementations, the status argument is an
*     inherited status. I.e. this routine returns without action if the
*     given status is not zero.

*  Arguments:
*     RECORD = CHARACTER * ( * ) (Given)
*        String containing the assignment.
*     IST = INTEGER (Given)
*        Character in the string (starting from 1) at which the
*        assignment starts.
*     ENV = CHARACTER * ( * ) (Given)
*        The environment for the data objects.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     CALL DSA_SETOBJ( 'SET UNITS = "AB magnitudes"', 4,
*    :   'SPIKE', STATUS )
*
*     This will generate a character data object 'SPIKE.Z.UNITS', which
*     will be set to 'AB magnitudes', assuming that UNITS has been
*     equated to .Z.UNITS in a structure definition file.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     24 Mar 1991 (ks):
*        Original version, based on FIG_SETOBJ.
*     25 Mar 1991 (ks):
*        Fixed bug in creation of character items.
*     24 Sep 1992 (hme):
*        Change name from FIGX_SETOBJ.
*     04 Mar 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER * ( * ) RECORD
      INTEGER IST
      CHARACTER * ( * ) ENV

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CHAR
      LOGICAL ERROR
      INTEGER IEND
      INTEGER IPT
      INTEGER ISTCH
      INTEGER IVEN
      INTEGER IVST
      INTEGER LNAME
      INTEGER LREC
      INTEGER NCH
      INTEGER NEXT
      INTEGER NSFIG
      DOUBLE PRECISION DVALUE
      CHARACTER * ( 80 ) NAME
      CHARACTER * ( 80 ) MESSAG
      CHARACTER * ( 8 )  TYPE
      CHARACTER * ( DAT__SZLOC ) LOC

*  Internal References:
      INTEGER CHR_LEN
      INTEGER DSA3_DELIM
      INTEGER DSA3_NUMBD
      INTEGER DSA3_VERIF

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  First try to delimit the object name
      ERROR = .TRUE.
      LREC = CHR_LEN( RECORD )
      IF ( IST .GT. LREC ) THEN
         MESSAG = 'No assignment specified.'
         GO TO 400
      END IF
      ISTCH = DSA3_VERIF( RECORD, IST, ' ' )
      IVST = 0
      IEND = DSA3_DELIM( RECORD, ISTCH, ' =' ) - 1
      IF ( IEND .GE. 0 ) IVST = DSA3_VERIF( RECORD, IEND+1, ' =' )
      IF ( IVST .EQ. 0 ) THEN
         MESSAG = 'No value specified in assignment.'
         GO TO 400
      END IF

*  Now try to decode the value part of the assignment
      IF ( RECORD(IVST:IVST) .EQ. '"' ) THEN
         CHAR = .TRUE.
         IVEN = 0
         IF ( IVST .LT. LREC ) IVEN = DSA3_DELIM( RECORD, IVST+1, '"' )
         IF ( IVEN .EQ. 0 ) THEN
            MESSAG = 'Unmatched quote marks in value.'
            GO TO 400
         END IF
         IVST = IVST + 1
         IVEN = IVEN - 1
         IF ( IVEN .LT. IVST ) THEN
            MESSAG = 'Null character string specified in value.'
            GO TO 400
         END IF
         NCH  = MAX( 40, IVEN-IVST+1 )
         TYPE = 'CHAR'
      ELSE
         STATUS = DSA3_NUMBD( RECORD, IVST, ' ;*', DVALUE, NSFIG, NEXT )
         IF ( STATUS .EQ. -1 ) THEN
            MESSAG = 'Null value in assignment.'
            GO TO 400
         ELSE IF ( STATUS .NE. 0 ) THEN
            MESSAG = 'Invalid numeric value specified.'
            GO TO 400
         END IF
         CHAR = .FALSE.
         NCH  = 0
         IF ( NSFIG .GT. 7 ) THEN
            TYPE = 'DOUBLE'
         ELSE
            TYPE = 'FLOAT'
         END IF
      END IF

*  Having got value and name for object, attempt to set it
      STATUS = 0
      CALL DSA_ELEMENT_NAME( ENV, RECORD(ISTCH:IEND), NAME, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500
      IF ( CHAR ) THEN
         LNAME = CHR_LEN( NAME )
         NAME(LNAME+1:) = '['
         CALL CHR_ITOC( NCH, NAME(LNAME+2:), IPT )
         IPT = CHR_LEN( NAME )
         NAME(IPT+1:IPT+1) = ']'
         CALL DTA_CRVAR( NAME, TYPE, STATUS )
         CALL DTA_LOC( NAME(:LNAME), LOC, STATUS )
         IF ( STATUS .EQ. 0 ) THEN
            CALL ERR_MARK
            STATUS = SAI__OK
            CALL DAT_PUT0C( LOC, RECORD(IVST:IVEN), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               STATUS = 1
            END IF
            CALL ERR_RLSE
         END IF
         CALL DTA_ANNUL( LOC, STATUS )
      ELSE
         CALL DTA_CRVAR( NAME, TYPE, STATUS )
         CALL DTA_WRVARD( NAME, 1, DVALUE, STATUS )
      END IF
      IF ( STATUS .NE. 0 ) THEN
         MESSAG = 'Unable to assign ' //
     :      NAME(:CHR_LEN(NAME)) // ' due to a DTA error.'
         GO TO 400
      END IF

*  All done OK.
      ERROR = .FALSE.

*  Exit (with or without error).
  400 CONTINUE
      IF ( ERROR ) THEN
         CALL ERR_MARK
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T014', MESSAG )
            CALL ERR_REP( 'FDA_E076', 'DSA_SETOBJ: ^FDA_T014', STATUS )
            CALL ERR_REP( 'FDA_E077', RECORD, STATUS )
            CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE
         STATUS=1
      ELSE
         STATUS=0
      END IF

*  Exit for case of report done and status set by called routine.
 500  CONTINUE
      END



      INTEGER FUNCTION DSA3_VERIF(STRING,IST,CHARS)

*  Copy of ICH_VERIF

      CHARACTER*(*) STRING,CHARS
      INTEGER IST

      INTEGER NST,I

      IF ((IST.LT.1).OR.(IST.GT.LEN(STRING))) THEN
         NST=1
      ELSE
         NST=IST
      END IF

      DSA3_VERIF=0
      DO I=NST,LEN(STRING)
         IF (INDEX(CHARS,STRING(I:I)).EQ.0) THEN
            DSA3_VERIF=I
            GO TO 340
         END IF
      END DO
  340 CONTINUE

      END



      INTEGER FUNCTION DSA3_DELIM(STRING,IST,CHARS)

*  Copy of ICH_DELIM.

      CHARACTER*(*) STRING,CHARS
      INTEGER IST

      INTEGER NST,I

      IF ((IST.LT.1).OR.(IST.GT.LEN(STRING))) THEN
         NST=1
      ELSE
         NST=IST
      END IF

      DSA3_DELIM=0
      DO I=NST,LEN(STRING)
         IF (INDEX(CHARS,STRING(I:I)).NE.0) THEN
            DSA3_DELIM=I
            GO TO 340
         END IF
      END DO
  340 CONTINUE

      END



      INTEGER FUNCTION DSA3_NUMBD(STRING,IST,DELIMS,VALUE,NSFIG,NEXT)

*  Copy of ICH_NUMBD.

      IMPLICIT NONE

      CHARACTER*(*) STRING,DELIMS
      INTEGER IST,NSFIG,NEXT
      DOUBLE PRECISION VALUE

      INTEGER DSA3_VERIF,DSA3_DELIM

      INTEGER NPLIM,NPLIMB,NPLIMO,NPLIMX
      DOUBLE PRECISION ALIM1,ALIM2
      PARAMETER (NPLIM=38,NPLIMB=126,NPLIMO=42,NPLIMX=31)
      PARAMETER (ALIM1=38.2,ALIM2=-38.5)

      LOGICAL DECIMAL,HEX
      INTEGER I,IA,IBASE,IP,IPTR,IZERO,LAST,LIMN
      INTEGER N,NDIGIT,NDMAX,NPSGN,NPWR
      DOUBLE PRECISION BASE,FAC,FRACTN,POWER,RBASE,SIGN
      CHARACTER EXPCH,EXPCH2,EXPCHL,EXPCHL2,ICH

      HEX=.FALSE.
      DECIMAL=.TRUE.
      DSA3_NUMBD=-1
      VALUE=0.0D0
      FRACTN=0.0D0
      SIGN=1.0D0
      NPWR=0
      IZERO=ICHAR('0')
      NSFIG=0

      IPTR=DSA3_VERIF(STRING,IST,' ')
      IF (IPTR.EQ.0) THEN
         NEXT=0
         GO TO 470
      END IF

      LAST=DSA3_DELIM(STRING,IPTR,DELIMS)
      IF (LAST.EQ.0) THEN
         NEXT=0
         LAST=LEN(STRING)
      ELSE
         NEXT=LAST+1
         IF (NEXT.GT.LEN(STRING)) THEN
            NEXT=0
         END IF
         LAST=LAST-1
      END IF
      IF (LAST.LT.IPTR)  GO TO 470

      DSA3_NUMBD=1
      ICH=STRING(IPTR:IPTR)

  330 IF (ICH.EQ.'+')   GO TO 340
      IF (ICH.NE.'-')   GO TO 350
      SIGN=-1.
  340 IPTR=IPTR+1
      IF (IPTR.GT.LAST)  GO TO 470
      ICH=STRING(IPTR:IPTR)

  350 CONTINUE
      IF (ICH.EQ.'''') THEN
         DECIMAL=.FALSE.
         IF (LAST.LT.IPTR+3) GO TO 470
         IF (STRING(LAST-1:LAST-1).NE.'''')  GO TO 470
         ICH=STRING(LAST:LAST)
         LAST=LAST-2
         IPTR=IPTR+1
      ELSE IF (ICH.EQ.'%') THEN
         DECIMAL=.FALSE.
         IF (LAST.LT.IPTR+2)  GO TO 470
         ICH=STRING(IPTR+1:IPTR+1)
         IPTR=IPTR+2
      END IF
      IF (.NOT.DECIMAL) THEN
         IF ((ICH.EQ.'B').OR.(ICH.EQ.'b')) THEN
            BASE=2.0D0
            LIMN=1
            NDMAX=NPLIMB
         ELSE IF ((ICH.EQ.'O').OR.(ICH.EQ.'o')) THEN
            BASE=8.0D0
            LIMN=7
            NDMAX=NPLIMO
         ELSE IF ((ICH.EQ.'X').OR.(ICH.EQ.'x')) THEN
            BASE=16.0D0
            LIMN=22
            NDMAX=NPLIMX
            HEX=.TRUE.
         ELSE
            GO TO 470
         END IF
         ICH=STRING(IPTR:IPTR)
      ELSE
         BASE=10.0D0
         LIMN=9
         NDMAX=NPLIM
      END IF

      IBASE=BASE
      RBASE=1.0D0/BASE
      IF (HEX) THEN
         EXPCH='X'
         EXPCHL='x'
         EXPCH2=EXPCH
         EXPCHL2=EXPCHL
      ELSE
         EXPCH='E'
         EXPCHL='e'
         EXPCH2='D'
         EXPCHL2='d'
      END IF

      DO 360 IP=IPTR,LAST
         ICH=STRING(IP:IP)
         IF (ICH.EQ.'.'.OR.ICH.EQ.EXPCH.OR.ICH.EQ.EXPCHL
     :        .OR.ICH.EQ.EXPCH2.OR.ICH.EQ.EXPCHL2) GO TO 370
  360 CONTINUE
      IP=LAST+1

  370 IA=IP
      FAC=1.
      NDIGIT=1
  380 IA=IA-1
      IF (IA.LT.IPTR)   GO TO 385
      ICH=STRING(IA:IA)
      IF (HEX) CALL CHR_UCASE(ICH)
      N=ICHAR(ICH)-IZERO
      IF (N.LT.0.OR.N.GT.LIMN)   GO TO 470
      IF (HEX.AND.(N.GT.9)) THEN
         IF (N.LT.17)   GO TO 470
         N=N-7
      END IF
      VALUE=VALUE+FLOAT(N)*FAC
      IF (N.NE.0) NSFIG=NDIGIT
      FAC=FAC*BASE
      NDIGIT=NDIGIT+1
      IF (NDIGIT.GT.NDMAX)  GO TO 470
      GO TO 380

  385 ICH=STRING(IP:IP)
      IF (ICH.NE.'.')   GO TO 410
      FAC=RBASE
  400 IP=IP+1
      IF (IP.GT.LAST)   GO TO 440
      ICH=STRING(IP:IP)
      IF (ICH.EQ.EXPCH.OR.ICH.EQ.EXPCHL
     :       .OR.ICH.EQ.EXPCH2.OR.ICH.EQ.EXPCHL2)   GO TO 415
      IF (HEX) CALL CHR_UCASE(ICH)
      N=ICHAR(ICH)-IZERO
      IF (N.LT.0.OR.N.GT.LIMN)   GO TO 470
      IF (HEX.AND.(N.GT.9)) THEN
         IF (N.LT.17)   GO TO 470
         N=N-7
      END IF
      NSFIG=NSFIG+1
      FRACTN=FRACTN+FLOAT(N)*FAC
      FAC=FAC*RBASE
      GO TO 400

  410 CONTINUE
      IF (ICH.NE.EXPCH.AND.ICH.NE.EXPCHL
     :      .AND.ICH.NE.EXPCH2.AND.ICH.NE.EXPCHL2)  GO TO 440
  415 IP=IP+1
      NPSGN=1
      ICH=STRING(IP:IP)
      IF (ICH.EQ.'+')   GO TO 420
      IF (ICH.NE.'-')   GO TO 430
      NPSGN=-1
  420 IP=IP+1
  430 CONTINUE
      NPWR=0
      DO I=IP,LAST
         ICH=STRING(I:I)
         IF (HEX) CALL CHR_UCASE(ICH)
         N=ICHAR(ICH)-IZERO
         IF (N.LT.0.OR.N.GT.LIMN) GO TO 470
         IF (HEX.AND.(N.GT.9)) THEN
            IF (N.LT.17)   GO TO 470
            N=N-7
         END IF
         NPWR=NPWR*IBASE+N
         IF (NPWR.GT.NDMAX)  GO TO 470
      END DO

  440 CONTINUE
      VALUE=(VALUE+FRACTN)*SIGN
      IF (NPWR.EQ.0)   GO TO 460
      FAC=BASE
      IF (NPSGN.LT.0)   FAC=RBASE
      POWER=1.0D0
      DO 450 I=1,NPWR
         POWER=POWER*FAC
  450 CONTINUE
      IF (VALUE.EQ.0.0D0)   GO TO 470
      FAC=DLOG10(ABS(VALUE))+DLOG10(POWER)
      IF (FAC.GT.ALIM1.OR.FAC.LT.ALIM2)   GO TO 470
      VALUE=VALUE*POWER

  460 CONTINUE
      DSA3_NUMBD=0
      GO TO 500

  470 CONTINUE
      VALUE=0.0D0

  500 CONTINUE

      END
