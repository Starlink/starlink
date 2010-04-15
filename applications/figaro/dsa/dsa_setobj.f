C+
      SUBROUTINE DSA_SETOBJ (RECORD,IST,ENV,STATUS)
C
C     D S A _ S E T O B J
C
C     Creates and/or sets data objects as described in a character
C     string.  The string should have the format 'object name = value'
C     with the value optionally enclosed in double quotes if it is a
C     character quantity.  The object name should be one defined in a
C     structure definition file (ie one that will be recognised by
C     DSA_ELEMENT_NAME). This routine will not create structures, so all
C     the upper levels should already exist.
C
C     Note that this routine replaces FIG_SETOBJ, which handled SET
C     commands which explicitly specified the structured name of the
C     object in question, eg 'SET .Z.UNITS = "AB magnitudes"'. It is
C     possible to have EQUATEd variables in a structure definition
C     file whose names are structured, and so it is possible to achieve
C     compatability with the old SET commands by using structure
C     definition files that EQUATE variables called, for example,
C     ".Z.UNITS" to their proper name (".UNITS" for an NDF file, ".Z.UNITS"
C     for a .DST file, in this case).
C
C     Parameters -  (">" input, "<" output)
C
C     (>) RECORD    (Character) String containing the assignment.
C     (>) IST       (Integer) Character in the string (starting from 1)
C                   at which the assignment starts.
C     (>) ENV       (Character) The environment for the data objects
C     (<) STATUS    (Integer) Return status code.  0 => OK, non-zero
C                   indicates an error.  This routine will have output
C                   a suitable error message.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     DTA_CRVAR    (DTA_ package) Create a data object
C     DTA_WRVARC   ( "     "    ) Write a character string to an object
C     DTA_WRVARD   ( "     "    ) Write double precision # "   "   "
C     ICH_DELIM    (ICH_   "    ) Find next char in a specified set
C     ICH_LEN      ( "     "    ) Position of last non-blank char in string
C     ICH_CI       ( "     "    ) Returns an integer formatted into a string
C     ICH_NUMBD    ( "     "    ) Evaluate free-format double precision #
C     ICH_VERIF    ( "     "    ) Find next char not in a specified set
C     PAR_WRUSER   (PAR_   "    ) Write a character string to the user
C     DSA_ELEMENT_NAME   (DSA_  ) Get DTA name of an equated variable
C
C     Example -
C
C     CALL DSA_SETOBJ ('SET UNITS = "AB magnitudes"',4,'SPIKE',
C                                                            STATUS)
C
C     will generate a character data object 'SPIKE.Z.UNITS', which
C     will be set to 'AB magnitudes', assuming that UNITS has been
C     equated to .Z.UNITS in a structure definition file.
C
C     History:
C
C     24th Mar 1991.  Original version, based on FIG_SETOBJ. KS/AAO.
C     25th Mar 1991.  Fixed bug in creation of character items. KS/AAO.
C     24th Sep 1992.  Change name from FIGX_SETOBJ. HME/UoE, Starlink.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IST, STATUS
      CHARACTER*(*) ENV, RECORD
C
C     Functions
C
      INTEGER ICH_DELIM, ICH_LEN, ICH_NUMBD, ICH_VERIF
      CHARACTER ICH_CI*4
C
C     Local variables
C
      LOGICAL CHAR, ERROR
      INTEGER IEND, IPT, ISTCH, IVEN, IVST, LNAME, LREC, NCH, NDIM
      INTEGER NEXT, NSFIG
      DOUBLE PRECISION DVALUE
      CHARACTER CHARS*64, NAME*80, TYPE*8
C
C     First try to delimit the object name
C
      ERROR=.TRUE.
      LREC=ICH_LEN(RECORD)
      IF (IST.GT.LREC) THEN
         CALL PAR_WRUSER('No assignment specified ',STATUS)
         GO TO 400
      END IF
      ISTCH=ICH_VERIF(RECORD,IST,' ')
      IVST=0
      IEND=ICH_DELIM(RECORD,ISTCH,' =')-1
      IF (IEND.GE.0) IVST=ICH_VERIF(RECORD,IEND+1,' =')
      IF (IVST.EQ.0) THEN
         CALL PAR_WRUSER('No value specified in assignment',STATUS)
         GO TO 400
      END IF
C
C     Now try to decode the value part of the assignment
C
      IF (RECORD(IVST:IVST).EQ.'"') THEN
         CHAR=.TRUE.
         IVEN=0
         IF (IVST.LT.LREC) IVEN=ICH_DELIM(RECORD,IVST+1,'"')
         IF (IVEN.EQ.0) THEN
            CALL PAR_WRUSER('Unmatched quote marks in value',STATUS)
            GO TO 400
         END IF
         IVST=IVST+1
         IVEN=IVEN-1
         IF (IVEN.LT.IVST) THEN
            CALL PAR_WRUSER('Null character string specified in value',
     :                                                           STATUS)
            GO TO 400
         END IF
         NDIM=1
         NCH=MAX(40,IVEN-IVST+1)
         TYPE='CHAR'
      ELSE
         STATUS=ICH_NUMBD(RECORD,IVST,' ;*',DVALUE,NSFIG,NEXT)
         IF (STATUS.EQ.-1) THEN
            CALL PAR_WRUSER('Null value in assignment',STATUS)
            GO TO 400
         ELSE IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Invalid numeric value specified',STATUS)
            GO TO 400
         END IF
         CHAR=.FALSE.
         NDIM=0
         NCH=0
         IF (NSFIG.GT.7) THEN
            TYPE='DOUBLE'
         ELSE
            TYPE='FLOAT'
         END IF
      END IF
C
C     Having got value and name for object, attempt to set it
C
      STATUS=0
      CALL DSA_ELEMENT_NAME(ENV,RECORD(ISTCH:IEND),NAME,STATUS)
      IF (STATUS.NE.0) GO TO 400
      IF (CHAR) THEN
         LNAME=ICH_LEN(NAME)
         NAME(LNAME+1:)='['//ICH_CI(NCH)
         IPT=ICH_LEN(NAME)
         NAME(IPT+1:IPT+1)=']'
         CALL DTA_CRVAR(NAME,TYPE,STATUS)
         NAME(LNAME+1:)=' '
         IF (IVEN-IVST+1.LT.40) THEN
            CHARS=' '
            CALL DTA_WRVARC(NAME,40,CHARS,STATUS)
         END IF
         CALL DTA_WRVARC(NAME,IVEN-IVST+1,RECORD(IVST:IVEN),STATUS)
      ELSE
         CALL DTA_CRVAR(NAME,TYPE,STATUS)
         CALL DTA_WRVARD(NAME,1,DVALUE,STATUS)
      END IF
      IF (STATUS.NE.0) THEN
         CALL DTA_ERROR(STATUS,CHARS)
         CALL PAR_WRUSER('Unable to assign '//NAME(:ICH_LEN(NAME)),
     :                                                      STATUS)
         CALL PAR_WRUSER(CHARS,STATUS)
         GO TO 400
      END IF
C
C     All done OK
C
      ERROR=.FALSE.
C
C     Exit (with or without error)
C
  400 CONTINUE
      IF (ERROR) THEN
         CALL PAR_WRUSER(RECORD(:ICH_LEN(RECORD)),STATUS)
         STATUS=1
      ELSE
         STATUS=0
      END IF
C
      END
