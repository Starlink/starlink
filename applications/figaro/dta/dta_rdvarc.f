C+
      SUBROUTINE DTA_RDVARC (NAME,NITEM,STRING,STATUS)
C
C     D T A _ R D V A R C
C
C     Reads data items from a data structure object.  This is
C     a specific version of the generic routine DTA_RDVAR
C     which reads data of type CHAR.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the object from which
C                  data is to be read. This should be in the
C                  standard data system format, ie name
C                  components separated by dots followed by
C                  optional dimensional information enclosed
C                  in square brackets.  Ideally the name should
C                  have been created by a call to DTA_CRVAR.
C     (>) NITEM    (Integer) The number of data items to be
C                  read. Note that this is an item count,
C                  and not a byte count.
C     (<) STRING   (Character) The data to be read.  Note that
C                  only the number of characters specified by
C                  NITEM will be read.  If STRING is longer than
C                  NITEM, remaining characters will be unchanged.
C                  If it is shorter, not all the characters
C                  specified by NITEM will be read.  Note also
C                  that the system will not attempt to convert
C                  numeric data into character form.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK.  Error codes may be returned by
C                  lower level routines, and there are many
C                  possibilities.  Use DTA_ERROR to produce
C                  error messages based on a non-zero STATUS.
C                  Most common errors will be
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVNAM => Object name is invalid
C                  DTA_INVDIM => Dimension specification is invalid
C                  DTA_CHRCVT => Attempt to read numeric data
C
C     Note limitation of current implementation: Character arrays are
C     held as arrays of character strings, not individual characters.
C     If a multi-dimensional array is being read from, then this
C     should be thought of as an array of strings, and in this case
C     STRING should also be an array of strings.  In this case, NITEM
C     needs to be set to the number of strings to be transferred times the
C     number of characters in each string of the data object (NOT the
C     length of STRING).  It is not possible (at the moment) to transfer
C     a subset of a string. That is, the only transfers allowed are ones
C     where the first index specified in NAME (if any) is 1. The transfer
C     that takes place is similar to a Fortran string assignment between
C     the strings in the data structure (whose length is the first dimension
C     of the original character array) and the string(s) passed as STRING.
C-
C     Functions / subroutines used -
C
C     DTA_PRETR    (DTA_ package) Preliminary set up for data transfer
C     DTA_HDSERC   ( "      "   ) Convert HDS error code to DTA code
C     DAT_GET      (HDS     "   ) Read data from structure component
C     DAT_ANNUL    ( "      "   ) Annul locator
C     EMS_BEGIN    (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL    ( "    "     ) Clear current EMS error status.
C     EMS_END      ( "    "     ) End current reporting environment.
C
C                                      KS / CIT 28th Oct 1982
C     Modified:
C
C     18th March 1986.  KS / AAO. Call to DTA_RDVAR modified to
C                       reflect changes required as a result of HDS
C                       conversion.  STRING now passed by
C                       descriptor, rather than by ref.
C     7th Feb    1990.  KS / AAO. Restriction on transfers crossing
C                       string boundaries has been removed.  Comments
C                       modified accordingly.
C     10th Jan   1992.  KS / AAO. Entire routine recoded as a modified
C                       version of DTA_RDVAR, rather than just calling
C                       DTA_RDVAR. This is necessary because the questionable
C                       passing of a character string as though it were a
C                       numeric array, which worked on a VAX, was not a
C                       portable construct.
C     24th  Jan  1992.  KS / AAO. Calls to EMS added to control error reporting.
C     12th  Mar  1993.  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER NITEM,STATUS
      CHARACTER*(*) STRING
C
C     DTA_ system parameters.  Used is -
C
C     DST_MAXDIM    Maximum allowed number of dimensions
C
      INCLUDE 'DTASDEF'
C
C     Data type definitions.  Used is:
C
C     TYP_DSCHAR    Code for CHAR data type
C
      INCLUDE 'DTATCON'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      LOGICAL UNDEF,TEMPLOC
      INTEGER EMSTAT,NLDIM,LDIMS(DST_MAXDIM),STPOST
      CHARACTER HDSTYPE*16,LOC*(DAT__SZLOC)
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Perform preliminary processing - check NAME, locate it
C     in structures, get suitable locator for transfer.
C
      CALL DTA_PRETR(NAME,NITEM,TYP_DSCHAR,LOC,HDSTYPE,NLDIM,LDIMS,
     :                                    UNDEF,TEMPLOC,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Read data into STRING, or set STRING to blank if the
C     object is undefined.
C
      IF (.NOT.UNDEF) THEN
         CALL DAT_GET(LOC,HDSTYPE,NLDIM,LDIMS,STRING,STATUS)
         IF (STATUS.NE.0) THEN
            CALL DTA_HDSERC(STATUS)
            GO TO 600
         END IF
      ELSE
         STRING=' '
      END IF
C
C     Tidy up afterwards, by annulling the locator used, if it
C     was flagged as temporary.
C
  600 CONTINUE
      IF (TEMPLOC) THEN
         STPOST=0
         CALL DAT_ANNUL(LOC,STPOST)
         IF (STATUS.EQ.0) THEN
            STATUS=STPOST
            CALL DTA_HDSERC(STATUS)
         END IF
      END IF
C
C     On way out, clear any EMS errors and revert to previous environment.
C
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

