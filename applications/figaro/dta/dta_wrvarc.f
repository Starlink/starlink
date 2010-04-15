C+
      SUBROUTINE DTA_WRVARC (NAME,NITEM,STRING,STATUS)
C
C     D T A _ W R V A R C
C
C     Writes data items to a data structure object.  This is
C     a specific version of the generic routine DTA_WRVAR
C     which writes data of type CHARACTER.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the object to which
C                  data is to be written.  This should be in the
C                  standard data system format, ie name
C                  components separated by dots followed by
C                  optional dimensional information enclosed
C                  in square brackets.  Ideally the name should
C                  have been created by a call to DTA_CRVAR.
C     (>) NITEM    (Integer) The number of data items to be
C                  written.  Note that this is an item count,
C                  and not a byte count.
C     (>) STRING   (Character) The character string to be written.
C                  Note that the system will not convert character
C                  data to a numeric form, and an error will
C                  result if this routine is used to write to an
C                  object whose type is numeric.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK.  Error codes may be returned by
C                  lower level routines, and there are many
C                  possibilities.  Use DTA_ERROR to produce
C                  error messages based on a non-zero STATUS.
C                  Most common errors will be
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVNAM => Object name is invalid
C                  DTA_INVDIM => Dimension specification is invalid
C                  DTA_CHRCVT => Character to numeric conversion
C                                implied by this transfer.
C
C     Note limitation of current implementation: Character arrays are
C     held as arrays of character strings, not individual characters.
C     If a multi-dimensional array is being written to, then this
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
C     DAT_PUT      (HDS     "   ) Write data into structure component
C     DAT_ANNUL    ( "      "   ) Annul locator
C     EMS_BEGIN    (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL    ( "    "     ) Clear current EMS error status.
C     EMS_END      ( "    "     ) End current reporting environment.
C
C                                      KS / CIT 27th Oct 1982
C     Modified:
C
C     18th March 1986.  KS / AAO. Call to DTA_WRVAR modified to
C                       reflect changes required as a result of HDS
C                       conversion.  STRING now passed by
C                       descriptor, rather than by ref.
C     7th Feb    1990.  KS / AAO. Restriction on transfers crossing
C                       string boundaries has been removed.
C     10th Jan   1992.  KS / AAO. Rewritten entirely as part of port to SUN.
C                       The questionable way the VMS version called DTA_WRVAR
C                       passing STRING to a numeric argument is not portable,
C                       so the whole routine has been redone using DTA_WRVAR
C                       as a template.
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
C     Data type definitions.  Used is:
C
C     TYP_DSCHAR    Code for CHAR data type
C
      INCLUDE 'DTATCON'
C
C     DTA_ system parameters.  Used is -
C
C     DST_MAXDIM    Maximum allowed number of dimensions
C
      INCLUDE 'DTASDEF'
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
C     Perform the transfer
C
      CALL DAT_PUT(LOC,HDSTYPE,NLDIM,LDIMS,STRING,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
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

