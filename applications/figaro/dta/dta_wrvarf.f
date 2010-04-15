C+
      SUBROUTINE DTA_WRVARF (NAME,NITEM,FARRAY,STATUS)
C
C     D T A _ W R V A R F
C
C     Writes data items to a data structure object.  This is
C     a specific version of the generic routine DTA_WRVAR
C     which writes data of type FLOAT.
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
C     (>) FARRAY   (Real array) The data to be written.
C                  The data is converted into the form required
C                  by the data structure before being written.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK.  Error codes may be returned by
C                  lower level routines, and there are many
C                  possibilities.  Use DTA_ERROR to produce
C                  error messages based on a non-zero STATUS.
C                  Most common errors will be
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVNAM => Object name is invalid
C                  DTA_INVDIM => Dimension specification is invalid
C                  DTA_BADCON => Data was transfered, but there were
C                                conversion errors.
C-
C     Functions / subroutines used -
C
C     DTA_WRVAR   (DTA_ package) Write data to data structure.
C
C                                      KS / CIT 27th Oct 1982
C     Modified:
C
C     10th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER NITEM,STATUS
      REAL FARRAY(NITEM)
C
C     Data type definitions
C
C     TYP_DSFLOAT   Code for FLOAT data type
C
      INCLUDE 'DTATCON'
C
      CALL DTA_WRVAR(NAME,NITEM,TYP_DSFLOAT,FARRAY,STATUS)
C
      END

