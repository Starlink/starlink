C+
      SUBROUTINE DTA_RDVARI (NAME,NITEM,IARRAY,STATUS)
C
C     D T A _ R D V A R I
C
C     Reads data items from a data structure object.  This is
C     a specific version of the generic routine DTA_RDVAR
C     which reads data of type INT.
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
C     (<) IARRAY   (Integer array) The data to be read.
C                  The data is converted into the form required
C                  by the call from the form it is held in by
C                  the data system, if necessary.
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
C     DTA_RDVAR   (DTA_ package) Read data from data structure.
C
C                                      KS / CIT 28th Oct 1982
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
      INTEGER IARRAY(NITEM)
C
C     Data type definitions
C
C     TYP_DSINT    Code for INT data type
C
      INCLUDE 'DTATCON'
C
      CALL DTA_RDVAR(NAME,NITEM,TYP_DSINT,IARRAY,STATUS)
C
      END

