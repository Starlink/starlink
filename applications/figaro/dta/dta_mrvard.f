C+
      SUBROUTINE DTA_MRVARD (NAME,NITEM,POINTER,STATUS)
C
C     D T A _ M R V A R D
C
C     Obtains a pointer to the start of a range of virtual
C     addresses which contains a named data object.  The
C     object is mapped read-only and should not be written to.
C     This is generally by far the most efficient means of
C     accessing a large data array, since it generally will
C     not require any I/O at all; the data system can simply
C     obtain workspace for the data and map that workspace
C     directly onto the data object on disk.  If the data
C     is not on disk in the form required by the call, it
C     will be converted, if possible.  The array cannot be
C     accessed directly by a Fortran calling routine, but
C     the pointer may be passed by value to a subroutine
C     that expects an array.  This is a specific version of
C     the generic routine DTA_MRVAR, which maps data of type
C     DOUBLE.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the object
C                  which is to be mapped. This should be in the
C                  standard data system format, ie name
C                  components separated by dots followed by
C                  optional dimensional information enclosed
C                  in square brackets.  Ideally the name should
C                  have been created by a call to DTA_CRVAR.
C     (>) NITEM    (Integer) The number of data items to be
C                  mapped. Note that this is an item count,
C                  and not a byte count.
C     (<) POINTER  (Integer) The pointer to the mapped data.
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
C                  DTA_BADCON => Data was mapped, but there were
C                                conversion errors.
C-
C     Functions / subroutines used -
C
C     DTA_MRVAR   (DTA_ package) Map data from data structure.
C
C                                      KS / CIT 16th March 1983
C     Modified:
C
C     20th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER NITEM,STATUS
      INTEGER POINTER
C
C     Data type definitions
C
C     TYP_DSDOUBLE   Code for DOUBLE data type
C
      INCLUDE 'DTATCON'
C
      CALL DTA_MRVAR(NAME,NITEM,TYP_DSDOUBLE,POINTER,STATUS)
C
      END

