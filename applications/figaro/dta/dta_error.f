C+
      SUBROUTINE DTA_ERROR (STATUS,ERROR)
C
C     D T A _ E R R O R
C
C     Generates an error message for the data structure
C     routines, based on a status code returned by them.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) STATUS    (Integer) A status code returned by one
C                   of the DAT_ routines.
C     (<) ERROR     (Character) Returned with a string describing
C                   the error.
C-
C     Common variables used -
C
C     (>) BADCONS   Number of conversion errors
C     (>) HDSTAT    Last HDS error code
C
C     All in common block PROBE.
C
C     Subroutines / functions used -
C
C     ICH_FOLD         (ICH package) Convert to upper case
C     DTA_HDSTXT       (DTA package) Get text for an HDS error code
C
C     This is a rather nasty looking program, but it shouldn't
C     need to be called very often..
C                                          KS / CIT 3rd March 1983
C     Modified:
C
C     11th Mar 1986.  KS / AAO.  HDS error codes added.
C     20th Aug 1987.  KS / AAO.  DTA_HDSTXT replaced by EXC_$MSG.
C     10th Jun 1988.  KS / AAO.  DTA_INVCPY added.
C     8th  Nov 1988.  KS / AAO.  DTA_UNSAFE added.
C     8th  Jan 1992.  KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN.
C     21st Jan 1992.  KS / AAO.  EXC$MSG no longer supported by HDS.
C                     DTA_HSDTXT now used again. No DTA routine now produces
C                     the DTA_RMSERR or DTA_SSERR codes, so these have been
C                     removed.
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C     12th Jan 1994.  HME / UoE, Starlink.  Change message for bad file.
C                     Formerly 'File was not produced by this data system'
C                     is now 'File is not HDS or cannot be overwritten'.
C                     Unix has no version numbers and HDS_NEW refuses to
C                     overwrite a file that is currently open.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER STATUS
      CHARACTER*(*) ERROR
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Functions and local variables
C
      INTEGER ICH_FOLD
      INTEGER INVOKE
C
C     Probe block -
C
      INCLUDE 'DTAPROBE'
C
      IF (STATUS.EQ.0) THEN
         ERROR=' '
C
      ELSEIF (STATUS.EQ.DTA_TRUNC) THEN
         ERROR='Characters truncated during object name generation'
C
      ELSEIF (STATUS.EQ.DTA_INVDIM) THEN
         ERROR='Name has invalid dimension specifications'
C
      ELSEIF (STATUS.EQ.DTA_INVNAM) THEN
         ERROR='Name has invalid component(s)'
C
      ELSEIF (STATUS.EQ.DTA_TOODEEP) THEN
         ERROR='Name has too many components/dimensions'
C
      ELSEIF (STATUS.EQ.DTA_NOFILE) THEN
         ERROR='No file open/available'
C
      ELSEIF (STATUS.EQ.DTA_INVPAR) THEN
         ERROR='Invalid parameter in subroutine call'
C
      ELSEIF (STATUS.EQ.DTA_CFULL) THEN
         ERROR='Name cache is full'
C
      ELSEIF (STATUS.EQ.DTA_CNTINT) THEN
         ERROR='Name cache is not initialised'
C
      ELSEIF (STATUS.EQ.DTA_BADFILE) THEN
         ERROR='File is not HDS or cannot be overwritten'
C
      ELSEIF (STATUS.EQ.DTA_BADVER) THEN
         ERROR='File is not compatible with this version of the system'
C
      ELSEIF (STATUS.EQ.DTA_INVLEV) THEN
         ERROR='Level of component names is invalid'
C
      ELSEIF (STATUS.EQ.DTA_EXIST) THEN
         ERROR='Attempt to create existing data object'
C
      ELSEIF (STATUS.EQ.DTA_NOENV) THEN
         ERROR='No environment available for object'
C
      ELSEIF (STATUS.EQ.DTA_NOTFND) THEN
         ERROR='Search for non-existent object in data structure'
C
      ELSEIF (STATUS.EQ.DTA_INVTYP) THEN
         ERROR='Invalid type specification code'
C
      ELSEIF (STATUS.EQ.DTA_INTERR) THEN
         ERROR='Internal error in structure. File may be corrupt'
C
      ELSEIF (STATUS.EQ.DTA_RNGERR) THEN
         ERROR='Transfer exceeds defined range of object'
C
      ELSEIF (STATUS.EQ.DTA_WRSTRUC) THEN
         ERROR='Attempt to write directly to a structure record'
C
      ELSEIF (STATUS.EQ.DTA_CHRCVT) THEN
         ERROR='Attempted character to numeric conversion'
C
      ELSEIF (STATUS.EQ.DTA_RDSTRUC) THEN
         ERROR='Attempt to read a structure record directly'
C
      ELSEIF (STATUS.EQ.DTA_MAXMAP) THEN
         ERROR='Maximum number of objects already mapped'
C
      ELSEIF (STATUS.EQ.DTA_MPSTRUC) THEN
         ERROR='Attempt to map a structure record directly'
C
      ELSEIF (STATUS.EQ.DTA_BADCON) THEN
         WRITE (ERROR,'(A,I10,A)') 'Bad conversions (',BADCONS,')'
C
      ELSEIF (STATUS.EQ.DTA_NOTMAP) THEN
         ERROR='Object has not been mapped'
C
      ELSEIF (STATUS.EQ.DTA_DIFLEV) THEN
         ERROR='Attempt to rename to a different level'
C
      ELSEIF (STATUS.EQ.DTA_DIFENV) THEN
         ERROR='Attempt to rename to a different environment'
C
      ELSEIF (STATUS.EQ.DTA_INCDIM) THEN
         ERROR='Attempt to rename with more dimensions'
C
      ELSEIF (STATUS.EQ.DTA_INCSIZ) THEN
         ERROR='Attempt to increase object size by renaming'
C
      ELSEIF (STATUS.EQ.DTA_RDONLY) THEN
         ERROR='Attempt to write to a write-protected file'
C
      ELSEIF (STATUS.EQ.DTA_DELELM) THEN
         ERROR='Attempt to delete an array element'
C
      ELSEIF (STATUS.EQ.DTA_DELTOP) THEN
         ERROR='Attempt to delete a top level object'
C
      ELSEIF (STATUS.EQ.DTA_STRARY) THEN
         ERROR='Cannot handle an array of structures'
C
      ELSEIF (STATUS.EQ.DTA_INVCHR) THEN
         ERROR='Unsupported type of string transfer'
C
      ELSEIF (STATUS.EQ.DTA_RENTOP) THEN
         ERROR='Attempt to rename a top-level object'
C
      ELSEIF (STATUS.EQ.DTA_INVCPY) THEN
         ERROR='Attempt to copy incompatible objects'
C
      ELSEIF (STATUS.EQ.DTA_UNSAFE) THEN
         ERROR='Object is not safe to access directly'
C
      ELSEIF (STATUS.EQ.DTA_HDSERR) THEN
         CALL DTA_HDSTXT(HDSTAT,ERROR)
C
      ELSE
         WRITE (ERROR,'(A,Z10,A)') ' Code = ',STATUS,
     :                      ' is not a valid data system code'
      END IF
C
      INVOKE=ICH_FOLD(ERROR(1:1))
C
      END

