C+
C                         P A R _ I N I T
C  Routine name:
C     PAR_INIT
C
C  Function:
C     Initialises the (F)PAR COMMON block.
C
C  Description:
C     This routine must be called before an application calls other
C     routines of the (F)PAR system. It puts the command name into the
C     COMMON block used by (F)PAR and resets the ABORT flag in that
C     COMMON block.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C      CALL PAR_INIT (COMMAND,STRING,ISTART,BATCH,STATUS)
C
C  Parameters:    (">" input, "<" output)
C
C     (>) COMMAND    (Fixed string,descr) The name of the command.  This
C                    is put into the COMMON block variable COM.
C     (>) STRING     (Fixed string,descr) Ignored.
C     (>) ISTART     (Integer,ref) Ignored.
C     (>) BATCH      (Logical,ref) The batch mode flag. The value is
C                    stored in the common block and can be retrieved
C                    as returned value of PAR_BATCH.
C     (<) STATUS     (Integer,ref) Returns a status code.
C                    0 => All OK.
C                    1 => Errors detected.
C
C  Note:
C     PAR_INIT will normally be called as part of the automatic startup
C     of a Figaro application.  It will not normally be called by
C     user-written code.
C
C  History:
C     1st June 1984  Original Caltech version brought to AAO.
C     2nd May 1986.  KS / AAO.  Fudge involving ISTART being passed
C                    as zero added, for use with callable Figaro.
C     13th May 1986. KS / AAO.  Now, for efficiency, uses Fortran
C                    I/O into structures rather than the DTA_ package
C                    to read the .PAR files.  Note that the .PAR files
C                    used by this version are incompatible with those
C                    used by its predecessor.
C     2nd June 1986. KS / AAO.  Now uses Logical unit numbers held in
C                    common to make it easier to shut down the system.
C     26th Aug 1986. KS / AAO.  Support for NOVAR option added.
C     1st  Sept 1988 KS / AAO. Abort flag cleared.
C     6th  Sept 1988 KS / AAO. New search path for .EXE files used.
C     20th Jan  1989 KS / AAO. Now uses DOUBLE internally for numerics.
C                    Now uses the parameter from PARBLK (MAXVEC) instead
C                    of its own NVMAX.
C     3rd  Mar  1991 KS / AAO. Support for repeated parameter input from
C                    file added.
C     11th Mar  1991 KS / AAO. VPTR moved to common for use by PAR_VARRY.
C     12th Aug  1992 HME / UoE, Starlink. New include file.
C     18th Nov  1992 HME / UoE, Stalrink.  Set the new batch flag in the
C                    common block.
C+
      SUBROUTINE PAR_INIT (COMMAND,STRING,ISTART,MODE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL MODE
      INTEGER ISTART,STATUS
      CHARACTER*(*) COMMAND,STRING
C
C     Parameter common block
C
      INCLUDE 'PARBLK'
C
      COM    = COMMAND
      ABORT  = .FALSE.
      BATCH  = MODE
      STATUS = 0
C
      END
