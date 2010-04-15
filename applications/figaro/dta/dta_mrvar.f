C+
      SUBROUTINE DTA_MRVAR (NAME,NITEM,ITYPE,POINTER,STATUS)
C
C     D T A _ M R V A R
C
C     Generic routine for obtaining mapped read-only access to
C     an object in the data structure.  This routine returns a
C     pointer to a range of virtual addresses which are mapped onto
C     all or part of the specified data object.  The pointer
C     may be to a range of addresses which have been mapped
C     directly onto the data structure disk file itself, or
C     if conversion or alignment changes were needed, will be to
C     a range of addresses containing the converted or
C     aligned data.
C
C     Parameters -   (">" input, "<" output )
C
C     (>) NAME     (Character) The object name, in the standard
C                  data structure format.  Should end with a
C                  blank or the string end, and is case
C                  insignificant.
C     (>) NITEM    (Integer) The number of elements to be
C                  read from the data structure.
C     (>) ITYPE    (Integer) The type code for the data to
C                  be read. Should be one of the TYP_DS..
C                  codes (not a conversion code).
C     (<) POINTER  (Integer) Pointer to the mapped data.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK
C                  Lower level routines may return other error codes
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     DTA_MVAR     (DTA_ package) General mapping routine
C
C                                         KS / CIT 16th March 1983
C     Modified:
C
C     23rd Sept 1985  KS / AAO. Uses DTA_FMTCON instead of STL_FMTCON
C     28th March 1986 KS / AAO. Rewritten for HDS version of the DTA
C                     package.  All the work is now done by DTA_MVAR.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER NITEM,ITYPE,POINTER,STATUS
C
      CALL DTA_MVAR(NAME,NITEM,ITYPE,'READ',POINTER,STATUS)
C
      END

