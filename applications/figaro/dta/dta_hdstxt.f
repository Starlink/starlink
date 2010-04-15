C+
      SUBROUTINE DTA_HDSTXT (STATUS,ERROR)
C
C     D T A _ H D S T X T
C
C     Generates an error message given an HDS error code.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) STATUS    (Integer) A status code returned by one
C                   of the HDS_ or DAT_ routines.
C     (<) ERROR     (Character) Returned with a string describing
C                   the error.
C
C     Common variables used - None.
C
C     Subroutines / functions used - None.
C
C                                          KS / AAO 11th March 1986
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
      INTEGER STATUS
      CHARACTER*(*) ERROR
C
C     Error codes -
C
      INCLUDE 'HDSCODES'
C
      IF (STATUS.EQ.0) THEN
         ERROR=' '
C
      ELSEIF (STATUS.EQ.DAT__ACCON) THEN
         ERROR='Access conflict'
C
      ELSEIF (STATUS.EQ.DAT__ACTIV) THEN
         ERROR='HDS is already active'
C
      ELSEIF (STATUS.EQ.DAT__BOUND) THEN
         ERROR='Outside bounds of object'
C
      ELSEIF (STATUS.EQ.DAT__COMEX) THEN
         ERROR='Component already exists'
C
      ELSEIF (STATUS.EQ.DAT__CONER) THEN
         ERROR='Conversion error'
C
      ELSEIF (STATUS.EQ.DAT__CONIN) THEN
         ERROR='Conversion invalid'
C
      ELSEIF (STATUS.EQ.DAT__DELIN) THEN
         ERROR='Deletion invalid'
C
      ELSEIF (STATUS.EQ.DAT__DIMIN) THEN
         ERROR='Dimensions invalid'
C
      ELSEIF (STATUS.EQ.DAT__ERACT) THEN
         ERROR='Error activating DAT'
C
      ELSEIF (STATUS.EQ.DAT__EREXH) THEN
         ERROR='Error establishing DAT exit handler'
C
      ELSEIF (STATUS.EQ.DAT__EXCPA) THEN
         ERROR='Too many data parameters'
C
      ELSEIF (STATUS.EQ.DAT__FATAL) THEN
         ERROR='Fatal internal error'
C
      ELSEIF (STATUS.EQ.DAT__FILCK) THEN
         ERROR='File locked'
C
      ELSEIF (STATUS.EQ.DAT__FILCL) THEN
         ERROR='File close error'
C
      ELSEIF (STATUS.EQ.DAT__FILCR) THEN
         ERROR='File create error'
C
      ELSEIF (STATUS.EQ.DAT__FILIN) THEN
         ERROR='File invalid'
C
      ELSEIF (STATUS.EQ.DAT__FILMP) THEN
         ERROR='File mapping error'
C
      ELSEIF (STATUS.EQ.DAT__FILND) THEN
         ERROR='File not deleted'
C
      ELSEIF (STATUS.EQ.DAT__FILNF) THEN
         ERROR='File not found'
C
      ELSEIF (STATUS.EQ.DAT__FILNX) THEN
         ERROR='File not extended'
C
      ELSEIF (STATUS.EQ.DAT__FILPR) THEN
         ERROR='File protected'
C
      ELSEIF (STATUS.EQ.DAT__FILRD) THEN
         ERROR='File read error'
C
      ELSEIF (STATUS.EQ.DAT__FILWR) THEN
         ERROR='File write error'
C
      ELSEIF (STATUS.EQ.DAT__GRPIN) THEN
         ERROR='Group invalid'
C
      ELSEIF (STATUS.EQ.DAT__INCHK) THEN
         ERROR='Integrity check'
C
      ELSEIF (STATUS.EQ.DAT__ISMAP) THEN
         ERROR='Data currently mapped'
C
      ELSEIF (STATUS.EQ.DAT__ISOPN) THEN
         ERROR='Parameter is currently active'
C
      ELSEIF (STATUS.EQ.DAT__LOCIN) THEN
         ERROR='Locator invalid'
C
      ELSEIF (STATUS.EQ.DAT__MODIN) THEN
         ERROR='Mode invalid'
C
      ELSEIF (STATUS.EQ.DAT__NAMIN) THEN
         ERROR='Name invalid'
C
      ELSEIF (STATUS.EQ.DAT__NOMAP) THEN
         ERROR='Not mapped'
C
      ELSEIF (STATUS.EQ.DAT__NOMEM) THEN
         ERROR='Insufficient memory available'
C
      ELSEIF (STATUS.EQ.DAT__OBJNF) THEN
         ERROR='Object not found'
C
      ELSEIF (STATUS.EQ.DAT__OBJIN) THEN
         ERROR='Object invalid'
C
      ELSEIF (STATUS.EQ.DAT__PRMAP) THEN
         ERROR='Primitive data mapped'
C
      ELSEIF (STATUS.EQ.DAT__RELIN) THEN
         ERROR='Relationship invalid'
C
      ELSEIF (STATUS.EQ.DAT__STKOF) THEN
         ERROR='Recursion stack overflow'
C
      ELSEIF (STATUS.EQ.DAT__SUBIN) THEN
         ERROR='Subscripts invalid'
C
      ELSEIF (STATUS.EQ.DAT__TRUNC) THEN
         ERROR='Text truncated'
C
      ELSEIF (STATUS.EQ.DAT__TYPIN) THEN
         ERROR='Type invalid'
C
      ELSEIF (STATUS.EQ.DAT__UNKPA) THEN
         ERROR='Unknown data parameter'
C
      ELSEIF (STATUS.EQ.DAT__UNSET) THEN
         ERROR='Primitive data undefined'
C
      ELSEIF (STATUS.EQ.DAT__VERMM) THEN
         ERROR='Version mismatch'
C
      ELSEIF (STATUS.EQ.DAT__WEIRD) THEN
         ERROR='Unknown error'
C
      ELSE
         WRITE (ERROR,'(A,Z10,A)') ' Code = ',STATUS,
     :                      ' is not a valid HDS error code'
      END IF
C
      END

