*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*-----------------------------------------------------------------------

      SUBROUTINE gen_doarry (opnd_addr1, opnd_type1,
     &                       opnd_addr2, opnd_type2,
     &                       address, ierr)

      IMPLICIT  none
      INCLUDE 'CNF_PAR'

*     Formal parameters

      INTEGER*4 opnd_addr1
      CHARACTER opnd_type1*(*)
      INTEGER*4 opnd_addr2
      CHARACTER opnd_type2*(*)
      INTEGER*4 address
      INTEGER*4 ierr

*     Local variables

      INTEGER*4 arg
      INTEGER*4 nbytes

*     Functions:

      INTEGER*4 gen_ilen

*  ok, go..

*     Only arrays implemented at present.
*     First get the array index in integer type:

      READ (opnd_type1(2:gen_ilen(opnd_type1)), '(I3)') nbytes
      CALL gen_cvt_type (%val(cnf_pval(opnd_addr2)), opnd_type2, nbytes,
     &                    arg,            'I4',        4,      ierr)

*     Determine the length of individual array element

      READ (opnd_type1(2:gen_ilen(opnd_type1)), '(I3)') nbytes
CD    Print *,'     offset in bytes =', nbytes*(arg-1)

*     Offset the address to the desired one:

      address = address + nbytes*(arg-1)
CD    Print *,'     new address ', address

*     copy value from this address to reserved part of workspace

      CALL xcopy (nbytes, %val(cnf_pval(address)), 
     :            %val(cnf_pval(opnd_addr1)))

      RETURN
      END
