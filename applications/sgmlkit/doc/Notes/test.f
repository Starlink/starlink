*+
*   <ROUTINENAME>
*     try_xxx
*     <othernames><name>try_r<name>try_i</othernames>
*
*   <purpose>This is a short description of the routine
*   
*   <DESCRIPTION>
*     To illustrate a simple routine
*
*   <RETURNVALUE none>
*
*   <ARGUMENTLIST>
*   <parameter given>
*     first = real(5)
*     An array of real numbers
*   <parameter givenandreturned>
*     status = integer
*     The global status
*
*   <authorlist>
*   <authorref id=ng>
*
*   <HISTORY>
*     <change date='09-Feb-1999' author=ng>
*     Original version
*-
      subroutine try (first, status)
      real first(5)
      integer status

      status = 1
      end
      
