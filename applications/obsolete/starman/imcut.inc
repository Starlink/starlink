CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCUT.INC
C
C  Include file for IMCUT


      integer   nxs		!X start of copy area
      integer   nxe		!X end of copy area
      integer   nxst		!X step in copy area
      integer   nys		!Y start of copy area
      integer   nye		!Y end of copy area
      integer   nyst		!Y step in copy area

      character*8  sample	!Sample method 'sample:highest:average'

      logical   xrev		!Reverse X diecrtion?
      logical   yrev		!Reverse Y diecrtion?
      logical   ignore          !Ignore INVALID pixels (true) or set out to INVALID (false)

      logical  imcut_dum1 
CX      logical  imcut_dum2, imcut_dum3

      common / imcuta /   nxs,     nxe,  nxst,  nys,    nye, 
     +                    nyst

      common /imcutb / xrev,  yrev,  ignore, imcut_dum1

      common /imcutc / sample

