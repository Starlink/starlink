C--------------------------------------------------------------------
C
C                 STAKPAR.INC
C                 -----------

C   Include file to define the STACK common block
C   Contains 5 integer*4 words

      INTEGER*4 LSTK          ! Length of a stack postion (inc. header)
      INTEGER*4 JSTK          ! Number of scans in the stack
      INTEGER*4 JTOP          ! Top stack position in use
      INTEGER*4 IDAT2         ! Position in STACK of start of Y-reg. data
      LOGICAL*4 XCLEAR        ! If XCLEAR => bottom stack position is empty

      COMMON /STAKPAR/
     &                LSTK,   JSTK,    JTOP,   IDAT2,    XCLEAR

      SAVE   /STAKPAR/
C--------------------------------------------------------------------
