! @(#)lutasc.prg	8.1.1.1 (ESO-IPG) 8/31/94 15:51:21 
! ++++++++++++++
!  
!  Midas procedure lutasc.prg 
!  to convert Midas LUT to ASCII file which can later be used to regenerate
!  that LUT via: CREATE/TABLE using lut.fmt as Format file
! 
!  xyz.lut  ->  xyz.lasc
! 
!  KB  910315
! 
! ++++++++++++++
! 
ASSIGN/PRINT FILE lut.lis
! 
-COPY 'P1'.lut temp.tbl
NAME/COLUMN temp :RED F12.5
NAME/COLUMN temp :GREEN F12.5
NAME/COLUMN temp :BLUE F12.5
PRINT/TABLE temp :RED :GREEN :BLUE 
! 
$ sh $MIDASHOME/$MIDVERS/systab/ascii/display/lutasc.sh lut.lis 'P1'.lasc
ASSIGN/PRINT T
! 
$ rm lut.lis
$ rm temp.tbl
! 
WRITE/OUT table 'P1'.lut processed ...
