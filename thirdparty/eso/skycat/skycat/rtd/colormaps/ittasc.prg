! @(#)ittasc.prg	8.1.1.1 (ESO-IPG) 8/31/94 15:51:57 
! ++++++++++++++
!  
!  Midas procedure ittasc.prg 
!  to convert Midas ITT to ASCII file which can later be used to regenerate
!  that ITT via: CREATE/TABLE using itt.fmt as Format file
! 
!  xyz.itt  ->  xyz.iasc
! 
!  KB  910315
! 
! ++++++++++++++
! 
ASSIGN/PRINT FILE itt.lis
! 
-COPY 'P1'.itt temp.tbl
NAME/COLUMN temp :ITT F12.5
PRINT/TABLE temp :ITT
! 
$ sh $MIDASHOME/$MIDVERS/systab/ascii/display/ittasc.sh itt.lis 'P1'.iasc
ASSIGN/PRINT T
! 
$ rm itt.lis
$ rm temp.tbl
! 
WRITE/OUT table 'P1'.itt processed ...
