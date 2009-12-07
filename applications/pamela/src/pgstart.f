        SUBROUTINE PGSTART(I1,I2,NEW)
*
* Non site-specific version of PGSTART
*
        CHARACTER*64 DEVICE, NEWNAME
        INTEGER PGBEGIN, LENSTR
        DATA DEVICE/'/xserve'/
*
10      WRITE(*,*) ' '
        NCHAR = LENSTR(DEVICE)
        WRITE(*,'(A,A,A,$)') 'Enter device name [',DEVICE(:NCHAR),'] '
        READ(*,'(A)') NEWNAME
        IF(NEWNAME.EQ.' ') NEWNAME = DEVICE
        IFAIL =  PGBEGIN(0, NEWNAME, I1, I2)
        IF(IFAIL.NE.1) GOTO 10
        DEVICE = NEWNAME
        NEW = 1
        RETURN
        END
