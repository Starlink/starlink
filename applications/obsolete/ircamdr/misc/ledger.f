        program ledger

! This program principally ledgers all the residuals outputted by the program
! anstd1. However, this program was written to only ledger the FS stars, and
! not the new faint standards. It takes as input the *anstd.results files
! and reads in the residuals for each FS star into its own array. It then
! also performs some statistics. It computes the mean J-H, H-K, and K residuals
! for each star, as well as the standard error of the mean (sem). For each
! residual it then computes the number of sigmas that its away from the mean.
! If the residual is greater than 2 sigma away from the mean, the mean and the
! sem is recalculated with that residual discluded, if the disclude residual is
! now 3 sigma away from the NEW mean, then the new mean and sem are outputted
! and the residual can in fact be discarded.

! The output file is ledger.txt

        implicit none

        integer status, n, i, l1, chr_len, j, h, k, p
        integer one, two, three, four, five, six, seven, ten, eleven, thirteen
        integer fourteen, fifteen, sixteen, seventeen, eighteen, nineteen
        integer twenty, twone, twthree, twfour, twfive, twseven, tweight
        integer twnine, thirty, thone, thtwo, ththree, thfour, thfive

        character file*80, curline*132, pref*80, read*80(200)

! Declare the arrays, and variables needed

        real*8 fs1(200,5), fs1jmhzp, fs1hmkzp, fs1kzp, fs1jmhsq, fs1hmksq
        real*8 fs1ksq, fs1a, fs1b, fs1c

        real*8 fs2(200,5), fs2jmhzp, fs2hmkzp, fs2kzp, fs2jmhsq, fs2hmksq
        real*8 fs2ksq, fs2a, fs2b, fs2c

        real*8 fs3(200,5), fs3jmhzp, fs3hmkzp, fs3kzp, fs3jmhsq, fs3hmksq
        real*8 fs3ksq, fs3a, fs3b, fs3c

        real*8 fs4(200,5), fs4jmhzp, fs4hmkzp, fs4kzp, fs4jmhsq, fs4hmksq
        real*8 fs4ksq, fs4a, fs4b, fs4c

        real*8 fs5(200,5), fs5jmhzp, fs5hmkzp, fs5kzp, fs5jmhsq, fs5hmksq
        real*8 fs5ksq, fs5a, fs5b, fs5c

        real*8 fs6(200,5), fs6jmhzp, fs6hmkzp, fs6kzp, fs6jmhsq, fs6hmksq
        real*8 fs6ksq, fs6a, fs6b, fs6c

        real*8 fs7(200,5), fs7jmhzp, fs7hmkzp, fs7kzp, fs7jmhsq, fs7hmksq
        real*8 fs7ksq, fs7a, fs7b, fs7c

        real*8 fs10(200,5), fs10jmhzp, fs10hmkzp, fs10kzp, fs10jmhsq, fs10hmksq
        real*8 fs10ksq, fs10a, fs10b, fs10c

        real*8 fs11(200,5), fs11jmhzp, fs11hmkzp, fs11kzp, fs11jmhsq, fs11hmksq
        real*8 fs11ksq, fs11a, fs11b, fs11c

        real*8 fs12(200,5), fs12jmhzp, fs12hmkzp, fs12kzp, fs12jmhsq, fs12hmksq
        real*8 fs12ksq, fs12a, fs12b, fs12c

        real*8 fs13(200,5), fs13jmhzp, fs13hmkzp, fs13kzp, fs13jmhsq, fs13hmksq
        real*8 fs13ksq, fs13a, fs13b, fs13c

        real*8 fs14(200,5), fs14jmhzp, fs14hmkzp, fs14kzp, fs14jmhsq, fs14hmksq
        real*8 fs14ksq, fs14a, fs14b, fs14c

        real*8 fs15(200,5), fs15jmhzp, fs15hmkzp, fs15kzp, fs15jmhsq, fs15hmksq
        real*8 fs15ksq, fs15a, fs15b, fs15c

        real*8 fs16(200,5), fs16jmhzp, fs16hmkzp, fs16kzp, fs16jmhsq, fs16hmksq
        real*8 fs16ksq, fs16a, fs16b, fs16c

        real*8 fs17(200,5), fs17jmhzp, fs17hmkzp, fs17kzp, fs17jmhsq, fs17hmksq
        real*8 fs17ksq, fs17a, fs17b, fs17c

        real*8 fs18(200,5), fs18jmhzp, fs18hmkzp, fs18kzp, fs18jmhsq, fs18hmksq
        real*8 fs18ksq, fs18a, fs18b, fs18c

        real*8 fs19(200,5), fs19jmhzp, fs19hmkzp, fs19kzp, fs19jmhsq, fs19hmksq
        real*8 fs19ksq, fs19a, fs19b, fs19c

        real*8 fs20(200,5), fs20jmhzp, fs20hmkzp, fs20kzp, fs20jmhsq, fs20hmksq
        real*8 fs20ksq, fs20a, fs20b, fs20c

        real*8 fs21(200,5), fs21jmhzp, fs21hmkzp, fs21kzp, fs21jmhsq, fs21hmksq
        real*8 fs21ksq, fs21a, fs21b, fs21c

        real*8 fs23(200,5), fs23jmhzp, fs23hmkzp, fs23kzp, fs23jmhsq, fs23hmksq
        real*8 fs23ksq, fs23a, fs23b, fs23c

        real*8 fs24(200,5), fs24jmhzp, fs24hmkzp, fs24kzp, fs24jmhsq, fs24hmksq
        real*8 fs24ksq, fs24a, fs24b, fs24c

        real*8 fs25(200,5), fs25jmhzp, fs25hmkzp, fs25kzp, fs25jmhsq, fs25hmksq
        real*8 fs25ksq, fs25a, fs25b, fs25c

        real*8 fs27(200,5), fs27jmhzp, fs27hmkzp, fs27kzp, fs27jmhsq, fs27hmksq
        real*8 fs27ksq, fs27a, fs27b, fs27c

        real*8 fs28(200,5), fs28jmhzp, fs28hmkzp, fs28kzp, fs28jmhsq, fs28hmksq
        real*8 fs28ksq, fs28a, fs28b, fs28c

        real*8 fs29(200,5), fs29jmhzp, fs29hmkzp, fs29kzp, fs29jmhsq, fs29hmksq
        real*8 fs29ksq, fs29a, fs29b, fs29c

        real*8 fs30(200,5), fs30jmhzp, fs30hmkzp, fs30kzp, fs30jmhsq, fs30hmksq
        real*8 fs30ksq, fs30a, fs30b, fs30c

        real*8 fs31(200,5), fs31jmhzp, fs31hmkzp, fs31kzp, fs31jmhsq, fs31hmksq
        real*8 fs31ksq, fs31a, fs31b, fs31c

        real*8 fs32(200,5), fs32jmhzp, fs32hmkzp, fs32kzp, fs32jmhsq, fs32hmksq
        real*8 fs32ksq, fs32a, fs32b, fs32c

        real*8 fs33(200,5), fs33jmhzp, fs33hmkzp, fs33kzp, fs33jmhsq, fs33hmksq
        real*8 fs33ksq, fs33a, fs33b, fs33c

        real*8 fs34(200,5), fs34jmhzp, fs34hmkzp, fs34kzp, fs34jmhsq, fs34hmksq
        real*8 fs34ksq, fs34a, fs34b, fs34c

        real*8 fs35(200,5), fs35jmhzp, fs35hmkzp, fs35kzp, fs35jmhsq, fs35hmksq
        real*8 fs35ksq, fs35a, fs35b, fs35c

        real*8 hmkvar, jmhvar, kvar, jmhstd, hmkstd, kstd
        real*8 hmksem, jmhsem, ksem, hmkresid, jmhresid, kresid
        real*8 hmkaway(200, 4), jmhaway(200, 4)
        real*8 kaway(200, 4)
        real*8 newjmh, newhmk, newk, newjmhstd, newhmkstd, newkstd

        real  date,std,istd

        logical exists, more, set, ok, jmhtrue, hmktrue, ktrue
        logical written1, written2, written3, written4, written5, written6
        logical written7, written10, written11, written12, written13, written14
        logical written16, written17, written18, written19, written20
        logical written21, written23, written24, written25, written27
        logical written28, written29, written30, written31, written32
        logical written33, written34, written35, written15

        include 'SAE_PAR'

        status = SAI__OK

! Initialize

        n = 0
        one = 0
        two = 0
        three = 0
        four = 0
        five = 0
        six = 0
        seven = 0
        ten = 0
        eleven = 0
        thirteen = 0
        fourteen = 0
        fifteen = 0
        sixteen = 0
        seventeen = 0
        eighteen = 0
        nineteen = 0
        twenty = 0
        twone = 0
        twthree = 0
        twfour = 0
        twfive = 0
        twseven = 0
        tweight = 0
        twnine = 0
        thirty = 0
        thone = 0
        thtwo = 0
        ththree = 0
        thfour = 0
        thfive = 0

        fs1a = 0
        fs1b = 0
        fs1c = 0
        fs1jmhzp = 0
        fs1hmkzp = 0
        fs1kzp = 0
        fs1jmhsq = 0
        fs1hmksq = 0
        fs1ksq = 0

        fs2a = 0
        fs2b = 0
        fs2c = 0
        fs2jmhzp = 0
        fs2hmkzp = 0
        fs2kzp = 0
        fs2jmhsq = 0
        fs2hmksq = 0
        fs2ksq = 0

        fs3a = 0
        fs3b = 0
        fs3c = 0
        fs3jmhzp = 0
        fs3hmkzp = 0
        fs3kzp = 0
        fs3jmhsq = 0
        fs3hmksq = 0
        fs3ksq = 0

        fs4a = 0
        fs4b = 0
        fs4c = 0
        fs4jmhzp = 0
        fs4hmkzp = 0
        fs4kzp = 0
        fs4jmhsq = 0
        fs4hmksq = 0
        fs4ksq = 0

        fs5a = 0
        fs5b = 0
        fs5c = 0
        fs5jmhzp = 0
        fs5hmkzp = 0
        fs5kzp = 0
        fs5jmhsq = 0
        fs5hmksq = 0
        fs5ksq = 0

        fs6a = 0
        fs6b = 0
        fs6c = 0
        fs6jmhzp = 0
        fs6hmkzp = 0
        fs6kzp = 0
        fs6jmhsq = 0
        fs6hmksq = 0
        fs6ksq = 0

        fs7a = 0
        fs7b = 0
        fs7c = 0
        fs7jmhzp = 0
        fs7hmkzp = 0
        fs7kzp = 0
        fs7jmhsq = 0
        fs7hmksq = 0
        fs7ksq = 0

        fs10a = 0
        fs10b = 0
        fs10c = 0
        fs10jmhzp = 0
        fs10hmkzp = 0
        fs10kzp = 0
        fs10jmhsq = 0
        fs10hmksq = 0
        fs10ksq = 0

        fs11a = 0
        fs11b = 0
        fs11c = 0
        fs11jmhzp = 0
        fs11hmkzp = 0
        fs11kzp = 0
        fs11jmhsq = 0
        fs11hmksq = 0
        fs11ksq = 0

        fs12a = 0
        fs12b = 0
        fs12c = 0
        fs12jmhzp = 0
        fs12hmkzp = 0
        fs12kzp = 0
        fs12jmhsq = 0
        fs12hmksq = 0
        fs12ksq = 0

        fs13a = 0
        fs13b = 0
        fs13c = 0
        fs13jmhzp = 0
        fs13hmkzp = 0
        fs13kzp = 0
        fs13jmhsq = 0
        fs13hmksq = 0
        fs13ksq = 0

        fs14a = 0
        fs14b = 0
        fs14c = 0
        fs14jmhzp = 0
        fs14hmkzp = 0
        fs14kzp = 0
        fs14jmhsq = 0
        fs14hmksq = 0
        fs14ksq = 0

        fs15a = 0
        fs15b = 0
        fs15c = 0
        fs15jmhzp = 0
        fs15hmkzp = 0
        fs15kzp = 0
        fs15jmhsq = 0
        fs15hmksq = 0
        fs15ksq = 0

        fs16a = 0
        fs16b = 0
        fs16c = 0
        fs16jmhzp = 0
        fs16hmkzp = 0
        fs16kzp = 0
        fs16jmhsq = 0
        fs16hmksq = 0
        fs16ksq = 0

        fs17a = 0
        fs17b = 0
        fs17c = 0
        fs17jmhzp = 0
        fs17hmkzp = 0
        fs17kzp = 0
        fs17jmhsq = 0
        fs17hmksq = 0
        fs17ksq = 0

        fs18a = 0
        fs18b = 0
        fs18c = 0
        fs18jmhzp = 0
        fs18hmkzp = 0
        fs18kzp = 0
        fs18jmhsq = 0
        fs18hmksq = 0
        fs18ksq = 0

        fs19a = 0
        fs19b = 0
        fs19c = 0
        fs19jmhzp = 0
        fs19hmkzp = 0
        fs19kzp = 0
        fs19jmhsq = 0
        fs19hmksq = 0
        fs19ksq = 0

        fs20a = 0
        fs20b = 0
        fs20c = 0
        fs20jmhzp = 0
        fs20hmkzp = 0
        fs20kzp = 0
        fs20jmhsq = 0
        fs20hmksq = 0
        fs20ksq = 0

        fs21a = 0
        fs21b = 0
        fs21c = 0
        fs21jmhzp = 0
        fs21hmkzp = 0
        fs21kzp = 0
        fs21jmhsq = 0
        fs21hmksq = 0
        fs21ksq = 0

        fs23a = 0
        fs23b = 0
        fs23c = 0
        fs23jmhzp = 0
        fs23hmkzp = 0
        fs23kzp = 0
        fs23jmhsq = 0
        fs23hmksq = 0
        fs23ksq = 0

        fs24a = 0
        fs24b = 0
        fs24c = 0
        fs24jmhzp = 0
        fs24hmkzp = 0
        fs24kzp = 0
        fs24jmhsq = 0
        fs24hmksq = 0
        fs24ksq = 0

        fs25a = 0
        fs25b = 0
        fs25c = 0
        fs25jmhzp = 0
        fs25hmkzp = 0
        fs25kzp = 0
        fs25jmhsq = 0
        fs25hmksq = 0
        fs25ksq = 0

        fs27a = 0
        fs27b = 0
        fs27c = 0
        fs27jmhzp = 0
        fs27hmkzp = 0
        fs27kzp = 0
        fs27jmhsq = 0
        fs27hmksq = 0
        fs27ksq = 0

        fs28a = 0
        fs28b = 0
        fs28c = 0
        fs28jmhzp = 0
        fs28hmkzp = 0
        fs28kzp = 0
        fs28jmhsq = 0
        fs28hmksq = 0
        fs28ksq = 0

        fs29a = 0
        fs29b = 0
        fs29c = 0
        fs29jmhzp = 0
        fs29hmkzp = 0
        fs29kzp = 0
        fs29jmhsq = 0
        fs29hmksq = 0
        fs29ksq = 0

        fs30a = 0
        fs30b = 0
        fs30c = 0
        fs30jmhzp = 0
        fs30hmkzp = 0
        fs30kzp = 0
        fs30jmhsq = 0
        fs30hmksq = 0
        fs30ksq = 0

        fs31a = 0
        fs31b = 0
        fs31c = 0
        fs31jmhzp = 0
        fs31hmkzp = 0
        fs31kzp = 0
        fs31jmhsq = 0
        fs31hmksq = 0
        fs31ksq = 0

        fs32a = 0
        fs32b = 0
        fs32c = 0
        fs32jmhzp = 0
        fs32hmkzp = 0
        fs32kzp = 0
        fs32jmhsq = 0
        fs32hmksq = 0
        fs32ksq = 0

        fs33a = 0
        fs33b = 0
        fs33c = 0
        fs33jmhzp = 0
        fs33hmkzp = 0
        fs33kzp = 0
        fs33jmhsq = 0
        fs33hmksq = 0
        fs33ksq = 0

        fs34a = 0
        fs34b = 0
        fs34c = 0
        fs34jmhzp = 0
        fs34hmkzp = 0
        fs34kzp = 0
        fs34jmhsq = 0
        fs34hmksq = 0
        fs34ksq = 0

        fs35a = 0
        fs35b = 0
        fs35c = 0
        fs35jmhzp = 0
        fs35hmkzp = 0
        fs35kzp = 0
        fs35jmhsq = 0
        fs35hmksq = 0
        fs35ksq = 0

! The files are continued to ask for until the user enters `-1'
        j = 0
! The user only has to enter the prefix
        type *, 'Enter the prefix of the next results file (-1 to exit): '
        read( 5, '(a)') pref
        if( pref .eq. '-1') then
          file = pref
        else
          l1 = chr_len( pref)
          file = pref( 1:l1)//'anstd.results'
        end if
! While we have a file to read...
        do while (file .ne. '-1')
! Does it exist?
           inquire(file=file, exist=exists)
           if( exists) then
              if( j .ne. 0) then
                do i = 1, j
! Check to see if the file has been read...
                   if( file .eq. read(i)) then
                     type *, 'That file has already been read'
                     goto 100
                   end if
                end do
                j = j + 1
                read(j) = file
              else
                j = j + 1
                read(j) = file
              end if
! If it hasn't been read before, and it exists, open it...
              open( unit=142, file=file, status='old')
           else
              type *, 'That file does not exist. Try again.'
              goto 100
           end if
           more = .true.
           written1 = .false.
           written2 = .false.
           written3 = .false.
           written4 = .false.
           written5 = .false.
           written6 = .false.
           written7 = .false.
           written10 = .false.
           written11 = .false.
           written12 = .false.
           written13 = .false.
           written14 = .false.
           written15 = .false.
           written16 = .false.
           written17 = .false.
           written18 = .false.
           written19 = .false.
           written20 = .false.
           written21 = .false.
           written23 = .false.
           written24 = .false.
           written25 = .false.
           written27 = .false.
           written28 = .false.
           written29 = .false.
           written30 = .false.
           written31 = .false.
           written32 = .false.
           written33 = .false.
           written34 = .false.
           written35 = .false.
! While there are more lines...
           do while ( more)
! Read line, if reached eof, goto 200
             read( 142, '(a)', end=200) curline
             if( curline(1:2) .eq. '##') then
! get the data
                call chr_ctor( curline(4:9), date, status)
             else if(curline(21:21) .eq. 'M') then
! get the standard deviation from the file
                call chr_ctor( curline(50:54), std, status)
! If the star is FS1
                if( curline(1:4) .eq. 'FS1') then
! if the we are at J-H, we are starting a set of three (J-H, H-K, K)
                  if( curline(13:15) .eq. 'J-H') then
                    one = one + 1
                    set = .true.
                  end if
! If we are at J-H, ans the sigma is less thjat 0.115, then read the
! residual, and get the values in order to compute the mean and sem
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs1(one,2), status)
                       fs1jmhzp = fs1jmhzp + dble(fs1(one,2))
                       fs1jmhsq = fs1jmhsq + dble(fs1(one,2))**2
                       fs1a = fs1a + 1
                     else
! otherwise flag it
                       fs1(one,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
! we've reach H-K
                     ok = .true.
! if we haven't encounter a J-H, then this is the beginning
                     if( .not. set) then
                       one = one + 1
                       set = .true.
                       fs1(one,2) = -99.99
                     end if
! read the airmass
                     call chr_ctod( curline(30:34), fs1(one,5), status)
                     if( std .lt. 0.115) then
! read the residual as a double, and comput parameters...
                       call chr_ctod( curline(64:69), fs1(one,3), status)
                       fs1hmkzp = fs1hmkzp + dble(fs1(one,3))
                       fs1hmksq = fs1hmksq + dble(fs1(one,3))**2
                       fs1b = fs1b + 1
                     else
! if sigma is too high, flag it, it will come out as stars in the output
                       fs1(one,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
! if we are at K, and we haven't been through J-h< or H-K, start, and flag the
! other values
                     if( .not. set) then
                       one = one + 1
                       fs1(one,2) = -99.99
                       fs1(one,3) = -99.99
! read airmass (if it hasn't been done)
                       call chr_ctod( curline(30:34), fs1(one,5), status)
                     end if
                     if( istd .lt. 0.115) then
! if sigma is low enough read the residual as a double and compute parameters
                       call chr_ctod( curline(64:69), fs1(one,4), status)
                       fs1kzp = fs1kzp + dble(fs1(one,4))
                       fs1ksq = fs1ksq + dble(fs1(one,4))**2
                       fs1c = fs1c + 1
                     else
! otherwise flag it...
                       fs1(one,4) = -99.99
                     end if
                    set = .false.
                  end if
! if we haven't written the date yet, write it to the array, it will only be
! written once per day, even if there is multiple observations
                  if( .not. written1) then
                     fs1(one,1) = date
                     written1 = .true.
                  end if
                  if( set) then
                    fs1(one,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
! If there wasn't a H-K or a K, then read airmass
                    call chr_ctod( curline(30:34), fs1(one,5), status)
                    fs1(one,3) = -99.99
                  end if
                  ok = .false.
! The code was copied for the other stars, so its exactly the same...
                else if( curline(1:4) .eq. 'FS2') then
                  if( curline(13:15) .eq. 'J-H') then
                    two = two + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs2(two,2), status)
                       fs2jmhzp = fs2jmhzp + dble(fs2(two,2))
                       fs2jmhsq = fs2jmhsq + dble(fs2(two,2))**2
                       fs2a = fs2a + 1
                     else
                       fs2(two,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       two = two + 1
                       set = .true.
                       fs2(two,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs2(two,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs2(two,3), status)
                       fs2hmkzp = fs2hmkzp + dble(fs2(two,3))
                       fs2hmksq = fs2hmksq + dble(fs2(two,3))**2
                       fs2b = fs2b + 1
                     else
                       fs2(two,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       two = two + 1
                       fs2(two,2) = -99.99
                       fs2(two,3) = -99.99
                       call chr_ctod( curline(30:34), fs2(two,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs2(two,4), status)
                       fs2kzp = fs2kzp + dble(fs2(two,4))
                       fs2ksq = fs2ksq + dble(fs2(two,4))**2
                       fs2c = fs2c + 1
                     else
                       fs2(two,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written2) then
                     fs2(two,1) = date
                     written2 = .true.
                  end if
                  if( set) then
                    fs2(two,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs2(two,5), status)
                    fs2(two,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS3') then
                  if( curline(13:15) .eq. 'J-H') then
                    three = three + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs3(three,2), status)
                       fs3jmhzp = fs3jmhzp + dble(fs3(three,2))
                       fs3jmhsq = fs3jmhsq + dble(fs3(three,2))**2
                       fs3a = fs3a + 1
                     else
                       fs3(three,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       three = three + 1
                       set = .true.
                       fs3(three,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs3(three,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs3(three,3), status)
                       fs3hmkzp = fs3hmkzp + dble(fs3(three,3))
                       fs3hmksq = fs3hmksq + dble(fs3(three,3))**2
                       fs3b = fs3b + 1
                     else
                       fs3(three,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       three = three + 1
                       fs3(three,2) = -99.99
                       fs3(three,3) = -99.99
                       call chr_ctod( curline(30:34), fs3(three,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs3(three,4), status)
                       fs3kzp = fs3kzp + dble(fs3(three,4))
                       fs3ksq = fs3ksq + dble(fs3(three,4))**2
                       fs3c = fs3c + 1
                     else
                       fs3(three,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written3) then
                     fs3(three,1) = date
                     written3 = .true.
                  end if
                  if( set) then
                    fs3(three,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs3(three,5), status)
                    fs3(three,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS4') then
                  if( curline(13:15) .eq. 'J-H') then
                    four = four + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs4(four,2), status)
                       fs4jmhzp = fs4jmhzp + dble(fs4(four,2))
                       fs4jmhsq = fs4jmhsq + dble(fs4(four,2))**2
                       fs4a = fs4a + 1
                     else
                       fs4(four,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       four = four + 1
                       set = .true.
                       fs4(four,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs4(four,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs4(four,3), status)
                       fs4hmkzp = fs4hmkzp + dble(fs4(four,3))
                       fs4hmksq = fs4hmksq + dble(fs4(four,3))**2
                       fs4b = fs4b + 1
                     else
                       fs4(four,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       four = four + 1
                       fs4(four,2) = -99.99
                       fs4(four,3) = -99.99
                       call chr_ctod( curline(30:34), fs4(four,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs4(four,4), status)
                       fs4kzp = fs4kzp + dble(fs4(four,4))
                       fs4ksq = fs4ksq + dble(fs4(four,4))**2
                       fs4c = fs4c + 1
                     else
                       fs4(four,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written4) then
                     fs4(four,1) = date
                     written4 = .true.
                  end if
                  if( set) then
                    fs4(four,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs4(four,5), status)
                    fs4(four,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS5') then
                  if( curline(13:15) .eq. 'J-H') then
                    five = five + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs5(five,2), status)
                       fs5jmhzp = fs5jmhzp + dble(fs5(five,2))
                       fs5jmhsq = fs5jmhsq + dble(fs5(five,2))**2
                       fs5a = fs5a + 1
                     else
                       fs5(five,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       five = five + 1
                       set = .true.
                       fs5(five,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs5(five,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs5(five,3), status)
                       fs5hmkzp = fs5hmkzp + dble(fs5(five,3))
                       fs5hmksq = fs5hmksq + dble(fs5(five,3))**2
                       fs5b = fs5b + 1
                     else
                       fs5(five,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       five = five + 1
                       fs5(five,2) = -99.99
                       fs5(five,3) = -99.99
                       call chr_ctod( curline(30:34), fs5(five,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs5(five,4), status)
                       fs5kzp = fs5kzp + dble(fs5(five,4))
                       fs5ksq = fs5ksq + dble(fs5(five,4))**2
                       fs5c = fs5c + 1
                     else
                       fs5(five,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written5) then
                     fs5(five,1) = date
                     written5 = .true.
                  end if
                  if( set) then
                    fs5(five,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs5(five,5), status)
                    fs5(five,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS6') then
                  if( curline(13:15) .eq. 'J-H') then
                    six = six + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs6(six,2), status)
                       fs6jmhzp = fs6jmhzp + dble(fs6(six,2))
                       fs6jmhsq = fs6jmhsq + dble(fs6(six,2))**2
                       fs6a = fs6a + 1
                     else
                       fs6(six,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       six = six + 1
                       set = .true.
                       fs6(six,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs6(six,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs6(six,3), status)
                       fs6hmkzp = fs6hmkzp + dble(fs6(six,3))
                       fs6hmksq = fs6hmksq + dble(fs6(six,3))**2
                       fs6b = fs6b + 1
                     else
                       fs6(six,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       six = six + 1
                       fs6(six,2) = -99.99
                       fs6(six,3) = -99.99
                       call chr_ctod( curline(30:34), fs6(six,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs6(six,4), status)
                       fs6kzp = fs6kzp + dble(fs6(six,4))
                       fs6ksq = fs6ksq + dble(fs6(six,4))**2
                       fs6c = fs6c + 1
                     else
                       fs6(six,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written6) then
                     fs6(six,1) = date
                     written6 = .true.
                  end if
                  if( set) then
                    fs6(six,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs6(six,5), status)
                    fs6(six,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS7') then
                  if( curline(13:15) .eq. 'J-H') then
                    seven = seven + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs7(seven,2), status)
                       fs7jmhzp = fs7jmhzp + dble(fs7(seven,2))
                       fs7jmhsq = fs7jmhsq + dble(fs7(seven,2))**2
                       fs7a = fs7a + 1
                     else
                       fs7(seven,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       seven = seven + 1
                       set = .true.
                       fs7(seven,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs7(seven,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs7(seven,3), status)
                       fs7hmkzp = fs7hmkzp + dble(fs7(seven,3))
                       fs7hmksq = fs7hmksq + dble(fs7(seven,3))**2
                       fs7b = fs7b + 1
                     else
                       fs7(seven,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       seven = seven + 1
                       fs7(seven,2) = -99.99
                       fs7(seven,3) = -99.99
                       call chr_ctod( curline(30:34), fs7(seven,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs7(seven,4), status)
                       fs7kzp = fs7kzp + dble(fs7(seven,4))
                       fs7ksq = fs7ksq + dble(fs7(seven,4))**2
                       fs7c = fs7c + 1
                     else
                       fs7(seven,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written7) then
                     fs7(seven,1) = date
                     written7 = .true.
                  end if
                  if( set) then
                    fs7(seven,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs7(seven,5), status)
                    fs7(seven,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS10') then
                  if( curline(13:15) .eq. 'J-H') then
                    ten = ten + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs10(ten,2), status)
                       fs10jmhzp = fs10jmhzp + dble(fs10(ten,2))
                       fs10jmhsq = fs10jmhsq + dble(fs10(ten,2))**2
                       fs10a = fs10a + 1
                     else
                       fs10(ten,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       ten = ten + 1
                       set = .true.
                       fs10(ten,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs10(ten,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs10(ten,3), status)
                       fs10hmkzp = fs10hmkzp + dble(fs10(ten,3))
                       fs10hmksq = fs10hmksq + dble(fs10(ten,3))**2
                       fs10b = fs10b + 1
                     else
                       fs10(ten,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       ten = ten + 1
                       fs10(ten,2) = -99.99
                       fs10(ten,3) = -99.99
                       call chr_ctod( curline(30:34), fs10(ten,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs10(ten,4), status)
                       fs10kzp = fs10kzp + dble(fs10(ten,4))
                       fs10ksq = fs10ksq + dble(fs10(ten,4))**2
                       fs10c = fs10c + 1
                     else
                       fs10(ten,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written10) then
                     fs10(ten,1) = date
                     written10 = .true.
                  end if
                  if( set) then
                    fs10(ten,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs10(ten,5), status)
                    fs10(ten,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS11') then
                  if( curline(13:15) .eq. 'J-H') then
                    eleven = eleven + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs11(eleven,2), status)
                       fs11jmhzp = fs11jmhzp + dble(fs11(eleven,2))
                       fs11jmhsq = fs11jmhsq + dble(fs11(eleven,2))**2
                       fs11a = fs11a + 1
                     else
                       fs11(eleven,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       eleven = eleven + 1
                       set = .true.
                       fs11(eleven,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs11(eleven,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs11(eleven,3), status)
                       fs11hmkzp = fs11hmkzp + dble(fs11(eleven,3))
                       fs11hmksq = fs11hmksq + dble(fs11(eleven,3))**2
                       fs11b = fs11b + 1
                     else
                       fs11(eleven,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       eleven = eleven + 1
                       fs11(eleven,2) = -99.99
                       fs11(eleven,3) = -99.99
                       call chr_ctod( curline(30:34), fs11(eleven,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs11(eleven,4), status)
                       fs11kzp = fs11kzp + dble(fs11(eleven,4))
                       fs11ksq = fs11ksq + dble(fs11(eleven,4))**2
                       fs11c = fs11c + 1
                     else
                       fs11(eleven,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written11) then
                     fs11(eleven,1) = date
                     written11 = .true.
                  end if
                  if( set) then
                    fs11(eleven,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs11(eleven,5), status)
                    fs11(eleven,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS12') then
                  if( curline(13:15) .eq. 'J-H') then
                    n = n + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs12(n,2), status)
                       fs12jmhzp = fs12jmhzp + dble(fs12(n,2))
                       fs12jmhsq = fs12jmhsq + dble(fs12(n,2))**2
                       fs12a = fs12a + 1
                     else
                       fs12(n,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       n = n + 1
                       set = .true.
                       fs12(n,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs12(n,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs12(n,3), status)
                       fs12hmkzp = fs12hmkzp + dble(fs12(n,3))
                       fs12hmksq = fs12hmksq + dble(fs12(n,3))**2
                       fs12b = fs12b + 1
                     else
                       fs12(n,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       n = n + 1
                       fs12(n,2) = -99.99
                       fs12(n,3) = -99.99
                       call chr_ctod( curline(30:34), fs12(n,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs12(n,4), status)
                       fs12kzp = fs12kzp + dble(fs12(n,4))
                       fs12ksq = fs12ksq + dble(fs12(n,4))**2
                       fs12c = fs12c + 1
                     else
                       fs12(n,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written12) then
                     fs12(n,1) = date
                     written12 = .true.
                  end if
                  if( set) then
                    fs12(n,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs12(n,5), status)
                    fs12(n,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS13') then
                  if( curline(13:15) .eq. 'J-H') then
                    thirteen = thirteen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs13(thirteen,2), status)
                       fs13jmhzp = fs13jmhzp + dble(fs13(thirteen,2))
                       fs13jmhsq = fs13jmhsq + dble(fs13(thirteen,2))**2
                       fs13a = fs13a + 1
                     else
                       fs13(thirteen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       thirteen = thirteen + 1
                       set = .true.
                       fs13(thirteen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs13(thirteen,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs13(thirteen,3), status)
                       fs13hmkzp = fs13hmkzp + dble(fs13(thirteen,3))
                       fs13hmksq = fs13hmksq + dble(fs13(thirteen,3))**2
                       fs13b = fs13b + 1
                     else
                       fs13(thirteen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       thirteen = thirteen + 1
                       fs13(thirteen,2) = -99.99
                       fs13(thirteen,3) = -99.99
                       call chr_ctod( curline(30:34), fs13(thirteen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs13(thirteen,4), status)
                       fs13kzp = fs13kzp + dble(fs13(thirteen,4))
                       fs13ksq = fs13ksq + dble(fs13(thirteen,4))**2
                       fs13c = fs13c + 1
                     else
                       fs13(thirteen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written13) then
                     fs13(thirteen,1) = date
                     written13 = .true.
                  end if
                  if( set) then
                    fs13(thirteen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs13(thirteen,5), status)
                    fs13(thirteen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS14') then
                  if( curline(13:15) .eq. 'J-H') then
                    fourteen = fourteen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs14(fourteen,2), status)
                       fs14jmhzp = fs14jmhzp + dble(fs14(fourteen,2))
                       fs14jmhsq = fs14jmhsq + dble(fs14(fourteen,2))**2
                       fs14a = fs14a + 1
                     else
                       fs14(fourteen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       fourteen = fourteen + 1
                       set = .true.
                       fs14(fourteen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs14(fourteen,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs14(fourteen,3), status)
                       fs14hmkzp = fs14hmkzp + dble(fs14(fourteen,3))
                       fs14hmksq = fs14hmksq + dble(fs14(fourteen,3))**2
                       fs14b = fs14b + 1
                     else
                       fs14(fourteen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       fourteen = fourteen + 1
                       fs14(fourteen,2) = -99.99
                       fs14(fourteen,3) = -99.99
                       call chr_ctod( curline(30:34), fs14(fourteen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs14(fourteen,4), status)
                       fs14kzp = fs14kzp + dble(fs14(fourteen,4))
                       fs14ksq = fs14ksq + dble(fs14(fourteen,4))**2
                       fs14c = fs14c + 1
                     else
                       fs14(fourteen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written14) then
                     fs14(fourteen,1) = date
                     written14 = .true.
                  end if
                  if( set) then
                    fs14(fourteen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs14(fourteen,5), status)
                    fs14(fourteen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS15') then
                  if( curline(13:15) .eq. 'J-H') then
                    fifteen = fifteen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs15(fifteen,2), status)
                       fs15jmhzp = fs15jmhzp + dble(fs15(fifteen,2))
                       fs15jmhsq = fs15jmhsq + dble(fs15(fifteen,2))**2
                       fs15a = fs15a + 1
                     else
                       fs15(fifteen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       fifteen = fifteen + 1
                       set = .true.
                       fs15(fifteen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs15(fifteen,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs15(fifteen,3), status)
                       fs15hmkzp = fs15hmkzp + dble(fs15(fifteen,3))
                       fs15hmksq = fs15hmksq + dble(fs15(fifteen,3))**2
                       fs15b = fs15b + 1
                     else
                       fs15(fifteen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       fifteen = fifteen + 1
                       fs15(fifteen,2) = -99.99
                       fs15(fifteen,3) = -99.99
                       call chr_ctod( curline(30:34), fs15(fifteen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs15(fifteen,4), status)
                       fs15kzp = fs15kzp + dble(fs15(fifteen,4))
                       fs15ksq = fs15ksq + dble(fs15(fifteen,4))**2
                       fs15c = fs15c + 1
                     else
                       fs15(fifteen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written15) then
                     fs15(fifteen,1) = date
                     written15 = .true.
                  end if
                  if( set) then
                    fs15(fifteen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs15(fifteen,5), status)
                    fs15(fifteen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS16') then
                  if( curline(13:15) .eq. 'J-H') then
                    sixteen = sixteen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs16(sixteen,2), status)
                       fs16jmhzp = fs16jmhzp + dble(fs16(sixteen,2))
                       fs16jmhsq = fs16jmhsq + dble(fs16(sixteen,2))**2
                       fs16a = fs16a + 1
                     else
                       fs16(sixteen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       sixteen = sixteen + 1
                       set = .true.
                       fs16(sixteen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs16(sixteen,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs16(sixteen,3), status)
                       fs16hmkzp = fs16hmkzp + dble(fs16(sixteen,3))
                       fs16hmksq = fs16hmksq + dble(fs16(sixteen,3))**2
                       fs16b = fs16b + 1
                     else
                       fs16(sixteen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       sixteen = sixteen + 1
                       fs16(sixteen,2) = -99.99
                       fs16(sixteen,3) = -99.99
                       call chr_ctod( curline(30:34), fs16(sixteen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs16(sixteen,4), status)
                       fs16kzp = fs16kzp + dble(fs16(sixteen,4))
                       fs16ksq = fs16ksq + dble(fs16(sixteen,4))**2
                       fs16c = fs16c + 1
                     else
                       fs16(sixteen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written16) then
                     fs16(sixteen,1) = date
                     written16 = .true.
                  end if
                  if( set) then
                    fs16(sixteen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs16(sixteen,5), status)
                    fs16(sixteen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS17') then
                  if( curline(13:15) .eq. 'J-H') then
                    seventeen = seventeen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                      call chr_ctod( curline(64:69), fs17(seventeen,2), status)
                       fs17jmhzp = fs17jmhzp + dble(fs17(seventeen,2))
                       fs17jmhsq = fs17jmhsq + dble(fs17(seventeen,2))**2
                       fs17a = fs17a + 1
                     else
                       fs17(seventeen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       seventeen = seventeen + 1
                       set = .true.
                       fs17(seventeen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs17(seventeen,5), status)
                     if( std .lt. 0.115) then
                      call chr_ctod( curline(64:69), fs17(seventeen,3), status)
                       fs17hmkzp = fs17hmkzp + dble(fs17(seventeen,3))
                       fs17hmksq = fs17hmksq + dble(fs17(seventeen,3))**2
                       fs17b = fs17b + 1
                     else
                       fs17(seventeen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       seventeen = seventeen + 1
                       fs17(seventeen,2) = -99.99
                       fs17(seventeen,3) = -99.99
                      call chr_ctod( curline(30:34), fs17(seventeen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                      call chr_ctod( curline(64:69), fs17(seventeen,4), status)
                       fs17kzp = fs17kzp + dble(fs17(seventeen,4))
                       fs17ksq = fs17ksq + dble(fs17(seventeen,4))**2
                       fs17c = fs17c + 1
                     else
                       fs17(seventeen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written17) then
                     fs17(seventeen,1) = date
                     written17 = .true.
                  end if
                  if( set) then
                    fs17(seventeen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs17(seventeen,5), status)
                    fs17(seventeen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS18') then
                  if( curline(13:15) .eq. 'J-H') then
                    eighteen = eighteen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs18(eighteen,2), status)
                       fs18jmhzp = fs18jmhzp + dble(fs18(eighteen,2))
                       fs18jmhsq = fs18jmhsq + dble(fs18(eighteen,2))**2
                       fs18a = fs18a + 1
                     else
                       fs18(eighteen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       eighteen = eighteen + 1
                       set = .true.
                       fs18(eighteen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs18(eighteen,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs18(eighteen,3), status)
                       fs18hmkzp = fs18hmkzp + dble(fs18(eighteen,3))
                       fs18hmksq = fs18hmksq + dble(fs18(eighteen,3))**2
                       fs18b = fs18b + 1
                     else
                       fs18(eighteen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       eighteen = eighteen + 1
                       fs18(eighteen,2) = -99.99
                       fs18(eighteen,3) = -99.99
                       call chr_ctod( curline(30:34), fs18(eighteen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs18(eighteen,4), status)
                       fs18kzp = fs18kzp + dble(fs18(eighteen,4))
                       fs18ksq = fs18ksq + dble(fs18(eighteen,4))**2
                       fs18c = fs18c + 1
                     else
                       fs18(eighteen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written18) then
                     fs18(eighteen,1) = date
                     written18 = .true.
                  end if
                  if( set) then
                    fs18(eighteen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs18(eighteen,5), status)
                    fs18(eighteen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS19') then
                  if( curline(13:15) .eq. 'J-H') then
                    nineteen = nineteen + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs19(nineteen,2), status)
                       fs19jmhzp = fs19jmhzp + dble(fs19(nineteen,2))
                       fs19jmhsq = fs19jmhsq + dble(fs19(nineteen,2))**2
                       fs19a = fs19a + 1
                     else
                       fs19(nineteen,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       nineteen = nineteen + 1
                       set = .true.
                       fs19(nineteen,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs19(nineteen,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs19(nineteen,3), status)
                       fs19hmkzp = fs19hmkzp + dble(fs19(nineteen,3))
                       fs19hmksq = fs19hmksq + dble(fs19(nineteen,3))**2
                       fs19b = fs19b + 1
                     else
                       fs19(nineteen,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       nineteen = nineteen + 1
                       fs19(nineteen,2) = -99.99
                       fs19(nineteen,3) = -99.99
                       call chr_ctod( curline(30:34), fs19(nineteen,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs19(nineteen,4), status)
                       fs19kzp = fs19kzp + dble(fs19(nineteen,4))
                       fs19ksq = fs19ksq + dble(fs19(nineteen,4))**2
                       fs19c = fs19c + 1
                     else
                       fs19(nineteen,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written19) then
                     fs19(nineteen,1) = date
                     written19 = .true.
                  end if
                  if( set) then
                    fs19(nineteen,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs19(nineteen,5), status)
                    fs19(nineteen,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS20') then
                  if( curline(13:15) .eq. 'J-H') then
                    twenty = twenty + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs20(twenty,2), status)
                       fs20jmhzp = fs20jmhzp + dble(fs20(twenty,2))
                       fs20jmhsq = fs20jmhsq + dble(fs20(twenty,2))**2
                       fs20a = fs20a + 1
                     else
                       fs20(twenty,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twenty = twenty + 1
                       set = .true.
                       fs20(twenty,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs20(twenty,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs20(twenty,3), status)
                       fs20hmkzp = fs20hmkzp + dble(fs20(twenty,3))
                       fs20hmksq = fs20hmksq + dble(fs20(twenty,3))**2
                       fs20b = fs20b + 1
                     else
                       fs20(twenty,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twenty = twenty + 1
                       fs20(twenty,2) = -99.99
                       fs20(twenty,3) = -99.99
                       call chr_ctod( curline(30:34), fs20(twenty,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs20(twenty,4), status)
                       fs20kzp = fs20kzp + dble(fs20(twenty,4))
                       fs20ksq = fs20ksq + dble(fs20(twenty,4))**2
                       fs20c = fs20c + 1
                     else
                       fs20(twenty,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written20) then
                     fs20(twenty,1) = date
                     written20 = .true.
                  end if
                  if( set) then
                    fs20(twenty,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs20(twenty,5), status)
                    fs20(twenty,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS21') then
                  if( curline(13:15) .eq. 'J-H') then
                    twone = twone + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs21(twone,2), status)
                       fs21jmhzp = fs21jmhzp + dble(fs21(twone,2))
                       fs21jmhsq = fs21jmhsq + dble(fs21(twone,2))**2
                       fs21a = fs21a + 1
                     else
                       fs21(twone,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twone = twone + 1
                       set = .true.
                       fs21(twone,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs21(twone,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs21(twone,3), status)
                       fs21hmkzp = fs21hmkzp + dble(fs21(twone,3))
                       fs21hmksq = fs21hmksq + dble(fs21(twone,3))**2
                       fs21b = fs21b + 1
                     else
                       fs21(twone,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twone = twone + 1
                       fs21(twone,2) = -99.99
                       fs21(twone,3) = -99.99
                       call chr_ctod( curline(30:34), fs21(twone,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs21(twone,4), status)
                       fs21kzp = fs21kzp + dble(fs21(twone,4))
                       fs21ksq = fs21ksq + dble(fs21(twone,4))**2
                       fs21c = fs21c + 1
                     else
                       fs21(twone,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written21) then
                     fs21(twone,1) = date
                     written21 = .true.
                  end if
                  if( set) then
                    fs21(twone,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs21(twone,5), status)
                    fs21(twone,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS23') then
                  if( curline(13:15) .eq. 'J-H') then
                    twthree = twthree + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs23(twthree,2), status)
                       fs23jmhzp = fs23jmhzp + dble(fs23(twthree,2))
                       fs23jmhsq = fs23jmhsq + dble(fs23(twthree,2))**2
                       fs23a = fs23a + 1
                     else
                       fs23(twthree,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twthree = twthree + 1
                       set = .true.
                       fs23(twthree,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs23(twthree,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs23(twthree,3), status)
                       fs23hmkzp = fs23hmkzp + dble(fs23(twthree,3))
                       fs23hmksq = fs23hmksq + dble(fs23(twthree,3))**2
                       fs23b = fs23b + 1
                     else
                       fs23(twthree,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twthree = twthree + 1
                       fs23(twthree,2) = -99.99
                       fs23(twthree,3) = -99.99
                       call chr_ctod( curline(30:34), fs23(twthree,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs23(twthree,4), status)
                       fs23kzp = fs23kzp + dble(fs23(twthree,4))
                       fs23ksq = fs23ksq + dble(fs23(twthree,4))**2
                       fs23c = fs23c + 1
                     else
                       fs23(twthree,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written23) then
                     fs23(twthree,1) = date
                     written23 = .true.
                  end if
                  if( set) then
                    fs23(twthree,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs23(twthree,5), status)
                    fs23(twthree,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS24') then
                  if( curline(13:15) .eq. 'J-H') then
                    twfour = twfour + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs24(twfour,2), status)
                       fs24jmhzp = fs24jmhzp + dble(fs24(twfour,2))
                       fs24jmhsq = fs24jmhsq + dble(fs24(twfour,2))**2
                       fs24a = fs24a + 1
                     else
                       fs24(twfour,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twfour = twfour + 1
                       set = .true.
                       fs24(twfour,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs24(twfour,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs24(twfour,3), status)
                       fs24hmkzp = fs24hmkzp + dble(fs24(twfour,3))
                       fs24hmksq = fs24hmksq + dble(fs24(twfour,3))**2
                       fs24b = fs24b + 1
                     else
                       fs24(twfour,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twfour = twfour + 1
                       fs24(twfour,2) = -99.99
                       fs24(twfour,3) = -99.99
                       call chr_ctod( curline(30:34), fs24(twfour,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs24(twfour,4), status)
                       fs24kzp = fs24kzp + dble(fs24(twfour,4))
                       fs24ksq = fs24ksq + dble(fs24(twfour,4))**2
                       fs24c = fs24c + 1
                     else
                       fs24(twfour,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written24) then
                     fs24(twfour,1) = date
                     written24 = .true.
                  end if
                  if( set) then
                    fs24(twfour,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs24(twfour,5), status)
                    fs24(twfour,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS25') then
                  if( curline(13:15) .eq. 'J-H') then
                    twfive = twfive + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs25(twfive,2), status)
                       fs25jmhzp = fs25jmhzp + dble(fs25(twfive,2))
                       fs25jmhsq = fs25jmhsq + dble(fs25(twfive,2))**2
                       fs25a = fs25a + 1
                     else
                       fs25(twfive,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twfive = twfive + 1
                       set = .true.
                       fs25(twfive,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs25(twfive,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs25(twfive,3), status)
                       fs25hmkzp = fs25hmkzp + dble(fs25(twfive,3))
                       fs25hmksq = fs25hmksq + dble(fs25(twfive,3))**2
                       fs25b = fs25b + 1
                     else
                       fs25(twfive,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twfive = twfive + 1
                       fs25(twfive,2) = -99.99
                       fs25(twfive,3) = -99.99
                       call chr_ctod( curline(30:34), fs25(twfive,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs25(twfive,4), status)
                       fs25kzp = fs25kzp + dble(fs25(twfive,4))
                       fs25ksq = fs25ksq + dble(fs25(twfive,4))**2
                       fs25c = fs25c + 1
                     else
                       fs25(twfive,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written25) then
                     fs25(twfive,1) = date
                     written25 = .true.
                  end if
                  if( set) then
                    fs25(twfive,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs25(twfive,5), status)
                    fs25(twfive,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS27') then
                  if( curline(13:15) .eq. 'J-H') then
                    twseven = twseven + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs27(twseven,2), status)
                       fs27jmhzp = fs27jmhzp + dble(fs27(twseven,2))
                       fs27jmhsq = fs27jmhsq + dble(fs27(twseven,2))**2
                       fs27a = fs27a + 1
                     else
                       fs27(twseven,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twseven = twseven + 1
                       set = .true.
                       fs27(twseven,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs27(twseven,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs27(twseven,3), status)
                       fs27hmkzp = fs27hmkzp + dble(fs27(twseven,3))
                       fs27hmksq = fs27hmksq + dble(fs27(twseven,3))**2
                       fs27b = fs27b + 1
                     else
                       fs27(twseven,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twseven = twseven + 1
                       fs27(twseven,2) = -99.99
                       fs27(twseven,3) = -99.99
                       call chr_ctod( curline(30:34), fs27(twseven,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs27(twseven,4), status)
                       fs27kzp = fs27kzp + dble(fs27(twseven,4))
                       fs27ksq = fs27ksq + dble(fs27(twseven,4))**2
                       fs27c = fs27c + 1
                     else
                       fs27(twseven,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written27) then
                     fs27(twseven,1) = date
                     written27 = .true.
                  end if
                  if( set) then
                    fs27(twseven,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs27(twseven,5), status)
                    fs27(twseven,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS28') then
                  if( curline(13:15) .eq. 'J-H') then
                    tweight = tweight + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs28(tweight,2), status)
                       fs28jmhzp = fs28jmhzp + dble(fs28(tweight,2))
                       fs28jmhsq = fs28jmhsq + dble(fs28(tweight,2))**2
                       fs28a = fs28a + 1
                     else
                       fs28(tweight,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       tweight = tweight + 1
                       set = .true.
                       fs28(tweight,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs28(tweight,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs28(tweight,3), status)
                       fs28hmkzp = fs28hmkzp + dble(fs28(tweight,3))
                       fs28hmksq = fs28hmksq + dble(fs28(tweight,3))**2
                       fs28b = fs28b + 1
                     else
                       fs28(tweight,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       tweight = tweight + 1
                       fs28(tweight,2) = -99.99
                       fs28(tweight,3) = -99.99
                       call chr_ctod( curline(30:34), fs28(tweight,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs28(tweight,4), status)
                       fs28kzp = fs28kzp + dble(fs28(tweight,4))
                       fs28ksq = fs28ksq + dble(fs28(tweight,4))**2
                       fs28c = fs28c + 1
                     else
                       fs28(tweight,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written28) then
                     fs28(tweight,1) = date
                     written28 = .true.
                  end if
                  if( set) then
                    fs28(tweight,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs28(tweight,5), status)
                    fs28(tweight,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS29') then
                  if( curline(13:15) .eq. 'J-H') then
                    twnine = twnine + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs29(twnine,2), status)
                       fs29jmhzp = fs29jmhzp + dble(fs29(twnine,2))
                       fs29jmhsq = fs29jmhsq + dble(fs29(twnine,2))**2
                       fs29a = fs29a + 1
                     else
                       fs29(twnine,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       twnine = twnine + 1
                       set = .true.
                       fs29(twnine,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs29(twnine,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs29(twnine,3), status)
                       fs29hmkzp = fs29hmkzp + dble(fs29(twnine,3))
                       fs29hmksq = fs29hmksq + dble(fs29(twnine,3))**2
                       fs29b = fs29b + 1
                     else
                       fs29(twnine,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       twnine = twnine + 1
                       fs29(twnine,2) = -99.99
                       fs29(twnine,3) = -99.99
                       call chr_ctod( curline(30:34), fs29(twnine,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs29(twnine,4), status)
                       fs29kzp = fs29kzp + dble(fs29(twnine,4))
                       fs29ksq = fs29ksq + dble(fs29(twnine,4))**2
                       fs29c = fs29c + 1
                     else
                       fs29(twnine,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written29) then
                     fs29(twnine,1) = date
                     written29 = .true.
                  end if
                  if( set) then
                    fs29(twnine,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs29(twnine,5), status)
                    fs29(twnine,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS30') then
                  if( curline(13:15) .eq. 'J-H') then
                    thirty = thirty + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs30(thirty,2), status)
                       fs30jmhzp = fs30jmhzp + dble(fs30(thirty,2))
                       fs30jmhsq = fs30jmhsq + dble(fs30(thirty,2))**2
                       fs30a = fs30a + 1
                     else
                       fs30(thirty,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       thirty = thirty + 1
                       set = .true.
                       fs30(thirty,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs30(thirty,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs30(thirty,3), status)
                       fs30hmkzp = fs30hmkzp + dble(fs30(thirty,3))
                       fs30hmksq = fs30hmksq + dble(fs30(thirty,3))**2
                       fs30b = fs30b + 1
                     else
                       fs30(thirty,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       thirty = thirty + 1
                       fs30(thirty,2) = -99.99
                       fs30(thirty,3) = -99.99
                       call chr_ctod( curline(30:34), fs30(thirty,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs30(thirty,4), status)
                       fs30kzp = fs30kzp + dble(fs30(thirty,4))
                       fs30ksq = fs30ksq + dble(fs30(thirty,4))**2
                       fs30c = fs30c + 1
                     else
                       fs30(thirty,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written30) then
                     fs30(thirty,1) = date
                     written30 = .true.
                  end if
                  if( set) then
                    fs30(thirty,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs30(thirty,5), status)
                    fs30(thirty,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS31') then
                  if( curline(13:15) .eq. 'J-H') then
                    thone = thone + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs31(thone,2), status)
                       fs31jmhzp = fs31jmhzp + dble(fs31(thone,2))
                       fs31jmhsq = fs31jmhsq + dble(fs31(thone,2))**2
                       fs31a = fs31a + 1
                     else
                       fs31(thone,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       thone = thone + 1
                       set = .true.
                       fs31(thone,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs31(thone,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs31(thone,3), status)
                       fs31hmkzp = fs31hmkzp + dble(fs31(thone,3))
                       fs31hmksq = fs31hmksq + dble(fs31(thone,3))**2
                       fs31b = fs31b + 1
                     else
                       fs31(thone,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       thone = thone + 1
                       fs31(thone,2) = -99.99
                       fs31(thone,3) = -99.99
                       call chr_ctod( curline(30:34), fs31(thone,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs31(thone,4), status)
                       fs31kzp = fs31kzp + dble(fs31(thone,4))
                       fs31ksq = fs31ksq + dble(fs31(thone,4))**2
                       fs31c = fs31c + 1
                     else
                       fs31(thone,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written31) then
                     fs31(thone,1) = date
                     written31 = .true.
                  end if
                  if( set) then
                    fs31(thone,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs31(thone,5), status)
                    fs31(thone,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS32') then
                  if( curline(13:15) .eq. 'J-H') then
                    thtwo = thtwo + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs32(thtwo,2), status)
                       fs32jmhzp = fs32jmhzp + dble(fs32(thtwo,2))
                       fs32jmhsq = fs32jmhsq + dble(fs32(thtwo,2))**2
                       fs32a = fs32a + 1
                     else
                       fs32(thtwo,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       thtwo = thtwo + 1
                       set = .true.
                       fs32(thtwo,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs32(thtwo,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs32(thtwo,3), status)
                       fs32hmkzp = fs32hmkzp + dble(fs32(thtwo,3))
                       fs32hmksq = fs32hmksq + dble(fs32(thtwo,3))**2
                       fs32b = fs32b + 1
                     else
                       fs32(thtwo,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       thtwo = thtwo + 1
                       fs32(thtwo,2) = -99.99
                       fs32(thtwo,3) = -99.99
                       call chr_ctod( curline(30:34), fs32(thtwo,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs32(thtwo,4), status)
                       fs32kzp = fs32kzp + dble(fs32(thtwo,4))
                       fs32ksq = fs32ksq + dble(fs32(thtwo,4))**2
                       fs32c = fs32c + 1
                     else
                       fs32(thtwo,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written32) then
                     fs32(thtwo,1) = date
                     written32 = .true.
                  end if
                  if( set) then
                    fs32(thtwo,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs32(thtwo,5), status)
                    fs32(thtwo,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS33') then
                  if( curline(13:15) .eq. 'J-H') then
                    ththree = ththree + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs33(ththree,2), status)
                       fs33jmhzp = fs33jmhzp + dble(fs33(ththree,2))
                       fs33jmhsq = fs33jmhsq + dble(fs33(ththree,2))**2
                       fs33a = fs33a + 1
                     else
                       fs33(ththree,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       ththree = ththree + 1
                       set = .true.
                       fs33(ththree,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs33(ththree,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs33(ththree,3), status)
                       fs33hmkzp = fs33hmkzp + dble(fs33(ththree,3))
                       fs33hmksq = fs33hmksq + dble(fs33(ththree,3))**2
                       fs33b = fs33b + 1
                     else
                       fs33(ththree,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       ththree = ththree + 1
                       fs33(ththree,2) = -99.99
                       fs33(ththree,3) = -99.99
                       call chr_ctod( curline(30:34), fs33(ththree,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs33(ththree,4), status)
                       fs33kzp = fs33kzp + dble(fs33(ththree,4))
                       fs33ksq = fs33ksq + dble(fs33(ththree,4))**2
                       fs33c = fs33c + 1
                     else
                       fs33(ththree,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written33) then
                     fs33(ththree,1) = date
                     written33 = .true.
                  end if
                  if( set) then
                    fs33(ththree,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs33(ththree,5), status)
                    fs33(ththree,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS34') then
                  if( curline(13:15) .eq. 'J-H') then
                    thfour = thfour + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs34(thfour,2), status)
                       fs34jmhzp = fs34jmhzp + dble(fs34(thfour,2))
                       fs34jmhsq = fs34jmhsq + dble(fs34(thfour,2))**2
                       fs34a = fs34a + 1
                     else
                       fs34(thfour,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       thfour = thfour + 1
                       set = .true.
                       fs34(thfour,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs34(thfour,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs34(thfour,3), status)
                       fs34hmkzp = fs34hmkzp + dble(fs34(thfour,3))
                       fs34hmksq = fs34hmksq + dble(fs34(thfour,3))**2
                       fs34b = fs34b + 1
                     else
                       fs34(thfour,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       thfour = thfour + 1
                       fs34(thfour,2) = -99.99
                       fs34(thfour,3) = -99.99
                       call chr_ctod( curline(30:34), fs34(thfour,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs34(thfour,4), status)
                       fs34kzp = fs34kzp + dble(fs34(thfour,4))
                       fs34ksq = fs34ksq + dble(fs34(thfour,4))**2
                       fs34c = fs34c + 1
                     else
                       fs34(thfour,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written34) then
                     fs34(thfour,1) = date
                     written34 = .true.
                  end if
                  if( set) then
                    fs34(thfour,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs34(thfour,5), status)
                    fs34(thfour,3) = -99.99
                  end if
                  ok = .false.
                else if( curline(1:4) .eq. 'FS35') then
                  if( curline(13:15) .eq. 'J-H') then
                    thfive = thfive + 1
                    set = .true.
                  end if
                  if( curline(13:15) .eq. 'J-H') then
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs35(thfive,2), status)
                       fs35jmhzp = fs35jmhzp + dble(fs35(thfive,2))
                       fs35jmhsq = fs35jmhsq + dble(fs35(thfive,2))**2
                       fs35a = fs35a + 1
                     else
                       fs35(thfive,2) = -99.99
                     end if
                  else if( curline(13:15) .eq. 'H-K') then
                     ok = .true.
                     if( .not. set) then
                       thfive = thfive + 1
                       set = .true.
                       fs35(thfive,2) = -99.99
                     end if
                     call chr_ctod( curline(30:34), fs35(thfive,5), status)
                     if( std .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs35(thfive,3), status)
                       fs35hmkzp = fs35hmkzp + dble(fs35(thfive,3))
                       fs35hmksq = fs35hmksq + dble(fs35(thfive,3))**2
                       fs35b = fs35b + 1
                     else
                       fs35(thfive,3) = -99.99
                     end if
                  else if( curline(13:13) .eq. 'K') then
                     if( .not. set) then
                       thfive = thfive + 1
                       fs35(thfive,2) = -99.99
                       fs35(thfive,3) = -99.99
                       call chr_ctod( curline(30:34), fs35(thfive,5), status)
                     end if
                     if( istd .lt. 0.115) then
                       call chr_ctod( curline(64:69), fs35(thfive,4), status)
                       fs35kzp = fs35kzp + dble(fs35(thfive,4))
                       fs35ksq = fs35ksq + dble(fs35(thfive,4))**2
                       fs35c = fs35c + 1
                     else
                       fs35(thfive,4) = -99.99
                     end if
                    set = .false.
                  end if
                  if( .not. written35) then
                     fs35(thfive,1) = date
                     written35 = .true.
                  end if
                  if( set) then
                    fs35(thfive,4) = -99.99
                  end if
                  if( ( .not. ok) .and. set) then
                    call chr_ctod( curline(30:34), fs35(thfive,5), status)
                    fs35(thfive,3) = -99.99
                  end if
                  ok = .false.
                end if
! we need to get the sigma for the K's off the I lines
             else if( curline(21:21) .eq. 'I') then
               call chr_ctor( curline(50:54), istd, status)
             end if
           end do
 200       continue
           close( 142)
! ask for next file
 100       type *, 'Enter the prefix of the next results file (-1 to exit): '
           read( 5, '(a)') pref
           if( pref .eq. '-1') then
             file = pref
           else
             l1 = chr_len( pref)
             file = pref( 1:l1)//'anstd.results'
           end if
        end do
! open output file
        open( unit=143, file='ledger.txt', status='unknown')

! compute the statistics for fs1
        hmkvar = (fs1hmksq - fs1b * (fs1hmkzp/fs1b)**2)
        if( fs1b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs1b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs1b)

        jmhvar = (fs1jmhsq - fs1a * (fs1jmhzp/fs1a)**2)
        if( fs1a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs1a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs1a)

        kvar = (fs1ksq - fs1c * (fs1kzp/fs1c)**2)
        if( fs1c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs1c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs1c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS1'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
! do for the depth of the array...
        do i = 1, one
! if the J-H wasn't flagged...
           if( fs1(i,2) .ne. -99.99) then
! compute the residual from the mean for each value, being careful if they
! have different signs
             if( ( fs1jmhzp * fs1(i,2)) .lt. 0) then
               jmhresid = abs(fs1jmhzp/fs1a) + abs(fs1(i,2))
             else if( ( fs1jmhzp * fs1(i,2)) .eq. 0) then
               jmhresid = abs(fs1jmhzp/fs1a) + abs(fs1(i,2))
             else
               jmhresid = abs(fs1jmhzp/fs1a) - abs(fs1(i,2))
             end if
             jmhresid = abs(jmhresid)
! how many sigmas away...
             jmhaway( p,1) = jmhresid/jmhstd
! if its greater than 2, compute the new mean and sem....
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs1jmhzp - fs1(i,2)
! store it
               jmhaway(p,3) = (newjmh/(fs1a-1))
               fs1jmhsq = fs1jmhsq - (fs1(i,2))**2
               jmhvar = (fs1jmhsq - (fs1a-1) * (newjmh/(fs1a-1))**2)
                 if( (fs1a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs1a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
! store the new sem...
               jmhaway(p,4) = newjmhstd/dsqrt(fs1a-1)
! compute the new residual
               if( ( newjmh * fs1(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs1a-1)) + abs(fs1(i,2))
               else if( ( newjmh * fs1(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs1a-1)) + abs(fs1(i,2))
               else
                 jmhresid = abs(newjmh/(fs1a-1)) - abs(fs1(i,2))
               end if
               jmhresid = abs(jmhresid)
! how many sigmas away...
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
! we have one greater than 2...
               jmhtrue = .true.
             end if
           end if
! identical for H-K, and K
           if( fs1(i,3) .ne. -99.99) then
             if( ( fs1hmkzp * fs1(i,3)) .lt. 0) then
               hmkresid = abs(fs1hmkzp/fs1b) + abs(fs1(i,3))
             else if( ( fs1hmkzp * fs1(i,3)) .eq. 0) then
               hmkresid = abs(fs1hmkzp/fs1b) + abs(fs1(i,3))
             else
               hmkresid = abs(fs1hmkzp/fs1b) - abs(fs1(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs1hmkzp - fs1(i,3)
               hmkaway(h,3) = newhmk/(fs1b-1)
               fs1hmksq = fs1hmksq - (fs1(i,3))**2
               hmkvar = (fs1hmksq - (fs1b-1) * (newhmk/(fs1b-1))**2)
                 if( (fs1b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs1b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs1b-1)
               if( ( newhmk * fs1(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs1b-1)) + abs(fs1(i,3))
               else if( ( newhmk * fs1(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs1b-1)) + abs(fs1(i,3))
               else
                 hmkresid = abs(newhmk/(fs1b-1)) - abs(fs1(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs1(i,4) .ne. -99.99) then
             if( ( fs1kzp * fs1(i,4)) .lt. 0) then
               kresid = abs(fs1kzp/fs1c) + abs(fs1(i,4))
             else if( ( fs1kzp * fs1(i,4)) .eq. 0) then
               kresid = abs(fs1kzp/fs1c) + abs(fs1(i,4))
             else
               kresid = abs(fs1kzp/fs1c) - abs(fs1(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs1kzp - fs1(i,4)
               kaway(k,3) = newk/(fs1c-1)
               fs1ksq = fs1ksq - (fs1(i,4))**2
               kvar = (fs1ksq - (fs1c-1) * (newk/(fs1c-1))**2)
                 if( (fs1c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs1c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs1c-1)
               if( ( newk * fs1(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs1c-1)) + abs(fs1(i,4))
               else if( ( newk * fs1(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs1c-1)) + abs(fs1(i,4))
               else
                 kresid = abs(newk/(fs1c-1)) - abs(fs1(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
! various options for outputting...a # is placed in front of a J-H residual
! that is graeater than 2 sigma away from the mean, a $ in form of a H-K
! residual, and a @ in from of a K residual...
! remeber this is still one line at a time...
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs1(i,1),
     :      '#',fs1(i,2),fs1(i,3),fs1(i,4), fs1(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs1(i,1),
     :           fs1(i,2),'$',fs1(i,3),fs1(i,4), fs1(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs1(i,1),
     :           fs1(i,2),fs1(i,3),'@',fs1(i,4), fs1(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs1(i,1),
     :           '#',fs1(i,2),'$',fs1(i,3),fs1(i,4), fs1(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs1(i,1),
     :           fs1(i,2),'$',fs1(i,3),'@',fs1(i,4), fs1(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs1(i,1),
     :           '#',fs1(i,2),fs1(i,3),'@',fs1(i,4), fs1(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs1(i,1),
     :           '#',fs1(i,2),'$',fs1(i,3),'@',fs1(i,4), fs1(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs1(i,1),
     :           fs1(i,2),fs1(i,3),fs1(i,4), fs1(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
! after all the lines have been output, output the rest of the statistics...
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs1jmhzp/fs1a),
     :       (fs1hmkzp/fs1b), (fs1kzp/fs1c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
! output the amount each flagged residual is away...and the new mean and sem
! if its graeter than 3sigma in the second pass
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

! The code is identical for the rest of the standards...

        hmkvar = (fs2hmksq - fs2b * (fs2hmkzp/fs2b)**2)
        if( fs2b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs2b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs2b)

        jmhvar = (fs2jmhsq - fs2a * (fs2jmhzp/fs2a)**2)
        if( fs2a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs2a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs2a)

        kvar = (fs2ksq - fs2c * (fs2kzp/fs2c)**2)
        if( fs2c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs2c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs2c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS2'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, two
          if( fs2(i,2) .ne. -99.99) then
           if( ( fs2jmhzp * fs2(i,2)) .lt. 0) then
               jmhresid = abs(fs2jmhzp/fs2a) + abs(fs2(i,2))
             else if( ( fs2jmhzp * fs2(i,2)) .eq. 0) then
               jmhresid = abs(fs2jmhzp/fs2a) + abs(fs2(i,2))
             else
               jmhresid = abs(fs2jmhzp/fs2a) - abs(fs2(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs2jmhzp - fs2(i,2)
               jmhaway(p,3) = (newjmh/(fs2a-1))
               fs2jmhsq = fs2jmhsq - (fs2(i,2))**2
               jmhvar = (fs2jmhsq - (fs2a-1) * (newjmh/(fs2a-1))**2)
                 if( (fs2a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs2a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs2a-1)
               if( ( newjmh * fs2(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs2a-1)) + abs(fs2(i,2))
               else if( ( newjmh * fs2(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs2a-1)) + abs(fs2(i,2))
               else
                 jmhresid = abs(newjmh/(fs2a-1)) - abs(fs2(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs2(i,3) .ne. -99.99) then
             if( ( fs2hmkzp * fs2(i,3)) .lt. 0) then
               hmkresid = abs(fs2hmkzp/fs2b) + abs(fs2(i,3))
             else if( ( fs2hmkzp * fs2(i,3)) .eq. 0) then
               hmkresid = abs(fs2hmkzp/fs2b) + abs(fs2(i,3))
             else
               hmkresid = abs(fs2hmkzp/fs2b) - abs(fs2(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs2hmkzp - fs2(i,3)
               hmkaway(h,3) = newhmk/(fs2b-1)
               fs2hmksq = fs2hmksq - (fs2(i,3))**2
               hmkvar = (fs2hmksq - (fs2b-1) * (newhmk/(fs2b-1))**2)
                 if( (fs2b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs2b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs2b-1)
               if( ( newhmk * fs2(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs2b-1)) + abs(fs2(i,3))
               else if( ( newhmk * fs2(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs2b-1)) + abs(fs2(i,3))
               else
                 hmkresid = abs(newhmk/(fs2b-1)) - abs(fs2(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs2(i,4) .ne. -99.99) then
             if( ( fs2kzp * fs2(i,4)) .lt. 0) then
               kresid = abs(fs2kzp/fs2c) + abs(fs2(i,4))
             else if( ( fs2kzp * fs2(i,4)) .eq. 0) then
               kresid = abs(fs2kzp/fs2c) + abs(fs2(i,4))
             else
               kresid = abs(fs2kzp/fs2c) - abs(fs2(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs2kzp - fs2(i,4)
               kaway(k,3) = newk/(fs2c-1)
               fs2ksq = fs2ksq - (fs2(i,4))**2
               kvar = (fs2ksq - (fs2c-1) * (newk/(fs2c-1))**2)
                 if( (fs2c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs2c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs2c-1)
               if( ( newk * fs2(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs2c-1)) + abs(fs2(i,4))
               else if( ( newk * fs2(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs2c-1)) + abs(fs2(i,4))
               else
                 kresid = abs(newk/(fs2c-1)) - abs(fs2(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs2(i,1),
     :      '#',fs2(i,2),fs2(i,3),fs2(i,4), fs2(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs2(i,1),
     :           fs2(i,2),'$',fs2(i,3),fs2(i,4), fs2(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs2(i,1),
     :           fs2(i,2),fs2(i,3),'@',fs2(i,4), fs2(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs2(i,1),
     :           '#',fs2(i,2),'$',fs2(i,3),fs2(i,4), fs2(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs2(i,1),
     :           fs2(i,2),'$',fs2(i,3),'@',fs2(i,4), fs2(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs2(i,1),
     :           '#',fs2(i,2),fs2(i,3),'@',fs2(i,4), fs2(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs2(i,1),
     :           '#',fs2(i,2),'$',fs2(i,3),'@',fs2(i,4), fs2(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs2(i,1),
     :           fs2(i,2),fs2(i,3),fs2(i,4), fs2(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs2jmhzp/fs2a),
     :       (fs2hmkzp/fs2b), (fs2kzp/fs2c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs3hmksq - fs3b * (fs3hmkzp/fs3b)**2)
        if( fs3b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs3b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs3b)

        jmhvar = (fs3jmhsq - fs3a * (fs3jmhzp/fs3a)**2)
        if( fs3a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs3a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs3a)

        kvar = (fs3ksq - fs3c * (fs3kzp/fs3c)**2)
        if( fs3c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs3c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs3c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS3'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, three
           if( fs3(i,2) .ne. -99.99) then
             if( ( fs3jmhzp * fs3(i,2)) .lt. 0) then
               jmhresid = abs(fs3jmhzp/fs3a) + abs(fs3(i,2))
             else if( ( fs3jmhzp * fs3(i,2)) .eq. 0) then
               jmhresid = abs(fs3jmhzp/fs3a) + abs(fs3(i,2))
             else
               jmhresid = abs(fs3jmhzp/fs3a) - abs(fs3(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs3jmhzp - fs3(i,2)
               jmhaway(p,3) = (newjmh/(fs3a-1))
               fs3jmhsq = fs3jmhsq - (fs3(i,2))**2
               jmhvar = (fs3jmhsq - (fs3a-1) * (newjmh/(fs3a-1))**2)
                 if( (fs3a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs3a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs3a-1)
               if( ( newjmh * fs3(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs3a-1)) + abs(fs3(i,2))
               else if( ( newjmh * fs3(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs3a-1)) + abs(fs3(i,2))
               else
                 jmhresid = abs(newjmh/(fs3a-1)) - abs(fs3(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs3(i,3) .ne. -99.99) then
             if( ( fs3hmkzp * fs3(i,3)) .lt. 0) then
               hmkresid = abs(fs3hmkzp/fs3b) + abs(fs3(i,3))
             else if( ( fs3hmkzp * fs3(i,3)) .eq. 0) then
               hmkresid = abs(fs3hmkzp/fs3b) + abs(fs3(i,3))
             else
               hmkresid = abs(fs3hmkzp/fs3b) - abs(fs3(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs3hmkzp - fs3(i,3)
               hmkaway(h,3) = newhmk/(fs3b-1)
               fs3hmksq = fs3hmksq - (fs3(i,3))**2
               hmkvar = (fs3hmksq - (fs3b-1) * (newhmk/(fs3b-1))**2)
                 if( (fs3b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs3b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs3b-1)
               if( ( newhmk * fs3(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs3b-1)) + abs(fs3(i,3))
               else if( ( newhmk * fs3(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs3b-1)) + abs(fs3(i,3))
               else
                 hmkresid = abs(newhmk/(fs3b-1)) - abs(fs3(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs3(i,4) .ne. -99.99) then
             if( ( fs3kzp * fs3(i,4)) .lt. 0) then
               kresid = abs(fs3kzp/fs3c) + abs(fs3(i,4))
             else if( ( fs3kzp * fs3(i,4)) .eq. 0) then
               kresid = abs(fs3kzp/fs3c) + abs(fs3(i,4))
             else
               kresid = abs(fs3kzp/fs3c) - abs(fs3(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs3kzp - fs3(i,4)
               kaway(k,3) = newk/(fs3c-1)
               fs3ksq = fs3ksq - (fs3(i,4))**2
               kvar = (fs3ksq - (fs3c-1) * (newk/(fs3c-1))**2)
                 if( (fs3c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs3c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs3c-1)
               if( ( newk * fs3(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs3c-1)) + abs(fs3(i,4))
               else if( ( newk * fs3(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs3c-1)) + abs(fs3(i,4))
               else
                 kresid = abs(newk/(fs3c-1)) - abs(fs3(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs3(i,1),
     :      '#',fs3(i,2),fs3(i,3),fs3(i,4), fs3(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs3(i,1),
     :           fs3(i,2),'$',fs3(i,3),fs3(i,4), fs3(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs3(i,1),
     :           fs3(i,2),fs3(i,3),'@',fs3(i,4), fs3(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs3(i,1),
     :           '#',fs3(i,2),'$',fs3(i,3),fs3(i,4), fs3(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs3(i,1),
     :           fs3(i,2),'$',fs3(i,3),'@',fs3(i,4), fs3(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs3(i,1),
     :           '#',fs3(i,2),fs3(i,3),'@',fs3(i,4), fs3(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs3(i,1),
     :           '#',fs3(i,2),'$',fs3(i,3),'@',fs3(i,4), fs3(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs3(i,1),
     :           fs3(i,2),fs3(i,3),fs3(i,4), fs3(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs3jmhzp/fs3a),
     :       (fs3hmkzp/fs3b), (fs3kzp/fs3c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs4hmksq - fs4b * (fs4hmkzp/fs4b)**2)
        if( fs4b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs4b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs4b)

        jmhvar = (fs4jmhsq - fs4a * (fs4jmhzp/fs4a)**2)
        if( fs4a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs4a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs4a)

        kvar = (fs4ksq - fs4c * (fs4kzp/fs4c)**2)
        if( fs4c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs4c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs4c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS4'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, four
           if( fs4(i,2) .ne. -99.99) then
             if( ( fs4jmhzp * fs4(i,2)) .lt. 0) then
               jmhresid = abs(fs4jmhzp/fs4a) + abs(fs4(i,2))
             else if( ( fs4jmhzp * fs4(i,2)) .eq. 0) then
               jmhresid = abs(fs4jmhzp/fs4a) + abs(fs4(i,2))
             else
               jmhresid = abs(fs4jmhzp/fs4a) - abs(fs4(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs4jmhzp - fs4(i,2)
               jmhaway(p,3) = (newjmh/(fs4a-1))
               fs4jmhsq = fs4jmhsq - (fs4(i,2))**2
               jmhvar = (fs4jmhsq - (fs4a-1) * (newjmh/(fs4a-1))**2)
                 if( (fs4a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs4a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs4a-1)
               if( ( newjmh * fs4(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs4a-1)) + abs(fs4(i,2))
               else if( ( newjmh * fs4(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs4a-1)) + abs(fs4(i,2))
               else
                 jmhresid = abs(newjmh/(fs4a-1)) - abs(fs4(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs4(i,3) .ne. -99.99) then
             if( ( fs4hmkzp * fs4(i,3)) .lt. 0) then
               hmkresid = abs(fs4hmkzp/fs4b) + abs(fs4(i,3))
             else if( ( fs4hmkzp * fs4(i,3)) .eq. 0) then
               hmkresid = abs(fs4hmkzp/fs4b) + abs(fs4(i,3))
             else
               hmkresid = abs(fs4hmkzp/fs4b) - abs(fs4(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs4hmkzp - fs4(i,3)
               hmkaway(h,3) = newhmk/(fs4b-1)
               fs4hmksq = fs4hmksq - (fs4(i,3))**2
               hmkvar = (fs4hmksq - (fs4b-1) * (newhmk/(fs4b-1))**2)
                 if( (fs4b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs4b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs4b-1)
               if( ( newhmk * fs4(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs4b-1)) + abs(fs4(i,3))
               else if( ( newhmk * fs4(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs4b-1)) + abs(fs4(i,3))
               else
                 hmkresid = abs(newhmk/(fs4b-1)) - abs(fs4(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs4(i,4) .ne. -99.99) then
             if( ( fs4kzp * fs4(i,4)) .lt. 0) then
               kresid = abs(fs4kzp/fs4c) + abs(fs4(i,4))
             else if( ( fs4kzp * fs4(i,4)) .eq. 0) then
               kresid = abs(fs4kzp/fs4c) + abs(fs4(i,4))
             else
               kresid = abs(fs4kzp/fs4c) - abs(fs4(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs4kzp - fs4(i,4)
               kaway(k,3) = newk/(fs4c-1)
               fs4ksq = fs4ksq - (fs4(i,4))**2
               kvar = (fs4ksq - (fs4c-1) * (newk/(fs4c-1))**2)
                 if( (fs4c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs4c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs4c-1)
               if( ( newk * fs4(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs4c-1)) + abs(fs4(i,4))
               else if( ( newk * fs4(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs4c-1)) + abs(fs4(i,4))
               else
                 kresid = abs(newk/(fs4c-1)) - abs(fs4(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs4(i,1),
     :      '#',fs4(i,2),fs4(i,3),fs4(i,4), fs4(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs4(i,1),
     :           fs4(i,2),'$',fs4(i,3),fs4(i,4), fs4(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs4(i,1),
     :           fs4(i,2),fs4(i,3),'@',fs4(i,4), fs4(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs4(i,1),
     :           '#',fs4(i,2),'$',fs4(i,3),fs4(i,4), fs4(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs4(i,1),
     :           fs4(i,2),'$',fs4(i,3),'@',fs4(i,4), fs4(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs4(i,1),
     :           '#',fs4(i,2),fs4(i,3),'@',fs4(i,4), fs4(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs4(i,1),
     :           '#',fs4(i,2),'$',fs4(i,3),'@',fs4(i,4), fs4(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs4(i,1),
     :           fs4(i,2),fs4(i,3),fs4(i,4), fs4(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs4jmhzp/fs4a),
     :       (fs4hmkzp/fs4b), (fs4kzp/fs4c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs5hmksq - fs5b * (fs5hmkzp/fs5b)**2)
        if( fs5b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs5b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs5b)

        jmhvar = (fs5jmhsq - fs5a * (fs5jmhzp/fs5a)**2)
        if( fs5a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs5a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs5a)

        kvar = (fs5ksq - fs5c * (fs5kzp/fs5c)**2)
        if( fs5c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs5c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs5c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS5'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, five
           if( fs5(i,2) .ne. -99.99) then
             if( ( fs5jmhzp * fs5(i,2)) .lt. 0) then
               jmhresid = abs(fs5jmhzp/fs5a) + abs(fs5(i,2))
             else if( ( fs5jmhzp * fs5(i,2)) .eq. 0) then
               jmhresid = abs(fs5jmhzp/fs5a) + abs(fs5(i,2))
             else
               jmhresid = abs(fs5jmhzp/fs5a) - abs(fs5(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs5jmhzp - fs5(i,2)
               jmhaway(p,3) = (newjmh/(fs5a-1))
               fs5jmhsq = fs5jmhsq - (fs5(i,2))**2
               jmhvar = (fs5jmhsq - (fs5a-1) * (newjmh/(fs5a-1))**2)
                 if( (fs5a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs5a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs5a-1)
               if( ( newjmh * fs5(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs5a-1)) + abs(fs5(i,2))
               else if( ( newjmh * fs5(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs5a-1)) + abs(fs5(i,2))
               else
                 jmhresid = abs(newjmh/(fs5a-1)) - abs(fs5(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs5(i,3) .ne. -99.99) then
             if( ( fs5hmkzp * fs5(i,3)) .lt. 0) then
               hmkresid = abs(fs5hmkzp/fs5b) + abs(fs5(i,3))
             else if( ( fs5hmkzp * fs5(i,3)) .eq. 0) then
               hmkresid = abs(fs5hmkzp/fs5b) + abs(fs5(i,3))
             else
               hmkresid = abs(fs5hmkzp/fs5b) - abs(fs5(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs5hmkzp - fs5(i,3)
               hmkaway(h,3) = newhmk/(fs5b-1)
               fs5hmksq = fs5hmksq - (fs5(i,3))**2
               hmkvar = (fs5hmksq - (fs5b-1) * (newhmk/(fs5b-1))**2)
                 if( (fs5b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs5b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs5b-1)
               if( ( newhmk * fs5(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs5b-1)) + abs(fs5(i,3))
               else if( ( newhmk * fs5(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs5b-1)) + abs(fs5(i,3))
               else
                 hmkresid = abs(newhmk/(fs5b-1)) - abs(fs5(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs5(i,4) .ne. -99.99) then
             if( ( fs5kzp * fs5(i,4)) .lt. 0) then
               kresid = abs(fs5kzp/fs5c) + abs(fs5(i,4))
             else if( ( fs5kzp * fs5(i,4)) .eq. 0) then
               kresid = abs(fs5kzp/fs5c) + abs(fs5(i,4))
             else
               kresid = abs(fs5kzp/fs5c) - abs(fs5(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs5kzp - fs5(i,4)
               kaway(k,3) = newk/(fs5c-1)
               fs5ksq = fs5ksq - (fs5(i,4))**2
               kvar = (fs5ksq - (fs5c-1) * (newk/(fs5c-1))**2)
                 if( (fs5c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs5c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs5c-1)
               if( ( newk * fs5(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs5c-1)) + abs(fs5(i,4))
               else if( ( newk * fs5(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs5c-1)) + abs(fs5(i,4))
               else
                 kresid = abs(newk/(fs5c-1)) - abs(fs5(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs5(i,1),
     :      '#',fs5(i,2),fs5(i,3),fs5(i,4), fs5(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs5(i,1),
     :           fs5(i,2),'$',fs5(i,3),fs5(i,4), fs5(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs5(i,1),
     :           fs5(i,2),fs5(i,3),'@',fs5(i,4), fs5(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs5(i,1),
     :           '#',fs5(i,2),'$',fs5(i,3),fs5(i,4), fs5(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs5(i,1),
     :           fs5(i,2),'$',fs5(i,3),'@',fs5(i,4), fs5(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs5(i,1),
     :           '#',fs5(i,2),fs5(i,3),'@',fs5(i,4), fs5(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs5(i,1),
     :           '#',fs5(i,2),'$',fs5(i,3),'@',fs5(i,4), fs5(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs5(i,1),
     :           fs5(i,2),fs5(i,3),fs5(i,4), fs5(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs5jmhzp/fs5a),
     :       (fs5hmkzp/fs5b), (fs5kzp/fs5c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do


        hmkvar = (fs6hmksq - fs6b * (fs6hmkzp/fs6b)**2)
        if( fs6b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs6b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs6b)

        jmhvar = (fs6jmhsq - fs6a * (fs6jmhzp/fs6a)**2)
        if( fs6a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs6a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs6a)

        kvar = (fs6ksq - fs6c * (fs6kzp/fs6c)**2)
        if( fs6c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs6c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs6c)


        write(143, '(a)') ' '
        write(143, '(a)') '                           FS6'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, six
           if( fs6(i,2) .ne. -99.99) then
             if( ( fs6jmhzp * fs6(i,2)) .lt. 0) then
               jmhresid = abs(fs6jmhzp/fs6a) + abs(fs6(i,2))
             else if( ( fs6jmhzp * fs6(i,2)) .eq. 0) then
               jmhresid = abs(fs6jmhzp/fs6a) + abs(fs6(i,2))
             else
               jmhresid = abs(fs6jmhzp/fs6a) - abs(fs6(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs6jmhzp - fs6(i,2)
               jmhaway(p,3) = (newjmh/(fs6a-1))
               fs6jmhsq = fs6jmhsq - (fs6(i,2))**2
               jmhvar = (fs6jmhsq - (fs6a-1) * (newjmh/(fs6a-1))**2)
                 if( (fs6a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs6a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs6a-1)
               if( ( newjmh * fs6(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs6a-1)) + abs(fs6(i,2))
               else if( ( newjmh * fs6(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs6a-1)) + abs(fs6(i,2))
               else
                 jmhresid = abs(newjmh/(fs6a-1)) - abs(fs6(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs6(i,3) .ne. -99.99) then
             if( ( fs6hmkzp * fs6(i,3)) .lt. 0) then
               hmkresid = abs(fs6hmkzp/fs6b) + abs(fs6(i,3))
             else if( ( fs6hmkzp * fs6(i,3)) .eq. 0) then
               hmkresid = abs(fs6hmkzp/fs6b) + abs(fs6(i,3))
             else
               hmkresid = abs(fs6hmkzp/fs6b) - abs(fs6(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs6hmkzp - fs6(i,3)
               hmkaway(h,3) = newhmk/(fs6b-1)
               fs6hmksq = fs6hmksq - (fs6(i,3))**2
               hmkvar = (fs6hmksq - (fs6b-1) * (newhmk/(fs6b-1))**2)
                 if( (fs6b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs6b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs6b-1)
               if( ( newhmk * fs6(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs6b-1)) + abs(fs6(i,3))
               else if( ( newhmk * fs6(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs6b-1)) + abs(fs6(i,3))
               else
                 hmkresid = abs(newhmk/(fs6b-1)) - abs(fs6(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs6(i,4) .ne. -99.99) then
             if( ( fs6kzp * fs6(i,4)) .lt. 0) then
               kresid = abs(fs6kzp/fs6c) + abs(fs6(i,4))
             else if( ( fs6kzp * fs6(i,4)) .eq. 0) then
               kresid = abs(fs6kzp/fs6c) + abs(fs6(i,4))
             else
               kresid = abs(fs6kzp/fs6c) - abs(fs6(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs6kzp - fs6(i,4)
               kaway(k,3) = newk/(fs6c-1)
               fs6ksq = fs6ksq - (fs6(i,4))**2
               kvar = (fs6ksq - (fs6c-1) * (newk/(fs6c-1))**2)
                 if( (fs6c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs6c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs6c-1)
               if( ( newk * fs6(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs6c-1)) + abs(fs6(i,4))
               else if( ( newk * fs6(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs6c-1)) + abs(fs6(i,4))
               else
                 kresid = abs(newk/(fs6c-1)) - abs(fs6(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs6(i,1),
     :      '#',fs6(i,2),fs6(i,3),fs6(i,4), fs6(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs6(i,1),
     :           fs6(i,2),'$',fs6(i,3),fs6(i,4), fs6(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs6(i,1),
     :           fs6(i,2),fs6(i,3),'@',fs6(i,4), fs6(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs6(i,1),
     :           '#',fs6(i,2),'$',fs6(i,3),fs6(i,4), fs6(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs6(i,1),
     :           fs6(i,2),'$',fs6(i,3),'@',fs6(i,4), fs6(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs6(i,1),
     :           '#',fs6(i,2),fs6(i,3),'@',fs6(i,4), fs6(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs6(i,1),
     :           '#',fs6(i,2),'$',fs6(i,3),'@',fs6(i,4), fs6(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs6(i,1),
     :           fs6(i,2),fs6(i,3),fs6(i,4), fs6(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs6jmhzp/fs6a),
     :       (fs6hmkzp/fs6b), (fs6kzp/fs6c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do


        hmkvar = (fs7hmksq - fs7b * (fs7hmkzp/fs7b)**2)
        if( fs7b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs7b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs7b)

        jmhvar = (fs7jmhsq - fs7a * (fs7jmhzp/fs7a)**2)
        if( fs7a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs7a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs7a)

        kvar = (fs7ksq - fs7c * (fs7kzp/fs7c)**2)
        if( fs7c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs7c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs7c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS7'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, seven
           if( fs7(i,2) .ne. -99.99) then
             if( ( fs7jmhzp * fs7(i,2)) .lt. 0) then
               jmhresid = abs(fs7jmhzp/fs7a) + abs(fs7(i,2))
             else if( ( fs7jmhzp * fs7(i,2)) .eq. 0) then
               jmhresid = abs(fs7jmhzp/fs7a) + abs(fs7(i,2))
             else
               jmhresid = abs(fs7jmhzp/fs7a) - abs(fs7(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs7jmhzp - fs7(i,2)
               jmhaway(p,3) = (newjmh/(fs7a-1))
               fs7jmhsq = fs7jmhsq - (fs7(i,2))**2
               jmhvar = (fs7jmhsq - (fs7a-1) * (newjmh/(fs7a-1))**2)
                 if( (fs7a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs7a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs7a-1)
               if( ( newjmh * fs7(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs7a-1)) + abs(fs7(i,2))
               else if( ( newjmh * fs7(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs7a-1)) + abs(fs7(i,2))
               else
                 jmhresid = abs(newjmh/(fs7a-1)) - abs(fs7(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs7(i,3) .ne. -99.99) then
             if( ( fs7hmkzp * fs7(i,3)) .lt. 0) then
               hmkresid = abs(fs7hmkzp/fs7b) + abs(fs7(i,3))
             else if( ( fs7hmkzp * fs7(i,3)) .eq. 0) then
               hmkresid = abs(fs7hmkzp/fs7b) + abs(fs7(i,3))
             else
               hmkresid = abs(fs7hmkzp/fs7b) - abs(fs7(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs7hmkzp - fs7(i,3)
               hmkaway(h,3) = newhmk/(fs7b-1)
               fs7hmksq = fs7hmksq - (fs7(i,3))**2
               hmkvar = (fs7hmksq - (fs7b-1) * (newhmk/(fs7b-1))**2)
                 if( (fs7b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs7b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs7b-1)
               if( ( newhmk * fs7(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs7b-1)) + abs(fs7(i,3))
               else if( ( newhmk * fs7(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs7b-1)) + abs(fs7(i,3))
               else
                 hmkresid = abs(newhmk/(fs7b-1)) - abs(fs7(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs7(i,4) .ne. -99.99) then
             if( ( fs7kzp * fs7(i,4)) .lt. 0) then
               kresid = abs(fs7kzp/fs7c) + abs(fs7(i,4))
             else if( ( fs7kzp * fs7(i,4)) .eq. 0) then
               kresid = abs(fs7kzp/fs7c) + abs(fs7(i,4))
             else
               kresid = abs(fs7kzp/fs7c) - abs(fs7(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs7kzp - fs7(i,4)
               kaway(k,3) = newk/(fs7c-1)
               fs7ksq = fs7ksq - (fs7(i,4))**2
               kvar = (fs7ksq - (fs7c-1) * (newk/(fs7c-1))**2)
                 if( (fs7c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs7c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs7c-1)
               if( ( newk * fs7(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs7c-1)) + abs(fs7(i,4))
               else if( ( newk * fs7(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs7c-1)) + abs(fs7(i,4))
               else
                 kresid = abs(newk/(fs7c-1)) - abs(fs7(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs7(i,1),
     :      '#',fs7(i,2),fs7(i,3),fs7(i,4), fs7(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs7(i,1),
     :           fs7(i,2),'$',fs7(i,3),fs7(i,4), fs7(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs7(i,1),
     :           fs7(i,2),fs7(i,3),'@',fs7(i,4), fs7(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs7(i,1),
     :           '#',fs7(i,2),'$',fs7(i,3),fs7(i,4), fs7(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs7(i,1),
     :           fs7(i,2),'$',fs7(i,3),'@',fs7(i,4), fs7(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs7(i,1),
     :           '#',fs7(i,2),fs7(i,3),'@',fs7(i,4), fs7(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
            write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs7(i,1),
     :           '#',fs7(i,2),'$',fs7(i,3),'@',fs7(i,4), fs7(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs7(i,1),
     :           fs7(i,2),fs7(i,3),fs7(i,4), fs7(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs7jmhzp/fs7a),
     :       (fs7hmkzp/fs7b), (fs7kzp/fs7c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs10hmksq - fs10b * (fs10hmkzp/fs10b)**2)
        if( fs10b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs10b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs10b)

        jmhvar = (fs10jmhsq - fs10a * (fs10jmhzp/fs10a)**2)
        if( fs10a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs10a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs10a)

        kvar = (fs10ksq - fs10c * (fs10kzp/fs10c)**2)
        if( fs10c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs10c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs10c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS10'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, ten
           if( fs10(i,2) .ne. -99.99) then
             if( ( fs10jmhzp * fs10(i,2)) .lt. 0) then
               jmhresid = abs(fs10jmhzp/fs10a) + abs(fs10(i,2))
             else if( ( fs10jmhzp * fs10(i,2)) .eq. 0) then
               jmhresid = abs(fs10jmhzp/fs10a) + abs(fs10(i,2))
             else
               jmhresid = abs(fs10jmhzp/fs10a) - abs(fs10(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs10jmhzp - fs10(i,2)
               jmhaway(p,3) = (newjmh/(fs10a-1))
               fs10jmhsq = fs10jmhsq - (fs10(i,2))**2
               jmhvar = (fs10jmhsq - (fs10a-1) * (newjmh/(fs10a-1))**2)
                 if( (fs10a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs10a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs10a-1)
               if( ( newjmh * fs10(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs10a-1)) + abs(fs10(i,2))
               else if( ( newjmh * fs10(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs10a-1)) + abs(fs10(i,2))
               else
                 jmhresid = abs(newjmh/(fs10a-1)) - abs(fs10(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs10(i,3) .ne. -99.99) then
             if( ( fs10hmkzp * fs10(i,3)) .lt. 0) then
               hmkresid = abs(fs10hmkzp/fs10b) + abs(fs10(i,3))
             else if( ( fs10hmkzp * fs10(i,3)) .eq. 0) then
               hmkresid = abs(fs10hmkzp/fs10b) + abs(fs10(i,3))
             else
               hmkresid = abs(fs10hmkzp/fs10b) - abs(fs10(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs10hmkzp - fs10(i,3)
               hmkaway(h,3) = newhmk/(fs10b-1)
               fs10hmksq = fs10hmksq - (fs10(i,3))**2
               hmkvar = (fs10hmksq - (fs10b-1) * (newhmk/(fs10b-1))**2)
                 if( (fs10b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs10b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs10b-1)
               if( ( newhmk * fs10(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs10b-1)) + abs(fs10(i,3))
               else if( ( newhmk * fs10(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs10b-1)) + abs(fs10(i,3))
               else
                 hmkresid = abs(newhmk/(fs10b-1)) - abs(fs10(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs10(i,4) .ne. -99.99) then
             if( ( fs10kzp * fs10(i,4)) .lt. 0) then
               kresid = abs(fs10kzp/fs10c) + abs(fs10(i,4))
             else if( ( fs10kzp * fs10(i,4)) .eq. 0) then
               kresid = abs(fs10kzp/fs10c) + abs(fs10(i,4))
             else
               kresid = abs(fs10kzp/fs10c) - abs(fs10(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs10kzp - fs10(i,4)
               kaway(k,3) = newk/(fs10c-1)
               fs10ksq = fs10ksq - (fs10(i,4))**2
               kvar = (fs10ksq - (fs10c-1) * (newk/(fs10c-1))**2)
                 if( (fs10c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs10c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs10c-1)
               if( ( newk * fs10(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs10c-1)) + abs(fs10(i,4))
               else if( ( newk * fs10(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs10c-1)) + abs(fs10(i,4))
               else
                 kresid = abs(newk/(fs10c-1)) - abs(fs10(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs10(i,1),
     :      '#',fs10(i,2),fs10(i,3),fs10(i,4), fs10(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs10(i,1),
     :           fs10(i,2),'$',fs10(i,3),fs10(i,4), fs10(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs10(i,1),
     :           fs10(i,2),fs10(i,3),'@',fs10(i,4), fs10(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs10(i,1),
     :           '#',fs10(i,2),'$',fs10(i,3),fs10(i,4), fs10(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs10(i,1),
     :           fs10(i,2),'$',fs10(i,3),'@',fs10(i,4), fs10(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs10(i,1),
     :           '#',fs10(i,2),fs10(i,3),'@',fs10(i,4), fs10(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs10(i,1),
     :           '#',fs10(i,2),'$',fs10(i,3),'@',fs10(i,4), fs10(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs10(i,1),
     :           fs10(i,2),fs10(i,3),fs10(i,4), fs10(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs10jmhzp/fs10a),
     :       (fs10hmkzp/fs10b), (fs10kzp/fs10c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs11hmksq - fs11b * (fs11hmkzp/fs11b)**2)
        if( fs11b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs11b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs11b)

        jmhvar = (fs11jmhsq - fs11a * (fs11jmhzp/fs11a)**2)
        if( fs11a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs11a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs11a)

        kvar = (fs11ksq - fs11c * (fs11kzp/fs11c)**2)
        if( fs11c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs11c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs11c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS11'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, eleven
           if( fs11(i,2) .ne. -99.99) then
             if( ( fs11jmhzp * fs11(i,2)) .lt. 0) then
               jmhresid = abs(fs11jmhzp/fs11a) + abs(fs11(i,2))
             else if( ( fs11jmhzp * fs11(i,2)) .eq. 0) then
               jmhresid = abs(fs11jmhzp/fs11a) + abs(fs11(i,2))
             else
               jmhresid = abs(fs11jmhzp/fs11a) - abs(fs11(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs11jmhzp - fs11(i,2)
               jmhaway(p,3) = (newjmh/(fs11a-1))
               fs11jmhsq = fs11jmhsq - (fs11(i,2))**2
               jmhvar = (fs11jmhsq - (fs11a-1) * (newjmh/(fs11a-1))**2)
                 if( (fs11a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs11a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs11a-1)
               if( ( newjmh * fs11(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs11a-1)) + abs(fs11(i,2))
               else if( ( newjmh * fs11(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs11a-1)) + abs(fs11(i,2))
               else
                 jmhresid = abs(newjmh/(fs11a-1)) - abs(fs11(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs11(i,3) .ne. -99.99) then
             if( ( fs11hmkzp * fs11(i,3)) .lt. 0) then
               hmkresid = abs(fs11hmkzp/fs11b) + abs(fs11(i,3))
             else if( ( fs11hmkzp * fs11(i,3)) .eq. 0) then
               hmkresid = abs(fs11hmkzp/fs11b) + abs(fs11(i,3))
             else
               hmkresid = abs(fs11hmkzp/fs11b) - abs(fs11(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs11hmkzp - fs11(i,3)
               hmkaway(h,3) = newhmk/(fs11b-1)
               fs11hmksq = fs11hmksq - (fs11(i,3))**2
               hmkvar = (fs11hmksq - (fs11b-1) * (newhmk/(fs11b-1))**2)
                 if( (fs11b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs11b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs11b-1)
               if( ( newhmk * fs11(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs11b-1)) + abs(fs11(i,3))
               else if( ( newhmk * fs11(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs11b-1)) + abs(fs11(i,3))
               else
                 hmkresid = abs(newhmk/(fs11b-1)) - abs(fs11(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs11(i,4) .ne. -99.99) then
             if( ( fs11kzp * fs11(i,4)) .lt. 0) then
               kresid = abs(fs11kzp/fs11c) + abs(fs11(i,4))
             else if( ( fs11kzp * fs11(i,4)) .eq. 0) then
               kresid = abs(fs11kzp/fs11c) + abs(fs11(i,4))
             else
               kresid = abs(fs11kzp/fs11c) - abs(fs11(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs11kzp - fs11(i,4)
               kaway(k,3) = newk/(fs11c-1)
               fs11ksq = fs11ksq - (fs11(i,4))**2
               kvar = (fs11ksq - (fs11c-1) * (newk/(fs11c-1))**2)
                 if( (fs11c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs11c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs11c-1)
               if( ( newk * fs11(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs11c-1)) + abs(fs11(i,4))
               else if( ( newk * fs11(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs11c-1)) + abs(fs11(i,4))
               else
                 kresid = abs(newk/(fs11c-1)) - abs(fs11(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs11(i,1),
     :      '#',fs11(i,2),fs11(i,3),fs11(i,4), fs11(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs11(i,1),
     :           fs11(i,2),'$',fs11(i,3),fs11(i,4), fs11(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs11(i,1),
     :           fs11(i,2),fs11(i,3),'@',fs11(i,4), fs11(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs11(i,1),
     :           '#',fs11(i,2),'$',fs11(i,3),fs11(i,4), fs11(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs11(i,1),
     :           fs11(i,2),'$',fs11(i,3),'@',fs11(i,4), fs11(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs11(i,1),
     :           '#',fs11(i,2),fs11(i,3),'@',fs11(i,4), fs11(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs11(i,1),
     :           '#',fs11(i,2),'$',fs11(i,3),'@',fs11(i,4), fs11(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs11(i,1),
     :           fs11(i,2),fs11(i,3),fs11(i,4), fs11(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs11jmhzp/fs11a),
     :       (fs11hmkzp/fs11b), (fs11kzp/fs11c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do


        hmkvar = (fs12hmksq - fs12b * (fs12hmkzp/fs12b)**2)
        if( fs12b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs12b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs12b)

        jmhvar = (fs12jmhsq - fs12a * (fs12jmhzp/fs12a)**2)
        if( fs12a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs12a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs12a)

        kvar = (fs12ksq - fs12c * (fs12kzp/fs12c)**2)
        if( fs12c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs12c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs12c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS12'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, n
           if( fs12(i,2) .ne. -99.99) then
             if( ( fs12jmhzp * fs12(i,2)) .lt. 0) then
               jmhresid = abs(fs12jmhzp/fs12a) + abs(fs12(i,2))
             else if( ( fs12jmhzp * fs12(i,2)) .eq. 0) then
               jmhresid = abs(fs12jmhzp/fs12a) + abs(fs12(i,2))
             else
               jmhresid = abs(fs12jmhzp/fs12a) - abs(fs12(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs12jmhzp - fs12(i,2)
               jmhaway(p,3) = (newjmh/(fs12a-1))
               fs12jmhsq = fs12jmhsq - (fs12(i,2))**2
               jmhvar = (fs12jmhsq - (fs12a-1) * (newjmh/(fs12a-1))**2)
                 if( (fs12a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs12a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs12a-1)
               if( ( newjmh * fs12(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs12a-1)) + abs(fs12(i,2))
               else if( ( newjmh * fs12(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs12a-1)) + abs(fs12(i,2))
               else
                 jmhresid = abs(newjmh/(fs12a-1)) - abs(fs12(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs12(i,3) .ne. -99.99) then
             if( ( fs12hmkzp * fs12(i,3)) .lt. 0) then
               hmkresid = abs(fs12hmkzp/fs12b) + abs(fs12(i,3))
             else if( ( fs12hmkzp * fs12(i,3)) .eq. 0) then
               hmkresid = abs(fs12hmkzp/fs12b) + abs(fs12(i,3))
             else
               hmkresid = abs(fs12hmkzp/fs12b) - abs(fs12(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs12hmkzp - fs12(i,3)
               hmkaway(h,3) = newhmk/(fs12b-1)
               fs12hmksq = fs12hmksq - (fs12(i,3))**2
               hmkvar = (fs12hmksq - (fs12b-1) * (newhmk/(fs12b-1))**2)
                 if( (fs12b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs12b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs12b-1)
               if( ( newhmk * fs12(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs12b-1)) + abs(fs12(i,3))
               else if( ( newhmk * fs12(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs12b-1)) + abs(fs12(i,3))
               else
                 hmkresid = abs(newhmk/(fs12b-1)) - abs(fs12(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs12(i,4) .ne. -99.99) then
             if( ( fs12kzp * fs12(i,4)) .lt. 0) then
               kresid = abs(fs12kzp/fs12c) + abs(fs12(i,4))
             else if( ( fs12kzp * fs12(i,4)) .eq. 0) then
               kresid = abs(fs12kzp/fs12c) + abs(fs12(i,4))
             else
               kresid = abs(fs12kzp/fs12c) - abs(fs12(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs12kzp - fs12(i,4)
               kaway(k,3) = newk/(fs12c-1)
               fs12ksq = fs12ksq - (fs12(i,4))**2
               kvar = (fs12ksq - (fs12c-1) * (newk/(fs12c-1))**2)
                 if( (fs12c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs12c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs12c-1)
               if( ( newk * fs12(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs12c-1)) + abs(fs12(i,4))
               else if( ( newk * fs12(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs12c-1)) + abs(fs12(i,4))
               else
                 kresid = abs(newk/(fs12c-1)) - abs(fs12(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs12(i,1),
     :      '#',fs12(i,2),fs12(i,3),fs12(i,4), fs12(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs12(i,1),
     :           fs12(i,2),'$',fs12(i,3),fs12(i,4), fs12(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs12(i,1),
     :           fs12(i,2),fs12(i,3),'@',fs12(i,4), fs12(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs12(i,1),
     :           '#',fs12(i,2),'$',fs12(i,3),fs12(i,4), fs12(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs12(i,1),
     :           fs12(i,2),'$',fs12(i,3),'@',fs12(i,4), fs12(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs12(i,1),
     :           '#',fs12(i,2),fs12(i,3),'@',fs12(i,4), fs12(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs12(i,1),
     :           '#',fs12(i,2),'$',fs12(i,3),'@',fs12(i,4), fs12(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs12(i,1),
     :           fs12(i,2),fs12(i,3),fs12(i,4), fs12(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs12jmhzp/fs12a),
     :       (fs12hmkzp/fs12b), (fs12kzp/fs12c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do


        hmkvar = (fs13hmksq - fs13b * (fs13hmkzp/fs13b)**2)
        if( fs13b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs13b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs13b)

        jmhvar = (fs13jmhsq - fs13a * (fs13jmhzp/fs13a)**2)
        if( fs13a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs13a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs13a)

        kvar = (fs13ksq - fs13c * (fs13kzp/fs13c)**2)
        if( fs13c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs13c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs13c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS13'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, thirteen
           if( fs13(i,2) .ne. -99.99) then
             if( ( fs13jmhzp * fs13(i,2)) .lt. 0) then
               jmhresid = abs(fs13jmhzp/fs13a) + abs(fs13(i,2))
             else if( ( fs13jmhzp * fs13(i,2)) .eq. 0) then
               jmhresid = abs(fs13jmhzp/fs13a) + abs(fs13(i,2))
             else
               jmhresid = abs(fs13jmhzp/fs13a) - abs(fs13(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs13jmhzp - fs13(i,2)
               jmhaway(p,3) = (newjmh/(fs13a-1))
               fs13jmhsq = fs13jmhsq - (fs13(i,2))**2
               jmhvar = (fs13jmhsq - (fs13a-1) * (newjmh/(fs13a-1))**2)
                 if( (fs13a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs13a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs13a-1)
               if( ( newjmh * fs13(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs13a-1)) + abs(fs13(i,2))
               else if( ( newjmh * fs13(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs13a-1)) + abs(fs13(i,2))
               else
                 jmhresid = abs(newjmh/(fs13a-1)) - abs(fs13(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs13(i,3) .ne. -99.99) then
             if( ( fs13hmkzp * fs13(i,3)) .lt. 0) then
               hmkresid = abs(fs13hmkzp/fs13b) + abs(fs13(i,3))
             else if( ( fs13hmkzp * fs13(i,3)) .eq. 0) then
               hmkresid = abs(fs13hmkzp/fs13b) + abs(fs13(i,3))
             else
               hmkresid = abs(fs13hmkzp/fs13b) - abs(fs13(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs13hmkzp - fs13(i,3)
               hmkaway(h,3) = newhmk/(fs13b-1)
               fs13hmksq = fs13hmksq - (fs13(i,3))**2
               hmkvar = (fs13hmksq - (fs13b-1) * (newhmk/(fs13b-1))**2)
                 if( (fs13b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs13b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs13b-1)
               if( ( newhmk * fs13(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs13b-1)) + abs(fs13(i,3))
               else if( ( newhmk * fs13(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs13b-1)) + abs(fs13(i,3))
               else
                 hmkresid = abs(newhmk/(fs13b-1)) - abs(fs13(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs13(i,4) .ne. -99.99) then
             if( ( fs13kzp * fs13(i,4)) .lt. 0) then
               kresid = abs(fs13kzp/fs13c) + abs(fs13(i,4))
             else if( ( fs13kzp * fs13(i,4)) .eq. 0) then
               kresid = abs(fs13kzp/fs13c) + abs(fs13(i,4))
             else
               kresid = abs(fs13kzp/fs13c) - abs(fs13(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs13kzp - fs13(i,4)
               kaway(k,3) = newk/(fs13c-1)
               fs13ksq = fs13ksq - (fs13(i,4))**2
               kvar = (fs13ksq - (fs13c-1) * (newk/(fs13c-1))**2)
                 if( (fs13c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs13c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs13c-1)
               if( ( newk * fs13(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs13c-1)) + abs(fs13(i,4))
               else if( ( newk * fs13(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs13c-1)) + abs(fs13(i,4))
               else
                 kresid = abs(newk/(fs13c-1)) - abs(fs13(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs13(i,1),
     :      '#',fs13(i,2),fs13(i,3),fs13(i,4), fs13(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs13(i,1),
     :           fs13(i,2),'$',fs13(i,3),fs13(i,4), fs13(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs13(i,1),
     :           fs13(i,2),fs13(i,3),'@',fs13(i,4), fs13(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs13(i,1),
     :           '#',fs13(i,2),'$',fs13(i,3),fs13(i,4), fs13(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs13(i,1),
     :           fs13(i,2),'$',fs13(i,3),'@',fs13(i,4), fs13(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs13(i,1),
     :           '#',fs13(i,2),fs13(i,3),'@',fs13(i,4), fs13(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs13(i,1),
     :           '#',fs13(i,2),'$',fs13(i,3),'@',fs13(i,4), fs13(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs13(i,1),
     :           fs13(i,2),fs13(i,3),fs13(i,4), fs13(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs13jmhzp/fs13a),
     :       (fs13hmkzp/fs13b), (fs13kzp/fs13c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs14hmksq - fs14b * (fs14hmkzp/fs14b)**2)
        if( fs14b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs14b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs14b)

        jmhvar = (fs14jmhsq - fs14a * (fs14jmhzp/fs14a)**2)
        if( fs14a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs14a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs14a)

        kvar = (fs14ksq - fs14c * (fs14kzp/fs14c)**2)
        if( fs14c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs14c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs14c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS14'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, fourteen
           if( fs14(i,2) .ne. -99.99) then
             if( ( fs14jmhzp * fs14(i,2)) .lt. 0) then
               jmhresid = abs(fs14jmhzp/fs14a) + abs(fs14(i,2))
             else if( ( fs14jmhzp * fs14(i,2)) .eq. 0) then
               jmhresid = abs(fs14jmhzp/fs14a) + abs(fs14(i,2))
             else
               jmhresid = abs(fs14jmhzp/fs14a) - abs(fs14(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs14jmhzp - fs14(i,2)
               jmhaway(p,3) = (newjmh/(fs14a-1))
               fs14jmhsq = fs14jmhsq - (fs14(i,2))**2
               jmhvar = (fs14jmhsq - (fs14a-1) * (newjmh/(fs14a-1))**2)
                 if( (fs14a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs14a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs14a-1)
               if( ( newjmh * fs14(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs14a-1)) + abs(fs14(i,2))
               else if( ( newjmh * fs14(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs14a-1)) + abs(fs14(i,2))
               else
                 jmhresid = abs(newjmh/(fs14a-1)) - abs(fs14(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs14(i,3) .ne. -99.99) then
             if( ( fs14hmkzp * fs14(i,3)) .lt. 0) then
               hmkresid = abs(fs14hmkzp/fs14b) + abs(fs14(i,3))
             else if( ( fs14hmkzp * fs14(i,3)) .eq. 0) then
               hmkresid = abs(fs14hmkzp/fs14b) + abs(fs14(i,3))
             else
               hmkresid = abs(fs14hmkzp/fs14b) - abs(fs14(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs14hmkzp - fs14(i,3)
               hmkaway(h,3) = newhmk/(fs14b-1)
               fs14hmksq = fs14hmksq - (fs14(i,3))**2
               hmkvar = (fs14hmksq - (fs14b-1) * (newhmk/(fs14b-1))**2)
                 if( (fs14b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs14b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs14b-1)
               if( ( newhmk * fs14(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs14b-1)) + abs(fs14(i,3))
               else if( ( newhmk * fs14(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs14b-1)) + abs(fs14(i,3))
               else
                 hmkresid = abs(newhmk/(fs14b-1)) - abs(fs14(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs14(i,4) .ne. -99.99) then
             if( ( fs14kzp * fs14(i,4)) .lt. 0) then
               kresid = abs(fs14kzp/fs14c) + abs(fs14(i,4))
             else if( ( fs14kzp * fs14(i,4)) .eq. 0) then
               kresid = abs(fs14kzp/fs14c) + abs(fs14(i,4))
             else
               kresid = abs(fs14kzp/fs14c) - abs(fs14(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs14kzp - fs14(i,4)
               kaway(k,3) = newk/(fs14c-1)
               fs14ksq = fs14ksq - (fs14(i,4))**2
               kvar = (fs14ksq - (fs14c-1) * (newk/(fs14c-1))**2)
                 if( (fs14c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs14c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs14c-1)
               if( ( newk * fs14(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs14c-1)) + abs(fs14(i,4))
               else if( ( newk * fs14(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs14c-1)) + abs(fs14(i,4))
               else
                 kresid = abs(newk/(fs14c-1)) - abs(fs14(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs14(i,1),
     :      '#',fs14(i,2),fs14(i,3),fs14(i,4), fs14(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs14(i,1),
     :           fs14(i,2),'$',fs14(i,3),fs14(i,4), fs14(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs14(i,1),
     :           fs14(i,2),fs14(i,3),'@',fs14(i,4), fs14(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs14(i,1),
     :           '#',fs14(i,2),'$',fs14(i,3),fs14(i,4), fs14(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs14(i,1),
     :           fs14(i,2),'$',fs14(i,3),'@',fs14(i,4), fs14(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs14(i,1),
     :           '#',fs14(i,2),fs14(i,3),'@',fs14(i,4), fs14(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs14(i,1),
     :           '#',fs14(i,2),'$',fs14(i,3),'@',fs14(i,4), fs14(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs14(i,1),
     :           fs14(i,2),fs14(i,3),fs14(i,4), fs14(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs14jmhzp/fs14a),
     :       (fs14hmkzp/fs14b), (fs14kzp/fs14c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs15hmksq - fs15b * (fs15hmkzp/fs15b)**2)
        if( fs15b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs15b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs15b)

        jmhvar = (fs15jmhsq - fs15a * (fs15jmhzp/fs15a)**2)
        if( fs15a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs15a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs15a)

        kvar = (fs15ksq - fs15c * (fs15kzp/fs15c)**2)
        if( fs15c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs15c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs15c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS15'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, fifteen
           if( fs15(i,2) .ne. -99.99) then
             if( ( fs15jmhzp * fs15(i,2)) .lt. 0) then
               jmhresid = abs(fs15jmhzp/fs15a) + abs(fs15(i,2))
             else if( ( fs15jmhzp * fs15(i,2)) .eq. 0) then
               jmhresid = abs(fs15jmhzp/fs15a) + abs(fs15(i,2))
             else
               jmhresid = abs(fs15jmhzp/fs15a) - abs(fs15(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs15jmhzp - fs15(i,2)
               jmhaway(p,3) = (newjmh/(fs15a-1))
               fs15jmhsq = fs15jmhsq - (fs15(i,2))**2
               jmhvar = (fs15jmhsq - (fs15a-1) * (newjmh/(fs15a-1))**2)
                 if( (fs15a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs15a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs15a-1)
               if( ( newjmh * fs15(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs15a-1)) + abs(fs15(i,2))
               else if( ( newjmh * fs15(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs15a-1)) + abs(fs15(i,2))
               else
                 jmhresid = abs(newjmh/(fs15a-1)) - abs(fs15(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs15(i,3) .ne. -99.99) then
             if( ( fs15hmkzp * fs15(i,3)) .lt. 0) then
               hmkresid = abs(fs15hmkzp/fs15b) + abs(fs15(i,3))
             else if( ( fs15hmkzp * fs15(i,3)) .eq. 0) then
               hmkresid = abs(fs15hmkzp/fs15b) + abs(fs15(i,3))
             else
               hmkresid = abs(fs15hmkzp/fs15b) - abs(fs15(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs15hmkzp - fs15(i,3)
               hmkaway(h,3) = newhmk/(fs15b-1)
               fs15hmksq = fs15hmksq - (fs15(i,3))**2
               hmkvar = (fs15hmksq - (fs15b-1) * (newhmk/(fs15b-1))**2)
                 if( (fs15b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs15b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs15b-1)
               if( ( newhmk * fs15(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs15b-1)) + abs(fs15(i,3))
               else if( ( newhmk * fs15(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs15b-1)) + abs(fs15(i,3))
               else
                 hmkresid = abs(newhmk/(fs15b-1)) - abs(fs15(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs15(i,4) .ne. -99.99) then
             if( ( fs15kzp * fs15(i,4)) .lt. 0) then
               kresid = abs(fs15kzp/fs15c) + abs(fs15(i,4))
             else if( ( fs15kzp * fs15(i,4)) .eq. 0) then
               kresid = abs(fs15kzp/fs15c) + abs(fs15(i,4))
             else
               kresid = abs(fs15kzp/fs15c) - abs(fs15(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs15kzp - fs15(i,4)
               kaway(k,3) = newk/(fs15c-1)
               fs15ksq = fs15ksq - (fs15(i,4))**2
               kvar = (fs15ksq - (fs15c-1) * (newk/(fs15c-1))**2)
                 if( (fs15c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs15c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs15c-1)
               if( ( newk * fs15(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs15c-1)) + abs(fs15(i,4))
               else if( ( newk * fs15(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs15c-1)) + abs(fs15(i,4))
               else
                 kresid = abs(newk/(fs15c-1)) - abs(fs15(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs15(i,1),
     :      '#',fs15(i,2),fs15(i,3),fs15(i,4), fs15(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs15(i,1),
     :           fs15(i,2),'$',fs15(i,3),fs15(i,4), fs15(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs15(i,1),
     :           fs15(i,2),fs15(i,3),'@',fs15(i,4), fs15(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs15(i,1),
     :           '#',fs15(i,2),'$',fs15(i,3),fs15(i,4), fs15(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs15(i,1),
     :           fs15(i,2),'$',fs15(i,3),'@',fs15(i,4), fs15(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs15(i,1),
     :           '#',fs15(i,2),fs15(i,3),'@',fs15(i,4), fs15(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs15(i,1),
     :           '#',fs15(i,2),'$',fs15(i,3),'@',fs15(i,4), fs15(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs15(i,1),
     :           fs15(i,2),fs15(i,3),fs15(i,4), fs15(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs15jmhzp/fs15a),
     :       (fs15hmkzp/fs15b), (fs15kzp/fs15c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do


        hmkvar = (fs16hmksq - fs16b * (fs16hmkzp/fs16b)**2)
        if( fs16b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs16b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs16b)

        jmhvar = (fs16jmhsq - fs16a * (fs16jmhzp/fs16a)**2)
        if( fs16a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs16a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs16a)

        kvar = (fs16ksq - fs16c * (fs16kzp/fs16c)**2)
        if( fs16c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs16c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs16c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS16'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, sixteen
           if( fs16(i,2) .ne. -99.99) then
             if( ( fs16jmhzp * fs16(i,2)) .lt. 0) then
               jmhresid = abs(fs16jmhzp/fs16a) + abs(fs16(i,2))
             else if( ( fs16jmhzp * fs16(i,2)) .eq. 0) then
               jmhresid = abs(fs16jmhzp/fs16a) + abs(fs16(i,2))
             else
               jmhresid = abs(fs16jmhzp/fs16a) - abs(fs16(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs16jmhzp - fs16(i,2)
               jmhaway(p,3) = (newjmh/(fs16a-1))
               fs16jmhsq = fs16jmhsq - (fs16(i,2))**2
               jmhvar = (fs16jmhsq - (fs16a-1) * (newjmh/(fs16a-1))**2)
                 if( (fs16a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs16a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs16a-1)
               if( ( newjmh * fs16(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs16a-1)) + abs(fs16(i,2))
               else if( ( newjmh * fs16(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs16a-1)) + abs(fs16(i,2))
               else
                 jmhresid = abs(newjmh/(fs16a-1)) - abs(fs16(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs16(i,3) .ne. -99.99) then
             if( ( fs16hmkzp * fs16(i,3)) .lt. 0) then
               hmkresid = abs(fs16hmkzp/fs16b) + abs(fs16(i,3))
             else if( ( fs16hmkzp * fs16(i,3)) .eq. 0) then
               hmkresid = abs(fs16hmkzp/fs16b) + abs(fs16(i,3))
             else
               hmkresid = abs(fs16hmkzp/fs16b) - abs(fs16(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs16hmkzp - fs16(i,3)
               hmkaway(h,3) = newhmk/(fs16b-1)
               fs16hmksq = fs16hmksq - (fs16(i,3))**2
               hmkvar = (fs16hmksq - (fs16b-1) * (newhmk/(fs16b-1))**2)
                 if( (fs16b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs16b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs16b-1)
               if( ( newhmk * fs16(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs16b-1)) + abs(fs16(i,3))
               else if( ( newhmk * fs16(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs16b-1)) + abs(fs16(i,3))
               else
                 hmkresid = abs(newhmk/(fs16b-1)) - abs(fs16(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs16(i,4) .ne. -99.99) then
             if( ( fs16kzp * fs16(i,4)) .lt. 0) then
               kresid = abs(fs16kzp/fs16c) + abs(fs16(i,4))
             else if( ( fs16kzp * fs16(i,4)) .eq. 0) then
               kresid = abs(fs16kzp/fs16c) + abs(fs16(i,4))
             else
               kresid = abs(fs16kzp/fs16c) - abs(fs16(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs16kzp - fs16(i,4)
               kaway(k,3) = newk/(fs16c-1)
               fs16ksq = fs16ksq - (fs16(i,4))**2
               kvar = (fs16ksq - (fs16c-1) * (newk/(fs16c-1))**2)
                 if( (fs16c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs16c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs16c-1)
               if( ( newk * fs16(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs16c-1)) + abs(fs16(i,4))
               else if( ( newk * fs16(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs16c-1)) + abs(fs16(i,4))
               else
                 kresid = abs(newk/(fs16c-1)) - abs(fs16(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs16(i,1),
     :      '#',fs16(i,2),fs16(i,3),fs16(i,4), fs16(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs16(i,1),
     :           fs16(i,2),'$',fs16(i,3),fs16(i,4), fs16(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs16(i,1),
     :           fs16(i,2),fs16(i,3),'@',fs16(i,4), fs16(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs16(i,1),
     :           '#',fs16(i,2),'$',fs16(i,3),fs16(i,4), fs16(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs16(i,1),
     :           fs16(i,2),'$',fs16(i,3),'@',fs16(i,4), fs16(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs16(i,1),
     :           '#',fs16(i,2),fs16(i,3),'@',fs16(i,4), fs16(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs16(i,1),
     :           '#',fs16(i,2),'$',fs16(i,3),'@',fs16(i,4), fs16(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs16(i,1),
     :           fs16(i,2),fs16(i,3),fs16(i,4), fs16(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs16jmhzp/fs16a),
     :       (fs16hmkzp/fs16b), (fs16kzp/fs16c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs17hmksq - fs17b * (fs17hmkzp/fs17b)**2)
        if( fs17b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs17b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs17b)

        jmhvar = (fs17jmhsq - fs17a * (fs17jmhzp/fs17a)**2)
        if( fs17a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs17a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs17a)

        kvar = (fs17ksq - fs17c * (fs17kzp/fs17c)**2)
        if( fs17c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs17c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs17c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS17'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, seventeen
           if( fs17(i,2) .ne. -99.99) then
             if( ( fs17jmhzp * fs17(i,2)) .lt. 0) then
               jmhresid = abs(fs17jmhzp/fs17a) + abs(fs17(i,2))
             else if( ( fs17jmhzp * fs17(i,2)) .eq. 0) then
               jmhresid = abs(fs17jmhzp/fs17a) + abs(fs17(i,2))
             else
               jmhresid = abs(fs17jmhzp/fs17a) - abs(fs17(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs17jmhzp - fs17(i,2)
               jmhaway(p,3) = (newjmh/(fs17a-1))
               fs17jmhsq = fs17jmhsq - (fs17(i,2))**2
               jmhvar = (fs17jmhsq - (fs17a-1) * (newjmh/(fs17a-1))**2)
                 if( (fs17a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs17a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs17a-1)
               if( ( newjmh * fs17(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs17a-1)) + abs(fs17(i,2))
               else if( ( newjmh * fs17(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs17a-1)) + abs(fs17(i,2))
               else
                 jmhresid = abs(newjmh/(fs17a-1)) - abs(fs17(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs17(i,3) .ne. -99.99) then
             if( ( fs17hmkzp * fs17(i,3)) .lt. 0) then
               hmkresid = abs(fs17hmkzp/fs17b) + abs(fs17(i,3))
             else if( ( fs17hmkzp * fs17(i,3)) .eq. 0) then
               hmkresid = abs(fs17hmkzp/fs17b) + abs(fs17(i,3))
             else
               hmkresid = abs(fs17hmkzp/fs17b) - abs(fs17(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs17hmkzp - fs17(i,3)
               hmkaway(h,3) = newhmk/(fs17b-1)
               fs17hmksq = fs17hmksq - (fs17(i,3))**2
               hmkvar = (fs17hmksq - (fs17b-1) * (newhmk/(fs17b-1))**2)
                 if( (fs17b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs17b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs17b-1)
               if( ( newhmk * fs17(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs17b-1)) + abs(fs17(i,3))
               else if( ( newhmk * fs17(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs17b-1)) + abs(fs17(i,3))
               else
                 hmkresid = abs(newhmk/(fs17b-1)) - abs(fs17(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs17(i,4) .ne. -99.99) then
             if( ( fs17kzp * fs17(i,4)) .lt. 0) then
               kresid = abs(fs17kzp/fs17c) + abs(fs17(i,4))
             else if( ( fs17kzp * fs17(i,4)) .eq. 0) then
               kresid = abs(fs17kzp/fs17c) + abs(fs17(i,4))
             else
               kresid = abs(fs17kzp/fs17c) - abs(fs17(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs17kzp - fs17(i,4)
               kaway(k,3) = newk/(fs17c-1)
               fs17ksq = fs17ksq - (fs17(i,4))**2
               kvar = (fs17ksq - (fs17c-1) * (newk/(fs17c-1))**2)
                 if( (fs17c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs17c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs17c-1)
               if( ( newk * fs17(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs17c-1)) + abs(fs17(i,4))
               else if( ( newk * fs17(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs17c-1)) + abs(fs17(i,4))
               else
                 kresid = abs(newk/(fs17c-1)) - abs(fs17(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs17(i,1),
     :      '#',fs17(i,2),fs17(i,3),fs17(i,4), fs17(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs17(i,1),
     :           fs17(i,2),'$',fs17(i,3),fs17(i,4), fs17(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs17(i,1),
     :           fs17(i,2),fs17(i,3),'@',fs17(i,4), fs17(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs17(i,1),
     :           '#',fs17(i,2),'$',fs17(i,3),fs17(i,4), fs17(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs17(i,1),
     :           fs17(i,2),'$',fs17(i,3),'@',fs17(i,4), fs17(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs17(i,1),
     :           '#',fs17(i,2),fs17(i,3),'@',fs17(i,4), fs17(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs17(i,1),
     :           '#',fs17(i,2),'$',fs17(i,3),'@',fs17(i,4), fs17(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs17(i,1),
     :           fs17(i,2),fs17(i,3),fs17(i,4), fs17(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs17jmhzp/fs17a),
     :       (fs17hmkzp/fs17b), (fs17kzp/fs17c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs18hmksq - fs18b * (fs18hmkzp/fs18b)**2)
        if( fs18b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs18b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs18b)

        jmhvar = (fs18jmhsq - fs18a * (fs18jmhzp/fs18a)**2)
        if( fs18a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs18a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs18a)

        kvar = (fs18ksq - fs18c * (fs18kzp/fs18c)**2)
        if( fs18c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs18c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs18c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS18'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, eighteen
           if( fs18(i,2) .ne. -99.99) then
             if( ( fs18jmhzp * fs18(i,2)) .lt. 0) then
               jmhresid = abs(fs18jmhzp/fs18a) + abs(fs18(i,2))
             else if( ( fs18jmhzp * fs18(i,2)) .eq. 0) then
               jmhresid = abs(fs18jmhzp/fs18a) + abs(fs18(i,2))
             else
               jmhresid = abs(fs18jmhzp/fs18a) - abs(fs18(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs18jmhzp - fs18(i,2)
               jmhaway(p,3) = (newjmh/(fs18a-1))
               fs18jmhsq = fs18jmhsq - (fs18(i,2))**2
               jmhvar = (fs18jmhsq - (fs18a-1) * (newjmh/(fs18a-1))**2)
                 if( (fs18a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs18a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs18a-1)
               if( ( newjmh * fs18(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs18a-1)) + abs(fs18(i,2))
               else if( ( newjmh * fs18(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs18a-1)) + abs(fs18(i,2))
               else
                 jmhresid = abs(newjmh/(fs18a-1)) - abs(fs18(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs18(i,3) .ne. -99.99) then
             if( ( fs18hmkzp * fs18(i,3)) .lt. 0) then
               hmkresid = abs(fs18hmkzp/fs18b) + abs(fs18(i,3))
             else if( ( fs18hmkzp * fs18(i,3)) .eq. 0) then
               hmkresid = abs(fs18hmkzp/fs18b) + abs(fs18(i,3))
             else
               hmkresid = abs(fs18hmkzp/fs18b) - abs(fs18(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs18hmkzp - fs18(i,3)
               hmkaway(h,3) = newhmk/(fs18b-1)
               fs18hmksq = fs18hmksq - (fs18(i,3))**2
               hmkvar = (fs18hmksq - (fs18b-1) * (newhmk/(fs18b-1))**2)
                 if( (fs18b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs18b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs18b-1)
               if( ( newhmk * fs18(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs18b-1)) + abs(fs18(i,3))
               else if( ( newhmk * fs18(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs18b-1)) + abs(fs18(i,3))
               else
                 hmkresid = abs(newhmk/(fs18b-1)) - abs(fs18(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs18(i,4) .ne. -99.99) then
             if( ( fs18kzp * fs18(i,4)) .lt. 0) then
               kresid = abs(fs18kzp/fs18c) + abs(fs18(i,4))
             else if( ( fs18kzp * fs18(i,4)) .eq. 0) then
               kresid = abs(fs18kzp/fs18c) + abs(fs18(i,4))
             else
               kresid = abs(fs18kzp/fs18c) - abs(fs18(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs18kzp - fs18(i,4)
               kaway(k,3) = newk/(fs18c-1)
               fs18ksq = fs18ksq - (fs18(i,4))**2
               kvar = (fs18ksq - (fs18c-1) * (newk/(fs18c-1))**2)
                 if( (fs18c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs18c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs18c-1)
               if( ( newk * fs18(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs18c-1)) + abs(fs18(i,4))
               else if( ( newk * fs18(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs18c-1)) + abs(fs18(i,4))
               else
                 kresid = abs(newk/(fs18c-1)) - abs(fs18(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs18(i,1),
     :      '#',fs18(i,2),fs18(i,3),fs18(i,4), fs18(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs18(i,1),
     :           fs18(i,2),'$',fs18(i,3),fs18(i,4), fs18(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs18(i,1),
     :           fs18(i,2),fs18(i,3),'@',fs18(i,4), fs18(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs18(i,1),
     :           '#',fs18(i,2),'$',fs18(i,3),fs18(i,4), fs18(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs18(i,1),
     :           fs18(i,2),'$',fs18(i,3),'@',fs18(i,4), fs18(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs18(i,1),
     :           '#',fs18(i,2),fs18(i,3),'@',fs18(i,4), fs18(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs18(i,1),
     :           '#',fs18(i,2),'$',fs18(i,3),'@',fs18(i,4), fs18(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs18(i,1),
     :           fs18(i,2),fs18(i,3),fs18(i,4), fs18(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs18jmhzp/fs18a),
     :       (fs18hmkzp/fs18b), (fs18kzp/fs18c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs19hmksq - fs19b * (fs19hmkzp/fs19b)**2)
        if( fs19b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs19b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs19b)

        jmhvar = (fs19jmhsq - fs19a * (fs19jmhzp/fs19a)**2)
        if( fs19a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs19a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs19a)

        kvar = (fs19ksq - fs19c * (fs19kzp/fs19c)**2)
        if( fs19c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs19c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs19c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS19'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, nineteen
           if( fs19(i,2) .ne. -99.99) then
             if( ( fs19jmhzp * fs19(i,2)) .lt. 0) then
               jmhresid = abs(fs19jmhzp/fs19a) + abs(fs19(i,2))
             else if( ( fs19jmhzp * fs19(i,2)) .eq. 0) then
               jmhresid = abs(fs19jmhzp/fs19a) + abs(fs19(i,2))
             else
               jmhresid = abs(fs19jmhzp/fs19a) - abs(fs19(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs19jmhzp - fs19(i,2)
               jmhaway(p,3) = (newjmh/(fs19a-1))
               fs19jmhsq = fs19jmhsq - (fs19(i,2))**2
               jmhvar = (fs19jmhsq - (fs19a-1) * (newjmh/(fs19a-1))**2)
                 if( (fs19a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs19a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs19a-1)
               if( ( newjmh * fs19(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs19a-1)) + abs(fs19(i,2))
               else if( ( newjmh * fs19(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs19a-1)) + abs(fs19(i,2))
               else
                 jmhresid = abs(newjmh/(fs19a-1)) - abs(fs19(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs19(i,3) .ne. -99.99) then
             if( ( fs19hmkzp * fs19(i,3)) .lt. 0) then
               hmkresid = abs(fs19hmkzp/fs19b) + abs(fs19(i,3))
             else if( ( fs19hmkzp * fs19(i,3)) .eq. 0) then
               hmkresid = abs(fs19hmkzp/fs19b) + abs(fs19(i,3))
             else
               hmkresid = abs(fs19hmkzp/fs19b) - abs(fs19(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs19hmkzp - fs19(i,3)
               hmkaway(h,3) = newhmk/(fs19b-1)
               fs19hmksq = fs19hmksq - (fs19(i,3))**2
               hmkvar = (fs19hmksq - (fs19b-1) * (newhmk/(fs19b-1))**2)
                 if( (fs19b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs19b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs19b-1)
               if( ( newhmk * fs19(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs19b-1)) + abs(fs19(i,3))
               else if( ( newhmk * fs19(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs19b-1)) + abs(fs19(i,3))
               else
                 hmkresid = abs(newhmk/(fs19b-1)) - abs(fs19(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs19(i,4) .ne. -99.99) then
             if( ( fs19kzp * fs19(i,4)) .lt. 0) then
               kresid = abs(fs19kzp/fs19c) + abs(fs19(i,4))
             else if( ( fs19kzp * fs19(i,4)) .eq. 0) then
               kresid = abs(fs19kzp/fs19c) + abs(fs19(i,4))
             else
               kresid = abs(fs19kzp/fs19c) - abs(fs19(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs19kzp - fs19(i,4)
               kaway(k,3) = newk/(fs19c-1)
               fs19ksq = fs19ksq - (fs19(i,4))**2
               kvar = (fs19ksq - (fs19c-1) * (newk/(fs19c-1))**2)
                 if( (fs19c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs19c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs19c-1)
               if( ( newk * fs19(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs19c-1)) + abs(fs19(i,4))
               else if( ( newk * fs19(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs19c-1)) + abs(fs19(i,4))
               else
                 kresid = abs(newk/(fs19c-1)) - abs(fs19(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs19(i,1),
     :      '#',fs19(i,2),fs19(i,3),fs19(i,4), fs19(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs19(i,1),
     :           fs19(i,2),'$',fs19(i,3),fs19(i,4), fs19(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs19(i,1),
     :           fs19(i,2),fs19(i,3),'@',fs19(i,4), fs19(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs19(i,1),
     :           '#',fs19(i,2),'$',fs19(i,3),fs19(i,4), fs19(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs19(i,1),
     :           fs19(i,2),'$',fs19(i,3),'@',fs19(i,4), fs19(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs19(i,1),
     :           '#',fs19(i,2),fs19(i,3),'@',fs19(i,4), fs19(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs19(i,1),
     :           '#',fs19(i,2),'$',fs19(i,3),'@',fs19(i,4), fs19(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs19(i,1),
     :           fs19(i,2),fs19(i,3),fs19(i,4), fs19(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs19jmhzp/fs19a),
     :       (fs19hmkzp/fs19b), (fs19kzp/fs19c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs20hmksq - fs20b * (fs20hmkzp/fs20b)**2)
        if( fs20b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs20b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs20b)

        jmhvar = (fs20jmhsq - fs20a * (fs20jmhzp/fs20a)**2)
        if( fs20a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs20a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs20a)

        kvar = (fs20ksq - fs20c * (fs20kzp/fs20c)**2)
        if( fs20c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs20c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs20c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS20'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twenty
           if( fs20(i,2) .ne. -99.99) then
             if( ( fs20jmhzp * fs20(i,2)) .lt. 0) then
               jmhresid = abs(fs20jmhzp/fs20a) + abs(fs20(i,2))
             else if( ( fs20jmhzp * fs20(i,2)) .eq. 0) then
               jmhresid = abs(fs20jmhzp/fs20a) + abs(fs20(i,2))
             else
               jmhresid = abs(fs20jmhzp/fs20a) - abs(fs20(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs20jmhzp - fs20(i,2)
               jmhaway(p,3) = (newjmh/(fs20a-1))
               fs20jmhsq = fs20jmhsq - (fs20(i,2))**2
               jmhvar = (fs20jmhsq - (fs20a-1) * (newjmh/(fs20a-1))**2)
                 if( (fs20a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs20a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs20a-1)
               if( ( newjmh * fs20(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs20a-1)) + abs(fs20(i,2))
               else if( ( newjmh * fs20(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs20a-1)) + abs(fs20(i,2))
               else
                 jmhresid = abs(newjmh/(fs20a-1)) - abs(fs20(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs20(i,3) .ne. -99.99) then
             if( ( fs20hmkzp * fs20(i,3)) .lt. 0) then
               hmkresid = abs(fs20hmkzp/fs20b) + abs(fs20(i,3))
             else if( ( fs20hmkzp * fs20(i,3)) .eq. 0) then
               hmkresid = abs(fs20hmkzp/fs20b) + abs(fs20(i,3))
             else
               hmkresid = abs(fs20hmkzp/fs20b) - abs(fs20(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs20hmkzp - fs20(i,3)
               hmkaway(h,3) = newhmk/(fs20b-1)
               fs20hmksq = fs20hmksq - (fs20(i,3))**2
               hmkvar = (fs20hmksq - (fs20b-1) * (newhmk/(fs20b-1))**2)
                 if( (fs20b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs20b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs20b-1)
               if( ( newhmk * fs20(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs20b-1)) + abs(fs20(i,3))
               else if( ( newhmk * fs20(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs20b-1)) + abs(fs20(i,3))
               else
                 hmkresid = abs(newhmk/(fs20b-1)) - abs(fs20(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs20(i,4) .ne. -99.99) then
             if( ( fs20kzp * fs20(i,4)) .lt. 0) then
               kresid = abs(fs20kzp/fs20c) + abs(fs20(i,4))
             else if( ( fs20kzp * fs20(i,4)) .eq. 0) then
               kresid = abs(fs20kzp/fs20c) + abs(fs20(i,4))
             else
               kresid = abs(fs20kzp/fs20c) - abs(fs20(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs20kzp - fs20(i,4)
               kaway(k,3) = newk/(fs20c-1)
               fs20ksq = fs20ksq - (fs20(i,4))**2
               kvar = (fs20ksq - (fs20c-1) * (newk/(fs20c-1))**2)
                 if( (fs20c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs20c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs20c-1)
               if( ( newk * fs20(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs20c-1)) + abs(fs20(i,4))
               else if( ( newk * fs20(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs20c-1)) + abs(fs20(i,4))
               else
                 kresid = abs(newk/(fs20c-1)) - abs(fs20(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs20(i,1),
     :      '#',fs20(i,2),fs20(i,3),fs20(i,4), fs20(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs20(i,1),
     :           fs20(i,2),'$',fs20(i,3),fs20(i,4), fs20(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs20(i,1),
     :           fs20(i,2),fs20(i,3),'@',fs20(i,4), fs20(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs20(i,1),
     :           '#',fs20(i,2),'$',fs20(i,3),fs20(i,4), fs20(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs20(i,1),
     :           fs20(i,2),'$',fs20(i,3),'@',fs20(i,4), fs20(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs20(i,1),
     :           '#',fs20(i,2),fs20(i,3),'@',fs20(i,4), fs20(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs20(i,1),
     :           '#',fs20(i,2),'$',fs20(i,3),'@',fs20(i,4), fs20(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs20(i,1),
     :           fs20(i,2),fs20(i,3),fs20(i,4), fs20(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs20jmhzp/fs20a),
     :       (fs20hmkzp/fs20b), (fs20kzp/fs20c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs21hmksq - fs21b * (fs21hmkzp/fs21b)**2)
        if( fs21b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs21b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs21b)

        jmhvar = (fs21jmhsq - fs21a * (fs21jmhzp/fs21a)**2)
        if( fs21a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs21a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs21a)

        kvar = (fs21ksq - fs21c * (fs21kzp/fs21c)**2)
        if( fs21c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs21c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs21c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS21'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twone
           if( fs21(i,2) .ne. -99.99) then
             if( ( fs21jmhzp * fs21(i,2)) .lt. 0) then
               jmhresid = abs(fs21jmhzp/fs21a) + abs(fs21(i,2))
             else if( ( fs21jmhzp * fs21(i,2)) .eq. 0) then
               jmhresid = abs(fs21jmhzp/fs21a) + abs(fs21(i,2))
             else
               jmhresid = abs(fs21jmhzp/fs21a) - abs(fs21(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs21jmhzp - fs21(i,2)
               jmhaway(p,3) = (newjmh/(fs21a-1))
               fs21jmhsq = fs21jmhsq - (fs21(i,2))**2
               jmhvar = (fs21jmhsq - (fs21a-1) * (newjmh/(fs21a-1))**2)
                 if( (fs21a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs21a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs21a-1)
               if( ( newjmh * fs21(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs21a-1)) + abs(fs21(i,2))
               else if( ( newjmh * fs21(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs21a-1)) + abs(fs21(i,2))
               else
                 jmhresid = abs(newjmh/(fs21a-1)) - abs(fs21(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs21(i,3) .ne. -99.99) then
             if( ( fs21hmkzp * fs21(i,3)) .lt. 0) then
               hmkresid = abs(fs21hmkzp/fs21b) + abs(fs21(i,3))
             else if( ( fs21hmkzp * fs21(i,3)) .eq. 0) then
               hmkresid = abs(fs21hmkzp/fs21b) + abs(fs21(i,3))
             else
               hmkresid = abs(fs21hmkzp/fs21b) - abs(fs21(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs21hmkzp - fs21(i,3)
               hmkaway(h,3) = newhmk/(fs21b-1)
               fs21hmksq = fs21hmksq - (fs21(i,3))**2
               hmkvar = (fs21hmksq - (fs21b-1) * (newhmk/(fs21b-1))**2)
                 if( (fs21b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs21b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs21b-1)
               if( ( newhmk * fs21(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs21b-1)) + abs(fs21(i,3))
               else if( ( newhmk * fs21(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs21b-1)) + abs(fs21(i,3))
               else
                 hmkresid = abs(newhmk/(fs21b-1)) - abs(fs21(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs21(i,4) .ne. -99.99) then
             if( ( fs21kzp * fs21(i,4)) .lt. 0) then
               kresid = abs(fs21kzp/fs21c) + abs(fs21(i,4))
             else if( ( fs21kzp * fs21(i,4)) .eq. 0) then
               kresid = abs(fs21kzp/fs21c) + abs(fs21(i,4))
             else
               kresid = abs(fs21kzp/fs21c) - abs(fs21(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs21kzp - fs21(i,4)
               kaway(k,3) = newk/(fs21c-1)
               fs21ksq = fs21ksq - (fs21(i,4))**2
               kvar = (fs21ksq - (fs21c-1) * (newk/(fs21c-1))**2)
                 if( (fs21c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs21c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs21c-1)
               if( ( newk * fs21(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs21c-1)) + abs(fs21(i,4))
               else if( ( newk * fs21(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs21c-1)) + abs(fs21(i,4))
               else
                 kresid = abs(newk/(fs21c-1)) - abs(fs21(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs21(i,1),
     :      '#',fs21(i,2),fs21(i,3),fs21(i,4), fs21(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs21(i,1),
     :           fs21(i,2),'$',fs21(i,3),fs21(i,4), fs21(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs21(i,1),
     :           fs21(i,2),fs21(i,3),'@',fs21(i,4), fs21(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs21(i,1),
     :           '#',fs21(i,2),'$',fs21(i,3),fs21(i,4), fs21(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs21(i,1),
     :           fs21(i,2),'$',fs21(i,3),'@',fs21(i,4), fs21(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs21(i,1),
     :           '#',fs21(i,2),fs21(i,3),'@',fs21(i,4), fs21(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs21(i,1),
     :           '#',fs21(i,2),'$',fs21(i,3),'@',fs21(i,4), fs21(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs21(i,1),
     :           fs21(i,2),fs21(i,3),fs21(i,4), fs21(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs21jmhzp/fs21a),
     :       (fs21hmkzp/fs21b), (fs21kzp/fs21c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs23hmksq - fs23b * (fs23hmkzp/fs23b)**2)
        if( fs23b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs23b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs23b)

        jmhvar = (fs23jmhsq - fs23a * (fs23jmhzp/fs23a)**2)
        if( fs23a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs23a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs23a)

        kvar = (fs23ksq - fs23c * (fs23kzp/fs23c)**2)
        if( fs23c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs23c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs23c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS23'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twthree
           if( fs23(i,2) .ne. -99.99) then
             if( ( fs23jmhzp * fs23(i,2)) .lt. 0) then
               jmhresid = abs(fs23jmhzp/fs23a) + abs(fs23(i,2))
             else if( ( fs23jmhzp * fs23(i,2)) .eq. 0) then
               jmhresid = abs(fs23jmhzp/fs23a) + abs(fs23(i,2))
             else
               jmhresid = abs(fs23jmhzp/fs23a) - abs(fs23(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs23jmhzp - fs23(i,2)
               jmhaway(p,3) = (newjmh/(fs23a-1))
               fs23jmhsq = fs23jmhsq - (fs23(i,2))**2
               jmhvar = (fs23jmhsq - (fs23a-1) * (newjmh/(fs23a-1))**2)
                 if( (fs23a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs23a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs23a-1)
               if( ( newjmh * fs23(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs23a-1)) + abs(fs23(i,2))
               else if( ( newjmh * fs23(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs23a-1)) + abs(fs23(i,2))
               else
                 jmhresid = abs(newjmh/(fs23a-1)) - abs(fs23(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs23(i,3) .ne. -99.99) then
             if( ( fs23hmkzp * fs23(i,3)) .lt. 0) then
               hmkresid = abs(fs23hmkzp/fs23b) + abs(fs23(i,3))
             else if( ( fs23hmkzp * fs23(i,3)) .eq. 0) then
               hmkresid = abs(fs23hmkzp/fs23b) + abs(fs23(i,3))
             else
               hmkresid = abs(fs23hmkzp/fs23b) - abs(fs23(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs23hmkzp - fs23(i,3)
               hmkaway(h,3) = newhmk/(fs23b-1)
               fs23hmksq = fs23hmksq - (fs23(i,3))**2
               hmkvar = (fs23hmksq - (fs23b-1) * (newhmk/(fs23b-1))**2)
                 if( (fs23b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs23b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs23b-1)
               if( ( newhmk * fs23(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs23b-1)) + abs(fs23(i,3))
               else if( ( newhmk * fs23(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs23b-1)) + abs(fs23(i,3))
               else
                 hmkresid = abs(newhmk/(fs23b-1)) - abs(fs23(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs23(i,4) .ne. -99.99) then
             if( ( fs23kzp * fs23(i,4)) .lt. 0) then
               kresid = abs(fs23kzp/fs23c) + abs(fs23(i,4))
             else if( ( fs23kzp * fs23(i,4)) .eq. 0) then
               kresid = abs(fs23kzp/fs23c) + abs(fs23(i,4))
             else
               kresid = abs(fs23kzp/fs23c) - abs(fs23(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs23kzp - fs23(i,4)
               kaway(k,3) = newk/(fs23c-1)
               fs23ksq = fs23ksq - (fs23(i,4))**2
               kvar = (fs23ksq - (fs23c-1) * (newk/(fs23c-1))**2)
                 if( (fs23c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs23c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs23c-1)
               if( ( newk * fs23(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs23c-1)) + abs(fs23(i,4))
               else if( ( newk * fs23(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs23c-1)) + abs(fs23(i,4))
               else
                 kresid = abs(newk/(fs23c-1)) - abs(fs23(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs23(i,1),
     :      '#',fs23(i,2),fs23(i,3),fs23(i,4), fs23(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs23(i,1),
     :           fs23(i,2),'$',fs23(i,3),fs23(i,4), fs23(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs23(i,1),
     :           fs23(i,2),fs23(i,3),'@',fs23(i,4), fs23(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs23(i,1),
     :           '#',fs23(i,2),'$',fs23(i,3),fs23(i,4), fs23(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs23(i,1),
     :           fs23(i,2),'$',fs23(i,3),'@',fs23(i,4), fs23(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs23(i,1),
     :           '#',fs23(i,2),fs23(i,3),'@',fs23(i,4), fs23(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs23(i,1),
     :           '#',fs23(i,2),'$',fs23(i,3),'@',fs23(i,4), fs23(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs23(i,1),
     :           fs23(i,2),fs23(i,3),fs23(i,4), fs23(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs23jmhzp/fs23a),
     :       (fs23hmkzp/fs23b), (fs23kzp/fs23c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs24hmksq - fs24b * (fs24hmkzp/fs24b)**2)
        if( fs24b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs24b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs24b)

        jmhvar = (fs24jmhsq - fs24a * (fs24jmhzp/fs24a)**2)
        if( fs24a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs24a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs24a)

        kvar = (fs24ksq - fs24c * (fs24kzp/fs24c)**2)
        if( fs24c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs24c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs24c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS24'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twfour
           if( fs24(i,2) .ne. -99.99) then
             if( ( fs24jmhzp * fs24(i,2)) .lt. 0) then
               jmhresid = abs(fs24jmhzp/fs24a) + abs(fs24(i,2))
             else if( ( fs24jmhzp * fs24(i,2)) .eq. 0) then
               jmhresid = abs(fs24jmhzp/fs24a) + abs(fs24(i,2))
             else
               jmhresid = abs(fs24jmhzp/fs24a) - abs(fs24(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs24jmhzp - fs24(i,2)
               jmhaway(p,3) = (newjmh/(fs24a-1))
               fs24jmhsq = fs24jmhsq - (fs24(i,2))**2
               jmhvar = (fs24jmhsq - (fs24a-1) * (newjmh/(fs24a-1))**2)
                 if( (fs24a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs24a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs24a-1)
               if( ( newjmh * fs24(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs24a-1)) + abs(fs24(i,2))
               else if( ( newjmh * fs24(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs24a-1)) + abs(fs24(i,2))
               else
                 jmhresid = abs(newjmh/(fs24a-1)) - abs(fs24(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs24(i,3) .ne. -99.99) then
             if( ( fs24hmkzp * fs24(i,3)) .lt. 0) then
               hmkresid = abs(fs24hmkzp/fs24b) + abs(fs24(i,3))
             else if( ( fs24hmkzp * fs24(i,3)) .eq. 0) then
               hmkresid = abs(fs24hmkzp/fs24b) + abs(fs24(i,3))
             else
               hmkresid = abs(fs24hmkzp/fs24b) - abs(fs24(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs24hmkzp - fs24(i,3)
               hmkaway(h,3) = newhmk/(fs24b-1)
               fs24hmksq = fs24hmksq - (fs24(i,3))**2
               hmkvar = (fs24hmksq - (fs24b-1) * (newhmk/(fs24b-1))**2)
                 if( (fs24b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs24b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs24b-1)
               if( ( newhmk * fs24(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs24b-1)) + abs(fs24(i,3))
               else if( ( newhmk * fs24(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs24b-1)) + abs(fs24(i,3))
               else
                 hmkresid = abs(newhmk/(fs24b-1)) - abs(fs24(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs24(i,4) .ne. -99.99) then
             if( ( fs24kzp * fs24(i,4)) .lt. 0) then
               kresid = abs(fs24kzp/fs24c) + abs(fs24(i,4))
             else if( ( fs24kzp * fs24(i,4)) .eq. 0) then
               kresid = abs(fs24kzp/fs24c) + abs(fs24(i,4))
             else
               kresid = abs(fs24kzp/fs24c) - abs(fs24(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs24kzp - fs24(i,4)
               kaway(k,3) = newk/(fs24c-1)
               fs24ksq = fs24ksq - (fs24(i,4))**2
               kvar = (fs24ksq - (fs24c-1) * (newk/(fs24c-1))**2)
                 if( (fs24c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs24c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs24c-1)
               if( ( newk * fs24(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs24c-1)) + abs(fs24(i,4))
               else if( ( newk * fs24(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs24c-1)) + abs(fs24(i,4))
               else
                 kresid = abs(newk/(fs24c-1)) - abs(fs24(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs24(i,1),
     :      '#',fs24(i,2),fs24(i,3),fs24(i,4), fs24(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs24(i,1),
     :           fs24(i,2),'$',fs24(i,3),fs24(i,4), fs24(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs24(i,1),
     :           fs24(i,2),fs24(i,3),'@',fs24(i,4), fs24(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs24(i,1),
     :           '#',fs24(i,2),'$',fs24(i,3),fs24(i,4), fs24(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs24(i,1),
     :           fs24(i,2),'$',fs24(i,3),'@',fs24(i,4), fs24(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs24(i,1),
     :           '#',fs24(i,2),fs24(i,3),'@',fs24(i,4), fs24(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs24(i,1),
     :           '#',fs24(i,2),'$',fs24(i,3),'@',fs24(i,4), fs24(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs24(i,1),
     :           fs24(i,2),fs24(i,3),fs24(i,4), fs24(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs24jmhzp/fs24a),
     :       (fs24hmkzp/fs24b), (fs24kzp/fs24c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs25hmksq - fs25b * (fs25hmkzp/fs25b)**2)
        if( fs25b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs25b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs25b)

        jmhvar = (fs25jmhsq - fs25a * (fs25jmhzp/fs25a)**2)
        if( fs25a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs25a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs25a)

        kvar = (fs25ksq - fs25c * (fs25kzp/fs25c)**2)
        if( fs25c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs25c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs25c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS25'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twfive
           if( fs25(i,2) .ne. -99.99) then
             if( ( fs25jmhzp * fs25(i,2)) .lt. 0) then
               jmhresid = abs(fs25jmhzp/fs25a) + abs(fs25(i,2))
             else if( ( fs25jmhzp * fs25(i,2)) .eq. 0) then
               jmhresid = abs(fs25jmhzp/fs25a) + abs(fs25(i,2))
             else
               jmhresid = abs(fs25jmhzp/fs25a) - abs(fs25(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs25jmhzp - fs25(i,2)
               jmhaway(p,3) = (newjmh/(fs25a-1))
               fs25jmhsq = fs25jmhsq - (fs25(i,2))**2
               jmhvar = (fs25jmhsq - (fs25a-1) * (newjmh/(fs25a-1))**2)
                 if( (fs25a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs25a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs25a-1)
               if( ( newjmh * fs25(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs25a-1)) + abs(fs25(i,2))
               else if( ( newjmh * fs25(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs25a-1)) + abs(fs25(i,2))
               else
                 jmhresid = abs(newjmh/(fs25a-1)) - abs(fs25(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs25(i,3) .ne. -99.99) then
             if( ( fs25hmkzp * fs25(i,3)) .lt. 0) then
               hmkresid = abs(fs25hmkzp/fs25b) + abs(fs25(i,3))
             else if( ( fs25hmkzp * fs25(i,3)) .eq. 0) then
               hmkresid = abs(fs25hmkzp/fs25b) + abs(fs25(i,3))
             else
               hmkresid = abs(fs25hmkzp/fs25b) - abs(fs25(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs25hmkzp - fs25(i,3)
               hmkaway(h,3) = newhmk/(fs25b-1)
               fs25hmksq = fs25hmksq - (fs25(i,3))**2
               hmkvar = (fs25hmksq - (fs25b-1) * (newhmk/(fs25b-1))**2)
                 if( (fs25b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs25b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs25b-1)
               if( ( newhmk * fs25(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs25b-1)) + abs(fs25(i,3))
               else if( ( newhmk * fs25(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs25b-1)) + abs(fs25(i,3))
               else
                 hmkresid = abs(newhmk/(fs25b-1)) - abs(fs25(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs25(i,4) .ne. -99.99) then
             if( ( fs25kzp * fs25(i,4)) .lt. 0) then
               kresid = abs(fs25kzp/fs25c) + abs(fs25(i,4))
             else if( ( fs25kzp * fs25(i,4)) .eq. 0) then
               kresid = abs(fs25kzp/fs25c) + abs(fs25(i,4))
             else
               kresid = abs(fs25kzp/fs25c) - abs(fs25(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs25kzp - fs25(i,4)
               kaway(k,3) = newk/(fs25c-1)
               fs25ksq = fs25ksq - (fs25(i,4))**2
               kvar = (fs25ksq - (fs25c-1) * (newk/(fs25c-1))**2)
                 if( (fs25c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs25c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs25c-1)
               if( ( newk * fs25(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs25c-1)) + abs(fs25(i,4))
               else if( ( newk * fs25(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs25c-1)) + abs(fs25(i,4))
               else
                 kresid = abs(newk/(fs25c-1)) - abs(fs25(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs25(i,1),
     :      '#',fs25(i,2),fs25(i,3),fs25(i,4), fs25(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs25(i,1),
     :           fs25(i,2),'$',fs25(i,3),fs25(i,4), fs25(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs25(i,1),
     :           fs25(i,2),fs25(i,3),'@',fs25(i,4), fs25(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs25(i,1),
     :           '#',fs25(i,2),'$',fs25(i,3),fs25(i,4), fs25(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs25(i,1),
     :           fs25(i,2),'$',fs25(i,3),'@',fs25(i,4), fs25(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs25(i,1),
     :           '#',fs25(i,2),fs25(i,3),'@',fs25(i,4), fs25(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs25(i,1),
     :           '#',fs25(i,2),'$',fs25(i,3),'@',fs25(i,4), fs25(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs25(i,1),
     :           fs25(i,2),fs25(i,3),fs25(i,4), fs25(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs25jmhzp/fs25a),
     :       (fs25hmkzp/fs25b), (fs25kzp/fs25c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs27hmksq - fs27b * (fs27hmkzp/fs27b)**2)
        if( fs27b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs27b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs27b)

        jmhvar = (fs27jmhsq - fs27a * (fs27jmhzp/fs27a)**2)
        if( fs27a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs27a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs27a)

        kvar = (fs27ksq - fs27c * (fs27kzp/fs27c)**2)
        if( fs27c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs27c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs27c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS27'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twseven
           if( fs27(i,2) .ne. -99.99) then
             if( ( fs27jmhzp * fs27(i,2)) .lt. 0) then
               jmhresid = abs(fs27jmhzp/fs27a) + abs(fs27(i,2))
             else if( ( fs27jmhzp * fs27(i,2)) .eq. 0) then
               jmhresid = abs(fs27jmhzp/fs27a) + abs(fs27(i,2))
             else
               jmhresid = abs(fs27jmhzp/fs27a) - abs(fs27(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs27jmhzp - fs27(i,2)
               jmhaway(p,3) = (newjmh/(fs27a-1))
               fs27jmhsq = fs27jmhsq - (fs27(i,2))**2
               jmhvar = (fs27jmhsq - (fs27a-1) * (newjmh/(fs27a-1))**2)
                 if( (fs27a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs27a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs27a-1)
               if( ( newjmh * fs27(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs27a-1)) + abs(fs27(i,2))
               else if( ( newjmh * fs27(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs27a-1)) + abs(fs27(i,2))
               else
                 jmhresid = abs(newjmh/(fs27a-1)) - abs(fs27(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs27(i,3) .ne. -99.99) then
             if( ( fs27hmkzp * fs27(i,3)) .lt. 0) then
               hmkresid = abs(fs27hmkzp/fs27b) + abs(fs27(i,3))
             else if( ( fs27hmkzp * fs27(i,3)) .eq. 0) then
               hmkresid = abs(fs27hmkzp/fs27b) + abs(fs27(i,3))
             else
               hmkresid = abs(fs27hmkzp/fs27b) - abs(fs27(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs27hmkzp - fs27(i,3)
               hmkaway(h,3) = newhmk/(fs27b-1)
               fs27hmksq = fs27hmksq - (fs27(i,3))**2
               hmkvar = (fs27hmksq - (fs27b-1) * (newhmk/(fs27b-1))**2)
                 if( (fs27b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs27b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs27b-1)
               if( ( newhmk * fs27(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs27b-1)) + abs(fs27(i,3))
               else if( ( newhmk * fs27(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs27b-1)) + abs(fs27(i,3))
               else
                 hmkresid = abs(newhmk/(fs27b-1)) - abs(fs27(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs27(i,4) .ne. -99.99) then
             if( ( fs27kzp * fs27(i,4)) .lt. 0) then
               kresid = abs(fs27kzp/fs27c) + abs(fs27(i,4))
             else if( ( fs27kzp * fs27(i,4)) .eq. 0) then
               kresid = abs(fs27kzp/fs27c) + abs(fs27(i,4))
             else
               kresid = abs(fs27kzp/fs27c) - abs(fs27(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs27kzp - fs27(i,4)
               kaway(k,3) = newk/(fs27c-1)
               fs27ksq = fs27ksq - (fs27(i,4))**2
               kvar = (fs27ksq - (fs27c-1) * (newk/(fs27c-1))**2)
                 if( (fs27c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs27c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs27c-1)
               if( ( newk * fs27(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs27c-1)) + abs(fs27(i,4))
               else if( ( newk * fs27(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs27c-1)) + abs(fs27(i,4))
               else
                 kresid = abs(newk/(fs27c-1)) - abs(fs27(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs27(i,1),
     :      '#',fs27(i,2),fs27(i,3),fs27(i,4), fs27(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs27(i,1),
     :           fs27(i,2),'$',fs27(i,3),fs27(i,4), fs27(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs27(i,1),
     :           fs27(i,2),fs27(i,3),'@',fs27(i,4), fs27(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs27(i,1),
     :           '#',fs27(i,2),'$',fs27(i,3),fs27(i,4), fs27(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs27(i,1),
     :           fs27(i,2),'$',fs27(i,3),'@',fs27(i,4), fs27(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs27(i,1),
     :           '#',fs27(i,2),fs27(i,3),'@',fs27(i,4), fs27(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs27(i,1),
     :           '#',fs27(i,2),'$',fs27(i,3),'@',fs27(i,4), fs27(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs27(i,1),
     :           fs27(i,2),fs27(i,3),fs27(i,4), fs27(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs27jmhzp/fs27a),
     :       (fs27hmkzp/fs27b), (fs27kzp/fs27c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs28hmksq - fs28b * (fs28hmkzp/fs28b)**2)
        if( fs28b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs28b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs28b)

        jmhvar = (fs28jmhsq - fs28a * (fs28jmhzp/fs28a)**2)
        if( fs28a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs28a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs28a)

        kvar = (fs28ksq - fs28c * (fs28kzp/fs28c)**2)
        if( fs28c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs28c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs28c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS28'
        write(143, '(a)') '                J-H         H-K         K      airmass'

        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, tweight
           if( fs28(i,2) .ne. -99.99) then
             if( ( fs28jmhzp * fs28(i,2)) .lt. 0) then
               jmhresid = abs(fs28jmhzp/fs28a) + abs(fs28(i,2))
             else if( ( fs28jmhzp * fs28(i,2)) .eq. 0) then
               jmhresid = abs(fs28jmhzp/fs28a) + abs(fs28(i,2))
             else
               jmhresid = abs(fs28jmhzp/fs28a) - abs(fs28(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs28jmhzp - fs28(i,2)
               jmhaway(p,3) = (newjmh/(fs28a-1))
               fs28jmhsq = fs28jmhsq - (fs28(i,2))**2
               jmhvar = (fs28jmhsq - (fs28a-1) * (newjmh/(fs28a-1))**2)
                 if( (fs28a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs28a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs28a-1)
               if( ( newjmh * fs28(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs28a-1)) + abs(fs28(i,2))
               else if( ( newjmh * fs28(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs28a-1)) + abs(fs28(i,2))
               else
                 jmhresid = abs(newjmh/(fs28a-1)) - abs(fs28(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs28(i,3) .ne. -99.99) then
             if( ( fs28hmkzp * fs28(i,3)) .lt. 0) then
               hmkresid = abs(fs28hmkzp/fs28b) + abs(fs28(i,3))
             else if( ( fs28hmkzp * fs28(i,3)) .eq. 0) then
               hmkresid = abs(fs28hmkzp/fs28b) + abs(fs28(i,3))
             else
               hmkresid = abs(fs28hmkzp/fs28b) - abs(fs28(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs28hmkzp - fs28(i,3)
               hmkaway(h,3) = newhmk/(fs28b-1)
               fs28hmksq = fs28hmksq - (fs28(i,3))**2
               hmkvar = (fs28hmksq - (fs28b-1) * (newhmk/(fs28b-1))**2)
                 if( (fs28b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs28b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs28b-1)
               if( ( newhmk * fs28(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs28b-1)) + abs(fs28(i,3))
               else if( ( newhmk * fs28(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs28b-1)) + abs(fs28(i,3))
               else
                 hmkresid = abs(newhmk/(fs28b-1)) - abs(fs28(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs28(i,4) .ne. -99.99) then
             if( ( fs28kzp * fs28(i,4)) .lt. 0) then
               kresid = abs(fs28kzp/fs28c) + abs(fs28(i,4))
             else if( ( fs28kzp * fs28(i,4)) .eq. 0) then
               kresid = abs(fs28kzp/fs28c) + abs(fs28(i,4))
             else
               kresid = abs(fs28kzp/fs28c) - abs(fs28(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs28kzp - fs28(i,4)
               kaway(k,3) = newk/(fs28c-1)
               fs28ksq = fs28ksq - (fs28(i,4))**2
               kvar = (fs28ksq - (fs28c-1) * (newk/(fs28c-1))**2)
                 if( (fs28c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs28c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs28c-1)
               if( ( newk * fs28(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs28c-1)) + abs(fs28(i,4))
               else if( ( newk * fs28(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs28c-1)) + abs(fs28(i,4))
               else
                 kresid = abs(newk/(fs28c-1)) - abs(fs28(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs28(i,1),
     :      '#',fs28(i,2),fs28(i,3),fs28(i,4), fs28(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs28(i,1),
     :           fs28(i,2),'$',fs28(i,3),fs28(i,4), fs28(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs28(i,1),
     :           fs28(i,2),fs28(i,3),'@',fs28(i,4), fs28(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs28(i,1),
     :           '#',fs28(i,2),'$',fs28(i,3),fs28(i,4), fs28(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs28(i,1),
     :           fs28(i,2),'$',fs28(i,3),'@',fs28(i,4), fs28(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs28(i,1),
     :           '#',fs28(i,2),fs28(i,3),'@',fs28(i,4), fs28(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs28(i,1),
     :           '#',fs28(i,2),'$',fs28(i,3),'@',fs28(i,4), fs28(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs28(i,1),
     :           fs28(i,2),fs28(i,3),fs28(i,4), fs28(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs28jmhzp/fs28a),
     :       (fs28hmkzp/fs28b), (fs28kzp/fs28c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs29hmksq - fs29b * (fs29hmkzp/fs29b)**2)
        if( fs29b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs29b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs29b)

        jmhvar = (fs29jmhsq - fs29a * (fs29jmhzp/fs29a)**2)
        if( fs29a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs29a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs29a)

        kvar = (fs29ksq - fs29c * (fs29kzp/fs29c)**2)
        if( fs29c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs29c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs29c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS29'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, twnine
           if( fs29(i,2) .ne. -99.99) then
             if( ( fs29jmhzp * fs29(i,2)) .lt. 0) then
               jmhresid = abs(fs29jmhzp/fs29a) + abs(fs29(i,2))
             else if( ( fs29jmhzp * fs29(i,2)) .eq. 0) then
               jmhresid = abs(fs29jmhzp/fs29a) + abs(fs29(i,2))
             else
               jmhresid = abs(fs29jmhzp/fs29a) - abs(fs29(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs29jmhzp - fs29(i,2)
               jmhaway(p,3) = (newjmh/(fs29a-1))
               fs29jmhsq = fs29jmhsq - (fs29(i,2))**2
               jmhvar = (fs29jmhsq - (fs29a-1) * (newjmh/(fs29a-1))**2)
                 if( (fs29a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs29a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs29a-1)
               if( ( newjmh * fs29(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs29a-1)) + abs(fs29(i,2))
               else if( ( newjmh * fs29(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs29a-1)) + abs(fs29(i,2))
               else
                 jmhresid = abs(newjmh/(fs29a-1)) - abs(fs29(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs29(i,3) .ne. -99.99) then
             if( ( fs29hmkzp * fs29(i,3)) .lt. 0) then
               hmkresid = abs(fs29hmkzp/fs29b) + abs(fs29(i,3))
             else if( ( fs29hmkzp * fs29(i,3)) .eq. 0) then
               hmkresid = abs(fs29hmkzp/fs29b) + abs(fs29(i,3))
             else
               hmkresid = abs(fs29hmkzp/fs29b) - abs(fs29(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs29hmkzp - fs29(i,3)
               hmkaway(h,3) = newhmk/(fs29b-1)
               fs29hmksq = fs29hmksq - (fs29(i,3))**2
               hmkvar = (fs29hmksq - (fs29b-1) * (newhmk/(fs29b-1))**2)
                 if( (fs29b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs29b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs29b-1)
               if( ( newhmk * fs29(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs29b-1)) + abs(fs29(i,3))
               else if( ( newhmk * fs29(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs29b-1)) + abs(fs29(i,3))
               else
                 hmkresid = abs(newhmk/(fs29b-1)) - abs(fs29(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs29(i,4) .ne. -99.99) then
             if( ( fs29kzp * fs29(i,4)) .lt. 0) then
               kresid = abs(fs29kzp/fs29c) + abs(fs29(i,4))
             else if( ( fs29kzp * fs29(i,4)) .eq. 0) then
               kresid = abs(fs29kzp/fs29c) + abs(fs29(i,4))
             else
               kresid = abs(fs29kzp/fs29c) - abs(fs29(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs29kzp - fs29(i,4)
               kaway(k,3) = newk/(fs29c-1)
               fs29ksq = fs29ksq - (fs29(i,4))**2
               kvar = (fs29ksq - (fs29c-1) * (newk/(fs29c-1))**2)
                 if( (fs29c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs29c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs29c-1)
               if( ( newk * fs29(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs29c-1)) + abs(fs29(i,4))
               else if( ( newk * fs29(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs29c-1)) + abs(fs29(i,4))
               else
                 kresid = abs(newk/(fs29c-1)) - abs(fs29(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs29(i,1),
     :      '#',fs29(i,2),fs29(i,3),fs29(i,4), fs29(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs29(i,1),
     :           fs29(i,2),'$',fs29(i,3),fs29(i,4), fs29(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs29(i,1),
     :           fs29(i,2),fs29(i,3),'@',fs29(i,4), fs29(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs29(i,1),
     :           '#',fs29(i,2),'$',fs29(i,3),fs29(i,4), fs29(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs29(i,1),
     :           fs29(i,2),'$',fs29(i,3),'@',fs29(i,4), fs29(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs29(i,1),
     :           '#',fs29(i,2),fs29(i,3),'@',fs29(i,4), fs29(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs29(i,1),
     :           '#',fs29(i,2),'$',fs29(i,3),'@',fs29(i,4), fs29(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs29(i,1),
     :           fs29(i,2),fs29(i,3),fs29(i,4), fs29(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs29jmhzp/fs29a),
     :       (fs29hmkzp/fs29b), (fs29kzp/fs29c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs30hmksq - fs30b * (fs30hmkzp/fs30b)**2)
        if( fs30b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs30b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs30b)

        jmhvar = (fs30jmhsq - fs30a * (fs30jmhzp/fs30a)**2)
        if( fs30a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs30a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs30a)

        kvar = (fs30ksq - fs30c * (fs30kzp/fs30c)**2)
        if( fs30c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs30c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs30c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS30'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, thirty
           if( fs30(i,2) .ne. -99.99) then
             if( ( fs30jmhzp * fs30(i,2)) .lt. 0) then
               jmhresid = abs(fs30jmhzp/fs30a) + abs(fs30(i,2))
             else if( ( fs30jmhzp * fs30(i,2)) .eq. 0) then
               jmhresid = abs(fs30jmhzp/fs30a) + abs(fs30(i,2))
             else
               jmhresid = abs(fs30jmhzp/fs30a) - abs(fs30(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs30jmhzp - fs30(i,2)
               jmhaway(p,3) = (newjmh/(fs30a-1))
               fs30jmhsq = fs30jmhsq - (fs30(i,2))**2
               jmhvar = (fs30jmhsq - (fs30a-1) * (newjmh/(fs30a-1))**2)
                 if( (fs30a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs30a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs30a-1)
               if( ( newjmh * fs30(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs30a-1)) + abs(fs30(i,2))
               else if( ( newjmh * fs30(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs30a-1)) + abs(fs30(i,2))
               else
                 jmhresid = abs(newjmh/(fs30a-1)) - abs(fs30(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs30(i,3) .ne. -99.99) then
             if( ( fs30hmkzp * fs30(i,3)) .lt. 0) then
               hmkresid = abs(fs30hmkzp/fs30b) + abs(fs30(i,3))
             else if( ( fs30hmkzp * fs30(i,3)) .eq. 0) then
               hmkresid = abs(fs30hmkzp/fs30b) + abs(fs30(i,3))
             else
               hmkresid = abs(fs30hmkzp/fs30b) - abs(fs30(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs30hmkzp - fs30(i,3)
               hmkaway(h,3) = newhmk/(fs30b-1)
               fs30hmksq = fs30hmksq - (fs30(i,3))**2
               hmkvar = (fs30hmksq - (fs30b-1) * (newhmk/(fs30b-1))**2)
                 if( (fs30b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs30b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs30b-1)
               if( ( newhmk * fs30(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs30b-1)) + abs(fs30(i,3))
               else if( ( newhmk * fs30(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs30b-1)) + abs(fs30(i,3))
               else
                 hmkresid = abs(newhmk/(fs30b-1)) - abs(fs30(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs30(i,4) .ne. -99.99) then
             if( ( fs30kzp * fs30(i,4)) .lt. 0) then
               kresid = abs(fs30kzp/fs30c) + abs(fs30(i,4))
             else if( ( fs30kzp * fs30(i,4)) .eq. 0) then
               kresid = abs(fs30kzp/fs30c) + abs(fs30(i,4))
             else
               kresid = abs(fs30kzp/fs30c) - abs(fs30(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs30kzp - fs30(i,4)
               kaway(k,3) = newk/(fs30c-1)
               fs30ksq = fs30ksq - (fs30(i,4))**2
               kvar = (fs30ksq - (fs30c-1) * (newk/(fs30c-1))**2)
                 if( (fs30c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs30c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs30c-1)
               if( ( newk * fs30(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs30c-1)) + abs(fs30(i,4))
               else if( ( newk * fs30(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs30c-1)) + abs(fs30(i,4))
               else
                 kresid = abs(newk/(fs30c-1)) - abs(fs30(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs30(i,1),
     :      '#',fs30(i,2),fs30(i,3),fs30(i,4), fs30(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs30(i,1),
     :           fs30(i,2),'$',fs30(i,3),fs30(i,4), fs30(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs30(i,1),
     :           fs30(i,2),fs30(i,3),'@',fs30(i,4), fs30(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs30(i,1),
     :           '#',fs30(i,2),'$',fs30(i,3),fs30(i,4), fs30(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs30(i,1),
     :           fs30(i,2),'$',fs30(i,3),'@',fs30(i,4), fs30(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs30(i,1),
     :           '#',fs30(i,2),fs30(i,3),'@',fs30(i,4), fs30(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs30(i,1),
     :           '#',fs30(i,2),'$',fs30(i,3),'@',fs30(i,4), fs30(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs30(i,1),
     :           fs30(i,2),fs30(i,3),fs30(i,4), fs30(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs30jmhzp/fs30a),
     :       (fs30hmkzp/fs30b), (fs30kzp/fs30c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs31hmksq - fs31b * (fs31hmkzp/fs31b)**2)
        if( fs31b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs31b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs31b)

        jmhvar = (fs31jmhsq - fs31a * (fs31jmhzp/fs31a)**2)
        if( fs31a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs31a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs31a)

        kvar = (fs31ksq - fs31c * (fs31kzp/fs31c)**2)
        if( fs31c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs31c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs31c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS31'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, thone
           if( fs31(i,2) .ne. -99.99) then
             if( ( fs31jmhzp * fs31(i,2)) .lt. 0) then
               jmhresid = abs(fs31jmhzp/fs31a) + abs(fs31(i,2))
             else if( ( fs31jmhzp * fs31(i,2)) .eq. 0) then
               jmhresid = abs(fs31jmhzp/fs31a) + abs(fs31(i,2))
             else
               jmhresid = abs(fs31jmhzp/fs31a) - abs(fs31(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs31jmhzp - fs31(i,2)
               jmhaway(p,3) = (newjmh/(fs31a-1))
               fs31jmhsq = fs31jmhsq - (fs31(i,2))**2
               jmhvar = (fs31jmhsq - (fs31a-1) * (newjmh/(fs31a-1))**2)
                 if( (fs31a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs31a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs31a-1)
               if( ( newjmh * fs31(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs31a-1)) + abs(fs31(i,2))
               else if( ( newjmh * fs31(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs31a-1)) + abs(fs31(i,2))
               else
                 jmhresid = abs(newjmh/(fs31a-1)) - abs(fs31(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs31(i,3) .ne. -99.99) then
             if( ( fs31hmkzp * fs31(i,3)) .lt. 0) then
               hmkresid = abs(fs31hmkzp/fs31b) + abs(fs31(i,3))
             else if( ( fs31hmkzp * fs31(i,3)) .eq. 0) then
               hmkresid = abs(fs31hmkzp/fs31b) + abs(fs31(i,3))
             else
               hmkresid = abs(fs31hmkzp/fs31b) - abs(fs31(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs31hmkzp - fs31(i,3)
               hmkaway(h,3) = newhmk/(fs31b-1)
               fs31hmksq = fs31hmksq - (fs31(i,3))**2
               hmkvar = (fs31hmksq - (fs31b-1) * (newhmk/(fs31b-1))**2)
                 if( (fs31b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs31b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs31b-1)
               if( ( newhmk * fs31(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs31b-1)) + abs(fs31(i,3))
               else if( ( newhmk * fs31(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs31b-1)) + abs(fs31(i,3))
               else
                 hmkresid = abs(newhmk/(fs31b-1)) - abs(fs31(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs31(i,4) .ne. -99.99) then
             if( ( fs31kzp * fs31(i,4)) .lt. 0) then
               kresid = abs(fs31kzp/fs31c) + abs(fs31(i,4))
             else if( ( fs31kzp * fs31(i,4)) .eq. 0) then
               kresid = abs(fs31kzp/fs31c) + abs(fs31(i,4))
             else
               kresid = abs(fs31kzp/fs31c) - abs(fs31(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs31kzp - fs31(i,4)
               kaway(k,3) = newk/(fs31c-1)
               fs31ksq = fs31ksq - (fs31(i,4))**2
               kvar = (fs31ksq - (fs31c-1) * (newk/(fs31c-1))**2)
                 if( (fs31c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs31c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs31c-1)
               if( ( newk * fs31(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs31c-1)) + abs(fs31(i,4))
               else if( ( newk * fs31(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs31c-1)) + abs(fs31(i,4))
               else
                 kresid = abs(newk/(fs31c-1)) - abs(fs31(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs31(i,1),
     :      '#',fs31(i,2),fs31(i,3),fs31(i,4), fs31(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs31(i,1),
     :           fs31(i,2),'$',fs31(i,3),fs31(i,4), fs31(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs31(i,1),
     :           fs31(i,2),fs31(i,3),'@',fs31(i,4), fs31(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs31(i,1),
     :           '#',fs31(i,2),'$',fs31(i,3),fs31(i,4), fs31(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs31(i,1),
     :           fs31(i,2),'$',fs31(i,3),'@',fs31(i,4), fs31(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs31(i,1),
     :           '#',fs31(i,2),fs31(i,3),'@',fs31(i,4), fs31(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs31(i,1),
     :           '#',fs31(i,2),'$',fs31(i,3),'@',fs31(i,4), fs31(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs31(i,1),
     :           fs31(i,2),fs31(i,3),fs31(i,4), fs31(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs31jmhzp/fs31a),
     :       (fs31hmkzp/fs31b), (fs31kzp/fs31c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs32hmksq - fs32b * (fs32hmkzp/fs32b)**2)
        if( fs32b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs32b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs32b)

        jmhvar = (fs32jmhsq - fs32a * (fs32jmhzp/fs32a)**2)
        if( fs32a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs32a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs32a)

        kvar = (fs32ksq - fs32c * (fs32kzp/fs32c)**2)
        if( fs32c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs32c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs32c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS32'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, thtwo
           if( fs32(i,2) .ne. -99.99) then
             if( ( fs32jmhzp * fs32(i,2)) .lt. 0) then
               jmhresid = abs(fs32jmhzp/fs32a) + abs(fs32(i,2))
             else if( ( fs32jmhzp * fs32(i,2)) .eq. 0) then
               jmhresid = abs(fs32jmhzp/fs32a) + abs(fs32(i,2))
             else
               jmhresid = abs(fs32jmhzp/fs32a) - abs(fs32(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = fs32jmhzp - fs32(i,2)
               jmhaway(p,3) = (newjmh/(fs32a-1))
               fs32jmhsq = fs32jmhsq - (fs32(i,2))**2
               jmhvar = (fs32jmhsq - (fs32a-1) * (newjmh/(fs32a-1))**2)
                 if( (fs32a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((fs32a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(fs32a-1)
               if( ( newjmh * fs32(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(fs32a-1)) + abs(fs32(i,2))
               else if( ( newjmh * fs32(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(fs32a-1)) + abs(fs32(i,2))
               else
                 jmhresid = abs(newjmh/(fs32a-1)) - abs(fs32(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( fs32(i,3) .ne. -99.99) then
             if( ( fs32hmkzp * fs32(i,3)) .lt. 0) then
               hmkresid = abs(fs32hmkzp/fs32b) + abs(fs32(i,3))
             else if( ( fs32hmkzp * fs32(i,3)) .eq. 0) then
               hmkresid = abs(fs32hmkzp/fs32b) + abs(fs32(i,3))
             else
               hmkresid = abs(fs32hmkzp/fs32b) - abs(fs32(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = fs32hmkzp - fs32(i,3)
               hmkaway(h,3) = newhmk/(fs32b-1)
               fs32hmksq = fs32hmksq - (fs32(i,3))**2
               hmkvar = (fs32hmksq - (fs32b-1) * (newhmk/(fs32b-1))**2)
                 if( (fs32b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((fs32b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(fs32b-1)
               if( ( newhmk * fs32(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(fs32b-1)) + abs(fs32(i,3))
               else if( ( newhmk * fs32(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(fs32b-1)) + abs(fs32(i,3))
               else
                 hmkresid = abs(newhmk/(fs32b-1)) - abs(fs32(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( fs32(i,4) .ne. -99.99) then
             if( ( fs32kzp * fs32(i,4)) .lt. 0) then
               kresid = abs(fs32kzp/fs32c) + abs(fs32(i,4))
             else if( ( fs32kzp * fs32(i,4)) .eq. 0) then
               kresid = abs(fs32kzp/fs32c) + abs(fs32(i,4))
             else
               kresid = abs(fs32kzp/fs32c) - abs(fs32(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = fs32kzp - fs32(i,4)
               kaway(k,3) = newk/(fs32c-1)
               fs32ksq = fs32ksq - (fs32(i,4))**2
               kvar = (fs32ksq - (fs32c-1) * (newk/(fs32c-1))**2)
                 if( (fs32c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((fs32c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(fs32c-1)
               if( ( newk * fs32(i,4)) .lt. 0) then
                 kresid = abs(newk/(fs32c-1)) + abs(fs32(i,4))
               else if( ( newk * fs32(i,4)) .eq. 0) then
                 kresid = abs(newk/(fs32c-1)) + abs(fs32(i,4))
               else
                 kresid = abs(newk/(fs32c-1)) - abs(fs32(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs32(i,1),
     :      '#',fs32(i,2),fs32(i,3),fs32(i,4), fs32(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs32(i,1),
     :           fs32(i,2),'$',fs32(i,3),fs32(i,4), fs32(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs32(i,1),
     :           fs32(i,2),fs32(i,3),'@',fs32(i,4), fs32(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') fs32(i,1),
     :           '#',fs32(i,2),'$',fs32(i,3),fs32(i,4), fs32(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs32(i,1),
     :           fs32(i,2),'$',fs32(i,3),'@',fs32(i,4), fs32(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') fs32(i,1),
     :           '#',fs32(i,2),fs32(i,3),'@',fs32(i,4), fs32(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') fs32(i,1),
     :           '#',fs32(i,2),'$',fs32(i,3),'@',fs32(i,4), fs32(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') fs32(i,1),
     :           fs32(i,2),fs32(i,3),fs32(i,4), fs32(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (fs32jmhzp/fs32a),
     :       (fs32hmkzp/fs32b), (fs32kzp/fs32c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs33hmksq - fs33b * (fs33hmkzp/fs33b)**2)
        if( fs33b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs33b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs33b)

        jmhvar = (fs33jmhsq - fs33a * (fs33jmhzp/fs33a)**2)
        if( fs33a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs33a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs33a)

        kvar = (fs33ksq - fs33c * (fs33kzp/fs33c)**2)
        if( fs33c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs33c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs33c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS33'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, ththree
           if( fs33(i,2) .ne. -99.99) then
             if( ( FS33jmhzp * FS33(i,2)) .lt. 0) then
               jmhresid = abs(FS33jmhzp/FS33a) + abs(FS33(i,2))
             else if( ( FS33jmhzp * FS33(i,2)) .eq. 0) then
               jmhresid = abs(FS33jmhzp/FS33a) + abs(FS33(i,2))
             else
               jmhresid = abs(FS33jmhzp/FS33a) - abs(FS33(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = FS33jmhzp - FS33(i,2)
               jmhaway(p,3) = (newjmh/(FS33a-1))
               FS33jmhsq = FS33jmhsq - (FS33(i,2))**2
               jmhvar = (FS33jmhsq - (FS33a-1) * (newjmh/(FS33a-1))**2)
                 if( (FS33a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((FS33a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(FS33a-1)
               if( ( newjmh * FS33(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(FS33a-1)) + abs(FS33(i,2))
               else if( ( newjmh * FS33(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(FS33a-1)) + abs(FS33(i,2))
               else
                 jmhresid = abs(newjmh/(FS33a-1)) - abs(FS33(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( FS33(i,3) .ne. -99.99) then
             if( ( FS33hmkzp * FS33(i,3)) .lt. 0) then
               hmkresid = abs(FS33hmkzp/FS33b) + abs(FS33(i,3))
             else if( ( FS33hmkzp * FS33(i,3)) .eq. 0) then
               hmkresid = abs(FS33hmkzp/FS33b) + abs(FS33(i,3))
             else
               hmkresid = abs(FS33hmkzp/FS33b) - abs(FS33(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = FS33hmkzp - FS33(i,3)
               hmkaway(h,3) = newhmk/(FS33b-1)
               FS33hmksq = FS33hmksq - (FS33(i,3))**2
               hmkvar = (FS33hmksq - (FS33b-1) * (newhmk/(FS33b-1))**2)
                 if( (FS33b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((FS33b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(FS33b-1)
               if( ( newhmk * FS33(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(FS33b-1)) + abs(FS33(i,3))
               else if( ( newhmk * FS33(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(FS33b-1)) + abs(FS33(i,3))
               else
                 hmkresid = abs(newhmk/(FS33b-1)) - abs(FS33(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( FS33(i,4) .ne. -99.99) then
             if( ( FS33kzp * FS33(i,4)) .lt. 0) then
               kresid = abs(FS33kzp/FS33c) + abs(FS33(i,4))
             else if( ( FS33kzp * FS33(i,4)) .eq. 0) then
               kresid = abs(FS33kzp/FS33c) + abs(FS33(i,4))
             else
               kresid = abs(FS33kzp/FS33c) - abs(FS33(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = FS33kzp - FS33(i,4)
               kaway(k,3) = newk/(FS33c-1)
               FS33ksq = FS33ksq - (FS33(i,4))**2
               kvar = (FS33ksq - (FS33c-1) * (newk/(FS33c-1))**2)
                 if( (FS33c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((FS33c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(FS33c-1)
               if( ( newk * FS33(i,4)) .lt. 0) then
                 kresid = abs(newk/(FS33c-1)) + abs(FS33(i,4))
               else if( ( newk * FS33(i,4)) .eq. 0) then
                 kresid = abs(newk/(FS33c-1)) + abs(FS33(i,4))
               else
                 kresid = abs(newk/(FS33c-1)) - abs(FS33(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') FS33(i,1),
     :      '#',FS33(i,2),FS33(i,3),FS33(i,4), FS33(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') FS33(i,1),
     :           FS33(i,2),'$',FS33(i,3),FS33(i,4), FS33(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') FS33(i,1),
     :           FS33(i,2),FS33(i,3),'@',FS33(i,4), FS33(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') FS33(i,1),
     :           '#',FS33(i,2),'$',FS33(i,3),FS33(i,4), FS33(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') FS33(i,1),
     :           FS33(i,2),'$',FS33(i,3),'@',FS33(i,4), FS33(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') FS33(i,1),
     :           '#',FS33(i,2),FS33(i,3),'@',FS33(i,4), FS33(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') FS33(i,1),
     :           '#',FS33(i,2),'$',FS33(i,3),'@',FS33(i,4), FS33(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') FS33(i,1),
     :           FS33(i,2),FS33(i,3),FS33(i,4), FS33(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (FS33jmhzp/FS33a),
     :       (FS33hmkzp/FS33b), (FS33kzp/FS33c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs34hmksq - fs34b * (fs34hmkzp/fs34b)**2)
        if( fs34b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs34b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs34b)

        jmhvar = (fs34jmhsq - fs34a * (fs34jmhzp/fs34a)**2)
        if( fs34a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs34a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs34a)

        kvar = (fs34ksq - fs34c * (fs34kzp/fs34c)**2)
        if( fs34c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs34c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs34c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS34'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, thfour
           if( fs34(i,2) .ne. -99.99) then
             if( ( FS34jmhzp * FS34(i,2)) .lt. 0) then
               jmhresid = abs(FS34jmhzp/FS34a) + abs(FS34(i,2))
             else if( ( FS34jmhzp * FS34(i,2)) .eq. 0) then
               jmhresid = abs(FS34jmhzp/FS34a) + abs(FS34(i,2))
             else
               jmhresid = abs(FS34jmhzp/FS34a) - abs(FS34(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = FS34jmhzp - FS34(i,2)
               jmhaway(p,3) = (newjmh/(FS34a-1))
               FS34jmhsq = FS34jmhsq - (FS34(i,2))**2
               jmhvar = (FS34jmhsq - (FS34a-1) * (newjmh/(FS34a-1))**2)
                 if( (FS34a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((FS34a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(FS34a-1)
               if( ( newjmh * FS34(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(FS34a-1)) + abs(FS34(i,2))
               else if( ( newjmh * FS34(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(FS34a-1)) + abs(FS34(i,2))
               else
                 jmhresid = abs(newjmh/(FS34a-1)) - abs(FS34(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( FS34(i,3) .ne. -99.99) then
             if( ( FS34hmkzp * FS34(i,3)) .lt. 0) then
               hmkresid = abs(FS34hmkzp/FS34b) + abs(FS34(i,3))
             else if( ( FS34hmkzp * FS34(i,3)) .eq. 0) then
               hmkresid = abs(FS34hmkzp/FS34b) + abs(FS34(i,3))
             else
               hmkresid = abs(FS34hmkzp/FS34b) - abs(FS34(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = FS34hmkzp - FS34(i,3)
               hmkaway(h,3) = newhmk/(FS34b-1)
               FS34hmksq = FS34hmksq - (FS34(i,3))**2
               hmkvar = (FS34hmksq - (FS34b-1) * (newhmk/(FS34b-1))**2)
                 if( (FS34b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((FS34b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(FS34b-1)
               if( ( newhmk * FS34(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(FS34b-1)) + abs(FS34(i,3))
               else if( ( newhmk * FS34(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(FS34b-1)) + abs(FS34(i,3))
               else
                 hmkresid = abs(newhmk/(FS34b-1)) - abs(FS34(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( FS34(i,4) .ne. -99.99) then
             if( ( FS34kzp * FS34(i,4)) .lt. 0) then
               kresid = abs(FS34kzp/FS34c) + abs(FS34(i,4))
             else if( ( FS34kzp * FS34(i,4)) .eq. 0) then
               kresid = abs(FS34kzp/FS34c) + abs(FS34(i,4))
             else
               kresid = abs(FS34kzp/FS34c) - abs(FS34(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = FS34kzp - FS34(i,4)
               kaway(k,3) = newk/(FS34c-1)
               FS34ksq = FS34ksq - (FS34(i,4))**2
               kvar = (FS34ksq - (FS34c-1) * (newk/(FS34c-1))**2)
                 if( (FS34c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((FS34c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(FS34c-1)
               if( ( newk * FS34(i,4)) .lt. 0) then
                 kresid = abs(newk/(FS34c-1)) + abs(FS34(i,4))
               else if( ( newk * FS34(i,4)) .eq. 0) then
                 kresid = abs(newk/(FS34c-1)) + abs(FS34(i,4))
               else
                 kresid = abs(newk/(FS34c-1)) - abs(FS34(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') FS34(i,1),
     :      '#',FS34(i,2),FS34(i,3),FS34(i,4), FS34(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') FS34(i,1),
     :           FS34(i,2),'$',FS34(i,3),FS34(i,4), FS34(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') FS34(i,1),
     :           FS34(i,2),FS34(i,3),'@',FS34(i,4), FS34(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') FS34(i,1),
     :           '#',FS34(i,2),'$',FS34(i,3),FS34(i,4), FS34(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') FS34(i,1),
     :           FS34(i,2),'$',FS34(i,3),'@',FS34(i,4), FS34(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') FS34(i,1),
     :           '#',FS34(i,2),FS34(i,3),'@',FS34(i,4), FS34(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') FS34(i,1),
     :           '#',FS34(i,2),'$',FS34(i,3),'@',FS34(i,4), FS34(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') FS34(i,1),
     :           FS34(i,2),FS34(i,3),FS34(i,4), FS34(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (FS34jmhzp/FS34a),
     :       (FS34hmkzp/FS34b), (FS34kzp/FS34c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

        hmkvar = (fs35hmksq - fs35b * (fs35hmkzp/fs35b)**2)
        if( fs35b .eq. 1 .or. hmkvar .lt. 0.0d0) then
          hmkvar = 0.0d0
        else
          hmkvar = hmkvar/(fs35b - 1.0d0)
        end if
        hmkstd = dsqrt(hmkvar)
        hmksem = hmkstd/dsqrt(fs35b)

        jmhvar = (fs35jmhsq - fs35a * (fs35jmhzp/fs35a)**2)
        if( fs35a .eq. 1 .or. jmhvar .lt. 0.0d0) then
          jmhvar = 0.0d0
        else
          jmhvar = jmhvar/(fs35a - 1.0d0)
        end if
        jmhstd = dsqrt(jmhvar)
        jmhsem = jmhstd/dsqrt(fs35a)

        kvar = (fs35ksq - fs35c * (fs35kzp/fs35c)**2)
        if( fs35c .eq. 1 .or. kvar .lt. 0.0d0) then
          kvar = 0.0d0
        else
          kvar = kvar/(fs35c - 1.0d0)
        end if
        kstd = dsqrt(kvar)
        ksem = kstd/dsqrt(fs35c)

        write(143, '(a)') ' '
        write(143, '(a)') '                           FS35'
        write(143, '(a)') '                J-H         H-K         K      airmass'
        jmhtrue = .false.
        hmktrue = .false.
        ktrue = .false.
        p = 1
        h = 1
        k = 1
        do i = 1, thfive
           if( fs35(i,2) .ne. -99.99) then
            if( ( FS35jmhzp * FS35(i,2)) .lt. 0) then
               jmhresid = abs(FS35jmhzp/FS35a) + abs(FS35(i,2))
             else if( ( FS35jmhzp * FS35(i,2)) .eq. 0) then
               jmhresid = abs(FS35jmhzp/FS35a) + abs(FS35(i,2))
             else
               jmhresid = abs(FS35jmhzp/FS35a) - abs(FS35(i,2))
             end if
             jmhresid = abs(jmhresid)
             jmhaway( p,1) = jmhresid/jmhstd
             if( jmhaway( p,1) .gt. 2) then
               newjmh = FS35jmhzp - FS35(i,2)
               jmhaway(p,3) = (newjmh/(FS35a-1))
               FS35jmhsq = FS35jmhsq - (FS35(i,2))**2
               jmhvar = (FS35jmhsq - (FS35a-1) * (newjmh/(FS35a-1))**2)
                 if( (FS35a-1) .eq. 1 .or. jmhvar .lt. 0.0d0) then
                    jmhvar = 0.0d0
                 else
                    jmhvar = jmhvar/((FS35a-1) - 1.0d0)
               end if
               newjmhstd = dsqrt(jmhvar)
               jmhaway(p,4) = newjmhstd/dsqrt(FS35a-1)
               if( ( newjmh * FS35(i,2)) .lt. 0) then
                 jmhresid = abs(newjmh/(FS35a-1)) + abs(FS35(i,2))
               else if( ( newjmh * FS35(i,2)) .eq. 0) then
                 jmhresid = abs(newjmh/(FS35a-1)) + abs(FS35(i,2))
               else
                 jmhresid = abs(newjmh/(FS35a-1)) - abs(FS35(i,2))
               end if
               jmhresid = abs(jmhresid)
               jmhaway(p,2) = jmhresid/newjmhstd
               p = p + 1
               jmhtrue = .true.
             end if
           end if
           if( FS35(i,3) .ne. -99.99) then
             if( ( FS35hmkzp * FS35(i,3)) .lt. 0) then
               hmkresid = abs(FS35hmkzp/FS35b) + abs(FS35(i,3))
             else if( ( FS35hmkzp * FS35(i,3)) .eq. 0) then
               hmkresid = abs(FS35hmkzp/FS35b) + abs(FS35(i,3))
             else
               hmkresid = abs(FS35hmkzp/FS35b) - abs(FS35(i,3))
             end if
             hmkresid = abs(hmkresid)
             hmkaway( h,1) = hmkresid/hmkstd
             if( hmkaway( h,1) .gt. 2) then
               newhmk = FS35hmkzp - FS35(i,3)
               hmkaway(h,3) = newhmk/(FS35b-1)
               FS35hmksq = FS35hmksq - (FS35(i,3))**2
               hmkvar = (FS35hmksq - (FS35b-1) * (newhmk/(FS35b-1))**2)
                 if( (FS35b-1) .eq. 1 .or. hmkvar .lt. 0.0d0) then
                    hmkvar = 0.0d0
                 else
                    hmkvar = hmkvar/((FS35b-1) - 1.0d0)
               end if
               newhmkstd = dsqrt(hmkvar)
               hmkaway(h,4) = newhmkstd/dsqrt(FS35b-1)
               if( ( newhmk * FS35(i,3)) .lt. 0) then
                 hmkresid = abs(newhmk/(FS35b-1)) + abs(FS35(i,3))
               else if( ( newhmk * FS35(i,3)) .eq. 0) then
                 hmkresid = abs(newhmk/(FS35b-1)) + abs(FS35(i,3))
               else
                 hmkresid = abs(newhmk/(FS35b-1)) - abs(FS35(i,3))
               end if
               hmkresid = abs(hmkresid)
               hmkaway(h,2) = hmkresid/newhmkstd
               h = h + 1
               hmktrue = .true.
             end if
           end if
           if( FS35(i,4) .ne. -99.99) then
             if( ( FS35kzp * FS35(i,4)) .lt. 0) then
               kresid = abs(FS35kzp/FS35c) + abs(FS35(i,4))
             else if( ( FS35kzp * FS35(i,4)) .eq. 0) then
               kresid = abs(FS35kzp/FS35c) + abs(FS35(i,4))
             else
               kresid = abs(FS35kzp/FS35c) - abs(FS35(i,4))
             end if
             kresid = abs(kresid)
             kaway( k,1) = kresid/kstd
             if( kaway( k,1) .gt. 2) then
               newk = FS35kzp - FS35(i,4)
               kaway(k,3) = newk/(FS35c-1)
               FS35ksq = FS35ksq - (FS35(i,4))**2
               kvar = (FS35ksq - (FS35c-1) * (newk/(FS35c-1))**2)
                 if( (FS35c-1) .eq. 1 .or. kvar .lt. 0.0d0) then
                    kvar = 0.0d0
                 else
                    kvar = kvar/((FS35c-1) - 1.0d0)
               end if
               newkstd = dsqrt(kvar)
               kaway(k,4) = newkstd/dsqrt(FS35c-1)
               if( ( newk * FS35(i,4)) .lt. 0) then
                 kresid = abs(newk/(FS35c-1)) + abs(FS35(i,4))
               else if( ( newk * FS35(i,4)) .eq. 0) then
                 kresid = abs(newk/(FS35c-1)) + abs(FS35(i,4))
               else
                 kresid = abs(newk/(FS35c-1)) - abs(FS35(i,4))
               end if
               kresid = abs(kresid)
               kaway(k,2) = kresid/newkstd
               k = k + 1
               ktrue = .true.
             end if
           end if
           if( jmhtrue .and. (.not. hmktrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') FS35(i,1),
     :      '#',FS35(i,2),FS35(i,3),FS35(i,4), FS35(i,5)
           else if( hmktrue .and. (.not. jmhtrue) .and. (.not. ktrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') FS35(i,1),
     :           FS35(i,2),'$',FS35(i,3),FS35(i,4), FS35(i,5)
           else if( ktrue .and. (.not. jmhtrue) .and. (.not. hmktrue)) then
             write(143,'(F8.0,5X,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') FS35(i,1),
     :           FS35(i,2),FS35(i,3),'@',FS35(i,4), FS35(i,5)
           else if( jmhtrue .and. hmktrue .and. (.not. ktrue)) then
             write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,3x,f8.5,3x,f8.5)') FS35(i,1),
     :           '#',FS35(i,2),'$',FS35(i,3),FS35(i,4), FS35(i,5)
           else if( hmktrue .and. ktrue .and. (.not. jmhtrue)) then
             write(143,'(F8.0,5X,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') FS35(i,1),
     :           FS35(i,2),'$',FS35(i,3),'@',FS35(i,4), FS35(i,5)
           else if( jmhtrue .and. ktrue .and. (.not. hmktrue)) then
             write(143,'(F8.0,4X,a,F8.5,3X,f8.5,2x,a,f8.5,3x,f8.5)') FS35(i,1),
     :           '#',FS35(i,2),FS35(i,3),'@',FS35(i,4), FS35(i,5)
           else if( jmhtrue .and. hmktrue .and. ktrue) then
           write(143,'(F8.0,4X,a,F8.5,2X,a,f8.5,2x,a,f8.5,3x,f8.5)') FS35(i,1),
     :           '#',FS35(i,2),'$',FS35(i,3),'@',FS35(i,4), FS35(i,5)
           else
             write(143,'(F8.0,5X,F8.5,3X,f8.5,3x,f8.5,3x,f8.5)') FS35(i,1),
     :           FS35(i,2),FS35(i,3),FS35(i,4), FS35(i,5)
           end if
           jmhtrue = .false.
           hmktrue = .false.
           ktrue = .false.
        end do
        write(143, '(a)') '         -----------------------------------'
        write(143, '(a,7x,F8.5,3x,f8.5,3x,f8.5)') 'Mean  ', (FS35jmhzp/FS35a),
     :       (FS35hmkzp/FS35b), (FS35kzp/FS35c)
        write(143, '(a,8x,F8.5,3x,f8.5,3x,f8.5)') 'sem  ', jmhsem, hmksem, ksem
        do i = 1, p-1
           write(143, '(a,f5.3,a,f5.3,a)') '# is ',jmhaway(i,1),' and ',jmhaway(i,2),'sigma away from the mean.'
           if( jmhaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised J-H mean & sem: ', jmhaway(i,3),
     :jmhaway(i,4)
           end if
        end do
        do i = 1, h-1
           write(143, '(a,f5.3,a,f5.3,a)') '$ is ',hmkaway(i,1),' and ',hmkaway(i,2),'sigma away from the mean.'
           if( hmkaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised H-K mean & sem: ', hmkaway(i,3),
     :hmkaway(i,4)
           end if
        end do
        do i = 1, k-1
           write(143, '(a,f5.3,a,f5.3,a)') '@ is ',kaway(i,1),' and ',kaway(i,2),'sigma away from the mean.'
           if( kaway(i,2) .gt. 3) then
             write(143, '(a,2f8.5)') 'Revised K mean & sem: ', kaway(i,3),
     :kaway(i,4)
           end if
        end do

! close the file
        close(143)
        end


