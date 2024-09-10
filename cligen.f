c
c     Please address inquiries to
c
c         WEPP Technical Support
c         USDA-ARS-NSERL
c         1196 Building SOIL
c         West Lafayette, IN 47907-1196
c         Phone 765 494-8673
c
c     Version 4.3 July 1998  West Lafayette, IN.
c     Changes to allow reading of data from any single station parameter file.
c     Correction of version numbers.
c     Multiple year generation, output file, and summary added
c     Dewpoint temperature added with wind speed and velocity
c     using mean standard deviation and skew coefficient.
c     Change made to place parameter in state files (i.e. al Alabama parms).
c     All parameters rainfall, temperature, radiation, dewpoint temperature,
c     wind speed and direction are combined into one set with 82 lines of data.
c     Storm duration calculation change (4.607 to 9.210).
c     Weighting factor removed from Maximum, Minimum, Dew Point Temperature and
c     Solar Radiation.
c     Addition of formatting to allow use of interpolated station file.
c
      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
      common /bk3/ alai(5),salb(5),dm(5),rd(5),uw,ep,es,rain,ws,
     1             je,j,ida
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
      common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
     1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
     1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
     1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
     1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)

c prw(1,12) monthly probability of wet day after dry day
c prw(2,12) monthly probability of wet day after wet day

      common /bk5/ r(366),vmx(5),v(5),flu(6),bp(5),hc(5),hcr(5),fp(5),
     1             cs(5),sk(5),smx(5),ev,sp,rl,q1,o,qd,af,pq,yp,aff,
     1             amps,cn,sml,cfp(5),swm(5),sfc(5),dk(5)
      common /bk9/ hyd(250,5),xi(2,5),wi(12),tc(6),al(5),ab,ab1,rn1,r1,
     1             pr,lu1,vo,nmb,tp,tr,ddt,tt
      common /cl1/ prcip(12,31),tgmx(12,31),tgmn(12,31),radg(12,31),
     1             dur(12,31)
      common /sumr/sump(13),sumtx(12),sumtm(12),sumrd(12),
     1             sumdr(12)
c
      dimension sumpp(13),sumptx(12),sumptm(12),sumprd(12),
     1          sumpdr(12)
c
      dimension smy(12),wgt(3),tymax(4),
     1          timpkd(0:12),tmpcmx(12),tmpcmn(12)
c
      integer elev,years
c
      character*19 site(3)
      character*2 dr(55)
      character*1 yc
c     character*6 nstat
      character*41 stid
      character*41 stidd
      character*51 outfil,infile,filein
      character*20 county

      data dr/'al','az','ar','ca','co','ct','de','fl','ga','id',
     1        'il','in','ia','ks','ky','la','me','md','ma','mi',
     1        'mn','ms','mo','mt','ne','nv','nh','nj','nm','ny',
     1        'nc','nd','oh','ok','or','pa','ri','sc','sd','tn',
     1        'tx','ut','vt','va','wa','wv','wi','wy','dc','ak',
     1        'hi','pr','pi','if','  '/

      data tymax/180.34,154.94,307.34,330.2/
      timpkd(0)=0.0
c     Version number set here for option 5 output header
      version=4.3
      iyear=1
c
      call header
      write(*,*)' Press Enter to Continue'
      read(*,'(a1)')yc
c CODE COMMENTED OUT BY DCF - TO REMOVE USE OF STATIONS FILE 7/27/98
c10   write(*,380)
c     write(*,360)
c     write(*,370)
c     write(*,700)
c     read(*,390,err=15)istate
c     go to 16
c15   write(*,*)' Error Entering State Code '
c     write(*,*)
c     write(*,*)' Press Enter to Continue'
c     read(*,'(a1)')yc
c     go to 10
c16   if(istate.eq.99) then
c       iscnt = 1
c       iyr = 0
c       go to 50
c     end if
c     open(11,file='stations',status='old')
c     rewind (11)
c     iscnt=0
c     iyr=0
c     write(*,710)
c     kknt=0
c20   read(11,410,end=30)stid,nstate
c     if(nstate.eq.istate) then
c        kknt=kknt+1
c        iscnt=iscnt+1
c        backspace(11)
c        read(11,420)stid,nst,nstat,lat,long,county
c          if(kknt.gt.20) then
c          kknt=0
c          write(*,*) 'Press "ENTER" to continue.'
c          read(*,'(a1)') yc
c          endif
c        write(*,430)stid,nstat,lat,long,county
c     endif
c     if(nstate.gt.istate) go to 30
c     go to 20
c30   close (11)
c     if(iscnt.gt.0) go to 50
c40   write(*,720)
c     read(*,660) yc
c     if(yc.eq.'y'.or.yc.eq.'Y') go to 10
c     go to 230
c50   write(*,730)
c     iname = istate
c     if(iname.eq.66) iname=52
c     if(iname.eq.91) iname=53
c     if(iname.eq.99) iname=54
c     read(*,440)index
c     open(10,file=dr(iname),status='old',err=55)
c     rewind (10)
c     go to 60
c55   write(*,*)'  Mother Nature does not want to run state ',dr(iname)
c     write(*,*)'  Parameters for this state are not loaded on '
c     write(*,*)'  this computer.  Check directory for file ',dr(iname)
c     write(*,*)'  -- Enter q to quit or c to Continue'
c     read(*,660) yc
c     if(yc.eq.'q'.or.yc.eq.'Q') then
c      go to 230
c     else
c      go to 10
c     endif
c CODE ABOVE COMMENTED OUT BY DCF - TO REMOVE USE OF STATIONS FILE 7/27/98

c    Gettin Station from State File
c60   read(10,450,end=40)stidd,nst,nstat,igcode
c     if(nst.ne.istate) then
c        read(10,460)
c        go to 60
c     endif
c     if(nstat.ne.index) then
c        read(10,460)
c        go to 60
c     endif

      index=1
 10   continue
      write(*,725)
      read(*,640)filein
      open(10,file=filein,status='old',err=10)
      read(10,450)stidd,nst,nstat,igcode
      

c     Read Precipitation, Temperature, Radiation, etc.
c     ylt         - Latitude.
c     yll         - Longitude.
c     rst(i,j)    - Array Of Monthly precipitation stats.
c     obmx        - Maximum Temperature.
c     obmn        - Minimum Temperature.
c     stdmx       - Standard Dev. Tmax.
c     stdmn       - Standard Dev. Tmin.
c     stdsl       - Standard Dev. Sol.
c     wi          - Average Maximum .5 Hour Precip.
c     rh          - Dew Point Temp.
      read(10,470)ylt,yll,years,itype,elev,tp6
      read(10,480)(rst(i,1),i=1,12),(rst(i,2),i=1,12),(rst(i,3),i=1,12)
      read(10,480)(prw(1,i),i=1,12),(prw(2,i),i=1,12)
      read(10,480)(obmx(i),i=1,12)
      read(10,480)(obmn(i),i=1,12)
      read(10,480)(stdtx(i),i=1,12),(stdtm(i),i=1,12)
      read(10,480)(obsl(i),i=1,12)
      read(10,480)(stdsl(i),i=1,12)
      read(10,480)(wi(i),i=1,12)
      read(10,480)(rh(i),i=1,12)
      read(10,485)(timpkd(i),i=1,12)
c     Wind Data read in here.
c     wvl(i,j,k) - array of wind paramters where
c     i - ith direction (1 - north  - 16 nnw)
c     j - parameters (1 - 4)
c       1 - % time from direction i
c       2 - mean speed from direction i
c       3 - standard deviation of speed from direction i
c       4 - skew coeficient of speed from direction i
c     Calm is treated seperately as direction 0, speed 0
c     Only a % time values is need for calm generation.

      read(10,1250)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
1250  format(8x,12f6.2)
      read(10,1250)(calm(i),i=1,12)
      read(10,1260)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
1260  format(a19,f6.3,2(2x,a19,f6.3))
      elev=elev*.3048

      do 70 i=1,12
         wi(i)=wi(i)
         cvtx(i)=stdtx(i)/obmx(i)
         cvtm(i)=stdtm(i)/obmn(i)
         if(obsl(i).le.0.0) then
            cvs(i)=0.0
            go to 70
         endif
         cvs(i)=stdsl(i)/obsl(i)
 70   continue
      do 80 i=1,12
         wi(i)=wi(i)
 80   continue
      do 6000 i=1,12
6000  dir(i,1)=wvl(1,1,i)
      do 7010 i=1,12
       do 7000 j=2,16
         dir(i,j) = dir(i,j-1)+wvl(j,1,i)
7000   continue
       j=17
       dir(i,j)=100.0
7010  continue
      do 7050 i=1,12
       do 7050 j=1,17
        dir(i,j)=dir(i,j)*.01
7050  continue
      close (10)

c     Close the Parameter File and Write the Parameter if He Wants.

      write(*,490)stidd,nstat
      read(*,660)yc
      if(yc.eq.'n'.or.yc.eq.'N') go to 110
      write(*,500)obmx,obmn
      write(*,520)obsl
      write(*,540)
      do 90 i=1,12
         write(*,560) prw(1,i),prw(2,i)
 90   continue
      write(*,570)
      do 100 i=1,12
         write(*,600)(rst(i,j),j=1,3)
 100  continue
      write(*,610)
      write(*,620)(stdtx(i),i=1,12)
      write(*,620)(stdtm(i),i=1,12)
      write(*,630)(stdsl(i),i=1,12)
      write(*,590)
      write(*,620)(cvtx(i),i=1,12)
      write(*,620)(cvtm(i),i=1,12)
      write(*,620)(cvs(i),i=1,12)
      write(*,620)(wi(i),i=1,12)
      write(*,265)rh
      write(*,620)tp6
      write(*,2070)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
2070  format(1x,'Wind Data Interpolated from',
     1       /1x,a19,f6.3,2(2x,a19,f6.3))
      write(*,2060)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
2060  format(1x,12f6.2)
      write(*,2060)(calm(i),i=1,12)
c
c     Begin Climate Generation
c
 110  sml=0.0
      r5max=0.0
      do 120 i=1,12
         if(wi(i).ge.r5max) r5max=wi(i)
 120  continue
      do 125 i=1,12
         wi(i)=wi(i)
 125  continue
      call r5mon(tp6)
      ab=0.02083
      ab1=1.0-ab
      nt=0
      clt=57.296
      pit=58.13
      pi2=6.283185
      xx=ylt/clt
      yls=sin(xx)
      ylc=cos(xx)
      vv=randn(k1)
      l=2
      if(vv.gt.prw(1,1)) l=1
      rn1=randn(k7)
      v1=randn(k2)
      v3=randn(k3)
      v5=randn(k4)
      v7=randn(k5)
      v9=randn(k8)
      v11=randn(k9)
      msim=1
      nsim=1

c     Get Options from User

56    write(*,750)
      read(*,*)iopt
      if (iopt.eq.8) go to 230
      if (iopt.eq.6) then
        write(*,*)'Enter Observed Data Input File Name'
        read(*,640)infile
        open(9,file=infile,status='old',err=57)
        rewind(9)
        read(9,775)ioyr
        backspace (9)
      endif
      go to 58
57    write(*,*)' Error - File not Found '
      write(*,*)' Check Directory for Observed Data File or'
      write(*,*)' Renter File Name'
      write(*,*)' Enter c to continue or q to quit'
      read(*,'(a1)')yc
      if(yc.eq.'q'.or.yc.eq.'Q') go to 230
      go to 56
58    numyr=1
      if (iopt.eq.1) go to 140

c     Single storm input data section

      if (iopt.eq.4.or.iopt.eq.7) then
        write(*,*)
        write(*,*)' Enter Month Day and Year of Storm (mo da yr)'
        read(*,*)mo,jd,ibyear
        write(*,*)' Enter Design Storm Amount in Inches (e.g. 6.30)'
        read(*,*)damt
       if(iopt.eq.4) then
         write(*,*)' Enter Storm Duration in Hours (e.g. 6)'
         read(*,*)usdur
         write(*,*)' Enter Time to Peak Intensity (% Duration e.g. .4)'
         read(*,*)ustpr
         write(*,*)' Enter Maximum Intensity Inches/Hour (e.g. 3.0)'
         read(*,*)uxmav
       endif
       go to 130
      endif
      if(iopt.eq.6) then
        ibyear=ioyr
        numyr=100
        go to 130
      endif
      write(*,755)
      read(*,*) ibyear
      write(*,760)
      read(*,*) numyr
130   iyear=ibyear
      if (iopt.eq.2) go to 140
 135  write(*,770)index
      read(*,640)outfil

c     Output Options
c      3 - CREAMS - GLEAMS
c      4 - Single Storm WEPP
c      5 - Continuous Storms WEPP
c      6 - Observed Data WEPP
c      7 - Design Storm WEPP
c
      if(iopt.eq.3) then
         open(8,file=outfil,status='new',err=138)
         rewind (8)
         go to 140
      else
         open(7,file=outfil,status='new',err=138)
         rewind (7)
         go to 140
      endif
 138  write(*,*)' '
      write(*,*)'     **** File Already Exists *****'
      write(*,*)' '
      write(*,*)'           ',outfil
      write(*,*)' '
      write(*,*)'          Enter New File Name '
      write(*,*)' '
      go to 135

c     Get Everything Ready to Start Generation by Options Selected

 140  do 150 i=1,12
         sumpp(i)=0.0
         sumptx(i)=0.0
         sumptm(i)=0.0
         sumprd(i)=0.0
         sumpdr(i)=0.0
 150  continue
      sumpp(13)=0.0

c     See What Option was Selected and Set Paths

      nbt=1
         if(iopt .ge. 4)then
            isim=1
          if(iopt.eq.4.or.iopt.eq.7) isim =2
            itemp=0
            write(7,642)version
            write(7,778)isim,itemp,igcode
            if(iopt.ge.4) then
              write(7,644)stidd,ylt,yll,elev,years,iyear,numyr
c              write(7,646) iyear,numyr
c  CALCULATE MONTHLY RAINFALL AMOUNTS
              do 111 kkk = 1,12
                xm = nc(kkk+1)-nc(kkk)
c        calculate number of days of rainfall in month
                smy(kkk) = xm*prw(2,kkk)/(1.-prw(1,kkk) + prw(2,kkk))
c        monthly rainfall in mm
                smy(kkk) = smy(kkk) * rst(kkk,1) * 25.4
                tmpcmx(kkk)=(obmx(kkk)-32.0)*(5.0/9.0)
                tmpcmn(kkk)=(obmn(kkk)-32.0)*(5.0/9.0)
 111          continue
              write(7,500)tmpcmx,tmpcmn
              write(7,520)obsl
              write(7,555)smy
              write(7,648)
            endif
      endif
      if(iopt.eq.4.or.iopt.eq.7) then
        nt=0
        if((iyear-iyear/4*4).eq.0) nt=1
        ntd1 = jdt(nc,jd,mo,nt)
        nbt = ntd1
        numyr=1
      endif

c    Generate Data by Number of Years and Option - MAIN LOOP

      do 220 ii=1,numyr
 160     ntd=365
         if(iopt.le.3.or.iopt.eq.5.or.iopt.eq.6) then
            if((iyear-iyear/4*4).eq.0) ntd=366
         endif
         do 170 i=1,12
            do 170 jk=1,31
               prcip(i,jk)=0.0
               tgmx(i,jk)=0.0
               tgmn(i,jk)=0.0
               radg(i,jk)=0.0
               dur(i,jk)=0.0
 170     continue

c     Start of the Daily Generation Loop  with nbt and ntd

         if(iopt.eq.4.or.iopt.eq.7) ntd=ntd1
         do 180 ida=nbt,ntd
         if(iopt.eq.6) then
           msim=0
           nsim=0
           read(9,777,end=225)irida,itmxg,itmng
           if(irida.eq.9999) nsim=1
           if(itmxg.eq.9999) msim=1
           if(itmng.eq.9999) msim=1
           r(ida)=irida*.01
           tmxg=itmxg
           tmng=itmng
         endif
         idr=ida
         call jlt(mo,jd,idr,ntd)
            call clgen
             call windg
            th=th*clt
            prcip(mo,jd)=r(ida)
            tgmx(mo,jd)=tmxg
            tgmn(mo,jd)=tmng
            tmxg=(tmxg-32.0)*(5.0/9.0)
            tmng=(tmng-32.0)*(5.0/9.0)
            tdp = (tdp-32.0)*(5.0/9.0)
            radg(mo,jd)=ra
            if(r(ida).le.0.0) then
               r(ida)=0.0
               dur(mo,jd)=0.0
            else
               call alph
               dur(mo,jd)=9.210/(-2.0*alog(1.0-r1))
               if(dur(mo,jd).gt.24.0) dur(mo,jd)=24.0
            endif

c    Set duration if a single storm is selected

c      if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=rdur
       if (iopt.ge.4) then
        if(r(ida).gt.0.) then
         call alph
         xr=r(ida)*25.4
         tpr=timepk(timpkd,k10)
         if(tpr.gt.0.99) tpr=0.99
         r5p=-2.0*xr*alog(1.0-r1)
         if(r5p.gt.tymax(itype)) r5p = tymax(itype)
         xmav=r5p/(xr/dur(mo,jd))
         if((tmxg+tmng)/2.0.le.0.0) xmav = 1.01
         if(xmav.lt.1.01) xmav =1.01
       else
         xr=r(ida)*25.4
         xmav=0.0
         tpr=0.0
        endif
c     Change for new option 4 and 7
       if(iopt.eq.4) then
         dur(mo,jd)=usdur
         xr=damt*25.4
         tpr=ustpr
         xmav=(uxmav*25.4)/(xr/dur(mo,jd))
         if(xmav.lt.1.01) xmav=1.01
        endif
       if(iopt.eq.7) then
         dur(mo,jd)=24.
         xr = damt*25.4
         xmav=tymax(itype)/(xr/dur(mo,jd))
         if(xmav.lt.1.01) xmav=1.01
         tpr=dtp(itype)
       endif

c     Write WEPP Continuous Storm File
c     Writes WEPP Single Storm when nbt=ndt

               write(7,650) jd,mo,iyear,xr,dur(mo,jd),tpr,
     1                      xmav,tmxg,tmng,radg(mo,jd),wv,th,tdp
      endif
 180     continue

c   End of Daily Loop

         if(iopt.gt.1) go to 190

c     Option 1 Stuff

         write(*,780)
         read(*,660) yc
         if(yc.eq.'y'.or.yc.eq.'Y') then
          iview=1
          call clmout(iview)
         end if
         write(*,790)
         read(*,660)yc
         if(yc.eq.'y'.or.yc.eq.'Y') go to 160
         write(*,800)
         read(*,660) yc
         if(yc.eq.'y'.or.yc.eq.'Y') then
            go to 10
         endif
         go to 230
 190     continue
       if(iopt.eq.3) go to 205
       if(iopt.ge.4) go to 215

c     Option 2 Stuff

       if(iopt.eq.2) then
         jj=ii
         call clmout(0)
         write(*,820) iyear
         write(*,*)' '
         write(*,670)
         write(*,240)jj,(sump(i),i=1,12)
         write(*,250)jj,(sumtx(i),i=1,12)
         write(*,260)jj,(sumtm(i),i=1,12)
         write(*,270)jj,(sumrd(i),i=1,12)
         write(*,280)jj,(sumdr(i),i=1,12)
         write(*,300)sump(13),stidd
         an=numyr
         do 200 i=1,12
            sumpp(i)=sumpp(i)+sump(i)/an
            sumptx(i)=sumptx(i)+sumtx(i)/an
            sumptm(i)=sumptm(i)+sumtm(i)/an
            sumprd(i)=sumprd(i)+sumrd(i)/an
            sumpdr(i)=sumpdr(i)+sumdr(i)/an
 200     continue
         sumpp(13)=sumpp(13)+sump(13)/an
      endif
      go to 215
 205  continue

c     Option 3 Stuff

      if(iopt.eq.3) then
         nb=1
         ne=10
         do 210  j=1, 36
            write(8,680)nstat,iyear,(r(i),i=nb,ne),j
            nb=ne + 1
            ne=nb + 9
 210     continue
         nb=361
         ne=366
         if(((iyear/4*4)-iyear).ne.0) r(366)=0.0
         j=37
         write(8,690)nstat,iyear,(r(i),i=nb,ne),j
      endif
 215     iyear=iyear+1
 220  continue

c   End - MAIN LOOP

      if(iopt.eq.2) then
        write(*,290)numyr
        write(*,670)
        write(*,310)(sumpp(i),i=1,12)
        write(*,320)(sumptx(i),i=1,12)
        write(*,330)(sumptm(i),i=1,12)
        write(*,340)(sumprd(i),i=1,12)
        write(*,350)(sumpdr(i),i=1,12)
        write(*,305)numyr,sumpp(13),stidd
      endif

c    Check for Another Run or End

 225  write(*,810)
      read(*,660) yc
      if(yc.eq.'y'.or.yc.eq.'Y') go to 10
      if(iopt.ge.4) then
        write(7,*) ' '
        close (7)
      endif
      if(iopt.eq.3) then
        write(8,*)' '
        close (8)
      endif
 230  stop 'Program terminated by user -  returning to DOS'
 240  format(1x,'prcp',i3,12f6.2)
 250  format(1x,'tmax',i3,12f6.2)
 260  format(1x,'tmin',i3,12f6.2)
 265  format(1x,'Average Monthly Dew Point Temperature',/1x,12f6.2)
 270  format(1x,'rad ',i3,12f6.1)
 280  format(1x,'dur ',i3,12f6.2)
 290  format(/22x,'Average Values for ',i2,' Years'/)
 300  format(/1x,'Annual Precipitation =',f6.2,a30/)
 305  format(/1x,'Average Annual Precipitation for ',i2,
     1' Years =',f6.2,a30/)
 310  format(1x,'prcp   ',12f6.2)
 320  format(1x,'tmax   ',12f6.2)
 330  format(1x,'tmin   ',12f6.2)
 340  format(1x,'rad    ',12f6.1)
 350  format(1x,'dur    ',12f6.2)
 360  format(1x,'01 Alabama       20 Michigan       39 S. Dakota',
     1/1x,'02 Arizona       21 Minnesota      40 Tennessee',
     2/1x,'03 Arkansas      22 Mississippi    41 Texas',
     3/1x,'04 California    23 Missouri       42 Utah',
     4/1x,'05 Colorado      24 Montana        43 Vermont',
     5/1x,'06 Connecticut   25 Nebraska       44 Virginia',
     6/1x,'07 Delaware      26 Nevada         45 Washington',
     7/1x,'08 Florida       27 New Hampshire  46 West Virginia',
     8/1x,'09 Georgia       28 New Jersey     47 Wisconsin',
     9/1x,'10 Idaho         29 New Mexico     48 Wyoming')
 370  format(1x,'11 Illinois      30 New York       49 Washington DC',
     1/1x,'12 Indiana       31 North Carolina 50 Alaska',
     2/1x,'13 Iowa          32 North Dakota   51 Hawaii',
     3/1x,'14 Kansas        33 Ohio           66 Puerto Rico',
     4/1x,'15 Kentucky      34 Oklahoma       91 Pacific Islands',
     5/1x,'16 Louisiana     35 Oregon         99 Interpolated File',
     6/1x,'17 Maine         36 Pennsylvania   ',
     7/1x,'18 Maryland      37 Rhode Island    ',
     8/1x,'19 Massachusetts 38 South Carolina  ')
 380  format(1x,'State Climate Code - Available Stations'/)
 390  format(i2)
 410  format(a41,i2)
 420  format(a41,i2,i4,i3,i4,a20)
 430  format(1x,a41,2x,i4,3x,i3,2x,i4,1x,a19)
 440  format(i4)
 450  format(a41,i2,i4,i2)
 460  format(//////////////////////////////////////////////////
     1       ///////////////////////////////)
 470  format(6x,f7.2,6x,f7.2,7x,i3,7x,i2/12x,i5,17x,f5.2)
 480  format(8x,12f6.2)
 485  format(8x,12f6.3)
 490  format(/1x,'Do you want to view data found for station',//,
     1 a41,2x,i4,' (y/n)?: ')
 500  format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 520  format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 555  format(1x,'Observed monthly ave precipitation (mm)',/,
     1       12(1x,f5.1))
 540  format(/1x,'wet-dry state probabilities'/)
 560  format(1x,2f10.5)
 570  format(/1x,'mean,st.dev.,and skew coef. of daily rainfall'/)
 590  format(/1x,'coefficient of variation for max, min temp,',
     1          ' solar radiation, and max .5 hr rain'/)
 600  format(1x,3f10.5)
 610  format(/1x,'standard deviation for max and min temp,',
     1          ' and solar radiation'/)
 620  format(1x,12f6.2)
 630  format(1x,12f6.1)
 640  format(a51)
 642  format(f5.2)
 644  format('   Station: ',a41,6x,' CLIGEN VERSION 4.3',/
     1       ' Latitude Longitude Elevation (m) Obs. Years ',
     1       '  Beginning year  Years simulated',/
     1 2f9.2,i12,2i12,i16)
 648  format(' da mo year  prcp  dur   tp     ip  tmax',
     1       '  tmin  rad  w-vl w-dir  tdew',
     2         /,13x,'(mm)  (h)',15x,'(C)   (C)',
     3       ' (l/d) (m/s)(Deg)   (C)')
c650  format(2i3,i5,f6.1,f6.2,f6.2,f7.2,2f6.1,f5.0,f5.1,f5.0,f6.1)
 650  format(2i3,1x,i4,1x,f5.1,1x,f5.2,1x,f4.2,1x,f6.2,2(1x,f5.1),
     1       1x,f4.0,1x,f4.1,2x,f4.0,1x,f5.1)
 660  format(a1)
 670  format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 680  format(i5,i5,10f5.2,8x,i2)
 690  format(i5,i5,6f5.2,28x,i2)
 700  format(/1x,'Enter state climate code (ex. 01 for Alabama): ')
 710  format(/1x,'Stations Available',19x,' Station No. ',
     1       'Lat.  Long County'/,
     11x,8('-'),1x,9('-'),19x,17('-'),2x,4('-'),1x,7('-')/)
 720  format(/1x,'No stations available - do you want to',
     1' continue (y/n)? ')
 725  format(/1x,'Enter the name of your CLIGEN parameter file: ')
 730  format(/1x,'Enter the station index: ')
 750  format(/1x,'Weather Generator Options',/,1x,7('-'),1x,
     19('-'),1x,7('-'),//,1x,'1 - Single Year Simulation - Screen',/,
     11x,'2 - Multiple Year - Screen Output',/,
     11x,'3 - Multiple Year Simulation - CREAMS - GLEAMS Output File',/,
     11x,'4 - Selected Single Storm WEPP - Output File',/,
     11x,'5 - Multiple Year - WEPP Output File',/,
     11x,'6 - Read Observed P and Temp and Generate Missing Data',/,
     11x,'7 - Single Design Storm - TR 55 Storm Type WEPP Output File',/
     11x,'8 - Exit Weather Generator Program',//,
     11x,'Enter generator option (1-8): ')
 755  format(/1x,'Enter beginning simulation year',
     1' (positive integer value; e.g. 1 ): ')
 760  format(/1x,'Enter number of years to simulate: ')
 770  format(/1x,'Enter output file name (ex. Indy.cli): ',
     1       'Station No. ',i4)
 775  format(13x,i2)
 777  format(15x,3i5)
 778  format(3i4)
 780  format(/1x,'Do you want to view generated data (y/n)? ')
 790  format(/1x,'Do you want to simulate another year (y/n)? ')
 800  format(/1x,'Do you want another Station (y/n)? ')
 810  format(/1x,'Do you want to continue (y/n)? ')
 820  format(/15x,'Summary of Elements Generated - Year ',i4)
      end
c
c
c
      subroutine alph
c
c     This subroutine computes alpha, a dimensionless parameter that
c     expresses the fraction of total rainfall that occurs
c     during 0.5 ho.
c
c
      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
      common /bk3/ alai(5),salb(5),dm(5),rd(5),uw,ep,es,rain,ws,
     1             je,j,ida
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
      common /bk5/ r(366),vmx(5),v(5),flu(6),bp(5),hc(5),hcr(5),fp(5),
     1             cs(5),sk(5),smx(5),ev,sp,rl,q1,o,qd,af,pq,yp,aff,
     1             amps,cn,sml,cfp(5),swm(5),sfc(5),dk(5)
      common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
     1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
     1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
     1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
     1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)
      common /bk9/ hyd(250,5),xi(2,5),wi(12),tc(6),al(5),ab,ab1,rn1,r1,
     1             pr,lu1,vo,nmb,tp,tr,ddt,tt
c
c      dimension k7(4)
c
      ei=r(ida)-sml
      ai=ab1/(wi(mo)-ab)
      if (ei.lt.1.0) go to 10
      ajp=1.0-exp(-5.0/ei)
      go to 20
 10   ajp=1.0
 20   r1=dstg(rn1,ai,k7)
      r1=(ei*(ab+r1*(ajp-ab))+sml*ab)/r(ida)
      return
      end
c
c
c
      block data
c
c     contains generator seeds for the weather generator
c
      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
      common /bk3/ alai(5),salb(5),dm(5),rd(5),uw,ep,es,rain,ws,
     1             je,j,ida
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
      common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
     1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
     1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
     1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
     1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)
      common /bk5/ r(366),vmx(5),v(5),flu(6),bp(5),hc(5),hcr(5),fp(5),
     1             cs(5),sk(5),smx(5),ev,sp,rl,q1,o,qd,af,pq,yp,aff,
     1             amps,cn,sml,cfp(5),swm(5),sfc(5),dk(5)
      common /bk9/ hyd(250,5),xi(2,5),wi(12),tc(6),al(5),ab,ab1,rn1,r1,
     1             pr,lu1,vo,nmb,tp,tr,ddt,tt
      common /cl1/ prcip(12,31),tgmx(12,31),tgmn(12,31),radg(12,31),
     1             dur(12,31)
      common /sumr/sump(13),sumtx(12),sumtm(12),sumrd(12),
     1             sumdr(12)
c
      data k1/9,98,915,92/
      data k2/135,28,203,85/
      data k3/43,54,619,33/
      data k4/645,9,948,65/
      data k5/885,41,696,62/
      data k6/51,78,648,0/
      data k7/227,57,929,37/
      data k8/205,90,215,31/
      data k9/320,73,631,49/
      data k10/22,103,82,4/
      data nc/0,31,59,90,120,151,181,212,243,273,304,334,365/
      data dtp/.4,.32,.5,.5/
      data dmxi/18.24,5.76,32.88,20.16/
      end
c
c
c
      subroutine clgen
c
c     This subroutine simulates daily solar radiation, simulates daily
c     precipitation and/or maximum and minimum air temperature at the
c     users option, and calls functions randn and dstn1.
c
      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
      common /bk3/ alai(5),salb(5),dm(5),rd(5),uw,ep,es,rain,ws,
     1             je,j,ida
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
      common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
     1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
     1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
     1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
     1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)
      common /bk5/ r(366),vmx(5),v(5),flu(6),bp(5),hc(5),hcr(5),fp(5),
     1             cs(5),sk(5),smx(5),ev,sp,rl,q1,o,qd,af,pq,yp,aff,
     1             amps,cn,sml,cfp(5),swm(5),sfc(5),dk(5)
c
      xi=ida
      sd=0.4102*sin((xi-80.25)/pit)
      ch=-yls*tan(sd)/ylc
      if (ch.ge.1.0) go to 10
      if (ch.le.-1.0) go to 20
      h=acos(ch)
      go to 30
 10   h=0.0
      go to 30
 20   h=3.1416
 30   ys=yls*sin(sd)
      yc=ylc*cos(sd)
      rmx=711.0*(h*ys+yc*sin(h))
      if (nsim.eq.0) go to 40
      vv=randn(k1)
      if (prw(l,mo).le.0.0) go to 60
      if (vv.gt.prw(l,mo)) go to 60
      v8=randn(k5)
      if(rst(mo,3).eq.0.0) rst(mo,3)=0.01
      r6=rst(mo,3)/6.0
      xlv=(dstn1(v7,v8)-r6)*r6+1.0
      xlv=(xlv**3-1.0)*2.0/rst(mo,3)
      v7=v8
      r(ida)=xlv*rst(mo,2)+rst(mo,1)
      if (r(ida).lt.0.01) r(ida)=0.01
      go to 50
 40   if (r(ida).le.0.0) go to 60
 50   l=1
      go to 70
 60   r(ida)=0.0
      l=2
 70   if(msim.eq.0) then
       xx=1.
       v12=randn(k9)
       tdp=xx*dstn1(v11,v12)
       v11=v12
       go to 80
       endif
      xx=1.
      v2=randn(k2)
      tmxg=xx*dstn1(v1,v2)
      v1=v2
      v4=randn(k3)
      tmng=xx*dstn1(v3,v4)
      v12=randn(k9)
      tdp =xx*dstn1(v11,v12)
      v3=v4
      v11=v12
      tmxg=obmx(mo)+tmxg*stdtx(mo)
      tmng=obmn(mo)+tmng*stdtm(mo)
      if (tmng.gt.tmxg) tmng=tmxg-0.2*abs(tmxg)
c     TDP now calculated using standard dev. instead of CV. 3/95
c80   tdp =rh(mo)*(1.0+tdp*(cvtm(mo)+cvtx(mo)/2.))
 80   tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo)/2.))
      if (tdp.gt.((tmxg+tmng)/2.)) tdp=((tmxg+tmng)/2.)*0.99
c     Change to limit - dew point in cold months ADNe
      if(tdp.lt.-10.) tdp=1.1*tmng
      v6=randn(k4)
      ra=xx*dstn1(v5,v6)
      rx=rmx-obsl(mo)
      if (obsl(mo).gt.rx) rx=obsl(mo)
      ra=obsl(mo)+ra*rx/4.0
      if(ra.ge.rmx) ra=0.90*rmx
      if (ra.le.0.0) ra=0.05*rmx
      v5=v6
      return
      end
c
c
c
      subroutine clmout(iview)
c     This subroutine calculates daily and monthly values for options
c     1 and 2.  It writes output to screen for option 1.

      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
      common /cl1/ prcip(12,31),tgmx(12,31),tgmn(12,31),radg(12,31),
     1             dur(12,31)
      common /sumr/sump(13),sumtx(12),sumtm(12),sumrd(12),
     1             sumdr(12)
c
      do 10 i =1,12
         sump(i) = 0.0
         sumtx(i)= 0.0
         sumtm(i)= 0.0
         sumrd(i)= 0.0
         sumdr(i)= 0.0
 10   continue
      sump(13)= 0.0
      do 20 i =1,12
         an = nc(i+1)-nc(i)
         do 20 j =1,31
            sump(i)=sump(i)+prcip(i,j)
            sump(13)=sump(13)+prcip(i,j)
            sumtx(i)=sumtx(i)+tgmx(i,j)/an
            sumtm(i)=sumtm(i)+tgmn(i,j)/an
            sumrd(i)=sumrd(i)+radg(i,j)/an
            sumdr(i)=sumdr(i)+dur(i,j)
 20   continue
      if(iopt.eq.2) return
      if(iview.eq.0) go to 90
 30   write(*,850)
      read(*,*)i
      goto(40,50,60,70,80,90),i
 40   write(*,125)
      write(*,150)
      write(*,200)(j,(prcip(i,j),i=1,12),j=1,31)
      write(*,150)
      write(*,300)(sump(i),i=1,12)
      write(*,350)sump(13)
      go to 30
 50   write(*,400)
      write(*,150)
      write(*,450)(j,(tgmx(i,j),i=1,12),j=1,31)
      write(*,150)
      write(*,500)(sumtx(i),i=1,12)
      go to 30
 60   write(*,550)
      write(*,150)
      write(*,600)(j,(tgmn(i,j),i=1,12),j=1,31)
      write(*,150)
      write(*,500)(sumtm(i),i=1,12)
      go to 30
 70   write(*,650)
      write(*,150)
      write(*,700)(j,(radg(i,j),i=1,12),j=1,31)
      write(*,150)
      write(*,500)(sumrd(i),i=1,12)
      go to 30
 80   write(*,750)
      write(*,150)
      write(*,800)(j,(dur(i,j),i=1,12),j=1,31)
      write(*,150)
      write(*,300)(sumdr(i),i=1,12)
      go to 30
c
 90   continue
      return
c
 125  format(20x,'Daily Precipitation Amounts (Inches)'/)
 150  format(1x,'     J     F     M     A     M     J     J     A',
     1'     S     O     N     D')
 200  format(1x,i2,12f6.2)
 300  format(/1x,'tot',f5.2,11f6.2)
 350  format(/1x,'Annual Precipitation ',f6.2/)
 400  format(/20x,'Daily  Maximum Air Temperature (F)'/)
 450  format(1x,i2,12f6.0)
 500  format(/1x,'ave',f5.0,11f6.0/)
 550  format(/20x,'Daily Minimum Air Temperature (F)'/)
 600  format(1x,i2,12f6.0)
 650  format(/20x,'Daily Solar Radiation (Langleys) '/)
 700  format(1x,i2,12f6.0)
 750  format(/20x,'Storm Duration (Hours)'/)
 800  format(1x,i2,12f6.2)
 850  format(/1x,'Climate Data Viewing Options',/,1x,7('-'),
     11x,4('-'),1x,7('-'),1x,7('-'),/,1x,'1 - Precipitation',/,1x,
     1'2 - Maximum Air Temperature',/,1x,'3 - Minimum Air',
     1' Temperature',/,1x,'4 - Solar Radiation',/,1x,'5 - Storm',
     1' Duration',/,1x,'6 - End',//,1x,'Enter viewing option')
      end
c
c
c
      function dstg (rn1,ai,k7)
c
c     This function provides numbers from a gamma distribution, given
c     two random numbers.
      dimension k7(4)
c
      data xn1/10.0/
c
 10   dstg=rn1
      xx=rn1*ai
      rn=randn(k7)
      fu=xx**xn1*exp(xn1*(1.0-xx))
      rn1=rn
      if (fu.lt.rn) go to 10
      return
      end
c
c
c
      function dstn1 (rn1,rn2)
c
c     This function computes the distance from the mean of a
c     normal distribution with mean = 0 and standard deviation
c     = 1, given two random numbers.
c
      dstn1=sqrt(-2.0*alog(rn1))*cos(6.283185*rn2)
c
      return
      end
c
c
c
      function jdt (nc,i,m,nt)
c
c     This subroutine computes the day of the year, given the month and
c     the day of the month.
c
      dimension nc(13)
c
      if (m.gt.2) go to 10
      jdt=nc(m)+i
      go to 20
 10   jdt=nc(m)+nt+i
 20   return
      end
c
c
c
      subroutine jlt (mo,nday,jday,ntd)
c
      dimension nn(12)
      data nn/31,28,31,30,31,30,31,31,30,31,30,31/
c
      if(ntd.eq.366) then
        nn(2) = 29
      else
        nn(2) = 28
      endif
      kday = jday
      do 40 j = 1,12
         jday = jday -nn(j)
         if (jday) 30,30,40
 30      nday = jday +nn(j)
         go to 50
 40   continue
 50   jday = kday
      mo = j
      return
      end
c
c
c
      subroutine r5mon(tp6)
c     This subroutine Smothes and corrects .r hour monthly rainfall
c     intensity data.
      common /bk9/ hyd(250,5),xi(2,5),wi(12),tc(6),al(5),ab,ab1,rn1,r1,
     1             pr,lu1,vo,nmb,tp,tr,ddt,tt
      common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
     1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
     1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
     1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
     1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
c
      dimension sm(12),smm(12)
c
      xy2=0.5/tp6
      do 10 i=1,12
         wi(i)=wi(i)*25.4
 10   continue
      sm(1)=(wi(12)+wi(1)+wi(2))/3.0
      do 20 i=2,11
         sm(i)=(wi(i-1)+wi(i)+wi(i+1))/3.0
 20   continue
      sm(12)=(wi(11)+wi(12)+wi(1))/3.0
      do 30 i=1,12
         if(prw(1,i).eq.0.0) prw(1,i)=0.01
         if(prw(2,i).eq.0.0) prw(2,i)=0.01
         if(rst(i,1).eq.0.0) rst(i,1)=0.01
         xm=nc(i+1)-nc(i)
         smm(i)=xm*prw(1,i)/(1.0-prw(2,i)+prw(1,i))
         r25=rst(i,1)
         f=xy2/smm(i)
         wi(i)=-sm(i)/alog(f)
         wi(i)=1.0-exp(-wi(i)/r25)
         if (wi(i).lt.0.1) wi(i)=0.1
         if (wi(i).gt.0.95) wi(i)=0.95
 30   continue
      return
      end
c
c
c
      function randn (k)
c
c     This function provides random numbers ranging from 0. to 1.0
c
      dimension k(4)
c
      k(4)=3*k(4)+k(2)
      k(3)=3*k(3)+k(1)
      k(2)=3*k(2)
      k(1)=3*k(1)
      i=k(1)/1000
      k(1)=k(1)-i*1000
      k(2)=k(2)+i
      i=k(2)/100
      k(2)=k(2)-100*i
      k(3)=k(3)+i
      i=k(3)/1000
      k(3)=k(3)-i*1000
      k(4)=k(4)+i
      i=k(4)/100
      k(4)=k(4)-100*i
      randn=(((float(k(1))*.001+float(k(2)))*.01+float(k(3)))*.001+
     1      float(k(4)))*.01
      return
      end
      subroutine windg
c     this subroutine simulates daily average wind velocity using mean,
c     standard deviation, and skew coefficient of wind speed.
c     Wind direction is selected from a uniform distribution.
      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
      common /bk3/ alai(5),salb(5),dm(5),rd(5),uw,ep,es,rain,ws,
     1             je,j,ida
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
      common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
     1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
     1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
     1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
     1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)
      common /bk5/ r(366),vmx(5),v(5),flu(6),bp(5),hc(5),hcr(5),fp(5),
     1             cs(5),sk(5),smx(5),ev,sp,rl,q1,o,qd,af,pq,yp,aff,
     1             amps,cn,sml,cfp(5),swm(5),sfc(5),dk(5)
c      common /bk9/ hyd(250,5),xi(2,5),wi(12),tc(6),al1,ab,ab1,rn1,r1,
c     1             pr,lu1,vo,nmb,tp,tr,ddt,tt
      fx=randn(k6)
      do 1 j=1,16
         j1=j-1
         if (dir(mo,j).gt.fx) go to 2
    1 continue
c     calm condition found

      wv=0.0
      th=0.0
      return
c
c   Wind direction calculations

    2 if (j.eq.1) go to 3
      g=(fx-dir(mo,j1))/(dir(mo,j)-dir(mo,j1))
      go to 4
    3 g=fx/dir(mo,j)
    4 xj1=j1
      th=pi2*(g+xj1-.5)/16.
      if (th.lt.0.) th=pi2+th

c     Wind speed calculations

      v10=randn(k8)
      if(wvl(j,4,mo).eq.0.0) wvl(j,4,mo)=0.01
      r6=wvl(j,4,mo)/6.0
      xlv=(dstn1(v9,v10)-r6)*r6+1.0
      xlv=(xlv**3-1.0)*2.0/wvl(j,4,mo)
      v9=v10
      wv=xlv*wvl(j,3,mo)+wvl(j,2,mo)
      if (wv.lt.0.) wv=.1
c
      return
      end

       subroutine nrmd(r1)
c     This subroutine returns the standard normal deviate for a
c     given probability value (e.g. r1=.99 returns r1=2.328)
      if(r1 .ge. .5) go to 1
      sgn = -1.
      hp = r1
      go to 2
1     sgn = 1.
      hp = 1. - r1
2     t = sqrt(alog(1./(hp*hp)))
      r1 = sgn*(t-(2.30753+.27061*t)/(1.+.99229*t+.04481*t*t))
      return
      end

      subroutine header
c     Subroutine to write header information to screen.

      write(*,100)
 100  format(////,2x,68('*'),/,2x,'*',66x,'*',/,2x,'*',66x,'*',
     1       /,2x,'*',14x,'USDA - WATER EROSION PREDICTION',
     1       ' PROJECT',13x,'*',/,2x,'*',66x,'*',/,2x,'*',16x,
     1       ' WEPP CLIMATE INPUT DATA GENERATOR',16x,'*'/2x,'*',
     1       66x,'*',/,2x,'*',20x,'CONTINUOUS SIMULATION',
     1       ' AND',21x,'*',/,
     1       2x,'*',23x,'SINGLE STORM OPTIONS',23x,'*',/,
     1       2x,'*',66x,'*',/,
     1       2x,'*',66x,'*',/,2x,'*',27x,'VERSION  4.3',27x,'*',/,
     1       2x,'*',66x,'*',/,
     1       2x,'*',26x,'  July  1998  ',26x,'*',/,2x,'*',66x,'*',/,
     1       2x,'*',66x,'*',/,2x,68('*'),//)
      return
      end
      function timepk(timpkd,k10)
c     ****************************************************************
c     function to calculate the time to peak from the 12 interval time
c     to peak accummulated distribution parameters timpkd input for
c     each station location.
      dimension timpkd(0:12),k10(4)
      z=randn(k10)
      do 10 i=1,12
      int=i
      if(timpkd(i).ge.z) then
        int=i
        go to 20
      endif
10    continue
20       diff1= timpkd(int)-z
         diff2=timpkd(int) - timpkd(int-1)
         ratio= diff1/diff2
         timepk=0.08333*int - ratio*0.08333
      return
      end

