c     Program Cligen. WEPP Water Erosion Project Durant, OK.  Version 4.2
c     Please address inquiries to
c
c         WEPP Technical Support
c         USDA-ARS-NSERL
c         1196 Building SOIL
c         West Lafayette, IN 47907-1196
c         Phone 765 494-8673
c
c --------------------------------------------------------------------------
c
c     (Radically) Recoded by Charles R. Meyer   August - November 1999.
c     e-mail: meyerc@ecn.purdue.edu  (Same surface mail as above.)
c     Note that questionable lines contain the string 'XXX'.
c     MANY variable definitions provided by David Hall, USFS, Moscow, ID.
c
c              Structure of Recoded CLIGEN:
c      
c      Main---sta_dat---header
c           |         |-sta_name
c           |         |-sta_parms
c           |   
c           |-r5mon
c           |-*randn
c           |-usr_opt
c           |-sing_stm
c           |-wxr_gen---*jdt
c                     |-day_gen---jlt
c                     |         |-clgen---*dstn1
c                     |         |       |-*randn
c                     |         |
c                     |         |-windg---*dstn1
c                     |         |       |-*randn
c                     |         |
c                     |         |-alph---*dstg--*randn
c                     |         |-timepk---*randn
c                     | 
c                     |-opt_calc---clmout
c       
c       
c      * -- denotes function. 
c       
c      Note: NRMD does not seem to be used.
c       
c --------------------------------------------------------------------------
c      
c     Version 4.2 April 1997  West Lafayette, IN.
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
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      write: pi2
c     pi2         - Pi * 2; ie, a full rotation.
c
      include 'cbk4.inc'
c      read: iopt
c      write: nt
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output Filec                    8 - Exit Weather Generator Program
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
      include 'cbk7.inc'
c      read: prw,k1,k2,k3,k4,k5,k7,k8,k9
c      write: v1,v3,v5,v7,v9,v11,yls,ylc,pit,nsim,msim,l
c     prw(1,12)   - monthly probability of wet day after dry day
c     prw(2,12)   - monthly probability of wet day after wet day
c     k1 ... k9   - Seeds for random number generation.
c     v1 ... v11  - Random numbers used to generate various daily values.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     nsim        - ??? Has value zero or one.  Used as a Switch.
c     msim        - ??? Has value zero or one.  Used as a Switch.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c
c
      include 'cbk5.inc'
c      write: sml
c     sml         - Used to compute R1
c
      include 'cbk9.inc'
c      write: ab1,rn1
c      modify: wi,ab
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     ab          - Set to 0.02083, and used to calculate AI
c
c
c     + + + LOCAL VARIABLES + + +
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
      real smy(12),wgt(3),tymax(4),timpkd(0:12),tmpcmx(12),tmpcmn(12)
      integer elev,years,moveto
      character*1 yc
c     character*6 nstat
      character*41 stidd
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c     outfil      - Output (.cli) file name.
c     xx          - latitude / 57.296; ie, in radians
c     vv          - random deviate
c     nt          - 0 or 1 ("leap year?" for jdt) (iopt = 4, 7)
c     r5max       - "max of monthly maximum .5-hr rain"
c
c      Variables Passed to other Modules:
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     damt        - Design Storm Amount in Inches for Single Storm.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     jd          - Day of the Storm.
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c     index       - 4-digit numeric station index.
c     ioyr        - first # of "infile" (-> ibyear) (iopt 6)
c     itype       - integer value [1..4] to set single storm parameters.
c     iyear       - Beginning Simulation Year.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     ntd1        - julian date of jd, mo (iopt = 4, 7)
c     numyr       - number of years to simulate
c     smy         - Observed Monthly Average Precipitation (mm)
c     stidd       - 41-character alphanumeric station name.
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     tmpcmx      - Observed Monthly Average Max Temperature (C)
c     tmpcmn      - Observed Monthly Average Min Temperature (C)
c     tp6         - maximum 6 hour precipitation depth (inches).
c     tymax(4)    - upper limit of r5p (based on itype)
c     usdur       - [User Supplied] Storm Duration in Hours for Single Storm.
c     ustpr       - [User Supplied] Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     version     - CLIGEN version (ie, 4.2)
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     xm          - number of days in the month of interest
c     years       - Years of Record at the Station.
c     ylt         - Station Degrees Latitude (+ is N, - is S).
c     yll         - Station Degrees Longitude (+ is E, - is W).
c
c     + + + SUBROUTINES CALLED + + +
c     sta_dat
c     r5mon
c     usr_opt
c     sing_stm
c     wxr_gen
c
c     + + + FUNCTION DECLARATIONS + + +
      real randn
c
c     + + + DATA INITIALIZATIONS + + +
      data tymax/180.34,154.94,307.34,330.2/
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/22x,'Average Values for ',i2,' Years'/)
 2010 format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 2020 format(1x,'prcp   ',12f6.2)
 2030 format(1x,'tmax   ',12f6.2)
 2040 format(1x,'tmin   ',12f6.2)
 2050 format(1x,'rad    ',12f6.1)
 2060 format(1x,'dur    ',12f6.2)
 2070 format(/1x,'Average Annual Precipitation for ',i2,
     1' Years =',f6.2,a30/)
 2080 format(/1x,'Do you want to continue (y/n)? ')
c
c     + + + END SPECIFICATIONS + + +
c
      timpkd(0)=0.0
      moveto = 0
c     Version number set here for option 5 output header
      version=4.2
c
C ***************************************************************************
C ---- Determine the desired station and return its climate generation parms.
 10   continue
      moveto = 0
      call sta_dat(ylt,yll,years,elev,itype,tp6,wgt,index,moveto,
     1            stidd,timpkd,igcode)
      if(moveto.eq.10) goto 10
C ***************************************************************************
c
c     Begin Climate Generation
c
C **** L1 IF ****
      if(moveto.ne.230) then
        sml=0.0
        r5max=0.0
        do 120 i=1,12
          if(wi(i).ge.r5max) r5max=wi(i)
 120    continue
c
c -- XXX -- Huh??? -- CRM -- 9/14/99
C       do 125 i=1,12
C         wi(i)=wi(i)
C125    continue
c
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
c
c ---- Get Options from User
      call usr_opt(moveto,ioyr)
C **** L1 ENDIF ****
      endif
c
C **** M1 IF ****
      if(moveto.ne.230) then
c
        call sing_stm(ioyr,moveto,jd,iyear,damt,usdur,ustpr,uxmav,
     1            numyr,index)
        call wxr_gen(version,igcode,stidd,ylt,yll,years,elev,
     1            jd,itype,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
     2            iyear,numyr,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     3            sumpp,sumptx,sumptm,sumprd,sumpdr)
C **** M1 ENDIF ****
      endif
c
C **** N1 IF ****
      if(moveto.ne.10 .and. moveto.ne.230 .and. moveto.ne.225) then
        if(iopt.eq.2) then
          write(*,2000)numyr
          write(*,2010)
          write(*,2020)(sumpp(i),i=1,12)
          write(*,2030)(sumptx(i),i=1,12)
          write(*,2040)(sumptm(i),i=1,12)
          write(*,2050)(sumprd(i),i=1,12)
          write(*,2060)(sumpdr(i),i=1,12)
          write(*,2070)numyr,sumpp(13),stidd
        endif
C **** N1 ENDIF ****
      endif
c
c      End - MAIN LOOP
c
c    Check for Another Run or End
c
C **** P1 IF ****
      if(moveto.ne.10 .and. moveto.ne.230) then
        write(*,2080)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
	  moveto = 10
        else
          if(iopt.ge.4) then
            write(7,*) ' '
            close (7)
          else if(iopt.eq.3) then
            write(8,*)' '
            close (8)
          endif
        endif
C **** P1 ENDIF ****
      endif
c
      if(moveto.eq.10) goto 10
      stop 'Program terminated by user -  returning to DOS'
      end
c
c
c
      subroutine alph
c
c     + + + PURPOSE + + +
c     Computes alpha, a dimensionless parameter that expresses the fraction 
c     of total rainfall that occurs during 0.5 ho.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     ab          - Set to 0.02083, and used to calculate AI
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstg
c
c     + + + END SPECIFICATIONS + + +
c
      ei=r(ida)-sml
      ai=ab1/(wi(mo)-ab)
      if (ei.lt.1.0) then
        ajp=1.0
      else
        ajp=1.0-exp(-5.0/ei)
      endif
      r1=dstg(ai,k7,rn1)
      r1=(ei*(ab+r1*(ajp-ab))+sml*ab)/r(ida)
      return
      end
c
c
c
      block data
c
c     + + + PURPOSE + + +
c     Initialize variables in the Common Blocks.
c     Contains generator seeds for the weather generator.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
      include 'cbk7.inc'
c
c     + + + DATA INITIALIZATIONS + + +
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
c     + + + PURPOSE + + +
c     Simulates daily solar radiation, simulates daily precipitation
c     and/or maximum and minimum air temperature at the users option,
c     and calls functions RANDN and DSTN1.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: rh
c      modify: tdp
c     rh          - Avg Monthly Dew Point Temperature.  Used to calculate TDP.
c     tdp         - Generated dewpoint temperature (C).
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk7.inc'
c      read: prw,obmx,obmn,obsl,k1,k2,k3,k4,k5,k9,yls,ylc,pit,nsim,msim,
c            stdtx,stdtm
c      modify: rst,v1,v3,v5,v7,v11,ra,tmxg,tmng,rmx,l
c     prw(1,12)   - monthly probability of wet day after dry day
c     prw(2,12)   - monthly probability of wet day after wet day
c     obmx        - Maximum Temperature.
c     obmn        - Minimum Temperature.
c     k1 ... k9   - Seeds for random number generation.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     nsim        - ??? Has value zero or one.  Used as a Switch.
c     msim        - ??? Has value zero or one.  Used as a Switch.
c     stdtx       - Standard deviation of daily max. temp. for the month.
c     stdtm       - Standard deviation of daily min. temp. for the month.
c     rst(i,j)    - Array Of Monthly precipitation stats.
c     v1 ... v11  - Random numbers used to generate various daily values.
c     ra          - Generated radiation?  RADG receives RA's value for output.
c     tmxg        - Generated daily max temp.
c     tmng        - Generated daily min temp.
c     rmx         - Maximum possible solar radiation.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c
      include 'cbk5.inc'
c      modify: r
c     r           - Daily Precipitation amount (inches of water)
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstn1
      real randn
c
c     + + + END SPECIFICATIONS + + +
c
      xi=ida
      sd=0.4102*sin((xi-80.25)/pit)
      ch=-yls*tan(sd)/ylc
c
      if (ch.ge.1.0) then
        h=0.0
      else if (ch.le.-1.0) then
        h=3.1416
      else
        h=acos(ch)
      endif
c
      ys=yls*sin(sd)
C -- XXX -- Note that YC is also used to store a Y/N user response! 
C           CRM -- 10/21/99
      yc=ylc*cos(sd)
c ---- max possible solar radiation for this day of the year.
      rmx=711.0*(h*ys+yc*sin(h))
c
      if (nsim.ne.0) then
        vv=randn(k1)
        if ((prw(l,mo).le.0.0).or.(vv.gt.prw(l,mo))) then
          r(ida)=0.0
          l=2
	else
C -------- A Mutated variant of Equation 2.1.5
C -------- Generated Daily Precip
c          According to Larry M. Younkin, the amount of precip is
c          Assumed to follow a Pearson type III
c          distribution: 
c 
c                     2s  / /  g  /      g  \     \3     \
c          R = Rbar + --- | | --- | x - --- | + 1 |  - 1 |   
c                      g  \ \  6  \      6  /     /      / 
c 
          v8=randn(k5)
c --------- skewness
          if(rst(mo,3).eq.0.0) rst(mo,3)=0.01
          r6=rst(mo,3)/6.0
          xlv=(dstn1(v7,v8)-r6)*r6+1.0
          xlv=(xlv**3-1.0)*2.0/rst(mo,3)
          v7=v8
c --------- std. dev. & mean
          r(ida)=xlv*rst(mo,2)+rst(mo,1)
          if (r(ida).lt.0.01) r(ida)=0.01
          l=1
        endif
      endif
c
      if(msim.eq.0) then
        xx=1.
        v12=randn(k9)
        tdp=xx*dstn1(v11,v12)
        v11=v12
      else
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
c -------Equations 2.1.10 & 2.1.11
C ------ Generated Max & Min Daily Temps.
        tmxg=obmx(mo)+tmxg*stdtx(mo)
        tmng=obmn(mo)+tmng*stdtm(mo)
        if (tmng.gt.tmxg) tmng=tmxg-0.2*abs(tmxg)
      endif
c
c       TDP now calculated using standard dev. instead of CV. 3/95
C ---- Generated Daily Dew Point Temperature.
c -----A mutated version of Equation 2.1.14:
C -- XXX -- CRM -- 10/21/99 -- I think there is a mis-placed paren here.
C        I believe it should be as follows:
C     tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo))/2.)
      tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo)/2.))
      if (tdp.gt.((tmxg+tmng)/2.)) tdp=((tmxg+tmng)/2.)*0.99
c     Change to limit - dew point in cold months ADNe
      if(tdp.lt.-10.) tdp=1.1*tmng
      v6=randn(k4)
C ---- compute daily radiation.
      ra=xx*dstn1(v5,v6)
c -----A mutation of Equations 2.1.12 & 2.1.13: (Mis-placed parens, this time in WEPP User Doc ?)
      rx=rmx-obsl(mo)
      if (obsl(mo).gt.rx) rx=obsl(mo)
      ra=obsl(mo)+ra*rx/4.0
      if(ra.ge.rmx) ra=0.90*rmx
      if (ra.le.0.0) ra=0.05*rmx
      v5=v6
c
      return
      end
c
c
c
      subroutine clmout
     i           (iview)
c
c     + + + PURPOSE + + +
c     Calculates daily and monthly values for options 1 and 2.  Writes
c     output to screen for option 1.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iview
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iview       -
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: nc,iopt
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output Filec                    8 - Exit Weather Generator Program
c
      include 'ccl1.inc'
c      read: prcip,tgmx,tgmn,radg,dur
c     prcip       - ??? Avg. Daily Precip?  Set to R.  Divided by NUMYR and
c                    added to SUMP.
c     tgmx        - ??? Avg. Max Daily Temp?  Set to TMXG.  Divided by NUMYR 
c                    and added to SUMTX.
c     tgmn        - ??? Avg. Min Daily Temp?  Set to TMNG.  Divided by NUMYR 
c                    and added to SUMTM.
c     radg        - Daily Solar Radiation (Langleys/Day)
c     dur         - Storm Duration in Hours for Single Storm.
c
      include 'csumr.inc'
c      modify: sump,sumtx,sumtm,sumrd,sumdr
c     sump        - [Avg Annual] Sum of Daily Precip?
c     sumtx       - [Avg Annual] Sum of Daily Max Temps?
c     sumtm       - [Avg Annual] Sum of Daily Min Temps?
c     sumrd       - [Avg Annual] Sum of Daily Radiation?
c     sumdr       - Sum of Storm Durations?
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Climate Data Viewing Options',/,1x,7('-'),
     11x,4('-'),1x,7('-'),1x,7('-'),/,1x,'1 - Precipitation',/,1x,
     1'2 - Maximum Air Temperature',/,1x,'3 - Minimum Air',
     1' Temperature',/,1x,'4 - Solar Radiation',/,1x,'5 - Storm',
     1' Duration',/,1x,'6 - End',//,1x,'Enter viewing option')
 2010 format(20x,'Daily Precipitation Amounts (Inches)'/)
 2020 format(1x,'     J     F     M     A     M     J     J     A',
     1'     S     O     N     D')
 2030 format(1x,i2,12f6.2)
 2040 format(/1x,'tot',f5.2,11f6.2)
 2050 format(/1x,'Annual Precipitation ',f6.2/)
 2060 format(/20x,'Daily  Maximum Air Temperature (F)'/)
 2070 format(1x,i2,12f6.0)
 2080 format(/1x,'ave',f5.0,11f6.0/)
 2090 format(/20x,'Daily Minimum Air Temperature (F)'/)
 2100 format(1x,i2,12f6.0)
 2110 format(/20x,'Daily Solar Radiation (Langleys) '/)
 2120 format(1x,i2,12f6.0)
 2130 format(/20x,'Storm Duration (Hours)'/)
 2140 format(1x,i2,12f6.2)
c
c     + + + END SPECIFICATIONS + + +
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
c
      if(iopt.ne.2 .and. iview.ne.0) then
 30     continue
          write(*,2000)
          read(*,*)i
          if(i.eq.1) then
            write(*,2010)
            write(*,2020)
            write(*,2030)(j,(prcip(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2040)(sump(i),i=1,12)
            write(*,2050)sump(13)
          elseif(i.eq.2) then
            write(*,2060)
            write(*,2020)
            write(*,2070)(j,(tgmx(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumtx(i),i=1,12)
          elseif(i.eq.3) then
            write(*,2090)
            write(*,2020)
            write(*,2100)(j,(tgmn(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumtm(i),i=1,12)
          elseif(i.eq.4) then
            write(*,2110)
            write(*,2020)
            write(*,2120)(j,(radg(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumrd(i),i=1,12)
          elseif(i.eq.5) then
            write(*,2130)
            write(*,2020)
            write(*,2140)(j,(dur(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2040)(sumdr(i),i=1,12)
          endif
C -- XXX -- How does one EXIT this loop? -- CRM -- 10/18/99
C           Replaced statement below:       CRM -- 11/10/99
C       goto 30
        if(i.gt.0 .and. i.lt.6) goto 30
      endif
c
 90   continue
      return
      end
c
c
c
      real function dstg
     i         (ai,k7,
     m          rn1)
c
c     + + + PURPOSE + + +
c     Provides numbers from a gamma distribution, given two random numbers.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real ai,rn1
      integer k7(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ai          -
c     k7          - Seed for random number generation.
c     rn1         - ??? -- used for precip gamma dsn
c
c     + + + DATA INITIALIZATIONS + + +
      data xn1/10.0/
c
c     + + + FUNCTION DECLARATIONS + + +
      real randn
c
c     + + + VARIABLE DECLARATIONS + + +
c -- XXX -- Added 1/5/2000 -- C. R. Meyer:
      double precision xx,fu
c
c     + + + END SPECIFICATIONS + + +
c
 10   dstg=rn1
        xx=rn1*ai
        rn=randn(k7)
        fu=xx**xn1*exp(xn1*(1.0-xx))
        rn1=rn
      if (fu.lt.rn) goto 10
c
      return
      end
c
c
c
      real function dstn1 
     i         (rn1,rn2)
c
c     + + + PURPOSE + + +
c     Computes the distance from the mean of a normal distribution,
c     with mean = 0 and standard deviation = 1, given two random numbers.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real rn1,rn2
c
c     + + + ARGUMENT DEFINITIONS + + +
c     rn1         - ??? -- used for precip gamma dsn
c     rn2         -
c
c     + + + END SPECIFICATIONS + + +
c
      dstn1=sqrt(-2.0*alog(rn1))*cos(6.283185*rn2)
c
      return
      end
c
c
c
      integer function jdt 
     i         (nc,i,m,nt)
c
c     + + + PURPOSE + + +
c     Computes the (Julian) day of the year, given the month and the day
c     of the month.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nc(13),i,m,nt
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     i           - Day of the Storm ("jd").
c     m           - The current month (1=Jan, 2=Feb...)("mo").
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
c     + + + END SPECIFICATIONS + + +
c
      if (m.gt.2) then
        jdt=nc(m)+nt+i
      else
        jdt=nc(m)+i
      endif
c
      return
      end
c
c
c
      subroutine jlt 
     i           (ntd,
     m            jday,
     o            mo,nday)
c
c     + + + PURPOSE + + +
c     Given the day of the year (Julian date) determine the month
c     and day of the month.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ntd,jday,mo,nday
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ntd         - number of days in year (365; 366 if leap year and
c                    iopt=3, 5, 6)
c     jday        - Julian date; ie, day of the year.
c     mo          - The current month (1=Jan, 2=Feb...).
c     nday        - The day of the current month.
c
c     + + + LOCAL VARIABLES + + +
      integer nn(12)
c
c     + + + LOCAL DEFINITIONS + + +
c     nn          - Number of days in each month (non-leap year).
c
c     + + + DATA INITIALIZATIONS + + +
      data nn/31,28,31,30,31,30,31,31,30,31,30,31/
c
c     + + + END SPECIFICATIONS + + +
c
c ---- Adjust for Leap Year.
      if(ntd.eq.366) then
        nn(2) = 29
      else
        nn(2) = 28
      endif
      kday = jday
c
c ---- Find the month.
      j = 0
 10   continue
        j = j + 1
        jday = jday -nn(j)
        ndflag = 0
        if (jday.le.0) then
          nday = jday +nn(j)
          ndflag = 50
        endif
      if(ndflag.eq.0 .and. j.lt.12) goto 10
c
      jday = kday
      mo = j
c
      return
      end
c
c
c
      subroutine r5mon
     i           (tp6)
c
c     + + + PURPOSE + + +
c     Smoothes and corrects .r hour monthly rainfall intensity data.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real tp6
c
c     + + + ARGUMENT DEFINITIONS + + +
c     tp6         - maximum 6 hour precipitation depth (inches).
c
c     + + + COMMON BLOCKS + + +
      include 'cbk9.inc'
c      modify: wi
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c
      include 'cbk7.inc'
c      modify: rst,prw 
c     rst(i,j)    - Array Of Monthly precipitation stats.
c     prw(1,12)   - monthly probability of wet day after dry day
c     prw(2,12)   - monthly probability of wet day after wet day
c
      include 'cbk4.inc'
c      read: nc
c     nc          - Number of days in the (non-leap) year preceeding each month.
c
c     + + + END SPECIFICATIONS + + +
c
      real sm(12),smm(12)
c
      xy2=0.5/tp6
c ---- convert WI in inches to WI in mm.
      do 10 i=1,12
         wi(i)=wi(i)*25.4
 10   continue
c
c ---- average each month's WI with the month on either side.
      sm(1)=(wi(12)+wi(1)+wi(2))/3.0
      do 20 i=2,11
         sm(i)=(wi(i-1)+wi(i)+wi(i+1))/3.0
 20   continue
      sm(12)=(wi(11)+wi(12)+wi(1))/3.0
c
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
c
      return
      end
c
c
c
      real function randn
     m         (k)
c
c     + + + PURPOSE + + +
c     Provides random numbers ranging from 0.0 to 1.0
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer k(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     k           - Seed for random number generation.
c
c     + + + END SPECIFICATIONS + + +
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
c
      return
      end
c
c
c
      subroutine windg
c
c     + + + PURPOSE + + +
c     Simulates daily average wind velocity using mean, standard deviation,
c     and skew coefficient of wind speed.  Wind direction is selected from
c     a uniform distribution.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: dir,pi2
c      modify: wvl,wv,th
c     dir(i,j)    - Cumulative % time (fraction) from dir1, dir1+dir2, ...
c                          dim1: month
c                          dim2: compass direction
c     pi2         - Pi * 2; ie, a full rotation.
c     wvl(i,j,k)  - array of wind paramters where:
c                     i - ith direction (1 - north  - 16 nnw)
c                     j - parameters (1 - 4)
c                          1 - % time from direction i
c                          2 - mean speed from direction i
c                          3 - standard deviation of speed from direction i
c                          4 - skew coeficient of speed from direction i
c                     k - month (1=Jan, 2=Feb...)
c     wv          - Wind Velocity (m/sec)
c     th          - Wind Direction (radians from North)
c
      include 'cbk3.inc'
C -- XXX -- Note that 'j' is a counter in a loop! -- CRM 10/18/99
c      modify: j
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk7.inc'
c      read: k6,k8
c      modify: v9
c     k1 ... k9   - Seeds for random number generation.
c     v1 ... v11  - Random numbers used to generate various daily values.
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstn1
      real randn
c
c     + + + END SPECIFICATIONS + + +
c
      fx=randn(k6)
      j = 0
 1    continue
        j = j + 1
        j1=j-1
        ndflag = 0
        if (dir(mo,j).gt.fx) ndflag = 2
      if(ndflag.eq.0 .and. j.lt.16) goto 1
c
c       calm condition found
c
c
C **** L1 IF ****
      if(ndflag.eq.0) then
        wv=0.0
        th=0.0
C **** L1 ELSE ****
      else 
c
c       Wind direction calculations
c
        if (j.eq.1) then
          g=fx/dir(mo,j)
        else
          g=(fx-dir(mo,j1))/(dir(mo,j)-dir(mo,j1))
        endif
        xj1=j1
        th=pi2*(g+xj1-.5)/16.
        if (th.lt.0.) th=pi2+th
c
c         Wind speed calculations
c
        v10=randn(k8)
        if(wvl(j,4,mo).eq.0.0) wvl(j,4,mo)=0.01
        r6=wvl(j,4,mo)/6.0
        xlv=(dstn1(v9,v10)-r6)*r6+1.0
        xlv=(xlv**3-1.0)*2.0/wvl(j,4,mo)
        v9=v10
        wv=xlv*wvl(j,3,mo)+wvl(j,2,mo)
        if (wv.lt.0.) wv=.1
C **** L1 ENDIF ****
      endif
c
      return
      end
c
c
c
      subroutine nrmd
     m           (r1)
c
c     + + + PURPOSE + + +
c     Returns the standard normal deviate for a given probability value
c     (e.g. r1=.99 returns r1=2.328)
c
c     + + + ARGUMENT DECLARATIONS + + +
      real r1
c
c     + + + ARGUMENT DEFINITIONS + + +
c     r1          - initially, the probability; finally the std. norm. deviate.
c
c     + + + END SPECIFICATIONS + + +
c
      if(r1 .ge. 0.5) then
        sgn = 1.0
        hp = 1.0 - r1
      else
        sgn = -1.0
        hp = r1
      endif 
      t = sqrt(alog(1./(hp*hp)))
      r1 = sgn*(t-(2.30753+.27061*t)/(1.+.99229*t+.04481*t*t))
      return
      end
c
c
c
      subroutine header
c
c     + + + PURPOSE + + +
c     Writes header information to screen.
c
c     + + + OUTPUT FORMATS + + +
 2000 format(////,2x,68('*'),/,2x,'*',66x,'*',/,2x,'*',66x,'*',
     1       /,2x,'*',14x,'USDA - WATER EROSION PREDICTION',
     1       ' PROJECT',13x,'*',/,2x,'*',66x,'*',/,2x,'*',16x,
     1       ' WEPP CLIMATE INPUT DATA GENERATOR',16x,'*'/2x,'*',
     1       66x,'*',/,2x,'*',20x,'CONTINUOUS SIMULATION',
     1       ' AND',21x,'*',/,
     1       2x,'*',23x,'SINGLE STORM OPTIONS',23x,'*',/,
     1       2x,'*',66x,'*',/,
     1       2x,'*',66x,'*',/,2x,'*',21x,'Recoded from VERSION 4.2',
     1       21x,'*',/,
     1       2x,'*',66x,'*',/,
c    1       2x,'*',26x,'  April 1997  ',26x,'*',/,2x,'*',66x,'*',/,
     1       2x,'*',26x,' October 1999 ',26x,'*',/,2x,'*',66x,'*',/,
     1       2x,'*',66x,'*',/,2x,68('*'),//)
c
c     + + + END SPECIFICATIONS + + +
c
      write(*,2000)
      return
      end
c
c
c
      real function timepk
     i         (timpkd,k10)
c
c     + + + PURPOSE + + +
c     Calculates the time to peak from the 12 interval time to peak
c     accummulated distribution parameters TIMPKD input for each station
c     location.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real timpkd(0:12)
      integer k10(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     k10         - Seed for random number generation.
c
c     + + + FUNCTION DECLARATIONS + + +
CC   real randn
c
c     + + + END SPECIFICATIONS + + +
c
      z=randn(k10)
      i=0
 10   continue
        i=i+1
      if(timpkd(i).lt.z .and. i.lt.12) goto 10
c
      diff1= timpkd(i)-z
      diff2=timpkd(i) - timpkd(i-1)
      ratio= diff1/diff2
      timepk=0.08333*i - ratio*0.08333
c
      return
      end
c
c
c
      subroutine sta_dat
     i           (ylt,yll,years,elev,itype,tp6,wgt,
     m            index,moveto,
     o            stidd,timpkd,igcode)
c     + + + PURPOSE + + +
c     To determine desired station and load the parameters required to
c     generate its weather.
c
c ----- Split out from the CLIGEN main module 9/22-27/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real ylt,yll,wgt(3),timpkd(0:12)
C--- XXX -- Huh?  ELEV is declared to be an *integer*, but in the
C           data file it is a floating-point!!!  --- CRM -- 9/27/99
      integer years,elev,index,moveto
      character*41 stidd
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ylt         - Station Degrees Latitude (+ is N, - is S).
c     yll         - Station Degrees Longitude (+ is E, - is W).
c     years       - Years of Record at Station.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     itype       - integer value [1..4] to set single storm parameters.
c     tp6         - maximum 6 hour precipitation depth (inches).
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     index       - 4-digit numeric station index.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     stidd       - 41-character alphanumeric station name.
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c
c     + + + LOCAL VARIABLES + + +
      integer iscnt,nst,nstat,ndflag
      character*41 stid
      character*2 dr(55)
      character*1 yc
c
c     + + + LOCAL DEFINITIONS + + +
c      Variables Passed to other Modules:
c     istate      - Numeric Climate Code of Desired State.
c     iscnt       - Number of Stations in Selected State.
c     stid        - 41-character ASCII Station Name.
c     nstat       - 4-digit Numeric Station Code.
c
c     nst         - Numeric State Code
c     index       - 4-digit numeric station index.
c     ndflag      - "Set" if an "End-of-File" is read.
c     dr          - Array of 2-character state (postal) abbreviations.
c     yc          - 1-character user response (y/n).
c     dr          - Array of 2-character state (postal) abbreviations.
c                    File by that name contains parameters for all the
c                    weather stations in that state for which we can
c                    generate weather.
c     version     - CLIGEN version (ie, 4.2)
c
c     + + + SUBROUTINES CALLED + + +
c     header
c     sta_name
c     sta_parms
c
c     + + + DATA INITIALIZATIONS + + +
      data dr/'al','az','ar','ca','co','ct','de','fl','ga','id',
     1        'il','in','ia','ks','ky','la','me','md','ma','mi',
     1        'mn','ms','mo','mt','ne','nv','nh','nj','nm','ny',
     1        'nc','nd','oh','ok','or','pa','ri','sc','sd','tn',
     1        'tx','ut','vt','va','wa','wv','wi','wy','dc','ak',
     1        'hi','pr','pi','if','  '/
c
c     + + + INPUT FORMATS + + +
 1000 format(i4)
 1010 format(a41,i2,i4,i2)
 1020 format(//////////////////////////////////////////////////
     1       ///////////////////////////////)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'No stations available - do you want to',
     1' continue (y/n)? ')
 2010 format(/1x,'Enter the station index: ')
c
c
c     + + + END SPECIFICATIONS + + +
c
      if(moveto.ne.10) then
        timpkd(0)=0.0
c ------ Version number set here for option 5 output header
C       version=4.2
c
        call header
        write(*,*)' Press Enter to Continue'
        read(*,'(a1)')yc
      endif
C
C ***************************************************************************
C ---- Determine Desired Station
 10   continue
      call sta_name(istate,iscnt,stid,nstat,moveto)
C ***************************************************************************
C
 40   continue
      if(moveto.eq.40 .or. (iscnt.le.0 .and. moveto .ne. 50)) then
C ---- No Stations Found
        write(*,2000)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          moveto = 10
        else
          moveto = 230
        endif
      endif
c
 50   continue
C **** L1 IF ****
      ndflag = 0
      if(moveto.ne.10 .and. moveto.ne.230) then
        write(*,2010)
        iname = istate
        if(iname.eq.66) iname=52
        if(iname.eq.91) iname=53
        if(iname.eq.99) iname=54
        read(*,1000)index
        ndflag = 55
        open(10,file=dr(iname),status='old',err=55)
        rewind (10)
        ndflag = 0
 55     continue
        if(ndflag .ne. 0) then
          write(*,*)'  Mother Nature does not want to run state ',
     1                 dr(iname)
          write(*,*)'  Parameters for this state are not loaded on '
          write(*,*)'  this computer.  Check directory for file ',
     1                 dr(iname)
          write(*,*)'  -- Enter q to quit or c to Continue'
          read(*,'(a1)')yc
          if(yc.eq.'q'.or.yc.eq.'Q') then
            moveto = 230
          else
            moveto = 10
          endif
        endif
c
C ****** L2 IF ****
        if(moveto.ne.10 .and. moveto.ne.230) then
 60       continue
c        Gettin Station from State File
          ndflag = 40
          read(10,1010,end=61)stidd,nst,nstat,igcode
C ------ Match 2-digit state code & 4-digit station code.
C ------ If they don't match, blow past 81 records.
          ndflag = 60
          if((nst.ne.istate).or.(nstat.ne.index)) then
            read(10,1020)
            ndflag = 0
          endif
 61       continue
          if(ndflag.eq.0) goto 60
C         if(ndflag.eq.40) goto 40
c
          if(ndflag.ne.40)
     1    call sta_parms(stidd,nstat,tp6,wgt,ylt,yll,years,elev,itype,
     2                   timpkd)
c
C ****** L2 ENDIF ****
        endif
C **** L1 ENDIF ****
      endif
      if(ndflag.eq.40) goto 40
c
 230  continue
      return
      end
c
c
      subroutine sta_name
     m           (istate,iscnt,stid,nstat,moveto)
c
c     + + + PURPOSE + + +
c     To determine desired climate station.
c
c ----- Split out from the STA_DAT module 9/27/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer istate,iscnt,nstat,moveto
      character*41 stid
c
c     + + + ARGUMENT DEFINITIONS + + +
c     istate      - Numeric Climate Code of Desired State.
c     iscnt       - Number of Stations in Selected State.
c     stid        - ASCII Station Name.
c     nstat       - 4-digit Numeric Station Code.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
c     + + + LOCAL VARIABLES + + +
      integer ndflag
C -- XXX -- COUNTY is supposed to be read & written, but space is never
C           allocated to _store_ it!  Added stmt below.  CRM -- 9/27/99.
      character*20 county
      character*1 yc
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c
c     + + + INPUT FORMATS + + +
 1000 format(i2)
 1010 format(a41,i2)
 1020 format(a41,i2,i4,i3,i4,a20)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(1x,'State Climate Code - Available Stations'/)
 2010 format(1x,'01 Alabama       20 Michigan       39 S. Dakota',
     1/1x,'02 Arizona       21 Minnesota      40 Tennessee',
     2/1x,'03 Arkansas      22 Mississippi    41 Texas',
     3/1x,'04 California    23 Missouri       42 Utah',
     4/1x,'05 Colorado      24 Montana        43 Vermont',
     5/1x,'06 Connecticut   25 Nebraska       44 Virginia',
     6/1x,'07 Delaware      26 Nevada         45 Washington',
     7/1x,'08 Florida       27 New Hampshire  46 West Virginia',
     8/1x,'09 Georgia       28 New Jersey     47 Wisconsin',
     9/1x,'10 Idaho         29 New Mexico     48 Wyoming')
 2020 format(1x,'11 Illinois      30 New York       49 Washington DC',
     1/1x,'12 Indiana       31 North Carolina 50 Alaska',
     2/1x,'13 Iowa          32 North Dakota   51 Hawaii',
     3/1x,'14 Kansas        33 Ohio           66 Puerto Rico',
     4/1x,'15 Kentucky      34 Oklahoma       91 Pacific Islands',
     5/1x,'16 Louisiana     35 Oregon         99 Interpolated File',
     6/1x,'17 Maine         36 Pennsylvania   ',
     7/1x,'18 Maryland      37 Rhode Island    ',
     8/1x,'19 Massachusetts 38 South Carolina  ')
 2030 format(/1x,'Enter state climate code (ex. 01 for Alabama): ')
 2040 format(/1x,'Stations Available',19x,' Station No. ',
     1       'Lat.  Long County'/,
     11x,8('-'),1x,9('-'),19x,17('-'),2x,4('-'),1x,7('-')/)
 2050 format(1x,a41,2x,i4,3x,i3,2x,i4,1x,a19)
c
c     + + + END SPECIFICATIONS + + +
c
C ---- Determine Desired State
 10   continue
      write(*,2000)
      write(*,2010)
      write(*,2020)
      write(*,2030)
      ndflag = 15 
      read(*,1000,err=15)istate
      ndflag = 0
 15   continue 
      if(ndflag .eq. 15) then 
        write(*,*)' Error Entering State Code '
        write(*,*)
        write(*,*)' Press Enter to Continue'
        read(*,'(a1)')yc 
      endif 
      if(ndflag .ne. 0) goto 10 
C
C ---- Interpolated File
      moveto = 0
      if(istate.eq.99) then
        iscnt = 1
C       iyr = 0
        moveto = 50
      end if
C
      if(moveto.ne.50) then
c
c ------ Read List of Available Stations
c -- istate -- Numeric Climate Code of Desired State
c -- kknt   -- Lines Displayed on Screen so far
c -- nstate -- Numeric State Code
c -- nst    -- Numeric State Code (same)
c
        open(11,file='stations',status='old')
        rewind (11)
        iscnt=0
C       iyr=0
        write(*,2040)
        kknt=0
 20     continue
        ndflag =29 
        read(11,1010,end=29)stid,nstate
        if(nstate.eq.istate) then
          kknt=kknt+1
          iscnt=iscnt+1
          backspace(11)
          read(11,1020)stid,nst,nstat,lat,long,county
          if(kknt.gt.20) then
            kknt=0
            write(*,*) 'Press "ENTER" to continue.'
            read(*,'(a1)') yc
          endif
          write(*,2050)stid,nstat,lat,long,county
        endif
        ndflag = 0
 29     continue
        if(ndflag.eq.0 .and. nstate.le.istate) goto 20
      endif
c
      close (11)
C     sta_name = moveto
      return
      end
c
c
c
      subroutine sta_parms
     i           (stidd,nstat,
     m            tp6,wgt,
     o            ylt,yll,years,elev,itype,timpkd)
c     + + + PURPOSE + + +
c     Derive parameters for the desired climate station.
c
c ----- Split out from the CLIGEN main module 9/28/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      character*41 stidd
      integer nstat,elev,years
      real ylt,yll,tp6,wgt(3),timpkd(0:12)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     stidd       - 41-character alphanumeric station name.
c     nstat       - 4-digit Numeric Station Code
c     tp6         - maximum 6 hour precipitation depth (inches).
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     ylt         - Station Latitude.
c     yll         - Station Longitude.
c     years       - Years of Record.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     itype       - integer value [1..4] to set single storm parameters.
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      modify: wvl,dir,rh,calm
c     wvl(i,j,k)  - array of wind paramters where:
c                     i - ith direction (1 - north  - 16 nnw)
c                     j - parameters (1 - 4)
c                          1 - % time from direction i
c                          2 - mean speed from direction i
c                          3 - standard deviation of speed from direction i
c                          4 - skew coeficient of speed from direction i
c                     k - month (1=Jan, 2=Feb...)
c
c     dir(i,j)    - Cumulative % time (fraction) from dir1, dir1+dir2, ...
c                          derived from wvl()
c                          dim1: month
c                          dim2: compass direction
c     rh          - Dew Point Temp.
c     calm        - % time air is calm (by month).
c                   Calm is treated separately [from WVL] as direction 0,
c                   speed 0.  Only a % time value is need for calm generation.
c
      include 'cbk7.inc'
c      modify: rst,prw,obmx,obmn,obsl,cvs,cvtx,cvtm,stdtx,stdtm,stdsl
c     rst(i,j)    - Array Of Monthly precipitation stats.
c     prw(1,12)   - monthly probability of wet day after dry day
c     prw(2,12)   - monthly probability of wet day after wet day
c     obmx        - Maximum Temperature.
c     obmn        - Minimum Temperature.
c     obsl        - Observed mean daily solar radiation (Langleys) (by month)
c     cvs         - Coefficient of Variation of Solar Radiation (by month)
c     cvtx        - Coefficient of Variation of Maximum Temperature (by month)
c     cvtm        - Coefficient of Variation of Minimum Temperature (by month)
c     stdmx       - Standard Dev. Tmax.
c     stdmn       - Standard Dev. Tmin.
c     stdsl       - Standard Dev. Sol.
c
      include 'cbk9.inc'
c      modify: wi
c     wi          - Average Maximum .5 Hour Precip.
c
c     + + + LOCAL VARIABLES + + +
      character*1 yc
      character*19 site(3)
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c     site(3)     - 3 wind station names used for triangulation.  Stations
c                   from which wind data were interpolated.  (Not used in
c                   CLIGEN computations, but reported in the CLIGEN output.)
c
c     + + + INPUT FORMATS + + +
 1000 format(6x,f7.2,6x,f7.2,7x,i3,7x,i2/12x,i5,17x,f5.2)
 1010 format(8x,12f6.2)
 1020 format(8x,12f6.3)
 1030 format(a19,f6.3,2(2x,a19,f6.3))
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Do you want to view data found for station',//,
     1 a41,2x,i4,' (y/n)?: ')
 2010 format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 2020 format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 2030 format(/1x,'wet-dry state probabilities'/)
 2040 format(1x,2f10.5)
 2050 format(/1x,'mean,st.dev.,and skew coef. of daily rainfall'/)
 2060 format(1x,3f10.5)
 2070 format(/1x,'standard deviation for max and min temp,',
     1          ' and solar radiation'/)
 2080 format(1x,12f6.2)
 2090 format(1x,12f6.1)
 2100 format(/1x,'coefficient of variation for max, min temp,',
     1          ' solar radiation, and max .5 hr rain'/)
 2110 format(1x,'Average Monthly Dew Point Temperature',/1x,12f6.2)
 2120 format(1x,'Wind Data Interpolated from',
     1           /1x,a19,f6.3,2(2x,a19,f6.3))
 2130 format(1x,12f6.2)
c
c     + + + END SPECIFICATIONS + + +
c
c     Read Precipitation, Temperature, Radiation, etc.
c
      read(10,1000)ylt,yll,years,itype,elev,tp6
      read(10,1010)(rst(i,1),i=1,12),(rst(i,2),i=1,12),(rst(i,3),
     1             i=1,12)
      read(10,1010)(prw(1,i),i=1,12),(prw(2,i),i=1,12)
      read(10,1010)(obmx(i),i=1,12)
      read(10,1010)(obmn(i),i=1,12)
      read(10,1010)(stdtx(i),i=1,12),(stdtm(i),i=1,12)
      read(10,1010)(obsl(i),i=1,12)
      read(10,1010)(stdsl(i),i=1,12)
      read(10,1010)(wi(i),i=1,12)
      read(10,1010)(rh(i),i=1,12)
      read(10,1020)(timpkd(i),i=1,12)
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
c
      read(10,1010)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
      read(10,1010)(calm(i),i=1,12)
      read(10,1030)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
C--- XXX -- Huh?  ELEV is declared to be an *integer*, but in the
C           data file it is a floating-point!!!  --- CRM -- 9/27/99
      elev=elev*.3048
c
c
      do 70 i=1,12
c -- XXX -- Huh??? -- CRM -- 9/14/99
C       wi(i)=wi(i)
        cvtx(i)=stdtx(i)/obmx(i)
        cvtm(i)=stdtm(i)/obmn(i)
        if(obsl(i).le.0.0) then
          cvs(i)=0.0
        else
          cvs(i)=stdsl(i)/obsl(i)
        endif
 70   continue
c
c -- XXX -- Huh??? -- CRM -- 9/14/99
C     do 80 i=1,12
C       wi(i)=wi(i)
c
      do 6000 i=1,12
6000  dir(i,1)=wvl(1,1,i)
c
      do 7010 i=1,12
        do 7000 j=2,16
          dir(i,j) = dir(i,j-1)+wvl(j,1,i)
7000    continue
        j=17
        dir(i,j)=100.0
7010  continue
c
      do 7050 i=1,12
        do 7049 j=1,17
          dir(i,j)=dir(i,j)*.01
7049    continue
7050  continue
c
      close (10)
c       Close the Parameter File and Write the Parameter if He Wants.
c
      write(*,2000)stidd,nstat
      read(*,'(a1)')yc
C
C **** L1 IF ****
      if(yc.ne.'n'.and.yc.ne.'N') then
        write(*,2010)obmx,obmn
        write(*,2020)obsl
        write(*,2030)
        do 90 i=1,12
          write(*,2040) prw(1,i),prw(2,i)
 90     continue
        write(*,2050)
        do 100 i=1,12
          write(*,2060)(rst(i,j),j=1,3)
 100    continue
        write(*,2070)
        write(*,2080)(stdtx(i),i=1,12)
        write(*,2080)(stdtm(i),i=1,12)
        write(*,2090)(stdsl(i),i=1,12)
        write(*,2100)
        write(*,2080)(cvtx(i),i=1,12)
        write(*,2080)(cvtm(i),i=1,12)
        write(*,2080)(cvs(i),i=1,12)
        write(*,2080)(wi(i),i=1,12)
        write(*,2110)rh
        write(*,2080)tp6
        write(*,2120)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
        write(*,2130)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
        write(*,2130)(calm(i),i=1,12)
C **** L1 ENDIF ****
      endif
      return
      end
c
c
c
      subroutine day_gen
     i           (nbt,jd,iyear,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
     i            itype,ntd1,
     m            ntd,moveto)
c     + + + PURPOSE + + +
c     To generate the Daily Outputs for CLIGEN.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nbt,jd,iyear,itype,ntd1,ntd,moveto
      real tymax(4),timpkd(0:12),usdur,damt,ustpr,uxmav
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nbt         - Julian day of the year.
c     jd          - Day of the Storm.
c     iyear       - Beginning Simulation Year.
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     tymax(4)    - upper limit of r5p (based on itype)
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     usdur       - Storm Duration in Hours for Single Storm.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     itype       - integer value [1..4] to set single storm parameters.
c     ntd         - Days in this year (?); ie, 365 or 366.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: wv
c      modify: th,tdp
c
      include 'cbk3.inc'
c      modify: ida
c
      include 'cbk4.inc'
c      read: mo,iopt,dtp
c
      include 'cbk7.inc'
c      read: k10,ra
c      write: nsim,msim
c      modify: tmxg,tmng
c
      include 'cbk5.inc'
c      modify: r
c
      include 'cbk9.inc'
c      read: r1 
c
      include 'ccl1.inc'
c      write: prcip,tgmx,tgmn
c      modify: radg,dur
c
c     + + + LOCAL VARIABLES + + +
c     tpr         - ratio of (time to rainfall peak)/(rainfall duration)
c     xmav        - ratio of (max rainfall intensity)/(avg rainfall intensity)
c     r5p         - apparently: max rainfall intensity; peak rainfall rate 
c                    (mm/h).  (Yu's r_p): instantaneous peak intensity (mm/h)
c
c
c     + + + SUBROUTINES CALLED + + +
c     jlt
c     clgen
c     windg
c     alph
c     timepk
c
c     + + + INPUT FORMATS + + +
 1000 format(15x,3i5)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(2i3,1x,i4,1x,f5.1,1x,f5.2,1x,f4.2,1x,f6.2,2(1x,f5.1),
     1       1x,f4.0,1x,f4.1,2x,f4.0,1x,f5.1)
c
c     + + + END SPECIFICATIONS + + +
c
c
c      Start of the Daily Generation Loop  with nbt and ntd
c
      if(iopt.eq.4.or.iopt.eq.7) ntd=ntd1
      ida=nbt
 180  continue
        if(iopt.eq.6) then
          msim=0
          nsim=0
          moveto = 225
          read(9,1000,end=199)irida,itmxg,itmng
          moveto = 0
 199      continue
          if(moveto .eq. 0) then
            if(irida.eq.9999) nsim=1
            if(itmxg.eq.9999) msim=1
            if(itmng.eq.9999) msim=1
            r(ida)=irida*.01
            tmxg=itmxg
            tmng=itmng
          endif
        endif
C ****** L1 ENDIF ****
        if(moveto .eq. 0) then
          idr=ida
          call jlt(ntd,idr,mo,jd)
          call clgen
          call windg
          th=th*clt
          prcip(mo,jd)=r(ida)
c -------- generated max & min daily temp (F)
          tgmx(mo,jd)=tmxg
          tgmn(mo,jd)=tmng
c -------- convert generated max & min daily temp from F to C.
          tmxg=(tmxg-32.0)*(5.0/9.0)
          tmng=(tmng-32.0)*(5.0/9.0)
          tdp = (tdp-32.0)*(5.0/9.0)
          radg(mo,jd)=ra
          if(r(ida).le.0.0) then
            r(ida)=0.0
            dur(mo,jd)=0.0
          else
            call alph
C ---------- Equation 2.1.6
            dur(mo,jd)=9.210/(-2.0*alog(1.0-r1))
            if(dur(mo,jd).gt.24.0) dur(mo,jd)=24.0
          endif
c
c        Set duration if a single storm is selected
c
C -- XXX -- Where is "RDUR" set? -- CRM -- 9/22/99
C -- Added initialization of RDUR to Zero:
C         rdur = 0.0
C         if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=rdur
          if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=0.0
C ******** L2 IF ****
          if(iopt.ge.4) then
            if(r(ida).gt.0.) then
              call alph
              xr=r(ida)*25.4
              tpr=timepk(timpkd,k10)
              if(tpr.gt.0.99) tpr=0.99
C ------------ Equation 2.1.7
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
c -------- Change for new option 4 and 7
            if(iopt.eq.4) then
              dur(mo,jd)=usdur
              xr=damt*25.4
              tpr=ustpr
              xmav=(uxmav*25.4)/(xr/dur(mo,jd))
              if(xmav.lt.1.01) xmav=1.01
C           endif
            else if(iopt.eq.7) then
C           if(iopt.eq.7) then
              dur(mo,jd)=24.
              xr = damt*25.4
              xmav=tymax(itype)/(xr/dur(mo,jd))
              if(xmav.lt.1.01) xmav=1.01
              tpr=dtp(itype)
            endif
c
c            Write WEPP Continuous Storm File
c            Writes WEPP Single Storm when nbt=ndt
            write(7,2000) jd,mo,iyear,xr,dur(mo,jd),tpr,
     1                 xmav,tmxg,tmng,radg(mo,jd),wv,th,tdp
C ******** L2 ENDIF ****
          endif
C ****** L1 ENDIF ****
        endif
C180  continue
      ida=ida+1
      if(ida .le. ntd) goto 180
c
c      End of Daily Loop
c
c
      moveto = 0
c     
      return
      end
c
c
c
      subroutine opt_calc
     i           (iyear,stidd,numyr,nstat,ii,
     m            sumpp,sumptx,sumptm,sumprd,sumpdr,
     o            moveto)
c     + + + PURPOSE + + +
c     To handle Options 1-3.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     iyear       - Beginning Simulation Year.
c     stidd       - 41-character alphanumeric station name.
c     numyr       - Number of years to simulate.
c     nstat       - 4-digit Numeric Station Code.
c     ii          -
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
      character*41 stidd
c
      character*1 yc
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: iopt
c
      include 'csumr.inc'
c      read: sump,sumtx,sumtm,sumrd,sumdr
c
      include 'cbk5.inc'
c      modify: r
c
c     + + + SUBROUTINES CALLED + + +
c     clmout
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Do you want to view generated data (y/n)? ')
 2010 format(/1x,'Do you want to simulate another year (y/n)? ')
 2020 format(/1x,'Do you want another Station (y/n)? ')
 2030 format(/15x,'Summary of Elements Generated - Year ',i4)
 2040 format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 2050 format(1x,'prcp',i3,12f6.2)
 2060 format(1x,'tmax',i3,12f6.2)
 2070 format(1x,'tmin',i3,12f6.2)
 2080 format(1x,'rad ',i3,12f6.1)
 2090 format(1x,'dur ',i3,12f6.2)
 2100 format(/1x,'Annual Precipitation =',f6.2,a30/)
 2110 format(i5,i5,10f5.2,8x,i2)
 2120 format(i5,i5,6f5.2,28x,i2)
c
c     + + + END SPECIFICATIONS + + +
c
c      Option 1 Stuff
      if(iopt.le.1) then
        write(*,2000)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          iview=1
          call clmout(iview)
        end if
        write(*,2010)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          moveto = 160
        else
          write(*,2020)
          read(*,'(a1)')yc
          if(yc.eq.'y'.or.yc.eq.'Y') then
            moveto = 10
          else
            moveto = 230
          endif
        endif
c
c      Option 2 Stuff
      elseif(iopt.eq.2) then
        jj=ii
        call clmout(0)
        write(*,2030) iyear
        write(*,*)' '
        write(*,2040)
        write(*,2050)jj,(sump(i),i=1,12)
        write(*,2060)jj,(sumtx(i),i=1,12)
        write(*,2070)jj,(sumtm(i),i=1,12)
        write(*,2080)jj,(sumrd(i),i=1,12)
        write(*,2090)jj,(sumdr(i),i=1,12)
        write(*,2100)sump(13),stidd
        an=numyr
        do 200 i=1,12
          sumpp(i)=sumpp(i)+sump(i)/an
          sumptx(i)=sumptx(i)+sumtx(i)/an
          sumptm(i)=sumptm(i)+sumtm(i)/an
          sumprd(i)=sumprd(i)+sumrd(i)/an
          sumpdr(i)=sumpdr(i)+sumdr(i)/an
 200    continue
        sumpp(13)=sumpp(13)+sump(13)/an
c
c      Option 3 Stuff
      elseif(iopt.eq.3) then
        nb=1
        ne=10
        do 210  j=1, 36
          write(8,2110)nstat,iyear,(r(i),i=nb,ne),j
          nb=ne + 1
          ne=nb + 9
 210    continue
        nb=361
        ne=366
        if(((iyear/4*4)-iyear).ne.0) r(366)=0.0
        j=37
        write(8,2120)nstat,iyear,(r(i),i=nb,ne),j
      endif
c
      return
      end
c
c
c
      subroutine sing_stm
     i           (ioyr,
     m            moveto,
     o            jd,iyear,damt,usdur,ustpr,uxmav,numyr,index)
c     + + + PURPOSE + + +
c     Generate Single Storm Data.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ioyr,moveto,jd,iyear,index,numyr
      real damt,usdur,ustpr,uxmav
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ioyr        - Years of Record for Observed Data File (?)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     jd          - Day of the Storm.
c     iyear       - Beginning Simulation Year.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     usdur       - Storm Duration in Hours for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     numyr       - Number of years to simulate.
c     index       - 4-digit numeric station index.
c
c     outfil      - File which will contain generated climate data.
      character*51 outfil
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: iopt
c      write: mo
c
c     + + + LOCAL VARIABLES + + +
c     ibyear      - year of storm (iopt 4 or 7); beginning simulation year
c
c     + + + INPUT FORMATS + + +
 1000 format(a51)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Enter beginning simulation year',
     1' (positive integer value; e.g. 1 ): ')
 2010 format(/1x,'Enter number of years to simulate: ')
 2020 format(/1x,'Enter output file name (ex. Indy.cli): ',
     1       'Station No. ',i4)
c
c     + + + END SPECIFICATIONS + + +
c
c
C **** L1 IF ****
      if (iopt.ne.1) then
C ****** K2 IF ****
        if(moveto.ne.135) then
c
c       Single storm input data section
c
          if (iopt.eq.4.or.iopt.eq.7) then
            write(*,*)
            write(*,*)' Enter Month Day and Year of Storm (mo da yr)'
            read(*,*)mo,jd,ibyear
            write(*,*)' Enter Design Storm Amount in Inches (e.g. 6.30)'
            read(*,*)damt
            if(iopt.eq.4) then
              write(*,*)' Enter Storm Duration in Hours (e.g. 6)'
              read(*,*)usdur
              write(*,*)
     1              ' Enter Time to Peak Intensity (% Duration e.g. .4)'
              read(*,*)ustpr
              write(*,*)' Enter Maximum Intensity Inches/Hour (e.g. 3.0)
     1'
              read(*,*)uxmav
            endif
          else
            if(iopt.eq.6) then
              ibyear=ioyr
              numyr=100
            else
              write(*,2000)
              read(*,*) ibyear
              write(*,2010)
              read(*,*) numyr
            endif
          endif
c
          iyear=ibyear
C ****** K2 ENDIF ****
        endif
c
C ****** L2 IF ****
C       if (moveto.eq.135 .or. iopt.ne.2) then
        if (iopt.ne.2) then
 135      continue
          write(*,2020)index
          read(*,1000)outfil
c
c         Output Options
c          3 - CREAMS - GLEAMS
c          4 - Single Storm WEPP
c          5 - Continuous Storms WEPP
c          6 - Observed Data WEPP
c          7 - Design Storm WEPP
c
          if(iopt.eq.3) then
             ndflag = 136
             open(8,file=outfil,status='new',err=136)
             ndflag = 0
             rewind (8)
 136         continue
          else
             ndflag = 137
             open(7,file=outfil,status='new',err=137)
             ndflag = 0
             rewind (7)
 137         continue
          endif
          if(ndflag.ne.0) then
            write(*,*)' '
            write(*,*)'     **** File Already Exists *****'
            write(*,*)' '
            write(*,*)'           ',outfil
            write(*,*)' '
            write(*,*)'          Enter New File Name '
            write(*,*)' '
            moveto = 135
          endif
          if(moveto.eq.135) goto 135
C ****** L2 ENDIF ****
        endif
C **** L1 ENDIF ****
      endif
c
      return
      end
c
c
c
      subroutine usr_opt
     m           (moveto,
     o            ioyr)
c     + + + PURPOSE + + +
c     Get Options from User.
c
c ----- Split out from the CLIGEN main module 10/6/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer moveto,ioyr
c
c     + + + ARGUMENT DEFINITIONS + + +
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     ioyr        - Years of Record for Observed Data File (?)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c
c     + + + LOCAL VARIABLES + + +
      character*1 yc
      character*51 infile
c
c     + + + LOCAL DEFINITIONS + + +
c     infile      - File contains observed climate data.
c     ndflag      - A flag for loop control, local to this module.
c
c     + + + INPUT FORMATS + + +
 1000 format(a51)
 1010 format(13x,i2)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Weather Generator Options',/,1x,7('-'),1x,
     19('-'),1x,7('-'),//,1x,'1 - Single Year Simulation - Screen',/,
     11x,'2 - Multiple Year - Screen Output',/,
     11x,'3 - Multiple Year Simulation - CREAMS - GLEAMS Output File',/,
     11x,'4 - Selected Single Storm WEPP - Output File',/,
     11x,'5 - Multiple Year - WEPP Output File',/,
     11x,'6 - Read Observed P and Temp and Generate Missing Data',/,
     11x,'7 - Single Design Storm - TR 55 Storm Type WEPP Output File',/
     11x,'8 - Exit Weather Generator Program',//,
     11x,'Enter generator option (1-8): ')
c
c     + + + END SPECIFICATIONS + + +
c
c
 56   continue
        write(*,2000)
        read(*,*)iopt
        if (iopt.eq.6) then
          write(*,*)'Enter Observed Data Input File Name'
          read(*,1000)infile
          ndflag = 57
          open(9,file=infile,status='old',err=57)
          ndflag = 0
 57       continue
          if(ndflag.eq.57) then
            write(*,*)' Error - File not Found '
            write(*,*)' Check Directory for Observed Data File or'
            write(*,*)' Re-enter File Name'
            write(*,*)' Enter c to continue or q to quit'
            read(*,'(a1)')yc
            if(yc.ne.'q'.and.yc.ne.'Q') then
              ndflag = 56
            else
              moveto = 230
            endif
          else
            rewind(9)
            read(9,1010)ioyr
            backspace (9)
          endif
        elseif (iopt.eq.8) then
          moveto = 230
        endif
      if(ndflag.eq.56) goto 56
C     if(moveto.ne.230) numyr=1
      return
      end
c
c
c
      subroutine wxr_gen
     i           (version,igcode,stidd,ylt,yll,years,elev,
     i            jd,itype,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
     m            iyear,numyr,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     o            sumpp,sumptx,sumptm,sumprd,sumpdr)
c
c ----- Split out from the CLIGEN main module 10/8/99 by C. R. Meyer.
c
c     + + + PURPOSE + + +
c     The "guts" of the weather generating code.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real version,ylt,yll,xm,clt,damt,ustpr,uxmav
      real smy(12),tymax(4),timpkd(0:12),tmpcmx(12),tmpcmn(12)
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
      integer igcode,years,elev,iyear,numyr,ntd1,jd,itype,moveto
      character*41 stidd
c
c     + + + ARGUMENT DEFINITIONS + + +
c     version     - CLIGEN version (ie, 4.2)
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c     stidd       - 41-character alphanumeric station name.
c     ylt         - Station Latitude.
c     yll         - Station Longitude.
c     years       - Years of Record.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     jd          - Day of the Storm.
c     itype       - integer value [1..4] to set single storm parameters.
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     tymax(4)    - upper limit of r5p (based on itype)
c     timpkd      - The 12 interval time to peak accummulated distribution
c     usdur       - Storm Duration in Hours for Single Storm.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     iyear       - Beginning Simulation Year.
c     numyr       - Number of years to simulate.
c     xm          - number of days in the month of interest
c     smy         - Observed Monthly Average Precipitation (mm)
c     tmpcmx      - Observed Monthly Average Max Temperature (C)
c     tmpcmn      - Observed Monthly Average Min Temperature (C)
c     ntd1        - julian date of jd, mo (iopt = 4, 7)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: nc,mo,iopt
c      modify: nt
c
      include 'cbk7.inc'
c      read: rst,prw,obmx,obmn,obsl
c
      include 'ccl1.inc'
c      write: prcip,tgmx,tgmn,radg,dur
c
c     + + + LOCAL VARIABLES + + +
c     isim        - simulation mode
c                     1 -- continuous storm (iopt = 5, 6)
c                     2 -- single storm     (iopt = 4, 7)
c     itemp       - breakpoint data flag
c                     0 -- no breakpoint data used
c                     1 -- breakpoint data used
c     xr          - daily precipitation amount (mm of water)
c                    simulated rainfall amount (mm)  [Yu's P]
c
c     + + + SUBROUTINES CALLED + + +
c     day_gen
c     opt_calc
c
c     + + + FUNCTION DECLARATIONS + + +
      integer jdt
c
c     + + + OUTPUT FORMATS + + +
 500  format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 520  format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 555  format(1x,'Observed monthly ave precipitation (mm)',/,
     1       12(1x,f5.1))
 642  format(f5.2)
 644  format('   Station: ',a41,6x,' CLIGEN VERSION 4.2',/
     1       ' Latitude Longitude Elevation (m) Obs. Years ',
     1       '  Beginning year  Years simulated',/
     1 2f9.2,i12,2i12,i16)
 648  format(' da mo year  prcp  dur   tp     ip  tmax',
     1       '  tmin  rad  w-vl w-dir  tdew',
     2         /,13x,'(mm)  (h)',15x,'(C)   (C)',
     3       ' (l/d) (m/s)(Deg)   (C)')
 778  format(3i4)
c
c     + + + END SPECIFICATIONS + + +
c
c
c     Get Everything Ready to Start Generation by Options Selected
c
 140    continue
        do 150 i=1,12
          sumpp(i)=0.0
          sumptx(i)=0.0
          sumptm(i)=0.0
          sumprd(i)=0.0
          sumpdr(i)=0.0
 150    continue
        sumpp(13)=0.0
c
c     See What Option was Selected and Set Paths
c
        nbt=1
        if(iopt .ge. 4)then
          isim=1
          if(iopt.eq.4.or.iopt.eq.7) isim =2
          itemp=0
          write(7,642)version
          write(7,778)isim,itemp,igcode
          if(iopt.ge.4) then
            write(7,644)stidd,ylt,yll,elev,years,iyear,numyr
c           write(7,646) iyear,numyr
c
c  CALCULATE MONTHLY RAINFALL AMOUNTS
            do 111 kkk = 1,12
              xm = nc(kkk+1)-nc(kkk)
c ---------- calculate number of days of rainfall in month
              smy(kkk) = xm*prw(2,kkk)/(1.-prw(1,kkk) + prw(2,kkk))
c ---------- monthly rainfall in mm
              smy(kkk) = smy(kkk) * rst(kkk,1) * 25.4
              tmpcmx(kkk)=(obmx(kkk)-32.0)*(5.0/9.0)
              tmpcmn(kkk)=(obmn(kkk)-32.0)*(5.0/9.0)
 111        continue
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
c
c      Generate Data by Number of Years and Option - MAIN LOOP
c
        ii = 1
 160    continue
          moveto = 0
          ntd=365
          if((iopt.le.3.or.iopt.eq.5.or.iopt.eq.6) .and.
     1                                   (iyear-iyear/4*4).eq.0) ntd=366
          do 170 i=1,12
            do 169 jk=1,31
              prcip(i,jk)=0.0
              tgmx(i,jk)=0.0
              tgmn(i,jk)=0.0
              radg(i,jk)=0.0
              dur(i,jk)=0.0
 169        continue
 170      continue
c
          call day_gen(nbt,jd,iyear,clt,tymax,timpkd,usdur,damt,ustpr,
     1                 uxmav,itype,ntd1,ntd,moveto)
c
          if(moveto .ne. 225) then
            call opt_calc(iyear,stidd,numyr,nstat,ii,sumpp,sumptx,
     1                    sumptm,sumprd,sumpdr,moveto)
c
            if(moveto.eq.0) then
              iyear=iyear+1
              ii = ii + 1
            endif
          endif
        if((moveto.eq.0 .and. ii.le.numyr).or.(moveto.eq.160)) goto 160
c
      return
      end
