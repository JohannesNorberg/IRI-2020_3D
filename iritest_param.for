c iritest.for, version number can be found at the end of this comment.
c-----------------------------------------------------------------------
c
c test program for the iri_web subroutine
c
c Intended for use only with rirtiest_ne and iritest_param wrappers. 
c When used with iritest_ne the altitude vector is taken from alt_ax.txt.
c
c***********************************************************************
c!********************* IMPORTANT PLEASE READ **************************        
c***********************************************************************
c
c This test program shows how to initialize and call the IRI subroutine 
c IRI_SUB. Any program using IRI_SUB needs to include the following 
c statements (this iritest.for program shows you how and where to include 
c these statements):
c
c	call read_ig_rz
c       call readapf107
c
c The indices values are stored in the arrays ionoindx, indrz, aap, af107
c and are passed to other subroutines via common/igrz/ and common/apfa/
c
c       do i=1,100
c          oar(i,1)=-1.0
c          enddo
c
c If you want to use the standard version of IRI you should make sure 
c that the JF(50) logical values are set to the recommended default 
c values: 
c     jf(4,5,6,23,30,33,35,39,40,47)=.false. all others =.true.
c You can turn off (jf(%)=.false.) the computation of certain parmeters 
c if you do not need these parameters:
c     jf(1) Ne, jf(2) Te Ti Tn, jf(3) Ni, jf(21) ion drift, 
c     jf(28) spread-F probability 
c For some parameters the default is already .false.:
c     jf(33) auroral boundaries,jf(35) foE storm model,
c     jf(47) CGM coordinates 
c If you use IRI with JF values other than the default values please
c make sure to mention this in any publication that results from your
c research.  
c
c Required files: irisub.for, irifun.for, iritec.for, iridreg.for, 
c                 iriflip.for, igrf.for, cira.for, rocdrift.for        
c
c Required i/o units:  
c  IRISUB.FOR, IRIFUN.FOR, IGRF.FOR:
c   KONSOL= 6: Program messages to konsol       (if jf(12)=.true.)
c   KONSOL=11: Program messages to MESSAGES.TXT (if jf(12)=.false.) 
c	  (COMMON/iounit/konsol,mess is used to pass the values from  
c	  IRISUB to IRIFUN and IGRF. If mess=false messages are turned off)
c  IRISUB:
c	IUCCIR=10: CCIR and URSI coefficients (CCIR%%.ASC, %%=month+10)
c  IRIFUN:
c   read_data_SD: UNIT=10 coefficients of Shubin (2015) hmF2 model  
c   read_ig_rz: UNIT=12 Solar/ionospheric indices IG12, R12 (IG_RZ.DAT) 
c   readapf107: UNIT=13 Magnetic indices and F10.7 (APF107.DAT)
c  IGRF: 
c   GETSHC: UNIT=14  IGRF coeff. (DGRF%%%%.DAT, IGRF%%%%.DAT, %%%%=year)
c
c***********************************************************************
c***********************************************************************
c***********************************************************************
c
c-version-mm/dd/yy ----------corrections--------------------------
c 2000.01 05/07/01 initial version
c 2000.02 07/11/01 line 210: do i=1,100 instead of i=2,100 (K. Tokar)
c 2000.03 28/12/01 output oar(39) for IG12 (R. Conkright, NGDC, NOAA)
c 2000.04 28/10/02 replace TAB/6 blanks, enforce 72/line (D. Simpson)
c 2000.05 02/06/03 Ne(Te) only 300,400; foF1 and hmF1 output corr.
c 2000.06 01/19/05 (.not.jf(20)) instead of (..jf(2)) (G. Schiralli)
c 2005.01 05/06/06 included spread-F (jf(28)) and topside (jf(29)) options
C 2007.00 05/18/07 Release of IRI-2007
c 2007.02 10/31/08 outf(100) -> outf(500), numhei=numstp=500
c 2007.03 02/12/09 added new D-region option (h=-3)
c 2007.11 04/19/10 correct TEC for normal output  [Shunrong Zhang] 
c
C 2012.00 10/05/11 IRI-2012: bottomside B0 B1 model (SHAMDB0D, SHAB1D),
C 2012.00 10/05/11    bottomside Ni model (iriflip.for), auroral foE
C 2012.00 10/05/11    storm model (storme_ap), Te with PF10.7 (elteik),
C 2012.00 10/05/11    oval kp model (auroral_boundary), IGRF-11(igrf.for), 
C 2012.00 10/05/11    NRLMSIS00 (cira.for), CGM coordinates, F10.7 daily
C 2012.00 10/05/11    81-day 365-day indices (apf107.dat), ap->kp (ckp),
C 2012.00 10/05/11    array size change jf(50) outf(20,1000), oarr(100).
C 2012.00 03/21/12    PIKTAB=4 output for D-region
C 2012.01 09/16/12 Corrected UT output (hour-25)
C 2012.03 09/18/14 input JF(18): FIELDG not UT_LT          (A. Mazzella)
C 2012.03 09/18/14 rzino, igino logical not real; and more (A. Mazzella)
C 2012.03 09/18/14 jf defaults and special for piktab=3    (A. Mazzella)
C 2012.03 09/23/14 jf(22): output option Ni in [m-3]/1.e9
C 2012.03 09/23/14 jf(36:38) include in input choices
C 2012.03 09/23/14 added output options for parameters oar(59:86) 
C 2012.05 04/27/15 delete double line CHARACTER*9  pname(6)
C 2015.01 06/30/15 scid=1.0E-9                              (A. Charisi)
C 2015.01 06/30/15 jino-outf(9)&jio2-outf(8) write.jio2,jino(A. Charisi)
C 2015.01 07/12/15 output of ion density/composition now with 3 digits
C 2015.01 07/12/15 call read_ig_rz readapf107
C 2015.02 08/13/15 delete COMMON/CONST2
C 2015.02 08/13/15 ursifo=jf(5) just before IRI_SUB call
C 2016.01 06/01/16 User specified B0 when jf(43)=false
C 2016.02 08/22/16 Allow user input options for F10.7D and F10.7_81
C 2016.03 04/25/18 cleaned up user input options: B0,B1,F10.7D,F10.7_81
C 2016.04 09/05/18 user input options: HNEA and HNEE
C 2020.01 09/08/20 deleted extra jf(43) input                (C. Vasyl) 
C 2020.02 09/08/20 re-arranged jf inputs - if user input no options 
C 2020.03 09/16/20 messages if auroral boundary or CGM are off
C 2020.04 10/03/21 hmF2: Shubin model default, jf(40)=.false 
C 2020.05 10/22/23 user input for htec_min for iri_web
C 2020.05 10/22/23 delete user input option for HNEA and HNEE
C 2020.05 10/22/23 hmF2: AMTB model (better results at low lat)(D.Drob)
C 2020.05 10/22/23 topside/plasmasphere: NeQuick default 
C
      INTEGER           pad1(6),jdprof(77), seconds
      DIMENSION         outf(20,1000),oar(100,1000),jfi(6)
      LOGICAL		    jf(50),rzino,igino
      CHARACTER*2       timev(2)
      CHARACTER*3       uni(86),sopt,seopt
      CHARACTER*4       IMZ(8),MAP,xtex,coorv(2),plpa
      CHARACTER*5       ITEXT(8),tsopt
      CHARACTER*6       pna(86)
      CHARACTER*7       popt
      CHARACTER*8       bopt,topt,tiopt,pplas
      CHARACTER*9       pname(7)
      CHARACTER*10      dopt,hopt
      CHARACTER*11      iopt,rzopt,igopt,f8opt,fdopt
      CHARACTER*16      f1opt
      INTEGER           jm, iut, hx, ivar
      INTEGER           htec_min, htec_max
      CHARACTER*32      arg      
      CHARACTER*32      filename
      CHARACTER*4       year_str, date_str
      CHARACTER*5       second_str      
      INTEGER           iostat 
      REAL              lat, lon, NmF2, hmF2, hour_input, vbeg, vend, vstp


      DATA  IMZ  /' km ','GEOD','GEOD','yyyy',' mm ',' dd ','YEAR',
     &      'L.T.'/, ITEXT/'  H  ',' LATI',
     &      ' LONG',' YEAR','MONTH',' DAY ','DAYOF',' HOUR'/

      data pna/'NmF2','hmF2','NmF1','hmF1','NmE','hmE','NmD','hmD',
     &  'h05','B0','NVmin','hVtop','Tpeak','hTpek','T300','T400','T600',
     &  'T1400','T3000','T120','Ti450','hTeTi','sza','sundec','dip',
     &  'diplat','modip','Lati','Srise','Sset','season','Longi',
     &  'Rz12','cov','B1','M3000','TEC','TECtop','IG12','F1prb','F107d',
     &  'C1','daynr','vdrft','foF2r','F10781','foEr','sprd_F','MLAT',
     &  'MLON','Ap_t','Ap_d','invdip','MLTinv','CGMlat','CGMlon',
     &  'CGMmlt','CGM_AB','CGMm0','CGMm1','CGMm2','CGMm3','CGMm4',
     &  'CGMm5','CGMm6','CGMm7','CGMm8','CGMm9','CGMm10','CGMm11',
     &  'CGMm12','CGMm13','CGMm14','CGMm15','CGMm16','CGMm17','CGMm18',
     &  'CGMm19','CGMm20','CGMm21','CGMm22','CGMm23','kp_t','dec','L',
     &  'DIMO'/
      data uni/'m-3','km','m-3','km','m-3','km','m-3','km','km','km',
     &   'm-3','km','K','km',7*'K','km',6*'deg',2*'h',' ','deg',4*' ',
     &   'm-2','%',5*' ','m/s',4*' ',2*'deg',2*' ','deg','h',2*'deg',
     &   'h',25*'deg',' ','deg',' ','Gau'/,
     &   timev/'LT','UT'/,coorv/'geog','geom'/

      data jfi/8,9,13,14,15,16/

      data pad1/1,2,5,6,10,35/
c	  COMMON/const2/icalls,montho,nmono,iyearo,idaynro,ursifo,rzino,
c     &	            igino,ut0
c
c		icalls=0
c		montho=-1
c		nmono=-1
c		iyearo=-1
c		idaynro=-1
c		rzino=.true.
c		igino=.true.
c		ut0=-1

        call read_ig_rz
        call readapf107
        
        nummax=1000
        
        DO i=1,100
        oar(i,1)=0.0
        ENDDO

c user input of IRI input parameters
c
C1       print *,'jmag(=0/1,geog/geom),lati/deg,long/deg'
C       read(5,*) jm,xlat,xlon
C       print *,'year(yyyy),mmdd(or -ddd),iut(=0/1,LT/UT),hour'
C       read(5,*) iy,imd,iut,hour
C       print *,'height/km'
C       read(5,*) hx
C
C       print *,'variable? (1/2/../8 for height/lat/long/year/month/',
C    &                        'day/day of year/hour)'
C       read(5,*) ivar
C       print *,'begin, end, and stepsize for the selected variable'
C       read(5,*) vbeg,vend,vstp
C
C       print *,'output-option (if variable=height then choose 0, 3,',
C    &                        '4, or 5)'
C       print *,'(enter 0 for standard table of IRI parameters)'
C       print *,'(enter 1 for list of peak heights and densities)'
C       print *,'(enter 2 for plasma frequencies, B0, M3000, ',
C    &                        'valley, width and depth,)'
C       print *,'(enter 3 for 6 parameters of your choice)'
C       print *,'(enter 4 for D-region models at 60,65,..,110 km)'
C       print *,'(enter 5 special test output)'
C       read(5,*) piktab
C
C       print *,'lower and upper height [km] for TEC integration',
C    &          ' (valid range: 65-30,000 km) (0,0 for no TEC)'
C       read(5,*) htec_min,htec_max
C
C       print *,'Options: t(rue) or f(alse)'
C       print *,'Enter 0 to use standard or 1 to enter your own'
C       read(5,*) jchoice

        CALL getarg(1, arg)
        READ(arg, *) iy
        CALL getarg(2, arg)
        READ(arg, *) imd
c       CALL getarg(5, arg)
c       READ(arg, *) iut
        iut = 1
        CALL getarg(3, arg)
        READ(arg, *) hour_input
        hx=0              ! height/km
        ivar=2
c       (1/2/../8 for height/lat/long/year/month/day/day of year/hour)
C       CALL getarg(4, arg)
C       READ(arg, *) vbeg
C       CALL getarg(5, arg)
C       READ(arg, *) vend
C       CALL getarg(6, arg)
C       READ(arg, *) vstp

        htec_min=0         ! lower height [km] for TEC
        htec_max=0         ! upper height [km] for TEC
        jm = 0
        

c     convert hours to deconds to include decimal hours for 00 hour    
C     seconds = NINT(hour_input * 3600)
C    
C     WRITE(year_str, '(I4)') iy         
C     WRITE(date_str, '(I4)') imd      
C     WRITE(second_str, '(I5)') seconds     
C     filename = year_str//date_str//second_str//"_param.txt"
      filename = "param.txt"


      OPEN(UNIT=11, FILE=filename, STATUS='UNKNOWN')
      
C       xtex=imz(ivar)
C       xtex='GEOM'

        WRITE(11,8191) 'lat', 'lon',
     &    (pna(pad1(j)),j=1,6)
 
 8191  FORMAT(2X,A5,A7,A8,5A10)
      

c Input from txt file
        
      OPEN(UNIT=12, FILE="lat_lon.txt", STATUS='OLD', IOSTAT=iostat)
      IF (iostat /= 0) THEN
          PRINT *, 'Error opening file: lat_lon.txt'
          STOP
      ENDIF

c     Skip header      
      READ(12, *)


        
          DO i=1,50 
                jf(i)=.true.
          ENDDO
c defaults for jf(1:50)
c          jf(1)=.false.      ! f=no electron densities (t) 
c          jf(2)=.false.      ! f=no temperatures (t)
c          jf(3)=.false.      ! f=no ion composition (t)
          jf(4)=.false.      ! t=B0table f=other models (f)
          jf(5)=.false.      ! t=CCIR  f=URSI foF2 model (f)
          jf(6)=.false.      ! t=DS95+DY85   f=RBV10+TBT15 (f)
c          jf(7)=.false.      ! t=tops f10.7<188 f=unlimited (t)
c          jf(19)=.false. 	 !F1 prob model   only if foF1>0 and not NIGHT (t)
c          jf(20)=.false.     !standard F1  standard F1 plus L condition  (t)
c (19,20) = (t,t) f1-prob, (t,f) f1-prob-L, (f,t) old F1, (f,f) no F1
c          jf(21)=.false.     ! t=ion drift computed f=not comp.(f)
c          jf(22)=.false.     ! ion densities in m-3 (t)
          jf(23)=.false.     ! t=AEROS/ISIS f=TTS Te with PF10.7 (f)
c          jf(24)=.false.     ! t=D-reg-IRI-1990 f=FT-2001 (t)
c          jf(25)=.false.     ! t=F107D from APF107.DAT  f=user (t)
c          jf(26)=.false.	 ! t=STORM model on   f= off (t)
c          jf(28)=.false.	 ! t=spread-F computed f=not comp. (f)
          jf(29)=.false.     ! t=old  f=New Topside options (f)
          jf(30)=.false.     ! t=corr f=NeQuick topside (f)
C (29,30) = (t,t) IRIold, (f,t) IRIcor, (f,f) NeQuick, (t,f) COR2
c          jf(31)=.false.     ! t=B0ABT f=Gulyaeva (t)
c          jf(32)=.false.     ! t=F107_81 from APF107.DAT  f=user (t)
          jf(33)=.false. 	  ! t=auroral boundary   f=off (f)
          jf(34)=.false. 	  ! t=messages on f= off (t)
          jf(35)=.false. 	  ! t=auroral E-storm model on f=off (f)
c          jf(36)=.false. 	  ! t=hmF2 w/out foF2_storm f=with (t)
c          jf(37)=.false. 	  ! t=topside w/out foF2_storm f=with (t)
c          jf(38)=.false. 	  ! t=WRITEs off in IRIFLIP f=on (t)
          jf(39)=.false. 	  ! t=M3000F2 model f=new hmF2 models (f)
c          jf(40)=.false. 	  ! t=AMTB-model, f=Shubin-COSMIC model (t) 
c          jf(41)=.false. 	  ! t:COV=F10.7_386 f:COV=f(IG12) (t) 
c          jf(42)=.false. 	  ! t/f=Te w/o PF10.7 dependance (t)
c          jf(43)=.false. 	  ! t= B0 model f= B0 user input (t)
c          jf(44)=.false. 	  ! t= B1 model f= B1 user input (t)
c          jf(45)=.false. 	  ! not used
c          jf(46)=.false. 	  ! not used
          jf(47)=.false. 	  ! t=CGM on  f=CGM off (f)
c          jf(48)=.false. 	  ! t=Ti-Tru2021 f=Ti-Bil1981 (t)



c option to enter six additional parameters 
c change defaults for computation of specific parameters
        	jf(21)=.true.  ! spread-F prob. computed
        	jf(28)=.true.  ! vertical ion drift computed
        	jf(33)=.true.  ! auroral boundary computed
        	jf(35)=.true.  ! foE_storm computed
C       if(pad1(1).eq.0) then
C       	pad1(1)=48     ! spread-F probability
C           pad1(2)=44     ! equatorial vertical ion drift
C           pad1(3)=45     ! fof2_storm/foF2_quiet
C           pad1(4)=47     ! foE_storm/foE_quiet
C           pad1(5)=58     ! CGM_lat auroral boundary
C           pad1(6)=51     ! ap for current UT
C           endif
       


c end of user input
c


        pplas='GCC-2000'
        if(jf(49)) pplas='OTSB2012'
        plpa='with'
        if(jf(50)) plpa='w/o '
        if(jf(29)) then
              if(jf(30)) then
                   popt='IRI2001''IRIcorr'
             else
                   popt='IRIcor2'
             endif
        else
             if(jf(30)) then
                   popt='IRIcorr'
             else
                   popt='NeQuick'
             endif
        endif
        map='URSI'
        if(jf(5)) map='CCIR'

        if(jf(39)) then
             hopt='CCIR-M3000'
        else
             if(jf(40)) then
                   hopt='AMTB-2013'
             else
                   hopt='Shubin2015'
             endif
        endif

        if(jf(4)) then
             bopt='BIl-2000'
        else
             if(jf(31)) then
                   bopt='ABT-2009'
             else
                   bopt='Gul-1987'
             endif
        endif

        iopt='RBV10+TBT15'
        if(jf(6)) iopt='DS95 + DY85'

        dopt='FT01+DRS95'
        if(jf(24)) dopt='IRI-1990'

        sopt='off'
        if(jf(26)) sopt='on '
        
        seopt='off'
        if(jf(35)) seopt='on '

        topt='TBT-2012'
        if(jf(23)) topt='Bil-1985'
        tsopt=' with'
        if(jf(23)) tsopt='  w/o'

        tiopt='Bil-1981'
        if(jf(48)) tiopt='TBKS2021'

        if(jf(19)) then
              f1opt='Scotto-97 no L'
              if(.not.jf(20)) f1opt='Scotto-97 with L'
        else
              f1opt='IRI-95'
              if(.not.jf(20)) f1opt='no F1 region'
        endif

        rzopt=' user input'
        if(jf(17)) rzopt=' '
        igopt=' user input'
        if(jf(27)) igopt=' '
        fdopt=' user input'
        if(jf(25)) fdopt=' '
        f8opt=' user input'
        if(jf(32)) f8opt=' '
        
        hxx=hx
        jmag=jm
        mmdd=imd


      DO 100 WHILE (iostat == 0)
         READ(12, *, IOSTAT=iostat) lat, lon
C        PRINT *, 'IOSTAT:', iostat                
         IF (iostat /= 0) EXIT
C   	 PRINT *, 'Lat:', lat, 'Lon:', lon
  
		 xlat = lat
		 xlon = lon
		 jm=0              ! (=0/1,geog/geom)

c        Only one value is computed, not interval
		 vbeg = xlat
		 vend = vbeg
		 vstp = 1
c        iri_web changes the value of hour variable
		 hour = hour_input


C      PRINT *, 'HERE1', xlat, vbeg

        num1=(vend-vbeg)/vstp+1
        numstp=iabs(num1)
        if(numstp.GT.nummax) numstp=nummax

c calling IRI subroutine
c 
C       PRINT *, 'HERE1', iut, hour

C       phour = hour
        call iri_web(jmag,jf,xlat,xlon,iy,mmdd,iut,hour,
     &    hxx,htec_min,htec_max,ivar,vbeg,vend,vstp,outf,oar)

C       PRINT *, 'HERE2', iut, hour
c
c table head .......................................................
c


        xcor=vbeg

        li=1

c
c output: 6 parameters of your choice    PIKTAB=3
c

        WRITE(11,3919) XCOR, xlon, oar(pad1(1),li),oar(pad1(2),li),
     &        oar(pad1(3),li),oar(pad1(4),li),oar(pad1(5),li),
     &        oar(pad1(6),li)
3919    FORMAT(F7.1, F7.1, 6(1X,1PE9.2))

C       GOTO 1234

C1234    xcor=xcor+vstp


100   CONTINUE

      CLOSE(12)
      CLOSE(11)

2357        stop
            end
