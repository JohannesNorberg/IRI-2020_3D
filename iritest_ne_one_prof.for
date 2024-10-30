c-----------------------------------------------------------------------
c
c test program for the iri_web subroutine modified from iritest.for
c returns individual electron density profiles in Ne/cm^3 
c Uses standard IRI or predefined switches
c takes parameters as inputs in the following order:
c lat long year mmdd h altmin altmax altstep switches(0/1) nmF2 hmF2 nmE hmE 
c e.g. ./iri_ne 65 25 2021 1110 12 0 1000 10 1 5.0E11 300 1.0E11 110
c if switches = 0 trailing parameters can be left in the call, but are not used
c if switches = 1 different switches are hard coded in the code and 
c nmF2 hmF2 nmE hmE taken from the call
c
c compiling e.g: gfortran-mp-14 -o iri_ne iritest_ne.for irisub.for irifun.for iritec.for 
c                                  iridreg.for igrf.for cira.for iriflip.for rocdrift.for
c-----------------------------------------------------------------------

      INTEGER           pad1(6),jdprof(77),piktab
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
      CHARACTER*32      arg
      

      DATA  IMZ  /' km ','GEOD','GEOD','yyyy',' mm ',' dd ','YEAR',
     &      'L.T.'/, ITEXT/'  H  ',' LATI',
     &      ' LONG',' YEAR','MONTH',' DAY ','DAYOF',' HOUR'/

      DATA pna/'NmF2','hmF2','NmF1','hmF1','NmE','hmE','NmD','hmD',
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
      DATA uni/'m-3','km','m-3','km','m-3','km','m-3','km','km','km',
     &   'm-3','km','K','km',7*'K','km',6*'deg',2*'h',' ','deg',4*' ',
     &   'm-2','%',5*' ','m/s',4*' ',2*'deg',2*' ','deg','h',2*'deg',
     &   'h',25*'deg',' ','deg',' ','Gau'/,
     &   timev/'LT','UT'/,coorv/'geog','geom'/

      DATA jfi/8,9,13,14,15,16/

        call read_ig_rz
        call readapf107
        
        nummax=1000
        
        do 6249 i=1,100
6249    oar(i,1)=0.0

c user input of IRI input parameters

1       CALL getarg(1, arg)
        READ(arg, *) xlat
        CALL getarg(2, arg)
        READ(arg, *) xlon
        jm=0              ! (=0/1,geog/geom)
        CALL getarg(3, arg)
        READ(arg, *) iy
        CALL getarg(4, arg)
        READ(arg, *) imd
c       CALL getarg(5, arg)
c       READ(arg, *) iut
        iut = 1
        CALL getarg(5, arg)
        READ(arg, *) hour
        hx=0              ! height/km
        ivar=1
c       (1/2/../8 for height/lat/long/year/month/day/day of year/hour)
        CALL getarg(6, arg)
        READ(arg, *) vbeg
        CALL getarg(7, arg)
        READ(arg, *) vend
        CALL getarg(8, arg)
        READ(arg, *) vstp
        piktab=0

        htec_min=0         ! lower height [km] for TEC
        htec_max=0         ! upper height [km] for TEC
c       (0,0 for no TEC)'
        CALL getarg(9, arg)
        READ(arg, *) jchoice ! 0: default switches
        
c        Standard IRI model
        if(jchoice.eq.0) then
          do i=1,50 
                jf(i)=.true.
          enddo

          jf(4)=.false.      ! t=B0table f=other models (f)
          jf(5)=.false.      ! t=CCIR  f=URSI foF2 model (f)
          jf(6)=.false.      ! t=DS95+DY85   f=RBV10+TBT15 (f)
          jf(23)=.false.     ! t=AEROS/ISIS f=TTS Te with PF10.7 (f)
          jf(29)=.false.     ! t=old  f=New Topside options (f)
          jf(30)=.false.     ! t=corr f=NeQuick topside (f)
          jf(33)=.false. 	  ! t=auroral boundary   f=off (f)
          jf(34)=.false. 
          jf(35)=.false. 	  ! t=auroral E-storm model on f=off (f)
          jf(39)=.false. 	  ! t=M3000F2 model f=new hmF2 models (f)
          jf(47)=.false. 	  ! t=CGM on  f=CGM off (f)
        else
		  jf(1) = .true.     ! Ne computed
		  jf(2) = .true.     ! Te, Ti computed
		  jf(3) = .true.     ! Ne & Ni computed
		  jf(4) = .false.    ! B0,B1 - Bil-2000 (false = other models jf(31))
		  jf(5) = .false.    ! foF2 - CCIR (false = URSI)
		  jf(6) = .false.    ! Ni - DS-1995 & DY-1985 (false = RBV-2010 & TBT-2015)
		  jf(7) = .true.     ! Ne - Tops: f10.7<188
		  jf(8) = .false.    ! foF2 from model
		  jf(9) = .false.    ! hmF2 from model
		  jf(10) = .true.    ! Te - Standard
		  jf(11) = .true.    ! Ne - Standard Profile
		  jf(12) = .false.    ! Messages to unit 6
		  jf(13) = .true.    ! foF1 from model
		  jf(14) = .true.    ! hmF1 from model
		  jf(15) = .false.    ! foE from model
		  jf(16) = .false.    ! hmE from model
		  jf(17) = .true.    ! Rz12 from file
		  jf(18) = .true.    ! IGRF dip, magbr, modip
		  jf(19) = .true.    ! F1 probability model
		  jf(20) = .true.    ! standard F1
		  jf(21) = .true.    ! ion drift computed
		  jf(22) = .true.    ! ion densities in %
		  jf(23) = .false.   ! Te_tops (Bil-1985) (false = TBT-2012)
		  jf(24) = .true.    ! D-region: IRI-1990
		  jf(25) = .true.    ! F107D from APF107.DAT
		  jf(26) = .true.    ! foF2 storm model
		  jf(27) = .true.    ! IG12 from file
		  jf(28) = .true.    ! spread-F probability
		  jf(29) = .false.   ! IRI01-topside (false = new options as defined by JF(30))
		  jf(30) = .false.   ! IRI01-topside corr. (false = NeQuick topside model)
		  jf(31) = .true.    ! B0,B1 ABT-2009 (false = B0 Gulyaeva-1987 h0.5)
		  jf(32) = .true.    ! F10.7_81 from file
		  jf(33) = .false.   ! Auroral boundary model on/off
		  jf(34) = .false.    ! Messages on
		  jf(35) = .false.   ! foE storm model
		  jf(36) = .true.    ! hmF2 w/out foF2_storm
		  jf(37) = .true.    ! topside w/out foF2-storm
		  jf(38) = .true.    ! turn WRITEs off in IRIFLIP
		  jf(39) = .false.   ! hmF2 (M3000F2)
		  jf(40) = .true.    ! hmF2 AMTB-model (false = Shubin-COSMIc model)
		  jf(41) = .true.    ! Use COV=F10.7_365
		  jf(42) = .true.    ! Te with PF10.7 dep.
		  jf(43) = .true.    ! B0 from model
		  jf(44) = .true.    ! B1 from model
		  jf(45) = .true.    ! not used
		  jf(46) = .true.    ! not used
		  jf(47) = .false.   ! CGM computation on/off
		  jf(48) = .true.    ! Ti Tru-2021
		  jf(49) = .true.    ! Plasmasphere: Ozhogin
		  jf(50) = .true.    ! without plasmapause
        endif      
       

		numstp=int((vend-vbeg)/vstp)+1            
			if(ivar.eq.1) numstp=1



c jchoice = 1: option to enter measured values for NmF2, hmF2, NmF1, hmF1, NmE, hmE,
c B0, N(300), N(400), N(600) if available; 
              
       if(jf(1)) then
         if(.not.jf(8).or..not.jf(9).or..not.jf(13).or..not.jf(14).or.
     &  .not.jf(15).or..not.jf(16).or..not.jf(43).or..not.jf(44)) then
c           var=vbeg
            i=1
c --------- user input: foF2 or NmF2            
2234        if(.not.jf(8)) then
              jf(26)=.false.    ! storm model off, if user input
			  CALL getarg(10, arg)            
			  READ(arg, *) oar(1, i)          
			  pname(1) = 'NmF2/m-3'          
			  endif
c --------- user input: hmf2 or M(3000)F2            
            if(.not.jf(9)) then
			  CALL getarg(11, arg)            
			  READ(arg, *) oar(2, i)          
			  pname(2) = 'hmF2/km'          
              endif
c --------- user input: foF1 or NmF1            
            if(.not.jf(13)) then
              print *,'foF1/MHz or NmF1/m-3 for ',itext(ivar),
     &               '=',var
              READ(5,*) oar(3,i)
              pname(3)='foF1/MHz'
              if(oar(3,i).gt.30.) pname(3)='NmF1/m-3'
              endif
c --------- user input: hmF1            
            if(.not.jf(14)) then
              print *,'hmF1/km for ',itext(ivar),'=',var
              READ(5,*) oar(4,i)
              pname(4)='hmF1/km'
              endif
c --------- user input: foE or NmE            
            if(.not.jf(15)) then
			  CALL getarg(12, arg)            
			  READ(arg, *) oar(5, i)          
			  pname(5) = 'NmE/m-3'          
              endif
c --------- user input: hmE            
            if(.not.jf(16)) then
			  CALL getarg(13, arg)            
			  READ(arg, *) oar(6, i)          
              pname(6)='hmE/km'
              endif
c --------- user input: B0            
            if(.not.jf(43)) then
              print *,'B0/km for ',itext(ivar),'=',var
              READ(5,*) oar(10,i)
              pname(7)='B0/km '
              endif
c --------- user input: B1            
            if(.not.jf(44)) then
              print *,'B1 for ',itext(ivar),'=',var
              READ(5,*) oar(35,i)
              pname(35)='B1    '
              endif
c           i=i+1
c           var=var+vstp
c           if(ivar.gt.1.and.var.le.vend) goto 2234
         endif
       endif

c from irisub.for function:
c    jf(8)  =.false.     OARR(1)=user input for foF2/MHz or NmF2/m-3
c    jf(9)  =.false.     OARR(2)=user input for hmF2/km or M(3000)F2
c    jf(10 )=.false.     OARR(15),OARR(16)=user input for Ne(300km),
c       Ne(400km)/m-3. Use OARR()=-1 if one of these values is not 
c       available. If jf(23)=.false. then Ne(300km), Ne(550km)/m-3.
c    jf(13) =.false.     OARR(3)=user input for foF1/MHz or NmF1/m-3 
c    jf(14) =.false.     OARR(4)=user input for hmF1/km
c    jf(15) =.false.     OARR(5)=user input for foE/MHz or NmE/m-3 
c    jf(16) =.false.     OARR(6)=user input for hmE/km
c    jf(17) =.flase.     OARR(33)=user input for Rz12
c    jf(25) =.false.     OARR(41)=user input for daily F10.7 index
c    jf(27) =.false.     OARR(39)=user input for IG12
c    jf(32) =.false.     OARR(46)=user input for 81-day avg F10.7
c    jf(43) =.false.     OARR(10)=user input for B0
c    jf(44) =.false.     OARR(35)=user input for B1


c end of user input
C This is from original code. Not checked if could be removed

        num1=(vend-vbeg)/vstp+1
        numstp=iabs(num1)
        if(numstp.GT.nummax) numstp=nummax

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

c calling IRI subroutine

        phour=hour
        call iri_web(jmag,jf,xlat,xlon,iy,mmdd,iut,hour,
     &    hxx,htec_min,htec_max,ivar,vbeg,vend,vstp,outf,oar)

c output
		
        xcor=vbeg

        do 1234 li=1,numstp

c	jne = int(outf(1,li) / 1.e6 + .5)
c	if(outf(1,li).lt.0) jne = -1
		jne = outf(1,li) / 1.e6
		if(outf(1,li).lt.0) jne = 0


        WRITE(6,7117) XCOR,jne
7117    FORMAT(F7.1, 1x, I7)

c       write(6, 7117) jne
c7117    FORMAT(I7)

1234    xcor=xcor+vstp

c    print *,'Enter 0 to exit or 1 to generate another profile?' 
c        READ(5,*) icontinue
c        if (icontinue.gt.0) goto 1
c		print *,oar(51,1),oar(52,1),oar(83,1)    
2357        stop
            end
