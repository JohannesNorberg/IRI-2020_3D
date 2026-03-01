c-----------------------------------------------------------------------
c
c test program for the iri_web subroutine modified from iritest.for
c reads input parameters Lat, Long, NmF2, hmF2, NmE, hmE, B0, B1 from input file
c reads altitudes from input as beginning end and stepsize

c returns electron density profiles in Ne/cm^3 for all coordinate points
c Uses standard IRI or predefined switches
c takes parameters as inputs in the following order:
c lat long year mmdd h vbeg vend step switches(0/1) 
c e.g. ./iri_ne 65 25 2021 1110 12 0 1000 10 0
c
c compiling e.g: gfortran-mp-14 -o iri_ne iritest_ne.for irisub.for irifun.for iritec.for 
c                                  iridreg.for igrf.for cira.for iriflip.for rocdrift.for
c-----------------------------------------------------------------------

      INTEGER           pad1(6),jdprof(77),piktab, seconds
      DIMENSION         outf(20,1000),oar(100,1000),jfi(6)
      LOGICAL		    jf(50),rzino,igino
      CHARACTER*2       timev(2)
      CHARACTER*3       uni(86),sopt,seopt
      CHARACTER*4       IMZ(8),MAP,xtex,coorv(2),plpa
      CHARACTER*5       ITEXT(8),tsopt
      CHARACTER*6       pna(86)
      CHARACTER*7       popt
      CHARACTER*8       bopt,topt,tiopt,pplas
      CHARACTER*9       pname(8)
      CHARACTER*10      dopt,hopt
      CHARACTER*11      iopt,rzopt,igopt,f8opt,fdopt
      CHARACTER*16      f1opt
      CHARACTER*32      arg
      CHARACTER*32      output_filename 
      CHARACTER*84      input_filename
      CHARACTER*4       year_str, date_str
      CHARACTER*5       second_str
      CHARACTER*256     line
      INTEGER           iostat 
      REAL              lat, lon, NmF2, hmF2, NmE, hmE
      REAL              B0, B1    
      
      INTEGER           iostat_alt, alt_count
	  REAL              alt_values(1000)        

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

        CALL read_ig_rz
        CALL readapf107
        
        nummax=1000
        
        DO i=1,100
        oar(i,1)=0.0
        ENDDO

c user input of IRI input parameters

        CALL getarg(1, arg)
        READ(arg, *) iy
        CALL getarg(2, arg)
        READ(arg, *) imd
c       CALL getarg(5, arg)
c       READ(arg, *) iut
        iut = 1
        CALL getarg(3, arg)
        READ(arg, *) hour
        hx=0              ! height/km
        ivar=1
c       (1/2/../8 for height/lat/long/year/month/day/day of year/hour)
C       CALL getarg(4, arg)
C       READ(arg, *) vbeg
C       CALL getarg(5, arg)
C       READ(arg, *) vend
C       CALL getarg(6, arg)
C       READ(arg, *) vstp
        piktab=0

        htec_min=0         ! lower height [km] for TEC
        htec_max=0         ! upper height [km] for TEC
c       (0,0 for no TEC)'
        CALL getarg(4, arg)
        READ(arg, *) jchoice ! 0: default switches

c     convert hours to deconds to include decimal hours for 00 hour    
      seconds = NINT(hour * 3600)
     
      WRITE(year_str, '(I4)') iy         
      WRITE(date_str, '(I4)') imd      
      WRITE(second_str, '(I5)') seconds     

C     output_filename = year_str//date_str//second_str//'_ne.txt'
C     input_filename = year_str//date_str//second_str//'_param.txt'
      output_filename = 'ne.txt'
      input_filename = 'param.txt'


      PRINT *, 'input:',  input_filename
      PRINT *, 'output:', output_filename

      OPEN(UNIT=11, FILE=output_filename, STATUS='UNKNOWN')

      WRITE(11,8191) 'lat', 'long', 'alt', 'Ne'
8191  FORMAT(2X,A5,A7,A9,A7)


c Input from txt file
      OPEN(UNIT=13, FILE='alt.txt', STATUS='OLD', IOSTAT=iostat_alt)
      IF (iostat_alt /= 0) THEN
          PRINT *, 'No altitude grid available in alt.txt'
          STOP
      ENDIF

	  alt_count = 0
	  DO WHILE (iostat_alt == 0)
	    READ(13, *, IOSTAT=iostat_alt) alt
		IF (iostat_alt == 0) THEN
		  alt_count = alt_count + 1
		  alt_values(alt_count) = alt
		END IF 	 
	  END DO
      CLOSE(13)  
      
C     These are not used as altitude grid, but are required for condition in  irisub.for
C     to indicate that there are several altitude steps
C     After that alt_values are used
      vbeg = alt_values(1) 
      vend = alt_values(alt_count)
      vstp = 10

      OPEN(UNIT=12, FILE=input_filename, STATUS='OLD', IOSTAT=iostat)
      IF (iostat /= 0) THEN
          PRINT *, 'Error opening file: ', input_filename
          STOP
      ENDIF
    

C     PRINT *, 'IOSTAT1: ', iostat                

      READ(12, *)

c     Set up jf flags once before the main loop (constant across iterations)
      DO i=1,50
        jf(i)=.true.
      ENDDO
      jf(2)=.false.       ! skip Te, Ti, Tn (not needed for Ne output)
      jf(3)=.false.       ! skip ion composition (not needed for Ne output)
      jf(4)=.false.       ! t=B0table f=other models
      jf(5)=.false.       ! t=CCIR  f=URSI foF2 model
      jf(6)=.false.       ! t=DS95+DY85   f=RBV10+TBT15
      jf(23)=.false.      ! t=AEROS/ISIS f=TTS Te with PF10.7
      jf(29)=.false.      ! t=old  f=New Topside options
      jf(30)=.false.      ! t=corr f=NeQuick topside
      jf(33)=.false.      ! t=auroral boundary   f=off
      jf(34)=.false.      ! t=messages on f=off
      jf(35)=.false.      ! t=auroral E-storm model on f=off
      jf(39)=.false.      ! t=M3000F2 model f=new hmF2 models
      jf(47)=.false.      ! t=CGM on  f=CGM off

      jm=0                ! (=0/1,geog/geom)
      num1=alt_count
      numstp=iabs(num1)
      IF(numstp.GT.nummax) numstp=nummax

      DO 100 WHILE (iostat == 0)

             IF(jchoice.eq.0) THEN
               READ(12, *, IOSTAT=iostat) lat, lon
               IF (iostat == 0) THEN
                 xlat = lat
                 xlon = lon
               ENDIF
             ELSE
               READ(12, *, IOSTAT=iostat) lat, lon, NmF2, hmF2,
     +  NmE, hmE, B0, B1
               IF (iostat == 0) THEN
c                Reset per-parameter flags to defaults before per-row overrides
                 jf(8) = .true.
                 jf(9) = .true.
                 jf(15) = .true.
                 jf(16) = .true.
                 jf(26) = .true.
                 jf(43) = .true.
                 jf(44) = .true.

c                Per-parameter: use external value if > 0, else IRI model
c                Sentinel value -1 means "use IRI internal default"
                 IF(NmF2.gt.0) THEN
                   jf(8) = .false.
                   oar(1, 1) = NmF2
                   jf(26) = .false.   ! storm model off when user NmF2
                 ENDIF
                 IF(hmF2.gt.0) THEN
                   jf(9) = .false.
                   oar(2, 1) = hmF2
                 ENDIF
                 IF(NmE.gt.0) THEN
                   jf(15) = .false.
                   oar(5, 1) = NmE
                 ENDIF
                 IF(hmE.gt.0) THEN
                   jf(16) = .false.
                   oar(6, 1) = hmE
                 ENDIF
                 IF(B0.gt.0) THEN
                   jf(43) = .false.
                   oar(10, 1) = B0
                 ENDIF
                 IF(B1.gt.0) THEN
                   jf(44) = .false.
                   oar(35, 1) = B1
                 ENDIF

                 xlat = lat
                 xlon = lon
               ENDIF
             ENDIF

c end of user input
      IF (iostat == 0) THEN
        hxx=hx
        jmag=jm
        mmdd=imd

c calling IRI subroutine

        phour=hour
        CALL iri_web(jmag,jf,xlat,xlon,iy,mmdd,iut,hour,
     &    hxx,htec_min,htec_max,ivar,vbeg,vend,vstp,outf,oar)

c output

        DO 1234 li=1,numstp
        
		jne = outf(1,li) / 1.e6
		IF(outf(1,li).lt.0) jne = 0
C        PRINT *, 'Lat:', lat, 'Lon:', lon, 'Ne', jne
        
        WRITE(11,7117) xlat, lon, alt_values(li),jne
7117    FORMAT(F7.2, 1X, F7.2, 1X, F7.1, 1x, I10)


1234  CONTINUE

      ENDIF
100   CONTINUE

      CLOSE(12)

      CLOSE(11)

C     PRINT *, 'Data written to file:', output_filename
c    print *,'Enter 0 to exit or 1 to generate another profile?' 
c        READ(5,*) icontinue
c        IF (icontinue.gt.0) goto 1
c		print *,oar(51,1),oar(52,1),oar(83,1)    
            STOP
            END
