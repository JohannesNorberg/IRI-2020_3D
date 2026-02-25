      PROGRAM IRITEST_CLEAN
C     ---------------------------------------------------------------
C     Cleaned test program for the IRI_WEB subroutine.
C     Reads latitude and longitude pairs from "lat_lon.txt",
C     calls IRI_WEB for each pair, and writes selected output
C     parameters to "param.txt".
C     ---------------------------------------------------------------

      IMPLICIT NONE

C     Declarations
      INTEGER nummax, numstp, i, iostat, li
      REAL*8 outf(20,1000), oar(100,1000)
      REAL*8 lat, lon, xlat, xlon, vbeg, vend, vstp
      REAL*8 hour_input, hxx, num1, xcor
      INTEGER jm, iut, iy, imd, ivar, htec_min, htec_max
      CHARACTER*32 filename
      LOGICAL jf(50)
      INTEGER pad1(6)
      DATA pad1 /1,2,5,6,10,35/

C     File names and constant input parameters
      filename = 'param.txt'
      jm = 0                ! Use geographic coordinates
      iut = 1               ! Use UT time
      iy = 2020             ! Year
      imd = 1015            ! Month/day in mmdd format
      hour_input = 12.0D0   ! Fixed hour value
      hxx = 300.0D0         ! Altitude (km)
      ivar = 2              ! Varying parameter index (here latitude)
      vbeg = 60.0D0         ! Starting latitude
      vend = 60.0D0         ! Ending latitude (yksi arvo)
      vstp = 1.0D0          ! Step size (ei iteroida)
      htec_min = 0          ! No TEC integration
      htec_max = 0
      nummax = 1000

C     Initialize default jf parameters: all TRUE except recommended ones set to FALSE.
      DO i = 1, 50
         jf(i) = .TRUE.
      ENDDO
      jf(4) = .FALSE.
      jf(5) = .FALSE.
      jf(6) = .FALSE.
      jf(23) = .FALSE.
      jf(28) = .FALSE. 
      jf(30) = .FALSE.
      jf(33) = .FALSE.
      jf(35) = .FALSE.
      jf(39) = .FALSE.
      jf(45) = .FALSE.
      jf(47) = .FALSE.

C     Initialize IRI indices by reading necessary files.
      CALL READ_IG_RZ
      CALL READAPF107

C     Initialize output array oar: set first column to zero.
      DO i = 1, 100
         oar(i,1) = 0.0D0
      ENDDO

C     Open output file "param.txt"
      OPEN(UNIT=11, FILE=filename, STATUS='UNKNOWN')
      WRITE(11,*) 
     &    'lat   long   NmF2    hmF2    NmF1    hmF1    NmE    hmE'
C     Open input file "lat_lon.txt" for reading latitude-longitude pairs.
      OPEN(UNIT=12, FILE='lat_lon.txt', STATUS='OLD', IOSTAT=iostat)
      IF (iostat /= 0) THEN
         PRINT *, 'Error opening lat_lon.txt'
         STOP
      ENDIF

C     Skip header line (olettaen, että tiedostossa on otsikko)
      READ(12, *)

C     Loop over coordinate pairs (max. 50 paria tässä esimerkissä)
      DO i = 1, 50
         READ(12, *, IOSTAT=iostat) lat, lon
         IF (iostat /= 0) EXIT

         xlat = lat
         xlon = lon
         jm = 0

C        In this test, the varying parameter is latitude.
         vbeg = xlat
         vend = xlat
         vstp = 1.0D0

C        Use fixed hour value
         hour_input = 12.0D0

C        Calculate number of steps (yksi profiili)
         num1 = (vend - vbeg) / vstp + 1
         numstp = ABS(num1)
         IF (numstp .GT. nummax) numstp = nummax

C        Call IRI_WEB with the set parameters.
         CALL IRI_WEB(jm, jf, xlat, xlon, iy, imd, iut, hour_input, 
     &       hxx, htec_min, htec_max, ivar, vbeg, vend, vstp, outf, oar)

C        Set xcor for output (esim. sama kuin vbeg)
         xcor = vbeg
         li = 1

C        Write selected output parameters to "param.txt"
         WRITE(11,3919) xcor, xlon, oar(pad1(1),li), oar(pad1(2),li), 
     &       oar(pad1(3),li), oar(pad1(4),li), oar(pad1(5),li), 
     &       oar(pad1(6),li)      
      END DO

      CLOSE(UNIT=12)
      CLOSE(UNIT=11)
      STOP
3919  FORMAT(F7.2,1X,F7.2,6(1X,1PE9.2))
      END