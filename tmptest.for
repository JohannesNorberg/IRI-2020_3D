      PROGRAM ReadData
      IMPLICIT NONE

      INTEGER           iostat
      REAL              lat, lon, NeF2, hmF2

      ! Avaa tiedosto yksikkönumerolla 10
      OPEN(UNIT=10, FILE="input.txt", STATUS='OLD', IOSTAT=iostat)
      IF (iostat /= 0) THEN
          PRINT *, 'Error opening file: input.txt'
          STOP
      END IF

      ! Ohita otsikkorivi
      READ(10, *)

      ! Lue tiedoston rivit
      DO 100 WHILE (iostat == 0)
          READ(10, *, IOSTAT=iostat) lat, lon, NeF2, hmF2
          IF (iostat == 0) THEN
              PRINT *, 'Lat:', lat, 'Lon:', lon
              PRINT *, 'NeF2: ', NeF2, 'hmF2: ', hmF2
          END IF
100   CONTINUE

      ! Sulje tiedosto
      CLOSE(10)

      PRINT *, 'File reading completed.'
      END PROGRAM ReadData