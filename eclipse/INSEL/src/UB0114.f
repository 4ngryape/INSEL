
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0114(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM

      INTEGER          SOY, SOY_start, SOY_end, status, T_HP_min,
     &                 T_HP_min2



      PARAMETER       (BNAMES = 'UBWPSTATUS2'
     &,                OPM    = 1
     &,                INMIN  = 0
     &,                INS    = 7
     &,                OUTS   = 1
     &,                IPS    = 45
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 6
     &,                BPS    = 6
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1),STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),II
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1),
     &                 HP_use, T_storage1, T_storage2, T_setstorage1,
     &                 T_setstorage2, D_QStorage1, D_QStorage2, P_HP,
     &                 T_ambient, heat
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1 ) THEN
C           Constructor call

         ELSE
C           Destructor call
         END IF
         RETURN
      END IF

      SOY = IN(1)
      T_storage1 = IN(3)
      T_storage2 = IN(4)
      T_setstorage1 = IN(5)
      T_setstorage2 = IN(6)
      D_QStorage1 = BP(1)
      D_QStorage2 = BP(2)
      P_HP = BP(3)
      T_HP_min = 300
      T_HP_min2 = 300
      T_ambient = IN(7)

      heat = rp(1)

      HP_use = IN(2)

      IF (HP_USE .EQ. 0 .AND. T_storage2 .GE. T_setstorage2
     & .AND. T_storage1 .GE. T_setstorage1) THEN
         status = 0
      ELSE IF (HP_USE .EQ. 0 .AND. T_storage2 .LT. T_setstorage2+BP(5)
     &.OR. HP_USE .EQ. 0 .AND. T_storage1 .LT. T_setstorage1+BP(4)) THEN
         status = -1
      END IF

      IF (HP_USE .NE. 1 .AND. HP_USE.NE. 0) THEN
        status = -1
      END IF


C     ---------Beladen Speicher 2--------------------------------------
      IF (HP_use .EQ. 1 .AND. T_storage2 .LT. T_setstorage2+BP(5)
     & .AND. status .eq. 0) THEN

        status = 2
        heat = 1
        SOY_start = SOY


      ELSE IF (T_storage2 .GE. T_setstorage2+BP(5) .AND. status .EQ. 2
     & .AND. SOY-SOY_start .GE. T_HP_min
     & .AND. T_storage1 .GE. T_setstorage1+BP(4)) THEN

        status = 0
        heat = 0
        SOY_end = SOY

      ELSE IF (T_storage2 .GE. T_setstorage2+BP(5) .AND. status .EQ. 2
     & .AND. T_storage1 .LT. T_setstorage1+BP(4)) THEN

        status = 1
        heat = 1


      END IF

C     ---------Beladen Speicher 1--------------------------------------
      IF (HP_use .EQ. 1 .AND. T_storage1 .LT. T_setstorage1+BP(4)
     & .AND. T_ambient .LT. BP(7) .AND. SOY-SOY_end .GE. T_HP_min2
     & .AND. heat .eq. 0) THEN

        status = 1
        heat = 1
        SOY_start = SOY

      ELSE IF (T_storage1 .GE. T_setstorage1+BP(4) .AND. heat .EQ. 1
     & .AND. SOY-SOY_start .GE. T_HP_min
     & .AND. T_storage2 .GE. T_setstorage2+BP(5)) THEN

        status = 0
        heat = 0
        SOY_end = SOY

      ELSE IF (T_storage1 .GE. T_setstorage1+BP(4) .AND. heat .EQ. 1
     & .AND. T_storage2 .LT. T_setstorage2+BP(5)) THEN

        status = 2
        heat = 1


      END IF



      RP(1) = heat
      out(1) = status

      RETURN


      END


