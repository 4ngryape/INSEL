
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0109(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM

      INTEGER          SOY, SOY_start, SOY_end, status, T_HP_min,
     &                 T_HP_min2



      PARAMETER       (BNAMES = 'UBWPSTATUS'
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
     &                 T_ambient
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

      IF (IN(2) .EQ. 0) THEN
        HP_use = 0
      Else
        HP_use = 1
      END IF

      IF (HP_use .GT. RP(1)) THEN
        SOY_start = SOY
      ELSE IF (HP_use .LT. RP(1)) THEN
        SOY_end = SOY
      END IF

C     ---------muss ausschalten-----------------------------------------
      IF ((T_storage1 .GE. (T_setstorage1+bp(4))
     & .OR. T_storage2 .GE. T_setstorage2+bp(5)) .AND.HP_use.EQ.1) THEN

        status = 7

C     ---------muss anschalten-----------------------------------------
      ELSE IF (T_storage1 .LT. T_setstorage1
     & .AND. T_ambient .LT. BP(7) .OR. T_storage2 .LT.
     & T_setstorage2) THEN

        status = 8

C     ---------kann aus anschalten------------------------------------------
      ELSE IF (SOY-SOY_end .GE. T_HP_min2 .AND. HP_use .EQ. 0) THEN
        status = 1

C     ---------kann ausschalten-----------------------------------------
      ELSE IF (SOY-SOY_start .GE. T_HP_min .AND. HP_use .EQ. 1) THEN

        status = 2

C     ---------muss an lassen-------------------------------------------
      ELSE IF (SOY-SOY_start .LT. T_HP_min .AND. HP_use .EQ. 1) THEN

        status = 3

C     ---------muss aus lassen------------------------------------------
      ELSE IF (SOY-SOY_end .LT. T_HP_min2 .AND. HP_use .EQ. 0) THEN

        status = 4

C     ---------kann aus lassen------------------------------------------
      ELSE IF (SOY-SOY_end .GE. T_HP_min2 .AND. HP_use .EQ. 0) THEN

        status = 5

C     ---------kann an lassen------------------------------------------
      ELSE IF (SOY-SOY_start .GE. T_HP_min .AND. HP_use .EQ. 1) THEN

        status = 6


      END IF

      RP(1) = HP_use

      out(1) = status

      RETURN


      END


