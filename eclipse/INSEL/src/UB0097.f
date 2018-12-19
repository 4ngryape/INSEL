
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0097(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM

      REAL Time
      INTEGER io, minute, x, I

      Real :: Random
      Integer :: Seed
      Integer :: OldSeed = 0
      Integer, Parameter :: C1 = 19423
      Integer, Parameter :: C2 = 811
      Save OldSeed

      PARAMETER       (BNAMES = 'UBRAND'
     &,                OPM    = 1
     &,                INMIN  = 1
     &,                INS    = 4
     &,                OUTS   = 1
     &,                IPS    = 45
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 0
     &,                BPS    = 0
     &,                SPMIN  = 1
     &,                SPS    = 1
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1),STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),II
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1),
     &                 Data(999999,10)
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF

      minute = (in(1)-1)*24*60

C---- Zufallsgenerierung------------------------------------------------




      IF (rp(1) .NE. in(1)) THEN

      CALL CPU_TIME(TIME)
      Seed = TIME


         If (OldSeed .EQ. 0) OldSeed = Seed
            OldSeed = Mod(C1 * OldSeed, C2)
            RANDOM = 1.0 * OldSeed / C2



         x = aint(RANDOM*10)

      END IF

!C-----Daten einlesen----------------------------------------------------


        I = in(2)


        OPEN(20,FILE=sp(1), STATUS='OLD', ACTION='READ',
     &  access='direct', RECL=102,FORM='FORMATTED',IOSTAT=IO)

        READ (20,'(10F10.3)',REC=I) Data(I,0),Data(I,1), Data(I,2),
     &  Data(I,3), Data(I,4), Data(I,5), Data(I,6), Data(I,7),
     &  Data(I,8), Data(I,9)






       out(1)=data(I,x)



       rp(1) = in(1)

        CLOSE(20)

      RETURN
      END


