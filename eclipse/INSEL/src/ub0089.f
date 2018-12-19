      SUBROUTINE UB0089(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBMPC2'
     &,                OPM    = 1
     &,                INMIN  = 0
     &,                INS    = 10
     &,                OUTS   = 2
     &,                IPS    = 10
     &,                RPS    = 2
     &,                DPS    = 0
     &,                BPMIN  = 3
     &,                BPS    = 10
     &,                SPMIN  = 0
     &,                SPS    = 3
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1), s, I, io, read, MOY, set, x,
     &                 set_out(9999), minute, tset, hour, HOY, time

      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1), C,
     &                 Qpv(9999), C_batt, cop, qbatt(1440),
     &                 Data(9999,10), maxprice, gain, minqbatt,
     &                 cset, CN(1440)

C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
         x = 0
         rp(1)=0
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------

      IF (x .EQ. 0) THEN
         x = 1
         maxprice = 0
         s = 0
         read = 0
         minute = in(3)
         tset = 0
         qbatt = 0
         set = 0
      END IF

      HOY = (in(1)-1)*24
      hour = in(2)
      C_batt = bp(1)




      IF ((hour .EQ. 12) .and. (in(2).gt.rp(1))) THEN

        maxprice = 0
        s = 0
        read = 0
        minute = in(3)
        tset = 0
        qbatt = 0
        set = 1

        rp(2) = 0

!
      END IF

C---- Einlesen der PV Prognosedaten-----------------------------------


!      IF (read .EQ. 0)  then
!
!      MOY = IN(3)
!
!
!       OPEN(20,FILE=sp(1), STATUS='OLD', ACTION='READ', ACCESS='DIRECT',
!     & RECL=62,FORM='FORMATTED',IOSTAT=IO)
!
!       DO I = 1, 1440
!
!       READ (20,'(6F10.3)',REC=(I+MOY-1)) Data(I,1),Data(I,2),
!     &  Data(I,3), Data(I,4), Data(I,5), Data(I,6)
!
!!     Pheat Building, PheatTWW, P_PV, Pel_household, Tambient, Spotprice
!       END DO
!
!       close(20)
!
!!
!      !C----Max Spotmarket price--------
!
!        DO I = 1, 1440
!
!
!!            qbatt(I) = Data(I,2)
!
!            IF (Data(I,6) .GT. maxprice) THEN
!
!            maxprice = Data(I,6)
!            tset = I
!
!            END IF
!
!
!
!       END DO



!     1:ElPheat Building+ TWW, 2:C_Batt, 3:P_PV, 4:Pel_household, 5:Tambient, 6:Spotprice ALT:Pheat Building, PheatTWW, P_PV, Pel_household, Tambient, Spotprice

       read = 1


       C = in(4)/1000

       time = (in(3)-minute)
       CN(time) = in(4)/1000


       IF (time .GT. 960 .AND. CN(time).GT.CN(time-1).AND.set.EQ.1)THEN

       out(2) = 1
       set = 2

       ELSE IF (time.GT.960.AND.CN(time).GT.CN(time-1).AND.set.EQ.2)THEN

       out(2) = 1
       set = 3

       ELSE IF(time .EQ. 1439.AND.set.EQ.1)THEN

       out(2) = 1
       set = 3


       ELSE
       out(2) = 0


       END IF




       out(1) = set !Data((in(3)-minute),2)
!

       rp(1)=in(2)

       END
