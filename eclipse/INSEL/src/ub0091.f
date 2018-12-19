      SUBROUTINE UB0091(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBMPC'
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
      INTEGER          IP(IPS+1), s, I, io, read, MOY, set, x, toff,
     &                 set_out(9999), minute, tset, hour, HOY, time,
     &                 stop

      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1), C,
     &                 Qpv(9999), C_batt, cop, qbatt(1440),
     &                 Data(9999,10), maxprice, gain, minqbatt,
     &                 cset

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
         toff = 4*60
      END IF

      HOY = (in(1)-1)*24
      hour = in(2)
      C_batt = bp(1)
      time = (in(3)-minute)


      IF ((hour .EQ. 0) .and. (in(2).ne.rp(1))) THEN

        maxprice = 0
        s = 0
        read = 0
        minute = in(3)
        tset = 0
        qbatt = 0
        set = 1
        !minqbatt = C_batt
        rp(2) = 0
        stop = 0

!
      END IF

C---- Einlesen der PV Prognosedaten-----------------------------------


       IF (read .EQ. 0)  then

       MOY = IN(3)


       OPEN(20,FILE=sp(1), STATUS='OLD', ACTION='READ', ACCESS='DIRECT',
     & RECL=62,FORM='FORMATTED',IOSTAT=IO)

       DO I = 1, 1440

       READ (20,'(6F10.3)',REC=(I+MOY-1)) Data(I,1),Data(I,2),
     &  Data(I,3), Data(I,4), Data(I,5), Data(I,6)

C      1:ElPheat Building+ TWW, 2:C_Batt, 3:P_PV, 4:Pel_household, 5:Tambient, 6:Spotprice ALT:Pheat Building, PheatTWW, P_PV, Pel_household, Tambient, Spotprice
       END DO

       close(20)

       read = 1


      !C----Bestimmen ob Batterie voll beladen wird--------

        DO I = 1, 1440

            IF (Data(I,2) .GE. C_batt .AND. stop .lt. 1) THEN

            tset = I
            stop = 1

            END IF

        END DO

       END IF



       IF (in(4)/1000 .EQ. C_Batt) THEN

            out(1) = 2

       ELSE IF (time .GT. (tset - toff) .AND. time .LT. tset) then

            out(1) = 1

       ELSE
            out(1) = 0

       END IF

       rp(1)=in(2)

       END
