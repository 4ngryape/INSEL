C-----------------------------------------------------------------------
C #Begin
C #Block UBFBHCON
C #Description
C    Einfacher Zweipunktregler
C #Layout
C    #Inputs      5
C    #Outputs     3
C    #Parameters  3
C    #Strings     0
C    #Group       3
C #Details
C    #Inputs
C       #IN(1) control_temperature Zone [Celsius]
C       #in(4) Setpoint_temperature Zone [Celsius]
C        #in(5) control_temperature Storage1 [Celsius]
C       #in(6) Setpoint_temperature Storage1 [Celsius]
C       #in(7) control_temperature Storage2 [Celsius]
C       #in(8) Setpoint_temperature Storage2 [Celsius]
C       #in(9) Massenstrom [kG/s]
C
C    #Outputs
C       #OUT(1) mdot Zone [kG/s]
C       #out(4) mdot Storage 1 [kG/s]
C       #out(5) mdot Storage 2 [kG/s]

C    #Parameters
C       #bp(1) deltaT Zone [Kelvin]
C       #bp(2) deltaT Storage1 [Kelvin]
C       #bp(3) deltaT Storage2 [Kelvin]
C    #Strings
C       #None
C #Internals
C    #Integers
C       #IP(1)  Return code
C       #IP(2)  Call mode
C               \begin{detaillist}
C                  \item[-1] Identification call
C                  \item[0]  Standard call
C                  \item[1]  Constructor call
C                  \item[2]  Destructor call
C               \end{detaillist}
C       #IP(3)  Operation mode
C       #IP(4)  User defined block number
C       #IP(5)  Number of current block inputs
C       #IP(6)  Jump parameter
C       #IP(7)  Debug level
C       #IP(8..10)  Reserved
C    #Reals
C       #None
C    #Doubles
C       #None
C #Dependencies
C    #Subroutine ID
C #Authors
C    Ruben Pesch
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0098(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBFBHCON'
     &,                OPM    = 1
     &,                INMIN  = 13
     &,                INS    = 20
     &,                OUTS   = 10
     &,                IPS    = 10
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 3
     &,                BPS    = 10
     &,                SPMIN  = 0
     &,                SPS    = 3
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),x, mode, heat, mode_heat_set,
     &                 mode_tww_set, mode_heat_delta, mode_tww_delta,
     &                 min_heat, min_tww, min_room, s, roomheat, I, io,
     &                 t_battload, hour, set_batt, z
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1), set,
     &                 Gain(8760), GainSum, GainSumSet, Azone1, Azone2,
     &                 Azone3, Atotal


C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
         rp(1)=0
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------

      Azone1 = 100.66
      Azone2 = 98.02
      Azone3 = 112.07
      Atotal = Azone1 + Azone2 + Azone3

      min_room = bp(4)
      min_heat = bp(5)
      min_tww = bp(6)
      hour = (in(20)-1)*24

      mode_heat_delta = bp(7)
      mode_tww_delta = bp(8)
      set = in(14)
      GainSum = 0
      GainSumSet = 10
      t_battload = 0

      set_batt = 0


C---- WP Betrieb bei vollem Batteriespeicher------------


!      IF((in(13).GT.0).AND.(in(12).eq.1)) THEN
!      set = 2
!      ELSE
!      set = in(14)
!      END IF




C---- Bestimmen des Batterieladezeitpunktes f�r Peak Shaving------------

      IF (set_batt .EQ.1) then
        I = 24 + hour

        DO while (I .GT. hour)

        GainSum = GainSum + Gain(I)

       IF (GainSumSet .LT. GainSum) then
          t_battload = (I - hour)

c          IF (t_battload .gt. 11) THEN
c              t_battload = 11
c          END IF

c        out(10) = t_battload
        I = 0
        END IF

        I = I - 1


        END DO

      END IF


C---- Bestimmen der Betriebsmodie d. thermischen Speicher f�r max Eigenverbrauch
!
!        IF (set_batt .EQ.2) then
!        I = hour
!
!        DO while (I .LT. (hour+23))
!
!        GainSum = GainSum + Gain(I)
!
!       IF (GainSumSet .LT. GainSum) then
!          t_battload = (I - hour)
!
!c          IF (t_battload .gt. 11) THEN
!c              t_battload = 11
!c          END IF
!
!        out(10) = t_battload
!        I = 0
!        END IF
!
!        I = I + 1
!
!
!        END DO
!
!      END IF


C-----------------------------------------------------------------------
C----- Battery normal mode----------------------------------------------

      IF (set_batt.EQ.0) then

       IF ((in(12).lt.1).and.(in(12).gt.0)) then
            out(8)=1
            out(9)=1
         END IF
         IF (in(12).eq.1) then
            out(8)=0
            out(9)=1
         END IF
         IF (in(12).eq.0) then
            out(8)=1
            out(9)=0
         END IF
      END IF


C----- Battery Peak Shaving---------------------------------------------

      IF (set_batt .EQ.1) then

         IF ((in(12).lt.1).and.(in(12).gt.0)
     &    .and. (t_battload .LE. in(15))) then
            out(8)=1
            out(9)=1
         END IF
         IF (t_battload .GT. in(15)) then
            out(8)=0
            out(9)=1
         END IF
         IF (in(12).eq.1) then
            out(8)=0
            out(9)=1
         END IF
         IF ((in(12).eq.0)
     &   .and. (t_battload .LE. in(15))) then
            out(8)=1
            out(9)=0
         END IF
      END IF

C----- Battery mode 2----------------------------------------------

      IF (set_batt .EQ.2) then
        IF ((in(12).lt.1).and.(in(12).gt.0)) then
            out(8)=1
            out(9)=0
         END IF
         IF (in(10).eq.1) then
            out(8)=0
            out(9)=0
         END IF
         IF (in(10).eq.0) then
            out(8)=1
            out(9)=0
         END IF
      END IF


C---- Heizgrenztemperatur-----------------------------------------------
      IF (in(16) .GT. in(19)) then

        roomheat = 0

        else

        roomheat = 1

      END IF
C---- ------------------------------------------------------------------

      IF (set .ge. rp(1)) then
            s = 1
            else
            s = 0
      END IF

      IF (s .ne. 1) then

            OUT(4) = 0
            OUT(5) = 0
            OUT(7) = 0
            heat = 0
      END IF



C----- Mode 0 "standard" -----------------------------------------------
        IF (set .EQ. 0 .and. s .eq. 1) THEN

C----- Thermal ---------------------------------------------------------
C----- Pufferspeicher TWW ----------------------------------------------
         IF ((in(7) .LT. in(8)) .and. (heat .eq. 0)) THEN

            out(5) = in(9)
            out(7) = in(9)
            heat = 1


         ELSE IF (in(7) .GT. (in(8)+bp(3)) .and. (heat .eq. 1))THEN

            out(5) = 0
            out(7) = 0
            heat = 0

         END IF

C----- Raumheizung -----------------------------------------------------
C----- Zone 1-----------------------------------------------------------

         IF (IN(1).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(1) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(1) .GT. (in(4)+BP(1))) THEN
            OUT(1) = 0
         END IF

C----- Zone 2-----------------------------------------------------------

         IF (IN(2).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(2) = in(9) * (Azone2 / Atotal)
         ELSE IF (IN(2) .GT. (in(4)+BP(1))) THEN
            OUT(2) = 0
         END IF

C----- Zone 3-----------------------------------------------------------

         IF (IN(3).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(3) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(3) .GT. (in(4)+BP(1))) THEN
            OUT(3) = 0
         END IF

C----- Pufferspeicher Heizung-------------------------------------------
         IF (in(5).LT.in(6).and.heat.eq.0.AND.roomheat.EQ.1) THEN

             out(4) = in(9)
             out(7) = in(9)
             heat = 2


          ELSE IF (in(5) .GE. (in(6)+BP(2)) .and. (heat .eq. 2)) THEN

             out(4) = 0
             out(7) = 0
             heat = 0

          END IF
       END IF


C----- Mode 1 "positive DR"---------------------------------------------
       IF ((set.EQ.1) .and. (s .eq. 1)) THEN

C----- Thermal ---------------------------------------------------------
C----- Pufferspeicher TWW ----------------------------------------------
         IF (in(7).LT.(in(8)-min_tww).and.heat.eq.0) THEN

            out(5) = in(9)
            out(7) = in(9)
            heat = 1


         ELSE IF (in(7) .GT. (in(8)+bp(3)) .and. (heat .eq. 1))THEN

            out(5) = 0
            out(7) = 0
            heat = 0

         END IF

C----- Raumheizung -----------------------------------------------------
C----- Zone 1-----------------------------------------------------------

      IF(IN(1).LT.(in(4)-min_room).AND.roomheat.EQ.1) THEN
            OUT(1) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(1) .GT. (in(4)+BP(1))) THEN
            OUT(1) = 0
         END IF

C----- Zone 2-----------------------------------------------------------

      IF(IN(2).LT.(in(4)-min_room).AND.roomheat.EQ.1) THEN
            OUT(2) = in(9) * (Azone2 / Atotal)
         ELSE IF (IN(2) .GT. (in(4)+BP(1))) THEN
            OUT(2) = 0
         END IF

C----- Zone 3-----------------------------------------------------------

      IF(IN(3).LT.(in(4)-min_room).AND.roomheat.EQ.1) THEN
            OUT(3) = in(9) * (Azone3 / Atotal)
         ELSE IF (IN(3) .GT. (in(4)+BP(1))) THEN
            OUT(3) = 0
         END IF

C----- Pufferspeicher Heizung-------------------------------------------
         IF (in(5) .LT. (in(6)-min_heat) .and. heat .eq. 0
     &    .AND. roomheat .EQ. 1) THEN

             out(4) = in(9)
             out(7) = in(9)
             heat = 2

         ELSE IF (in(5) .GT. (in(6)+BP(2)) .and. (heat .eq. 2)) THEN

             out(4) = 0
             out(7) = 0
             heat = 0

         END IF
       END IF


C----- Mode 2 "negative DR"---------------------------------------------

       IF ((set.EQ.2) .and. (s .eq. 1)) then

C----- Thermal ---------------------------------------------------------
C----- Pufferspeicher TWW ----------------------------------------------

         IF (in(7) .LT. (in(18)).and.(heat .eq. 0)) THEN

            out(5) = in(9)
            out(7) = in(9)
            heat = 1


         ELSE IF (in(7).GT.(in(18)+mode_tww_delta).and.(heat.eq.1))THEN

            out(5) = 0
            out(7) = 0
            heat = 0

         END IF

C----- Raumheizung -----------------------------------------------------
C----- Zone 1-----------------------------------------------------------

         IF (IN(1) .LT. (in(4)+bp(9)).AND. roomheat .EQ. 1) THEN
            OUT(1) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(1) .GT. (in(4)+BP(1)+bp(9))) THEN
            OUT(1) = 0
         END IF

C----- Zone 2-----------------------------------------------------------

         IF (IN(2) .LT. (in(4)+bp(9)).AND. roomheat .EQ. 1) THEN
            OUT(2) = in(9) * (Azone2 / Atotal)
         ELSE IF (IN(2) .GT. (in(4)+BP(1)+bp(9))) THEN
            OUT(2) = 0
         END IF

C----- Zone 3-----------------------------------------------------------

         IF (IN(3) .LT. in((4)+bp(9)).AND. roomheat .EQ. 1) THEN
            OUT(3) = in(9) * (Azone3 / Atotal)
         ELSE IF (IN(3) .GT. (in(4)+BP(1)+bp(9))) THEN
            OUT(3) = 0
         END IF

C----- Pufferspeicher Heizung-------------------------------------------
         IF (in(5).LT.in(17).and.heat.eq.0 .AND. roomheat .EQ. 1) THEN

             out(4) = in(9)
             out(7) = in(9)
             heat = 2

        ELSE IF (in(5).GE.(in(17)+mode_heat_delta).and.(heat.eq.2))THEN

             out(4) = 0
             out(7) = 0
             heat = 0

         END IF
       END IF

C----- Mode 3 "ON" -----------------------------------------------
        IF (set .EQ. 3 .and. s .eq. 1) THEN

C----- Thermal ---------------------------------------------------------
C----- Pufferspeicher TWW ----------------------------------------------
         IF (heat .eq. 0) THEN

            out(5) = in(9)
            out(7) = in(9)
            heat = 1

         END IF

C----- Raumheizung -----------------------------------------------------
C----- Zone 1-----------------------------------------------------------

         IF (IN(1).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(1) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(1) .GT. (in(4)+BP(1))) THEN
            OUT(1) = 0
         END IF

C----- Zone 2-----------------------------------------------------------

         IF (IN(2).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(2) = in(9) * (Azone2 / Atotal)
         ELSE IF (IN(2) .GT. (in(4)+BP(1))) THEN
            OUT(2) = 0
         END IF

C----- Zone 3-----------------------------------------------------------

         IF (IN(3).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(3) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(3) .GT. (in(4)+BP(1))) THEN
            OUT(3) = 0
         END IF

C----- Pufferspeicher Heizung-------------------------------------------
         IF (heat.eq.0.AND.roomheat.EQ.1) THEN

             out(4) = in(9)
             out(7) = in(9)
             heat = 2

          END IF
       END IF

C----- Mode 4 "OFF" -----------------------------------------------
        IF (set .EQ. 4 .and. s .eq. 1) THEN

C----- Thermal ---------------------------------------------------------
C----- Pufferspeicher TWW ----------------------------------------------

            out(5) = 0
            out(7) = 0
            heat = 0


C----- Raumheizung -----------------------------------------------------
C----- Zone 1-----------------------------------------------------------

         IF (IN(1).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(1) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(1) .GT. (in(4)+BP(1))) THEN
            OUT(1) = 0
         END IF

C----- Zone 2-----------------------------------------------------------

         IF (IN(2).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(2) = in(9) * (Azone2 / Atotal)
         ELSE IF (IN(2) .GT. (in(4)+BP(1))) THEN
            OUT(2) = 0
         END IF

C----- Zone 3-----------------------------------------------------------

         IF (IN(3).LT.in(4).AND.roomheat.EQ.1) THEN
            OUT(3) = in(9) * (Azone1 / Atotal)
         ELSE IF (IN(3) .GT. (in(4)+BP(1))) THEN
            OUT(3) = 0
         END IF

C----- Pufferspeicher Heizung-------------------------------------------

             out(4) = 0
             out(7) = 0
             heat = 0


       END IF


             rp(1)= in(14)
             out(10)=rp(1)


      RETURN
      END
