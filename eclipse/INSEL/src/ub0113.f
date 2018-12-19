      SUBROUTINE UB0113(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBHPCTRL'
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


C----- Battery mode 1---------------------------------------------

      IF (set_batt .EQ.1) then

      END IF

C----- Battery mode 2----------------------------------------------

      IF (set_batt .EQ.2) then

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



C----- Mode -1 "standard" -----------------------------------------------
        IF (set .EQ. -1 .and. s .eq. 1) THEN

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

C----- Mode 0 "HP OFF"---------------------------------

       IF ((set.EQ.0)) then

            OUT(1) = 0
            OUT(2) = 0
            OUT(3) = 0

            OUT(4) = 0
            OUT(5) = 0
            OUT(7) = 0

       END IF


C----- Mode 1 "HP to Storage 1 (Space Heating)"-------------------------
       IF ((set.EQ.1)) THEN

C----- Thermal ---------------------------------------------------------
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


         out(4) = in(9)
         out(7) = in(9)


       END IF


C----- Mode 2 "HP to Storage 2 (DHW)"-----------------------------------

      IF ((set.EQ.2)) THEN

C----- Thermal ---------------------------------------------------------
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

C----- Pufferspeicher DHW-----------------------------------------------


         out(5) = in(9)
         out(7) = in(9)


       END IF

C----- Mode 3 "Thermal Building Demand"---------------------------------

       IF ((set.EQ.3)) then

            OUT(1) = 0
            OUT(2) = 0
            OUT(3) = 0

            OUT(4) = 0
            OUT(5) = 0
            OUT(7) = 0

       END IF



             rp(1)= in(14)
             out(10)=rp(1)


      RETURN
      END
