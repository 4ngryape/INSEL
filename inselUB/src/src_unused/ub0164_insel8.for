C-----------------------------------------------------------------------
C #Begin
C #Block HPPOLY
C #Description
C    The HPPOLY block simulates a compression heat pump by using
C    the polynomial functions that describe the compressor
C    performance according to EN12900.
C #Layout
C    #Inputs      4
C    #Outputs     9
C    #Parameters  25
C    #Strings     0
C    #Group       S
C #Details
C    #Inputs
C       #IN(1)  Fluid inlet temperature at condenser side Tc_in [°C]
C       #IN(2)  Fluid mass flow rate in the condenser mdot_cond [kg/s]
C       #IN(3)  Fluid inlet temperature at evaporator side Te_in [°C]
C       #IN(4)  Fluid mass flow rate in the evaporator mdot_eva [kg/s]
C    #Parameters
C       #BP(1)  Number of compressors Nc[-]
C       #BP(2)  Specific heat capacity of fluid at evaporator side cp_e [kJ/(kgK)]
C       #BP(3)  Specific heat capacity of fluid at condenser side cp_c [kJ/(kgK)]
C       #BP(4)  UA value of HX at evaporator side UAe [kW/K]
C       #BP(5)  UA value of HX at condenser side UAc [kW/K]
C       #BP(6)  Polynomial coefficient 1 for cooling capacity c1
C       #BP(7)  Polynomial coefficient 2 for cooling capacity c2
C       #BP(8)  Polynomial coefficient 3 for cooling capacity c3
C       #BP(9)  Polynomial coefficient 4 for cooling capacity c4
C       #BP(10)  Polynomial coefficient 5 for cooling capacity c5
C       #BP(11)  Polynomial coefficient 6 for cooling capacity c6
C       #BP(12)  Polynomial coefficient 7 for cooling capacity c7
C       #BP(13)  Polynomial coefficient 8 for cooling capacity c8
C       #BP(14)  Polynomial coefficient 9 for cooling capacity c9
C       #BP(15)  Polynomial coefficient 10 for cooling capacity c10
C       #BP(16)  Polynomial coefficient 1 for electricity consumption d1
C       #BP(17)  Polynomial coefficient 2 for electricity consumption d2
C       #BP(18)  Polynomial coefficient 3 for electricity consumption d3
C       #BP(19)  Polynomial coefficient 4 for electricity consumption d4
C       #BP(20)  Polynomial coefficient 5 for electricity consumption d5
C       #BP(21)  Polynomial coefficient 6 for electricity consumption d6
C       #BP(22)  Polynomial coefficient 7 for electricity consumption d7
C       #BP(23)  Polynomial coefficient 8 for electricity consumption d8
C       #BP(24)  Polynomial coefficient 9 for electricity consumption d9
C       #BP(25)  Polynomial coefficient 10 for electricity consumption d10
C    #Outputs
C       #OUT(1) Outlet fluid temperature at condenser side Tc_out [°C]
C       #OUT(2) Outlet fluid temperature at evaporator side Te_out [°C]
C       #OUT(3) Power dissipated at condenser Qcond [kW]
C       #OUT(4) Power absorbed at evaporator Qeva [kW]
C       #OUT(5) Electrical power consumed by the compressor Pe [kW]
C       #OUT(6) COP of the heat pump in heating mode COPh [-]
C       #OUT(7) COP of the heat pump in cooling mode COPc [-]
C       #OUT(8) Condenser temperature Tcond [°C]
C       #OUT(9) Evaporator temperature Teva[°C]
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
C #End
C     Authors: Antoine Dalibard
C-----------------------------------------------------------------------
      SUBROUTINE ub0164(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT   NONE
      INCLUDE   'ub0164.h'
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'HPPOLY'
     &,                OPM    = 1
     &,                INMIN  = 4
     &,                INS    = 4
     &,                OUTS   = 9
     &,                IPS    = 14
     &,                RPS    = 10
     &,                DPS    = 4
     &,                BPMIN  = 25
     &,                BPS    = 25
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1), STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),Nc,I,J
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      REAL             Tc_in,mdot_cond,Te_in,mdot_eva,cp_e,cp_c,UAe,UAc,
     &                 Tc_out,Te_out,Qcond,Qeva,Pe,COPh,COPc,Tcond,Teva,
     &                 e_cond,e_eva,c(10),d(10),e     
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,
     &         OPM,INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------

C-----SET PARAMETERS-----------------------------
      Nc       = BP(1)
      cp_e     = BP(2)
	cp_c     = BP(3)
	UAe      = BP(4)
      UAc      = BP(5)
	c(1)     = BP(6)
	c(2)     = BP(7)
	c(3)     = BP(8)
	c(4)     = BP(9)
	c(5)     = BP(10)
	c(6)     = BP(11)
	c(7)     = BP(12)
	c(8)     = BP(13)
	c(9)     = BP(14)
	c(10)    = BP(15)
	d(1)     = BP(16)
	d(2)     = BP(17)
	d(3)     = BP(18)
	d(4)     = BP(19)
	d(5)     = BP(20)
	d(6)     = BP(21)
	d(7)     = BP(22)
	d(8)     = BP(23)
	d(9)     = BP(24)
	d(10)    = BP(25)
C-----SET INPUTS---------------------------------
      Tc_in     = IN(1)
      mdot_cond = IN(2)
	Te_in     = IN(3)
	mdot_eva  = IN(4)

C     Return if no mass flow in the HX
      IF ((mdot_eva .LE. 0.0) .OR. (mdot_cond .LE. 0.0)) THEN
	  Tc_out   = Tc_in
	  Te_out   = Te_in
	  Qcond    = 0.0
	  Qeva     = 0.0
	  Pe       = 0.0
	  COPh     = 0.0
	  COPc     = 0.0
	  Tcond    = Tc_in
	  Teva     = Te_in
	  OUT(1)   = Tc_out
	  OUT(2)   = Te_out
	  OUT(3)   = Qcond
	  OUT(4)   = Qeva
	  OUT(5)   = Pe
	  OUT(6)   = COPh
	  OUT(7)   = COPc
	  OUT(8)   = Tcond
	  OUT(9)   = Teva 
	  RETURN
	END IF

C     thermal effectiveness of the evaporator HX
      e_eva  = 1 - EXP(-UAe / (mdot_eva * cp_e))
C     thermal effectiveness of the condenser HX
      e_cond = 1 - EXP(-UAc / (mdot_cond * cp_c))

c     guess condensator temperature
      Tcond = Tc_in + 5.0
	RP(1) = Tcond
c     guess evaporator temperature
      Teva  = Te_in - 5.0
	RP(2) = Teva

C     Loop for condensator temperature
      DO I=1,20
	  DO J=1,20
c         calculate the cooling capacity at the evaporator side according
c         to EN12900 polynomial (in kW)
          Qeva = (c(1) + c(2)*Teva + c(3)*Tcond + c(4)*Teva**2.0
     &           + c(5)*Teva*Tcond + c(6)*Tcond**2.0 + c(7)*Teva**3.0
     &           + c(8)*Tcond*Teva**2.0 + c(9)*Teva*Tcond**2.0
     &           + c(10)*Tcond**3.0) * Nc / 1000.0
c         calculate new evaporator temperature
          Teva = Te_in - Qeva / (e_eva * mdot_eva * cp_e)
C         error
          e = Teva - RP(2)
          IF (ABS(e) .LT. 0.0001) THEN 
	      EXIT ! exit the loop
	    END IF
	    IF (J .EQ. 20) THEN
C         did not converge: print a warning
c            IP(1) = 390003
c	      CALL MSG(IP,RP,SP)
	    END IF
C         store value for next iteration
	    RP(2) = Teva                             
C       end of the evaporator temperature loop
	  END DO
C       calculate electricity consumption of the compressor according
c       to EN12900 polynomial (in kW)
        Pe = (d(1) + d(2)*Teva + d(3)*Tcond + d(4)*Teva**2.0
     &           + d(5)*Teva*Tcond + d(6)*Tcond**2.0 + d(7)*Teva**3.0
     &           + d(8)*Tcond*Teva**2.0 + d(9)*Teva*Tcond**2.0
     &           + d(10)*Tcond**3.0) * Nc / 1000.0
c       calculate heating power at condenser side according to energy balance
        Qcond = Qeva + Pe
c       calculate new condenser temperature Tcond
        Tcond = Tc_in + Qcond / (e_cond * mdot_cond * cp_c)
C       error
        e = Tcond - RP(1)
        IF (ABS(e) .LT. 0.0001) THEN 
	    EXIT ! exit the loop
	  END IF
	  IF (I .EQ. 20) THEN
C         did not converge: print a warning
c         IP(1) = 390003
c	    CALL MSG(IP,RP,SP)
	  END IF
C       store value for next iteration
	  RP(1) = Tcond 
C     end of the condensator temperature loop
	END DO

C     calculate outlet temperatures
      Tc_out = Tc_in + Qcond / (mdot_cond * cp_c)
	Te_out = Te_in - Qeva / (mdot_eva * cp_e)
C     calculate COPs
      COPh = Qcond / Pe
	COPc = Qeva / Pe

C     SET OUTPUTS
	OUT(1)   = Tc_out
	OUT(2)   = Te_out
	OUT(3)   = Qcond
	OUT(4)   = Qeva
	OUT(5)   = Pe
	OUT(6)   = COPh
	OUT(7)   = COPc
	OUT(8)   = Tcond
	OUT(9)   = Teva 
      RETURN
      END

C-----END OF MAIN PROGRAM--------------------------------------------------
