C-----------------------------------------------------------------------
C #Begin
C #Block UBHPREFP
C #Description
C    The UBHPREFP block simulates a compression heat pump by using
C    REFPROP subroutines. The isentropic compression efficiency
C    is modeled using a cross-correlation function of the
C    condenser and evaporator temperatures.
C #Layout
C    #Inputs      8
C    #Outputs     10
C    #Parameters  12
C    #Strings     0
C    #Group       S
C #Details
C    #Inputs
C       #IN(1) Fluid inlet temperature at condenser side
C              $T_{\mrm c,in}$ / \degC
C       #IN(2) Fluid mass flow rate in the condenser
C              $\dot{m}_{\mrm c}$ / kg\,s$^{-1}$
C       #IN(3) Fluid inlet temperature at evaporator side
C              $T_{\mrm e,in}$ / \degC
C       #IN(4) Fluid mass flow rate in the evaporator
C              $\dot{m}_{\mrm e}$ / kg\,s$^{-1}$
C       #IN(5) Set point temperature for user (cooling or heating)
C              $T_{\mrm set}$ / \degC
C       #IN(6) Mode $M$ of the heat pump (0: heating, 1: cooling)
C       #IN(7) Superheating at the end of evaporator
C              $\Delta T_{\mrm sh}$ / K
C       #IN(8) Subcooling at the beginning of condenser
C              $\Delta T_{\mrm sc}$ / K
C    #Parameters
C       #BP(1) Efficiency of the compressor $\eta$
C       #BP(2) Specific heat capacity of fluid at evaporator side
C              $c_{\mrm p,e}$ / kJ\,kg$^{-1}$\,K$^{-1}$
C       #BP(3) Specific heat capacity of fluid at condenser side
C              $c_{\mrm p,c}$  / kJ\,kg$^{-1}$\,K$^{-1}$
C       #BP(4) UA value of HX at evaporator side
C              $UA_{\mrm e}$ / kW\,K$^{-1}$
C       #BP(5) UA value of HX at condenser side
C              $UA_{\mrm c}$ / kW\,K$^{-1}$
C       #BP(6) Type of refrigerant used
C                (0: R134a, 1: R407c, 2: CO$_2$)
C       #BP(7)  Coefficient $A_0$ isentropic efficiency
C       #BP(8)  Coefficient $A_1$ isentropic efficiency / K$^{-1}$
C       #BP(9)  Coefficient $A_2$ isentropic efficiency / K$^{-2}$
C       #BP(10) Coefficient $A_3$ isentropic efficiency / K$^{-1}$
C       #BP(11) Coefficient $A_4$ isentropic efficiency / K$^{-2}$
C       #BP(12) Coefficient $A_5$ isentropic efficiency / K$^{-2}$
C    #Outputs
C       #OUT(1)  Outlet fluid temperature at condenser side
C                $T_{\mrm c,out}$ / \degC
C       #OUT(2)  Outlet fluid temperature at evaporator side
C                $T_{\mrm e,out}$  / \degC
C       #OUT(3)  Power dissipated at condenser $\dot{Q}_{\mrm c}$ / kW
C       #OUT(4)  Power absorbed at evaporator $\dot{Q}_{\mrm e}$ / kW
C       #OUT(5)  Usefull mechanical work of compressor
C                $P_{\mrm comp}$ / kW
C       #OUT(6)  Electrical power consumed by the compressor
C                $P_{\mrm el}$ / kW
C       #OUT(7)  COP of the heat pump
C       #OUT(8)  Condenser temperature $T_{\mrm c}$ / \degC
C       #OUT(9)  Evaporator temperature $T_{\mrm e}$ / \degC
C       #OUT(10) Isentropic compression efficiency $\eta_{\mrm i}$
C    #Strings
C       #SP(1)  Full path to REFPROP directory\newline
C               \eg "C:/Program Files/REFPROP"
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
C       #IP(11) Maximum number of iterations exceeded
C    #Reals
C       #None
C    #Doubles
C       #None
C #Dependencies
C    #Subroutine ID
C #Authors
C    Antoine Dalibard
C    Juergen Schumacher (minor modifications 09.2016)
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0161(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT   NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBHPREFP'
     &,                OPM    = 1
     &,                INMIN  = 8
     &,                INS    = 8
     &,                OUTS   = 10
     &,                IPS    = 11
     &,                RPS    = 10
     &,                DPS    = 4
     &,                BPMIN  = 12
     &,                BPS    = 12
     &,                SPMIN  = 1
     &,                SPS    = 1
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1),SPATH
      INTEGER          ILEN,IRC
      CHARACTER*1      X00
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),I,MODE,TYP,FLUID
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      DOUBLE PRECISION Tc_in,mdot_cond,Te_in,mdot_eva,Tset,dTsh,dTsc,
     &                 eta,cp_e,cp_c,UAe,UAc,Tc_out,Te_out,
     &                 Qcond,Qeva,Pcomp,Pe,COP,Qneed,
     &                 Tcond,Teva,T1,Ps_eva,
     &                 rho1,h1,s1,Ps_cond,T2S,rho2S,h2S,h2,eta_isen,T2,
     &                 rho3,h3,s3,rho4_st,h4_st,s4_st,T4,rho4,
     &                 h4,s4,h5,x5,s5,
     &                 rho6,h6,s6,mdot_ref,e,e_cond,e_eva,FUNC1,
     &                 Teva_out,Tcond_in,Tcond_out,Teva_in,FUNC,
     &                 RTSEC,Teva_in0,Teva_in1,xacc,RTSEC1,Tcond_in0,
     &                 Tcond_in1,A0,A1,A2,A3,A4,A5
C     Variable definition for refprop subroutines
      INTEGER          nc,ncmax,ierr,kph
      PARAMETER        (ncmax=20) ! max number of components in mixture
      DOUBLE PRECISION x(ncmax),xliq(ncmax),xvap(ncmax),p,
     &                 rhol,rhov,TK,wm,ttp,tnbp,tc,pc,dc,WMOL,
     &                 zc,acf,dip,rgas,rho,ei,h,s,cv,cp,w,hjt,
     &                 D,Dl,Dv,q,t,kr
      CHARACTER        hrf*3, herr*255
      CHARACTER*255    hf(ncmax),hfmix
C     Use functions as input in other functions
      EXTERNAL         FUNC,FUNC1
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
            IF (IP(11) .GT. 0) THEN
C              Display number of unsuccessful iterations
               IP(9) = IP(11)
               IP(1) = 205097
               CALL MSG(IP,RP,SP)
            END IF
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
      eta       = BP(1)
      cp_e      = BP(2)
      cp_c      = BP(3)
      UAe       = BP(4)
      UAc       = BP(5)
      TYP       = ANINT(BP(6))
      A0        = BP(7)
      A1        = BP(8)
      A2        = BP(9)
      A3        = BP(10)
      A4        = BP(11)
      A5        = BP(12)
      Tc_in     = IN(1)
      mdot_cond = IN(2)
      Te_in     = IN(3)
      mdot_eva  = IN(4)
      Tset      = IN(5)
      MODE      = ANINT(IN(6))
      DTsh      = IN(7)
      DTsc      = IN(8)

      X00 = CHAR(0)
      CALL STRIP(SP(1),' ',0,SPATH,ILEN,IRC)

C     Return if no mass flow in the HX
      IF (mdot_eva .LE. 0.0 .OR. mdot_cond .LE. 0.0) THEN
         Tc_out   = Tc_in
         Te_out   = Te_in
         Qcond    = 0.0
         Qeva     = 0.0
         Pcomp    = 0.0
         Pe       = 0.0
         COP      = 0.0
         Tcond    = Tc_in
         Teva     = Te_in
         eta_isen = 0.0
         OUT(1)   = Tc_out
         OUT(2)   = Te_out
         OUT(3)   = Qcond
         OUT(4)   = Qeva
         OUT(5)   = Pcomp
         OUT(6)   = Pe
         OUT(7)   = COP
         OUT(8)   = Tcond
         OUT(9)   = Teva
         OUT(10)  = eta_isen
         RETURN
      END IF

C     Thermal effectiveness of the evaporator HX
      e_eva  = 1 - EXP(-UAe / (mdot_eva * cp_e))
C     Thermal effectiveness of the condenser HX
      e_cond = 1 - EXP(-UAc / (mdot_cond * cp_c))

C     Define refrigerant
      IF (Typ .EQ. 0) THEN ! R134a
         FLUID = 0 ! pure fluid
         SPATH = SPATH(1:ILEN) // '\fluids' // X00
         CALL SETPATH(SPATH)
         nc = 1 ! number of component
         hf(1) = 'R134a.fld' ! fluid name
         hfmix = 'hmx.bnc' ! mixture file name
         hrf = 'DEF' ! Reference state (DEF means default)
         CALL SETUP(nc,hf,hfmix,hrf,ierr,herr)
         IF (IERR .NE. 0) THEN
            IP(1) = 404010
            CALL MSG(IP,RP,herr)
            RETURN
         END IF
         CALL INFO(nc,wm,ttp,tnbp,tc,pc,dc,zc,acf,dip,rgas)
      ELSE IF (Typ .EQ. 1) THEN ! R407C
         FLUID = 1 ! mixture
         SPATH = SPATH(1:ILEN) // '\mixtures' // X00
         CALL SETPATH(SPATH)
         hf(1) = 'R407C.mix' ! fluid name
         hfmix = 'hmx.bnc' ! mixture file name
         hrf = 'DEF' !Reference state (DEF means default)
C        Read constituents and mole fractions
         CALL SETMIX(hf,hfmix,hrf,nc,hf,x,ierr,herr)
         IF (IERR .NE. 0) THEN
            IP(1) = 404010
            CALL MSG(IP,RP,herr)
            RETURN
         END IF
         CALL SETUP(nc,hf,hfmix,hrf,ierr,herr)
         IF (IERR .NE. 0) THEN
            IP(1) = 404010
            CALL MSG(IP,RP,herr)
            RETURN
         END IF
         CALL SETREF(hrf,2,x,0.d0,0.d0,0.d0,0.d0,ierr,herr)
         IF (IERR .NE. 0) THEN
            IP(1) = 404010
            CALL MSG(IP,RP,herr)
            RETURN
         END IF
         wm = WMOL(x) ! molecular weight for the mixture
      ELSE IF (Typ .EQ. 2) THEN ! CO2
         FLUID = 0 ! pure fluid
         SPATH = SPATH(1:ILEN) // '\fluids' // X00
         CALL SETPATH(SPATH)
         nc = 1 ! number of component
         hf(1) = 'CO2.fld' ! fluid name
         hfmix = 'hmx.bnc' ! mixture file name
         hrf = 'DEF' !Reference state (DEF means default)
         CALL SETUP(nc,hf,hfmix,hrf,ierr,herr)
         IF (IERR .NE. 0) THEN
            IP(1) = 404010
            CALL MSG(IP,RP,herr)
            RETURN
         END IF
         CALL INFO(nc,wm,ttp,tnbp,tc,pc,dc,zc,acf,dip,rgas)
      END IF

C     Two cases: 1. pure fluid 2. mixture
      IF (FLUID .EQ. 0) THEN ! pure fluid
         IF (MODE .EQ. 0) THEN ! Heat pump in heating mode
C           Power needed by the user in kW
            Qneed  = mdot_cond * cp_c * (Tset - Tc_in)
C           Condenser temperature necessary to get this power
            Tcond  = Tc_in + Qneed / (e_cond * cp_c * mdot_cond)
C           Assume evaporator temperature
            Teva = Te_in - 5.0
            DP(2)= Teva

            DO I = 1,20
C              Refrigerant thermodynamic cycle
C              saturated pressure of refrigerant at evaporator
C              temperature in kPa
               kph = 2 ! x is vapor phase
               TK = Teva + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               Ps_eva = p

C              Entry of the compressor (point 1)
C              Temperature of the overheated refrigerant
C              The refrigerant is overheated at the end of the
C              evaporator in order to be sure that no liquid enters
C              the compressor
               T1 = Teva + dTsh
C              Density of the superheated refrigerant
C              (gaseous state) in kg/m**3
               TK  = T1 + 273.15
               kph = 2 ! x is vapor phase
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho1 = rho * wm ! rho in kg/m**3
C              Enthalpy and entropy of the superheated refrigerant
C              (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h1 = h / wm ! in J/g or kJ/kg
               s1 = s / wm ! in J/(gK) or kJ/(kgK)

C              Saturated pressure of refrigerant at condenser
C              temperature
               kph = 2 ! x is vapor phase
               TK = Tcond + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               Ps_cond = p ! in kPa

C              End of the isentropic compression (point 2s)
C              Refrigerant temperature
               CALL PSFLSH(Ps_cond,s,x,t,D,Dl,Dv,xliq,xvap,q,ei,h,
     &                     cv,cp,w,ierr,herr)
               T2S = T - 273.15
C              Density of the refrigerant in kg/m**3
               TK = T2S + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho2S = rho * wm ! rho in kg/m**3
C              Enthalpy of the refrigerant in kJ/kg
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h2S = h / wm ! in J/g or kJ/kg

C              End of the polytropic compression (point 2)
C              Isentropic efficiency
               eta_isen = A0 + A1 * Tcond + A2 * Tcond**2.0 + A3 * Teva
     &                    + A4 * Teva**2.0 + A5 * Tcond * Teva
               IF (eta_isen .LT. 0.0) eta_isen = 0.01
               IF (eta_isen .GT. 1.0) eta_isen = 1.0
C              Enthalpy of the refrigerant in kJ/kg
               h2  = (h2S + h1 * (eta_isen - 1.0)) / eta_isen
C              Refrigerant temperature
               h = h2 * wm ! in J/mol
               CALL PHFLSH(Ps_cond,h,x,t,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                     cv,cp,w,ierr,herr)
               T2 = t - 273.15

C              Entry of the condenser (point 3*)
C              density of the refrigerant (gaseous state) in kg/m**3
               TK = Tcond + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho3 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h3 = h / wm ! in J/g or kJ/kg
               s3 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the condenser (point 4*)
C              Density of the refrigerant (liquid state) in kg/m**3
               kph = 1 ! x is liquid phase
               TK = Tcond + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               rho4_st = rhol * wm
C              Enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rhol,x,p,ei,h,s,cv,cp,w,hjt)
               h4_st = h / wm ! in J/g or kJ/kg
               s4_st = s / wm ! in J/(gK) or kJ/(kgK)

C              Entry of the expansion valve (point 4)
C              Refrigerant temperature
C              The refrigerant is cooled down in order to maximize the
C              amount of liquid entering the evaporator
C              (maximize cooling power)
               T4 = Tcond - dTsc
C              density of the refrigerant (liquid state) in kg/m**3
               TK = T4 + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho4   = rho * wm ! rho in kg/m³
C              enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h4 = h / wm ! in J/g or kJ/kg
               s4 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the expansion valve (entry of evaporator) (point 5)
C              Enthalpy of the refrigerant (liquid + vapor state) in kJ/kg
               h5 = h4 ! Adiabatic expansion assumed
C              Percentage of vapor in the refrigerant mixture
C              (liquid and vapor) and entropy
               h  = h5 * wm ! J/mol
               kr = 1 ! lower density root
               TK = Teva + 273.15
               CALL THFLSH(TK,h,x,kr,p,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                     cv,cp,w,ierr,herr)
               x5  = q
               s5  = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the evaporator before superheating (point 6*)
C              Density of the refrigerant (gaseous state) in kg/m**3
               kph = 2 ! x is gas phase
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho6 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h6 = h / wm ! in J/g or kJ/kg
               s6 = s / wm ! in J/(gK) or kJ/(kgK)

C              Refrigerant mass flow rate (kg/s)
               mdot_ref = Qneed / (h2 - h4)
C              Power absorbed at the evaporator (kW)
               Qeva = mdot_ref * (h1 - h5)
C              New evaporator temperature
               Teva = Te_in - Qeva / (e_eva * cp_e * mdot_eva)
C              Error
               e = Teva - DP(2)
               IF (ABS(e) .LT. 0.001) THEN
                  EXIT ! exit the loop
               END IF
               IF (I .EQ. 20) THEN
C                 Maximum number of iterations exceeded
                  IF (IP(11) .EQ. 0) THEN
                     IP(1) = 205098
                     CALL MSG(IP,RP,SP)
                     IP(11) = 1
                  ELSE
                     IP(11) = IP(11) + 1
                  END IF
               END IF
C              Store value for next iteration
               DP(2) = Teva
            END DO
            Tc_out = Tset
            Te_out = Te_in - Qeva / (mdot_eva * cp_e)
            Qcond  = Qneed
            Qeva   = Qeva
            Pcomp  = mdot_ref * (h2 - h1)
            Pe     = Pcomp / eta
            COP    = Qcond / Pe
         ELSE ! Heat pump in cooling mode
C           Power needed by the user in kW
            Qneed  = mdot_eva * cp_e * (Te_in - Tset)
C           Evaporator temperature needed to deliver the required power
            Teva   = Te_in - Qneed / (e_eva * cp_e * mdot_eva)
C           Assume condenser temperature
            Tcond = Tc_in + 10.0
            DP(2) = Tcond
            DO I = 1,20
C              Saturated pressure of refrigerant at evaporator
C              temperature in kPa
               kph = 2 ! x is vapor phase
               TK = Teva + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               Ps_eva = p

C              Entry of the compressor (point 1)
C              temperature of the overheated refrigerant
               T1 = Teva + dTsh
C              The refrigerant is overheated in order to be sure that
C              no liquid enters the compressor
C              Density of the superheated refrigerant (gaseous state)
C              in kg/m**3
               TK  = T1 + 273.15
               kph = 2 ! x is vapor phase
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho1 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the superheated refrigerant
C              (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h1 = h / wm ! in J/g or kJ/kg
               s1 = s / wm ! in J/(gK) or kJ/(kgK)

C              Saturated pressure of refrigerant at condenser temperature
C              in kPa
               kph = 2 ! x is vapor phase
               TK = Tcond + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               Ps_cond = p ! in kPa

C              End of the isentropic compression (point 2s)
C              refrigerant temperature in degC
               CALL PSFLSH(Ps_cond,s,x,t,D,Dl,Dv,xliq,xvap,q,ei,h,
     &                     cv,cp,w,ierr,herr)
               T2S = T - 273.15 ! in kPa
C              Density of the refrigerant in kg/m**3
               TK = T2S + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho2S = rho * wm ! rho in kg/m**3
C              Enthalpy of the refrigerant in kJ/kg
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h2S = h / wm ! in J/g or kJ/kg

C              End of the polytropic compression (point 2)
c              Isentropic efficiency
               eta_isen = A0 + A1 * Tcond + A2 * Tcond**2.0 + A3 * Teva
     &                    + A4 * Teva**2.0 + A5 * Tcond * Teva
               IF (eta_isen .LT. 0.0) eta_isen = 0.01
               IF (eta_isen .GT. 1.0) eta_isen = 1.0
C              Enthalpy of the refrigerant in kJ/kg
               h2  = (h2S + h1 * (eta_isen - 1.0)) / eta_isen
C              Refrigerant temperature
               h = h2 * wm ! in J/mol
               CALL PHFLSH(Ps_cond,h,x,t,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                     cv,cp,w,ierr,herr)
               T2 = t - 273.15

C              Entry of the condenser (point 3*)
C              Density of the refrigerant (gaseous state) in kg/m**3
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho3 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h3 = h / wm ! in J/g or kJ/kg
               s3 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the condenser (point 4*)
C              density of the refrigerant (liquid state) in kg/m**3
               kph = 1 ! x is liquid phase
               TK = Tcond + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               rho4_st = rhol * wm
C              enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rhol,x,p,ei,h,s,cv,cp,w,hjt)
               h4_st = h / wm ! in J/g or kJ/kg
               s4_st = s / wm ! in J/(gK) or kJ/(kgK)

C              Entry of the expansion valve (point 4)
C              refrigerant temperature
               T4 = Tcond - dTsc
C              The refrigerant is cooled down in order to maximize
C              the amount of liquid entering the evaporator (maximize
C              cooling power)
C              Density of the refrigerant (liquid state) in kg/m**3
               TK = T4 + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho4 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h4 = h / wm ! in J/g or kJ/kg
               s4 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the expansion valve (entry of evaporator) (point 5)
C              Enthalpy of the refrigerant (liquid + vapor state) in kJ/kg
               h5 = h4 ! Adiabatic expansion assumed
C              Percentage of vapor in the refrigerant mixture (liquid
C              and vapor) and entropy
               h = h5 * wm ! J/mol
               kr = 1 ! lower density root
               TK= Teva + 273.15
               CALL THFLSH(TK,h,x,kr,p,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                     cv,cp,w,ierr,herr)
               x5 = q
               s5 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the evaporator (point 6)
C              Density of the refrigerant (gaseous state) in kg/m**3
               kph = 2 ! x is gas phase
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho6 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h6 = h / wm ! in J/g or kJ/kg
               s6 = s / wm ! in J/(gK) or kJ/(kgK)

C              Refrigerant mass flow rate (kg/s)
               mdot_ref = Qneed / (h1 - h5)
C              Power dissipated at the condenser (kW)
               Qcond = mdot_ref * (h2 - h4)
C              New condenser temperature
               Tcond  = Tc_in + Qcond / (e_cond * cp_c * mdot_cond)

C              Error
               e = Tcond - DP(2)
               IF (ABS(e) .LT. 0.001) THEN
                  EXIT ! exit the loop
               END IF
               IF (I .EQ. 20) THEN
C                 Maximum number of iterations exceeded
                  IF (IP(11) .EQ. 0) THEN
                     IP(1) = 205098
                     CALL MSG(IP,RP,SP)
                     IP(11) = 1
                  ELSE
                     IP(11) = IP(11) + 1
                  END IF
               END IF
C              Store value for next iteration
               DP(2)  = Tcond
            END DO
            Tc_out = Tc_in + Qcond / (mdot_cond * cp_c)
            Te_out = Tset
            Qcond  = Qcond
            Qeva   = Qneed
            Pcomp  = mdot_ref * (h2 - h1)
            Pe     = Pcomp / eta
            COP    = Qeva / Pe
            Tcond  = Tcond
            Teva   = Teva
            eta_isen = eta_isen
         END IF
      ELSE ! FLUID = 1 mixture
         IF (MODE .EQ. 0) THEN ! Heat pump in heating mode
C           Power needed by the user in kW
            Qneed  = mdot_cond * cp_c * (Tset - Tc_in)
C           Mean condenser temperature necessary to get this power
            Tcond  = Tc_in + Qneed / (e_cond * cp_c * mdot_cond)
c           Set limit for inlet condenser temperature
            Tcond_in0 = Tcond + 10.0
            Tcond_in1 = Tcond
            xacc = 0.001 ! accuracy
C           calculate new Tcond_in using secant method
            Tcond_in = RTSEC1(FUNC1,Tcond_in0,Tcond_in1,xacc,Tcond,x,nc)
C           calculate outlet condenser temperature
            Tcond_out = 2.D0 * Tcond - Tcond_in

C           Assume inlet evaporator temperature
            Teva_in = Te_in - 5.0
            DP(2) = Teva_in
            Teva = Teva_in + 2.0
            DO I = 1,20
C              Loop for inlet evaporator temperature
C              Saturated pressure of refrigerant at inlet condenser
C              temperature in kPa
               kph = 2 ! x is vapor phase
               TK = Tcond_in + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               Ps_cond = p ! in kPa

C              Entry of the condenser (point 3*)
C              Density of the refrigerant (gaseous state) in kg/m**3
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho3   = rho * wm ! rho in kg/m**3
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h3 = h / wm ! in J/g or kJ/kg
               s3 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the condenser (point 4*)
C              Density of the refrigerant (liquid state) in kg/m**3
               kph = 1 ! x is liquid phase
               TK = Tcond_out + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               rho4_st = rhol * wm
C              Enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rhol,x,p,ei,h,s,cv,cp,w,hjt)
               h4_st = h / wm ! in J/g or kJ/kg
               s4_st = s / wm ! in J/(gK) or kJ/(kgK)

C              Entry of the expansion valve (point 4)
C              Refrigerant temperature
               T4 = Tcond_out - dTsc
C              The refrigerant is cooled down in order to maximize the
C              amount of liquid entering the evaporator (maximize
C              cooling power)
C              Density of the refrigerant (liquid state) in kg/m**3
               TK = T4 + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho4 = rho * wm ! rho in kg/m**3
C              Enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h4 = h / wm ! in J/g or kJ/kg
               s4 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the expansion valve (entry of evaporator) (point 5)
C              Enthalpy of the refrigerant (liquid + vapor state) in kJ/kg
               h5 = h4 ! Adiabatic expansion assumed
C              Saturated pressure of refrigerant at evaporator
C              temperature in kPa
               TK = Teva_in + 273.15
               h = h5 * wm
               kr = 1
               CALL THFLSH(TK,h,x,kr,p,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                      cv,cp,w,ierr,herr)
               Ps_eva = p
C              Percentage of vapor in the refrigerant mixture
C              (liquid+vapor) and entropy
               x5  = q
               s5  = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the evaporator before superheating (point 6*)
C              temperature at the outlet of the evaporator
               kph = 2 ! x is gas phase
               CALL SATP(Ps_eva,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
               Teva_out = t - 273.15
C              density of the refrigerant (gaseous state) in kg/m**3
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho6 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h6 = h / wm ! in J/g or kJ/kg
               s6 = s / wm ! in J/(gK) or kJ/(kgK)

C              Entry of the compressor (point 1)
C              Temperature of the overheated refrigerant
C              The refrigerant is overheated at the end of the
C              evaporator in order to be sure that no liquid enters the
C              compressor
               T1 = Teva_out + dTsh
C              Density of the superheated refrigerant (gaseous state)
C              in kg/m**3
               TK = T1 + 273.15
               kph = 2 ! x is vapor phase
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho1 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the superheated refrigerant
C              (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h1 = h / wm ! in J/g or kJ/kg
               s1 = s / wm ! in J/(gK) or kJ/(kgK)

C              End of the isentropic compression (point 2s)
C              Refrigerant temperature in degC
               CALL PSFLSH(Ps_cond,s,x,t,D,Dl,Dv,xliq,xvap,q,ei,h,
     &                      cv,cp,w,ierr,herr)
               T2S = t - 273.15
C              Density in kg/m**3
               TK = T2S + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho2S = rho * wm ! rho in kg/m**3
C              Enthalpy in kJ/kg
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h2S = h / wm ! in J/g or kJ/kg

C              End of the polytropic compression (point 2)
c              Isentropic efficiency
               eta_isen = A0 + A1 * Tcond + A2 * Tcond**2.0 + A3 * Teva
     &                  + A4 * Teva**2.0 + A5 * Tcond * Teva
               IF (eta_isen .LT. 0.0) eta_isen = 0.01
               IF (eta_isen .GT. 1.0) eta_isen = 1.0
C              Enthalpy in kJ/kg
               h2  = (h2S + h1 * (eta_isen - 1.0)) / eta_isen
C              Refrigerant temperature
               h = h2 * wm ! in J/mol
               CALL PHFLSH(Ps_cond,h,x,t,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                      cv,cp,w,ierr,herr)
               T2   = t - 273.15

C              Refrigerant mass flow rate (kg/s)
               mdot_ref = Qneed / (h2 - h4)
C              Power absorbed at the evaporator (kW)
               Qeva = mdot_ref * (h1 - h5)
C              New evaporator temperature
               Teva = Te_in - Qeva / (e_eva * cp_e * mdot_eva)

c              Set limit for inlet evaporator temperature
               Teva_in0 = Teva - 10.0
               Teva_in1 = Teva
               xacc = 0.001 ! accuracy
C              Calculate Teva_in using secant method
               h = h5 * wm
               Teva_in = RTSEC(FUNC,Teva_in0,Teva_in1,xacc,h,Teva,x,nc)

C              Error
               e = Teva_in - DP(2)
               IF (ABS(e) .LT. 0.0001) THEN
                  EXIT ! exit the loop
               END IF
               IF (I .EQ. 20) THEN
C                 Maximum number of iterations exceeded
                  IF (IP(11) .EQ. 0) THEN
                     IP(1) = 205098
                     CALL MSG(IP,RP,SP)
                     IP(11) = 1
                  ELSE
                     IP(11) = IP(11) + 1
                  END IF
               END IF
C              Store value for next iteration
               DP(2) = Teva_in
            END DO
            Tc_out = Tset
            Te_out = Te_in - Qeva / (mdot_eva * cp_e)
            Qcond  = Qneed
            Qeva   = Qeva
            Pcomp  = mdot_ref * (h2 - h1)
            Pe     = Pcomp / eta
            COP    = Qcond / Pe
         ELSE ! Heat pump in cooling mode
C           Power needed by the user in kW
            Qneed  = mdot_eva * cp_e * (Te_in - Tset)
C           Mean evaporator temperature needed to deliver the
C           required power
            Teva   = Te_in - Qneed / (e_eva * cp_e * mdot_eva)
C           Assume inlet condenser temperature
            Tcond_in = Tc_in + 10.0
            DP(2) = Tcond_in
            Tcond = Tcond_in - 2.0
            DO I = 1,20
C              Loop for inlet condenser temperature
C              Saturated pressure of refrigerant at inlet condenser
C              temperature in kPa
               kph = 2 ! x is vapor phase
               TK = Tcond_in + 273.15
               CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
               Ps_cond = p ! in kPa

C              Entry of the condenser (point 3*)
C              density of the refrigerant (gaseous state) in kg/m**3
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho3   = rho * wm ! rho in kg/m**3
C              enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h3 = h / wm ! in J/g or kJ/kg
               s3 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the condenser (point 4*)
C              condenser temperature at outlet of condenser
               kph = 1 ! x is liquid phase
               CALL SATP(Ps_cond,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
               Tcond_out = t - 273.15
C              Density of the refrigerant (liquid state) in kg/m**3
               rho4_st = rhol * wm
C              Enthalpy and entropy of the refrigerant (liquid state)
               TK = t
               CALL THERM(TK,rhol,x,p,ei,h,s,cv,cp,w,hjt)
               h4_st = h / wm ! in J/g or kJ/kg
               s4_st = s / wm ! in J/(gK) or kJ/(kgK)

C              Entry of the expansion valve (point 4)
C              Refrigerant temperature
               T4      = Tcond_out - dTsc
C              The refrigerant is cooled down in order to maximize the
C              amount of liquid entering the evaporator (maximize
C              cooling power)
C              Density of the refrigerant (liquid state) in kg/m**3
               TK = T4 + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho4 = rho * wm ! rho in kg/m**3
C              Enthalpy and entropy of the refrigerant (liquid state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h4 = h / wm ! in J/g or kJ/kg
               s4 = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the expansion valve (entry of evaporator) (point 5)
C              Enthalpy of the refrigerant (liquid and vapor state)
C              in kJ/kg
               h5 = h4 ! Adiabatic expansion assume
c              Set limit for inlet evaporator temperature
               Teva_in0 = Teva - 10.0
               Teva_in1 = Teva
               xacc = 0.0001 ! accuracy
C              Calculate Teva_in using secant method
               Teva_in = RTSEC(FUNC,Teva_in0,Teva_in1,xacc,h,Teva,x,nc)

c              Calculate evaporator pressure in kPa
               TK = Teva_in + 273.15
               kr = 1
               CALL THFLSH(TK,h,x,kr,p,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                     cv,cp,w,ierr,herr)
               Ps_eva = p
C              Percentage of vapor in the refrigerant mixture
C              (liquid and vapor) and entropy
               x5  = q
               s5  = s / wm ! in J/(gK) or kJ/(kgK)

C              Exit of the evaporator (point 6)
c              Calculate outlet evaporator temperature
               kph = 2 ! vapor phase
               CALL SATP(Ps_eva,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
               Teva_out = t - 273.15
C              Density of the refrigerant (gaseous state) in kg/m**3
               CALL TPRHO(t,Ps_eva,x,kph,0,rho,ierr,herr)
               rho6 = rho * wm ! rho in kg/m³
C              Enthalpy and entropy of the refrigerant (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h6 = h / wm ! in J/g or kJ/kg
               s6 = s / wm ! in J/(gK) or kJ/(kgK)

C              Entry of the compressor (point 1)
C              Temperature of the overheated refrigerant
               T1 = Teva_out + dTsh
C              The refrigerant is overheated in order to be sure that
C              no liquid enters the compressor
C              Density of the superheated refrigerant (gaseous state) in kg/m**3
               TK= T1 + 273.15
               kph = 2 ! x is vapor phase
               CALL TPRHO(TK,Ps_eva,x,kph,0,rho,ierr,herr)
               rho1 = rho * wm ! rho in kg/m**3
C              Enthalpy and entropy of the superheated refrigerant
C              (gaseous state)
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h1 = h / wm ! in J/g or kJ/kg
               s1 = s / wm ! in J/(gK) or kJ/(kgK)

C              End of the isentropic compression (point 2s)
C              Refrigerant temperature in degC
               CALL PSFLSH(Ps_cond,s,x,t,D,Dl,Dv,xliq,xvap,q,ei,h,
     &                     cv,cp,w,ierr,herr)
               T2S = T - 273.15
C              Density of the refrigerant in kg/m**3
               TK = T2S + 273.15
               CALL TPRHO(TK,Ps_cond,x,kph,0,rho,ierr,herr)
               rho2S = rho * wm ! rho in kg/m**3
C              Enthalpy of the refrigerant in kJ/kg
               CALL THERM(TK,rho,x,p,ei,h,s,cv,cp,w,hjt)
               h2S = h / wm ! in J/g or kJ/kg

C              End of the polytropic compression (point 2)
c              Isentropic efficiency
               eta_isen = A0 + A1 * Tcond + A2 * Tcond**2.0 + A3 * Teva
     &                    + A4 * Teva**2.0 + A5 * Tcond * Teva
               IF (eta_isen .LT. 0.0) eta_isen = 0.01
               IF (eta_isen .GT. 1.0) eta_isen = 1.0
C              enthalpy of the refrigerant in kJ/kg
               h2  = (h2S + h1 * (eta_isen - 1.0)) / eta_isen
C              Refrigerant temperature
               h = h2 * wm ! in J/mol
               CALL PHFLSH(Ps_cond,h,x,t,D,Dl,Dv,xliq,xvap,q,ei,s,
     &                     cv,cp,w,ierr,herr)
               T2 = t - 273.15

C              End of refrigerant thermodynamic cycle
C              Refrigerant mass flow rate (kg/s)
               mdot_ref = Qneed / (h6 - h5)
C              Power dissipated at the condenser (kW)
               Qcond = mdot_ref * (h3 - h4_st)
C              New condenser temperature
               Tcond  = Tc_in + Qcond / (e_cond * cp_c * mdot_cond)

c              Set limit for inlet condenser temperature
               Tcond_in0 = Tcond + 10.0
               Tcond_in1 = Tcond
               xacc = 0.0001 ! accuracy
C              Calculate new Tcond_in using secant method
               Tcond_in = RTSEC1(FUNC1,Tcond_in0,Tcond_in1,xacc,
     &                           Tcond,x,nc)
C              Error
               e = Tcond_in - DP(2)
               IF (ABS(e) .LT. 0.001) THEN
                 EXIT ! exit the loop
               END IF
               IF (I .EQ. 20) THEN
C                 Maximum number of iterations exceeded
                  IF (IP(11) .EQ. 0) THEN
                     IP(1) = 205098
                     CALL MSG(IP,RP,SP)
                     IP(11) = 1
                  ELSE
                     IP(11) = IP(11) + 1
                  END IF
               END IF
C              Store value for next iteration
               DP(2)  = Tcond_in
            END DO
            Tc_out = Tc_in + Qcond / (mdot_cond * cp_c)
            Te_out = Tset
            Qcond  = Qcond
            Qeva   = Qneed
            Pcomp  = mdot_ref * (h2 - h1)
            Pe     = Pcomp / eta
            COP    = Qeva / Pe
            Tcond  = Tcond
            Teva   = Teva
            eta_isen = eta_isen
         END IF
      END IF
      OUT(1) = Tc_out
      OUT(2) = Te_out
      OUT(3) = Qcond
      OUT(4) = Qeva
      OUT(5) = Pcomp
      OUT(6) = Pe
      OUT(7) = COP
      OUT(8) = Tcond
      OUT(9) = Teva
      OUT(10)= eta_isen
      RETURN
      END

C     Function inlet evaporator temperature
      FUNCTION FUNC(Tin,h,T,x,nc)
      IMPLICIT NONE
      INTEGER nc,ierr,kph
      DOUBLE PRECISION T,h,Tin,p,x(nc),TK,D,Dl,Dv,xliq(nc),xvap(nc),q,
     &                 ei,s,cv,cp,w,rhol,rhov,Tout,Tnew,FUNC,kr
      CHARACTER        herr*255

c     Calculate evaporator pressure in kPa
      TK = Tin + 273.15
      kr = 1
      CALL THFLSH(TK,h,x,kr,p,D,Dl,Dv,xliq,xvap,q,ei,s,cv,cp,w,
     &            ierr,herr)
c     Calculate outlet evaporator temperature
      kph = 2 ! vapor phase
      CALL SATP(p,x,kph,TK,rhol,rhov,xliq,xvap,ierr,herr)
      Tout = TK - 273.15
c     New evaporator temperature
      Tnew = (Tin + Tout) / 2.D0
      FUNC = T - Tnew
      RETURN
      END

C     Function inlet condenser temperature
      FUNCTION FUNC1(Tin,T,x,nc)
      IMPLICIT NONE
      INTEGER nc,ierr,kph
      DOUBLE PRECISION T,Tin,p,x(nc),TK,xliq(nc),xvap(nc),
     &                 rhol,rhov,Tout,Tnew,FUNC1
      CHARACTER        herr*255

c     Calculate condenser pressure in kPa
      TK = Tin + 273.15
      kph = 2 ! vapor phase
      CALL SATT(TK,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)

c     Calculate outlet condenser temperature
      kph = 1 ! liquid phase
      CALL SATP(p,x,kph,TK,rhol,rhov,xliq,xvap,ierr,herr)
      Tout = TK - 273.15

c     New condenser temperature
      Tnew = (Tin + Tout) / 2.D0

      FUNC1 = T - Tnew

      RETURN
      END

C     Root finding function with secant method for inlet evaporator temperature
      FUNCTION RTSEC(FUNC,X1,X2,XACC,h,T,x,nc)
      IMPLICIT NONE
      INTEGER nc,MAXIT,J
      DOUBLE PRECISION RTSEC,X1,X2,XACC,h,T,x(nc),F,FL,XL,SWAP,DX,FUNC
      PARAMETER (MAXIT=30)
      FL=FUNC(X1,h,T,x,nc)
      F=FUNC(X2,h,T,x,nc)
      IF(ABS(FL).LT.ABS(F))THEN
        RTSEC=X1
        XL=X2
        SWAP=FL
        FL=F
        F=SWAP
      ELSE
        XL=X1
        RTSEC=X2
      ENDIF
      DO 11 J=1,MAXIT
        DX=(XL-RTSEC)*F/(F-FL)
        XL=RTSEC
        FL=F
        RTSEC=RTSEC+DX
        F=FUNC(RTSEC,h,T,x,nc)
        IF(ABS(DX).LT.XACC.OR.F.EQ.0.)RETURN
11    CONTINUE
      !PAUSE 'RTSEC exceed maximum iterations'
      END

C---- Root finding function with secant method for inlet condenser temperature
      FUNCTION RTSEC1(FUNC,X1,X2,XACC,T,x,nc)
      IMPLICIT NONE
      INTEGER nc,MAXIT,J
      DOUBLE PRECISION RTSEC1,X1,X2,XACC,T,x(nc),F,FL,XL,SWAP,DX,FUNC
      PARAMETER (MAXIT=30)
      FL=FUNC(X1,T,x,nc)
      F=FUNC(X2,T,x,nc)
      IF(ABS(FL).LT.ABS(F))THEN
        RTSEC1=X1
        XL=X2
        SWAP=FL
        FL=F
        F=SWAP
      ELSE
        XL=X1
        RTSEC1=X2
      ENDIF
      DO 12 J=1,MAXIT
        DX=(XL-RTSEC1)*F/(F-FL)
        XL=RTSEC1
        FL=F
        RTSEC1=RTSEC1+DX
        F=FUNC(RTSEC1,T,x,nc)
        IF(ABS(DX).LT.XACC.OR.F.EQ.0.)RETURN
12    CONTINUE
      !PAUSE 'RTSEC1 exceed maximum iterations'
      END
