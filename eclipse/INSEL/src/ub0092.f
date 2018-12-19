C-----------------------------------------------------------------------
C #Begin
C #Block UBRADPREDICT
C #Predictino of solar radiation
C    
C #Layout
C  #Inputs      10
C  #Outputs     10
C  #Parameters  0
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1)
C     #IN(2)
C  #Outputs
C     #OUT(1)
C  #Parameters
C
C  #Strings
C     #None
C #Internals
C  #Integers
C     #IP(1)  Return code
C     #IP(2)  Call mode
C             \begin{detaillist}
C                 \item[-1] Identification call
C                 \item[0]  Standard call
C                 \item[1]  Constructor call
C                 \item[2]  Destructor call
C             \end{detaillist}
C     #IP(3)  Operation mode
C     #IP(4)  User defined block number
C     #IP(5)  Number of current block inputs
C     #IP(6)  Jump parameter
C     #IP(7) Debug level
C     #IP(8..10) Reserved
C   #Reals
C      #None
C   #Doubles
C      #None
C #Dependencies
C   #Subroutine ID
C #Authors
C   Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0092(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     & OPM, GROUP
      PARAMETER (BNAMES = 'UBRADPREDICT'
     &,          OPM   = 1
     &,          INMIN = 1
     &,          INS   = 15
     &,          OUTS  = 15
     &,          IPS   = 12
     &,          RPS   = 99
     &,          DPS   = 0
     &,          BPMIN = 0
     &,          BPS   = 0
     &,          GROUP = 3
     &,          SPMIN = 3
     &,          SPS   = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)

      REAL             Sonnenzeit, Standardzeit, Breite, Laenge, Gex,
     &                 E, B, n, Hwinkel, Awinkel, LaengeN, Deklination,
     &                 Stundenwinkel, GexD, StundenwinkelSrise, pi, DR,
     &                 RD, GexH, Stundenwinkel2, Sonnenzeit2,
     &                 Stundenwinkel3, Sonnenzeit3, Zwinkel, Airmass,
     &                 Gtot, eu, kT, Gdiff, Gdir, Kneigung, Kazimuth,
     &                 Ewinkel, GdirK, GtotK, Gain(24), Qstart, y,
     &                 EffK, GainTOT, Acoll, Run, RunT, GdiffK,Tl(9999),
     &                 HwinkelN, ac, bc, CloudI(9999), Gref, Albedo,
     &                 Temp(9999)


      INTEGER I,A, TimeP, nC, io, x
      PARAMETER (A=24)





C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN

C           Constructor call
      rp(1) = 1


         ELSE
C           Destructor call
         END IF
         RETURN
      END IF

      IF (IP(11) .EQ. 0) THEN
      !     Initialising

      END IF




C---- Standard call ----------------------------------------------------

      pi = 3.141592654
      eu = 2.71828
      DR = pi/180 !Converts Degree to Radiant
      RD = 180/pi !Converts Radiant to Degree
      ac = 0.72 !Wert für Deutschland für Auswirkung von Bewölkungsgrad
      bc = 3.2 !Wert für Deutschland für Auswirkung von Bewölkungsgrad



      Standardzeit = IN(1)
      Breite = IN(2)
      Laenge = IN(3)
      LaengeN = IN(4)
      n = IN(5)
      Kazimuth = IN(7) - 180
      EffK = in(8) !Kollektoreffizienz
      Acoll = in(9)
      TimeP = in(10) !Vorhersagedauer [h]
      Qstart = in(11)
C     Wenn in(19)=1 Automatische Kollektornachführung, sonst wird eingegebene Kollektorneigung berücksichtigt
      Albedo = in(13)



C---- Einlesen der Prognosedaten-----------------------------------

      DO I = 1, 24
        Gain(I) = 0
      END DO

!

       OPEN(20,file=sp(1),
     & status='old', access='sequential')

      DO I = 1, 8760

          READ(20,*, IOSTAT=IO) CloudI(I)

      END DO
      close(20)

      OPEN (21, file=sp(2),
     & status='old', access='sequential')

      DO I = 1, 8760

          READ(21,*, IOSTAT=IO) Tl(I)

      END DO
      close(21)


      OPEN (22, file=sp(3),
     & status='old', access='sequential')

      DO I = 1, 8760

          READ(22,*, IOSTAT=IO) Temp(I)

      END DO
      close(22)



C---- Konvetieren von % Werten in DWD Skala und von Humidity

      DO I = 1, 8760

      IF (CloudI(I) .LT. 12.5) THEN
      CloudI(I) = 1
      ELSE IF ((CloudI(I) .GE. 12.5) .and. (CloudI(I) .LT. 25)) THEN
      CloudI(I) = 2
      ELSE IF ((CloudI(I) .GE. 25) .and. (CloudI(I) .LT. 37.5)) THEN
      CloudI(I) = 3
      ELSE IF ((CloudI(I) .GE. 37.5) .and. (CloudI(I) .LT. 50)) THEN
      CloudI(I) = 4
      ELSE IF ((CloudI(I) .GE. 50) .and. (CloudI(I) .LT. 62.5)) THEN
      CloudI(I) = 5
      ELSE IF ((CloudI(I) .GE. 62.5) .and. (CloudI(I) .LT. 75)) THEN
      CloudI(I) = 6
      ELSE IF ((CloudI(I) .GE. 75) .and. (CloudI(I) .LT. 87.5)) THEN
      CloudI(I) = 7
      ELSE IF ((CloudI(I) .GE. 87.5) .and. (CloudI(I) .LE. 100)) THEN
      CloudI(I) = 8
      END IF


      IF ((Tl(I) .LT. 60)) THEN

         IF (Temp(I) .LT. 10) THEN
         Tl(I) = 1.8
         ELSE
         Tl(I) = 1.8
         END IF

      ELSE IF ((Tl(I) .LT. 70)) THEN

         IF (Temp(I) .LT. 12) THEN
         Tl(I) = 2
         ELSE
         Tl(I) = 2.0
         END IF

      ELSE IF ((Tl(I) .LT. 80)) THEN

         IF (Temp(I) .LT. 12) THEN
         Tl(I) = 2.5
         ELSE
         Tl(I) = 2.5
         END IF

      ELSE IF ((Tl(I) .LT. 90)) THEN


         IF (Temp(I) .LT. 12) THEN
         Tl(I) = 2.5
         ELSE
         Tl(I) = 2.5
         END IF

      ELSE IF ((Tl(I) .GE. 90)) THEN
      Tl(I) = 3.5
      END IF





      END DO



C---- Ertragsprognose für n Stunden-------------------------------------

      DO I = 1, TimeP

      nC = rp(1) !(Standardzeit+I)+(n-rp(1))*24

C---- Bestimmen der Sonnenzeit------------------------------------------

      B = (n - 81)*0.989
      E = 9.87*sin(2*B*DR)-7.53*cos(B*DR)-1.5*sin(B*DR)
      Sonnenzeit = Standardzeit*60 + 4*(Laenge - LaengeN) + E
      Sonnenzeit2 = (Standardzeit+0.5)*60 + 4*(Laenge - LaengeN) + E
      Sonnenzeit3 = (Standardzeit-0.5)*60 + 4*(Laenge - LaengeN) + E


C-----Position der Sonne------------------------------------------------

      Deklination = 23.45*sin(DR*((284+n)*0.9863))

      Stundenwinkel = 0.25*(Sonnenzeit - 720)
      IF (Stundenwinkel .GT. 360) THEN  !Stimmt das so?
      Stundenwinkel = Stundenwinkel - 360
      END IF

      Stundenwinkel2 = 0.25*(Sonnenzeit2 - 720)
      IF (Stundenwinkel2 .GT. 360) THEN
      Stundenwinkel2 = Stundenwinkel2 - 360
      END IF

      Stundenwinkel3 = 0.25*(Sonnenzeit3 - 720)
      IF (Stundenwinkel3 .GT. 360) THEN
      Stundenwinkel3 = Stundenwinkel3 - 360
      END IF

      Hwinkel = asin((cos(Breite*DR)*cos(Deklination*DR)*
     &cos(Stundenwinkel*DR)+sin(Breite*DR)*sin(Deklination*DR)))

      HwinkelN = asin((cos(Breite*DR)*cos(Deklination*DR)*
     &cos(360*DR)+sin(Breite*DR)*sin(Deklination*DR)))

      IF (Hwinkel .LE. 0) THEN
      Hwinkel = 0
      END IF





      Awinkel =  asin((cos(Deklination*DR)*(sin(Stundenwinkel*DR)/
     & cos(Hwinkel))))




!      y = (cos(Stundenwinkel*DR)*
!     &sin(Breite*DR)-tan(Deklination*DR)*cos(Breite*DR))
!
!      IF ((y .GT. -0.2) .and. (y .LT. 0.2)) THEN !Bei Unstetigkeit an Nullstelle
!
!
!      Awinkel =  (asin((cos(Deklination*DR)*(sin(Stundenwinkel2*DR)/
!     &cos(Hwinkel))))+asin((cos(Deklination*DR)*(sin(Stundenwinkel3*DR)/
!     &cos(Hwinkel)))))/2

!      END IF


!      x = (2*cos(Stundenwinkel*DR)*cos(Hwinkel))/(cos(2*Hwinkel)+1)
!
!      IF ((x .LE. 0) .and. (Standardzeit .LT. 12)) THEN
!
!      Awinkel = Awinkel*(-1)
!
!      ELSE IF (x .GT. 0) THEN
!
!      Awinkel = Awinkel + pi
!
!      ELSE IF ((x .LE. 0) .and. (Standardzeit .GT. 12)) THEN
!
!      Awinkel = Awinkel*(-1)+2*pi
!
!      END IF




C-----Tägliche extraterrestrische Strahlung-----------------------------

      StundenwinkelSrise = acos(-tan(DR*Breite)*tan(DR*Deklination))

      Gex = 1367*(1+0.033*cos(2*pi*n/365))

      GexD = Gex*24/pi*(StundenwinkelSrise*sin(DR*Breite)*
     &sin(DR*Deklination)+cos(DR*Deklination)*cos(DR*Breite)*
     &sin(StundenwinkelSrise))

      IF (GexD .LE. 0) THEN
      GexD = 0
      END IF

C-----Stündliche extraterrestrische Strahlung---------------------------

      GexH = 12/pi*Gex*(cos(Breite*DR)*cos(Deklination*DR)*
     &(sin(Stundenwinkel2*DR)-sin(Stundenwinkel3*DR))+
     &2*pi*(Stundenwinkel2-Stundenwinkel3)/360*sin(Breite*DR)*
     %sin(Deklination*DR))

      IF (GexH .LE. 0) THEN
      GexH = 0
      END IF


C-----Air Mass----------------------------------------------------------

      IF (Hwinkel .GE. 0) then
            Zwinkel = 90 - Hwinkel*RD
            else
            Zwinkel = 90
      END IF

      Airmass = 1/(cos(DR*Zwinkel)+0.50572*
     &(6.07995+90-Zwinkel)**(-1.6364))



c      Airmass =1/sin(Hwinkel)


C-----Stündliche terrestrische Strahhlung-------------------------------


c      Gtot = 1.1*GexH*0.7**(Airmass**0.678)


      !Tl = 2 (sehr saubere Kaltluft), 3 (saubere Warmluft), 4 - 6 (feuchtwarme luft), >6 (verunreinigte luft)
      Gtot = GexH*eu**(-Tl(nC)*airmass/(0.9*airmass+9.4)*1) !anstelle von 1 kann P/P0 für den Luftdruck als höhenkorrektur eingefügt werden

      IF (Gtot .LT. 0) then
      Gtot = 0
      END IF


      Gtot = Gtot*(1-ac*(CloudI(nC)/8)**bc) !Gleichung nach "Regenerative Energietechnik, Springer-E-Book

C-----Direkt / Diffusstrahlung (Orgill Hollands)------------------------

      kT = Gtot/GexH

      IF (kT .LT. 0.35) THEN

         Gdiff = Gtot*(1-0.249*kT)

      ELSE IF ((kT .LT. 0.75) .AND. (kT .GE. 0.35)) THEN

         Gdiff = Gtot*(1.557-1.84*kT)

      ELSE

         Gdiff = Gtot*0.177

      END IF

      Gdir = Gtot - Gdiff

C-----Einstrahlung in Kollektorebene------------------------------------

      Kneigung = atan(abs(cos(Kazimuth*DR-Awinkel))*tan(Zwinkel*DR))*RD
      !Kollektorneigung = Transversalwinkel


      IF (Kneigung .LE. 0) THEN
         Kneigung = 0
      END IF

      IF (Hwinkel .LT. HwinkelN*0.5) THEN
         Kneigung = 0
      END IF

C-----On / Off automatische Nachführung)--------------------------------
      If (in(19) .eq. 0) then
      Kneigung = in(6)
      end if
C-----------------------------------------------------------------------

      Ewinkel = acos(cos(Hwinkel)*cos(Awinkel - Kazimuth*DR)*
     &sin(Kneigung*DR)+sin(Hwinkel)*cos(Kneigung*DR))



C      Ewinkel = 0
      GdirK = Gdir*cos(Ewinkel)/cos(Zwinkel*DR)
      GdiffK = (1+cos(Kneigung*DR))/2*Gdiff
      Gref = (1-cos(Kneigung*DR))*Albedo/2*Gtot

      GtotK = GdirK + GdiffK + Gref


C-----------------------------------------------------------------------

      Gain(I) = GtotK*EffK/1000*Acoll


      END DO

C-----Output------------------------------------------------------------

      GainTOT = 0

      DO I = 1, TimeP
         GainTOT = GainTOT + Gain(I)
      ENDDO

      IF ((GainTOT .GT. Qstart) .AND. (RunT .EQ. 0)) THEN
      Run = 1
      RunT = TimeP

      ELSE IF (RunT .GT. 0) THEN
      Run = 1
      RunT = RunT - 1

      ELSE
      Run = 0

      END IF


      out(1) = Gtot
      out(2) = Gdir
      out(3) = Gdiff
      out(4) = GtotK

      IF (GdirK .ge. 3000) then
      GdirK = 0

      Else IF (GdirK .le. 1) then
      GdirK = 0

      End If

      out(5) = GdirK
      out(6) = GdiffK
      out(7) = Run
      out(8) = 0
      out(9) = 0
      out(10) = GexH
      out(11)= Gref

      rp(1) = rp(1)+1


      RETURN
      END
C-----------------------------------------------------------------------
