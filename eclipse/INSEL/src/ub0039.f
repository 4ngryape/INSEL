C-----------------------------------------------------------------------
C #Begin
C #Block UBINTRADMAERZ2015
C #Description
C    
C #Layout
C  #Inputs      6
C  #Outputs     6
C  #Parameters  7
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) Temperature (°C) wall 1
C     #IN(2) Temperature (°C) wall 2
C     #IN(3) Temperature (°C) wall 3
C     #IN(4) Temperature (°C) wall 4
C     #IN(5) Temperature (°C) wall 5
C     #IN(6) Temperature (°C) wall 6
C     #IN(7) Temperature (°C) wall 7
C     #IN(8) Temperature (°C) wall 8
C     #IN(9) Temperature (°C) wall 9
C     #IN(10) Temperature (°C) wall 10
C     #IN(11) Temperature (°C) wall 11
C     #in(19) Temperature (°C) wall 12
C     #IN(13) Temperature (°C) wall 13
C     #IN(14) Temperature (°C) wall 14
C     #IN(15) Temperature (°C) wall 15
C     #IN(16) Temperature (°C) wall 16
C     #IN(17) Temperature (°C) wall 17
C     #IN(18) Temperature (°C) wall 18
C     #IN(19) Temperature (°C) wall 19
C     #IN(20) Temperature (°C) wall 20
C  #Outputs
C     #OUT(1) Radiative heat flux (W/m^2) from wall 1
C     #OUT(2) Radiative heat flux (W/m^2) from wall 2
C     #OUT(3) Radiative heat flux (W/m^2) from wall 3
C     #OUT(4) Radiative heat flux (W/m^2) from wall 4
C     #OUT(5) Radiative heat flux (W/m^2) from wall 5
C     #OUT(6) Radiative heat flux (W/m^2) from wall 6
C     #OUT(7) Radiative heat flux (W/m^2) from wall 7
C     #OUT(8) Radiative heat flux (W/m^2) from wall 8
C     #OUT(9) Radiative heat flux (W/m^2) from wall 9
C     #OUT(10) Radiative heat flux (W/m^2) from wall 10
C     #OUT(11) Radiative heat flux (W/m^2) from wall 11
C     #OUT(12) Radiative heat flux (W/m^2) from wall 12
C     #OUT(13) Radiative heat flux (W/m^2) from wall 13
C     #OUT(14) Radiative heat flux (W/m^2) from wall 14
C     #OUT(15) Radiative heat flux (W/m^2) from wall 15
C     #OUT(16) Radiative heat flux (W/m^2) from wall 16
C     #OUT(17) Radiative heat flux (W/m^2) from wall 17
C     #OUT(18) Radiative heat flux (W/m^2) from wall 18
C     #OUT(19) Radiative heat flux (W/m^2) from wall 19
C     #OUT(20) Radiative heat flux (W/m^2) from wall 20
C  #Parameters
C     #BP(1) Afootprint m2
C     #BP(2) Hroom
C     #BP(3) ndiv
C     #BP(4) Nwalls (no roof or ceiling)
C     #BP(5) Nwindows
C     #BP(6) Emission coef
C     #BP(7) Param walls and winds
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
C   INSEL Block Wizard
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0039(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBINTRADMAERZ2015'
     &,          OPM   = 1
     &,          INMIN = 6
     &,          INS   = 20
     &,          OUTS  = 20
     &,          IPS   = 11
     &,          RPS   = 500
     &,          DPS   = 0
     &,          BPMIN = 27
     &,          BPS   = 151
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)

      double precision  pi/3.141592653590/
      integer       i,j,kWall,kWind,k,m,n,mm
      real          Afoot,Hroom,pointW(20,2),normal(20),alpha_ini(20),
     >                Awind(20),H(20),pointR(2,2)
      integer       ndiv,Nwalls,Nwind,WhichWall(20)
      logical       bWall(20),bWind(20)
      real          incWi,incWj,Xr,Yr,Ar,Br,Xmin,Xmax,Ymin,
     >                Ymax,Xw,Yw,Aw,Bw,X,Y
      integer       ray(20,100,20,100),touch(20,100,20,100),choque(20),
     >                closest
      real          dist(20),alpha,D,Rxy(20,100,20,100),
     >                phiixy(20,100,20,100),phijxy(20,100,20,100)
      logical       crash,wall(20)

      integer       iWalls,jWalls
      real          Lxi,Lxj,Lzi,Lzj,incAi,incAj,incZ,sumi
      real          A(20),R,Fij(20,20),phi(20,20),Aaux(20)

      integer       Nsurf
      REAL          QRAD(20),E(20)
      REAL          QRADM,SIGMA/0.0000000567/,SUME,SUMEPS
      REAL          EPSILON(20),SUMAB

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
C---- Standard call ----------------------------------------------------
      !First call
      IF (IP(11) .EQ. 0) THEN
        IP(11) = 1
          !Param preprocessing
          Afoot = BP(1)
          Hroom = BP(2)
          ndiv = BP(3)
          Nwalls = BP(4)
          Nwind = BP(5)
          kWall=2
          kWind=0
          j=6+Nwalls+Nwind+2
          do i=3,Nwalls+2+Nwind
            bWall(i)=.false.
            bWind(i)=.false.
            if(BP(j).eq.0) then
              kWall=kWall+1
              bWall(i)=.true.
              pointW(kWall,1)=BP(j+1)
              pointW(kWall,2)=BP(j+2)
              normal(kWall)=BP(j+3)*pi/180              !rad
              alpha_ini(kWall)=(BP(j+3)-90)*pi/180      !rad
              if(alpha_ini(kWall).lt.0) then
                alpha_ini(kWall)=2*pi+alpha_ini(kWall)
              end if
              j=j+4
            elseif(BP(j).eq.1) then
              kWind=kWind+1
              bWind(i)=.true.
              Awind(kWind)=BP(j+1)
              WhichWall(kWind)=BP(j+2)
              j=j+3
            else
              write(*,*) 'ERROR'
            end if
          end do
          do i=3,Nwalls+2
            j=i+1
            If(i.eq.Nwalls+2) j=3
            H(i)=((pointW(j,1)-pointW(i,1))**2+
     >        (pointW(j,2)-pointW(i,2))**2)**0.5        !wall footprint long (m)
          end do

          !Initializing variables
          Fij=0
          ray=0
          touch=0
          Nwalls=Nwalls+2

          !Throughting rays in footprint plane
          do i=3,Nwalls-1
            incWi=H(i)/ndiv
            do j=1,ndiv
              !initial point of the ray
              pointR(1,1)=pointW(i,1)+incWi*(j-0.5)*sin(normal(i))           !x value of the ray in wall i
              pointR(1,2)=pointW(i,2)-incWi*(j-0.5)*cos(normal(i))           !y value of the ray in wall i
              A(20)=sin(normal(i))
              A(19)=cos(normal(i))
              do k=1+i,Nwalls
                incWj=H(k)/ndiv
                do m=1,ndiv
                  do n=1,Nwalls
                    choque(n)=0
                    dist(n)=1000000000
                  end do
                  !final point of the ray
                  pointR(2,1)=pointW(k,1)+incWj*(m-0.5)*sin(normal(k))       !x value of the ray in wall j
                  pointR(2,2)=pointW(k,2)-incWj*(m-0.5)*cos(normal(k))       !y value of the ray in wall j
                  !angle of the ray (rad)
                  Xr=pointR(2,1)-pointR(1,1)
                  Yr=pointR(2,2)-pointR(1,2)
                  If(Xr.eq.0) Then
                    If(Yr.gt.0) Then
                      alpha=pi/2
                    Else
                      alpha=3*pi/2
                    End If
                  Else
                    alpha=atan(Yr/Xr)
                  End If
                  If(Yr.gt.0) Then
                    If(alpha.lt.0) Then
                      alpha=alpha+pi
                    End If
                  ElseIf(Yr.lt.0) Then
                    If(alpha.lt.0) Then
                      alpha=alpha+2*pi
                    Else
                      alpha=alpha+pi
                    End If
                  Else
                    If(Xr.gt.0) Then
                      alpha=0
                    Else
                      alpha=pi
                    End If
                  End If
                  If(cos(abs(alpha-normal(i))).lt.0) Then
                    crash=.False.
                  Else
                    !Definition of lines
                    !ray
                    If(abs(Xr).lt.0.00000001) Then
                      If(Yr.gt.0) Then
                        Ar=1000000000
                      Else
                        Ar=-1000000000
                      End If
                    Else
                      Ar=Yr/Xr
                      Br=pointR(1,2)-Ar*pointR(1, 1)
                    End If
                    !wall n
                    do n=i+1,Nwalls
                      mm=n+1                                                !Wall 1 has points 1 and 2, wall 2, points 2 + 3, ...
                      If(mm.gt.Nwalls) mm=1
                      !boundaries of wall n
                      If(pointW(n,2).gt.pointW(mm,2)) Then
                        Ymin=pointW(mm,2)
                        Ymax=pointW(n,2)
                      Else
                        Ymin=pointW(n,2)
                        Ymax=pointW(mm,2)
                      End If
                      If (pointW(n,1).gt.pointW(mm,1)) Then
                        Xmin=pointW(mm,1)
                        Xmax=pointW(n,1)
                      Else
                        Xmin=pointW(n,1)
                        Xmax=pointW(mm,1)
                      End If
                      !line wall n
                      Xw=pointW(mm,1)-pointW(n,1)
                      Yw=pointW(mm,2)-pointW(n,2)
                      If (Xw.eq.0) Then
                        If (Yw.gt.0) Then
                          Aw=1000000000
                        Else
                          Aw=-1000000000
                        End If
                      Else
                        Aw=Yw/Xw
                        Bw=pointW(n,2)-Aw*pointW(n,1)
                      End If
                      !Does the ray touch wall n?
                     If((pointW(i+1,1).eq.pointW(n,1).and.(pointW(i+1,2)
     >                  .eq.pointW(n,2))).and.(sin(normal(n)-normal(i))
     >                  .lt.0)) Then
                        crash=.False.
                      Else
                        If(abs(Ar).eq.1000000000) Then
                          If(abs(Aw).eq.1000000000) Then
                            crash=.False.
                          Else
                            X=pointR(1,1)
                            Y=Bw+Aw*X
                            If(((Xmin.le.X).and.(X.le.Xmax)).or.
     >                        ((Xmax-Xmin.lt.0.00000001).and.(Xmax-X.lt.
     >                        0.00000001))) Then
                              If(cos(abs(alpha-normal(n))).lt.0) Then
                                crash=.True.
                              Else
                                crash=.False.
                              End If
                            Else
                              crash=.False.
                            End If
                          End If
                        Else
                          If(abs(Aw).eq.1000000000) Then
                            X=pointW(n,1)
                            Y=Br+Ar*X
                            If(((Ymin.le.Y).and.(Y.le.Ymax)).or.
     >                        ((Ymax-Ymin.lt.0.00000001).and.
     >                        (Ymax-Y.lt.0.00000001))) Then
                              If(cos(abs(alpha-normal(n))).lt.0) Then
                                crash=.True.
                              Else
                                crash=.False.
                              End If
                            Else
                              crash=.False.
                            End If
                          Else
                            If(abs(Ar-Aw).lt.0.00000001) Then               !parallels
                              crash=.False.
                            Else
                              X=(Bw-Br)/(Ar-Aw)
                              Y=Aw*X+Bw
                              If((((Xmin.le.X).and.(X.le.Xmax)).or.
     >                        ((Xmax-Xmin.lt.0.00000001).and.(Xmax-X.lt.
     >                        0.00000001))).and.(((Ymin.le.Y).and.(Y.le.
     >                        Ymax)).or.((Ymax-Ymin.lt.0.00000001).and.
     >                        (Ymax-Y.lt.0.00000001)))) Then
                                If(cos(abs(alpha-normal(n))).lt.0) Then
                                  crash=.True.
                                Else
                                  crash=.False.
                                End If
                              Else
                                crash=.False.
                              End If
                            End If
                          End If
                        End If
                      End If
                      If (crash) Then
                        choque(n)=1
                        dist(n)=((pointR(2,1)-pointR(1,1))** 2+
     >                    (pointR(2,2)-pointR(1,2))**2)**0.5                !Longitude of the ray to know which crash is closest
                      End If
                    end do
                  End If
                  !Searching the closest wall
                  closest=0
                  dist(closest)=1000000000
                  do n=1,Nwalls
                    If(choque(n).eq.1) Then
                      If(dist(n).lt.dist(closest)) Then
                        closest=n
                        D=dist(n)
                      End If
                    End If
                  end do
                  ray(i,j,k,m)=closest
                  Rxy(i,j,k,m)=D
                  phiixy(i,j,k,m)=abs(atan(tan(normal(i)-alpha)))
                  phijxy(i,j,k,m)=abs(atan(tan(normal(k)-alpha)))
                  If ((ray(i,j,k,m).eq.k)) Then
                    touch(i,j,k,m)=1
                  Else
                    touch(i,j,k,m)=0
                  End If
                end do
              end do
            end do
          end do

          !Calculate Fij
          do i=3,Nwalls
            A(i)=H(i)*Hroom
          end do
          Lzi=Hroom/ndiv
          Lzj=Hroom/ndiv
          do iWalls=3,Nwalls-1
            Lxi=H(iWalls)/ndiv
            do jWalls=iWalls+1,Nwalls
              Lxj=H(jwalls)/ndiv
              incAi=Lxi*Lzi
              incAj=Lxj*Lzj
              do i=1,ndiv                                                   !i wall footprint divisions
                do j=1,ndiv                                                 !i wall hinght divisions
                  do k=1,ndiv                                               !j wall footprint divisions
                    do m=1,ndiv                                             !j wall hinght divisions
                      incZ=Lzj*(m-j)
                      R=Rxy(iWalls,i,jWalls,k)**2+incZ**2
                      R=R**0.5
                      Fij(iWalls,jWalls)=Fij(iWalls,jWalls)+
     >                  (Rxy(iWalls,i,jWalls,k)**2*
     >                  cos(phiixy(iWalls,i,jWalls,k))*
     >                  cos(phijxy(iWalls,i,jWalls,k))/R**4)
     >                  *touch(iWalls,i,jWalls,k)
                    end do
                  end do
                end do
              end do
              Fij(iWalls,jWalls)=Fij(iWalls,jWalls)/A(iWalls)/pi*incAi
     >          *incAj
            end do
          end do
          do i=3,Nwalls-1
            do j=i+1,Nwalls
              Fij(j,i)=Fij(i,j)*A(i)/A(j)
            end do
          end do

          do i=3,Nwalls
            sumi = 0
            do j=3,Nwalls
              sumi=sumi+Fij(i,j)
            end do
            Fij(i,1)=(1-sumi)/2
            Fij(i,2)=Fij(i,1)
            Fij(1,i)=Fij(i,1)*A(i)/Afoot
            Fij(2,i)=Fij(i,2)*A(i)/Afoot
          end do
          sumi=0
          do j=3,Nwalls
            sumi=sumi+Fij(1,j)
          end do
          Fij(1,2)=(1-sumi)
          Fij(2,1)=(1-sumi)
          A(1)=Afoot
          A(2)=Afoot

          !Windows treatment (percentage of the wall area)
          Aaux(1)=A(1)
          Aaux(2)=A(2)
          j=2
          do i=3,Nwalls+Nwind
            if(bWall(i)) then
              j=j+1
              Aaux(i)=A(j)
            end if
          end do
          j=0
          do i=1,Nwalls+Nwind
            if(bWind(i)) then
              j=j+1
              Aaux(WhichWall(j))=Aaux(WhichWall(j))-Awind(j)
              Aaux(i)=Awind(j)
            end if
          end do
          A=0
          j=0
          do i=1,Nwalls+Nwind
            A(i)=Aaux(i)
          end do
          do i=1,Nwalls+Nwind
            if(bWind(i)) then
              j=j+1
              A(i)=0
              A(WhichWall(j))=Awind(j)+A(WhichWall(j))
            end if
          end do

          !placing the walls in their real position of all surfaces
          !first moving i
          do i=3,Nwalls+Nwind
            if(bWind(i)) then
              do j=i,Nwalls+Nwind-1
                m=Nwalls+Nwind-j+i
                do k=1,Nwalls+Nwind
                  Fij(m,k)=Fij(m-1,k)
                  Fij(m-1,k)=0
                end do
              end do
            end if
          end do
          !second moving j
          do i=3,Nwalls+Nwind
            if(bWind(i)) then
              do j=i,Nwalls+Nwind-1
                m=Nwalls+Nwind-j+i
                do k=1,Nwalls+Nwind
                  Fij(k,m)=Fij(k,m-1)
                  Fij(k,m-1)=0
                end do
              end do
            end if
          end do

          do i=1,Nwalls+Nwind
            m=0
            do j=1,Nwalls+Nwind
              if(bWind(j)) then
                m=m+1
                Fij(i,j)=Fij(i,WhichWall(m))*Aaux(j)/A(WhichWall(m))
              end if
            end do
          end do

          do i=1,Nwalls+Nwind
            m=0
            wall=.true.
            do j=1,Nwalls+Nwind
              if(bWind(j)) then
                m=m+1
                if(wall(WhichWall(m))) then
                  Fij(i,WhichWall(m))=Fij(i,WhichWall(m))
     >              *Aaux(WhichWall(m))/A(WhichWall(m))
                  wall(WhichWall(m))=.false.
                end if
              end if
            end do
          end do


          m=0
          do i=1,Nwalls+Nwind
            if(bWind(i)) then
              m=m+1
              do j=1,Nwalls+Nwind
                Fij(i,j)=Fij(WhichWall(m),j)
              end do
            end if
          end do

          m=0
          do i=1,Nwalls+Nwind
            if(bWind(i)) then
              m=m+1
              do j=1,Nwalls+Nwind
                Fij(i,j)=Fij(j,i)*Aaux(j)/Aaux(i)
                Fij(WhichWall(m),j)=Fij(j,WhichWall(m))*Aaux(j)/
     >            Aaux(WhichWall(m))
              end do
            end if
          end do

        RP(1)=Nwalls+Nwind
        do i=1,Nwalls+Nwind
          RP(79+i)=Aaux(i)
        end do
        m=0
        do i=1,Nwalls+Nwind
          do j=1,Nwalls+Nwind
            m=m+1
            RP(99+m)=Fij(i,j)
          end do
        end do
      end if

      !Second call
      Nsurf=RP(1)
      do i=1,Nsurf
        epsilon(i)=BP(5+i)
      end do
      do i=1,Nsurf
        A(i)=RP(79+i)
      end do
      m=0
      do i=1,Nsurf
        do j=1,Nsurf
          m=m+1
          Fij(i,j)=RP(99+m)
        end do
      end do

      do i=1,Nsurf
        do j=1,Nsurf
          Phi(i,j)=Fij(i,j)*A(i)
        end do
      end do

      SUME   = 0.0
      SUMEPS = 0.0

      DO I = 1,Nsurf
         E(I)   = SIGMA  * EPSILON(I) * (IN(I)+273.15)**4
         SUME   = SUME   + E(I)       * A(I)
         SUMEPS = SUMEPS + EPSILON(I) * A(I)
      END DO

C     Mittlere Flaechenhelligkeit des Raumes: QRADM
      if(SUMEPS.gt.0) then
        QRADM = SUME / SUMEPS
      else
        QRADM=0
      end if

C     Calculate sum of radiative fluxes from other walls onto wall i
C     using view factors and mean radiative flux
      DO I = 1,Nsurf
        SUMAB = 0.0
        DO J = 1,Nsurf
          SUMAB = SUMAB + PHI(I,J) * (E(J) + (1-EPSILON(J)) * QRADM)
        END DO
        QRAD(I) = E(I) - EPSILON(I) * SUMAB/ A(I)
      END DO
      DO I = 1,Nsurf
        OUT(I) = -QRAD(I)
      END DO



      RETURN
      END
C-----------------------------------------------------------------------
