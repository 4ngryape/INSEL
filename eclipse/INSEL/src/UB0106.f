
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0106(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM

      INTEGER I, io



      PARAMETER       (BNAMES = 'UBREAD'
     &,                OPM    = 1
     &,                INMIN  = 0
     &,                INS    = 4
     &,                OUTS   = 1
     &,                IPS    = 45
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 2
     &,                BPS    = 2
     &,                SPMIN  = 1
     &,                SPS    = 1
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1),STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),II
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1),
     &                 Data(999,9999)
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



         I = 1



            OPEN(30,FILE=sp(1), STATUS='OLD', IOSTAT=IO,
     &      ACTION='READ', access='direct', RECL=12,FORM='FORMATTED')
            READ (30,'(1F12.4)',REC=I) Data(I,0)
            CLOSE(30)

            out(1) = Data(I,0)

      RETURN
      END


