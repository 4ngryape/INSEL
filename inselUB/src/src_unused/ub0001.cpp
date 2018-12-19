/*-----------------------------------------------------------------------
  #Begin
  #Block CPP
  #Description
     The CPP block is a user-defined prototype block.
  #Layout
     #Inputs      1
     #Outputs     1
     #Parameters  1
     #Strings     0
     #Group       S
  #Details
     #Inputs
        #IN[0] Any number $x$
     #Outputs
        #OUT[0] $x + p$
     #Parameters
        #BP[0] Any number $p$
     #Strings
        #None
  #Internals
     #Integers
        #IP[0]  Return code
        #IP[1]  Call mode
                \begin{detaillist}
                   \item[-1] Identification call
                   \item[0]  Standard call
                   \item[1]  Constructor call
                   \item[2]  Destructor call
                \end{detaillist}
        #IP[2]  Operation mode
        #IP[3]  User defined block number
        #IP[4]  Number of current block inputs
        #IP[5]  Jump parameter
        #IP[6]  Debug level
        #IP[7..9]  Reserved
     #Reals
        #None
     #Doubles
        #None
  #Dependencies
     #None
  #Authors
     Juergen Schumacher
  #End
-----------------------------------------------------------------------*/

// Attention: out must not be renamed to OUT (Windows.h conflict)

extern "C" void id (float *in, float *out, int *IP, float *RP, double *DP,
                    float *BP, char SP[1024], char BNAMES[1024],
                    int *OPM, int *INMIN, int *INS, int *OUTS,
                    int *IPS, int *RPS, int *DPS,
                    int *BPMIN, int *BPS, int *SPMIN,
                    int *SPS, int *GROUP);

extern "C" void ub0001(float *in, float *out, int *IP, float *RP,
   double *DP, float *BP, char SP[])
{
   char BNAMES[1024] = "CPP";
   int  OPM   = 1;
   int  INMIN = 1;
   int  INS   = 1;
   int  OUTS  = 1;
   int  IPS   = 10;
   int  RPS   = 0;
   int  DPS   = 0;
   int  BPMIN = 1;
   int  BPS   = 1;
   int  SPMIN = 0;
   int  SPS   = 0;
   int  GROUP = 3;

   if (IP[1] != 0)
   {
      if (IP[1] == -1)
      {
          // Identification call
          id(in, out, IP, RP, DP, BP, SP, BNAMES,
             &OPM, &INMIN, &INS, &OUTS, &IPS, &RPS, &DPS,
             &BPMIN, &BPS, &SPMIN, &SPS, &GROUP);
      }
      else if(IP[1] == 1)
      {
         // Constructor call
      }
      else
      {
         // Destructor call
      }
      return;
   }
   // Standard call ----------------------------------------------------
   out[0] = in[0] + BP[0];
}
