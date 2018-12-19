/*-----------------------------------------------------------------------
  #Begin
  #Block UBPRODCONS
  #Description
     
  #Layout
     #Inputs     2
     #Outputs    3
     #Parameters 0
     #Strings    0
     #Group      S
  #Details
     #Inputs
        #IN[0] PV
        #IN[1] Load
     #Outputs
        #OUT[0] Own (without battery)
        #OUT[1] Energy surplus
     #Parameters
        #None
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
     INSEL Block Wizard
  #End
-----------------------------------------------------------------------*/

// Attention: out must not be renamed to OUT (Windows.h conflict)

extern "C" void id (float *in, float *out, int *IP, float *RP, double *DP,
                    float *BP, char SP[][1024], char BNAMES[1024],
                    int *OPM, int *INMIN, int *INS, int *OUTS,
                    int *IPS, int *RPS, int *DPS,
                    int *BPMIN, int *BPS, int *SPMIN,
                    int *SPS, int *GROUP);

extern "C" void ub0012(float *in, float *out, int *IP, float *RP,
   double *DP, float *BP, char SP[][1024])
{
   char BNAMES[1024] = "UBPRODCONS";
   int  OPM   = 1;
   int  INMIN = 2;
   int  INS   = 2;
   int  OUTS  = 3;
   int  IPS   = 11;
   int  RPS   = 1;
   int  DPS   = 2;
   int  BPMIN = 0;
   int  BPS   = 0;
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
   DP[0]=in[0]-in[1];
   DP[1]=in[1]-in[0];
   
   if(in[0]<in[1])
	   out[0]=in[0];
   else
	   out[0]=in[1];
   if(DP[0]>0)
	   out[1]=DP[0];
   else
	   out[1]=0;
   if(DP[1]>0)
	   out[2]=DP[1];
   else
	   out[2]=0;
}
