/*-----------------------------------------------------------------------
  #Begin
  #Block UBSTORAGE
  #Description
     
  #Layout
     #Inputs     2
     #Outputs    7
     #Parameters 9
     #Strings    0
     #Group      S
  #Details
     #Inputs
        #IN[0] Energy surplus
        #IN[1] Energy deficiency
     #Outputs
        #OUT[0] State of charge
        #OUT[1] To battery
     #Parameters
        #BP[0] Battery capacity
        #BP[1] Initial capacity
        #BP[2] Charging efficiency
        #BP[3] Discharging efficiency
        #BP[4] Time step
        #BP[5] DOD
        #BP[6] Self discharge rate per month
        #BP[7] Inverter efficiency
        #BP[8] Ageing coefficient
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

#include <math.h>

// Attention: out must not be renamed to OUT (Windows.h conflict)

extern "C" void id (float *in, float *out, int *IP, float *RP, double *DP,
                    float *BP, char SP[][1024], char BNAMES[1024],
                    int *OPM, int *INMIN, int *INS, int *OUTS,
                    int *IPS, int *RPS, int *DPS,
                    int *BPMIN, int *BPS, int *SPMIN,
                    int *SPS, int *GROUP);

extern "C" void ub0024(float *in, float *out, int *IP, float *RP,
   double *DP, float *BP, char SP[][1024])
{
   char BNAMES[1024] = "UBSTORAGE";
   int  OPM   = 1;
   int  INMIN = 2;
   int  INS   = 2;
   int  OUTS  = 7;
   int  IPS   = 14;
   int  RPS   = 7;
   int  DPS   = 14;
   int  BPMIN = 9;
   int  BPS   = 9;
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
        out[0]=BP[1]; /*state of charge = initial capacity*/
	DP[0]=out[0];
	DP[7]=BP[0]*1000;
	out[6]=DP[7];
			
	/* To battery*/
	if(in[0]<(1-DP[0])*BP[0]*DP[9]*1000/BP[2])
		      out[1]=in[0];
	else{
		out[1]=(1-DP[0])*BP[0]*DP[9]*1000/BP[2];
		}
	DP[1]=out[1];
	
	/* From battery*/		
	if(in[1]<(DP[0]-(1-(BP[5]/100)))*BP[0]*DP[9]*1000*BP[3])
		out[2]=in[1];
	else{
		out[2]=(DP[0]-(1-(BP[5]/100)))*BP[0]*DP[9]*1000*BP[3];
		}
	DP[2]=out[2]; 
	
	DP[3]=DP[1];
	DP[4]=DP[2];
		
	IP[12]=1; /* counter*/
       }
      else
      {
         // Destructor call   
	      
      }
      return;
   }
   // Standard call ----------------------------------------------------        
       
        /* conditions for capacity loss depending on the time step*/
        if(BP[4]==0){
		DP[5]=730;
		DP[9]=1;
		}
	else if(BP[4]==1){ /*16s Intervall */
		DP[5]=11716;
		DP[9]=225;
		}
	else if(BP[4]==2){ /*30s Intervall */
		DP[5]=21900;
		DP[9]=120;
		}		
	else if(BP[4]==3){ /*60s Intervall */
		DP[5]=43800;
		DP[9]=60;
		}
	else if(BP[4]==4){ /*5min Intervall */
			DP[5]=8760;
			DP[9]=12;
			}
		
		
        /* state of charge including capacity loss due to ageing */

	out[0]=DP[0]+(DP[1]*BP[2]-DP[2]/BP[3])/(out[6]*DP[9]);
	DP[6]=BP[0]*1000*BP[8]*(DP[0]-out[0]);
	if(DP[6]>0)
		DP[8]=DP[6];
	else
		DP[8]=0;
	out[6]=DP[7]-DP[8];
	DP[7]=out[6];
	       		        
	/* capacity threshold depending on DOD */
	if(out[0]>=1)
		{
		out[0]=1;
	        }
	else if(out[0]<(1-(BP[5]/100)))
		{
		out[0]=(1-(BP[5]/100));
		}
	/* self-discharge of battery */	
	int zahl2=1;
	for (int i=0; i<(IP[12]/DP[5]);i++){
	zahl2*=(1-BP[6]);
	}

	if(in[0]==0&&in[1]==0){
		out[0]=DP[0]*zahl2;
		IP[12]=IP[12]+1; 
		}
	else{
		DP[0]=out[0];
	        IP[12]=1;
		}

		

	/* Capacity in Wh */	
	out[3]=out[0]*out[6];	
	if(out[3]<BP[0]*1000*(1-(BP[5]/100)))
		{
		out[3]=BP[0]*1000*(1-(BP[5]/100));
		}
		
	/* To battery*/
	
	if(in[0]<(1-DP[0])*out[6]*DP[9]/BP[2])
		out[1]=in[0]*BP[7];
	else{
		out[1]=((1-DP[0])*out[6]*DP[9]/BP[2])*BP[7];
	      }
		
	/*if(out[1]>18000)
		out[1]=18000; */

	DP[1]=out[1];  /* peak output*/
	
	
        /* From battery*/
	
	if(in[1]<(DP[0]-(1-(BP[5]/100)))*out[6]*DP[9]*BP[3])
		out[2]=in[1]*BP[7];
	else{
		out[2]=(DP[0]-(1-(BP[5]/100)))*out[6]*DP[9]*BP[3]*BP[7];
		}
		
	DP[2]=out[2];
		
	DP[3]=DP[3]+DP[1]; /* To battery accumulation */
        DP[4]=DP[4]+DP[2]; /* From battery accumulation */
		
	out[5]=(DP[3]-DP[4])/DP[9]; /* Battery losses */
	
	/* number of cycles counter */	
        out[4]=(DP[3]/DP[7])/DP[9];
}
