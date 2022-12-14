/* MODEL EUTROF1A                                                           */
/* 	EENVOUDIG MODEL VOOR N/P HUISHOUDING EN ALGENGROEI                  */
/* STOWA project uitbreiding procesbeschrijvingen DUFLOW                    */
/*                                                                          */
/* Landbouwuniversiteit Wageningen                                          */
/* Vakgroep Waterkwaliteitsbeheer en Aquatische Oecologie                   */
/* Postbus 8080                                                             */
/* 6700 DD Wageningen                                                       */


WATER A    [2.00] MG C/L;ALGENCONCENTRATIE
WATER DP   [0.01] MG/L  ;ORTO-P CONCENTRATIE
WATER PP   [0.10] MG/L  ;PARTICULAIR-P CONCENTRATIE
WATER NORG [0.20] MG/L  ;ORGANISCH-N CONCENTRATIE
WATER NH4  [0.10] MG/L  ;AMMONIUM-N CONCENTRATIE
WATER NO3  [0.20] MG/L  ;NITRAAT-N CONCENTRATIE

PARM UMAX   [1.00] 1/DAG ;MAXIMALE GROEISNELHEID
PARM ALFA   [0.02] M2/W  ;LICHTEFFICIENTIE
PARM IOPT   [40.0] W/M2  ;OPTIMALE LICHTINTENSITEIT
PARM PIOPT  [1   ] -     ;KEUZE LICHTLIMITATIE (1=SMITH,2=STEELE)
PARM ACA    [30] UG CHL/MG C; CHLOROFYL/KOOLSTOF VERHOUDING
PARM KLOSS  [0.1]  1/DAG ;SNELHEIDCONSTANTE ALGENVERLIEZEN
PARM KMIN   [0.1]  1/DAG ;SNELHEIDSCONSTANTE MINERALISATIE
PARM KNIT   [0.2]  1/DAG ;SNELHEIDSCONSTANTE NITRIFICATIE
PARM KDEN   [0.05] 1/DAG ;SNELHEIDSCONSTANTE DENITRIFICATIE
PARM KP     [0.005] MG/L ;MONOD CONSTANTE P
PARM KN     [0.010] MG/L ;MONOD CONSTANTE N
PARM EPS0   [2.0]   1/M  ;ACHTERGRONDEXTINCTIE
PARM EPSALG [0.016] M2/MG CHL ;SPECIFIEKE EXTINCTIE CHLOROFYL
PARM THGA   [1.04] -     ;TEMPERATUURCOEFFICIENT ALGENGROEI
PARM THMIN  [1.04] -     ;TEMPERATUURCOEFFICIENT MINERALISATIE
PARM THNIT  [1.06] -     ;TEMPERATUURCOEFFICIENT NITRIFICATIE
PARM THDEN  [1.06] -     ;TEMPERATUURCOEFFICIENT DENITRIFICATIE
PARM VSO    [1.0]  M/DAG ;SEDIMENTATIESNELHEID ORGANISCHE STOF
PARM ANC    [0.176] MG N/MG C ; STIKSTOF/KOOLSTOF VERHOUDING
PARM APC    [0.024] MG P/MG C ; FOSFAAT/KOOLSTOF VERHOUDING


XT ISOM  [100.0]   J/CM2  ;DAGSOM INSTRALING
XT L     [12]    UUR    ;DAGLENGTE
XT T     [20]    oC     ;TEMPERATUUR
XT NFLUX [0.0] G/M2,DAG ;NALEVERINGFLUX STIKSTOF
XT PFLUX [0.0] G/M2,DAG ;NALEVERINGFLUX FOSFAAT

FLOW Z [1.0]     M      ;Waterdiepte

{
FT=THGA^(T-20);
FN=MIN(DP/(DP+KP),NH4/(NH4+KN));
IOMAX=2*ISOM*10000/(L*3600);
CHLA=ACA*A;
ETOT=EPS0+EPSALG*CHLA;
IF (PIOPT==1)
{
H1=(1+(ALFA*IOMAX)^2)^0.5;
FL=L*(LN(ALFA*IOMAX+H1+(1-H1)/(ALFA*IOMAX)))/(24*ETOT*Z);
}

IF (PIOPT==2)
{
f=(L/24);
ALFA1=ALFA*exp(-1*ETOT*Z);
FL=2.718*f*(exp(-1*ALFA1)-exp(-1*ALFA))/(ETOT*Z);
}
GA=UMAX*FT*FN*FL;
K1(A)=GA-KLOSS;

KMINT=KMIN*THMIN^(T-20);
K1(NORG)=-KMINT-VSO/Z;
K0(NORG)=KLOSS*ANC*A;

KNITT=KNIT*THNIT^(T-20);
K1(NH4)=-KNITT;
K0(NH4)=KMINT*NORG-GA*ANC*A+NFLUX/Z;

KDENT=KDEN*THDEN^(T-20);
K1(NO3)=-KDENT;
K0(NO3)=KNITT*NH4;

K1(PP)=-KMINT-VSO/Z;
K0(PP)=KLOSS*APC*A;

K1(DP)=0;
K0(DP)=KMINT*PP-GA*APC*A+PFLUX/Z;
KJN=NORG+NH4+ANC*A;
NTOT=KJN+NO3;
PTOT=PP+DP+APC*A;

}

