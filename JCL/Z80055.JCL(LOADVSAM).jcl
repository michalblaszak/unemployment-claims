//*********************************************************************/
//*                     KSDS File loader                              */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* This script is to compile and run the utility program which       */
//* initialy loads an empty KSDS dataset                              */
//*********************************************************************/
//LOADVSAM JOB 1,NOTIFY=&SYSUID
/*JOBPARM LINES=999999
//*********************************************************************/
//* Copile  LOADVSAM                                                  */
//*********************************************************************/
//COBRUN1  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(INITKSDS),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..CPY,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(INITKSDS),DISP=SHR
//*********************************************************************/
// IF RC = 0 THEN
//*********************************************************************/
//* Run INITKSDS                                                      */
//*********************************************************************/
//RUN     EXEC PGM=INITKSDS
//STEPLIB    DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSLIB     DD DSN=&SYSUID..LOAD,DISP=SHR
//REPORTDD   DD DSN=&SYSUID..UNEMPL.KSDS,DISP=SHR
//SYSOUT     DD SYSOUT=*,OUTLIM=20000
//CEEDUMP    DD DUMMY
//SYSUDUMP   DD DUMMY
//*********************************************************************/
// ELSE
// ENDIF
