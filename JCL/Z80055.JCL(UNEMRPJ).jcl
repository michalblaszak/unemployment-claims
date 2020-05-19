//*********************************************************************/
//*                     Report Generator compiler                     */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* This script is to compile and run the report generation           */
//* application. The application consists of two programs:            */
//* 1. UNEMPAPI - the backend part of the application retrieving data */
//*               from the database and exposing an API to retrieve   */
//*               data according to filtering criteria.               */
//* 2. UNEMMAIN - the frontend program sending requiests to the       */
//*               backend API, and displaying results to the user.    */
//*********************************************************************/
//UNEMRPJ JOB 1,NOTIFY=&SYSUID
/*JOBPARM LINES=999999
//*********************************************************************/
//* Copile  UNEMPAPI - backend API                                    */
//*********************************************************************/
//COBRUN1  EXEC IGYWCL,PARM='XMLPARSE(XMLSS)'
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMPAPI),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..CPY,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMPAPI),DISP=SHR
//*********************************************************************/
//* Copile  UNEMMAIN - frontend                                       */
//*********************************************************************/
//COBRUN10  EXEC IGYWCL,PARM='XMLPARSE(XMLSS)'
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMMAIN),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..CPY,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMMAIN),DISP=SHR
//*********************************************************************/
// IF RC = 0 THEN
//*********************************************************************/
//* Run UNEMMAIN                                                      */
//*********************************************************************/
//RUN     EXEC PGM=UNEMMAIN
//STEPLIB    DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSLIB     DD DSN=&SYSUID..LOAD,DISP=SHR
//REPORTDD   DD DSN=&SYSUID..UNEMPL.KSDS,DISP=SHR
//* AIX for UNEMPLRP KSDS VSAM
//REPORTD1   DD DSN=&SYSUID..UNEMPL.KAIX.PATH,DISP=SHR
//SYSOUT     DD SYSOUT=*,OUTLIM=20000
//CEEDUMP    DD DUMMY
//SYSUDUMP   DD DUMMY
//*********************************************************************/
// ELSE
// ENDIF
