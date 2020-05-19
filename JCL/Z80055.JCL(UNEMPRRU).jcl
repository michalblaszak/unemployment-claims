//*********************************************************************/
//*                          Report Generator runner                  */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* This script is to run the report  application. It just runs the   */
//* frontend program: UNEMMAIN                                        */
//* REPORTDD - the database DSN needed by the subprogram (UNEMPAPI)   */
//*********************************************************************/
//UNEMPLJ JOB 1,NOTIFY=&SYSUID
/*JOBPARM LINES=999999
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
