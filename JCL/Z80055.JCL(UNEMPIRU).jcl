//*********************************************************************/
//*                         Data Importer runner                      */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* This script is to run the application gathering data from XML     */
//* source files, combining them according to the common 'record_id'  */
//* attribute. It consists of the following programs:                 */
//* 1. UNEMPL   - The main, control program to call follwoing         */
//*               subprograms. Each of these programs share the same  */
//*               logic and structrue however they import data from   */
//*               different source XML files. Each if these XML files */
//*               has diffrent internal structure.                    */
//* The importer subprograms taking data from their source XML file   */
//* and saving to UNEMPLRP VSAM file:                                 */
//* 2. IMPBYAGE - Source: UNEMPL(BYAGE).xml                           */
//* 3. IMPBYGEN - Source: UNEMPL(BYGENDER).xml                        */
//* 4. IMPBYIND - Source: UNEMPL(BYINDUST).xml                        */
//* 5. IMPBYRAC - Source: UNEMPL(BYRACE).xml                          */
//* 6. IMPBYETH - Source: UNEMPL(BYETHNIC).xml                        */
//*********************************************************************/
//UNEMPLJ JOB 1,NOTIFY=&SYSUID
/*JOBPARM LINES=999999
//*********************************************************************/
//* Excute UNEMPL main program.                                       */
//*********************************************************************/
//RUN     EXEC PGM=UNEMPL
//STEPLIB    DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSLIB     DD DSN=&SYSUID..LOAD,DISP=SHR
//BYAGEDD    DD DSN=&SYSUID..UNEMPL(BYAGE),DISP=SHR
//BYGENDDD   DD DSN=&SYSUID..UNEMPL(BYGENDER),DISP=SHR
//BYINDDD    DD DSN=&SYSUID..UNEMPL(BYINDUST),DISP=SHR
//BYRACEDD   DD DSN=&SYSUID..UNEMPL(BYRACE),DISP=SHR
//BYETHDD    DD DSN=&SYSUID..UNEMPL(BYETHNIC),DISP=SHR
//REPORTDD   DD DSN=&SYSUID..UNEMPL.KSDS,DISP=SHR
//* AIX for UNEMPLRP KSDS VSAM
//REPORTD1   DD DSN=&SYSUID..UNEMPL.KAIX.PATH,DISP=SHR
//SYSOUT     DD SYSOUT=*,OUTLIM=20000
//CEEDUMP    DD DUMMY
//SYSUDUMP   DD DUMMY
//*********************************************************************/
