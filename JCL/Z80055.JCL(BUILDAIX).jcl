//*********************************************************************/
//* The utility job.                                                  */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* Build the alternate index for the KSAM KSDS cluster               */
//*********************************************************************/
//BUILDAIX  JOB CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
//*
//BUILDIDX EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
 BLDINDEX -
     INDATASET (Z80055.UNEMPL.KSDS) -
     OUTDATASET (Z80055.UNEMPL.KAIX)
/*