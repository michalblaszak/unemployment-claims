//*********************************************************************/
//* The utility job.                                                  */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* Define the alternate index for the KSAM KSDS cluster              */
//*********************************************************************/
//AIXJ  JOB CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
//*
//MAKEAIX EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
    DEFINE AIX (NAME(Z80055.UNEMPL.KAIX) -
       RELATE(Z80055.UNEMPL.KSDS) -
       CYLINDERS(2 1) -
       VOLUMES(VPWRKA) -
       RECORDSIZE(23 23) -
       KEYS(10 8) -
       ORDERED -
       UNIQUEKEY -
       UPGRADE -
       ) -
       DATA (NAME(Z80055.UNEMPL.KAIX.DATA)) -
       INDEX (NAME(Z80055.UNEMPL.KAIX.INDEX))
/*
