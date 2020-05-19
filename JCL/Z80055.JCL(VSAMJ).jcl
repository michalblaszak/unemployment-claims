//*********************************************************************/
//* The utility job.                                                  */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* Define the KSAM KSDS cluster                                      */
//*********************************************************************/
//VSAMJ  JOB CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
//*
//MAKEVSAM EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD *
    DEFINE CLUSTER (NAME(Z80055.UNEMPL.KSDS) -
    INDEXED -
    RECORDSIZE(312 312) -
    KEYS(8 0) -
    VOLUMES(VPWRKA) -
    RECORDS(10 50)) -
    DATA (NAME(Z80055.UNEMPL.KSDS.DATA)) -
    INDEX (NAME(Z80055.UNEMPL.KSDS.INDEX))
/*
