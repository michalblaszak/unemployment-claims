//*********************************************************************/
//* The utility job.                                                  */
//* Author: Michal Blaszak                                            */
//* Date:   2020.05.15                                                */
//*********************************************************************/
//* Define the path to the alternate index for the KSAM KSDS cluster  */
//*********************************************************************/
//PATHJ  JOB CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
//*
//MAKEPATH EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
 DEFINE    PATH     (NAME (Z80055.UNEMPL.KAIX.PATH)         -
                     PATHENTRY (Z80055.UNEMPL.KAIX)        -
                     UPDATE                                   -
                    )
/*
