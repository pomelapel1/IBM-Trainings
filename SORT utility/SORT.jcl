//RTPOT12 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=0M,COND=(16,LT)
//SORTSTEP EXEC PGM=SORT
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=RTPOT12.LEARN.EMPPROJ,DISP=SHR
//SORTOUT  DD DSN=RTPOT12.LEARN.EMPPRJ.SORT.DTA,DISP=(NEW,CATLG,KEEP),
//           STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),
//           DCB=(LRECL=170,BLKSIZE=0,RECFM=FB,DSORG=PS)
//SYSIN DD *
  SORT FIELDS=(1,4,CH,A,20,2,CH,A)
/*