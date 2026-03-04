```cobol
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01660000
           DISPLAY ' '                                                  01670000
                                                                        01680000
           .                                                            01690000
      *    OPEN OUTPUT OPFILE1                                          01700000
      *    IF WS-OUTFL1-STATUS =  SPACES OR '00'                        01710000
      *       CONTINUE                                                  01720000
      *    ELSE                                                         01730000
      *       DISPLAY 'ERROR IN OPENING OPFILE1:' WS-OUTFL1-STATUS      01740000
      *       PERFORM 9999-ABEND                                        01750000
      *    END-IF                                                       01760000
      *                                                                 01770000
      *    OPEN OUTPUT OPFILE2                                          01780000
      *    IF WS-OUTFL2-STATUS =  SPACES OR '00'                        01790000
      *       CONTINUE                                                  01800000
      *    ELSE                                                         01810000
      *       DISPLAY 'ERROR IN OPENING OPFILE2:' WS-OUTFL2-STATUS      01820000
      *       PERFORM 9999-ABEND                                        01830000
      *    END-IF.                                                      01840000
      *                                                                 01850000
      *                                                                 01860000
       1000-EXIT.                                                       01870000
            EXIT.                                                       01880000
      *                                                                 01890000
      *----------------------------------------------------------------*01900000
       2000-FIND-NEXT-AUTH-SUMMARY.                                     01910000
      *----------------------------------------------------------------*01920000
      *                                                                 01930000
      *     DISPLAY 'IN 2000 READ ROOT SEGMENT PARA'                    01940002
      *              PAUT-PCB-STATUS                                    01950000
            INITIALIZE PAUT-PCB-STATUS                                  01960000
            CALL 'CBLTDLI'            USING  FUNC-GN                    01970000
                                        PAUTBPCB                        01980000
                                        PENDING-AUTH-SUMMARY            01990000
                                        ROOT-UNQUAL-SSA.                02000000
      *     DISPLAY ' *******************************'                  02010002
      *     DISPLAY ' AFTER THE ROOT SEG IMS CALL    '                  02020002
      *     DISPLAY 'SEG LEVEL: ' PAUT-SEG-LEVEL                        02030002
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02040002
      *     DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                       02050002
      *     DISPLAY ' *******************************'                  02060000
```
