```cobol
       1000-INITIALIZE.                                                 01570000
      *----------------------------------------------------------------*01580000
      *                                                                 01590000
           ACCEPT CURRENT-DATE     FROM DATE                            01600000
           ACCEPT CURRENT-YYDDD    FROM DAY                             01610000
                                                                        01620000
      *    ACCEPT PRM-INFO FROM SYSIN                                   01630000
           DISPLAY 'STARTING PROGRAM DBUNLDGS::'                        01640000
           DISPLAY '*-------------------------------------*'            01650000
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
```
