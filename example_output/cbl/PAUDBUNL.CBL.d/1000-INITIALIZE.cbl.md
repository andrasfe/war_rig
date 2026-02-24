```cobol
       1000-INITIALIZE.                                                 01690026
      *----------------------------------------------------------------*01700026
      *                                                                 01710026
           ACCEPT CURRENT-DATE     FROM DATE                            01720026
           ACCEPT CURRENT-YYDDD    FROM DAY                             01730026
                                                                        01740026
      *    ACCEPT PRM-INFO FROM SYSIN                                   01750038
           DISPLAY 'STARTING PROGRAM PAUDBUNL::'                        01760054
           DISPLAY '*-------------------------------------*'            01770026
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01790043
           DISPLAY ' '                                                  01800026
                                                                        01810026
           .                                                            01960026
           OPEN OUTPUT OPFILE1                                          01961028
           IF WS-OUTFL1-STATUS =  SPACES OR '00'                        01962028
              CONTINUE                                                  01963028
           ELSE                                                         01964028
              DISPLAY 'ERROR IN OPENING OPFILE1:' WS-OUTFL1-STATUS      01965028
              PERFORM 9999-ABEND                                        01966028
           END-IF                                                       01967028
      *                                                                 01968028
           OPEN OUTPUT OPFILE2                                          01969028
           IF WS-OUTFL2-STATUS =  SPACES OR '00'                        01969128
              CONTINUE                                                  01969228
           ELSE                                                         01969328
              DISPLAY 'ERROR IN OPENING OPFILE2:' WS-OUTFL2-STATUS      01969428
              PERFORM 9999-ABEND                                        01969528
           END-IF.                                                      01969634
```
