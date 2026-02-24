```cobol
       1000-INITIALIZE.                                                 01690026
      *----------------------------------------------------------------*01700026
      *                                                                 01710026
           ACCEPT CURRENT-DATE     FROM DATE                            01720026
           ACCEPT CURRENT-YYDDD    FROM DAY                             01730026
                                                                        01740026
           DISPLAY '*-------------------------------------*'            01770026
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01790043
           DISPLAY ' '                                                  01800026
                                                                        01810026
           .                                                            01960026
           OPEN INPUT  INFILE1                                          01961054
           IF WS-INFIL1-STATUS =  SPACES OR '00'                        01962053
              CONTINUE                                                  01963028
           ELSE                                                         01964028
              DISPLAY 'ERROR IN OPENING INFILE1:' WS-INFIL1-STATUS      01965053
              PERFORM 9999-ABEND                                        01966028
           END-IF                                                       01967028
      *                                                                 01968028
           OPEN INPUT INFILE2                                           01969054
           IF WS-INFIL2-STATUS =  SPACES OR '00'                        01969153
              CONTINUE                                                  01969228
           ELSE                                                         01969328
              DISPLAY 'ERROR IN OPENING INFILE2:' WS-INFIL2-STATUS      01969453
              PERFORM 9999-ABEND                                        01969528
           END-IF.                                                      01969634
      *                                                                 01969728
      *                                                                 01970026
       1000-EXIT.                                                       01980026
            EXIT.                                                       01990026
```
