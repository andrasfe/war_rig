```cobol
       MAIN-PARA.                                                       01220026
      *     DISPLAY 'CHECK PROG PCB:' PAUTBPCB.                         01222039
            ENTRY 'DLITCBL'                 USING PAUTBPCB.             01225033
                                                                        01226029
            DISPLAY 'STARTING PAUDBLOD'.                                01227053
      *                                                                 01230026
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01240026
      *                                                                 01250026
           PERFORM 2000-READ-ROOT-SEG-FILE        THRU 2000-EXIT        01260053
           UNTIL   END-ROOT-SEG-FILE  = 'Y'                             01280053
                                                                        01281053
           PERFORM 3000-READ-CHILD-SEG-FILE       THRU 3000-EXIT        01290058
           UNTIL   END-CHILD-SEG-FILE = 'Y'                             01300053
                                                                        01531150
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01532030
      *                                                                 01540026
      *                                                                 01560026
      *                                                                 01650026
           GOBACK.                                                      01660026
      *                                                                 01670026
      *----------------------------------------------------------------*01680026
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
```
