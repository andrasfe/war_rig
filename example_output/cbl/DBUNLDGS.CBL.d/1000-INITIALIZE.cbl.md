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
```
