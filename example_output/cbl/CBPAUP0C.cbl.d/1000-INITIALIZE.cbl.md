```cobol
       1000-INITIALIZE.
      *----------------------------------------------------------------*
      *
           ACCEPT CURRENT-DATE     FROM DATE
           ACCEPT CURRENT-YYDDD    FROM DAY

           ACCEPT PRM-INFO FROM SYSIN
           DISPLAY 'STARTING PROGRAM CBPAUP0C::'
           DISPLAY '*-------------------------------------*'
           DISPLAY 'CBPAUP0C PARM RECEIVED :' PRM-INFO
           DISPLAY 'TODAYS DATE            :' CURRENT-YYDDD
```
