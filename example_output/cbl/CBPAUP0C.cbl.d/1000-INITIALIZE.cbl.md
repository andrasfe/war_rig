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
           DISPLAY ' '

           IF P-EXPIRY-DAYS IS NUMERIC
              MOVE P-EXPIRY-DAYS     TO WS-EXPIRY-DAYS
           ELSE
              MOVE 5                 TO WS-EXPIRY-DAYS
           END-IF
           IF P-CHKP-FREQ = SPACES OR 0 OR LOW-VALUES
              MOVE 5                 TO P-CHKP-FREQ
           END-IF
           IF P-CHKP-DIS-FREQ = SPACES OR 0 OR LOW-VALUES
              MOVE 10                TO P-CHKP-DIS-FREQ
           END-IF
           IF P-DEBUG-FLAG NOT = 'Y'
              MOVE 'N'               TO P-DEBUG-FLAG
           END-IF
           .
      *
       1000-EXIT.
            EXIT.
```
