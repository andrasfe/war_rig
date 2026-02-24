```cobol
       PROCESS-PF8-KEY.
      *****************************************************************

           IF CDEMO-CPVS-PAUKEY-LAST = SPACES OR LOW-VALUES
               MOVE LOW-VALUES             TO WS-AUTH-KEY-SAVE
           ELSE
               MOVE CDEMO-CPVS-PAUKEY-LAST TO WS-AUTH-KEY-SAVE

               PERFORM GET-AUTH-SUMMARY
               PERFORM REPOSITION-AUTHORIZATIONS
           END-IF

           MOVE -1                         TO ACCTIDL OF COPAU0AI

           SET SEND-ERASE-NO               TO TRUE

           IF NEXT-PAGE-YES
               PERFORM INITIALIZE-AUTH-DATA

               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               MOVE 'You are already at the bottom of the page...'
                                           TO WS-MESSAGE
           END-IF
           .
```
