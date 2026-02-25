```cobol

               MOVE PA-MERCHANT-NAME          TO MERNAMEO
               MOVE PA-MERCHANT-ID            TO MERIDO
               MOVE PA-MERCHANT-CITY          TO MERCITYO
               MOVE PA-MERCHANT-STATE         TO MERSTO
               MOVE PA-MERCHANT-ZIP           TO MERZIPO
           END-IF
           .

       RETURN-TO-PREV-SCREEN.

           MOVE WS-CICS-TRANID TO CDEMO-FROM-TRANID
           MOVE WS-PGM-AUTH-DTL TO CDEMO-FROM-PROGRAM
           MOVE ZEROS          TO CDEMO-PGM-CONTEXT
           SET CDEMO-PGM-ENTER TO TRUE

           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.


       SEND-AUTHVIEW-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COPAU1AO
           MOVE -1       TO CARDNUML

           IF SEND-ERASE-YES
```
