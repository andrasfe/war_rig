```cobol
      *----------------------------------------------------------------*
      *  IMS SEGMENT LAYOUT
      *----------------------------------------------------------------*
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT
       01 PENDING-AUTH-SUMMARY.
       COPY CIPAUSMY.

      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD
       01 PENDING-AUTH-DETAILS.
       COPY CIPAUDTY.

       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF     TO TRUE
           SET SEND-ERASE-YES  TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COPAU1AO

           IF EIBCALEN = 0
               INITIALIZE CARDDEMO-COMMAREA

               MOVE WS-PGM-AUTH-SMRY        TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               MOVE SPACES                  TO CDEMO-CPVD-FRAUD-DATA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   PERFORM PROCESS-ENTER-KEY

                   PERFORM SEND-AUTHVIEW-SCREEN
               ELSE
                   PERFORM RECEIVE-AUTHVIEW-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN DFHPF3
                           MOVE WS-PGM-AUTH-SMRY     TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF5
                           PERFORM MARK-AUTH-FRAUD
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN DFHPF8
                           PERFORM PROCESS-PF8-KEY
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN OTHER
                           PERFORM PROCESS-ENTER-KEY

                           MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                           PERFORM SEND-AUTHVIEW-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-CICS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
```
